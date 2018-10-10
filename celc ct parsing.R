# celc
# --
# Fabricio Kury, Patrick Ryan, Chunhua Weng
# celc ct 3.R start: 2018/10/2

setwd('C:\\Users\\fabri\\OneDrive\\IB\\DBMI\\Projetos\\celc\\R')
library(XML)
library(RCurl)
library(stringr)
library(readr)
library(xml2)

curtime <- function() format(Sys.time(), "%Y_%m_%d-%H_%M")
exec_label <- curtime()
console_only <- F
only_these_nct <- NULL
do_trials <- T # If false, will do everything but process the trials.
do_csv <- T
csv_header <- 'Trial|Component|Text\n'
n_trials <- -1 # Number of trials to process from input file. If negative, will process all.
output_dir <- paste0('../Data/R output/brat/', exec_label)
csv_outfile <- paste0(output_dir, '/', 'celc ', exec_label, '.csv')
options("encoding" = "UTF-8")
clinical_trials_list_file <- '../Data/Clinical trials/cs2.txt'
elig_crit_block_rgx <- paste0('(?:<th class="header3 banner_color rowHeader">Eligibility Criteria&nbsp;',
  '<sup style="color:blue">)([\\s\\S]+?)(?=<th class="header2 banner_color sectionBanner" colspan="2">Administrative',
  ' Information</th>)')
elig_crit_rgx <- '(?:<li style="margin-top:0\\.7ex;">)(.*)(?:</li>)'

if(do_csv)
  message('Output directory set to ', output_dir, '.')

if(!dir.exists(output_dir))
  dir.create(output_dir, recursive = T)

if(do_csv) {
  if(file.exists(csv_outfile))
    file.remove(csv_outfile)
  cat(csv_header, file=csv_outfile)
}

unescape_html <- function(str)
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))

clean_text <- function(m) {
  m <- unescape_html(paste0(str_replace_all(m, '\r|\n|\\|\\n', ' '), collapse = '; '))
  while(grepl('  ', m))
    m <- str_replace_all(m, '  ', ' ')
  m
}

n_count <- 0
do_trial <- function(nct) {
  to_csv <- function(name, body) {
    m <- paste(sep='|', nct, name, clean_text(body))
    if(console_only)
      message(m)
    else
      cat(file=csv_outfile, sep = '\n', append=TRUE, m)
  }

  xr <- xmlRoot(xmlParse(read_file(
    paste0('https://clinicaltrials.gov/ct2/show/', nct, '?displayxml=true')),
    useInternalNode=TRUE))
  fi <- read_file(paste0('https://clinicaltrials.gov/ct2/show/record/', nct, '?view=record'))

  get_it <- function(...)
    sapply(getNodeSet(xr, paste0('/clinical_study/', sep='/', ...)), xmlValue)
  
  official_title <- get_it('official_title')
  to_csv('TITLE', ifelse(length(official_title), official_title, get_it('brief_title')))
  detailed_description <- get_it('detailed_description/textblock')
  if(length(detailed_description))
    to_csv('DESCRIPTION', detailed_description)
  
  to_csv('SUMMARY', get_it('brief_summary/textblock'))

  primary_outcomes_measure <- get_it('primary_outcome/measure')
  primary_outcomes_time_frame <- get_it('primary_outcome/time_frame')
  for(i in 1:length(primary_outcomes_measure))
    to_csv('PRIMARY OUTCOME', paste0(primary_outcomes_measure[i], ' [', primary_outcomes_time_frame[i], ']'))

  secondary_outcomes_measure <- get_it('secondary_outcome/measure')
  secondary_outcomes_time_frame <- get_it('secondary_outcome/time_frame')
  if(length(secondary_outcomes_measure))
    for(i in 1:length(secondary_outcomes_measure))
      to_csv('SECONDARY OUTCOME', paste0(secondary_outcomes_measure[i],
        ' [', secondary_outcomes_time_frame[i], ']'))

    arm_groups <- xpathApply(xr, '/clinical_study/arm_group', xmlChildren)
    if(length(arm_groups))
      for(i in 1:length(arm_groups))
        to_csv('ARM GROUP', paste0(xmlValue(arm_groups[[i]]$arm_group_label), ' (',
          xmlValue(arm_groups[[i]]$arm_group_type), '): ', xmlValue(arm_groups[[i]]$description)))

  interventions <- xpathApply(xr, '/clinical_study/intervention', xmlChildren)
  for(i in 1:length(interventions)) {
    intervention <- interventions[[i]]
    arm_group_label <- str_replace_all(xmlValue(intervention$arm_group_label), "(\\'|\\\")", "\\$1")
    arm_group <- xpathApply(xr, paste0('/clinical_study/arm_group[arm_group_label = "',
      arm_group_label, '"]/arm_group_label'), xmlChildren)
    to_csv('INTERVENTION', paste0(xmlValue(intervention$intervention_name), ' (',
      xmlValue(intervention$intervention_type), '): ', xmlValue(intervention$description),
      ifelse(length(arm_group), paste0(' [arm groups: ', paste0(lapply(arm_group,
      function(e) { xmlValue(e$text) }), collapse = '; '), ']'), '')))
  }

  get_all_criteria <- function(nct)
    str_match_all(read_file(paste0('https://clinicaltrials.gov/ct2/show/', nct)),
      '<li style="margin-top:0\\.7ex;">(.*)</li>')[[1]][,2]
  
  get_criteria <- function(m)
    str_match_all(m, '<li style="margin-top:0\\.7ex;">(.*)</li>')[[1]][,2]
  
  tryCatch({
    criteria_textblock <- str_match_all(fi, elig_crit_block_rgx)[[1]][,2]
    for(criterium in str_match_all(criteria_textblock, elig_crit_rgx)[[1]][,2])
      to_csv('INC/EXC CRITERIA', criterium)
  }, error = function(e) {
    message('Unable to distinguish inclusion vs exclusion criteria in ', nct, '.')
    criteria <- get_all_criteria(nct)
    for(i in 1:length(criteria))
      to_csv('INC/EXC CRITERIA ', criteria[i])
  })

  n_count <<- n_count+1
  message(nct, ' done. (n = ', n_count, ', target = ', n_trials, ')')
  TRUE
}

input_nct <- readLines(clinical_trials_list_file, n_trials)
if(!is.null(only_these_nct) || is.null(input_nct))
  input_nct <- only_these_nct

if(do_trials)
  lapply(input_nct, function(t) {
      tryCatch(do_trial(t),
               error = function(e) { message('Error in trial ', t, '.') })
    })
