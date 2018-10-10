## celc
# ----
# celc trial xml.R
# Reads trial XML files downloaded from ClinicalTrials.gov, produces a data frame to allow filtering
# the list.

setwd("C:/Users/fk2374/OneDrive/IB/DBMI/Projetos/celc/R")
library(XML)

## globals
data_dir <- "../Data"
trials_xml_dir <- paste0(data_dir, "/Clinical trials/celc2")
outfile <- paste0(data_dir, "/Clinical trials/celc2/celc2.csv")

## functions
pastep <- function(...) # paste file path
  paste(sep="/", ...)

## run
progress <- 1
res <- lapply(pastep(trials_xml_dir, list.files(trials_xml_dir, "(?i).*\\.xml")), function(filepath) {
  message('Processing ', filepath, '. (n = ', progress, ')')
  progress <<- progress+1
  xml_root <- xmlRoot(xmlParse(filepath))
  
  # nct: /clinical_study/id_info/nct_id
  # title: /clinical_study/official_title|brief_title
  # intervention model: /clinical_study/study_design_info/intervention_model
  # number of arms: /clinical_study/number_of_arms
  # has active comparator: /clinical_study/arm_group/arm_group_type
  # has placebo: /clinical_study/arm_group/arm_group_type
  # has drug: /clinical_study/intervention/intervention_type
  # study size: /clinical_study/enrollment
  
  get_element <- function(xpath)
    tryCatch({ getNodeSet(xml_root, xpath)[[1]] },
      error = function(e) { NULL })
  
  nct <- xmlValue(get_element('/clinical_study/id_info/nct_id'))
  
  title <- (function() {
    official_title <- xmlValue(get_element('/clinical_study/official_title'))
    if(is.na(official_title))
      xmlValue(get_element('/clinical_study/brief_title'))
    else
      official_title
  })()
  
  intervention_model <- xmlValue(get_element('/clinical_study/study_design_info/intervention_model'))
  
  n_arms <- xmlValue(get_element('/clinical_study/number_of_arms'))
  
  # has_actv_comp <- (function() {
  #   for(n in getNodeSet(xml_root, '/clinical_study/arm_group/arm_group_type'))
  #     if(xmlValue(n) %in% c('Experimental', 'Active Comparator'))
  #       return(TRUE)
  #   FALSE
  # })()
  
  has_placebo <- (function() {
    for(n in getNodeSet(xml_root, '/clinical_study/arm_group/arm_group_type'))
      if(grepl('placebo', xmlValue(n), ignore.case = T))
        return(TRUE)
    for(n in getNodeSet(xml_root, '/clinical_study/arm_group/description'))
      if(grepl('placebo', xmlValue(n), ignore.case = T))
        return(TRUE)
    FALSE
  })()
  
  has_drug <- (function() {
    for(n in getNodeSet(xml_root, '/clinical_study/intervention/intervention_type'))
      if(grepl('drug', xmlValue(n), ignore.case = T))
        return(TRUE)
    FALSE
  })()
  
  study_size <- xmlValue(get_element('/clinical_study/enrollment'))
  c(nct, title, intervention_model, n_arms, # has_actv_comp, 
    has_placebo, has_drug, study_size)
})

res <- as.data.frame(do.call(rbind, res))
colnames(res) <- c('nct', 'title', 'intervention_model', 'n_arms', # 'has_actv_comp',
  'has_placebo', 'has_drug', 'study_size')
write.csv(res, outfile, row.names = F, na = "")
