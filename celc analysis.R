# celc
# --
# Fabricio Kury, Patrick Ryan, Chunhua Weng
# celc analysis.R start: 2018/10/8

setwd('C:\\Users\\fk2374\\OneDrive\\IB\\DBMI\\Projetos\\celc\\R')
# setwd('C:\\Users\\fabri\\OneDrive\\IB\\DBMI\\Projetos\\celc\\R')

library(readxl)

## Functions
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

rem_eoa <- function(x) sub("(?<=\\S)]", "", x, perl = T) # "Remove end-of-appraisal" (when it is not the sole character)

data_dir <- '../Data'
xlsx_file <- paste0(data_dir, '/celc - appraisals - cs2 2018-10-08.xlsx')
outfile <- paste0(data_dir, '/celc - trial aggregates.csv')
presence_flags <- c('1', '2', '3')
query_flags <- c('t', 'e', 'c', 'h')
flags <- c(presence_flags, query_flags)

message('Reading up to 2500 rows from file ', xlsx_file, '.')
xf <- read_xlsx(xlsx_file, 'celc', n_max = 2500)
xf$Presence <- rem_eoa(xf$Presence)
xf$`Query-ability` <- rem_eoa(xf$`Query-ability`)
xf <- xf[grepl(paste(collapse='|', flags), xf$Presence), ] # Keep only appraised elements

tally <- function(flags, column) {
  res <- lapply(flags, function(e, column)
      c(e, sum(grepl(e, xf[[column]])), length(unique(xf$Trial[grepl(e, xf[[column]])]))),
      column = column)
  res <- as.data.frame(do.call(rbind, res))
  rownames(res) <- flags
  colnames(res) <- c('Flag', 'Elements', 'Trials')
  res
}

presence_tally <- tally(presence_flags, 'Presence')
query_tally <- tally(query_flags, 'Query-ability')

aggregate_by_trial <- function(flags, column)
  t(apply(xf, 1, function(e) c(Trial=e[['Trial']], sapply(flags, grepl, x = e[[column]]))))

res <- merge(aggregate_by_trial(presence_flags, 'Presence'),
             aggregate_by_trial(query_flags, 'Query-ability'))
res[,-1] <- lapply(res[,-1], function(x) as.logical(x))
res <- aggregate(res[,-1], by = list(Trial=res$Trial), any)
res <- res[rowSums(res[, -1])>0,]
rownames(res) <- NULL
message('Saving results to ', outfile, '.')
write.csv(cbind(res, Link=paste0('https://clinicaltrials.gov/ct2/show/NCT', trim(res$Trial))), outfile, row.names = F)

cross_freq <- do.call(rbind, lapply(flags, function(c) colSums(res[res[[c]], -1])))
rownames(cross_freq) <- flags
# cross_freq <- cross_freq[rowSums(cross_freq)>0, colSums(cross_freq)>0]

cat(paste0('Total trials: ', length(unique(res$Trial)), '.\n'))
cat('\n')

cat('Tallies of data elements:\n')
print(xtabs(~Presence+`Query-ability`, data=xf))
cat('\n')

cat('Tallies of trials:\n')
print(cross_freq)
