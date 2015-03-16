#This script takes our three primary corpora and creates a random sample of each
files = c('data//en_US//en_US.blogs.txt', 'data//en_US//en_US.news.txt', 'data//en_US//en_US.twitter.txt')

sampleFile <- function(fullPath, sampleRate = .1)
{
  fh <- file(fullPath, "rb")
  all <- readLines(fh, encoding="UTF-8", skipNul = T)
  close(fh)
  keepers = rbinom(length(all),1, sampleRate)
  subset = all[keepers == 1]
  writeLines(subset, paste(fullPath, "_SAMPLE", sep = ''))
}

sapply(files, sampleFile, sampleRate = .1)
