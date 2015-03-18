#This script takes our three primary corpora and creates a random sample of each
files = c('data//en_US//en_US.blogs.txt', 'data//en_US//en_US.news.txt', 'data//en_US//en_US.twitter.txt')

readFile <- function(fullPath)
{
  fh <- file(fullPath, "rb")
  all <- readLines(fh, encoding="UTF-8", skipNul = T)
  close(fh)
  
  all
}

sampleFile <- function(fullPath, sampleRate = .1)
{
  set.seed(1976L)
  all <- readFile(fullPath)
  keepers = rbinom(length(all),1, sampleRate)
  subset = all[keepers == 1]
  
  #writeLines(subset, paste(fullPath, "_SAMPLE", sep = ''))
  output <- file(paste(fullPath, "_SAMPLE", sep = ''), open="w", encoding = "UTF-8")
  writeLines(subset, output)
  close(output)
}

#After processing a corpus with readLines, perform basic tokenization and filtering
# stringi is a super fast collection of string utilities
library(stringi)

#returns of a vector of vectors for each lines
# x[1] = a vector of tokens from the first line
# x[[1]][1] = the first token
#
# NOTE this means the 'documents' (i.e. the first level split) can include multiple sentences and
# that can get lost as punctuation is stripped off.  The presence of words in the same document
# can still convey information though, so let's keep it for now.
#
getTokens <- function(x)
{
  #Initial tokenization split
  y <- stri_split_regex(x, pattern = '\\s+')
  
  #collapse everything to lowercase
  y <- lapply(y, stri_trans_tolower)
  
  #On each token discard any non-letters
  y <- lapply(y, stri_replace_all, replacement = '', charclass = '[\\P{Letter}]')
  
  #Drop any tokens that have been reduced to nothing (i.e. had no letters to begin with)
  y <- lapply(y, stri_subset, regex='[\\p{Letter}]')
}

sapply(files, sampleFile, sampleRate = .01)
