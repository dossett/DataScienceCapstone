sampleNews <- readFile("data//en_US//en_US.news.txt_SAMPLE")
sampleTweets <- readFile("data//en_US//en_US.twitter.txt_SAMPLE")
sampleBlogs <- readFile("data//en_US//en_US.blogs.txt_SAMPLE")
#allText <- paste(sampleNews, sampleTweets, sampleBlogs, collapse = "\n")

#All purpose clean up function
library(stringi)
cleanUpText <- function(x)
{
  #drop unicode(?) markings [why did this show up in UTF-8?]
  x <- stri_replace_all(x, replacement="", regex = "<U\\+\\d\\d\\d\\d>")
  
  #Lowercase everything
  x <- stri_trans_tolower(x)
  
  #translate dashes to spaces
  x <- stri_replace_all(x, replacement= " ", fixed = '-')
  
  #drop all punctuation other than apostrophe and all numbers
  #regex via http://stackoverflow.com/a/8698368
  x <- stri_replace_all(x, replacement = "", regex="[^[A-Za-z][:space:]']") 
}

#Combine our sources and clean them up
# FIXME - split all lines into sentences
#allText <- sapply(c(sampleNews, sampleTweets, sampleBlogs), cleanUpText)

#Special look at first words of sentences
# FIXME - do this
library(data.table)


make3Gram <- function(x)
{
  return (make3GramHelper(x, data.table()))
}

make3GramHelper <- function(x, table)
{
  if (length(x) < 3)
  {
    return (table)
  }
  else
  {
    table <- rbind(table, list(n1=x[1], n2=x[2], n3=x[3], occur=1))
    return (make3GramHelper(x[-1], table))
  }
}

#Construct 1-, 2-, 3-, and 4-gram tokenizers to process these documents
# library(RWeka)
# library(tm)
# delimit = ' \t' #delimit on space and tab, leave newline to separate sentences
# OneGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters=delimit))
# TwoGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters=delimit))
# ThreeGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters=delimit))
# FourGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters=delimit))
# 
# vc <- VCorpus(VectorSource(cleanText))
# one_tdm <- TermDocumentMatrix(vc, control=list(tokenize=OneGramTokenizer))
# two_tdm <- TermDocumentMatrix(vc, control=list(tokenize=TwoGramTokenizer))
# three_tdm <- TermDocumentMatrix(vc, control=list(tokenize=ThreeGramTokenizer))
# four_tdm <- TermDocumentMatrix(vc, control=list(tokenize=FourGramTokenizer))

#system.time(all_three <- rbindlist(lapply(newsTokens, make3Gram), use.names = T, fill = F))

start <- Sys.time()
tweetTokens <- getTokens(sampleTweets)
newsTokens <- getTokens(sampleNews)
blogTokens <- getTokens(sampleBlogs)
three_tweet <- rbindlist(lapply(tweetTokens, make3Gram), use.names = T, fill = F)
three_news <- rbindlist(lapply(newsTokens, make3Gram), use.names = T, fill = F)
three_blog <- rbindlist(lapply(blogTokens, make3Gram), use.names = T, fill = F)
end <- Sys.time()

three_all <- rbindlist(list(three_tweet, three_news, three_blog))
two_all <- three_all
two_all$n3 <- NULL
one_all <- two_all
one_all$n2 <- NULL

library(dplyr)
one_summed <- one_all %>% group_by(n1) %>% summarise(total=sum(occur)) %>% arrange(desc(total))
two_summed <- two_all %>% group_by(n1,n2) %>% summarise(total=sum(occur)) %>% arrange(desc(total))
three_summed <- three_all %>% group_by(n1,n2,n3) %>% summarise(total=sum(occur)) %>% arrange(desc(total))

#FIXME -- cleanup memory here

#A function that for each data table add a percent of total column and cumulative total column
addCumStats <- function(x)
{
  grandTotal <- sum (x$total)
  x$percTotal <- x$total / grandTotal
  mutate(x[order(desc(x$total)),], cumPerc = cumsum(percTotal))
}

three_enriched <- addCumStats(three_summed)
two_enriched <- addCumStats(two_summed)
one_enriched <- addCumStats(one_summed)

