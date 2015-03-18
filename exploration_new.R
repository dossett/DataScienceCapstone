sampleNews <- readFile("data//en_US//en_US.news.txt_SAMPLE")
sampleTweets <- readFile("data//en_US//en_US.twitter.txt_SAMPLE")
sampleBlogs <- readFile("data//en_US//en_US.blogs.txt_SAMPLE")

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
  
  #drop all punctuation other than apostrophe
  #regex via http://stackoverflow.com/a/8698368
  x <- stri_replace_all(x, replacement = "", regex="[^[:alnum:][:space:]']") 
}

#Combine our sources and clean them up
# FIXME - split all lines into sentences
allText <- sapply(c(sampleNews, sampleTweets, sampleBlogs), cleanUpText)

#Construct 1-, 2-, 3-, and 4-gram tokenizers to process these documents
library(RWeka)
library(tm)
delimit = ' \t' #delimit on space and tab, leave newline to separate sentences
OneGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters=delimit))
TwoGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters=delimit))
ThreeGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters=delimit))
FourGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters=delimit))

one_tdm <- TermDocumentMatrix(VCorpus(VectorSource(allText)), control=list(tokenize=OneGramTokenizer))
two_tdm <- TermDocumentMatrix(VCorpus(VectorSource(allText)), control=list(tokenize=OneGramTokenizer))
three_tdm <- TermDocumentMatrix(VCorpus(VectorSource(allText)), control=list(tokenize=OneGramTokenizer))
four_tdm <- TermDocumentMatrix(VCorpus(VectorSource(allText)), control=list(tokenize=OneGramTokenizer))
