#Start with our exploratory data analysis and try to get ALL of the data now,
#or at least a LOT of it.  Several functions will be reimplemented.

#Load unigram distribution from sampled data
# unigrams <- read.table("working_data/one.enriched.txt", stringsAsFactors = F, header = T)
# topN <- 75
# topNWords <- unigrams[1:topN, "n1"]

readFile <- function(fullPath)
{
  fh <- file(fullPath, "rb")
  all <- readLines(fh, encoding="UTF-8", skipNul = T)
  close(fh)
  
  all
}

library(stringi)
cleanAndTokenize <- function(x)
{
  #Initial tokenization split
  y <- stri_split_regex(x, pattern = '\\s+')
  
  #collapse everything to lowercase
  y <- lapply(y, stri_trans_tolower)
  
  #On each token discard any non-letters
  y <- lapply(y, stri_replace_all, replacement = '', charclass = '[\\P{Letter}]')
  
#   #A little ugly.  For each element of y, filter it against stopwords
#   stopWordFilter <- function(x)
#   {
#     unlist(Filter(function(token){!(token %in% topNWords)},x))
#   }
#   #UGLY
#   if (topN > 0)
#   {
#     y <- lapply(y, stopWordFilter)  
#   }
  
  #Drop any tokens that have been reduced to nothing (i.e. had no letters to begin with)
  y <- lapply(y, stri_subset, regex='[\\p{Letter}]')
}

make3Gram <- function(x)
{
  return (make3GramHelper(x, data.table()))
}

make3GramHelper <- function(x, table)
{
  if (length(x) < 3)
    return(table)
  max = length(x) - 2
  tableList = list()
  for (i in 1:max)
  {
    tableList[[length(tableList)+1]] <- 
      list(n1=x[i], n2=x[i+1], n3=x[i+2], occur=1)
    #table <- rbind(table, list(n1=x[i], n2=x[i+1], n3=x[i+2], occur=1))
  }
  return(rbindlist(tableList, use.names = T, fill = F))
}

make4Gram <- function(x)
{
  if (length(x) < 4)
    return(data.table())
  max = length(x) - 3
  tableList = list()
  for (i in 1:max)
  {
    tableList[[length(tableList)+1]] <- 
      list(n1=x[i], n2=x[i+1], n3=x[i+2], n4=x[i+3], occur=1)
  }
  return(rbindlist(tableList, use.names = T, fill = F))
}

make4GramSTART <- function(x)
{
  if (length(x) < 4)
    return(data.table())
  return(rbindlist(list(list(n1=x[1], n2=x[2], n3=x[3], n4=x[4], occur=1)), use.names=T, fill=F))
}
    
#Collapse a 3gram data table into a summation
library(dplyr)
sum3Grams <- function(x)
{
  x %>% group_by(n1,n2,n3) %>% 
    summarise(total=sum(occur)) %>% 
    arrange(desc(total))
}

sum4Grams <- function(x)
{
  x %>% group_by(n1,n2,n3,n4) %>% 
    summarise(total=sum(occur)) %>% 
    arrange(desc(total))
}

#Function to get tokens from a set of files and write back as 3gram data tables
library(data.table)
write3Grams <- function(filePath="second_pass/", filePrefix="^SAMPLEaa")
{
  #Get list of files
  files <- dir(path=filePath, pattern=filePrefix)
  
  #For each file
  # - read it in
  # - cleanse and tokenize it
  # - convert to a 3gram data table
  # - save the 3gram table to disk
  first <- T
  returnList = list()
  for (f in files)
  {
    print(paste("Processing [", f, "]"))
    lines <- readFile(paste(filePath, f, sep = ""))
    print("   Creating Tokens")
    tokens <- cleanAndTokenize(lines)
    print("   Creating 3Grams")
    TriGramDT <- rbindlist(lapply(tokens, make3Gram), use.names = T, fill = F)
    print("   Summing and Saving 3Grams")
    write.table(sum3Grams(TriGramDT), file=paste(filePath,f,".3gram", sep=""))
  }
}

write4Grams <- function(filePath="second_pass/", filePrefix="^SAMPLEaa")
{
  #Get list of files
  files <- dir(path=filePath, pattern=filePrefix)
  
  for (f in files)
  {
    print(paste("Processing [", f, "]"))
    lines <- readFile(paste(filePath, f, sep = ""))
    print("   Creating Tokens")
    tokens <- cleanAndTokenize(lines)
    print("   Creating 4Grams")
    FourGramDT <- rbindlist(lapply(tokens, make4Gram), use.names = T, fill = F)
    print("   Summing and Saving 4Grams")
    write.table(sum4Grams(FourGramDT), file=paste(filePath,f,".4gram", sep=""))
  }
}

#Create ONLY the 4grams that begin a sentence/document
writeStarting4Grams <- write4Grams <- function(filePath="second_pass/", filePrefix="^SAMPLEaa")
  {
    #Get list of files
    files <- dir(path=filePath, pattern=filePrefix)
    
    for (f in files)
    {
      print(paste("Processing [", f, "]"))
      lines <- readFile(paste(filePath, f, sep = ""))
      print("   Creating Tokens")
      tokens <- cleanAndTokenize(lines)
      FourGramDT <- rbindlist(lapply(tokens, make4GramSTART), use.names = T, fill = F)
      print("   Summing and Saving Starting 4Grams")
      write.table(sum4Grams(FourGramDT), file=paste(filePath,f,".4gramSTART", sep=""))
    }
}

#Combine all of the 3gram files into an uber data table
loadTables <- function(path="second_pass/", pattern="^SAMPLE.*3gram")
{
  files <- dir(path, pattern, full.names = T)
  uberTable <- rbindlist(lapply(files, read.table), use.names = T, fill=F)
  uberTable %>% group_by(n1,n2,n3) %>% 
    summarise(grandTotal=sum(total)) %>% 
    arrange(desc(grandTotal))
}

load4Grams <- function(path="second_pass/", pattern="^SAMPLE.*4gram")
{
  files <- dir(path, pattern, full.names = T)
  uberTable <- rbindlist(lapply(files, read.table), use.names = T, fill=F)
  uberTable %>% group_by(n1,n2,n3, n4) %>% 
    summarise(grandTotal=sum(total)) %>% 
    arrange(desc(grandTotal))
}

#Convert a 3gram table to a 2gram
Three2TwoGram <- function(x)
{
  twoGram <- x
  twoGram$n3 <- NULL
  twoGram$total <- twoGram$grandTotal
  twoGram %>% group_by(n1,n2) %>%
    summarise(grandTotal = sum(total)) %>%
    arrange(desc(grandTotal))
}