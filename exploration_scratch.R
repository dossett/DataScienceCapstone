#scratch exploration
blogTokens <- getTokens(sampleBlog)
counted <- table(unlist(blogTokens))
blogFrame <- as.data.frame(counted)
sum(blogFrame$Freq)
tots <- mutate(mutate(mutate(arrange(blogFrame, desc(Freq)), perc = Freq/3648378), cumFreq = cumsum(perc)), rownum = row_number())
head(filter(tots, cumFreq >= .5))

#Given a vector of vectors (i.e. a vector of sentences)
# generate all 3-grams from each sentence
makeAll3Grams <- function(x, n=100)
{
  if (length(x) <= n)
    return (makeAll3GramHelper(x, data.table()))
  else
    return (rbind(makeAll3Grams(x[1:n]), makeAll3Grams(x[-n])))
}

makeAll3GramHelper <- function(x, table)
{
  if (length(x) > 0)
    return (makeAll3GramHelper(x[-1], make3GramHelper(x[[1]], table)))
  else
    return (table)
}

#Takes a vector of strings, returns all n-grams (includes duplicates)
#Returns a data.table with n1 = first, n2 = second, n3 = third word
#vector with length < 3 returns empty

makeAll3GramLoop <- function(x)
{
  gramTable = data.table()
  LL <- vector()
  for (i in 1:length(x))
  {
    #print(paste("Working on ", x[i]))
    #print(make3Gram(x[i]))
    #gramTable <- rbindlist(list(gramTable, make3Gram(x[[i]])), use.names = T, fill = F)
    LL[i] <- make3Gram(x[[i]])
  }
  return(rbindlist(list(LL), use.names=T, fill=F))
}

