#scratch exploration
blogTokens <- getTokens(sampleBlog)
counted <- table(unlist(blogTokens))
blogFrame <- as.data.frame(counted)
sum(blogFrame$Freq)
tots <- mutate(mutate(mutate(arrange(blogFrame, desc(Freq)), perc = Freq/3648378), cumFreq = cumsum(perc)), rownum = row_number())
head(filter(tots, cumFreq >= .5))
