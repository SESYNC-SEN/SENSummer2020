# code rolled into a function


# default cleaning stuff (function names self explanatory)
BDN_words <- BDN_vcorpus %>%
  tm_map(removePunctuation) %>%
  #tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

KJ_words <- KJ_vcorpus %>%
  tm_map(removePunctuation) %>%
  #tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

MT_words <- MT_vcorpus %>%
  tm_map(removePunctuation) %>%
  #tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

MS_words <- MS_vcorpus %>%
  tm_map(removePunctuation) %>%
  #tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)


PPH_words <- PPH_vcorpus %>%
  tm_map(removePunctuation) %>%
  #tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

SJ_words <- SJ_vcorpus %>%
  tm_map(removePunctuation) %>%
  #tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

# stop words!
BDN_words <- BDN_words %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords("english"))

KJ_words <- KJ_words %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords("english"))
MT_words <- MT_words %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords("english"))

MS_words <- MS_words %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords("english"))


PPH_words <- PPH_words %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords("english"))

SJ_words <- SJ_words %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords("english"))
