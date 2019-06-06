library(stringr)
library(wordcloud)
rawData <- read.csv("C:/Users/nurrencd/Documents/1-Rose-Hulman/MA/386/Datasets/PascalsPensees.csv")

stopwords <- readLines("C:/Users/nurrencd/Documents/1-Rose-Hulman/MA/386/Datasets/stopwords.txt")

stopwords <- stopwords[stopwords!=""]
stopwords <- str_replace_all(stopwords, "[:punct:]","")
stopwords_regex = paste(stopwords, collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')

cleanThoughts <- tolower(str_replace_all(rawData$Thought, pattern=c("_"=" ","[:punct:]"="")))


corpus <- Corpus(VectorSource(cleanThoughts))
corpus <- tm_map(corpus, removeWords, stopwords)
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

fivePercent <- floor(length(allWords)*.05)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=fivePercent, random.color = TRUE)

wordCount <- length(Terms(DTM))
thoughtCount <- length(cleanThoughts)

# search engine
library(tm)
corpus <- Corpus(VectorSource(cleanThoughts))
corpus <- tm_map(corpus, removeWords, stopwords)
DTM <- DocumentTermMatrix(corpus)

presentDTM <- as.matrix(DTM) > 0
occursDTM <- apply(as.matrix(presentDTM), 2, sum)
occursDTM <- 1 + log(thoughtCount/occursDTM)

weights <- matrix(rep(0, thoughtCount * wordCount),nrow = thoughtCount,ncol=wordCount)

for (i in 1:thoughtCount){
  weights[i,] <- as.matrix(DTM)[i,] * occursDTM
  weights[i,] <- weights[i,]/sqrt(sum(weights[i,]^2))
}
colnames(weights) <- Terms(DTM)

createDocument <- function(words){
  words <- tolower(words)
  words <- intersect(words, Terms(DTM))
  newDoc <- matrix(rep(0, wordCount), ncol=wordCount, nrow=1)
  colnames(newDoc) <- Terms(DTM)
  
  wordCount <- as.matrix(table(words))
  if (length(words) > 0){
    normVec <- wordCount[,1]/sqrt(sum(wordCount[,1]^2))
    newDoc[1,rownames(wordCount)] = normVec
  }
  return(as.vector(newDoc))
  
}

searchForDocument <- function(words){
  doc <- createDocument(words)
  results <- c(rep(0, thoughtCount))
  for (i in 1:thoughtCount){
    thought <- weights[i,]
    results[i] <- crossprod(thought, doc)/sqrt(crossprod(thought)  * crossprod(doc))
  }
  finalIndex <- which.max(results)
  return(rawData[finalIndex,])
}
