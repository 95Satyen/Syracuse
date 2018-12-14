#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#11
#Date the assignement is due: 11/29/2018 
#Date in which the assignment is submitted: 11/21/2018

#Part A: Load and condition the text file that contains the speech

#1.	The data is available on blackboard, as a JSON file (see HW8 if you need a reminder on the dataset or how to load the dataset).
install.packages("RJSONIO")
library(RJSONIO)

dataset.name <- "hotelSurveySherison.json" #set dataset name
hotelSurveyOut<- fromJSON(dataset.name,simplify = TRUE, nullValue = NA) # covert JSON content into R objects
hotelSurvey <- data.frame(hotelSurveyOut, stringsAsFactors = FALSE) # convert R object into Data Frame
View(hotelSurvey)

#2.	The key column to focus on is the 'freeText' column.
View(hotelSurvey$freeText)

#scan the file and read the content into "positive"
pos<-"positive-words.txt"
positive <- scan(pos,character(0), sep="\n")
positive<-positive[c(-1:-34)] # Removing redundant records 
summary(positive)
View(positive)

neg <- "negative-words.txt"
negative <- scan(neg,character(0), sep="\n")
negative<-negative[c(-1:-34)] # Removing redundant records
summary(negative)
View(negative)

#Part B: Create a list of word counts from the speech

install.packages("tm")
install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)
library(tm)


createWordCounts <- function(vFreeText) {
  words.vec <- VectorSource(vFreeText)
  words.corpus <- Corpus(words.vec) #create a big bag of words, i.e. corpus
  words.corpus <- tm_map(words.corpus, content_transformer(tolower)) #converting words to lower case
  words.corpus <- tm_map(words.corpus, removePunctuation) #removing punctuations
  words.corpus <- tm_map(words.corpus, removeNumbers) #removing numbers
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english")) #removing stopwords
  
  tdmatrix <- TermDocumentMatrix(words.corpus)
  
  # create a list of counts for each word
  m<- as.matrix(tdmatrix)
  wordCounts <- rowSums(m)
  wordCounts <- sort(wordCounts, decreasing = TRUE)
  return(wordCounts)
}

wordCounts<-createWordCounts(hotelSurvey$freeText) #wordCounts for each word
View(wordCounts)

#3.	Starting with the code at the bottom of page 180 in the text book, use a similar approach to transform the free text into a term document matrix, 
#and then determine positive and negative word matches.

getMatched <- function(wordCounts,PosorNeg)
{
  words<- names(wordCounts)
  
  matched<-match(words,PosorNeg,nomatch=0)
  return(matched)
}


matchedP<-getMatched(wordCounts,positive)
matchedN<-getMatched(wordCounts,negative)

View(matchedP)
View(matchedN)

#4.	Calculate the percent positive words and negative words.

CalcPosNeg<- function(wordCounts,matchedP, matchedN)
{
  pTotal<- sum(wordCounts[which(matchedP!=0)])
  View(pTotal)
  nTotal<- sum(wordCounts[which(matchedN!=0)])
  View(nTotal)
  totalWords <- sum(wordCounts)
  retVal <- c(pTotal, pTotal/totalWords,nTotal, nTotal/totalWords)
  return(retVal)
} 

ret<-CalcPosNeg(wordCounts,matchedP,matchedN)
ret
cat("num pos words (%)",ret[1]," ",ret[2],"num of neg words (%)",ret[3]," ",ret[4])

#5.	Write a block comment that summarizes what you learned from ratioPos and ratioNeg.
# The ratio of positive words is more than that or negative words, i.e more connections are positive than negative.

#Part C: Visualize the results
#6.	Create a word cloud

genWordCloud<-function(wordCounts){
  cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
  wordcloud(names(wordCounts),wordCounts,min.freq=2,max.words=30,rot.per=0.35,random.order=FALSE,colors=brewer.pal(8,"Dark2"))
}

genWordCloud(wordCounts)

#7.	Create a barplot of the positive and negative words that matched (at least twice)

genBarChart<-function(wordCounts,matched){
  sortedWords<-sort(wordCounts[matched>1])
  barplot(sortedWords,las=2,cex.names=0.75)
}

genBarChart(wordCounts,matchedP)
genBarChart(wordCounts,matchedN)

#8.	Write a block comment on what you observe from these two barplots and the wordcloud. 
#barplots are easier to understand than wordcloud.

#9.	Does these results make sense to you in terms of the kinds of emotions you see?
#Many, but not all the results make sense in terms of the kinds of emotions I see.for ex: happy, smile make sense while upgraded doesn't.
#Which do you think is more informative - barplot or the wordcloud?
#Barplots is more informative as it provides the frequency of the words. 
#By comparing the words using barplots, we can know the difference between the frequencies more clearly.

#Part D: Evaluate Happy and not Happy customer responses
#10.	Create two subset of the text vectors: one for happy customers and one for not happy customers 
#(based on overall customer satisfaction)
hotelSurveylow<- hotelSurvey[hotelSurvey$overallCustSat<7,]
wordCountsl<-createWordCounts(hotelSurveylow$freeText)

hotelSurveyhigh<- hotelSurvey[hotelSurvey$overallCustSat>=7,]
wordCountsh<-createWordCounts(hotelSurveyhigh$freeText)

#11.	Redo Steps B, C & D, for these two subsets of the text strings.

matchedlP <- getMatched(wordCountsl,positive)
matchedlN <- getMatched(wordCountsl,negative)
retl <- CalcPosNeg(wordCountsl, matchedlP, matchedlN)
cat("num pos words (%)",retl[1],"",retl[2],"num neg words (%)",retl[3], retl[4])
genWordCloud(wordCountsl)
genBarChart(wordCountsl,matchedlP)
genBarChart(wordCountsl,matchedlN)

matchedhP <- getMatched(wordCountsh,positive)
matchedhN <- getMatched(wordCountsh,negative)
reth <- CalcPosNeg(wordCountsh, matchedhP, matchedhN)
cat("num pos words(%)",reth[1],"",reth[2],"num neg words",reth[3], reth[4])
genWordCloud(wordCountsh)
genBarChart(wordCountsh,matchedhP)
genBarChart(wordCountsh,matchedhN)

#12.	Compare the positive and negative ratios for these two different group of customers 
#The group with lower overall customer satisfaction has more negative words compared to positive.
#The group with high overall customer satisfaction has more positive words as compared to negative.