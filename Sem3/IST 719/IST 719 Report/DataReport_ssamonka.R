#
# Author: Satyen Amonkar
# Purpose: IST 719 Final Project Report

# Reading Source Files
# my.dir <- "C:/Users/DELL/Documents/GitHub/Syracuse/Sem3/IST 719/Source/ProjectData/"
my.dir <- "C:/Users/satye.MSI/Desktop/"

fname <- paste0(my.dir, "zomato.csv")

zomato <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)

# class(zomato)

NumberOfRows <- nrow(zomato) #9551 Records
NumberOfColumns  <- ncol(zomato) #21 columns

DatasetScore <- (NumberOfColumns * 4) * (NumberOfRows/100)
DatasetScore # Score 8022.84

if(DatasetScore >= 100 ){
  print("Valid Score")
} else {
  print("Invalid Score")
}

str(zomato)

# We do not have country name in data set
# install.packages('xlsx')
library(xlsx)
fname2 <- paste0(my.dir, "Country-Code.xlsx")
cn <- read.xlsx2(fname2, sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE)
head(cn)
head(zomato$Country.Code )
for (dfindex in 1:nrow(zomato)) {
  i <- 1
  while(zomato$Country.Code[dfindex]!= cn$Country.Code[i]){
    i = i+1 }
  zomato$country.Name[dfindex] <-cn$Country[i] 
  }
View(zomato)
########## Exploratory Analysis #################################
par(mar=c(5,6,2,2)) #setting margins c(bottom, left, top, right) 


# Distribution of Country
countryfreq<- table(zomato$country.Name)
names(countryfreq)
colFunc <- colorRampPalette(c("red","blue", "gold"))
my.cols <- colFunc(length(countryfreq))

barplot(countryfreq, col=my.cols,
        las=1,
         horiz = TRUE,
        border = "tan",
        # xlab = "Country Name",
        cex.axis = 0.7,
        cex.names = 0.6,
        xlab = "Frequency",
        xlim = c(0, max(countryfreq)),
        main="Country Distribution"
         )
# Distribution of price range
plot(density(zomato$Price.range))
boxplot(zomato$Price.range,
        xlab = "Price Range",
        main = "Distribution of Price Range",
        pch="$")
abline(h = median(zomato$Price.range), lty = 2, col = "red")

# Distribution of avg rating
hist(zomato[zomato$Aggregate.rating>0,"Aggregate.rating"],
     xlab = "Average Rating of Restaurants",
     col = c("#8B0000"),
     main = "Histogram of Zomato Restaurant Avg Ratings")

# Multidimensional plots
a <- tapply(zomato$Votes, list(zomato$country.Name), mean)
a
par(mar=c(7,5,2,1))

barplot(a,
        las=2,
        # xlab = "Country",
        ylab = "Average Votes",
        main = "Avg votes for restaurants in each country",
        ylim = c(0,max(a)),
        cex.names = 0.8,
        col = my.cols)
mtext(text = "Country",
      side = 1, 
      line = 5)

par(mar=c(7,4,2,4))
b <- tapply(zomato$Aggregate.rating, list(zomato$Has.Table.booking, zomato$country.Name), mean)
barplot(b, beside = TRUE,
        las = 2,
        legend.text = row.names(b),
        col = c("Red","green"),
        args.legend = list(x="topright",inset=c(-0.12,0.12),xpd = TRUE),
        bty = "n",
        cex.names = 0.7,
        ylab = "Avg Rating",
        main="Avg Rating for Restaurants across Countries ")
mtext(text = "Country",
      side = 1, 
      line = 5)

table(zomato$Cuisines[2])

######### Exploratory Analysis of Indian Restaurants ################
zomato[zomato$country.Name=="India",]
z2<- zomato[zomato$country.Name=="India",]
library(RColorBrewer)
pal <- colorRampPalette(c("red", "green"))
colred <- pal(43)
pal2 <- colorRampPalette(c("green","red"))
colg <- pal2(43)



c <- tapply(z2$Aggregate.rating, list(z2$City), mean)
Rat <-sort(c, decreasing = TRUE)
barplot(Rat,
        las=2,
        ylab = "Average Rating",
        ylim=c(0,5),
        xlab = "city",
        cex.axis = 0.5,
        cex.names = 0.6,
        main = "Restaurant ratings across cities in India",
        bty="n",
        col=colg)
# cbind(z2$Aggregate.rating, z2$Price.range)
rating<-aggregate(z2$Aggregate.rating, list(z2$City),mean)
colnames(rating) <- c("city","rating")
pr<- aggregate(z2$Price.range,list(z2$City),mean)
colnames(pr)<- c("city","price.range")
library(plyr)
ratprt<- merge(pr,rating, by.x = "city", by.y = "city")
# ratprt<- arrange(ratprt, rating, decreasing = TRUE)
ratprt <- ratprt[order(ratprt[,3], decreasing = TRUE),]
x<- barplot(ratprt[,3],names.arg=ratprt[,1], las =2,
        ylab = "Average Rating",
        ylim=c(0,5),
        xlab = "city",
        cex.axis = 0.5,
        cex.names = 0.5,
        main = "Restaurant ratings across cities in India",
        bty="n",
        col=colg,)
lines(x=x, y=ratprt[,2])

d <- aggregate(z2$Votes, list(z2$Has.Table.booking, z2$City), mean)
names <- d$Group.2[d$Group.1=="Yes"]
z<- d[d$Group.2 %in% names,]
library(ggplot2)
ggplot(z, aes(fill=Group.1, y=x, x=Group.2))  + 
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Restaurant Ratings across cities") + 
  scale_fill_discrete(name = "Has Table Booking")


d <- aggregate(z2$Votes, list(z2$Has.Online.delivery, z2$City), mean)
names <- d$Group.2[d$Group.1=="Yes"]
z<- d[d$Group.2 %in% names,]
library(ggplot2)
ggplot(z, aes(fill=Group.1, y=x, x=Group.2))  + 
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Restaurant Ratings across cities") + 
  scale_fill_discrete(name = "Has Online Booking")

e <- tapply(z2$Average.Cost.for.two, list(z2$City), mean)
cost<- sort(e,decreasing = TRUE)
barplot(cost,
        las=2,
        ylab = "Average Cost for 2",
        xlab = "city",
        cex.axis = 0.5,
        cex.names = 0.6,
        main = "Average cost of 2 across cities in India",
        bty="n",
        col = colred)

f <- tapply(z2$Price.range, list(z2$City), mean)
pr<- sort(f,decreasing = TRUE)



barplot(pr,
        las=2,
        ylab = "Average Cost for 2",
        xlab = "city",
        cex.axis = 0.5,
        cex.names = 0.6,
        main = "Price Range across cities in India",
        bty="n",
        col = colred)


plot(y=z2$Aggregate.rating,x=z2$Average.Cost.for.two, xlim = c(0,3000))
abline(lm(z2$Aggregate.rating ~ z2$Average.Cost.for.two) , col = "brown"
       , lwd = 3, lty = 3)
head(z2)

hist(z2[z2$Aggregate.rating>0,"Aggregate.rating"],
     xlab = "Average Rating of Restaurants",
     xlim = c(1.5,5),
     col = c("#8B0000"),
     main = "Histogram of Zomato Restaurant Avg Ratings")


boxplot(z2$Price.range,
        xlab = "Price Range",
        main = "Distribution of Price Range",
        pch="$")


### adding population ###
fnamep <- paste0(my.dir, "population.csv")
pop<- read.csv(fnamep, stringsAsFactors = FALSE, header = TRUE)
z2[z2$City == "Bhubaneshwar","City"] <- "Bhubaneswar"
z3<-merge(pop,z2, by.x= "City", by.y = "City")
a<- unique(z3$City)
b<- unique(z2$City)
ind <-b %in% a
b[!ind]
colnames(z3)

## Calculating top 5 cities to invest using weights
z3<-z3[,c("City","Population","Votes","Average.Cost.for.two","Aggregate.rating")]
z3[,c("Population","Votes","Average.Cost.for.two","Aggregate.rating")]<- scale(z3[,c("Population","Votes","Average.Cost.for.two","Aggregate.rating")])
z3$scores<- round(z3$Population*0.2+z3$Votes*0.2+z3$Average.Cost.for.two*0.4+z3$Aggregate.rating*0.4)
scores<- tapply(z3$scores, list(z3$City), mean)
scores2<- sort(scores, decreasing = TRUE)
barplot(scores2[1:5],
        las=2,
        ylab = "Scores",
        xlab = "city",
        ylim = c(0,max(scores2)),
        cex.axis = 0.5,
        cex.names = 0.7,
        main = "Top 5 cities to invest",
        bty="n",
        col = colg)

### Map function ###
range(zclean$Longitude)

fix(z2)
z2[z2$Longitude==0,"City"]
ind <- which(z2$Longitude==0)
zclean<- z2[-ind,]
ind2 <- which(zclean$Longitude<60)
zclean<- zclean[-ind2,]
write.csv(zclean,file = paste0(my.dir, "ZomatoIndia.csv"))


# 
# install.packages("mapproj")
# library(maps)
# require(maps)
# map("world","india")
# map.cities(country = "India", capitals = 1)
# map.cities(zomato)
# data("us.cities")
fname4 <- paste0(my.dir, "ZomatoIndia.csv")
zomatoClean <- read.csv(fname4, header = TRUE, stringsAsFactors = FALSE)
# 
# density<- ddply(zomatoClean, "City", "nrow")
# names(density)[2] <- "count"
# zomatoClean <- merge(zomatoClean, density)



install.packages('ggmap')
library(ggmap)
library(ggplot2)
register_google("AIzaSyDc88u8_AUpVqLKynKQGhyJRWqvKS4gw18")
Ind <- as.numeric(geocode("India"))
ggmap(get_googlemap(center=Ind, scale=2, zoom=5), extent="normal") +
  geom_point(aes(x=Longitude, y=Latitude), data=zomatoClean, col="cadetblue")



### Top Cuisine ##
z2$Cuisines

install.packages('qdap')
install.packages('tm')
install.packages('textstem')
install.packages('quanteda')
library('qdap')
library('tm')
library('textstem')
library('quanteda')
cF<- Corpus(VectorSource(Kolk$Cuisines))
cF<- tm_map(cF,tolower)
cF <- tm_map(cF, removePunctuation)
cF <- tm_map(cF, removeWords, stopwords("english"))
cF <- tm_map(cF, removeNumbers)

Bang<-z2[z2$City=="Bangalore",c("Cuisines","Aggregate.rating")]
Bang<- Bang[Bang$Aggregate.rating>3,]
str(Bang)
Kolk<-z2[z2$City=="Kolkata",c("Cuisines","Aggregate.rating")]
Kolk<- Kolk[Kolk$Aggregate.rating>3,]
Hyd<-z2[z2$City=="Hyderabad",c("Cuisines","Aggregate.rating")]
Hyd<- Hyd[Hyd$Aggregate.rating>3,]
Mu<-z2[z2$City=="Mumbai",c("Cuisines","Aggregate.rating")]
Mu<- Mu[Mu$Aggregate.rating>3,]
C<-z2[z2$City=="Chennai",c("Cuisines","Aggregate.rating")]
C<- C[C$Aggregate.rating>3,]

stem_word <- lemmatize_words(cF, dictionary = lexicon::hash_lemmas)
freq_terms(stem_word, 20)

bigram<- tokens(Kolk$Cuisines)%>%
  tokens_remove("\\p{P}", valuetype="regex", padding = TRUE) %>%
  tokens_ngrams(n=2) %>%
  dfm()
topfeatures(bigram,20)

### Most Popular Cuisine Wordcloud ###

cF1<- Corpus(VectorSource(z2$Cuisines))
cF1<- tm_map(cF1,tolower)
cF1 <- tm_map(cF1, removePunctuation)
cF1 <- tm_map(cF1, removeWords, stopwords("english"))
cF1 <- tm_map(cF1, removeNumbers)
stem_word1 <- lemmatize_words(cF1, dictionary = lexicon::hash_lemmas)
freq_terms(stem_word1, 20)
tdm<- TermDocumentMatrix(stem_word1)
tdmatrix<-as.matrix(tdm)
colT<- apply(tdmatrix, 2, sum)
tdmclean<- tdmatrix[,colT>0]
tdfreq<- rowSums(tdmclean)
tdfreq<- sort(tdfreq,decreasing = T)
tdfreq[1:20]
wordfreq<- data.frame(term=names(tdfreq),num=tdfreq)
library('wordcloud')
wordcloud(wordfreq$term, wordfreq$num, max.words=50, colors=c("aquamarine","darkgoldenrod","tomato"))
