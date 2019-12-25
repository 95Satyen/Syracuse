#
# Author: Satyen Amonkar
# Purpose: Homework 5

# Reading Source Files
my.dir <- "C:/Users/DELL/Documents/GitHub/Syracuse/Sem3/IST 719/Source/ProjectData/"
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
cn
zomato$Country.Code 

for (dfindex in 1:nrow(zomato)) {
  i <- 1
  while(zomato$Country.Code[dfindex]!= cn$Country.Code[i]){
    i = i+1 }
  zomato$country.Name[dfindex] <-cn$Country[i] 
  }
View(zomato)
## Descriptive plots ##
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
zomato[zomato$country.Name=="India",]
table(zomato$Cuisines[2])
write.csv(zomato[zomato$country.Name=="India",],file = paste0(my.dir, "ZomatoIndia.csv"))
z2<- zomato[zomato$country.Name=="India",]
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

d <- aggregate(z2$Votes, list(z2$Has.Table.booking, z2$City), mean)
names <- d$Group.2[d$Group.1=="Yes"]
z<- d[d$Group.2 %in% names,]
library(ggplot2)
ggplot(z, aes(fill=Group.1, y=x, x=Group.2))  + 
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Restaurant Ratings across cities") + 
  scale_fill_discrete(name = "Has Table Booking")

e <- tapply(z2$Average.Cost.for.two, list(z2$City), mean)
cost<- sort(e,decreasing = TRUE)
library(RColorBrewer)
pal <- colorRampPalette(c("red", "green"))
colred <- pal(43)
pal2 <- colorRampPalette(c("green","red"))
colg <- pal2(43)
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


plot(y=z2$Aggregate.rating,x=z2$Average.Cost.for.two, xlim = c(0,3000), pch="*")
abline(lm(z2$Aggregate.rating ~ z2$Average.Cost.for.two) , col = "brown"
       , lwd = 3, lty = 3)
head(z2)

hist(z2[z2$Aggregate.rating>0,"Aggregate.rating"],
     xlab = "Average Rating of Restaurants",
     xlim = c(1.5,5),
     col = c("#8B0000"),
     main = "Histogram of Zomato Restaurant Avg Ratings")

z2$scores<- round((z2$Votes)/(z2$Average.Cost.for.two+1))
scores<- tapply(z2$scores, list(z2$City), mean)
scores2<- sort(scores, decreasing = TRUE)
barplot(scores2[1:5],
        las=2,
        ylab = "Scores",
        xlab = "city",
        ylim = c(0,max(scores2)),
        cex.axis = 0.5,
        cex.names = 0.6,
        main = "Top 5 cities to invest",
        bty="n",
        col = colg)
boxplot(z2$Price.range,
        xlab = "Price Range",
        main = "Distribution of Price Range",
        pch="$")
