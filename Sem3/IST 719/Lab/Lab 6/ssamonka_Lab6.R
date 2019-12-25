#
# Author: Satyen Amonkar
# Purpose: Lab 6: layout and date-time
#


x<- 1:24
y <- rnorm(length(x))
A<- runif(n=length(x), min = 2, max=9)
A<- A + y^2
B<- sample(c("D","W")
           ,size = length(x), replace = T)

M <- matrix( #matrix to set layout what we want
  c(1,1,3,
  1,4,3,
  2,2,3),
  nrow = 3,
  byrow = T
)

my.dir <- "h:/Desktop/IST 719/"
pdf(file=paste0(my.dir,"funnyplot.pdf") #
    , width = 8.5, height = 11)
layout(M) #setting layout
?layout
# layout.show(3)

par(mar = c(0,4,4,2), bty="n") #utilizing all the space
plot(x,y, type="n", xaxt = "n", ylab = "" #xaxt turns of x axis
     , xlab = ""
     , ylim = c(2*min(y), 2*max(y)))
lines(x,y, type="l", col="purple", lwd = 2)
mtext(text = "Hourly Rate", side = 3, line = 1
      , cex = 1.3)
mtext("volume",side=2, line = 2)
par(mar=c(5,4,0,2))
barplot(A,names.arg = x, col = "purple",
        border = NA)
par(mar=c(4,4,4,4), bty = "n")
boxplot(A~B, col="purple")

pie(table(B), col=c("yellow","brown"))


dev.off() #closing  the file

fname <- paste0(my.dir,"ClimateTweets_June2016.csv")

tweets <- read.delim(fname,quote="\""
                     , header = TRUE, sep = ","
                     , stringsAsFactors = FALSE)
dim(tweets)
View(tweets)

table(tweets$media)
#need some data cleaning

my.media <- tweets$media
my.media[my.media ==""] <-"text"

my.media <- gsub("photo\\|photo","photo", my.media)
table(my.media)

barplot(table(my.media))

tweets$created_at
my.conv.str<- "%a %b %d %H:%M:%S +0000 %Y" #+0000 is some standard text so we include string here to add the wierd stuff.. make sure the conversion string is right 
# my.date <- as.POSIXct(strptime(tweets$created_at
#                                , my.conv.str))
my.date <- as.POSIXct(strptime(tweets$created_at
                               , my.conv.str))
class(my.date)
?strptime #use this to find codes and conversion

length(my.date)
class(my.date) # these are unix names for date formats with which we can use functions
min(my.date)
max(my.date)

range(my.date)
summary(my.date)

library(lubridate)
barplot(table(hour(my.date)))
barplot(table(day(my.date)))

difftime(max(my.date), min(my.date), units = "hours") #comparing hours
difftime(max(my.date), min(my.date), units = "days")  #comparing days
