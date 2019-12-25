#
# Author: Satyen Amonkar
# Purpose: Homework 2

### Part 1 ###

## Question 1: Bar chart- Fig 4-11
hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv"
                    ,sep=","
                    , header=TRUE)
summary(hotdogs)

# Selecting color red for new records(boolean: 1)
fill_colors <- c()
for ( i in 1:length(hotdogs$New.record) ) {
  if (hotdogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

barplot(hotdogs$Dogs.eaten
, names.arg = hotdogs$Year # years on x axis
, col = fill_colors # red for new records
, border=NA # no border
, space=0.3 #spacing between bars
, xlab="Year"
, ylab="Hot dogs and buns (HDB) eaten"
, main = "Nathan's Hot Dog Eating Contest Results, 1980-2010"
, cex.main = 0.8 #adjusting title
, cex.names = 0.7 # adjusting x axis names
, cex.lab=0.8  #adjusting labels
, ylim = c(0,60) # limiting scale
)

## Question 2: Stacked bar chart- Fig 4-22
hot_dog_places <- read.csv('http://datasets.flowingdata.com/hot-dog-places.csv'
                            ,sep=","
                            , header=TRUE)
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", "2004"
                           ,"2005", "2006", "2007", "2008", "2009", "2010")
# rm(hot_dog_matrix)
# rm(hot_dog_places)
head(hot_dog_places) # information of top 3 hot dog eaters
str(hot_dog_places)
hot_dog_matrix <- as.matrix(hot_dog_places) #converting df into matrix
str(hot_dog_matrix)
barplot(hot_dog_matrix
        , border=NA
        , space=0.25
        , ylim=c(0, 200)
        , xlab="Year"
        , ylab="Hot dogs and buns (HDBs) eaten"
        , main="Hot Dog Eating Contest Results, 1980-2010"
        , cex.lab=0.8
        )

## Question 3: Scatterplot- Fig 4-28
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv"
                        , sep=","
                        , header=TRUE)
subscribers[1:5,]
plot(subscribers$Subscribers) #plot subscribers' count
plot(subscribers$Subscribers, type="p", ylim=c(0, 30000))

plot(subscribers$Subscribers
     , type="h"
     , ylim=c(0, 30000)
     , xlab="Day"
     , ylab="Subscribers"
     , cex.axis= 0.8
     , cex.lab=0.7)

points(subscribers$Subscribers, pch=19, col="black")

## Question 4: Time series- Fig 4-34
population <- read.csv("http://datasets.flowingdata.com/world-population.csv"
                       , sep=","
                       , header=TRUE)
head(population)

plot(population$Year
     , population$Population
     , frame.plot = FALSE # removing border in time series
     , type="l" #plot of type line
     , lwd = 3 #line width
     , ylim=c(0, 7000000000)
     , xlab="Year"
     , ylab="Population")

## Question 5: Step chart- Fig 4-43
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv"
                    , sep=","
                    , header=TRUE)
head(postage)

plot(postage$Year, postage$Price
     , type="s"
     , main="US Postage Rates for Letters, First Ounce, 1991-2010"
     , xlab="Year", ylab="Postage Rate (Dollars)"
     , cex.main = 0.9
     , cex.lab = 0.8)

### Part 2 ###
# choose.files()
my.dir <-"C:\\Users\\DELL\\Documents\\GitHub\\Syracuse\\Sem3\\IST 719\\Homework\\HW Week 2\\"
my.file <- "art.csv"
fname <- paste0(my.dir,my.file)

art <- read.csv(fname
                , sep = ","
                , header = TRUE)
str(art)

par(mfrow= c(2,2))
# 1. What is the distribution of total.sale for the whole dataset? 
#    Provide two different plots that show two different ways of showing distribution.
hist(art$total.sale
     , main = "Distribution of Total Sale using Histogram"
     , cex.main = 0.8
     , xlab = "Total Sale"
     , ylab = "Frequency of Total Sale"
     # , col = "#006400"
     , las = 1
     , cex.axis = 0.8
     , cex.lab = 0.8
     , breaks = 20
     , col = "green"
     )

# plot(art$total.sale)
# plot(sort(art$total.sale))
plot(density(art$total.sale)
     , main = "Distribution of Total Sale using Density Plot"
     , cex.main = 0.8
     , ylab = "Denisty of Total Sale"
     , las = 1
     , cex.axis = 0.7
     # , col = "blue"
     , bty = "n"
     , cex.lab = 0.8 
     )
polygon(density(art$total.sale), col = "blue", border = "black")
# 2. Next we want to compare the distributions of subsets of total.sales. 
#    Use a third type of distribution plot (different than what you used for the question above) for both of these plots.
#    What is the distribution of the totals sales for drawing paper?
#    What is the distribution of the totals sales for watercolor paper?
d.totalsales <- art$total.sale[art$paper == "drawing"]
wc.totalsales <- art$total.sale[art$paper == "watercolor"]
boxplot(d.totalsales
        , ylim = c(0,100)
        , xlab = "Drawing Paper"
        , ylab = "Total Sale"
        , cex.lab = 0.8
        , cex.main = 0.8
        , main = "Total Sales for Drawing Paper"
        , pch = "*"
        , col="Red"
        , frame = F
        , cex.axis = 0.7)
boxplot(wc.totalsales
        , ylim = c(0,100)
        , xlab = "Watercolor Paper"
        , ylab = "Total Sale"
        , cex.lab = 0.8
        , cex.main = 0.8
        , main = "Total Sales for Watercolor Paper"
        , pch = "*"
        , col="cadetblue"
        , frame = F
        , cex.axis = 0.7)
# boxplot(art$total.sale )
# ?par()
