#
# Author: Satyen Amonkar
# Purpose: Homework 4



## Question 1: Bar chart- Fig 4-5
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
        , ylim = c(0,70) # limiting scale
        , las = 1 
        )

  ## Question 2: Stacked bar chart- Fig 4-21
hot_dog_places <- read.csv('http://datasets.flowingdata.com/hot-dog-places.csv'
                           ,sep=","
                           , header=TRUE)
names(hot_dog_places) <- c("'00", "'01", "'02", "'03", "'04"
                           ,"'05", "'06", "'07", "'08", "'09", "'10")
# rm(hot_dog_matrix)
# rm(hot_dog_places)
head(hot_dog_places) # information of top 3 hot dog eaters
str(hot_dog_places)
hot_dog_matrix <- as.matrix(hot_dog_places) #converting df into matrix
str(hot_dog_matrix)
barplot(hot_dog_matrix
        # , border=NA
        , space=0.25
        , ylim=c(0, 200)
        , xlab="Year"
        , ylab="Hot dogs and buns (HDBs) eaten"
        , main="Hot Dog Eating Contest Results, 1980-2010"
        , cex.lab=0.8
        # , las = 1
)

  ## Question 3: Scatterplot- Fig 4-25
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv"
                        , sep=","
                        , header=TRUE)
subscribers[1:5,]


plot(subscribers$Subscribers
     , type="p"
     , ylim=c(0, 30000)
     , xlab="Day"
     , ylab="Subscribers"
     , cex.axis= 0.8
     , cex.lab=0.7
     , las = 1)


## Question 4: Time series- Fig 4-40
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
     , ylab="Population"
     ,las = 1)

  ## Question 5: Step chart- Fig 4-42
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

## Question 6: LOESS Curve Fig: 4-47
unemployment <-  read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv"
                          ,    sep=",")

unemployment[1:10,]
# Plain scatter plot
plot(1:length(unemployment$Value), unemployment$Value)
scatter.smooth(x=1:length(unemployment$Value),
               y=unemployment$Value, ylim=c(0,11), degree=2, col="#CCCCCC", span=0.5
               ,las = 1)
