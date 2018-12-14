#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#7
#Date the assignement is due: 10/18/2018
#Date in which the assignment is submitted: 10/17/2018

#Step A: Load and Merge datasets
#1)	Read in the census and the USArrests datasets and merge them.(just like HW6)

#Read in the census dataset (using the function created in HW 3)
link <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
dfStates <- read.csv(url(link))
View(dfStates)
#dfStates <- read.csv("states.csv", stringsAsFactors = FALSE)


DataCleaning <- function()
{
  #Remove unneeded columns and rows by using the minus sign in the rows or columns of the [ , ] accessor.
  nrow(dfStates)    #53
  dfStates <- dfStates[-1,]  #Removes United States
  nrow(dfStates)    #52
  
  #Remove the last Row (for Puerto Rico)
  row.number <- nrow(dfStates)
  dfStates <- dfStates[-row.number,] #Removes Puerto Rico
  #dfStates <- dfStates[-52,]
  
  #Make sure there are exactly 51 rows (one per state + the district of Columbia).  
  #Hint: remove Puerto Rico and the summary for the united states
  nrow(dfStates)    #51
  View(dfStates)
  summary(dfStates)
  
  #	Make sure there are precisely 4 columns, with the following names:
  #stateName, population, popOver18, percentOver18. 
  #Hint: use colnames( ) and you will need to remove some columns
  dfStates <- dfStates[,-1:-4]       #Remove unwanted columns 
  View(dfStates)
  colnames(dfStates)                 #Display existing columns
  colnames(dfStates) <- c("stateName", "population", "popOver18", "percentOver18") #rename the columns
  colnames(dfStates)                 #Display new columns
  
  return(dfStates)                   #return clean version of dfStates 
}

nrow(dfStates)                     #Rows before running the function
states <- DataCleaning()           #Calling function with no paramters

View(states)
str(states)

#	Copy the USArrests dataset into a local variable (similar to HW 2)
arrests <- USArrests
View(arrests)
str(arrests)

#	Create a merged dataframe -- with the attributes from both dataset
#Hint: get the state names from the USArrests dataset with the rownames() 
#Hint: use the merge() command 
rownames(arrests)
#rownames(dfStates)
#colnames(arrests)
#colnames(states)

#merge.dataframe <- merge(arrests,states,by.x="row.names" ,by.y = "stateName") ~ alternative
arrests$stateName<-rownames(arrests)
merge.df <- merge(arrests,states,by="stateName") #50


#2)	 Create a new Data frame that has the area of each state (state.area), and the center of each 
#state (state.center), and then merge (by stateName) it with your final data frame in step #1. 
library(maps)


area <- state.area
center <- state.center
stateName<- state.name
otherDF <- data.frame(stateName,area, center)
View(otherDF)

merge.df <- merge(merge.df, otherDF,by="stateName")
merge.df$popPerArea <- merge.df$population / merge.df$area
merge.df$stateName <- tolower(merge.df$stateName)



#Step B: Generate a color coded map
# 3)	Create a color coded map, based on the area of the state 
library(maps)
library(ggmap)
#get data on state to be mapped using ggplot2
us <- map_data("state")  
mapPlot <- ggplot(merge.df, aes(map_id=stateName))
mapPlot <- mapPlot + geom_map(map=us, aes(fill=merge.df$area)) #fill the map based on area
#mapPlot<- mapPlot + expand_limits(x= merge.df$x, y=merge.df$y) ~ alternative
mapPlot<- mapPlot + expand_limits(x= us$long, y=us$lat) 
#mapPlot <- mapPlot + coord_map() #not necessary 
mapPlot

#Step C: Create a color shaded map of the U.S. based on the Murder rate for each state 
#4)	Repeat step B, but color code the map based on the murder rate of each state.
us <- map_data("state")  
mapPlot2 <- ggplot(merge.df, aes(map_id=stateName))
mapPlot2 <- mapPlot2 + geom_map(map=us, aes(fill=merge.df$Murder)) #fill the map based on murder rate.
mapPlot2<- mapPlot2 + expand_limits(x= us$long, y=us$lat)
mapPlot2 <- mapPlot2 + ggtitle("Murder Per State")
mapPlot2

#5)	 Show the population as a circle per state (the larger the population, the larger the circle),
#using the location defined by the center of each state
mapPlot3 <- ggplot(merge.df, aes(map_id=stateName))
mapPlot3 <- mapPlot3 + geom_map(map=us, aes(fill=merge.df$Murder)) #fill the map based on murder rate.
mapPlot3<- mapPlot3 + expand_limits(x= us$long, y=us$lat)
mapPlot3 <- mapPlot3 + ggtitle("Population and Murder Per State")
mapPlot3 <- mapPlot3 + geom_point(data=merge.df, aes(x=merge.df$x, y=merge.df$y, size=merge.df$population))
mapPlot3 <- mapPlot3 + coord_map()
mapPlot3


#Step D: Zoom the map
#6)	Repeat step C, but only show the states in the north east
#Hint: get the lat and lon of new york city
#Hint: set the xlim and ylim to NYC +/- 10
latlon <- geocode(source="dsk","nyc, new york, ny")
latlon #longitude and latitude for nyc.
mapPlot4 <- ggplot(merge.df, aes(map_id=stateName))
mapPlot4 <- mapPlot4 + geom_map(map=us, aes(fill=merge.df$Murder)) #fill the map based on area
mapPlot4<- mapPlot4 + expand_limits(x= us$long, y=us$lat)
mapPlot4 <- mapPlot4 + ggtitle("Population and Murder Per State")
mapPlot4 <- mapPlot4 + geom_point(data=merge.df, aes(x=merge.df$x, y=merge.df$y, size=merge.df$population))
mapPlot4 <- mapPlot4  +  xlim(c((latlon$lon-10),(latlon$lon+10))) + ylim(c((latlon$lat-10),(latlon$lat+10)))
mapPlot4
                                                                               