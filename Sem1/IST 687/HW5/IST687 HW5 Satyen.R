#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#5
#Date the assignement is due: 10/4/2018
#Date in which the assignment is submitted: 10/3/2018


#Installing required packages
install.packages("RCurl") #used in Option #1 getURL
library(RCurl) #get a checked mark in the packages, ready to use

install.packages("RJSONIO")
library(RJSONIO)

install.packages("sqldf")
library(sqldf)

# Step A: Load the data
# 1	Read in the following JSON dataset
# http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD

jsonLocation <- "http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD"
datasetURL <- getURL(jsonLocation) #to get the url using RCurl
mydata <- fromJSON(datasetURL, simplify = FALSE, nullValue = NA)
myList <- mydata[[2]] # Accessing the data (1st column is metadata)
numRows <- length(myList) #18638
df <- data.frame(matrix(unlist(myList), nrow=numRows, byrow=T), stringsAsFactors = FALSE) #Creating a dataframe in matrix format

#Step B: Clean the data
#2.	Remove the first 8 columns, 
ncol(df) #26
dfnew <- df[,-1:-8]  
ncol(dfnew) #18

#3.	Then, to make it easier to work with, name the rest of the columns as follows:

nameOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK",
                    "ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME",
                    "COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY",
                    "COLLISION_WITH_1","COLLISION_WITH_2")
colnames(dfnew) <- nameOfColumns #Renaming the column names
View(dfnew)

#Step C: Explore the data - using the dataframe you created
#4.	What was the total number of accidents with injuries?
sqldf("Select COUNT(INJURY) FROM dfnew WHERE INJURY ='YES'") #6433

#5.	How many accidents happened on Sunday?
sqldf("Select COUNT(DAY_OF_WEEK) FROM dfnew WHERE DAY_OF_WEEK LIKE'%SUNDAY%'") #2373

#6.	How many injuries occurred each day of the week?
#sqldf("Select DAY_OF_WEEK, COUNT(INJURY) FROM dfnew WHERE INJURY='YES'Group By DAY_OF_WEEK")
sqldf("Select DAY_OF_WEEK, COUNT(INJURY) AS INJURY_COUNT FROM dfnew WHERE INJURY='YES'Group By DAY_OF_WEEK")

#Step D: Explore the data - using dplyr
install.packages("dplyr")
library(dplyr)
df.GroupBydays <- group_by(dfnew, DAY_OF_WEEK)
View(df.GroupBydays)

#7.	What was the total number of accidents with injuries?
accidents.withInjuries <- filter(dfnew, INJURY=="YES")
nrow(accidents.withInjuries) #6433

#8. How many accidents happened on Sunday?
accidents.Sunday <- filter(dfnew, grepl("SUNDAY",DAY_OF_WEEK))
nrow(accidents.Sunday)#2373

#9.	How many injuries occurred each day of the week?
df.GroupBydays <- group_by(accidents.withInjuries, DAY_OF_WEEK)
injuries.eachDay <- summarize(df.GroupBydays, count = n())
View(injuries.eachDay)

#10.In a block comment, explain if you find doing the analysis with the 
#dataframe directly, or using dplyr easier
# deplyr has easier commands to find results and filter rows. However, 
#I am more comfortable with sqldf as I have prior experience in sql.


#Step E: Explore the distribution of the number of vehicles in accidents
#11.	What is the distribution of the number of vehicles in accidents on Friday?
#(use a histogram and quantile)

vehicleNumberF <- sqldf("select VEHICLE_COUNT from dfnew where DAY_OF_WEEK like '%FRIDAY%'")
vehicleNumberF <- na.omit(vehicleNumberF$VEHICLE_COUNT) #assuming na is any number not available
hist(as.numeric(vehicleNumberF)) #Histogram
quantile(as.numeric(vehicleNumberF), probs = c(0.05,0.95)) #Quantile
# The freaquency of accidents on friday is maximum for 2
# vehicles(approx 1200 accidents) followed by 1 vehicle (approx 850 accidents)

#12.	How does this distribution compare with the distribution of the number of vehicles in 
#accidents on Sunday?  (use a histogram and quantile)
vehicleNumberS <- sqldf("select VEHICLE_COUNT from dfnew where DAY_OF_WEEK like '%SUNDAY%'")
vehicleNumberS <- na.omit(vehicleNumberS$VEHICLE_COUNT) #assuming na is any number not available
hist(as.numeric(vehicleNumberS), breaks = 40) #Histogram
quantile(as.numeric(vehicleNumberS), probs = c(0.05,0.95)) #Quantile
#The frequency of number of accidents on Sunday is maximum for 1 vehicle(approx 1200 accidents) 
#followed by 2 vehicles (approx 800 accidents)
#By comparing both Friday and Sunday,frequency of accidents involving single vehicles is more on 
#Sunday as compared to Friday. Also frequency of accidents involving two vehicles is more on Friday as
#compared to Sunday.
