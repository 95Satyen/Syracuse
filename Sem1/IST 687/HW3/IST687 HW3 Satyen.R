#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#3
#Date the assignement is due: 9/20/2018 
#Date in which the assignment is submitted: 9/15/2018

# Step A: Use read.csv( ) and url( ) to read a CSV file form the web into a data frame

#1.	Use R code to read directly from a URL on the web. Store the dataset into a new dataframe, called dfStates. Use stringsAsFactors=FALSE. The URL is: 
#  https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv

link <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
dfStates <- read.csv(url(link))

#dfStates <- read.csv("states.csv", stringsAsFactors = FALSE)

#Step B: Clean the dataframe

#2.	Use View( ), head( ), and tail( ) to examine the data frame. 
View(dfStates)    #Display the states
head(dfStates)    #Display first 6 states
tail(dfStates)    #Display last 6 states
summary(dfStates) #Descriptive Stats
str(dfStates)     #Descriptive Stats

#Step C: Create a Function
#7.	Create a function that takes no parameters and returns the clean dataframe created in step 6 above.

DataCleaning <- function()
{
#3.	Remove unneeded columns and rows by using the minus sign in the rows or columns of the [ , ] accessor.
nrow(dfStates)    #53
dfStates <- dfStates[-1,]  #Removes United States
nrow(dfStates)    #52

#4.	Remove the last Row (for Puerto Rico)
row.number <- nrow(dfStates)
dfStates <- dfStates[-row.number,] #Removes Puerto Rico
#dfStates <- dfStates[-52,]

#5.	Make sure there are exactly 51 rows (one per state + the district of Columbia).  
#Hint: remove Puerto Rico and the summary for the united states
nrow(dfStates)    #51
View(dfStates)
summary(dfStates)

#6.	Make sure there are precisely 4 columns, with the following names:
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
View(states)                       #Displaying clean version of dfStates
nrow(states)                       #51

#Step D: Explore the dataframe

#8.	Calculate the average population of the states
mean(states$population)            #6386651

#9.	Find the state with the highest population  (use which.max)
state.highestpopulation.index <- which.max(states$population)
state.highestpopulation.index           #5 index
states[state.highestpopulation.index, ] #California has the highest population

#10. Create a histogram of the state populations, what do you observe?
hist(states$population, breaks = 20) 
#I observe a histogram with higher frequency in the left side. There are nearly 
#40 states with population under 10 million.

#11. Sort the data frame by population (hint the use 'order' function)
Sorted.States <- states[order(states$population), ]
View(Sorted.States)                     #Sorted based on increasing population

#12. Show the 10 states with the lowest populations
View(Sorted.States[1:10, ])

#13. 13.	Use barplot( ) to create a plot of each of the population from the sorted dataframe.  What do you observe?
barplot(Sorted.States$population)
#I see a bargraph with increasing population towards the right(California having the highest).