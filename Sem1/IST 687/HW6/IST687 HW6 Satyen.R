#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#6
#Date the assignement is due: 10/11/2018
#Date in which the assignment is submitted: 10/10/2018

#Step A: Load and Merge datasets

#1)	Read in the census dataset (using the function created in HW 3)
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

#2)	Copy the USArrests dataset into a local variable (similar to HW 2)
arrests <- USArrests
View(arrests)
str(arrests)

#3)	Create a merged dataframe -- with the attributes from both dataset
#Hint: get the state names from the USArrests dataset with the rownames() 
#Hint: use the merge() command 
rownames(arrests)
#rownames(dfStates)
#colnames(arrests)
#colnames(states)

#merge.dataframe <- merge(arrests,states,by.x="row.names" ,by.y = "stateName") ~ alternative
arrests$stateName<-rownames(arrests)
merge.df <- merge(arrests,states,by="stateName") #50
#Step B: Explore the Data - Understanding distributions

#4)	Create a histogram using ggplot2() for the population and a different histogram for the murder rate

#Hint: Don't forget to install and library the ggplot2 package.

#Ensure each line of code is explained (comments) in terms of what it is doing. Then build similar code to 
#create histograms of each of the other three variables in the merged data frame. 
#What parameter will you have to adjust to make the other histograms look right? <- bandwidth or bins needs to be adjusted
install.packages("ggplot2")
library(ggplot2)
merge.df$population #display population
#ggplot(merge.dataframe, aes(x=population)) + geom_histogram(bandwidth=5000000, color="black", fill="white") + ggtitle("Histogram of population")

#Histogram of Population
myPlot <- ggplot(merge.df, aes(x=population)) #adding aesthetics 
myPlot <- myPlot + geom_histogram(binwidth=5000000, color="black", fill="white") #Creates histogram 
myPlot<- myPlot + ggtitle("Histogram of population") #adding description
myPlot
str(merge.df)
summary(merge.df)

#Histogram of Murder
myPlot2 <- ggplot(merge.df, aes(x=Murder))
myPlot2 <- myPlot2 + geom_histogram( color="black", fill="red", bins=5)
myPlot2 <- myPlot2 + ggtitle("Histogram of Murder")
myPlot2

#Histogram of Assault  
myPlot3 <- ggplot(merge.df, aes(x=Assault))
myPlot3 <- myPlot3 + geom_histogram(color="black", fill="red", bins=20) 
myPlot3 <- myPlot3 + ggtitle("Histogram of Assault")
myPlot3

#Histogram of Rape
myPlot4 <- ggplot(merge.df, aes(x=Rape))
myPlot4 <- myPlot4 + geom_histogram(color="black", fill="red", bins=10)
myPlot4 <- myPlot4 + ggtitle("Histogram of Rape")
myPlot4
  
#Histogram of Urban Population
myPlot5 <- ggplot(merge.df, aes(x=UrbanPop))
myPlot5 <- myPlot5 + geom_histogram(color="black", bins=25) 
myPlot5 <- myPlot5 + ggtitle("Histogram of Urban Population")
myPlot5

#5)	Create a boxplot for the population, and a different boxplot for the murder rate.

#Boxplot for Population
myBoxPlot1 <- ggplot(merge.df, aes(x=factor(0), y=population)) # factor is used as only one axis is used
myBoxPlot1 <- myBoxPlot1 + geom_boxplot() #Creates box-and-whisker plot of the given
myBoxPlot1 <- myBoxPlot1 + ggtitle("Boxplot for Population")
myBoxPlot1

#Boxplot for Murder Rate
myBoxPlot2 <- ggplot(merge.df, aes(x=factor(0),y=Murder))
myBoxPlot2 <- myBoxPlot2 + geom_boxplot()
myBoxPlot2 <- myBoxPlot2 + ggtitle("Boxplot for Murder Rate")
myBoxPlot2

#6)	Create a block comment explaining which visualization (boxplot or histogram) you thought was more helpful (explain why)
# I believe, histogram is more helpful as histogram is more visually apealing. Histogram is more clearer to uderstand 
# for a layman.   

#Step C: Which State had the Most Murders - bar charts
#7)	Calculate the number of murders per state
merge.df$numMurders <- (merge.df$population * merge.df$Murder) / 100000 #Since Murder is numeric value per 100,000 
merge.df$numMurders

#8)	Generate a bar chart, with the number of murders per state
#Hint: use the geom_col() function
myBarPlot <- ggplot(merge.df, aes(x=stateName, y=numMurders))
myBarPlot <- myBarPlot + geom_col() #Creates bar chart where heights of the bars represent values in data
myBarPlot <- myBarPlot + ggtitle("Number of murders per state")
#myBarPlot <- myBarPlot + labs(x="State Name", y="Number of Murders" ) ~ Renaming labels
myBarPlot

#9) Generate a bar chart, with the number of murders per state. 
#Rotate text (on the X axis), so we can see x labels, also add a title named "Total Murders".
myBarPlot2 <- ggplot(merge.df, aes(x=stateName, y=numMurders))
myBarPlot2 <- myBarPlot2 + geom_col()
myBarPlot2 <- myBarPlot2 + ggtitle("Total Murders")
myBarPlot2 <- myBarPlot2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate text on X axis
myBarPlot2

#10) Generate a new bar chart, the same as in the previous step, but also sort the x-axis by the number of murders
myBarPlot3 <- ggplot(merge.df, aes(x=stateName, y=numMurders))
myBarPlot3 <- myBarPlot3 + geom_col()
myBarPlot3 <- myBarPlot3 + ggtitle("Total Murders")
myBarPlot3 <- myBarPlot3 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate text on X axis
myBarPlot3 <- myBarPlot3 + aes(x= reorder(stateName, numMurders), y=numMurders)
#myBarPlot3 <- myBarPlot3 + aes(x= reorder(stateName, Murder), y=numMurders) ~ sort the x-axis by murder rate
myBarPlot3
#myBarPlot3 <- myBarPlot2 + aes(x= reorder(stateName, numMurders), y=numMurders) ~ alternative
#myBarPlot3 

#11) Generate a third bar chart, the same as the previous step, but also 
#showing percentOver18 as the color of the bar
myBarPlot4 <- ggplot(merge.df, aes(x=stateName, y=numMurders))
myBarPlot4 <- myBarPlot4 + geom_col()
myBarPlot4 <- myBarPlot4 + ggtitle("Total Murders")
myBarPlot4 <- myBarPlot4 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate text on X axis
myBarPlot4 <- myBarPlot4 + aes(x= reorder(stateName, numMurders), y=numMurders)
myBarPlot4 <- myBarPlot4 + aes(fill=percentOver18)
myBarPlot4

#Step D: Explore Murders - scatter chart
#12) Generate a scatter plot - have population on the X axis, 
#the percent over 18 on the y axis, and the size & color represent the murder rate
myScatterPlot <- ggplot(merge.df, aes(x=population, y=percentOver18))
myScatterPlot <- myScatterPlot + geom_point() #creates scatterplots
myScatterPlot <- myScatterPlot + aes(size=Murder, color=Murder)
myScatterPlot

       