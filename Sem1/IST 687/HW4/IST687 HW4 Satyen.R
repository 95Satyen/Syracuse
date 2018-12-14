#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#4
#Date the assignement is due: 9/26/2018 
#Date in which the assignment is submitted: 9/24/2018

#Part A: Write a function to reveal the distribution of a vector of numeric values

#1, 2 & 4	Create a new function 'printVecInfo' and have it take one numeric vector as its input argument.
#Make the function print the following information for the vector supplied in the argument:
#a.	Mean
#b.	Median
#c.	Min & Max
#d.	Standard deviation
#e.	0.05 and 0.95 quantiles (Use the quantile( ) function) 
# Add labels to each element of the function's output.

printVecInfo <- function(numericVector){
                print(paste("Mean:", mean(numericVector)))      #prints the mean
                print(paste("Median:", median(numericVector)))  #prints the median
                print(paste("Min:", min(numericVector), " Max: ", max(numericVector))) # prints the min and max value
                print(paste("Standard Deviation:", sd(numericVector))) #prints the standard deviation                 
                print(paste("quantile 0.05 is ", quantile(numericVector, probs=0.05))) #less than 5% 
                print(paste("quantile 0.95 is ", quantile(numericVector, probs=0.95))) #more than 95%           
}

#3.	Test the function with this vector: testVector <- 1:10. 
testVector <- 1:10
testVector
printVecInfo(testVector) # Min is 1 and Max is 10, median is 5.5, quantiles are 1.45, 9.55, standard deviation is 3.02765


# Part B: Read the census dataset

#5.	Read in the Census dataset
#link <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
#dfStates <- read.csv(url(link))
dfStates <- read.csv("states.csv", stringsAsFactors = FALSE)
View(dfStates)    #Display the states
head(dfStates)    #Display first 6 states
tail(dfStates)    #Display last 6 states
summary(dfStates) #Descriptive Stats
str(dfStates)     #Descriptive Stats
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
  
#Make sure there are precisely 4 columns, with the following names:
  #stateName, population, popOver18, percentOver18. 
  #Hint: use colnames( ) and you will need to remove some columns
  dfStates <- dfStates[,-1:-4]       #Remove unwanted columns 
  View(dfStates)
  colnames(dfStates)                 #Display existing columns
  colnames(dfStates) <- c("stateName", "population", "popOver18", "percentOver18") #rename the columns
  colnames(dfStates)                 #Display new columns
  
  return(dfStates)                   #return clean version of dfStates 
}
states <- DataCleaning()           #Calling function with no paramters
View(states)                       #Displaying clean version of dfStates
nrow(states)                       #51

#Part C: Sample from the state population data frame
#6.Sample 20 observations from states$population and use printVecInfo( ) 
#to display the characteristics of the resulting sample, and then display the results as a histogram.
sample1 <- sample(states$population, size = 20 ) #eric did not replace as if he removes one pop he wants to keep it out
View(sample1)
printVecInfo(sample1) #characteristics of the resulting sample
hist(sample1, breaks = 20) #histogram for sample1

#7.	Repeat step six two more times. Each time that you create a sample, 
#run the resulting vector through printVecInfo( ) and create a histogram.
sample2 <- sample(states$population, size = 20 )
View(sample2)
printVecInfo(sample2) #characteristics of the resulting sample
hist(sample2, breaks = 20) #histogram for sample2

sample3 <- sample(states$population, size = 20 )
View(sample3)
printVecInfo(sample3) #characteristics of the resulting sample
hist(sample3, breaks = 20) #histogram for sample3

#8.	Using a block comment, explain in a comment why each result is different.

# Answer: since each of the samples are different (randomly generated), each of their result is different.

#Part D: Replicate the sampling
#9.	Use the replicate function, to replicate the sampling (described in step 6 above).
#Replicate the sampling 2000 times, then use printVecInfo( ) to display the characteristics
#of the resulting replicated sample, and then display the results as a histogram.

repsample1 <- replicate(2000,sample(states$population, size = 20 ))
#View(repsample1)
printVecInfo(repsample1)
hist(repsample1, breaks = 20)

#10.	Repeat step 8 two more times. Each time that you create the replicated sample, 
#run the resulting vector through printVecInfo( ) and create a histogram. 
repsample2 <- replicate(2000,sample(states$population, size = 20 ))
printVecInfo(repsample2)
hist(repsample2, breaks = 20)
repsample3 <- replicate(2000,sample(states$population, size = 20 ))
printVecInfo(repsample3) #mean = 6343662
hist(repsample3, breaks = 20)

mean(states$population) #6386651

#Using a block comment,  explain why the histograms generated 
#in Part C are different than the histograms generated in Part D
# Answer: In Part C we are considering small samples, while in Part D we are considering 
#2000 replications of the sample function. Therefore, Part D shows more accurate 
#mean than Part C.  




