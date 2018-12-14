#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#9
#Date the assignement is due: 11/08/2018
#Date in which the assignment is submitted: 11/04/2018

#Part A: Explore Data Set
#1)	Load the dataset: hotelSurveyBarriot.json

install.packages("RJSONIO")
library(RJSONIO)

dataset.name <- "hotelSurveyBarriot.json" #set dataset name
hotelSurveyOut<- fromJSON(dataset.name,simplify = TRUE, nullValue = NA) # covert JSON content into R objects

#2)	Name the dataframe hotelSurvey
hotelSurvey <- data.frame(hotelSurveyOut) # convert R object into Data Frame

#Part B: Explore Data Set
#1)	Ensure hotelSurvey is a dataframe, and look at the structure via the str() command
View(hotelSurvey)
str(hotelSurvey)  #Checking whether hotelSurvey is a dataframe

#2)	Map each numeric attribute to a category  - Since we want to create rules, we should convert the attributes that have a numeric range into buckets (ex. low or high)
colnames(hotelSurvey)
hotelSurvey <- hotelSurvey[,-11] #removing non numeric column free text

#For Survey attributes that range from 0 to 10 one can use the following:

createBucketSurvey <- function(vec) {
  vBuckets <- replicate(length(vec), "Average")
 vBuckets[vec > 7] <- "High"
 vBuckets[vec < 7] <- "Low"
 return(vBuckets)
 }

#For other attributes, the following code:

CreateBuckets <- function(vec) {
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}


#Survey attributes that range from 0 to 10
happyCust <- createBucketSurvey(hotelSurvey$overallCustSat)
View(happyCust)
checkIn<- createBucketSurvey(hotelSurvey$checkInSat)
View(checkIn)
hotelClean<- createBucketSurvey(hotelSurvey$hotelClean)
View(hotelClean)
hotelfriendly<- createBucketSurvey(hotelSurvey$hotelFriendly)
View(hotelfriendly)
lenOfStay <- createBucketSurvey(hotelSurvey$lengthOfStay)
View(lenOfStay)

#Other attributes
age<- CreateBuckets(hotelSurvey$guestAge)
View(age)
#hotelSize <- CreateBuckets(hotelSurvey$hotelSize)
whenBooked <- CreateBuckets(hotelSurvey$whenBookedTrip)
View(whenBooked)

#3)	Count the people in each category of for the age and friendliness attributes
#Hint: Use the table( ) command.

#review descriptive analysis - age
t <- table(age)
t 

#review descriptive analysis - friendliness
t1 <- table(hotelfriendly)
t1

#4)	Express the results of problem 3 as percentages by sending the results of the table( ) command into the prop.table( ) command
prop.table(t)
prop.table(t1)

#5)	Show a "contingency table" of percentages for the age and the overall satisfaction variables together. 
#Write a block comment about what you see.

contigencyTable <- table(age,happyCust)
prop.table(contigencyTable)

#I see a cross matrix relationship between age and overall satisfaction in percentages.

#Part C: Coerce the data frame into transactions
#6)	Install and library two packages: arules and arulesViz.
install.packages("arules")
install.packages("arulesViz", dependencies = TRUE) #install all dependencies
library(arules)
library(arulesViz)

#7)	Coerce the hotelSurvey data frame into a sparse transactions matrix using:

ruleDF<- data.frame(happyCust, checkIn, hotelClean, hotelfriendly, age, lenOfStay, whenBooked) #all numeric attributes in a dataframe
View(ruleDF)
#Coerce
hotelSurveyX <- as(ruleDF,"transactions")
View(hotelSurveyX)

#8)	Use the inspect( ), itemFrequency( ), and itemFrequencyPlot( ) commands to explore the contents of hotelSurveyX.
inspect(hotelSurveyX) #Display Associations and Transactions in Readable Form
itemFrequency(hotelSurveyX) #Getting Frequency/Support for Single Items
itemFrequencyPlot(hotelSurveyX, cex.names=0.6) #Creating a Item Frequencies/Support Bar Plot

#Part D: : Use arules to discover patterns

#9)	Run the apriori command to try and predict happy customers 
#(as defined by their overall satisfaction being high - above 7).

ruleset <- apriori(hotelSurveyX, parameter = list(support=0.1, confidence=0.5), appearance = list(default="lhs",rhs=("happyCust=High"))) #Mining Associations with Apriori

#10)	Once you have a reasonable number of rules, use inspect( ) to view the ruleset. 
#check ruleset
summary(ruleset)             
inspect(ruleset)

#11)	 If you had to provide two rules to the hotel owner 
#(in terms of what helps drive high overall customer satisfaction, what would those two rules be? 
#Use a block comment to explain your answer.
#Based on the summary of ruleset, maximum lift score is 2.08938 for 2 rules. 
#The best two rules to provide to the hotel owner for high overall customer satisfaction 
#will be rule number 46 and rule number 57 as they have the maximum lift score.

#[46] {checkIn=High, hotelClean=High, hotelfriendly=Average, whenBooked=High}       => {happyCust=High}  2.089386
#[57] {checkIn=High, hotelClean=High, hotelfriendly=Average, lenOfStay=Low, whenBooked=High}       => {happyCust=High}   2.089386

