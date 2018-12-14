#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#10
#Date the assignement is due: 11/15/2018
#Date in which the assignment is submitted: 11/10/2018

#Part A: Load and condition the data  
#1.	The data is available on blackboard (hotelSurveyBarriot), as a JSON file.
#Hint: Don't forget to use setwd() to make sure that R is looking in the right folder for your text file.

install.packages("RJSONIO")
library(RJSONIO)

dataset.name <- "hotelSurveyBarriot.json" #set dataset name
hotelSurveyOut<- fromJSON(dataset.name,simplify = TRUE, nullValue = NA) # covert JSON content into R objects
hotelSurvey <- data.frame(hotelSurveyOut) # convert R object into Data Frame

#Part B: Create a happy customer variable

#2.	To focus on predicting happy customers, we need to generate a new column (where overallCustSat is 8 or higher).

hotelSurvey$happyCustomer <- hotelSurvey$overallCustSat > 7 #happyCustomer with overallCustSat 8 or higher have True as Output
hotelSurvey <- hotelSurvey[,-11]
hotelSurvey <- as.data.frame(hotelSurvey)
View(hotelSurvey)

#Part C: Create training and test data sets
#Using techniques discussed in class, create two datasets - one for training, one for testing.
#3.	Pages 235 - 237 of the book describe how to create a training data set and a test data set. 
#Following the strategy in the book, the training data should contain about two thirds of the whole data set, with the remaining one third going to the test data.

install.packages("kernlab")
library(kernlab)
dim(hotelSurvey) #[1] represents the number of records and [2] the number of columns
cutPoint <- floor(2*dim(hotelSurvey)[1]/3) #Cutpoint(6666) as 2/3rd of  the 10000 records
randIndex <- sample(1:dim(hotelSurvey)[1]) #Select random index 
summary(randIndex)
#View(randIndex)
length(randIndex) #length of randIndex is same as length of hotelSurvey
train <- hotelSurvey[randIndex[1:cutPoint],] # training data consisting of 2/3rd of whole data set  
test <- hotelSurvey[randIndex[(cutPoint+1):dim(hotelSurvey)[1]],] # testing data consisting of 1/3rd of whole data set 

#4.	Use the dim( ) function to demonstrate that the resulting training data set and test data set contain the appropriate number of cases.
dim(test) #3334
dim(train)#6666
str(train)
str(test)

#Part D: Build a Model using ksvm( ) 
#5.	Build a support vector model using the ksvm( ) function using two or three of the variables to predict a happy customer. 
#Once you have specified the model statement and the name of the training data set, 
#you can use the same parameters as shown on page 237: kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE

# Considering rule with hotelClean=High,hotelfriendly=Average, whenBooked=High with Lift Score 2.084722
svmOutput <- ksvm(happyCustomer ~hotelClean+hotelFriendly+whenBookedTrip, data= train, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)

#6.	Write a block comment that summarizes what you learned from the book about those parameters. 
#The two parameters of greatest interest are C=5 and cross=3.
# The parameter C determinces the Cost. The higher the value of Cost, the less likely it is that the algorithm will misclassify a point and lower is the separation.
# we are using the threefold cross validation for guiding the fitting function and to prevent specialization of training dataset.  
# We are using radial basis function kernel to generate separation boundary by projecting the low-dimensional problem into higher-dimensional space

#7.	Store the output of kvsm( ) in a variable and then echo that variable to the console.   
svmOutput
#2183 support vectors 

#Part E: Predict Values in the Test Data and Create a Confusion Matrix

#8.	Use the predict( ) function to validate the model against test data. Assuming that you put the output from the ksvm( ) call into svmOutput and that your test data set is in a data frame called testData, the call would be:
#svmPred <- predict(svmOutput, testData, type = "votes")

svmPred <- predict(svmOutput, test, type = "votes")

View(svmPred)
#9.	Now the svmPred object contains a list of votes in each of its rows. The votes are either for "happy" or "notHappy". 
#Review the contents of svmPred using str( ) and head( ).

str(svmPred)
head(svmPred)

#10.	Create a confusion matrix (a 2 x 2 table) that compares the second row of svmPred to the contents of testData$happy variable.
svmPred[svmPred[,1]>0.8] <- 1
svmPred[svmPred[,1]<=0.8] <- 0
compT <- data.frame(test[,11], svmPred)
confusionMatrix <- table(compT)
confusionMatrix
#11.	Calculate an error rate based on what you see in the confusion matrix. See pages 243-244 for more information.
test1 <- confusionMatrix[1,1] + confusionMatrix[2,2]
test2 <- confusionMatrix[1,2] + confusionMatrix[2,1]
errorRate <- (test2/(test1+test2))*100
errorRate
#error Rate is 14.84703S

#Part F: Find a good prediction
#12.	Repeat Parts C and D to try and improve your prediction
cutPoint2 <- floor(1*dim(hotelSurvey)[1]/2) #Cutpoint2(8000) as 4/5th of  the 10000 records
train2 <- hotelSurvey[randIndex[1:cutPoint2],] # training data consisting of 4/5th of whole data set  
test2 <- hotelSurvey[randIndex[(cutPoint2+1):dim(hotelSurvey)[1]],] # testing data consisting of 1/5th of whole data set 
length(train2$overallCustSat)
svmOutput2 <- ksvm(happyCustomer ~hotelClean+hotelFriendly+whenBookedTrip, data= train2, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 10, prob.model = TRUE)
svmOutput2
svmPred2 <- predict(svmOutput2, test2, type = "votes")
svmPred2[svmPred2[,1]>0.8] <- 1
svmPred2[svmPred2[,1]<=0.8] <- 0
compT2 <- data.frame(test2[,11], svmPred2)
confusionMatrix2 <- table(compT2)
confusionMatrix2
tests1 <- confusionMatrix2[1,1] + confusionMatrix2[2,2]
tests2 <- confusionMatrix2[1,2] + confusionMatrix2[2,1]
errorRate2 <- (tests2/(tests1+tests2))*100
errorRate2
#error Rate is 14.74

# I observed that the error rate could be reduced with the combination of 
#1. less Cost and less train data compared to test data 2. more Cost abd more train data compared to test data


#13.	Explain, in a block comment, why it is valuable to have a "test" dataset that is separate from a "training" dataset?
#we need to check whether the traied data is 

#If we only use a single test and train dataset, the ksvm algorithm can be pushed to the point where it essentially can memorize the input data and perfectly replicate the outcome data in the training set. 
#Therefore this model if pushed too hard, can become too specialized to the training data and won't be useful on any other dataset. 