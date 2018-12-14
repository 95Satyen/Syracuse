#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#8
#Date the assignement is due: 10/25/2018
#Date in which the assignment is submitted: 10/23/2018

#Step A: Load and condition the data  
#1.	The data is available on blackboard (hotelSurveySherison), as a JSON file.
install.packages("RJSONIO")
install.packages("ggplot2")
library(RJSONIO)
library(ggplot2)

dataset.name <- "hotelSurveySherison.json" #set dataset name
hotelSurveyOut<- fromJSON(dataset.name,simplify = TRUE, nullValue = NA) # covert JSON content into R objects
hotelSurvey <- data.frame(hotelSurveyOut) # convert R object into Data Frame
View(hotelSurvey)
#2.	Use the str command to make sure you can see the following attributes
str(hotelSurvey)

#Step B: Explore the data
#3.	Create bivariate plots for each of the attributes (numeric).
#Your code should produce nine separate plots. Make sure the Y-axis and X-axis are labeled. Keeping in mind that the overall customer satisfaction is the outcome (or dependent) variable, which axis should it go on in your plots?
#  Hint: use the jitter command, so you can see all the surveys (something such as)
#jitter(hotelSurvey$checkInSat)

# 1. bivariate plot where hotelsize is independent variable
ggplot(hotelSurvey, aes(y=overallCustSat, x=hotelSize)) + geom_point()
overallCustSatJ <- jitter(hotelSurvey$overallCustSat)
hotelSizeJ<- jitter(hotelSurvey$hotelSize)
ggplot(hotelSurvey,aes(x=hotelSizeJ,y=overallCustSatJ))+geom_point()


# 2. bivariate plot where checkInSat is independent variable
ggplot(hotelSurvey, aes(x=checkInSat,y=overallCustSat)) + geom_point()
checkInSatJ<- jitter(hotelSurvey$checkInSat)
ggplot(hotelSurvey,aes(x=checkInSatJ,y=overallCustSatJ))+geom_point()

# 3. bivariate plot where hotelClean is independent variable
ggplot(hotelSurvey, aes(x=hotelClean,y=overallCustSat)) + geom_point()
hotelCleanJ<- jitter(hotelSurvey$hotelClean)
ggplot(hotelSurvey,aes(x=hotelCleanJ,y=overallCustSatJ))+geom_point()

# 4. bivariate plot where  hotelFriendly is independent variable
ggplot(hotelSurvey, aes(x= hotelFriendly,y=overallCustSat)) + geom_point()
hotelFriendlyJ<- jitter(hotelSurvey$hotelFriendly)
ggplot(hotelSurvey,aes(x= hotelFriendlyJ,y=overallCustSatJ))+geom_point()

# 5. bivariate plot where  guestAge is independent variable
ggplot(hotelSurvey, aes(x= guestAge,y=overallCustSat)) + geom_point()
guestAgeJ<- jitter(hotelSurvey$guestAge)
ggplot(hotelSurvey,aes(x= guestAgeJ,y=overallCustSatJ))+geom_point()

# 6. bivariate plot where  lengthOfStay is independent variable
ggplot(hotelSurvey, aes(x= lengthOfStay,y=overallCustSat)) + geom_point()
lengthOfStayJ<- jitter(hotelSurvey$lengthOfStay)
ggplot(hotelSurvey,aes(x= lengthOfStayJ,y=overallCustSatJ))+geom_point()


# 7. bivariate plot where  whenBookedTrip is independent variable
ggplot(hotelSurvey, aes(x= whenBookedTrip,y=overallCustSat)) + geom_point()
whenBookedTripJ<- jitter(hotelSurvey$whenBookedTrip)
ggplot(hotelSurvey,aes(x= whenBookedTripJ,y=overallCustSatJ))+geom_point()


#4.	What do you observe from the plots? Note via a block comment.
# The jitter adds more noise to the plot, which gives better visualization of the overlapping values in the plot.

#Step C: Generate a linear model
#reomve free Text and jitter
View(hotelSurvey)
hotelSurvey <- data.frame(hotelSurveyOut)
hotelSurvey1 <- hotelSurvey[,-11]
View(hotelSurvey1)

#5. Next, create one regression model predicting the overall customer satisfaction from the other variables (but not the freeText response). Refer to page 202 in the text for syntax and explanations of lm( ). 
#Make sure to include all predictors in one model - NOT different models each with one predictor.
model <- lm(formula = overallCustSat~., data=hotelSurvey1)
summary(model)

#6.	Report the R-Squared in a comment. Which of the predictors are statistically significant in the model? 
#In a comment, report the coefficients (AKA slopes or B-weights) for each predictor that is statistically significant.
# The Rsquared value is 0.6702
# Most statistically significant predictors are : 
#checkInSat, hotelClean, hotelFriendly, guestAge, lengthOfStay and whenBookedTrip
# The coefficient for each predictor that is statistically significant is as follows:
# checkInSat:    -2.381e-01
# hotelClean:     4.042e-02
# hotelFriendly:  1.122e+
# guestAge:      -1.205e-01
# lengthOfStay:  -3.284e-01
# whenBookedTrip: 6.421e-03

#7.	Write a block comment that explains in a narrative your overall interpretation of the model.
#Make sure to refer to each variable (one dependent and three independent) by a descriptive name.
#Based on the summary, there are many possible intercepts using the 9 independent variables(checkInSat, hotelClean, hotelFriendly, guestAge,hotelState, gender,hotelSize, lengthOfStay and whenBookedTrip)
#and 1 dependent variable(overallCustSat). 
#The statistically significant predictors that determine overall customer satisfaction(overallCustSat) are checkInSat, hotelClean, hotelFriendly, guestAge, lengthOfStay and whenBookedTrip.
#The model is good as the R squared value is close to 1.

#Step D: Generate a different linear model  
#8.	Next, create a different regression model predicting the overall customer satisfaction 
#from the one variable you think is best.  Then create another using two variables.
modelcompare1 <- lm(formula = overallCustSat~hotelFriendly, data=hotelSurvey1)
summary.lm(modelcompare1) # Rsquared : 0.3785
#plot(jitter(hotelSurvey$hotelFriendly),jitter(hotelSurvey$overallCustSat))
#abline(modelcompare1)

modelcompare2 <- lm(formula = overallCustSat~hotelFriendly+hotelClean, data=hotelSurvey1)
summary.lm(modelcompare2) # Rsquared: 0.3919

#9.	Write a block comment comparing the two lm models in #8.
#The Rsquared value of model two(0.3919) is closer to 1 as compared to model one(0.3785). 
#Therefore the second model is better as compared to model one.