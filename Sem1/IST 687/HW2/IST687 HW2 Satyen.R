#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#2
#Date the assignement is due: 9/12/2018 
#Date in which the assignment is submitted : 9/9/2018

# Step A: Initialize an 'arrests' dataframe

# 1)	Copy USArrests into a new variable (called 'arrests').  
# The USarrests data should be right in your R datasets.

arrests <- USArrests
View(arrests) #display arrests

#Step B: Explore the assault rate

#2)	Write a comment: Is a higher or lower assault rate best?
#lower assault rate is the best 

#3)	Which state has the best assault rate? 
min(arrests$Assault) #lowest assault value
low.assaultrate.index <- which.min(arrests$Assault) #index of the lowest assault rate
arrests[low.assaultrate.index,] #row for the lowest assault rate
rownames(arrests[low.assaultrate.index,]) # North Dakota has the best assault rate

#Step C: Explore the murder rate  
#4)	Which state has the highest murder rate?
max(arrests$Murder)  #highest murder value
high.murderrate.index <- which.max(arrests$Murder) #index of the highest murder rate
arrests[high.murderrate.index,] #row for the highest murder rate
rownames(arrests[high.murderrate.index,]) # Georgia has the highest murder rate

#5)	Create a sorted dataframe, based on descending murder rate
sorted.Arrests <- arrests[order(-arrests$Murder), ] #creating a new dataframe based on descending murder rate
View(sorted.Arrests)

#6)	Show the 10 states with the highest murder rate
sorted.Arrests[1:10,]
row.names(sorted.Arrests)[1:10] #names of the 10 highest murder rate states

#7)	What is the value of the 20th row, third column (in the sorted dataframe)? Use R code (not visual inspection)
sorted.Arrests[20,3] #50

#Step D: Which state is the least safe? Explain your logic

#8)	Write the R code to determine your answer
sorted.Arrests$Crime <- 1000*sorted.Arrests$UrbanPop*(sorted.Arrests$Murder + sorted.Arrests$Assault + sorted.Arrests$Rape) #Total Crime for a state
sorted.Arrests$Crimerate <- sorted.Arrests$Crime / 100000 #Crime per 100,000
sorted.Arrests.maxcrimerate.index <-  which.max(sorted.Arrests$Crimerate)
sorted.Arrests[sorted.Arrests.maxcrimerate.index,] #Florida
#9)	Write a comment to explain your logic
#The Murder, Assault and Raper are numeruc values per 100,000 and UrbanPop is numeric value Percent
#Crimerate is total crime per 100,000
# The state with maximum crime rate will be the least safe: Florida

