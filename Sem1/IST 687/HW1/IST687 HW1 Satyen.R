#Course number (i.e., IST 687)
#Satyen Amonkar
#Homework#1
#Date the assignement is due: 9/5/2018 
#Date in which the assignment is submitted : 8/31/2018

#Step A: Create a Vector  
# 1)  Define a vector 'grades', which contains the 
#numbers 4.0, 3.3 and 3.7 (i.e., three numbers in 
# the vector 'grades'). 
grades <- c(4.0,3.3,3.7)
grades

# 2)	Define a vector 'courseName', which contain the
# strings "Bio", "Math", "History".
courseName <- c("Bio" , "Math" , "History")
#courseName

# 3) Define a variable 'BetterThanB', that is equal to 3
BetterThanB <- 3
#BetterThanB

#Step B: Calculating statistics using R  
# 4)	Compute the average of the grades vector with the mean() function
mean(grades)
#3.66

#5)	Calculate the number of observations in the grades vector with the 
#length() function, and store the result in the variable 'total.length'
total.length<-length(grades)

#6)	Output the value of 'total.length' 
total.length   #3

#7)	Calculate the sum of  'grades' with the sum() function,
#store the result in 'total'.
total <- sum(grades) #11
#total

#8)	Recompute the average of all the grades by combining 
#questions 2 and 4
total/total.length  #3.66

# Step C: Using the max/min functions in R

#9)	Compute the max grades, store the result in 'maxG'
maxG <- max(grades) #4
#maxG

#10)	Compute the min grades, store the results in 'minG'
minG <- min(grades) #3.3
#minG

#Step D: Vector Math

#11)	Create a new vector called betterGrades, which is the
#grades + 0.3 (each grade improved each grade by  0.3 points)

betterGrades <- grades + 0.3 # 4.3, 3.6, 4
#betterGrades

#12) Compute the average of betterGrades
mean(betterGrades)  #3.96

#Step E: Using Conditional if statements
#13)	 Test if maxG is greater than3.5 (output "yes" or "no")
if (maxG > 3.5) "yes" else "no"  #yes

#14)	 Test if minG is greater than the variable 'BetterThanB''
#(output "yes" or "no")
if(minG > BetterThanB) "yes" else "no" #yes

#Step F: Accessing an element in a vector
#15)  Output the name of the second class, in the 'courseName' vector
courseName[2]  #"Math"


