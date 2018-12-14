subject.Name <- c("Mathematics","Physics","Chemistry","Biology")
#print(subject.Name)
grades.student1 <- c(88,75,83,97)
#print(grades.student1)
grades.student2 <- c(96,94,86,77)
#print(grades.student2)
grades.DF <- data.frame(subject.Name, grades.student1, grades.student2) #Creating dataframe
#print(grades.DF)
str(grades.DF) #structure of dataframe
print(grades.DF$grades.student2) #Printing the grades of student 2

x <- grades.DF[which(grades.DF$subject.Name=='Biology'), ,]
print(c(x$grades.student1,x$grades.student2)) #Print marks of both students in Biology
y <- grades.DF[which(grades.DF$subject.Name=='Physics'), ,]
print(y$grades.student1) #Print marks of student 1 in Physics
student1 <-  grades.DF[which(grades.DF$grades.student1>90), ,]
student2 <-  grades.DF[which(grades.DF$grades.student2>90), ,]
grades <- c(student1$grades.student1,student2$grades.student2)
print(grades) #Print marks greater than 90
