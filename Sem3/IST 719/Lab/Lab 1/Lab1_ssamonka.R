#
# Author: Satyen Amonkar
# Purpose: Lab 1: Introduction to class and R

pie(c(10,15,7,20))

# main used for title
pie(c(10,15,7,20), main="How many employees per division")

pie(c(10,15,7,20), main="How many employees per division"
    , labels = c("A","B","C","D")
    , col = c("red","tan","orange","brown"))


## Dot plot (Dot plot does not have x and y axis)
plot(c(1,2,4,8,1,4))

#pch allows to change the simple dots
plot(c(1,2,4,8,1,4),pch=5)

# each dot a different visual
plot(c(1,2,4,8,1,4),pch=c(2,4,5,8,20))

# expansion: 3 times the size
plot(c(1,2,4,8,1,4),pch=c(2,4,5,8,20),cex=3)

plot(c(1,2,4,8,1,4),pch=c(2,4,5,8,20),cex=3
     , col=c("red","tan","orange","brown","blue"))

## Scatter plot
# R assumes first argument as x, and second if given as y provided its numbers
plot(1:6,6:1, pch=1:6)
plot(x=1:6,y=6:1, pch=1:6)

n<-20
plot(1:n,1:n, pch=1:n)

# by default plot assumes you need a dot plot or scatter plot but it can be changed using type
x<- c(1,2,4,8,1,4)
# lwd is line width
plot(x=x, type="l", lwd= 3,col="orange")
plot(x=x, type="h",lwd=3, col="orange")
# lyt changes line type, bty change box type
# make the plot as simple as possible (remove chart junk)
plot(x=x, type="l",lwd=3, col="orange"
     , lty = 3, bty="n")

plot(x=x, type="l",lwd=3, col="orange"
     , lty = 3)

plot(x=x, type="l",lwd=3, col="orange"
     , lty = 3, bty="n",xlab="",ylab="", main="Satyen's Plot")

plot(x=x, type="l",lwd=2, col="red"
     , lty = 2, bty="n",xlab="",ylab="", main="Satyen's Plot")

# ? (name of funtion) to know the parameters
?plot
?par

par(bg="tan")
plot(x=x, type="l",lwd=2, col="red"
     , lty = 2, bty="n",xlab="",ylab="", main="Satyen's Plot")

n<-27

slets <- sample(letters[1:3], size=n, replace = T)
slets

slets[2]
slets[5:7]
# counts how many a,b,c in a form of vector
table(slets)
# barplot assumes first argument is vector of numbers
barplot(table(slets))

stab <- table(slets)
barplot(stab, col="red")
# if dont provide number of colors as required then it will recycle the colors
barplot(stab, col=c("red","blue"))

barplot(stab, col=c("cadetblue","blue","cyan"))
#border NA removes the border around the barplot
#horiz makes bar horizontal
#las controls how x axis names are represented 0,1 and 2
barplot(stab, col=c("cadetblue","blue","cyan")
        , border = "tan"
        , names.arg = c("Alice","Bob","Candy")
        , horiz = T
        , las = 2)

#density tells how dense the lines should be (how many lines per inch)
# anglw is the angle of the lines
barplot(stab, col=c("cadetblue","blue","cyan")
        , border = "tan"
        , names.arg = c("Alice","Bob","Candy")
        , horiz = T
        , las = 2
        , density = 20
        , angle = 45
        , xlab="employees"
        , ylab="Department")

barplot(stab, col=c("cadetblue","blue","cyan")
        , border = "tan"
        , names.arg = c("Alice","Bob","Candy")
        , horiz = T
        , las = 2
        , density = c(15,25,35)
        , angle = c(45,15,60)
        , xlab="employees"
        , ylab="Department")

# to avoid overlap of department with y axis
barplot(stab, col=c("cadetblue","blue","cyan")
        , border = "tan"
        , names.arg = c("Alice","Bob","Candy")
        , horiz = T
        , las = 2
        , density = c(15,25,35)
        , angle = c(45,15,60)
        , xlab="employees"
        , ylab="Department"
        , adj = 0)

# Space used for space between bars
barplot(stab, col=c("cadetblue","blue","cyan")
        , border = "tan"
        , names.arg = c("Satyen","Corey","Srinath")
        , horiz = T
        , las = 2
        , density = c(15,25,35)
        , angle = c(60,20,30)
        , xlab="employees"
        , ylab="Department"
        , adj = 0
        , space = 0.5)
# creating 100 random normal values with mean 10 (by default 0)
x <- rnorm(n=100, mean=10, sd=1)
#Understanding the distribution of random normal data, central tendancy
# Median is the dark line, the lines are the range,outside the line are the outliers
boxplot(x,horizontal = T)
?boxplot
#histogram shows central tendancy
hist(x)
