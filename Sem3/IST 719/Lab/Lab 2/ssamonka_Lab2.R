#
# Author: Satyen Amonkar
# Purpose: Lab 2:
#               1) Work with files
#               2) Data interogation
#               3) Data types and other stuff

#can use one forward or two backward slashes as escape character
#gives all values in directory
list.files("h:/Desktop/IST 719/")
#file choose show the path for the located file
fname <- file.choose()

#String as factor : almost in all cases for convinience, string kept as string and not categories
tips <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)

## Exploring the data

#one way to interogate the data is to just look, by using view
View(tips)

str(tips)
hist(tips$size)
hist(tips$total_bill)
tips$total_bill
tips$total_bill[1]
# tips[1,"total_bill"] This way is better in solving a problem

summary(tips$total_bill)
dim(tips)
#fix is a dangerous spreadsheet view as you can edit data
fix(tips)
# read csv, read table and read delim are basically the same with diff separator
wine <- read.table("h:/Desktop/IST 719/Wine.txt"
                   , header = TRUE
                   , sep = "\t" #think of this as a tab
                   , stringsAsFactors = FALSE)
View(wine)
str(wine)
hist(wine$rep.feedback)
summary(wine$rep.feedback)
hist(wine$unit.price)
summary(wine$unit.price)
# In R convention is . to separate words (like camel case in python and Java)
my.dir <- "H:\\Desktop\\IST 719\\"
my.file <- "Wine.txt"
#constructing a filename using paste (concatination of string)
fname <- paste(my.dir,my.file) # this wont work as there is a space in between
fname <- paste(my.dir,my.file, sep = "") # separator solves the issue
# paste 0 is paste which by default considers separator as ""
fname <- paste0(my.dir,my.file)
paste("Satyen","Amonkar")
paste("Satyen","Amonkar", sep = "$")
# Used to make random data
paste("Satyen",sample(letters[1:4], size=10, replace = T))

# median of value is somewhere between 180-200 in the distribution
#Distribution of a vector(data) - central tendancy and range
plot(wine$recipt)#(mean, max, density, median, mean) #1
plot(sort(wine$recipt)) # This also shows distribution (min, max, density) #2

range(wine$recipt) # gives min and max
summary(wine$recipt)
hist(wine$recipt) #distribution plot #3 (most times better to explain central tendancy of data)
#buckets are of equal width
boxplot(wine$recipt) #distribution plot #4

d <- density(wine$recipt)
plot(d) # distribution plot #5 continuous representation of density

par(mfrow= c(2,2)) # used to customize the plot space
plot(wine$recipt)
hist(wine$recipt)
boxplot(wine$recipt)
plot(d)

plot(tips$tip)
par(mfrow = c(1,1)) #bring me back to one row and once column plot
plot(tips$tip)

#break data into pieces

m.tips <- tips$tip[tips$sex == "Male"]
f.tips <- tips$tip[tips$sex == "Female"]

par(mfrow=c(1,2))
boxplot(m.tips)
boxplot(f.tips)
# the scale is different


par(mfrow=c(1,2))
boxplot(m.tips, ylim = c(0,12), main= "Males", col="cadetblue")
boxplot(f.tips, ylim = c(0,12), main="Females", col="pink")
# in analytics we could try to explain this