#
# Author: Satyen Amonkar
# Purpose: Lab 3: answering questions and multi dim plots

#use csv instead of excel while loading in r

my.dir <- "C:/Users/DELL/Documents/GitHub/Syracuse/Sem3/IST 719/Source/"
fname <- paste0(my.dir, "sales.csv")

sales <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)

# Viewing columns to get a sense of what the data is about
colnames(sales)
# str(sales)
# View(sales)

# Question: What is the relationship between expenses and recipt?

plot(sales$expenses, sales$recipt)
# as expenses sales go up, recipt go up(sales)
# correlation & causation

# Trend of Data
plot(sales$expenses, sales$recipt
     , main = "scatter", col="grey"
     , pch = 20, cex= .7
     , ylab = "recipt", xlab = "expenses"
     , bty = "n")
str(sales)
abline(lm(sales$recipt ~ sales$expenses) , col = "brown"
       , lwd = 3, lty = 3)
abline(h= 200, col = "blue", lty = 2)
abline(v=6, col = "blue", lty = 2)
# rug gives distribution of data, where data is dense, tick mark for every observation
rug(x = sales$expenses, side = 1, col = "orange")
rug(x = sales$recipt, side = 2, col = "orange")
#scatter plots are usually square plotted
# rug(x = sales$recipt, side = 4, col = "pink") 
# in r bottom is always side 1, left is always side 2, etc

colnames(sales)
sales$type
#table counts the instances. There are 2 types, red and white , red is appearing more than white but not by much
table(sales$type)
unique(sales$type) # shows unique value

# QUESTION: What is the relationship between recipt and type?

# bty for some reason doesnt work for boxplot. Then par can be used which sets it a default
par(bty = "n")
boxplot(sales$recipt ~ sales$type, col = "cadetblue"
        , main = "Relationship: recipt & type"
        , xlab = "Wine Type", ylab = "Sale Recipt"
        , pch = "*")
abline(h = median(sales$recipt), lty = 2, col = "red")
# white wine make little bit more money than red wine

par(bty = "n")
boxplot(sales$recipt ~ sales$type, col = "cadetblue"
        , main = "Relationship: recipt & type"
        , ylab = "Wine Type", xlab = "Sale Recipt"
        , pch = "*"
        , horizontal = TRUE) # for vertical line since it saves space
abline(v = median(sales$recipt), lty = 2, col = "red")


colnames(sales)
table(sales$rep.region) # table is helpful
sales$units.sold
table(sales$units.sold) # table is not that helpful
hist(sales$units.sold)
# next we try to find if some region sell more
boxplot(sales$units.sold ~ sales$rep.region) 
# Boxplot tells what is the distibution of each region but doesn't answer above question
# to answer the question we need to subset the regions

sum(sales$units.sold[sales$rep.region == "East"]) # but we need to do it for each region

# In built functions to do it for all regions

units.by.reg <- aggregate(sales$units.sold
                          , list(sales$rep.region)
                          , sum)
barplot(units.by.reg$x, names.arg = units.by.reg$Group.1)
units.by.reg
# you can match sum and aggregate to check validity
class(units.by.reg) # class tells what kind of thing it is

# any function can be passed in aggregate
aggregate(sales$units.sold
          , list(sales$rep.region)
          , mean) 
aggregate(sales$units.sold # give min and max value
          , list(sales$rep.region)
          , range) 
aggregate(sales$units.sold
          , list(sales$rep.region)
          , median) 
# table just counts, aggregate gives useful data (colnames can be used)
units.by.reg
colnames(units.by.reg) <- c("region","units.sold")
barplot(units.by.reg$units.sold, names.arg = units.by.reg$region)

units.by.reg.T <- tapply(sales$units.sold
                          , list(sales$rep.region)
                          , sum)
units.by.reg.T
# both aggregate and Tapply do the same thing, 
# parameter names is different and how data comes out is different
# aggregate <- df, tapply <- array
class(units.by.reg.T)
names(units.by.reg.T)
barplot(units.by.reg.T) # with tapply we do not need to fiddle with tapply for barplot with names

barplot(tapply(sales$units.sold
               , list(sales$rep.region)
               , sum))
# can be done but not a good practise
# summary can be used as function instead of sum for storing, range df is better
# QUESTION: Is there a difference in red and white sales by region
M <- tapply(sales$units.sold
       , list(sales$rep.region, sales$type)
       , sum)
M # It gives a matrix of data coulmn are type and row anre name of region
class(M)
barplot(M) # stacked bar plot is bad plot as we cannot easily compare within and between
barplot(M, beside = TRUE) # this is still a bad plot but a little better(fewer larger groups would be better)
barplot(t(M), beside = TRUE) 
M.T <- tapply(sales$units.sold
            , list(sales$type, sales$rep.region)
            , sum)
barplot(M.T, beside = TRUE) # in practise we need more groups with fewer bars
# by default las is set to 0
barplot(M.T, beside = TRUE, legend.text = row.names(M.T)
        , col = c("red","white")
        , main = "Wine Type by Region"
        , horiz = T, las = 1)

# QUESTION: are recipts growing over time for each region?

M <- tapply(sales$recipt, list(sales$year), sum)
names(M) # need to convert string to number
x <- as.numeric(names(M)) #typecasting
plot(x, y= M, type = "l", lwd=3) # this is a relative plot
plot(x, y= M, type = "l", lwd=3, ylim = c(0,350000)) # best practise to start from 0

class(M)

M <- tapply(sales$recipt, list(sales$rep.region, sales$year)
            , sum)
class(M) # a matrox
names(M) # will be null as matrix
colnames(M)
year<- as.numeric(colnames(M)) #typecasting
max(M)

options(scipen = 89) #y axis is better
plot(year, y = M[1,]
     , type = "n", lwd = 3, ylim = c(0, max(M))
     , xlab = "Year", ylab = "Recipts"
     , main = "Sales by region") # max doesnt care if it is a vector or matrix
# lines(year, M[1,], col="red", lwd = 3)
xspline(year, M[1,], shape = 1, border = "red", lwd = 3)
# lines(year, M[2,], col="yellow", lwd = 3)
xspline(year, M[2,], shape = 1, border = "yellow", lwd = 3)
lines(year, M[3,], col="green", lwd = 3)
lines(year, M[4,], col="blue", lwd = 3)
lines(year, M[5,], col="cyan", lwd = 3)

legend("bottomleft", legend = rownames(M), lwd = 3
       , lty = 1, bty = "n" #lty can also be passed as a vector
       , col = c("red","yellow","green","blue","cyan"))

# xspline takes a x and y and a shape parameter, it is approximate curve and misses lines
