# Q1 #
#Assume you have read in a csv file and that the data is now in a dataframe called my.df. Your data has 739 rows and one of the columns, called date.time, contains dates and times. The line below contains the first 3 (out of 739) dates in the dataset:

#"2014, Aug, Fri the 16 at 18:40" "2014, Jun, Sat the 24 at 11:51" "2014, Jun, Sun the 25 at 7:22"



date.time<- c("2014, Aug, Fri the 16 at 18:40","2014, Jun, Sat the 24 at 11:51","2014, Jun, Sun the 25 at 7:22") 

my.df <- data.frame(as.data.frame(date.time))
my.df[4:739,] <- NA
my.df$date.time
# First, define a conversion string that can be used in strptime that will convert the strings to dates without modifying the strings first.
conversion.string <- "%Y, %b, %a the %d at %R"
strptime(my.df$date.time, conversion.string)
#Next, show R code that uses your string to convert the dates in the dataframe to the data type as.POSIXct, then find the min and max dates.
my.df$date.time2 <- as.POSIXct(strptime(my.df$date.time, conversion.string))
my.df$date.time2
min(my.df$date.time2, na.rm = TRUE) # "2014-06-24 11:51:00 EDT"
max(my.df$date.time2, na.rm = TRUE) # "2014-08-16 18:40:00 EDT"

# Q2 #


my.df$x <- seq(0,100,length=739)
my.df$y <- seq(0,100,length=739)
# nrow(my.df)
# ncol(my.df)
head(my.df)
plot(my.df$x,my.df$y)
my.df$mode<- replicate(1,sample(c("T","H","Q"),739,rep=TRUE))
my.df$mode <- as.factor(my.df$mode)
str(my.df)
my.df$col <- rgb(144,238,144,alpha=125, max=255) # set T to light green
my.df[my.df$mode=="H","col"] <- rgb(139,0,0,alpha=125, max=255) # set H to dark red
my.df[my.df$mode=="Q","col"] <- rgb(93,71,139,alpha=125, max=255) # set Q to medium purple
plot(my.df$x,my.df$y, col=my.df$col, pch="*")


# Q9 #
plot(rnorm(100), col = "#FF7733", pch = 16, cex = 3)
plot(rnorm(100), col = 	rgb(1, .5, .2), pch = 16, cex = 3)
plot(rnorm(100), col = 	rgb(.3, .7, 1), pch = 16, cex = 3)


# Q 7 #
attach(mtcars)
layout(matrix(c(1,2,3,2), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)