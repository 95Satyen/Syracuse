# Presentation
install.packages("lubridate")
library(lubridate)
#oracle db format
bd <- "5-JUL-1995"
#Poster presentation Date
as.Date("2019-11-22")
as.Date("2019-22-11")

class(bd)
dmy(bd) #convert date into date month year format
x<- dmy(bd)
this_day <- today()
interv<- interval(x, this_day)
as.period(interv)
EST<- now()
ymd("1989-05-17")
help("lubridate")
ymd(EST, tz="India")
nyc <- now("America/New_York")
update(EST, hours = 8, minutes = 34, seconds = 55)
#different date formats
dmy_hms("1 Jan 2017 23:59:59") 
ymd_hms("2017-11-28T14:02:00")
dmy_hm("1 Jan 2017 23:59")
ydm("20173101")
mdy("July 4th, 2000") 
myd("July 2000,4th")
dym("4th 99 July")
dmy("4th of July '99") 
yq("2001: Q3")
x=dmy_hms("12-JAN-19 11:45:20")
date(x)
year(x) #Year. year(dt)
isoyear(x) #The ISO 8601 year.
epiyear(x) #Epidemiological year
month(x, label=T, abbr=F)
month(x, label=T, abbr=T)
month(x)
day(x) #Day of month. day(dt)
wday(x,label=T,abbr=T) #Day of week.
qday(x) #Day of quarter.
hour(x)
minute(x)
second(x)
week(x)
semester(x)
am(x)
pm(x)
dst(x)
leap_year(x)

# CurrentTime <- today()



my.dir <- "C:/Users/DELL/Documents/GitHub/Syracuse/Sem3/IST 719/Source/"
fname <- paste0(my.dir, "NFLX.csv")
NFLX <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
head(NFLX)
as.Date(NFLX$Date[1])
month<- ymd(NFLX$Date)

library(lubridate)
# Year-Month-Date Format
ymd("20191105")
# Month-Date-Year Format
mdy("11-05-2019")
# Date-Month-Year Format
dmy("05/11/2019")

temp <- now()
#Setting Information
second(temp)
second(temp) <- 0
temp
#Extracting Information
week(temp)
month(temp, label=TRUE)
wday(temp, label=TRUE)
yday(temp)
day(temp)
minute(temp)

#Current Timezone
Sys.timezone() 
presentation <- dmy_hms("05-11-2019 09:30:00"
                        , tz = "America/New_York")
#Changes Printing
with_tz(presentation, "Asia/Calcutta")
#Change Time
force_tz(presentation, "Asia/Calcutta")



# project <- mdy_hms("11-22-2019 10:00:00"
#                    , tz = "America/New_York")
# timeline <- interval(presentation,project)
# timeline
# as.period(timeline)


#Timespan:
CurrentTime <- now("America/New_York")
FoundationDate<- mdy("Mar-24-1870"
                     , tz="America/New_York")
#Interval
Timeinterval<- interval(FoundationDate, CurrentTime)
Timeinterval
#Period
as.period(Timeinterval)
#Duration
as.duration(Timeinterval)

#Instants
#POSIXct class object
CurrentTimeP <- now()
CurrentTimeP
is.POSIXlt(CurrentTimeP)
is.POSIXct(CurrentTimeP)
#Date class object
CurrentTimeD <- today()
CurrentTimeD
is.Date(CurrentTimeD) 

# head(grades)
# table(grades$Percentage)
# nrow(grades)

#Time Series for IST 719 Submissions
fname <- paste0(my.dir, "grades.csv")
grades<-read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
dates<- mdy(grades$date)
submission<- sort(dates)
plot(table(submission), typ="l"
     , las=2, cex.axis=0.5, main="Time Series for IST 719 submissions"
     , xlab="Submission date", ylab="Number of submissions")
