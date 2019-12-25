#
# Author: Satyen Amonkar
# Purpose: HW 6:
# Load the data


# Scatterplot Matrix, Figure 6-9

crime <-read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv",
            sep=",", header=TRUE)
crime[1:3,]
plot(crime$murder, crime$burglary)
crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]
plot(crime2$murder, crime2$burglary)
plot(crime2$murder, crime2$burglary, xlim=c(0,10), ylim=c(0, 1200))
scatter.smooth(crime2$murder, crime2$burglary,
               xlim=c(0,10), ylim=c(0, 1200))

plot(crime2[,2:9])
pairs(crime2[,2:9], panel=panel.smooth,
      # main="Rates per 100,000 population",
      # cex.main=0.5,
      col.smooth = "blue",
      cex.axis=0.6,
      cex.labels = 0.5
      #, las = 1
      )

mtext("Rates per 100,000 population", side=3, adj=0, line=2.2, cex=0.5, font=2)
names(crime2) <- c("State", "Murder", "Forcible Rape","Robbery","Aggravated Assault",
                   "Burglary","Larceny/Theft","Motor Vehicle Theft","Population")

# Bubble chart, Figure 6-15

crime <-read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv",
           header=TRUE, sep="\t")
symbols(crime$murder, crime$burglary, circles=crime$population)

radius <- sqrt( crime$population/ pi )
symbols(crime$murder, crime$burglary, circles=radius)

# symbols(crime$murder, crime$burglary,
#         squares=sqrt(crime$population), inches=0.5)
symbols(crime$murder, crime$burglary, circles=radius, inches=0.35,
        fg="white", bg="red", xlab="", ylab="",
        bty="n",
        las=1)
text(crime$murder, crime$burglary, crime$state, cex=0.5)
mtext("MURDERS VERSUS BURGLARIES IN THE UNITED STATES", side=3, adj=-0.25, line= 1, cex=0.7, font=2)
mtext("Burglaries", side=3, adj=-0.1, line=-0.5, cex=0.5, font=2)
mtext("per 100,000 population", side=3, adj=-0.11, line=-1, cex=0.5, font=1)
mtext("Murders", side=1, adj=0, line=2, cex=0.5, font=2)
mtext("per 100,000 population", side=1, adj=0, line=2.5, cex=0.5, font=1)
mtext("Source U.S Census Bureau | Nathan Yau", side=1, adj=0.95, line=2.5, cex=0.4, font=1)


# Histogram, Figure 6-24

birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
stem(birth$X2008)
hist(birth$X2008)

hist(birth$X2008,  xlim = c(0,60), ylim = c(0,60),col="#8B008B", border = "tan",
     main = "",xlab = "",ylab = "",
     las=1)
mtext("GLOBAL DISTRIBUTION OF BIRTH RATES", side=3, adj=-0.25, line= 3, cex=1, font=2) 
mtext("In 2008, most countires had birth rates less than 25 live births per 1,000 population.", side=3, adj=-0.5, line=2, cex=0.7, font=1)
mtext("There are, however, many developing countries where women tend to bear more", side=3, adj=-0.45, line=1.5, cex=0.7, font=1)
mtext(" children.", side=3,  adj=-0.09,line=1, cex=0.7, font=1)

# Density Plot. Figure 6-32

birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)
d2008frame <- data.frame(d2008$x, d2008$y)
write.table(d2008frame, "birthdensity.txt", sep="\t")

plot(d2008, type="n", las =1, main = "", cex.axis=0.7, xlab = "", ylab="")
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
polygon(d2008, col="#821122", border="#cccccc")
mtext("GLOBAL DISTRIBUTION OF BIRTH RATES IN 2008", side=3, adj=-0.35, line= 1.5, cex=1, font=2) 
mtext("Density", side=3,  adj=-0.1,line=-0.5, cex=0.7, font=1)
mtext("Live births per 1,000 population", side=1,  adj=0.08,line=2, cex=0.7, font=1)
mtext("Source: The World Bank", side=1,  adj=1,line=2, cex=0.5, font=1)

# Multiple plots : Using Data Report - Zomato Restaurants dataset
my.dir <- "C:/Users/DELL/Documents/GitHub/Syracuse/Sem3/IST 719/Source/ProjectData/"
fname <- paste0(my.dir, "zomato.csv")
zomato <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
library(xlsx)
fname2 <- paste0(my.dir, "Country-Code.xlsx")
cn <- read.xlsx2(fname2, sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE)
for (dfindex in 1:nrow(zomato)) {
  i <- 1
  while(zomato$Country.Code[dfindex]!= cn$Country.Code[i]){
    i = i+1 }
  zomato$country.Name[dfindex] <-cn$Country[i] 
}

str(zomato)
table(zomato$country.Name) #country names where zomato has restaurants
table(zomato$Aggregate.rating) #aggregate rating of restaurants

# Set the layout
par(mfrow=c(4,1))
par(mar=c(4,3,3,1))
?par
breaks = seq(0, 5, by=0.5)

hist(zomato[zomato$country.Name == "India",]$Aggregate.rating, breaks = breaks, xlab="Average Rating for Restaurants in India", main = "Distribution of Average Rating for Restaurants in India", las=1, cex.main = 0.8, cex.lab=0.8, cex.axis=0.7)
hist(zomato[zomato$country.Name == "United States",]$Aggregate.rating, breaks=breaks, xlab="Average Rating for Restaurants in US", main = "Distribution of Average Rating for Restaurants in US", las=1, cex.main = 0.8, cex.lab=0.8, cex.axis=0.7)
hist(zomato[zomato$country.Name == "United Kingdom",]$Aggregate.rating, breaks = breaks, xlab="Average Rating for Restaurants in UK", main = "Distribution of Average Rating for Restaurants in UK", las=1, cex.main = 0.8, cex.lab=0.8, cex.axis=0.7)
hist(zomato[zomato$country.Name == "UAE",]$Aggregate.rating, breaks = breaks, xlab="Average Rating for Restaurants in UAE", main = "Distribution of Average Rating for Restaurants in UAE", las=1, cex.main = 0.8, cex.lab=0.8, cex.axis=0.7)
# hist(zomato[zomato$country.Name == "South Africa",]$Aggregate.rating, breaks = breaks, xlab="Average Rating for Restaurants in South Africa", main = "Distribution of Average Rating for Restaurants in South Africa", las=1, cex.main = 0.8, cex.lab=0.8)
# hist(zomato[zomato$country.Name == "Brazil",]$Aggregate.rating, breaks = breaks, xlab="Average Rating for Restaurants in South Brazil", main = "Distribution of Average Rating for Restaurants in Brazil", las=1, cex.main = 0.8, cex.lab=0.8)
