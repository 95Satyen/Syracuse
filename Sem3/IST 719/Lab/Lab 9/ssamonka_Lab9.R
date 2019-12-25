#
# Author: Satyen Amonkar
# Purpose: Lab 9: 

my.dir <- "h:/Desktop/IST 719/"

x <- c(8.11,8.21,8.11,8.33,8.34,8.11,7.91,7.79,8.68,9.82,10.33,8.66,7.62,7.5,7.64,6.32,5.75,0,0.11,1.2,0.96,2.72,3.01,3.93,4.58,4.52,4.68,4.47,4.64,4.95,5.36,6.27,8.11)

y <- c(7.78,6.78,6.35,5.23,4.34,1.87,1,0.54,0.77,1.15,1.02,0.21,0,0.23,0.75,1.52,2.41,2.54,3.05,3.94,4.71,4.73,4.66,4.8,5.15,5.66,5.85,5.99,6.43,6.58,7.07,7.72,7.78)

plot(x,y,type = "l") #maps are just set of points and lines


load(paste0(my.dir,"shootings.rda")) #rda is standard r object

View(shootings)
colnames(shootings)
shootings$State
table(shootings$State) #data is messy

#gsub("^\\c+|\\c+$","",c(" sat yen", "satyen ")) #practising

gsub("^\\s+|\\s+$","",shootings$State) #^ check only begining of string, \\s is special char, + is one or more, | is or, \\ is a escape char, used to make regular char to special char, $ is the end

shootings$State <- gsub("^\\s+|\\s+$","",shootings$State)

agg.dat <- aggregate(shootings$Total.Number.of.Victims
                     , list(shootings$State)
                     , sum)
agg.dat # all should be right justified
colnames(agg.dat) <- c("state","victims")
table(shootings$State)

#10 colors such that least shootings has light red and most has dark red
#num.cols <- 100 more slices .. max 255 colors
num.cols <- 10
my.col.vec <- rev(heat.colors(num.cols))
pie(rep(1,num.cols), col=my.col.vec)
my.col.vec[3]
my.col.vec[c(3,7,1,1,9)] #we can select colors in any order
pie(c(1,1,1,1,1),col = my.col.vec[c(3,7,1,1,9)])
range(agg.dat$victims)
tmp <- agg.dat$victims/max(agg.dat$victims) #rescaling, scales data to 0-1 range
tmp2 <- 1 + round((num.cols-1)*tmp,0) # round scales value 0-9, we need min 1 value for color tmp2 1-10, if we have num.cols then tmp2 will be 1-14
tmp2
agg.dat$color <- my.col.vec[tmp2] #assigning colors to new column
agg.dat# california has the highest value and darkest red, therefore validated
install.packages('maps')
library(maps)
map("state")
m <- map("state") #passing a name of the db, returns object
m
plot(m$x,m$y)
m$names# we need to match all washington with our dataset
state.order <- match.map(database = "state"
                         , regions = agg.dat$state
                         , exact = FALSE # there are different washington islands
                         , warn = TRUE)
state.order # NAs tell that either there are states in our data set not present in states or vice versa

install.packages('mapproj')
library(mapproj)
map("state",col=agg.dat$color[state.order]
    , fill = TRUE, resolution = 0, lty = 1
    , projection = "polyconic", border = "tan") #projection handles distortion, make shapes of states little more like they are on earth, for general audience it is not required
mtext(text = "SAVE THIS MAP AS A PDF!"
      , SIDE = 3, LINE =1, cex = 2.5)
m <- map("county","new york")
m$names
map()
#US cities data set
head(us.cities)
map("state")
install.packages('plotrix')
library(plotrix)
my.cols <- rep(rgb(1,.6,.2,.7)
               , length(us.cities$name))
my.cols[us.cities$capital>0] <- rgb(.2,.6,1,.9)
points(us.cities$long, us.cities$lat
       , col = my.cols
       , pch = 16
       , cex = rescale(us.cities$pop, c(.5,7))) #bubble chart
#ggmap geocoding points is googles geocoding engine cheap paid geocoding
install.packages('rworldmap')
library(rworldmap)
countries <- read.delim(paste0(my.dir,"countries.csv")
                        , quote = "\""
                        , header = TRUE
                        , sep = ";"
                        , stringsAsFactors = FALSE)
colnames(countries)
range(countries$Life.expectancy) # Life expectancies starts with 0?
countries$Life.expectancy #some countries show 0 life expectancy. some countries dont record this data
countries <- countries[countries$Life.expectancy > 0 ,]
range(countries$Life.expectancy) # range is now more meaningful
#color countries based on life expectancy
num.cats <- 10
colnames(countries) # country..en. gives english names of country
countries$Country..en.
iso3.codes <- tapply(countries$Country..en.
                     , 1:length(countries$Country..en.)
                     , rwmGetISO3) #shortcut to for loops

iso3.codes <- rep("", length(countries$Country..en.))
for(i in 1:length(countries$Country..en.)){
  iso3.codes[i] <- rwmGetISO3(countries$Country..en.[i])
  print(paste(i, countries$Country..en.[i], iso3.codes[i]))# print is to validate
}

iso3.codes # converts english names to iso country codes in same order as data

df <- data.frame(country = iso3.codes #created column country
                 , labels = countries$Country..en.
                 , life = countries$Life.expectancy)

df.map <- joinCountryData2Map(df, joinCode = "ISO3"
                              , nameJoinColumn = "country") #column name to join data on

mapCountryData(df.map
    , nameColumnToPlot = "life"
    , numCats = num.cats
    , catMethod = "fixedwidth"
    , colourPalette =colorRampPalette(c("orangered"
                         ,"palegoldenrod"
                         ,"forestgreen"))(num.cats)
    , oceanCol = "royalblue4"
    , borderCol = "white"
    , missingCountryCol = "peachpuff4"
    , mapTitle = "life expectancy")

