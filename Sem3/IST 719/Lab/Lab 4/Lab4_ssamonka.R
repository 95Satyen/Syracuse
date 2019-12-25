#
# Author: Satyen Amonkar
#Purpose: Lab 5: wine
#plot to work on in illustrator 
my.dir <- "C:/Users/DELL/Documents/GitHub/Syracuse/Sem3/IST 719/Source/"
fname <- paste0(my.dir, "sales.csv")

sales <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)

colnames(sales)
str(sales)
View(sales)

pie(c(1,2), col= c("#A40017","#FFD31E"))

library(RColorBrewer)

display.brewer.all()
rand.data <- replicate(8
                       ,rnorm(27, 27, sd = 1.5))
boxplot(rand.data, col = brewer.pal(8, "BrBG")
        , main = "Satyen: BrBG")
# boxplot(rand.data, col = brewer.pal(8, "PiYG")
#         , main = "Satyen: BrBG") # 8 colors for 8 different plots
num.colors <- 8

FUN <- colorRampPalette(c("red","blue", "gold"))
?colorRampPalette
my.cols <- FUN(num.colors)
boxplot(rand.data, col = my.cols
        , main = "Satyen: COl Ramp")

plot(sales$expenses, sales$recipt, pch = 16
     , cex = 1)

col.vec <- rep(rgb(30, 144, 255,
                   maxColorValue = 255)
               , nrow(sales)) #create vector for 100000 points 

plot(sales$expenses, sales$recipt, pch = 16
     , cex = 1, col = col.vec)

col.vec[sales$rep.sex ==1] <- rgb(255, 64, 64
                                  , maxColorValue = 255) #find male = 1 and change it to red


plot(sales$expenses, sales$recipt, pch = 16
     , cex = 1, col = col.vec)


col.vec <- rep(rgb(30, 144, 255,
                   maxColorValue = 255)
               , nrow(sales))

col.vec[sales$type == "red"] <- rgb(255, 64, 64
                                  , maxColorValue = 255) # find all the red wine to and change color
#from blue to red 

plot(sales$expenses, sales$recipt, pch = 16
     , cex = 1, col = col.vec)


col.vec <- rep(rgb(30, 144, 255,
                   maxColorValue = 255)
               , nrow(sales))

col.vec[sales$unit.price > 14] <- rgb(255, 64, 64
                                    , maxColorValue = 255) 

plot(sales$expenses, sales$recipt, pch = 15
     , cex = 1, col = col.vec)

hist(sales$unit.price)

plot(sales$expenses, sales$recipt, pch = 16, cex = 1)

plot(sales$expenses, sales$recipt, pch = 16, cex = 0.3)

overplot.col <- rgb(.8,.15,.15,alpha = .4)
plot(sales$expenses, sales$recipt, pch = 16
     , cex = 0.6, col = overplot.col
     , main = "over ploting") #rgb function give the tranparent of the point 
agg.dat <- aggregate(sales$units.sold
                     , list(type = sales$type
                            , wine = sales$wine)
                            , sum)
barplot(agg.dat$x, names.arg = agg.dat$wine)
wine.cols <- c(rgb(255,240,150, maxColorValue = 255)
                , rgb(160,30,65,maxColorValue = 255 ))
pie(c(1,1), col = wine.cols)

bar.cols <- rep('white', nrow(agg.dat))
bar.cols[agg.dat$type == "red"] <- wine.cols[2]
bar.cols[agg.dat$type == "white"] <- wine.cols[1]

par(mar = c(5,10,4,1)) # c(bottom, left, top, right) margins
barplot(agg.dat$x, names.arg = agg.dat$wine
        , col = bar.cols
        , border = NA
        , horiz = TRUE
        , las = 1 #moving to horizontal for the text
        , main = "Units Sold")

agg.dat.sales <- aggregate(sales$recipt
                     , list(type = sales$type
                            , wine = sales$wine)
                     , sum)
agg.dat.units <- aggregate(sales$units.sold
                     , list(type = sales$type
                            , wine = sales$wine)
                     , sum)

colnames(agg.dat.sales) <- c("type","wine","recipts")
agg.dat.sales$units <- agg.dat.units$x #create a new dataframe call agg.dat.sales

plot(agg.dat.sales$units, agg.dat.sales$recipts
     , pch = 16)



options(scipen = 999)
# install.packages("png")
library(png)
my.dir
ima <- readPNG(paste0(my.dir,"bottles.png"))
r1 <- readPNG(paste0(my.dir,"r1.png"))
w1 <- readPNG(paste0(my.dir,"w1.png"))

agg.dat.sales$col <- bar.cols


pch <- rep("W",7)
pch[agg.dat.sales$type == "red"] <- "R"
plot(agg.dat.sales$units, agg.dat.sales$recipts
     , pch = pch, cex = 2, col = bar.cols
     , xlim = c(0, (1.5 * max(agg.dat.sales$units)))
     , ylim = c(0,(1.5*max(agg.dat.sales$recipts)))
     , bty = "n"
     , xlab = "units sold", ylab = "receipt"
     , main = "IST 719 simulated wine data")

lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3],
            lim$usr[2],
            lim$usr[4])

rect(lim$usr[1], lim$usr[3],
                 lim$usr[2],
                 lim$usr[4], 
                 col = rgb(1,1,1,.85), border = "white") #give a shadow to the pictute

r1.x1 <- agg.dat.sales$units[agg.dat.sales$type == "red"]
r1.x2 <- r1.x1 +3000
r1.y1 <- agg.dat.sales$recipts[agg.dat.sales$type == "red"]
r1.y2 <- r1.y1 +65000
rasterImage(r1,r1.x1,r1.y1,r1.x2,r1.y2 )# where r1 is the image


w1.x1 <- agg.dat.sales$units[agg.dat.sales$type == "white"]
w1.x2 <- w1.x1 +3000
w1.y1 <- agg.dat.sales$recipts[agg.dat.sales$type == "white"]
w1.y2 <- w1.y1 +65000
rasterImage(w1,w1.x1,w1.y1,w1.x2,w1.y2 )
text(agg.dat.sales$units + 3000, agg.dat.sales$recipts + 10000
     , labels = agg.dat.sales$wine, adj = 0
     , cex = 1.2)
points(agg.dat.sales$units, agg.dat.sales$recipts
       , pch = 8, col = "blue", cex = 3)
