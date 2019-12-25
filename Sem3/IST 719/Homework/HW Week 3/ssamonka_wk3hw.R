#
# Author: Satyen Amonkar
# Purpose: Homework 3

my.dir <- "C:/Users/DELL/Documents/GitHub/Syracuse/Sem3/IST 719/Source/"
fname <- paste0(my.dir, "art.csv")
# file.choose()
art <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
str(art)

## Question 1: Is there a relationship between the unit price of art goods and their units sold? 
#              If so, what kind of relationship is it? 

plot(art$units.sold,art$unit.price, col = "cyan"
     , main = "Relationship between unit price & units sold"
     , pch = "*", xlab= "Unit Sold", ylab = "Unit Price")
abline(lm(art$unit.price ~ art$units.sold) , col = "brown"
       , lwd = 3, lty = 3)
rug(x = art$units.sold, side = 1, col = "orange")
rug(x = art$unit.price, side = 2, col = "orange")

#==== Answer 1 ===##
# The scatter plot shows a negative trend between unit price of art goods and their units sold.
# Which means more units are sold with lesser unit price

# units.avg <- aggregate(art$unit.price, list(art$units.sold), mean)
# colnames(units.avg)<-c("units.sold","avg.unit.price")
# barplot(units.avg$avg.unit.price, names.arg = units.avg$units.sold
#         , ylab= "Unit Sold", xlab = "Avg Unit Price"
#         , main = "Q1"
#         , cex.main=0.8
#         # , axis(1,a)
#         , cex.names = 0.5
#         , horiz = TRUE)



## Question 2: Does the art company sell more units of drawing paper or watercolor paper?

boxplot(art$units.sold ~ art$paper
        , col = "cadetblue"
        , main = "Relationship: art sold & paper type"
        , ylab = "Paper Type", xlab = "Art Sold"
        , pch = "*"
        , horizontal = TRUE
        )
abline(v = median(art$units.sold), lty = 2, col = "red")
# The boxplot tells us the distribution of art sold across drawing and watercolor.
# To understand the relationship, we need to use subset of paper type

art.tus<- tapply(art$units.sold,list(art$paper)
          , sum)
class(art.tus)

barplot(art.tus, col = c("red","blue")
        , ylab = "Total Units Sold", xlab= "Paper Type" 
        , main = "Relationship between total units sold and paper types"
        , cex.main = 0.8)
#==== Answer 2 ===##
# Art Company sells more units of watercolor

## Question 3: Does the art company bring in more money (revenue) selling drawing paper or watercolor paper? 
art.tr<- tapply(art$total.sale, list(art$paper), sum)
barplot(art.tr, col = c("red","blue")
        , ylab = "Total Revenue", xlab= "Paper Type" 
        , main = "Q3"
        , ylim = c(0, max(art.tr))
        , cex.main = 0.8
        # , las = 1 
        )

#==== Answer 3 ===##
# Art Company brings more money from watercolor paper as compared to drawing paper

# Question 4: Each paper (watercolor and drawing) has different subtypes. 
#             It is possible that at some stores, some subtypes sell better than others. For drawing paper only, 
# make a plot that allows the viewer to compare which subtypes of drawing paper sell more (and less) units across the stores.

art.drawing <- art[art$paper=="drawing",]
drawingart <- tapply(art.drawing$units.sold, list(art.drawing$paper.type, art.drawing$store), sum)

barplot(drawingart, beside = TRUE
        , legend.text = rownames(drawingart)
        , col = c("blue","green","red","yellow")
        , main = "Paper Sub type by Store"
        # , horiz = T, las = 1
        , args.legend = list(x="topright",inset=c(-0.08,-0.10),xpd = TRUE)
        , bty = "n"
        , cex.sub = 1)

#==== Answer 4 ===##
# For Drawing Paper: Portland has the maximum number of units sold for journals, pads, rolls and sheets. 

###########################
# PLOT #1
par(mfrow = c(2,2))
plot(art$units.sold,art$unit.price, col = "black"
     , main = "Unit Price V/S Unit Sold"
     , pch = "*", xlab= "Unit Sold", ylab = "Unit Price"
     , cex.main = 0.7)
abline(lm(art$unit.price ~ art$units.sold) , col = "brown"
       , lwd = 3, lty = 2)
rug(x = art$units.sold, side = 1, col = "orange")
rug(x = art$unit.price, side = 2, col = "orange")

barplot(art.tus, col = c("red","blue")
        , ylab = "Total Units Sold", xlab= "Paper Type" 
        , main = "Total Units Sold V/S Paper Types"
        , ylim = c(0,max(art.tus))
        , cex.main = 0.7)

barplot(art.tr, col = c("red","blue")
        , ylab = "Total Revenue", xlab= "Paper Type" 
        , main = "Total Revenue V/S Paper Types "
        , ylim = c(0, max(art.tr))
        , cex.main = 0.7
        )
barplot(drawingart, beside = TRUE
        , col = c("blue","green","red","yellow")
        , main = "Paper Sub type by Store"
        # , horiz = T, las = 1
        , xlab = "Store" , ylab = "Total Units Sold"
        , bty = "n"
        , cex.main = 0.7
        , ylim = c(0, max(drawingart))
        # , cex.names = 0.7
        )
legend("topleft"
       ,legend = row.names(drawingart)
       , title = "Paper Sub Type"
       , title.adj=0.35
       , cex = 0.48
       , ncol = 2
       , xpd = FALSE
       # , inset = c(0.1, -0.08)
       , inset = c(0.12, -0.08)
       , bty = "n"
       , fill = c("blue","green","red","yellow") )
###########################

## Question 5: The dataset covers 4 years of data. 
#  Compare the revenue gained each year from the sales drawing paper with that of watercolor paper. 
#  Are sales growing over time for both?
str(art)
art$year

art.yearly <- tapply(art$total.sale, list(art$paper, art$year)
            , sum)

year <- as.numeric(colnames(art.yearly))
# options(scipen = 50)
plot(year, y = art.yearly[1,] 
     , type = "n", lwd = 3, ylim = c(0, max(art.yearly))
     , xlab = "Year", ylab = "Revenue"
     , main = "Sales by Paper Type"
     , xaxt='n')
axis(side = 1, at = seq(2012,2015,by=1)) 

xspline(year, art.yearly[1,], shape = 1, border = "red", lwd = 3)
xspline(year, art.yearly[2,], shape = 1, border = "blue", lwd = 3)

legend("bottomleft", legend = rownames(art.yearly), lwd = 3
       , lty = 1, bty = "n" #lty can also be passed as a vector
       , col = c("red","blue"))
#==== Answer 5 ===##
# Revenue sales are growing over time for both drawing and watercolor papers.