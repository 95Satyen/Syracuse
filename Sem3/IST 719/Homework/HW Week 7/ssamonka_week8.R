#
# Author: Satyen Amonkar
# Purpose: HW 7:


# Load the data
my.dir <- "C:/Users/DELL/Documents/GitHub/Syracuse/Sem3/IST 719/Source/"
fname <- paste0(my.dir, "sales.csv")
# file.choose()
sales <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)

recipts <- tapply(sales$recipt, list(sales$sales.rep), sum)
units <- tapply(sales$units.sold, list(sales$sales.rep), sum)
expenses <- tapply(sales$expenses, list(sales$sales.rep), sum)       
feedback <- tapply(sales$rep.feedback, list(sales$sales.rep), mean)

M1 <- rbind(recipts, units, expenses, feedback)
M2 <- cbind(recipts, units, expenses, feedback)
M1
M2
head(M2)

# Using M1, make a heatmap (see page 229, VT). Use something other than the defaults (colors and other settings).
heatmap(M1, Rowv = NA, Colv = NA, col= heat.colors(256), scale="column"
        , margins = c(10,10))
red_colors <- c("#ffd3cd", "#ffc4bc", "#ffb5ab",
                "#ffa69a", "#ff9789", "#ff8978", "#ff7a67", "#ff6b56",
                "#ff5c45", "#ff4d34")

newcols <- c("#64f700","#5de600",  "#56d500"
             , "#50c400", "#49b300", "#42a200","#3b9100","#348000","#2d6f00","#2d6f00")
heatmap(M1, Rowv = NA, Colv = NA, col= newcols, scale="column",
        margins = c(8,5), main="")
mtext("Sales Statistic per Representative", side=3, adj= 0.25, line= 3, cex=1, font=2) 

class(M1)
View(M1) 
library(RColorBrewer)
bball_heatmap <- heatmap(M1, Rowv=NA,
                         Colv=NA, col = brewer.pal(10, "RdGy"),
                         scale="column", margins=c(8,5)
                          ) #we need variation between the columns feedback, expense,etc

# Using M2, make a set of Chernoff Faces (see pg 238 VT). See what modifications you can make.
install.packages('aplpack')
library(aplpack)
faces(M2,  face.type = 2, print.info = TRUE, cex = 0.7, col.hair = brewer.pal(10, "Greys"))
# modified item       Var       
# "height of face   " "recipts" 
# "width of face    " "units"   
# "structure of face" "expenses"
# "height of mouth  " "feedback"
# "width of mouth   " "recipts" 
# "smiling          " "units"   
# "height of eyes   " "expenses"
# "width of eyes    " "feedback"
# "height of hair   " "recipts" 
# "width of hair   "  "units"   
# "style of hair   "  "expenses"
# "height of nose  "  "feedback"
# "width of nose   "  "recipts" 
# "width of ear    "  "units"   
# "height of ear   "  "expenses"
View(M2)
?faces()
# Using M2, make a set of star charts (see page 245, VT). See if you can modify colors and other elements of the plots.
par(mar=c(7,1,1,1))
stars(M2, flip.labels=FALSE, key.loc = c(15, 1.5), draw.segments=TRUE, cex=0.4,col.segments = c("Orange","Green","Blue","Yellow"))

# Using M2, make a parallel coordinates plot (see page 252, VT). Again, make modifications from the default.
library(lattice)
parallel(M2, horizontal.axis=FALSE, col=brewer.pal(9, "Blues"))
