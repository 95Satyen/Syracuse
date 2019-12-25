# Quiz 5: 

#Q3

my.dir <- "C:/Users/DELL/Documents/GitHub/Syracuse/Sem3/IST 719/Source/"
fname <- paste0(my.dir, "sales.csv")
sales <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)

salesSub <- subset(sales,year == 2010 & type == "white")

salessum <- tapply(salesSub$units.sold, list(salesSub$sales.rep,salesSub$rep.region), sum)
salesdf <- as.data.frame(salessum)

# Central Region
CR <- salesdf[which.max(salesdf$Central),]
row.names(CR) # "Dannette Saltsman"

# East Region
ER <- salesdf[which.max(salesdf$East),]
row.names(ER) # "Aiko Hamsher"

# North Region
NR <- salesdf[which.max(salesdf$North),]
row.names(NR) # "Clement Arias"

# South Region
SR <- salesdf[which.max(salesdf$South),]
row.names(SR) # "Jewel Copas"

# West Region
WR <- salesdf[which.max(salesdf$West),]
row.names(WR) # "Deloras Mcfetridge"


#Q4
salesSub2 <- subset(sales,year==2012)
salessum2 <- tapply(salesSub2$recipt, list(salesSub2$wine, salesSub2$rep.region), sum)
salesdf2 <- as.data.frame(salessum2)

# Central Region
CR2<- salesdf2[which.max(salesdf2$Central),]
row.names(CR2) #  "Merlot"

# East Region
ER2<- salesdf2[which.max(salesdf2$East),]
row.names(ER2) #  "Pinot Gris"

# North Region
NR2<- salesdf2[which.max(salesdf2$North),]
row.names(NR2) #  "Chardonnay"

# South Region
SR2<- salesdf2[which.max(salesdf2$South),]
row.names(SR2) #  ""Merlot"

# West Region
WR2<- salesdf2[which.max(salesdf2$West),]
row.names(WR2) #  "Merlot"
