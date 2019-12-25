#
# Author: Satyen Amonkar
# Purpose: Lab 6: 

install.packages('tidyverse')
library('tidyverse')
?mpg
str(mpg)

ggplot(data=mpg) + geom_point(mapping = aes(x=displ, y=hwy))

ggplot() + geom_point(data = mpg, mapping = aes(x=displ, y=hwy)) + geom_point() + geom_line()

#Not Recommended
ggplot(data = mpg, mapping = aes(x=displ,y=hwy))+geom_point(mapping = aes(x=displ,hwy)) + geom_line()

#NR
ggplot(mpg) + geom_point(aes(displ,hwy)) + theme(axis.title = element_text(size = 20), axis.line=element_line(size=1,arrow = arrow()))

ggplot(mpg) + geom_point(aes(displ,hwy)) + theme(axis.title = element_text(size = 20), axis.line=element_line(size=1,arrow = arrow()),
                                    axis.line.y=element_line(size=3,arrow=arrow()))

ggplot(mpg) + geom_point(aes(x=displ,y=hwy, color=class))

ggplot(mpg) + geom_point(aes(x=displ,y=hwy, alpha=class))

ggplot(mpg) + geom_point(aes(x=displ,y=hwy, shape=class))

ggplot(mpg) + geom_point(aes(x=displ,y=hwy, size=class))

#NR to keep all aesthetics in one aes
ggplot(mpg) + geom_point(aes(x=displ,y=hwy, shape=class,alpha=class,color=class))

#visual aesthetics can be moved out of aes
ggplot(mpg) + geom_point(aes(x=displ,y=hwy),color="red")

#incase of wrong no of paranthesis press esc to get out of command prompt
ggplot(mpg) + geom_point(aes(x=displ),color="white",y=1)

#color is considered as the first factor within aes. since 1 factor value corresponds to red color by default, red appears. Be careful of paranthesis are balanced
ggplot(mpg) + geom_point(aes(x=displ,y=hwy,color="blue"))

ggplot(mpg) + geom_smooth(aes(displ,hwy))

ggplot(mpg,aes(displ,hwy)) + geom_smooth() + geom_point()

ggplot(mpg,aes(displ,hwy)) + geom_smooth() + geom_point(aes(color=class))

ggplot(mpg) + geom_bar(aes(x=class))

?geom_bar #uses stat count by default, if have aggregated things then use stat identity

ggplot(mpg) + stat_count(aes(x=class))


ggplot(mpg) + stat_count(aes(x=class, fill=trans)) #never to do

ggplot(mpg) + stat_count(aes(x=class, fill=trans), position = "dodge")

ggplot(mpg) + stat_count(aes(x=class, fill=trans), position = position_dodge(preserve = "single")) #preserves values

## Mapping ##

f <- colorRampPalette(c("blue","yellow","red"))
f(7) #function

types <- unique(mpg$trans)

pal = f(length(types))

ggplot(mpg) + stat_count(aes(x=class,fill=trans))+scale_fill_manual(values=pal)

ggplot(mpg) + stat_count(aes(x=class,fill=trans))+scale_fill_brewer(palette = "Set3") #std palletes
ggplot(mpg) + stat_count(aes(x=class,fill=trans))+scale_fill_brewer(palette = "BuGn") #std palletes
# it drops pallet if no of color is less
ggplot(mpg) + stat_count(aes(x=class,fill=trans))+scale_fill_brewer(palette = pal)

install.packages("viridis") # good for different kind of color blindness and grey scale
library(viridis)

ggplot(mpg) + stat_count(aes(x=class,fill=trans))+scale_fill_viridis(discrete = TRUE) #std palletes
#It by default gives the number of colors required as it is a dicrete value

ggplot(mpg) + stat_count(aes(x=class,fill=trans))+scale_fill_viridis(discrete = TRUE, option = "magma")

ggplot(mpg) + geom_point(aes(displ,hwy,color=trans),size=4) +scale_color_viridis(option="magma", discrete = TRUE) # data is kinda curved

ggplot(mpg) + geom_point(aes(displ,hwy,color=trans),size=4) +scale_color_viridis(option="magma", discrete = TRUE) + scale_y_log10()# scale y axis

ggplot(mpg) + geom_point(aes(displ,hwy,color=trans),size=4) +scale_color_viridis(option="magma", discrete = TRUE) + scale_y_log10() + coord_flip() + ggtitle("Highway MPG x Displacement") + xlab("Displacement") + ylab("Highway MPG") # by default title is left justified

ggplot(mpg) + geom_point(aes(displ,hwy,color=trans),size=4) +scale_color_viridis(option="magma", discrete = TRUE) + scale_y_log10() + coord_flip() + ggtitle("Highway MPG x Displacement") + xlab("Displacement") + ylab("Highway MPG") + theme(plot.title = element_text(hjust = 0.7, face="bold", family = "serif", size=8)) 

ggplot(mpg) + 
  geom_point(aes(displ,hwy,color=trans),size=4) +
  scale_color_viridis(option="magma", discrete = TRUE) + scale_y_log10() + 
  coord_flip() + ggtitle("Highway MPG x Displacement") + xlab("Displacement") + ylab("Highway MPG") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold", family = "serif", size=8),legend.position = "none") + facet_wrap(.~trans) #facet wrap takes data and wraps wrt axis

ggplot(mpg) + 
  geom_point(aes(displ,hwy,color=trans),size=4) +
  scale_color_viridis(option="magma", discrete = TRUE) + scale_y_log10() + 
  coord_flip() + ggtitle("Highway MPG x Displacement") + xlab("Displacement") + ylab("Highway MPG") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold", family = "serif", size=8),legend.position = "none") + facet_grid(cyl~trans) #facet wrap takes data and wraps wrt axis

g <- ggplot(mpg) + 
  geom_point(aes(displ,hwy,color=trans),size=4) +
  scale_color_viridis(option="magma", discrete = TRUE) + scale_y_log10() + 
  coord_flip() + ggtitle("Highway MPG x Displacement") + xlab("Displacement") + ylab("Highway MPG") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold", family = "serif", size=8),legend.position = "none") + facet_grid(cyl~trans) #facet wrap takes data and wraps wrt axis

g

ggsave("h:/Desktop/IST 719/Lab 5/mpg_class.pdf",g,width=8,height=6)
