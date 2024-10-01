# install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

# mpg: Fuel economy data from 1999 to 2008 for 38 popular models of cars

data(mpg)
glimpse(mpg)
View(mpg)
names(mpg)

## Box plot
# Basic box plot
p <- ggplot(mpg, aes(x=class, y=displ)) + geom_boxplot( )+ coord_flip()
p


# Change outlier, color, shape and size
ggplot(mpg, aes(x=class, y=displ)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


###Change box plot colors by groups for one variable
ggplot(mpg, aes(x=class, y=displ,color=class)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



# Box plot with multiple variables
  ggplot(mpg, aes(x=class, y=displ,fill=as.factor(drv))) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=2)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    labs(fill="DRV")+   # set the legend's title
    theme(legend.position = c(0.5, 0.9),   # Set legend position as numeric vector c(x, y) or "top"
          legend.direction = "horizontal")