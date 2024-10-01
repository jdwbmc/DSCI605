if (!require(tidyverse)) {install.packages('tidyverse')}; require(tidyverse)
if (!require(readxl)) {install.packages('readxl')}; require(readxl)
if (!require(colorspace))  {install.packages(colorspace)}; require(colorspace)
if (!require(gridExtra))  {install.packages("gridExtra")}; require(gridExtra)

setwd("~/GitHub/DSCI605/Mod8")
land = readxl::read_xlsx("Crop_Range_GOES0901_CountJday.xlsx")

key=length(seq(235,243))
co1 = colorspace::diverge_hcl(key)

#Define export image
png(file="Crop_Arrange.png",
  width=800, height=500)

#Histogram
p1 <- land %>% filter(jday %in% seq(235,243)) %>%
  ggplot(aes(FDCount)) +
  geom_histogram(binwidth=5, color='black', fill="blue") +
  labs(x="FDCount") +
  ggtitle("Histogram")

#Scatter plot

p2 <-  land %>% filter(jday %in% seq(235,243)) %>%
  ggplot(aes(gmt, FDCount)) +
  geom_point(size = 1.5, color='blue') +
  labs(x="GMT time") + labs(y="Count") +
  ggtitle("Scatter Plot")

#Box plot
p3 <- land %>% filter(jday %in% seq(235,243)) %>%
  ggplot(aes(x=as.factor(jday), y=FDCount, fill=as.factor(jday))) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(fill="Julian Day") +
  ggtitle("Boxplot")

#Facets
p4 <- land %>% filter(jday %in% seq(235,243)) %>%
  ggplot() +
  geom_point(aes(x=gmt, y=FDCount), color="blue") +
  scale_color_manual(values=co1) +
  scale_x_continuous(breaks=seq(1,24,11), limits=c(1,24)) +
  labs(color='Julianday') + labs(x='Local Time') + labs(y="Count") +
  facet_wrap(~jday, nrow=1) +
  ggtitle("Facets")

#Generate a ggplot2 plot grob

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)

grid.arrange(g1,g2,g3,g4,widths=c(2,2,1),
             layout_matrix = rbind(c(1,2,3),
                                   c(4,4,4)))

dev.off()
