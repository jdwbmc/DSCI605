if (!require(tidyverse)) {install.packages('tidyverse')}; require(tidyverse)
if (!require(readxl)) {install.packages('readxl')}; require(readxl)
if (!require(colorspace))  {install.packages(colorspace)}; require(colorspace)

#Read data
setwd("~/GitHub/DSCI605/Mod8")
crimes = readxl::read_xlsx("Crimes.xlsx")

co1 = colorspace::diverge_hcl(12)
#Scatter plot with facets
png(file="crime0.png",
    width=800, height=500)
crimes %>% filter(State == "Idaho"|
                    State == "Indiana"|
                    State == "NewYork"|
                    State == "Illinois"|
                    State == "California") %>%
  ggplot() +
  geom_point(aes(x=Population/10^6, y=Crimecases/10^5, color=as.factor(Year)), size=3) +
  scale_x_continuous(breaks=seq(1,40,10), limits=c(0,40)) +
  scale_y_continuous(breaks=seq(1,2,1), limits=c(0,2)) +
  scale_color_manual(values=co1) +
  labs(color = 'Population vs Crime') + labs(x='Population/10^6')+labs(y='Crimecases/10^5') +
  facet_wrap(~as.factor(Year), ncol=3)
dev.off()