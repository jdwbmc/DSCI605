library(tidyverse)
library(hrbrthemes)
library(viridis)

setwd("~/GitHub/DSCI605/Mod7")

data(mpg)
glimpse(mpg)
view(mpg)
names(mpg)

# The most basic scatter plot

mpg %>%
  arrange(desc(class)) %>%
  ggplot(aes(x=cty, y=displ, fill=class, shape=as.factor(cyl))) +
  geom_point(alpha=0.5, color="black") +
  scale_fill_viridis(discrete=TRUE, option="A") +
  ylab("Engine displacement, in liters (displ)") +
  xlab("City miles per gallon (cty)")


mpg %>%
  arrange(desc(class)) %>%
  ggplot(aes(x=cty, y=displ, fill=class, shape=as.factor(cyl))) +
  geom_point(alpha=0.5, color="black") +
  scale_fill_viridis(discrete=TRUE, option="A") +
  guides(fill = FALSE, shape=FALSE) +
  # theme(legend.position = "none") +
  ylab("Engine displacement, in liters (displ)") +
  xlab("City miles per gallon (cty)")


mpg %>%
  arrange(desc(class)) %>%
  ggplot(aes(x=cty, y=displ, fill=class, shape=as.factor(cyl))) +
  geom_point(alpha=0.5, color="black") +
  scale_fill_viridis(discrete=TRUE, option="A") +
  # guides(fill = FALSE, shape=FALSE) +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title = "users by guides")) +
  ylab("Engine displacement, in liters (displ)") +
  xlab("City miles per gallon (cty)")


mpg %>%
  arrange(desc(class)) %>%
  ggplot(aes(x=cty, y=displ, fill=class, shape=as.factor(cyl))) +
  geom_point(alpha=0.5, color="black") +
  scale_fill_viridis(discrete=TRUE, option="A") +
  # guides(fill = FALSE, shape=FALSE) +
  theme(legend.position = "bottom") +
  guides(shape=guide_legend(nrow=2, byrow=TRUE, title="CYL")) +
  guides(fill=guide_legend(nrow=2, byrow=TRUE, title="Class")) +
  ylab("Engine displacement, in liters (displ)") +
  xlab("City miles per gallon (cty)")


# Most basic bubble chart

mpg %>%
  arrange(desc(class)) %>%
  ggplot(aes(x=cty, y=displ, size=cyl, fill=class)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range=c(.1,10), name="Model") +
  scale_fill_viridis(discrete=TRUE, option="A") +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  guides(size=guide_legend(nrow=2, byrow=TRUE, title="CYL")) +
  guides(fill=guide_legend(nrow=2, byrow=TRUE, title="Class")) +
  ylab("Engine displacement, in liters (displ)") +
  xlab("City miles per gallon (cty)")



