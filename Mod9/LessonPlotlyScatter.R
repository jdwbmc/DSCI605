library(tidyverse)
library(plotly)

plot_ly(data=iris, x= ~Sepal.Length, y= ~Petal.Length,
        color= ~Species,
        type='scatter', mode='markers')

plot_ly(data=iris, x= ~Sepal.Length, y= ~Petal.Length) %>%
  add_markers()
#Box
plot_ly(iris, x= ~Petal.Width, color= ~Species, type="box")

#Bar Plot
categories <- c("Apples", "Oranges", "Bananas")
values <- c(15,10,8)
plot_ly(x=categories, y=values, type="bar")

#Pipe operation
iris %>% plot_ly(x= ~Sepal.Length, y= ~Sepal.Width,
                 type="scatter", mode="markers",
                 #Hover text:
                 text = ~paste("Length: ", Sepal.Length, '<br>Width: ',Sepal.Width),
                 color = ~Species, size= ~Sepal.Length) %>%
  layout(title="Length vs Width", plot_bgcolor='#e5ecf6',
         xaxis=list(title="Sepal Length"),
         yaxis=list(title="Sepal Width"))

#ggplotly

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000),]

p <- ggplot(data = d, aes(x=carat, y=price)) +
  geom_point(aes(text= paste("Clarity:", clarity)), size=1) +
  geom_smooth(aes(colour=cut, fill=cut)) + facet_wrap(~ cut)
ggplotly(p)


