library(plotly)
#Line
plot_ly ( x = c( 1, 2, 3 ), y = c( 5, 6, 7), type = 'scatter' , mode = 'lines' )
#Scatter
plot_ly ( x = c( 1, 2, 3 ), y = c( 5, 6, 7 ), type = 'scatter' , mode = 'markers' )
#Bar
plot_ly ( x = c( 1, 2, 3), y = c( 5, 6, 7), type = 'bar' , mode = 'markers' )
#Bubble
plot_ly ( x = c( 1, 2, 3 ), y = c( 5, 6, 7 ), type = 'scatter' , mode = 'markers' , size = c( 1, 5, 10 ), marker = list( color = c( 'red', 'blue', 'green' )))
#Area
plot_ly ( x = c( 1, 2, 3 ), y = c( 5, 6, 7 ), type = 'scatter' , mode = 'lines' , fill = 'tozeroy' )
#Legends
set.seed( 123 )
  x = 1 : 100
  y1 = 2*x + rnorm ( 100 )
  y2 = -2*x + rnorm ( 100 )
plot_ly ( x = x , y = y1 , type = 'scatter' ) %>% 
  add_trace( x = x , y = y2 ) %>%
  layout( legend = list( x = 0.5 , y = 1 , bgcolor = '#F3F3F3' ))
#Axes
set.seed( 123 )
  x = 1 : 100
  y1 = 2*x + rnorm( 100 )
  y2 = -2*x + rnorm( 100 )
axis_template <- list(
  showgrid = F ,
  zeroline = F ,
  nticks = 20 ,
  showline = T ,
  title = 'AXIS' ,
  mirror = 'all' )
plot_ly ( x = x , y = y1 ,
          type = 'scatter' ) %>%
  layout(
    xaxis = axis_template ,
    yaxis = axis_template )
#Histogram
x <- rchisq(100,5,0)
plot_ly(
  x=x,
  type='histogram')
#Box Plot
plot_ly(
  y=rnorm(50),
  type='box') %>%
  add_trace(y=rnorm(50,1))
#2D Histogram (Heat Map)
plot_ly(
  x=rnorm(1000, sd=10),
  y=rnorm(1000, sd=5),
  type='histogram2d')
#Bubble Map
plot_ly(
  type='scattergeo',
  lon=c(-73.5, 151.2),
  lat=c(45.5, -33.8),
  marker=list(
    color=c('red','blue'),
    size=c(30,50),
    mode='markers'))
#Choropleth Map
plot_ly(
  type='choropleth',
  locations=c('AZ','CA','VT'),
  locationmode='USA-states',
  colorscale='Viridis',
  z=c(10,20,40)) %>%
  layout(geo=list(scope='usa'))
#Scatter Map - Where was the text?
plot_ly(
  type='scattergeo',
  lon=c(42,39),
  lat=c(12,22),
  text=c('Rome','Greece'),
  mode='markers')
#3D Surface Plots
#Using a dataframe
plot_ly(
  type='surface',
  z=~volcano)
#3D Line Plots
plot_ly(
  type='scatter3d',
  x=c(9,8,5,1),
  y=c(1,2,4,8),
  z=c(11,8,15,3),
  mode='lines')
#3D Scatter Plots
plot_ly(
  type='scatter3d',
  x=c(9,8,5,1),
  y=c(1,2,4,8),
  z=c(11,8,15,3),
  mode='markers')

