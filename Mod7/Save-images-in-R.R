
library(ggplot2)
#setwd("C:/Users/ali3/OneDrive - Ball State University/Desktop/Courses/Coursera/DSCI605/Week6")
setwd("~/GitHub/DSCI605/Mod7")
df <- read.csv("Income-by-states.csv")

#plot the basic histogram
ggplot(df, aes(x=income)) + 
  geom_histogram(binwidth=10000,color="black", fill="blue")

#save as jpeg
jpeg(file="Histogram_jep.jpeg",width = 800, height = 1000,res=300)
ggplot(df, aes(x=income)) + 
  geom_histogram(binwidth=5000,color="black", fill="blue")
dev.off()




#save as png
png(file="Histogram_png.png",
    width=600, height=350)
ggplot(df, aes(x=income)) + 
  geom_histogram(binwidth=5000,color="black", fill="blue")
dev.off()
#save as bmp: specify ppi with res
bmp(file="Histogram_bmp.bmp",
    width=6, height=4, units="in", res=300)
#    width=1200, height=800, units="px")
ggplot(df, aes(x=income)) + 
  geom_histogram(binwidth=5000,color="black", fill="blue")
dev.off()
#save as tiff
tiff(file="Histogram_tif.tiff",
#     width=6, height=4, units="in")
      width=1200, height=800, units="px")
ggplot(df, aes(x=income)) + 
  geom_histogram(binwidth=5000,color="black", fill="blue")
dev.off()







#save into a pdf
pdf(file="Histogram.pdf",width = 8, height = 10)
ggplot(df, aes(x=income)) + 
  geom_histogram(binwidth=5000,color="black", fill="blue")

ggplot(df, aes(x=income)) + 
  geom_histogram(binwidth=5000,color="black", fill="red")

ggplot(df, aes(x=income)) + 
  geom_histogram(binwidth=5000,color="black", fill="green")
dev.off()

#save as jpeg
jpeg(file="Scatter_jep.jpeg",width = 800, height = 1000,res=300)
# Create the scatter plot
# Aggregate total income by state
state_income_totals <- df %>%
  group_by(State) %>%
  summarize(Total_Income = sum(income, na.rm = TRUE))

# Create the histogram
ggplot(state_income_totals, aes(x=State)) + 
  geom_histogram(binwidth=1000000, color="black", fill="blue") +
  labs(x = "Total Income", y = "Number of States", title = "Histogram of Total Income by State") +
  theme_minimal()
dev.off()