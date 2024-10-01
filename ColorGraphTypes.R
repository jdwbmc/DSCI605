library(tidyverse)

data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(23, 45, 56, 78)
)

# Create a bar plot using warm colors
p <- ggplot(data, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "orange", "yellow", "darkred")) +
  theme_minimal() +
  labs(title = "Bar Plot with Warm Colors", x = "Category", y = "Value")

# Save the plot as a JPEG file
ggsave("warm_colors_plot.jpeg", plot = p, width = 8, height = 6, dpi = 300)

# Create a sample data frame
data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(23, 45, 56, 78)
)

# Create the plot with cool colors (excluding green)
p <- ggplot(data, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "cyan", "violet", "navy")) +
  theme_minimal() +
  labs(title = "Bar Plot with Cool Colors", x = "Category", y = "Value")

# Save the plot as a JPEG file
ggsave("cool_colors_plot.jpeg", plot = p, width = 8, height = 6, dpi = 300)

# Create a sample data frame
data <- data.frame(
  category = c("A", "B", "C", "D", "E", "F"),
  value = c(23, 45, 56, 78, 62, 34)
)

# Create the plot with a rainbow of warm and cool colors
p <- ggplot(data, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue", "purple")) +
  theme_minimal() +
  labs(title = "Bar Plot with Rainbow Colors", x = "Category", y = "Value")

# Save the plot as a JPEG file
ggsave("rainbow_colors_plot.jpeg", plot = p, width = 8, height = 6, dpi = 300)# Create a sample data frame

data <- data.frame(
  category = c("A", "B", "C", "D", "E", "F"),
  value = c(23, 45, 56, 78, 62, 34)
)

# Create the plot with a rainbow of warm and cool colors
p <- ggplot(data, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue", "purple")) +
  theme_minimal() +
  labs(title = "Bar Plot with Rainbow Colors", x = "Category", y = "Value")

# Save the plot as a JPEG file
ggsave("rainbow_colors_plot.jpeg", plot = p, width = 8, height = 6, dpi = 300)