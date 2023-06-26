install.packages("tidyverse")
# Load the necessary libraries
library(tidyverse)
# Import the data
data <- read_csv("Amazon Sale Report.csv")

# View the first few rows of the data
head(data)
# Remove rows with missing values
data_clean <- data %>%
  drop_na()

# View the first few rows of the cleaned data
head(data_clean)
# Filter rows where 'Amount' is greater than 100
data_filtered <- data_clean %>%
  filter(Amount > 100)

# View the first few rows of the filtered data
head(data_filtered)


# View the first few rows of the data with the new column
head(data_mutated)

summary(data_mutated)


# 1.
# Analyzing a categorical variable 'ship-state'
state_counts <- data_mutated %>%
  group_by(`ship-state`) %>%
  summarise(count = n(), .groups = "drop")

# Plotting the categorical variable
ggplot(state_counts, aes(x = `ship-state`, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "State Counts", x = "State", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Analyzing a numerical variable 'Amount'
summary(data_mutated$Amount)

# Plotting the numerical variable
ggplot(data_mutated, aes(x = Amount)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Amount Distribution", x = "Amount", y = "Count")



# 2. 

install.packages("cowplot")

library(ggplot2)

total_amount_order_by_state <- data_mutated %>%
  group_by(`ship-state`) %>%
  summarize(total_amount = sum(Amount, na.rm = TRUE),
            num_orders = n()) %>%
  arrange(desc(total_amount))  # Sort by descending order

# Create bar chart
bar_chart <- ggplot(total_amount_order_by_state, aes(x = reorder(`ship-state`, -total_amount), y = total_amount)) +
  geom_bar(stat = "identity", fill = "#3366FF", width = 0.5) +
  labs(title = "Total Amount of Orders by State",
       x = "State",
       y = "Total Amount") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank())

# Create line chart for number of orders
line_chart <- ggplot(total_amount_order_by_state, aes(x = reorder(`ship-state`, -total_amount), y = num_orders)) +
  geom_line(color = "red", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(title = "Number of Orders by State",
       x = "State",
       y = "Number of Orders") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank())

# Combine the bar chart and line chart
combined_chart <- cowplot::plot_grid(bar_chart, line_chart, ncol = 1, align = "v")

# Print the combined chart
print(combined_chart)

# Load required packages
library(dplyr)
library(ggplot2)

# Calculate total amount and number of orders by state
total_amount_order_by_state <- data_mutated %>%
  group_by(`ship-state`) %>%
  summarize(total_amount = sum(Amount, na.rm = TRUE),
            total_orders = n()) %>%
  arrange(desc(total_amount)) %>%
  head(5)  # Select top 5 states

# Display the table
total_amount_order_by_state



install.packages("ggrepel")
library(ggrepel)
# Load required packages
library(dplyr)
library(ggplot2)
library(ggrepel)  # Added this line to load the ggrepel package

# Assuming you have a data frame named 'data_mutated' with columns 'ship-state', 'Amount', and 'OrderID'

data_mutated %>%
  group_by(`ship-state`) %>%
  summarize(total_amount = sum(Amount, na.rm = TRUE),
            total_orders = n()) %>%
  ggplot(aes(x = total_orders, y = total_amount, label = `ship-state`)) +
  geom_point() +
  geom_text_repel(aes(label = `ship-state`), size = 3) + 
  labs(title = "Total Number of Orders vs. Total Amount Spent by State",
       x = "Total Number of Orders",
       y = "Total Amount Spent") +
  theme_minimal()







# 3.
# Load required packages
library(dplyr)
library(ggplot2)

# Set the seed for reproducibility
set.seed(123)

# Choose the variable to analyze
variable <- data_mutated$Amount

# Define different sample sizes
sample_sizes <- c(30, 50, 100)

# Function to generate random samples and plot the distribution
generate_samples <- function(sample_size) {
  # Generate random sample
  sample <- sample(variable, size = sample_size, replace = TRUE)
  
  # Plot the distribution
  ggplot(data.frame(x = sample), aes(x = x)) +
    geom_histogram(binwidth = 100, fill = "blue", color = "white") +
    labs(title = paste("Sample Size:", sample_size), x = "Amount") +
    theme_minimal()
}

# Generate and plot random samples for different sample sizes
plots <- lapply(sample_sizes, generate_samples)

# Arrange the plots in a grid
gridExtra::grid.arrange(grobs = plots, ncol = 2)


# Simple Random Sampling
set.seed(123)
sample_random <- data_mutated %>% sample_n(1000)

# Stratified Sampling - based on 'amount'
set.seed(123)
sample_stratified <- data_mutated %>% 
  group_by(`Amount`) %>% 
  sample_frac(size = 0.1)

# Cluster Sampling - assuming 10 clusters chosen
set.seed(123)
chosen_clusters <- unique(data_mutated$`Amount`) %>% sample(10)
sample_cluster <- data_mutated %>% filter(`Amount` %in% chosen_clusters)

# Load required libraries
library(ggplot2)

# Simple Random Sampling
ggplot(sample_random, aes(x = Amount)) +
  geom_histogram(bins = 30, fill = 'steelblue') +
  ggtitle("Simple Random Sampling")

# Stratified Sampling
ggplot(sample_stratified, aes(x = Amount)) +
  geom_histogram(bins = 30, fill = 'steelblue') +
  ggtitle("Stratified Sampling")

# Cluster Sampling
ggplot(sample_cluster, aes(x = Amount)) +
  geom_histogram(bins = 30, fill = 'steelblue') +
  ggtitle("Cluster Sampling")


# Display top rows of each sampled data
head(sample_random)
head(sample_stratified)
head(sample_cluster)
# Summary of 'Amount' for each sampling method
summary(sample_random$Amount)
summary(sample_stratified$Amount)
summary(sample_cluster$Amount)


# Calculate density of 'Amount' in original dataset
amount_density <- density(data_mutated$Amount)
density_data <- data.frame(x = amount_density$x, y = amount_density$y)

# Simple Random Sampling with Density
ggplot(sample_random, aes(x = Amount)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = 'steelblue', alpha = 0.5) +
  geom_line(data = density_data, aes(x = x, y = y), color = "red", size = 1) +
  ggtitle("Simple Random Sampling")

# Stratified Sampling with Density
ggplot(sample_stratified, aes(x = Amount)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = 'steelblue', alpha = 0.5) +
  geom_line(data = density_data, aes(x = x, y = y), color = "red", size = 1) +
  ggtitle("Stratified Sampling")

# Cluster Sampling with Density
ggplot(sample_cluster, aes(x = Amount)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = 'steelblue', alpha = 0.5) +
  geom_line(data = density_data, aes(x = x, y = y), color = "red", size = 1) +
  ggtitle("Cluster Sampling")






# 5.

# Create a new variable for price per item
data_mutated <- data_mutated %>%
  mutate(Price_per_item = Amount / Qty)

# Visualize relationship between Qty and Price_per_item
ggplot(data_mutated, aes(x = Qty, y = Price_per_item)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Scatterplot of Price per Item by Quantity")

# Calculate correlation
cor.test(data_mutated$Qty, data_mutated$Price_per_item, method = "pearson")


