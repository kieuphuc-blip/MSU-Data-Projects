setwd("C:/Users/mishi/Downloads/")

# Load the dataset
data <- read.csv("full_subset.csv")  
# Load libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load the dataset
file_path <- "~/Desktop/ACC822/full_subset.csv"
data <- read_csv(file_path, show_col_types = FALSE)

# Calculate conversion rate by retailer
retailer_conversion <- data %>%
  group_by(retailer_property_name) %>%
  summarize(
    total_sessions = n_distinct(session),
    checkout_sessions = n_distinct(session[event_type == "Checkout"]),
    conversion_rate = round(checkout_sessions / total_sessions, 4) * 100
  ) %>%
  arrange(desc(conversion_rate))

# Print table
print(retailer_conversion)

# Plot conversion rate by retailer with colors
ggplot(retailer_conversion, aes(x = reorder(retailer_property_name, -conversion_rate), y = conversion_rate, fill = retailer_property_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Conversion Rate by Retailer", x = "Retailer", y = "Conversion Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()  # Use a color palette for the bars

# Calculate conversion rates by session duration
session_duration <- data %>%  # Changed from data_matched to data
  group_by(session) %>%
  summarize(
    start_time = min(start_time),
    end_time = max(end_time),
    duration_seconds = as.numeric(difftime(max(end_time), min(start_time), units = "secs")),
    reached_checkout = any(event_type == "Checkout")  
  )


time_conversion <- session_duration %>%
  mutate(duration_group = ifelse(duration_seconds > 60, "More than 60 seconds", "Less than 60 seconds")) %>%
  group_by(duration_group) %>%
  summarize(
    total_sessions = n(),
    checkout_sessions = sum(reached_checkout),
    conversion_rate = round(checkout_sessions / total_sessions, 4) * 100
  )

# Check the time_conversion data to verify conversion rates
print(time_conversion)

# Create a bar plot for conversion rates
ggplot(time_conversion, aes(x = duration_group, y = conversion_rate, fill = duration_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(conversion_rate, "%")), vjust = -0.5) +
  labs(title = "Conversion Rates by Session Duration",
       x = "Duration Group", 
       y = "Conversion Rate (%)") +
  theme_minimal()




# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the dataset 
data <- read.csv("full_subset.csv")

# Check unique event types to find the checkout event name
print("Unique event types in the dataset:")
print(unique(data$event_type))

# Convert start_time to date and extract month
shopping_peaks <- data %>%
  mutate(
    date = as.Date(start_time_local),  # Using start_time_local column
    month = month(date),
    month_name = month.name[month]
  ) %>%
  filter(month %in% c(11, 12))  # Filter for November and December

# Count sessions and checkouts by date
daily_activity <- shopping_peaks %>%
  group_by(date, month_name) %>%
  summarize(
    total_sessions = n_distinct(session),
    # Using multiple possible checkout event names
    checkout_count = sum(event_type %in% c("Checkout", "checkout", "Purchase", "purchase")),
    conversion_rate = ifelse(n_distinct(session) > 0,
                             round(checkout_count / n_distinct(session), 4) * 100,
                             0)
  )

# Compare overall November vs December metrics
monthly_comparison <- daily_activity %>%
  group_by(month_name) %>%
  summarize(
    total_sessions = sum(total_sessions),
    total_checkouts = sum(checkout_count),
    avg_conversion = round(mean(conversion_rate), 2)
  )

# Define gold and blue colors
month_colors <- c("November" = "#FFD700", "December" = "#0000FF")  # Gold and Blue

# Plot daily session counts with gold and blue colors
ggplot(daily_activity, aes(x = date, y = total_sessions, color = month_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = month_colors) +
  labs(title = "Daily Shopping Sessions: November vs December Peak Season",
       subtitle = "Comparing holiday shopping patterns",
       x = "Date", 
       y = "Number of Sessions",
       color = "Month") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

# Plot daily conversion rates with gold and blue colors
ggplot(daily_activity, aes(x = date, y = conversion_rate, color = month_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = month_colors) +
  labs(title = "Daily Conversion Rates: November vs December Peak Season",
       subtitle = "Comparing shopper purchase behavior during holidays",
       x = "Date", 
       y = "Conversion Rate (%)",
       color = "Month") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

# Plot monthly comparison of sessions with gold and blue fill
ggplot(monthly_comparison, aes(x = month_name, y = total_sessions, fill = month_name)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label = total_sessions), vjust = -0.5, size = 5) +
  scale_fill_manual(values = month_colors) +
  labs(title = "Total Shopping Sessions: November vs December",
       subtitle = "Comparing overall volume during peak season",
       x = "Month", 
       y = "Number of Sessions") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  )

# Plot monthly comparison of conversion rates with gold and blue fill
ggplot(monthly_comparison, aes(x = month_name, y = avg_conversion, fill = month_name)) +
  geom_bar(stat = "identity", width = 0.28) +
  geom_text(aes(label = paste0(avg_conversion, "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = month_colors) +
  labs(title = "Average Conversion Rate: November vs December",
       subtitle = "Comparing purchase behavior during peak holiday shopping season",
       x = "Month", 
       y = "Conversion Rate (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  )

# Add a comparison of total checkouts between months
ggplot(monthly_comparison, aes(x = month_name, y = total_checkouts, fill = month_name)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label = total_checkouts), vjust = -0.5, size = 5) +
  scale_fill_manual(values = month_colors) +
  labs(title = "Total Checkouts: November vs December",
       subtitle = "Comparing completed purchases during peak season",
       x = "Month", 
       y = "Number of Checkouts") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  )

# Calculate peak shopping days
peak_days <- daily_activity %>%
  arrange(desc(total_sessions)) %>%
  head(10)

# Print out top shopping days
cat("\nTop 10 Peak Shopping Days:\n")
print(peak_days[, c("date", "month_name", "total_sessions", "conversion_rate")])





# First, check what event types actually exist in your data
print("Unique event types in dataset:")
print(unique(data$event_type))

# Calculate the proportion of sessions with basket view that led to checkout
# Using your actual dataframe name 'data' instead of 'df'
basket_checkout <- data %>%
  group_by(session) %>%
  summarize(
    # Try multiple potential basket view event names
    basket_view = any(event_type %in% c("basket_view", "Basket View", "cart_view", "Cart View", "Add to Cart")),
    # Try multiple potential checkout event names 
    checkout = any(event_type %in% c("checkout", "Checkout", "purchase", "Purchase"))
  ) %>%
  group_by(basket_view) %>%
  summarize(
    total_sessions = n(),
    checkout_sessions = sum(checkout),
    conversion_rate = round(checkout_sessions / total_sessions, 4) * 100
  )

# Print to check the results
print("Basket checkout analysis results:")
print(basket_checkout)




