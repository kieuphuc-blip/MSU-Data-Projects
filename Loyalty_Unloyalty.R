# Load libraries
library(dplyr)
library(ggplot2)

# Load data
df <- read.csv("~/Desktop/CSE 801/full_subset.csv")

# --- GRAPH 1: Most Loyal Store per Customer (Bar Chart) ---
# Count purchases per customer per store
loyalty_df <- df %>%
  group_by(panelist_id, retailer_property_name) %>%
  summarise(purchase_count = n()) %>%
  ungroup()

# Get each customer’s top store
top_store <- loyalty_df %>%
  group_by(panelist_id) %>%
  filter(purchase_count == max(purchase_count)) %>%
  ungroup()

# Focus on 3 main stores
top3 <- c("Target", "Walmart", "Amazon")
top_store_filtered <- top_store %>%
  filter(retailer_property_name %in% top3)

# Count loyal customers by store
loyalty_count <- top_store_filtered %>%
  count(retailer_property_name)

# Plot Graph 1
ggplot(loyalty_count, aes(x = retailer_property_name, y = n, fill = retailer_property_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Loyal Store per Customer",
       x = "Store", y = "Number of Loyal Customers") +
  theme_minimal()

# --- GRAPH 2: Number of Stores Visited per Customer (Histogram) ---
store_diversity <- df %>%
  group_by(panelist_id) %>%
  summarise(num_stores = n_distinct(retailer_property_name))

# Plot Graph 2
ggplot(store_diversity, aes(x = num_stores)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  labs(title = "Store Variety per Customer",
       x = "Number of Unique Stores Visited",
       y = "Number of Customers") +
  theme_minimal()

# --- GRAPH 3: Purchase Frequency by Store (Boxplot) ---
store_freq <- df %>%
  group_by(panelist_id, retailer_property_name) %>%
  summarise(purchase_count = n()) %>%
  filter(retailer_property_name %in% top3)

# Plot Graph 3
ggplot(store_freq, aes(x = retailer_property_name, y = purchase_count, fill = retailer_property_name)) +
  geom_boxplot() +
  labs(title = "Purchase Frequency per Customer by Store",
       x = "Store", y = "Number of Purchases") +
  theme_minimal()
