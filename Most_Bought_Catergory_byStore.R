# Load required libraries
library(dplyr)   
library(ggplot2) 
library(readr) 

# Read the data from the file
file_path <- "~/Desktop/ACC822/full_subset.csv" 
data <- read_csv(file_path, show_col_types = FALSE)

# Clean the data by removing unnecessary columns
data <- data %>%
  select(-c(...1, X))

# Filter the data to include only rows where matched == 1
data_matched <- data %>% filter(matched == 1)

# Keyword lists for categorizing products into categories
haircare_keywords <- c("shampoo", "conditioner", "hair mask", "hair serum", "hair oil", "scalp", "styling cream", "mousse", "gel")
skincare_keywords <- c("moisturizer", "serum", "cleanser", "toner", "face mask", "exfoliator", "cream", "lotion", "oil", "sunscreen")
makeup_keywords <- c("foundation", "lipstick", "lip gloss", "concealer", "eyeshadow", "blush", "highlighter", "mascara", "bronzer", "primer", "eye liner", "setting powder", "setting spray")

# Function to categorize products based on their name using defined keywords
categorize_product_by_keywords <- function(product_name) {
  product_name <- tolower(product_name) 
  
# Check if the product name contains any of the keywords
  if (any(grepl(paste(haircare_keywords, collapse="|"), product_name, ignore.case = TRUE))) {
    return("Haircare")
  }
  else if (any(grepl(paste(skincare_keywords, collapse="|"), product_name, ignore.case = TRUE))) {
    return("Skincare")
  }
  else if (any(grepl(paste(makeup_keywords, collapse="|"), product_name, ignore.case = TRUE))) {
    return("Makeup")
  }
  else {
    return("Other")
  }
}

# Apply the categorization function to each product name in the dataset
data_matched$category <- sapply(data_matched$product_name, categorize_product_by_keywords)

# Remove the rows where the category is "Other"
data_matched <- data_matched %>%
  filter(category != "Other")

# Summarize the number of purchases for each category and store
category_by_store <- data_matched %>%
  group_by(retailer_property_name, category) %>%
  summarise(purchase_count = n()) %>%
  arrange(retailer_property_name, desc(purchase_count))

# Get the top 3 categories for each retailer (store)
top_3_categories <- category_by_store %>%
  group_by(retailer_property_name) %>%
  slice_max(order_by = purchase_count, n = 3) %>%
  ungroup() 

# Define custom colors for the categories in the plot
colors <- c("Haircare" = "lightblue", "Skincare" = "lightgreen", "Makeup" = "lightpink")

# Create the plot using ggplot2
ggplot(top_3_categories, aes(x = category, y = purchase_count, fill = category)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~ retailer_property_name) +  
  labs(title = "Most Bought Categories for Each Store", x = "Category", y = "Number of Purchases") +  
  scale_fill_manual(values = colors) +  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 