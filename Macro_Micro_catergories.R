# 1. Load Required Packages
library(dplyr)

# 2. Set Working Directory (Keep Jungkyu's original path)
setwd("D:/MSU2/목2_822_Information Systems Project Management/Project/tab_data v2/")

# 3. Read Data File
beauty_data <- read.csv("full_subset.csv", header = TRUE)

# 4. Check Number of Rows
nrow(beauty_data)

# 5. Convert Product Names to Lowercase
beauty_data$product_name <- tolower(beauty_data$product_name)

# 6. Step 1: Classify by Keyword-Based Micro Categories
beauty_data <- beauty_data %>%
  mutate(micro_category = case_when(
    grepl("cleanser|face wash|facial wash|cleansing", product_name) ~ "Skin Care - Cleanser",
    grepl("moisturizer|hydrating|lotion|cream", product_name) ~ "Skin Care - Moisturizer",
    grepl("serum|essence|ampoule", product_name) ~ "Skin Care - Serum/Essence",
    grepl("sunscreen|sunblock|sun cream", product_name) ~ "Skin Care - Sun Care",
    grepl("mask|sheet mask|mud mask|clay mask|peel off", product_name) ~ "Skin Care - Mask",
    grepl("foundation|concealer|bb cream|cc cream", product_name) ~ "Makeup - Base",
    grepl("mascara|eyeliner|eyeshadow|brow|eyebrow|lash", product_name) ~ "Makeup - Eye",
    grepl("lipstick|lip balm|lip gloss|lip tint", product_name) ~ "Makeup - Lip",
    grepl("shampoo|conditioner", product_name) ~ "Hair Care - Shampoo/Conditioner",
    grepl("hair spray|hair gel|hair mousse|styling", product_name) ~ "Hair Care - Styling",
    grepl("body wash|body lotion|body cream|hand cream|hand wash", product_name) ~ "Body Care",
    TRUE ~ NA_character_
  ))

# 7. Step 2: Supplement Using Brand Names
beauty_data <- beauty_data %>%
  mutate(micro_category = case_when(
    is.na(micro_category) & grepl("cerave|cetaphil|neutrogena|la roche posay|aveeno|differin|olay", product_name) ~ "Skin Care",
    is.na(micro_category) & grepl("maybelline|l'oreal|covergirl|nyx|elf|revlon", product_name) ~ "Makeup",
    is.na(micro_category) & grepl("ogx|dove|pantene|head & shoulders|maui moisture|garnier", product_name) ~ "Hair Care",
    TRUE ~ micro_category
  ))

# 8. Step 3: Add Nail Care and Beauty Tools Categories
beauty_data <- beauty_data %>%
  mutate(micro_category = case_when(
    is.na(micro_category) & grepl("nail polish|nail color|top coat|glitter coat", product_name) ~ "Nail Care",
    is.na(micro_category) & grepl("tweezer|hand mirror", product_name) ~ "Beauty Tools",
    TRUE ~ micro_category
  ))

# 9. Step 4: Add Body and Makeup Remover Categories
beauty_data <- beauty_data %>%
  mutate(micro_category = case_when(
    is.na(micro_category) & grepl("deodorant|antiperspirant", product_name) ~ "Body Care - Deodorant",
    is.na(micro_category) & grepl("shave gel|razor", product_name) ~ "Body Care - Shaving",
    is.na(micro_category) & grepl("makeup remover", product_name) ~ "Skin Care - Cleanser",
    TRUE ~ micro_category
  ))

# 10. Step 4_1: Add Additional Micro Category Classifications
beauty_data <- beauty_data %>%
  mutate(micro_category = case_when(
    is.na(micro_category) & grepl("primer|corrector|highlighter|illuminator", product_name) ~ "Makeup - Base",
    is.na(micro_category) & grepl("lip liner|lip oil", product_name) ~ "Makeup - Lip",
    is.na(micro_category) & grepl("brow pencil|eye pencil|mascara|eyeliner|eyebrow", product_name) ~ "Makeup - Eye",
    is.na(micro_category) & grepl("lip & cheek|lip and cheek", product_name) ~ "Makeup",
    is.na(micro_category) & grepl("kabuki brush|makeup sponge|applicator|brush|mirror", product_name) ~ "Beauty Tools",
    is.na(micro_category) & grepl("moisturizing stick|hydrating stick", product_name) ~ "Skin Care - Moisturizer",
    is.na(micro_category) & grepl("glow liquid|radiance serum", product_name) ~ "Skin Care - Serum/Essence",
    TRUE ~ micro_category
  ))


# Extract only beauty products (matched == 1)
beauty_only <- beauty_data %>% filter(matched == 1)

# Check results
table(beauty_only$micro_category)

# View a part of the data
head(beauty_only %>% select(product_name, micro_category), 20)

# View in data viewer
View(beauty_only)

# Load additional packages
library(ggplot2)

# Step (2). Create Main Categories
beauty_only <- beauty_only %>%
  mutate(main_category = case_when(
    grepl("Skin Care", micro_category) ~ "Skin Care",
    grepl("Hair Care", micro_category) ~ "Hair Care",
    grepl("Body Care", micro_category) ~ "Body Care",
    grepl("Makeup", micro_category) ~ "Makeup",
    grepl("Nail Care", micro_category) ~ "Nail Care",
    grepl("Beauty Tools", micro_category) ~ "Beauty Tools",
    TRUE ~ "Others"
  ))

# Step (3). Analyze purchase count by Main Category
main_purchase_count <- beauty_only %>%
  filter(!is.na(micro_category)) %>%
  group_by(main_category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Step (4). Plot purchase count by Main Category
ggplot(main_purchase_count, aes(x = reorder(main_category, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Purchase Count by Main Category",
       x = "Main Category", y = "Purchase Count")

# Step (5). Analyze purchase count by Micro Category
micro_purchase_count <- beauty_only %>%
  filter(!is.na(micro_category)) %>%
  group_by(micro_category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Step (6). Plot purchase count by Micro Category
ggplot(micro_purchase_count, aes(x = reorder(micro_category, count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Purchase Count by Micro Category",
       x = "Micro Category", y = "Purchase Count")

# Convert purchase_price and purchase_quantity to numeric
beauty_only <- beauty_only %>%
  mutate(
    purchase_price = as.numeric(purchase_price),
    purchase_quantity = as.numeric(purchase_quantity)
  )

# Calculate total revenue
beauty_only <- beauty_only %>%
  mutate(total_revenue = purchase_price * purchase_quantity)

# Summarize total revenue by Main Category
main_revenue <- beauty_only %>%
  filter(!is.na(micro_category)) %>%
  group_by(main_category) %>%
  summarise(total_revenue = sum(total_revenue, na.rm = TRUE)) %>%
  arrange(desc(total_revenue))

# Plot total revenue by Main Category
ggplot(main_revenue, aes(x = reorder(main_category, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total Revenue by Main Category",
       x = "Main Category", y = "Total Revenue ($)")

# Summarize total revenue by Micro Category
micro_revenue <- beauty_only %>%
  filter(!is.na(micro_category)) %>%
  group_by(micro_category) %>%
  summarise(total_revenue = sum(total_revenue, na.rm = TRUE)) %>%
  arrange(desc(total_revenue))

# Plot total revenue by Micro Category
ggplot(micro_revenue, aes(x = reorder(micro_category, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total Revenue by Micro Category",
       x = "Micro Category", y = "Total Revenue ($)")

# Extract only Skin Care data
skincare_data <- beauty_only %>%
  filter(main_category == "Skin Care")

# Summarize purchase counts by Skin Care Micro Category
skincare_count <- skincare_data %>%
  group_by(micro_category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Calculate percent for skincare
skincare_count <- skincare_count %>%
  mutate(percent = count / sum(count) * 100,
         label = paste0(round(percent, 1), "%"))

# Plot pie chart with percentage labels
ggplot(skincare_count, aes(x = "", y = count, fill = micro_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +
  labs(title = "Purchase Share by Skin Care Micro Category") +
  theme(legend.title = element_blank())

# Summarize total revenue by Skin Care Micro Category
skincare_revenue <- skincare_data %>%
  group_by(micro_category) %>%
  summarise(total_revenue = sum(total_revenue, na.rm = TRUE)) %>%
  arrange(desc(total_revenue))

# Plot bar chart for total revenue by Skin Care Micro Category
ggplot(skincare_revenue, aes(x = reorder(micro_category, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total Revenue by Skin Care Micro Category",
       x = "Micro Category", y = "Total Revenue ($)")

# Analyze purchase behavior by price range
beauty_only <- beauty_only %>%
  mutate(price_range = case_when(
    purchase_price < 5 ~ "Under $5",
    purchase_price >= 5 & purchase_price < 15 ~ "$5 - $15",
    purchase_price >= 15 & purchase_price < 30 ~ "$15 - $30",
    purchase_price >= 30 ~ "Above $30",
    TRUE ~ "Unknown"
  ))

# Summarize purchase counts by price range and main category
price_range_count <- beauty_only %>%
  filter(!is.na(price_range)) %>%
  group_by(price_range, main_category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot price range analysis
ggplot(price_range_count, aes(x = reorder(main_category, count), y = count, fill = price_range)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Purchase Count by Price Range and Main Category",
       x = "Main Category", y = "Purchase Count") +
  scale_fill_brewer(palette = "Set2")

# Basket Analysis
library(arules)

# Prepare transaction data
basket_data <- beauty_only %>%
  filter(!is.na(product_name)) %>%
  group_by(panelist_id, retailer_property_name, session) %>%
  summarise(products = list(unique(product_name))) %>%
  ungroup()

# Convert to transactions
transactions <- as(basket_data$products, "transactions")

# Apply Apriori algorithm
rules <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.1))

# Sort rules by confidence
rules_sorted <- sort(rules, by = "confidence", decreasing = TRUE)

# View top 10 rules
inspect(head(rules_sorted, 10))

# Visualize top 20 rules as network
library(arulesViz)
plot(rules_sorted[1:20], method = "graph", control = list(type = "items"))


# Visualize Top 10 Association Rules by Lift
# Sort by lift instead of confidence
top10_lift_rules <- sort(rules, by = "lift", decreasing = TRUE)[1:10]

# Plot network of top 10 rules by lift
plot(top10_lift_rules, method = "graph", engine = "igraph", 
     control = list(type = "items"),
     main = "Top 10 Association Rules by Lift")



# Basket analysis for Skin Care only
skincare_basket <- beauty_only %>%
  filter(main_category == "Skin Care", !is.na(product_name)) %>%
  group_by(panelist_id, retailer_property_name, session) %>%
  summarise(products = list(unique(product_name))) %>%
  ungroup()

# Transactions for Skin Care
skincare_transactions <- as(skincare_basket$products, "transactions")

# Apriori for Skin Care
skincare_rules <- apriori(skincare_transactions, parameter = list(supp = 0.001, conf = 0.1))

# Sort rules
skincare_rules_sorted <- sort(skincare_rules, by = "confidence", decreasing = TRUE)

# View top 10 Skin Care rules
inspect(head(skincare_rules_sorted, 10))

# Visualize Skin Care rules
plot(skincare_rules_sorted[1:20], method = "graph", engine = "igraph")

# Extract stronger Skin Care rules (support ≥ 0.002, confidence ≥ 0.5)
skincare_rules_strong <- apriori(skincare_transactions,
                                 parameter = list(supp = 0.002, conf = 0.5))

# Sort strong rules
skincare_rules_strong_sorted <- sort(skincare_rules_strong, by = "confidence", decreasing = TRUE)

# View top 10 strong Skin Care rules
inspect(head(skincare_rules_strong_sorted, 10))

# Load required packages
library(arules)
library(arulesViz)

# Visualizing the top 10 strongest Skin Care association rules
plot(skincare_rules_strong_sorted[1:10],
     method = "graph",
     engine = "igraph",
     control = list(main = "Top 10 Strong Skin Care Rules"))
