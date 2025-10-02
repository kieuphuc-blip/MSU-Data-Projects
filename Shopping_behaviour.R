setwd("~/Desktop/ACC822/")

#combine mulitple month data
data1 =read.csv("jan_feb_2025_beauty.csv",header=TRUE)
data2 =read.csv("nov_dec_2024_beauty.csv",header=TRUE)
data3 =read.csv("sep-oct-2024_beauty.csv",header=TRUE)
data4 =read.csv("jul_aug_2024_beauty.csv",header=TRUE)
data5 =read.csv("may_jun_2024_beauty.csv",header=TRUE)
selectedshopper=rbind(data1,data2,data3,data4,data5)
rm(data1,data2,data3,data4,data5)

write.csv(selectedshopper, "full_subset.csv")

# Process the demos data
demos=read.csv("demos.txt",header=TRUE,sep="\t")
demos=demos[!duplicated(demos),]
demos=demos[!is.na(demos$postal_code),]
demos=demos[,!(names(demos) %in% c("instance_id","industry","industry_id","occupation","occupation_id"))]

library(dplyr)
#Brands dictionary
#start to take care of branded products first
brands=c("CeraVe","Clinique","Neutrogena","La Roche Posay","Cetaphil","Mario Badescu","Corsx","Dr Teal’s","Burt’s Bees","Biore","Aveeno")

brands_haircare_walmart= c("Garnier Whole Blends","TRESemme", "being MEGA SHINE","Camille Rose Naturals", "Shea Moisture", "Curls","L'Oreal Paris","Aussie","Pantene","OGX","Dove","Love Beauty and Planet","Herbal Essences","Head and Shoulders", "Kristin Ess","Carol's Daughter", "Suave","Maui Moisture","Not Your Mother's","Cantu")

brands_makeup_walmart= c( "Maybelline", "L’Oreal Paris", "Covergirl", "e.l.f Cosmetics", "Revlon", "NYX Professional Makeup", "wet n wild", "Milani", "Neutrogena", "Rimmel", "Physicians Formula", "Almay", "L.A. Colors", "Hardcandy", "Airspun", "ChapStick", "Profusion Cosmetics", "Real Techniques", "Eco Tools", "Burt’s Bees", "eos", "Shany")

brands_haircare_target = c( "Kristin Ess", "SheaMoisture","Camille Rose", "Mielle Organics","Ouai", "Eva NYC", "Tresemmé","Being Frenshe", "Odele","NEQI","Cécred","Saltair","Divi","Pattern","Not Your Mother's", "Dove","Pantene","Head & Shoulders","OGX","Garnier","L'Oreal Paris","Aussie","Suave","Maui Moisture","Love Beauty and Planet","Herbal Essences","Kristin Ess","Carol's Daughter","Cantu")

brands_makeup_target = c("Charlotte Tilbury", "Anastasia Beverly Hills", "Estee Lauder", "Too Faced", "Fenty Beauty", "NARS", "Pixi", "Lancome", "Laura Mercier", "Makeup Revolution", "Vitamesques", "Versed", "Clinique", "Benefit Cosmetics", "La Roche Posay", "Essence", "Elf", "MCo Beauty", "Maybelline", "ColourPop","NYX", "Covergirl", "WinkyLux", "Peripera", "bareMinerals", "Tarte", "Smashbox", "Kylie Cosmetics", "Nudestix")

brands_skincare_walmart = c("Neutrogena", "CeraVe", "Olay", "Equate", "Peach Slices", "ITK", "Cetaphil", "L'Oreal", "Hero Cosmetics", "Clean & Clear", "Aveeno", "Garnier", "Biore", "RoC", "Differin", "PanOxyl", "Pond's", "Clearasil", "St. Ives", "Noxzema", "OXY", "Stridex")

brands=c(brands,brands_haircare_walmart,brands_makeup_walmart,brands_haircare_target,brands_makeup_target,brands_skincare_walmart)
brands=unique(brands)

#for Amazon
#get the event level data frame
amazon=selectedshopper%>%group_by(panelist_id,retailer_property_name,session)%>%
  filter(retailer_property_name=="Amazon")%>%
  mutate(row=row_number(),numrecords=n(),eventlength=as.numeric(difftime(end_time,start_time,units="mins")),
         sessionlength=as.numeric(difftime(max(end_time),min(start_time),units="mins")),
         eventstart=as.numeric(difftime(start_time,min(start_time),units="mins")))%>%
  mutate(row_relative=row/numrecords,eventlength_relative=eventlength/sessionlength,eventstart_relative=eventstart/sessionlength)%>%
  as.data.frame()
View(amazon)

#for Walmart
#get the event level data frame
walmart=selectedshopper%>%group_by(panelist_id,retailer_property_name,session)%>%
  filter(retailer_property_name=="Walmart")%>%
  mutate(row=row_number(),numrecords=n(),eventlength=as.numeric(difftime(end_time,start_time,units="mins")),
         sessionlength=as.numeric(difftime(max(end_time),min(start_time),units="mins")),
         eventstart=as.numeric(difftime(start_time,min(start_time),units="mins")))%>%
  mutate(row_relative=row/numrecords,eventlength_relative=eventlength/sessionlength,eventstart_relative=eventstart/sessionlength)%>%
  as.data.frame()

#for Target
#get the event level data frame
target=selectedshopper%>%group_by(panelist_id,retailer_property_name,session)%>%
  filter(retailer_property_name=="Target")%>%
  mutate(row=row_number(),numrecords=n(),eventlength=as.numeric(difftime(end_time,start_time,units="mins")),
         sessionlength=as.numeric(difftime(max(end_time),min(start_time),units="mins")),
         eventstart=as.numeric(difftime(start_time,min(start_time),units="mins")))%>%
  mutate(row_relative=row/numrecords,eventlength_relative=eventlength/sessionlength,eventstart_relative=eventstart/sessionlength)%>%
  as.data.frame()

# Combine with demo data
fullsubset_demos=selectedshopper%>%inner_join(demos,by="panelist_id")

#search term
#add search terms from amazon containing brand names to index
amazon$search_term=tolower(amazon$search_term)
searchterm_unique=unique(amazon$search_term)
index=c()
for(i in 1:length(brands)){
  #match the search term containing the brand name
  a=grep(paste("^",tolower(brands[i]),"\\s.*",sep=""),searchterm_unique,fixed=FALSE)
  index=union(index,a)
}

#add search terms from walmart containing brand names to index
walmart$search_term=tolower(walmart$search_term)
searchterm_unique=unique(walmart$search_term)
for(i in 1:length(brands)){
  #match the search term containing the brand name
  a=grep(paste("^",tolower(brands[i]),"\\s.*",sep=""),searchterm_unique,fixed=FALSE)
  index=union(index,a)
}
#add search terms from target containing brand names to index
target$search_term=tolower(target$search_term)
searchterm_unique=unique(target$search_term)
for(i in 1:length(brands)){
  #match the search term containing the brand name
  a=grep(paste("^",tolower(brands[i]),"\\s.*",sep=""),searchterm_unique,fixed=FALSE)
  index=union(index,a)
}

search_terms_brands = searchterm_unique[index]
#contains search terms with brand names from all 3 stores
search_terms_brands_df <- data.frame(Search_Term_Brands = search_terms_brands)
View(search_terms_brands_df)
#search terms containing beauty brand names
search_terms_brands <- fullsubset_demos %>%filter(search_term %in% searchterm_unique[index])
dim(search_terms_brands)
#search terms not containing brand names
search_terms_nonbrands <- fullsubset_demos %>%
  filter(!search_term %in% search_terms_brands$search_term & !is.na(search_term))
dim(search_terms_nonbrands)
#------------------------------------------------------------------------------------
# pie chart for search terms contianing beauty products vs non-beauty products 
library(ggplot2)
library(dplyr)

# Prepare counts
search_term_counts <- data.frame(
  Type = c("Beauty Brands", "Non BeautyBrands"),
  Count = c(nrow(search_terms_brands), nrow(search_terms_nonbrands))
)

# Add label text with counts and percentages
search_term_counts <- search_term_counts %>%
  mutate(
    label = paste0(Count, " (", round(Count / sum(Count) * 100), "%)")
  )

# Plot standard pie chart with labels
ggplot(search_term_counts, aes(x = "", y = Count, fill = Type)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            size = 5) +
  labs(title = "Proportion of Brand vs Non-Brand Search Terms") +
  theme_void() +
  theme(legend.title = element_blank())
#--------------------------------------------------------------------------------

#Bar chart for search terms containing beauty brands that is segmented by different genders
gender_searchterms <- fullsubset_demos %>% filter(search_term %in% searchterm_unique[index]) %>% count(gender)
gender_searchterms
library(ggplot2)

# Assuming gender_searchterms is already defined:
# gender_searchterms <- fullsubset_demos %>%
#   filter(search_term %in% searchterm_unique[index]) %>%
#   count(gender)

# Bar chart
ggplot(gender_searchterms, aes(x = gender, y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Brand Search Terms by Gender",
       x = "Gender",
       y = "Count of Search Terms") +
  theme_minimal() +
  theme(legend.position = "none")

#--------------------------------------------------------------------------------
#proportion of products in the beauty catergory - creating data frame for each retailer
amazon_product=amazon%>%group_by(panelist_id,retailer_property_name,session,product_name,purchase_price,matched)%>%
  filter(!is.na(product_name))%>%
  summarize()%>%
  as.data.frame()

walmart_product=walmart%>%group_by(panelist_id,retailer_property_name,session,product_name,purchase_price,matched)%>%
  filter(!is.na(product_name))%>%
  summarize()%>%
  as.data.frame()

target_product=target%>%group_by(panelist_id,retailer_property_name,session,product_name,purchase_price,matched)%>%
  filter(!is.na(product_name))%>%
  summarize()%>%
  as.data.frame()

#--------------------------------------------------------------------------------
#creating pie chart for matched by different retailer
amazon_product$retailer <- "Amazon"
walmart_product$retailer <- "Walmart"
target_product$retailer <- "Target"

combined_products <- bind_rows(amazon_product, walmart_product, target_product)
#filter and summarize by matched
matched_by_retailer <- combined_products %>%
  filter(matched == TRUE) %>%
  count(retailer) %>%
  mutate(
    pct = round(n / sum(n) * 100),
    label = paste0(retailer, ": ", pct, "%")
  )
#pie chart with labels
library(ggplot2)

ggplot(matched_by_retailer, aes(x = "", y = n, fill = retailer)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Matched Products by Retailer") +
  theme_void()
#--------------------------------------------------------------------------------
#Looking into average prices of products by different retailers
# Filter to matched == 1 and valid prices
matched_only <- combined_products %>%
  filter(matched == 1, !is.na(purchase_price), purchase_price > 0) %>%
  mutate(log_price = log(purchase_price))

#Plot histograms
ggplot(matched_only, aes(x = log_price)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_wrap(~ retailer, scales = "free_y") +
  labs(title = "Log(Purchase Price) Histogram for Matched Products",
       x = "Log(Purchase Price)",
       y = "Count") +
  theme_minimal()

# Function to get mode from density
get_mode_log_price <- function(x) {
  d <- density(x)
  mode_val <- d$x[which.max(d$y)]
  return(mode_val)
}

# Compute mode for each retailer
mode_by_retailer <- matched_only %>%
  group_by(retailer) %>%
  summarise(log_price = get_mode_log_price(log_price),
            dollars = exp(log_price))
mode_by_retailer
#--------------------------------------------------------------------------------
#Looking into average prices of products by gender
# Combine with demo data
combined_demo=combined_products%>%inner_join(demos,by="panelist_id")

# Filter to matched rows with valid purchase prices and gender
matched_with_gender <- combined_demo %>%
  filter(matched == 1, !is.na(purchase_price), purchase_price > 0, !is.na(gender))

# Compute average purchase price per user by gender
avg_price_by_user <- matched_with_gender %>%
  filter(gender != "Other/I prefer to self classify") %>%
  group_by(panelist_id, gender) %>%
  summarise(avg_price = mean(purchase_price), .groups = "drop")

#Plot histogram of average purchase prices by gender
ggplot(avg_price_by_user, aes(x = avg_price, fill = gender)) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 20) +
  labs(title = "Distribution of Average Purchase Price by Gender",
       x = "Average Purchase Price",
       y = "Count",
       fill = "Gender") +
  theme_minimal()

#find the mode density peak
get_mode_from_density <- function(x) {
  d <- density(x)
  mode_val <- d$x[which.max(d$y)]
  return(mode_val)
}
#Apply function by gender
avg_price_by_gender <- avg_price_by_user %>%
  group_by(gender) %>%
  summarise(avg_price = get_mode_from_density(avg_price))

print(avg_price_by_gender)