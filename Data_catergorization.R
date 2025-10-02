

#0. load the packages and set working directory
library(dplyr)

setwd("~/Desktop/ACC822/")

# 0. process the demos data
demos=read.csv("demos.txt",header=TRUE,sep="\t")
demos=demos[!duplicated(demos),]
demos=demos[!is.na(demos$postal_code),]
demos=demos[,!(names(demos) %in% c("instance_id","industry","industry_id","occupation","occupation_id"))]


# 1. check the other data
shopper1=read.csv("export_shopper=JAN-25.txt",header=TRUE,sep="\t")
shopper2=read.csv("export_shopper=FEB-25.txt",header=TRUE,sep="\t")
shopper=rbind(shopper1,shopper2)
rm(shopper1,shopper2)

#remove useless columns from shopper table
shopper=shopper%>%inner_join(demos[,c("panelist_id","age")],by="panelist_id")
shopper=shopper[,!(names(shopper) %in% c("page_view_id","event_name","age"))]
#replace product_name and search_term as NA if the value is ""; we can use na.rm=TRUE to skip them 
shopper[shopper$product_name=="","product_name"]=NA
shopper[shopper$search_term=="","search_term"]=NA

shopper=shopper%>%arrange(panelist_id,retailer_property_name,start_time_local)

#remove the duplicated product_name and product_id, by capturing the pattern "[\\.*\\,"
shopper$product_name=sub("\\[\\\\\\\\.*\\\\\\\\,","",shopper$product_name)
shopper$product_id=sub("\\[\\\\\\\\.*\\\\\\\\,","",shopper$product_id)
shopper$search_term=sub("\\[\\\\\\\\.*\\\\\\\\,","",shopper$search_term)
shopper$purchase_price=sub("\\[\\\\\\\\.*\\\\\\\\,","",shopper$purchase_price)
#standardize search and Search
shopper[shopper$event_type=="search","event_type"]="Search"

#remove remaining punctuation 
shopper$product_id=gsub("[[:punct:]]","",shopper$product_id)
shopper$product_name=gsub("[[:punct:]]","",shopper$product_name)
shopper$search_term=gsub("[[:punct:]]","",shopper$search_term)
shopper$purchase_price=gsub("[[:punct:]]","",shopper$purchase_price)
shopper$purchase_price=as.numeric(shopper$purchase_price)/100

#to lower case
shopper$product_name=tolower(shopper$product_name)

# 2. categorize using beauty product names
pname=unique(shopper$product_name)


#start to take care of branded products first
brands=c("CeraVe","Clinique","Neutrogena","La Roche Posay","Cetaphil","Mario Badescu","Corsx","Dr Teal’s","Burt’s Bees","Biore","Aveeno")

brands_haircare_walmart= c("Garnier Whole Blends","TRESemme", "being MEGA SHINE","Camille Rose Naturals", "Shea Moisture", "Curls","L'Oreal Paris","Aussie","Pantene","OGX","Dove","Love Beauty and Planet","Herbal Essences","Head and Shoulders", "Kristin Ess","Carol's Daughter", "Suave","Maui Moisture","Not Your Mother's","Cantu")

brands_makeup_walmart= c( "Maybelline", "L’Oreal Paris", "Covergirl", "e.l.f Cosmetics", "Revlon", "NYX Professional Makeup", "wet n wild", "Milani", "Neutrogena", "Rimmel", "Physicians Formula", "Almay", "L.A. Colors", "Hardcandy", "Airspun", "ChapStick", "Profusion Cosmetics", "Real Techniques", "Eco Tools", "Burt’s Bees", "eos", "Shany")

brands_haircare_target = c( "Kristin Ess", "SheaMoisture","Camille Rose", "Mielle Organics","Ouai", "Eva NYC", "Tresemmé","Being Frenshe", "Odele","NEQI","Cécred","Saltair","Divi","Pattern","Not Your Mother's", "Dove","Pantene","Head & Shoulders","OGX","Garnier","L'Oreal Paris","Aussie","Suave","Maui Moisture","Love Beauty and Planet","Herbal Essences","Kristin Ess","Carol's Daughter","Cantu")

brands_makeup_target = c("Charlotte Tilbury", "Anastasia Beverly Hills", "Estee Lauder", "Too Faced", "Fenty Beauty", "NARS", "Pixi", "Lancome", "Laura Mercier", "Makeup Revolution", "Vitamesques", "Versed", "Clinique", "Benefit Cosmetics", "La Roche Posay", "Essence", "Elf", "MCo Beauty", "Maybelline", "ColourPop","NYX", "Covergirl", "WinkyLux", "Peripera", "bareMinerals", "Tarte", "Smashbox", "Kylie Cosmetics", "Nudestix")

brands_skincare_walmart = c("Neutrogena", "CeraVe", "Olay", "Equate", "Peach Slices", "ITK", "Cetaphil", "L'Oreal", "Hero Cosmetics", "Clean & Clear", "Aveeno", "Garnier", "Biore", "RoC", "Differin", "PanOxyl", "Pond's", "Clearasil", "St. Ives", "Noxzema", "OXY", "Stridex")

brands=c(brands,brands_haircare_walmart,brands_makeup_walmart,brands_haircare_target,brands_makeup_target,brands_skincare_walmart)
brands=unique(brands)

index=c()
for(i in 1:length(brands)){
  #match the product name containing the brand name
  #a=grep(tolower(brands[i]),pname,fixed=TRUE)
  #match the product name starting with the brand name
  a=grep(paste("^",tolower(brands[i]),"\\s.*",sep=""),pname,fixed=FALSE)
  index=union(index,a)
}


#obtain the selected list of products and the corresponding shopper behavioral data
selectedproduct=pname[index]
write.table(selectedproduct,"selectedproduct.txt",sep="\t",row.names=FALSE,col.names=TRUE)
#selectedproduct_df <- data.frame(ProductName = selectedproduct)
#write.csv(selectedproduct_df,"selectedproduct.csv")

# 3. identify the shopping sessions
#sort the shopper data 
shopper=shopper%>%arrange(panelist_id,retailer_property_name,start_time_local)
#as.POSIXct() convert the data storage to "number of seconds since the start of 1970"
shopper$start_time_local=as.POSIXct(strptime(shopper$start_time_local,"%Y-%m-%d %H:%M:%S"))
shopper$end_time_local=as.POSIXct(strptime(shopper$end_time_local,"%Y-%m-%d %H:%M:%S"))

#group by the shopper data by user and retailer; use that to cut data records into shopping sessions
#if the current record is a search, use 30 mins as the time gap to define a new session; if it is not search, use 60 mins
shopper=shopper%>%group_by(panelist_id,retailer_property_name)%>%
  mutate(diff_start=difftime(start_time_local,lag(start_time_local),units="mins"))%>%
  mutate(timelag60=ifelse(diff_start>60,1,0))%>%
  mutate(timelag60=ifelse(is.na(diff_start),1,timelag60))%>%
  mutate(timelag30=ifelse(diff_start>30,1,0))%>%
  mutate(timelag30=ifelse(is.na(diff_start),1,timelag30))%>%
  mutate(issearch=ifelse(event_type=="Search",1,0))%>%
  mutate(newsession=case_when(
    issearch==1 & timelag30==1 ~ 1,
    issearch==0 & timelag60==1 ~ 1,
    TRUE ~ 0
  ))%>%
  mutate(session=cumsum(newsession))
shopper=shopper[,!(names(shopper) %in% c("diff_start","timelag60","timelag30","newsession"))]

#combine "similar rows"
shopper=shopper%>%group_by(panelist_id,retailer_property_name,session,
                           event_type,search_term,product_id,product_name,purchase_price,purchase_quantity,currency)%>%
  mutate(row=row_number(),start_time=min(start_time_local),end_time=max(end_time_local))
shopper=shopper%>%filter(row==1)

#select the data with "Beauty" category; 
#select the shopping session as long as there is at least one line containing the selected products
shopper$matched=ifelse(shopper$product_name %in% selectedproduct,1,0)

selectedshopper=shopper%>%group_by(panelist_id,retailer_property_name,session)%>%
  mutate(maxmatched=max(matched),numrecords=n(),numproducts=n_distinct(product_name,na.rm=TRUE))%>%
  filter(maxmatched==1)%>%
  select(-maxmatched)

#write.table(selectedshopper,"selectedshopper.txt",sep="\t",row.names=FALSE,col.names=TRUE)
write.csv(selectedshopper, "jan_feb_2025_beauty.csv")

#combine mulitple month data
data1 =read.csv("jan_feb_2025_beauty.csv",header=TRUE)
data2 =read.csv("nov_dec_2024_beauty.csv",header=TRUE)
data3 =read.csv("sep-oct-2024_beauty.csv",header=TRUE)
data4 =read.csv("jul_aug_2024_beauty.csv",header=TRUE)
data5 =read.csv("may_jun_2024_beauty.csv",header=TRUE)
selectedshopper=rbind(data1,data2,data3,data4,data5)
rm(data1,data2,data3,data4,data5)

write.csv(selectedshopper, "full_subset.csv")
