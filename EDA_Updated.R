folder <- "C:/Users/Bob/Desktop/Capstone"
library(readxl)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(scales)

rawdata <- read.csv(paste(folder,"Sales_data_for_Capstone_Project_-_Student_Version_-_v3_CLEAN.csv",sep = "/"))
coerce_cols <- c("fiscalquartername","FiscalPeriod","Company","Top_Most_BP","BusinessPartner","Item_Number","Item_Description","Ship_to_State","Ship_to_City","Line_of_Business")

rawdata[coerce_cols] <- lapply(rawdata[coerce_cols],as.factor)

rawdata$Planned_Delivery_Date <- as.Date(rawdata$Planned_Delivery_Date)
cols_of_interest <- c("Company","Top_Most_BP","Item_Number","Ship_to_State","Quantity","Planned_Delivery_Date","Line_of_Business")
df <- rawdata[cols_of_interest]
levels(df$Line_of_Business) <- c("CITRUS","PFOTHR","TOMATO","TROPCL")

#Filter to only Tomato
df <- df[df$Line_of_Business=="TOMATO",]
#Quantity Distribution
ggplot(df,aes(x=Quantity))+
  geom_histogram()+
  scale_x_continuous(label=comma)

#Quantity vs. Planned Delivery Date
proc_data <- df %>%
  #mutate produces month & year columns
  mutate(month = format(Planned_Delivery_Date, "%m"), year = format(Planned_Delivery_Date, "%Y")) %>%
  #groupby gets me each of the data elements
  group_by(month, year, Item_Number) %>%
  #summarise gets me the sum of quantity
  summarise(tot = sum(Quantity)) 

#generate time series complete data
proc_data_complete <- proc_data%>%
  #add in a column for the first of the month to be processed as date data
  mutate(dt = paste(year,month,"01",sep="-")) %>%
  #add a date/time version of Date
  mutate(Date=as.Date(dt))%>%
  #complete the 
  complete(Item_Number,Date = seq.Date(min(Date), max(Date), by="month")) %>%
  #modify the total column to replace NA's
  mutate(tot = if_else(is.na(tot),0,tot))
proc_data_complete%>%
  group_by(Date)%>%
  summarise(tot = sum(tot))%>%
  ggplot(aes(x=Date, y=tot))+
  geom_line()+
  scale_y_continuous(labels = comma)+
  ylab("Aggregate Bag Quantity")+
  xlab("Planned Delivery Date")+
  ggtitle("Aggregate Bag Quantity Vs. Planned Delivery Date")
  
#Quantity vs. Month of year
proc_data_complete
proc_data_complete %>%
  group_by(Date)%>%
  ggplot(aes(x = year, y = tot))+ 
  geom_col() + 
  scale_y_continuous(labels=comma) +
  facet_wrap(~ month)+
  scale_x_discrete(labels =c("2010","","","","2014","","","",""))+
  ylab("Aggregate Bag Quantity")+
  xlab("Planned Delivery Date")

#Cumulative aggregate by Item Number
totals_by_Item_Number <- aggregate(Quantity~Item_Number,data=df,sum)
totals_by_Item_Number <- totals_by_Item_Number[order(totals_by_Item_Number$Quantity,decreasing = T),]

tot_IN_cum_sum <- cumsum(totals_by_Item_Number$Quantity)
cum_tot_pct <- tot_IN_cum_sum / sum(totals_by_Item_Number$Quantity)
cum_tot_pct <- as.data.frame(cum_tot_pct)*100
cum_tot_pct$ID <- seq(1,length(cum_tot_pct$cum_tot_pct), by=1)
ggplot(data=cum_tot_pct,aes(x=ID,y=cum_tot_pct))+
  geom_point()+
  labs(x="Item Number Index",y="Percent Total",title="Cumulative Aggregate Quantity By Item Number Index") +
  geom_vline(xintercept = 10)+
  geom_text(aes(x=20,y=0.86),label=paste("Top Ten:",prettyNum(cum_tot_pct[10,]$cum_tot_pct*100,big.mark = ",",format="d",digits=2),"%"))+
  scale_y_continuous(labels = percent)
head(df)
totals_by_BP <- aggregate(Quantity~Top_Most_BP,data=df,sum)
totals_by_BP <- totals_by_BP[order(totals_by_BP$Quantity,decreasing = T),]
tot_BP_cum_sum <- cumsum(totals_by_BP$Quantity)
cum_tot_pct_BP <- tot_BP_cum_sum / sum(totals_by_BP$Quantity)
cum_tot_pct_BP <- as.data.frame(cum_tot_pct_BP)
cum_tot_pct_BP$ID <- seq(1,length(cum_tot_pct_BP$cum_tot_pct_BP), by=1)
ggplot(data=cum_tot_pct_BP,aes(x=ID,y=cum_tot_pct_BP))+
  geom_point()+
  labs(x="Item Number Index",y="Percent Total",title="Cumulative Aggregate Quantity By Business Partner Index") +
  geom_vline(xintercept = 10)+
  geom_text(aes(x=13,y=0.93),label=paste("Top Ten:",prettyNum(cum_tot_pct_BP[10,]$cum_tot_pct*100,big.mark = ",",format="d",digits=2),"%"))+
  scale_y_continuous(labels = percent)