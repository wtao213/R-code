library("tidyverse")
library("tidyr")
library("ggplot2")
library("readxl")
library(lubridate)
library(opendatatoronto) 
library(dplyr) 

# import data
df <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 860 R\\Final_project\\TTC_delay_data_full_Jul27.csv")


df$ym <- as.Date(with(df, paste(year, month,sep="-")), "%Y-%m")


head(sort(table(df$Line),decreasing = T),20)


df$Line <- ifelse(df$Line %in% c("YU / BD","YU/ BD","YU - BD","YU-BD","YU & BD","BD/YU","BD/YUS"), "YU/BD", df$Line)
df$Line <- ifelse(df$Line %in% c("B/D","BD LINE"), "BD", df$Line)
head(sort(table(df$Line),decreasing = T),20)


# only keep key line info
# from 139793 to  139201
df <- df[df$Line %in% c("YU","BD","SRT","SHP","YU/BD"),]



# aggregate by day and line
dff  <- df %>% group_by(Line, Date)%>% summarise(
  dl_times = length(column_label),
  precipitation_ind = max(precipitation_ind),
  rain_ind = max(rain_ind),
  snow_ind = max(snow_ind),
  Day_name = first(Day_name)
  
)
table(dff$Day_name)

# we might need to multiply the station number
## delay rate by day

dff$ttl_train <- ifelse(dff$Line =="SHP", ifelse(dff$Day_name == "Sunday",196.3636, ifelse(dff$Day_name == "Saturday",218.1818,218.1818)),
                                    ifelse(dff$Line == "SRT",ifelse(dff$Day_name == "Sunday",196.3636, ifelse(dff$Day_name == "Saturday",218.1818,235.1515)),
                                           ifelse(dff$Day_name == "Sunday",240, ifelse(dff$Day_name == "Saturday",266.67,341.3333))))
dff$delay_rate <- dff$dl_times / dff$ttl_train
  


## delay rate by day
t_day  <- dff %>% group_by(Day_name)%>% summarise(
  N  = length(dl_times),
  dl_times = sum(dl_times),
  ttl_train = sum(ttl_train),
  delay_rate_avg = sum(dl_times)/sum(ttl_train) ,
  delay_rate_min = min(delay_rate),
  delay_rate_max = max(delay_rate)
)
summary(t_day$delay_rate_avg)  


ggplot(t_day , aes(x=reorder( Day_name, delay_rate_avg), y= delay_rate_avg)) + 
  geom_bar(color="white",stat = "identity", fill="lightblue") +
  theme(text = element_text(size=12),axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_hline(yintercept= 0.04359, linetype="dashed", color = "red")



##################################
## delay rate by precipitation
t_precipitation <- dff %>% group_by(precipitation_ind)%>% summarise(
  N  = length(dl_times),
  dl_times = sum(dl_times),
  ttl_train = sum(ttl_train),
  delay_rate_avg = sum(dl_times)/sum(ttl_train) ,
  delay_rate_min = min(delay_rate),
  delay_rate_max = max(delay_rate)
)
summary(t_precipitation$delay_rate_avg) 


ggplot(t_precipitation , aes(x=precipitation_ind , y= delay_rate_avg)) + 
  geom_bar(color="white",stat = "identity", fill="lightblue") +
  theme(text = element_text(size=8),axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_hline(yintercept= 0.04359, linetype="dashed", color = "red")



  
  
  
  
##############################################
# seasonality

# aggregate by year and month
t1  <- df %>% group_by(year, month)%>% summarise(
  dl_times = length(column_label)
)

t1$ym <- make_date(t1$year,t1$month)


ggplot(t1[t1$year>=2016,], aes(x= ym, y= dl_times )) + 
  geom_bar(color="white",stat = "identity", fill="lightblue")+
  scale_x_date(date_labels = "%y-%m", date_breaks = "6 months",expand=c(0,0))


# before and after covid
ggplot(t1[t1$year>=2018,], aes(x= ym, y= dl_times )) + 
  geom_bar(color="white",stat = "identity", fill="lightblue")+
  scale_x_date(date_labels = "%y-%m", date_breaks = "6 months",expand=c(0,0))




#################################
# line table
sort(table(df$Line),decreasing=TRUE)



#################################
# delay reason table
head(sort(table(df$Description),decreasing=TRUE),20)

td  <- df[df$Min.Delay >0,] %>% group_by(Description)%>% summarise(
  dl_times = length(column_label),
  delaymin = mean(Min.Delay)
)
summary(td$dl_times)


ggplot(td[td$dl_times >3000,], aes(x= reorder(Description, dl_times), y= dl_times )) + 
  geom_bar(color="white",stat = "identity", fill="lightblue")+
  theme(text = element_text(size=8),axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_hline(yintercept=980, linetype="dashed", color = "red")


# customer related reason to ?
df$dr_group <- ifelse(df$Code %in% c("MUIR", "MUIS", "MUPAA", "SUDP", "SUO", "SUUT"), 1, 0)















#################################
# aggregate by day and station
dfs  <- df %>% group_by(Station, Date)%>% summarise(
  dl_times = length(column_label),
  precipitation_ind = max(precipitation_ind),
  rain_ind = max(rain_ind),
  snow_ind = max(snow_ind),
  Day_name = first(Day_name),
  Line = first(Line)
)
table(dfs $Day_name)

# we might need to multiply the station number
## delay rate by day

dfs$ttl_train <- ifelse(dfs$Line =="SHP", ifelse(dfs$Day_name == "Sunday",196.3636, ifelse(dfs$Day_name == "Saturday",218.1818,218.1818)),
                        ifelse(dfs$Line == "SRT",ifelse(dfs$Day_name == "Sunday",196.3636, ifelse(dfs$Day_name == "Saturday",218.1818,235.1515)),
                               ifelse(dfs$Day_name == "Sunday",240, ifelse(dfs$Day_name == "Saturday",266.67,341.3333))))
dfs$delay_rate <- dfs$dl_times / dfs$ttl_train



## station delay rate
t_s <- dfs %>% group_by(Station)%>% summarise(
  N  = length(dl_times),
  dl_times = sum(dl_times),
  ttl_train = sum(ttl_train),
  delay_rate_avg = sum(dl_times)/sum(ttl_train) ,
  delay_rate_min = min(delay_rate),
  delay_rate_max = max(delay_rate)
)
summary(t_s$delay_rate_avg) 
head(sort(t_s$delay_rate_avg ,decreasing = TRUE),20)


ggplot(t_s[t_s$delay_rate_avg > 0.006 & t_s$N > 100,] , aes(x= reorder(Station,delay_rate_avg) , y= delay_rate_avg)) + 
  geom_bar(color="white",stat = "identity", fill="lightblue") +
  theme(text = element_text(size=8),axis.text.x = element_text(angle = 60, hjust = 1))+ 
  geom_hline(yintercept= 0.003587, linetype="dashed", color = "red")



