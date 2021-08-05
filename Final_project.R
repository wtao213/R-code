library("tidyverse")
library("tidyr")
library("ggplot2")
library("readxl")
library(lubridate)
library(opendatatoronto) 
library(dplyr) 


package <- show_package("996cfe8d-fb35-40ce-b569-698d51fc683b") 
list <- list_package_resources(package)

## meta data about the code
meta_code <- data.frame(get_resource(list[1,]))
## read me is in list 23
readme <- data.frame(get_resource(list[23,]))


## ttc data,remove row 1 and 23 in the list, that's not ttc actual data
list<-list[-c(1,23),]
listofdfs <- list() #Create a list in which you intend to save your df's.

for(i in 1:50){ 
  df<- data.frame(get_resource(list[i,]))
  listofdfs[[i]] <- df # save your dataframes into the list
}


TTC_delay_data <- bind_rows(listofdfs, .id = "column_label")


#############################
# 
sort(table(TTC_delay_data$Code),decreasing=TRUE)

head(sort(table(TTC_delay_data$Station),decreasing=TRUE),20)

head(sort(table(TTC_delay_data$Line),decreasing=TRUE),20)

#######
# remove unneeded  object
rm(list)
rm(listofdfs)
rm(package,df)


###############################
# read csv weather files
w1 <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 860 R\\Final_project\\en_climate_daily_ON_6158355_2018_P1D.csv")
w2 <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 860 R\\Final_project\\en_climate_daily_ON_6158355_2019_P1D.csv")
w3 <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 860 R\\Final_project\\en_climate_daily_ON_6158355_2020_P1D.csv")
w4 <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 860 R\\Final_project\\en_climate_daily_ON_6158355_2021_P1D.csv")

# stack dataframes togehter
weather <-rbind(w1,w2,w3,w4)

rm(w1,w2,w3,w4)



# weather sorce 2:
weather <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 860 R\\Final_project\\weatherstats_toronto_daily.csv")

weather$precipitation_ind <- ifelse(weather$precipitation >0, 1,0)
weather$rain_ind <- ifelse(weather$rain >0, 1,0)
weather$snow_ind <- ifelse(weather$snow >0, 1,0)


sort(table(weather$precipitation_ind),decreasing=TRUE)
sort(table(weather$rain_ind),decreasing=TRUE)
sort(table(weather$snow_ind),decreasing=TRUE)

w1 <-  weather[c("date","precipitation_ind", "rain_ind", "snow_ind","precipitation", "rain", "snow","avg_temperature")]
names(w1)[1] <- "Date"

## Date convert
TTC_delay_data$Date <- as.Date(TTC_delay_data$Date)

TTC_delay_data$year <- as.numeric(format(TTC_delay_data$Date, "%Y"))
TTC_delay_data$month<- as.numeric(format(TTC_delay_data$Date, "%m"))
TTC_delay_data$day  <- as.numeric(format(TTC_delay_data$Date, "%d"))

# create weekend indicator
names(TTC_delay_data)[4] <- "Day_name"

TTC_delay_data$weekend_ind <- ifelse(TTC_delay_data$Day_name %in% c("Sunday","Saturday"), 1,0)

table(TTC_delay_data$Day_name)
table(TTC_delay_data$weekend_ind)








# create column for off-peak  on-peak

TTC_delay_data_full$Time_cal <- strptime(TTC_delay_data_full$Time, format = "%H:%M")

time1 <- as.POSIXct("2021-07-27 06:30:00") #lower bound
time2 <- as.POSIXct("2021-07-27 10:00:00") #upper bound
rng <- new_interval(time1, time2)          #desired range

result <- df[df$time %within% rng,]


TTC_delay_data_full$peak_ind <- ifelse(TTC_delay_data_full$weekend_ind == 0 & ((TTC_delay_data_full$Time_cal >= "2021-07-27 06:30:00" & TTC_delay_data_full$Time_cal <= "2021-07-27 10:00:00") | (TTC_delay_data_full$Time_cal >= "2021-07-27 15:30:00" & TTC_delay_data_full$Time_cal <= "2021-07-27 19:00:00")),1,0)


#################################
# join data together
TTC_delay_data_full <- inner_join(TTC_delay_data, w1, by="Date")




######################
# 
#
names(meta_code)[2] <- "Code"
names(meta_code)[3] <- "Description"

code1 <- meta_code[c("Code","Description")]
code2 <-meta_code[c(6,7)]

names(code2)[1] <- "Code"
names(code2)[2] <- "Description"


code <- rbind(code1, code2)
code <- na.omit(code)

rm(code1,code2)


TTC_delay_data_full <- inner_join(TTC_delay_data_full, code, by="Code")





# export to csv
write.csv(TTC_delay_data_full,"C:\\Users\\wanti\\Desktop\\MMA\\MMA 860 R\\Final_project\\TTC_delay_data_full_Jul27.csv", row.names = FALSE)






