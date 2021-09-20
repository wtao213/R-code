#######################################
#
#  https://www.kaggle.com/olistbr/brazilian-ecommerce/home
library(forecast)
library(fpp)
library(dplyr)
library (readr)
library(ggplot2)
library(tidyverse)


# read csv file
cus        <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 867 Predictive Modelling\\final\\data\\olist_customers_dataset.csv")
order_list <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 867 Predictive Modelling\\final\\data\\olist_order_items_dataset.csv")
order_pay  <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 867 Predictive Modelling\\final\\data\\olist_order_payments_dataset.csv")
orders     <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 867 Predictive Modelling\\final\\data\\olist_orders_dataset.csv")
product    <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 867 Predictive Modelling\\final\\data\\olist_products_dataset.csv")
sellers    <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 867 Predictive Modelling\\final\\data\\olist_sellers_dataset.csv")
trans      <- read_csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 867 Predictive Modelling\\final\\data\\product_category_name_translation.csv")



###########################################
# join data together

df <- inner_join(order_list, product, by="product_id")
df <- left_join(df, trans, by="product_category_name")
df <- left_join(df, orders, by="order_id")
####################
# join with seller id and check seller
sort(table(df$seller_id), decreasing= TRUE)

head(sort(table(df$seller_id), decreasing= TRUE),10)

# when df inner join with order pay from 112650 to 117601 records
# df <- inner_join(df, order_pay, by="order_id")
df <- left_join(df, order_pay, by="order_id")

length(unique(df$order_id))
length(unique(df[c("order_id","product_id" )]))
length(unique(order_pay$order_id))

unique(df[c("yad", "per")])



##############################################################
# look at the sales for the top 1-3 sellers

# 6560211a19b47992c3666cc44a7e94c0     2033
# 4a3ca9315b744ce9f8e9374361493884    1987
# 1f50f920176fa81dab994f9023523100    1931


#############################################
sell1 <- df[df$seller_id == "6560211a19b47992c3666cc44a7e94c0",] 

# out of the 2033 records, 1996 delivered
sell1[sell1$order_status == "delivered",]


##
# the most ordered project
sort(table(sell1$product_category_name_english), decreasing= TRUE)


## look at the ordered per day
sell1$approve_date <- as.Date(sell1$order_approved_at)


# aggregate by day and line
dff  <- sell1 %>% group_by(approve_date)%>% summarise(
  N = length(price),
  sum_price = sum(price)
)

## look at item  per day some days don't have item, please group by weeks
plot.ts(dff$N, xlab="Date", ylab="order number")

## cut them into weeks
dff <- dff %>% 
  mutate(week = cut.Date(approve_date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(approve_date)


## another aggregate

dff2  <- dff %>% group_by(week)%>% summarise(
  week_date = min(approve_date),
  N = sum(N),
  sum_price = sum(sum_price)
)

plot.ts(dff2$N[3:80], xlab="week", ylab="order number")

## less than two years, doesn't work,
# time series has no or less than 2 periods
a10Comp <- decompose(dff$N,type=c("additive"))
autoplot(a10Comp)
###########################################################
# Preprocessing 

order_number <- dff2$N[3:80]
order_number <- ts(order_number, frequency=52, start=c(2017, 9))

plot.ts(order_number, xlab="week", ylab="order number per week")


order_number.lambda <- BoxCox.lambda(order_number)  # The transform is parameterized by lambda
# lambda is usually chosen between -2 and 2
order_number.BoxCox<-BoxCox(order_number, order_number.lambda)
par(mfrow=c(1,2))
plot.ts(order_number.BoxCox, xlab="Date", ylab="Order Number")
plot.ts(log(order_number), xlab="Date", ylab="Order Number")


logorder_number<-log(order_number) # We use log transform for simplicity 
plot.ts(logorder_number, xlab="Date", ylab="log US New cases")

Acf(logorder_number,lag.max =25)
Acf(diff(logorder_number,1),lag.max =25) # We see spikes at p=7,14,21.. What does it suggest?  # Here we first perform regular differencing "diff(logorder_number,1)" to make the series more stationary, so its seasonality becomes easier to detect in the Acf plot

# We now remove the seasonality using seasonal differencing
logorder_number.deSeasonality <- diff(logorder_number,1) # period is 7 because of the weekly pattern 
plot.ts(logorder_number.deSeasonality, xlab="Date", ylab="log US New Case after removing trend and seasonality")
Acf(logorder_number.deSeasonality,lag.max =25) 


#-------------Check Stationarity -------------------
# Perform the augmented Dickey-Fuller (ADF) test to check stationarity. The null hypothesis assumes that the series is non-stationary.
adf.test(order_number,alternative = "stationary") # What? the test suggests stationarity? Now try the following
adf.test(logorder_number,alternative = "stationary")
adf.test(logorder_number.deSeasonality,alternative = "stationary")

#-------------Automatic ARIMA Modeling -------------------
# To begin, we use an automated algorithm to find a good model. However, there is no guarantee that it is the best model. So we treat it as a starting point. 
model.auto <- auto.arima( logorder_number.deSeasonality, stepwise=FALSE, seasonal= FALSE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
model.auto
# It suggests a ARIMA(4,0,1) model with zero mean
checkresiduals(model.auto) 

# We can use the auto selected model to make forecasting 
fit.yourself <- Arima(logorder_number, order=c(5,0,0), seasonal=list(order=c(0,1,0),period=1)) # The seasonal differencing with period=7 is equivalent to "seasonal=list(order=c(0,1,0),period=7)"
fit.yourself
autoplot( forecast(fit.yourself,10))

# Plot the forecasting in the original scale
fc<-forecast(fit.yourself,10)

fc$x <- exp(fc$x)
fc$mean <- exp(fc$mean)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
autoplot(fc)

















