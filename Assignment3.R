library(forecast)
library(fpp)
library(dplyr)
library (readr)



####################
# vaccinationfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
Covid19casesfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
#Variantfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/variants/covid-variants.csv"

#vaccination.data<-read_csv(url(vaccinationfile))
Covid19cases.data<-read_csv(url(Covid19casesfile))
#Variant.data<-read_csv(url(Variantfile))

# install.packages("dplyr")
# Use the pipe %>% function in "dplyr" package

table(Covid19cases.data$iso_code)

CAN.vaccination.data = vaccination.data %>% filter(vaccination.data$iso_code == "CAN")
CAN.cases.data = Covid19cases.data %>% filter(Covid19cases.data$iso_code == "CAN")
# = Variant.data %>% filter(Variant.data$location == "Canada", Variant.data$variant == "Delta")

plot.ts(CAN.cases.data$new_cases)
plot.ts(CAN.cases.data$stringency_index)
#plot.ts(US.vaccination.data$total_vaccinations)
#plot.ts(US.vaccination.data$daily_vaccinations_raw)
#plot.ts(US.Variant.data$num_sequences)



# #############################################################################
# want to predict 2021-09-14 to 2021-09-28
CANNewcase<- CAN.cases.data[CAN.cases.data$date <= as.Date("2021-09-13"),]$new_cases
CANNewcase <- ts(CANNewcase, frequency=365, start=c(2020, 26))

#auto select without any preparation
model.auto <- auto.arima( CANNewcase, stepwise=FALSE, seasonal= FALSE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
model.auto
autoplot( forecast(model.auto,14))
checkresiduals(model.auto) 




########################################################################
# Preprocessing 
plot.ts(CAN.cases.data$new_cases)

CANNewcase<- CAN.cases.data[CAN.cases.data$date <= as.Date("2021-09-13"),]$new_cases
CANNewcase<-tail(CANNewcase,-150) # Remove the first 100 days when the case numbers are small


# after removing 100 days, start on 2020-05-05
CANNewcase <- ts(CANNewcase, frequency=365, start=c(2020, 175))
plot.ts(CANNewcase, xlab="Date", ylab="Canada cases")


# We notice two properties:
# (1)  the variance increases with mean (heteroscedasticity)
# (2)  the data has cyclic pattern(seasonality)  
# We cannot directly apply the ARIMA model when either of the above behaviors shows up! 

#-----------(1) Stabilizing the Variance---------------
# To stabilized the variance, we need to transform the original data using Box-Cox Transform, 
# also known as Power transform. It also makes the data more Gaussian   
CANNewcase.lambda <- BoxCox.lambda(CANNewcase)  # The transform is parameterized by lambda
# lambda is CAually chosen between -2 and 2
CANNewcase.BoxCox<-BoxCox(CANNewcase, CANNewcase.lambda)

# Check the transformed data and compare it with log transform
par(mfrow=c(1,2))
plot.ts(CANNewcase.BoxCox, xlab="Date", ylab="CA New cases")
plot.ts(log(CANNewcase), xlab="Date", ylab="CA New cases")
# if lambda is close to zero, BoxCox is essentially doing log transform.

# Transformedback<-InvBoxCox(CANNewcase.BoxCox, CANNewcase.lambda, biasadj = FALSE, fvar = NULL) # transform the forecasting back to original scale

logCANNewcase<-log(CANNewcase) # We use log transform for simplicity 
plot.ts(logCANNewcase, xlab="Date", ylab="log CA New cases")


#-----------(2) Remove Seasonality through Seasonal Differencing ---------------
# To check the period of cyclic pattern, use the autocorrelation function 
Acf(diff(logCANNewcase,1),lag.max =25) # We see spikes at p=7,14,21.. What does it suggest?  # Here we first perform regular differencing "diff(logCANNewcase,1)" to make the series more stationary, so its seasonality becomes easier to detect in the Acf plot

# We now remove the seasonality using seasonal differencing
logCANNewcase.deSeasonality <- diff(logCANNewcase,7) # period is 7 because of the weekly pattern 
plot.ts(logCANNewcase.deSeasonality, xlab="Date", ylab="log CA New Case after removing trend and seasonality")
Acf(logCANNewcase.deSeasonality,lag.max =25) 

# What does "logCANNewcase.deSeasonality" mean? 

#-------------Check Stationarity -------------------
# Perform the augmented Dickey-Fuller (ADF) test to check stationarity. The null hypothesis assumes that the series is non-stationary.
adf.test(CANNewcase,alternative = "stationary") # What? the test suggests stationarity? Now try the following
adf.test(logCANNewcase.deSeasonality,alternative = "stationary")

#-------------Automatic ARIMA Modeling -------------------
# To begin, we use an automated algorithm to find a good model. However, there is no guarantee that it is the best model. So we treat it as a starting point. 
# ARIMA(5,1,0) with zero mean  with AIC=374.35   AICc=374.53   BIC=399.49
model.auto <- auto.arima( logCANNewcase.deSeasonality, stepwise=FALSE, seasonal= FALSE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
model.auto

# It suggests a ARIMA(4,0,1) model with zero mean
checkresiduals(model.auto)  # Check the quality of fit. Residuals should: 
# (1) not have any significant autocorrelation
# (2) follow normal distribution
# (3) have stable variance over time



#-------------Improving the Automatically selected Model -------------------
# We can use the auto selected model to make forecasting 
fit.yourself <- Arima(logCANNewcase, order=c(5,0,4), seasonal=list(order=c(0,1,0),period=7)) # The seasonal differencing with period=7 is equivalent to "seasonal=list(order=c(0,1,0),period=7)"
fit.yourself
autoplot( forecast(fit.yourself,14))

# Plot the forecasting in the original scale
fc<-forecast(fit.yourself,14)

fc$x <- exp(fc$x)
fc$mean <- exp(fc$mean)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
autoplot(fc)
checkresiduals(fit.yourself)









#============================================
# ARIMA with Covariates ("dynamic regression")
#============================================

df <-CAN.cases.data[CAN.cases.data$date <= as.Date("2021-09-13"),c("new_cases","stringency_index","people_vaccinated")]
plot.ts(CAN.cases.data$new_cases)
plot.ts(CAN.cases.data$stringency_index)
plot.ts(CAN.cases.data$people_vaccinated)

# with insurance and advertising data (also part of FPP)

plot(df$date,df$new_cases, main="", xlab="Year")
plot(df$date,df$stringency_index, main="", xlab="Year")
View(df)
table(df$stringency_index)




# removing tail, start date is 2020-05-05
df<-tail(df,-150)
df<- data.matrix(df)

# start from row 175 all columns contain values
len <- length(df[,2])
stringindex <- cbind(df[,2],
                     c(NA,df[1:(len-1),2]),
                     c(NA,NA,df[1:(len-2),2]),
                     c(NA,NA,NA,df[1:(len-3),2]),
                     c(NA,NA,NA,NA,df[1:(len-4),2]),
                     c(NA,NA,NA,NA,NA,df[1:(len-5),2]),
                     c(rep(NA,7),df[1:(len-7),2]),
                     c(rep(NA,14),df[1:(len-14),2]),
                     df[,3],
                     c(NA,df[1:(len-1),3])
                       )                     
colnames(stringindex) <- paste("siLag",c(0:5,7,14,00,01),sep="")
stringindex




colnames(stringindex)


# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(logCANNewcase[175:len], xreg=stringindex[175:len,1], d=0)
fit2 <- auto.arima(logCANNewcase[175:len], xreg=stringindex[175:len,1:2], d=0)
fit3 <- auto.arima(logCANNewcase[175:len], xreg=stringindex[175:len,1:3], d=0)
fit4 <- auto.arima(logCANNewcase[175:len], xreg=stringindex[175:len,1:4], d=0)
fit5 <- auto.arima(logCANNewcase[175:len], xreg=stringindex[175:len,c(1,7)], d=0)
fit6 <- auto.arima(logCANNewcase[175:len], xreg=stringindex[175:len,c(1,7:8)], d=0)
fit7 <- auto.arima(logCANNewcase[175:len], xreg=stringindex[175:len,7], d=0)
fit8 <- auto.arima(logCANNewcase[175:len], xreg=stringindex[175:len,7:8], d=0)


fit4 <- auto.arima(df[15:len,1], xreg=stringindex[15:len,7:8], d=0)
fit4
# Compute Akaike Information Criteria
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
AIC(fit6)
AIC(fit7)
AIC(fit8)

# Compute Bayesian Information Criteria
BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)
BIC(fit6)
BIC(fit7)
BIC(fit8)

#Best fit (as per AIC and BIC) is with all data (1:2), so the final model becomes
fit <- auto.arima(logCANNewcase[175:len], xreg=stringindex[175:len,1:2], d=0) # d is the order of first-differencing
fit

# forecast insurance quotes with advertising = 10
fc10 <- forecast(fit, xreg=cbind(rep(10,20),c(Advert[40,1],rep(10,19))), h=20)
plot(fc10, main="Forecast quotes with advertising set to 10", ylab="Quotes")

# see how forecasts with advertising = 8 will differ from advertising = 2
par(mfrow=c(1,2))
fc8 <- forecast(fit, xreg=cbind(rep(8,20),c(Advert[40,1],rep(8,19))), h=20)
plot(fc8, main="Forecast quotes with advertising set to 8", ylab="Quotes")

fc2 <- forecast(fit, xreg=cbind(rep(2,20),c(Advert[40,1],rep(2,19))), h=20)
plot(fc2, main="Forecast quotes with advertising set to 2", ylab="Quotes")



