library("tidyverse")
library("tidyr")
library("ggplot2")
library("readxl")
library(lubridate)
library(opendatatoronto) 
library(dplyr)
library(pROC)


# Source of data and code: Dimitris Bertsimas @ MIT
mr <-read.csv("C:\\Users\\wanti\\Desktop\\MMA\\MMA 867 Predictive Modelling\\Assignment2\\MusicData.csv") #load data
# How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(mr$artistname == "Michael Jackson")

# Alternatively, use the pipe %>% function in "dplyr" package
df %>% filter(artistname == "Michael Jackson") %>% summarize(count = n())


# first use the filter function to split the data into a training set "SongsTrain" 
# consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", 
# consisting of the 2010 song releases.
SongsTrain = mr %>% filter(year <= 2009)
SongsTest = mr %>% filter(year == 2010)

######
#target base rate
table(mr$Top10)

table(SongsTrain$Top10)
table(SongsTest$Top10)

# we want to exclude some of the variables in our dataset from being used as independent variables 
# ("year", "songtitle", "artistname", "songID", and "artistID"). To do this, we can use the following trick. 
# First define a vector of variable names called nonvars - these are the variables that we won't use in our model.
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# To remove these variables from your training and testing sets:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

# build a logistic regression model to predict Top10 using the training data. 
# We can now use "." in place of enumerating all the remaining independent variables in the following way:
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

summary(SongsLog1)


#####################################
# You can make predictions on the test set by using the command:
testPredict = predict(SongsLog1, newdata=SongsTest, type="response")

# Then, you can create a confusion matrix with a threshold of 0.15 by using the table command:
confusion.matrix<-table(SongsTest$Top10, testPredict >= 0.15)

# The accuracy of the model is? 
Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]
Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]

Accuracy.rate<-Count.correct/(Count.correct+Count.wrong)
# What is the prediction accuracy of the model?

# To generate the ROC curve
test_prob = predict(SongsLog1, newdata = SongsTest, type = "response")
par("mar")
par(mar=c(1,1,1,1))
test_roc = roc(SongsTest$Top10 ~ test_prob, plot = TRUE, print.auc = TRUE)




##############################################
# modification start here
summary(mr)


# logit plot
mr2 = mr[ , !(names(mr) %in% nonvars) ]

par(mar=c(1,1,1,1))
par(mfrow = c(3, 3))
for (i in 1:9) { 
  tt <-mr2[,c(i,34)]
  bin <- unique(quantile(tt[,1], probs = c(seq(0, 1, by = 0.05))))
  tt$gx <-cut(tt[,1], breaks = bin,right = FALSE)
  table(tt$gx)
  name =colnames(tt)[1]
  
  tt2  <- tt %>% group_by(tt$gx)%>% summarise(
    N  = length(Top10),
    y_sum = sum(Top10),
    x = mean(!! rlang::sym(name)),
    y_log = log((sum(Top10)+1)/(length(Top10)-sum(Top10)+1))
  )
  
  plot(tt2$x,tt2$y_log,type="l", col="blue", lwd=5, xlab=name, ylab="logit y", main=paste( name," vs Target", sep=""))
  
  rm(tt,bin,tt2)
}


#############################################
#
mr2 = mr
summary(mr$timbre_4_max)
summary(mr$timbre_8_max)


mr2$timbre_4_delta <- mr2$timbre_4_max - mr2$timbre_4_min
mr2$timbre_8_delta <- mr2$timbre_8_max - mr2$timbre_8_min

mr2$energy       <- log(mr2$energy)
mr2$timbre_4_max <- log(mr2$timbre_4_max+1)
mr2$timbre_8_max <- log(mr2$timbre_8_max+26)



SongsTrain = mr2 %>% filter(year <= 2009)
SongsTest = mr2 %>% filter(year == 2010)

## remove all variable not related to y first and then rerun
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# To remove these variables from your training and testing sets:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

summary(SongsLog1)


# To generate the ROC curve
test_prob = predict(SongsLog1, newdata = SongsTest, type = "response")
par("mar")
par(mar=c(1,1,1,1))
test_roc = roc(SongsTest$Top10 ~ test_prob, plot = TRUE, print.auc = TRUE)


