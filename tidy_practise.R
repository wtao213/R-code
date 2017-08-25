filename <- "C:/Users/TaoWa/Desktop/2013_14_Apprenticeship_Data.csv"
dfo <- read.csv("C:/Users/TaoWa/Desktop/2013_14_Apprenticeship_Data.csv")

####combine columns
dfo2<-dfo%>%
  gather(question,rank,q70:q74,na.rm=TRUE)
dfo2

###seperate columns
dfo3<-dfo2%>%spread(question,rank)
