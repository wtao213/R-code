
filename <- "C:/Users/TaoWa/Desktop/by_client_with_sequences.csv"
df1<- read.csv("C:/Users/TaoWa/Desktop/by_client_with_sequences.csv")


library(ggplot2)
library(tidyverse)
############ one way frequency table
########### answers for gender, year, parents' college, 
table(df1$year)
t_parent<-table(df1$year,df1$q80)#q80 is parents college, Q80* year
t_parent
c_parent<-table(df1$collcode,df1$q80) #q80 *college
c_parent
g_parent<-table(df1$q72,df1$q80) #q72*q80
g_parent

t_gender<-table(df1$year,df1$q72)#q72 is gender

prop.table(t_gender,1)   # row percentage
prop.table(t_gender,2)   # column percentage
prop.table(t_gender)     # cell percentage




#Start with q80 - parents' college. Make tables and plots that break it down by:
# 1.	Overall (i.e., how many of each answer in the whole data set)
#2.	Year
#3.	College
#4.	Sector
#5.	Gender
#6.	Top 10 most registered trades
#7.	Year x college
#8.	Something you're interested in

######### two way frequency table
######### year * trade, year* college
t_college<-table(df1$collcode,df1$year)
t_trade <- 
  table(df1$trade, df1$year) %>%
  as.data.frame() %>%
  rename(trade = Var1, year = Var2, surveys = Freq) 
  

s_trade <- 
  t_trade %>%
  spread(key = year, value = surveys) %>%
  mutate(total = `13/14` + `14/15` + `15/16`) %>%
  arrange(desc(total))

top_10<-slice(s_trade,1:10)


  arrange(`13/14`)

# s_trade<-sort(table(t_trade),decreasing = T)
# 
# #sort table by numeric variable:  s_trade[order(t_trade$trade),]
# s_trade[order(t_trade$trade),decreasing=T] # dosen't work
# s_trade



################  for client dataset summary
full_tables<-list()

nl_names <-c(seq(1,187))

nl<-names(df1)
for(i in 1:length(nl)){
#  print(i)
  full_tables[[nl_names[i]]]<-table(df1[,nl[[i]]], useNA = "ifany")
}
sum_talbe2<-Reduce(`+`, full_tables)


## plots
hist(df1$collcode)

ggplot(df1,aes(collcode))+geom_bar()

ggplot(df1,aes(q4s))+geom_bar()

######macro plots
f_plots<-list()

#plots_all
#instead of aes(variable), use aes_string("variable") to indicate the x-axis.
for(i in 1:length(nl)){
  f_plots[[nl_names[i]]] <- ggplot(df1, aes_string(df1[, nl[i]])) + geom_bar()
}
f_plots[[2]]
        
df1 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

###########
### discrete X, discrete Y
ggplot(df1,aes(q80,q72))+geom_jitter()