filename <- "C:/Users/TaoWa/Desktop/by_client_with_sequences.csv"
df1<- read.csv("C:/Users/TaoWa/Desktop/by_client_with_sequences.csv")


library(ggplot2)
library(tidyverse)
library(DT)
library(forcats)


df1$collcode_y<-as.character(paste(df1$collcode,df1$year,sep="_"))
df1$y_collcode<-as.character(paste(df1$year,df1$collcode,sep="_"))

dff<-
  df1%>%
  rename(parents_college=q80)%>%
  mutate(parents_college=fct_recode(parents_college, 
                                    "yes" = "1", 
                                    "no" = "2", 
                                    "missing" = ".a", 
                                    "multiple" = ".b" ))%>%
  rename(gender=q72)%>%
  mutate(gender=fct_recode(gender, 
                           "female" = "1", 
                           "male" = "2", 
                           "uncertain" = "3",
                           "missing" = ".a", 
                           "multiple" = ".b" ))%>%
  rename(coop=q79)%>%
  mutate(coop=fct_recode(coop,
                         "yes" = "1", 
                         "no" = "2", 
                         "unknown" = "3",
                         "missing" = ".a", 
                         "multiple" = ".b"  ))

## for q79 and year

t_q79 <- 
  table(dff$coop, dff$year) %>%
  as.data.frame() %>%
  rename(coop = Var1, year = Var2, surveys = Freq) 

s_q79 <- 
  t_q79 %>%
  spread(key = year, value = surveys) %>%
  mutate(total = `13/14` + `14/15` + `15/16`) %>%
  arrange(desc(total))

table(dff$coop) # overall distribution for q79
ggplot(dff,aes(coop))+geom_bar()# total distubution for q79

s_q79%>%datatable()

ggplot(dff,aes(year,fill=coop))+geom_bar(position="fill")# plot for year, parents' education

## college *coop
c_parent<-table(dff$collcode,dff$coop) #q80 *college
c_parent

c_parent<-
  table(dff$collcode,dff$coop) %>%
  as.data.frame() %>%
  rename(collcode =Var1, coop = Var2)

s_parent<-
  c_parent %>%
  spread(key=collcode,value=Freq)

ggplot(dff,aes(coop,collcode))+geom_jitter()# plot for college, parents' education

ggplot(dff,aes(collcode,fill=coop))+geom_bar(position="fill")# position adjustments plot for coop in each college

ggplot(dff,aes(coop,fill=collcode))+geom_bar(position="fill")# position adjustments plot for each college' coop.

### gender * parents'education
g_parent<-table(dff$gender,dff$coop) #q72*q79

g_parent<-
  table(dff$gender,dff$coop) %>%
  as.data.frame() %>%
  rename(gender = Var1, coop= Var2)

sg_parent<-
  g_parent %>%
  spread(key=coop,value=Freq)
g_parent

##### top 10 trade in year
t_trade <- 
  table(df1$trade, df1$year) %>%
  as.data.frame() %>%
  rename(trade = Var1, year = Var2, surveys = Freq) #dataframe table by trade


s_trade <- 
  t_trade %>%
  spread(key = year, value = surveys) %>%
  mutate(total = `13/14` + `14/15` + `15/16`) %>%
  arrange(desc(total))

slice(s_trade,1:10)%>% datatable() #cut the top 10 trades in total

#subset observations
top<-c("309A","310S","306A","403A","310T","620C","433A","313A","308A","415A")
top_data<-filter(df1,trade%in%top)

ggplot(top_data,aes(coop,fill=trade))+geom_bar(position="fill") # coop distribution by top trades
ggplot(top_data,aes(trade,fill=coop))+geom_bar(position="fill") # coop distribution for top trades


########## college*year*parents' education
table(dff$collcode_y,dff$coop)

##create new variable combine year and collcode


yc_parents<-
  table(dff$y_collcode,dff$coop)%>%
  as.data.frame()%>%
  rename(y_collcode=Var1,coop=Var2)

# college response nubmer change by year
ggplot(dff,aes(collcode,fill=y_collcode))+geom_bar(position="fill")

# q80 distribution by college*year and change lable angle
yc_q79<-ggplot(dff,aes(y_collcode,fill=coop))+geom_bar(position="fill")
yc_q79 + theme(axis.text.x=element_text(angle=90,hjust=1))