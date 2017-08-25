filename <- "C:/Users/TaoWa/Desktop/by_client_with_sequences.csv"
df1<- read.csv("C:/Users/TaoWa/Desktop/by_client_with_sequences.csv")


library(ggplot2)
library(tidyverse)
library(DT)
library(forcats)
library(plotly)

plot_ly(x=dff$collcode,y=dff$parents_college,z=dff$year,type="heatmap")

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
                           "multiple" = ".b" ))


## for q80 and year
t_q80 <- 
  table(df1$q80, df1$year) %>%
  as.data.frame() %>%
  rename(q80 = Var1, year = Var2, surveys = Freq) 

s_q80 <- 
  t_q80 %>%
  spread(key = year, value = surveys) %>%
  mutate(total = `13/14` + `14/15` + `15/16`) %>%
  arrange(desc(total))

table(dff$parents_college) # overall distribution for q80
ggplot(dff,aes(parents_college))+geom_bar()# total distubution for q80

t_q80 <- 
  table(dff$parents_college, df1$year) %>%
  as.data.frame() %>%
  rename(parents_college = Var1, year = Var2, surveys = Freq)

s_q80 <- 
  t_q80 %>%
  spread(key = year, value = surveys) %>%
  mutate(total = `13/14` + `14/15` + `15/16`) %>%
  arrange(desc(total))

s_q80%>%datatable()

ggplot(dff,aes(year,fill=parents_college))+geom_bar(position="fill")# plot for year, parents' education
###change value name
t_q80 <- 
  table(df1$q80, df1$year) %>%
  as.data.frame() %>%
  rename(parents_college = Var1, year = Var2, surveys = Freq) %>%
  mutate(parents_college = fct_recode(parents_college, 
                                      "yes" = "1", 
                                      "no" = "2", 
                                      "missing" = ".a", 
                                      "multiple" = ".b" ))
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
                                    "multiple" = ".b" ))
  



### position Adjustments
ggplot(df1,aes(year,fill=q80))+geom_bar(position="dodge")
ggplot(df1,aes(year,fill=q80))+geom_bar(position="fill")

ggplot(s_q80,aes(total,fill=q80))+geom_bar(position="")

## college *parents' education
c_parent<-table(df1$collcode,df1$q80) #q80 *college
c_parent

c_parent<-
  table(df1$collcode,df1$q80) %>%
  as.data.frame() %>%
  rename(collcode =Var1, q80 = Var2)

s_parent<-
  c_parent %>%
  spread(key=collcode,value=Freq)

ggplot(df1,aes(q80,collcode))+geom_jitter()# plot for college, parents' education

ggplot(df1,aes(collcode,fill=q80))+geom_bar(position="fill")# position adjustments plot for parents' education in each college

ggplot(df1,aes(q80,fill=collcode))+geom_bar(position="fill")# position adjustments plot for each college' parents education.

### gender * parents'education
g_parent<-table(df1$q72,df1$q80) #q72*q80

g_parent<-
  table(df1$q72,df1$q80) %>%
  as.data.frame() %>%
  rename(gender = Var1, q80= Var2)

sg_parent<-
  g_parent %>%
  spread(key=q80,value=Freq)
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

########## college*year*parents' education
table(df1$collcode,df1$q80)

##create new variable combine year and collcode


yc_parents<-
  table(df1$y_collcode,df1$q80)%>%
  as.data.frame()%>%
  rename(y_collcode=Var1,q80=Var2)

# college response nubmer change by year
q2<-ggplot(df1,aes(collcode,fill=y_collcode))+geom_bar(position="fill")
q2 + theme(axis.text.x=element_text(angle=45,hjust=1))

### three variables plots
ggplot(dff,aes(parents_college,collcode))+geom_raster(aes(fill=year),hjust=0.5,vjust=0.5,interpolate=FALSE)
ggplot(dff,aes(collcode,parents_college))+geom_raster(aes(fill=year),hjust=0.5,vjust=0.5,interpolate=FALSE)


ggplot(dff,aes(parents_college,collcode))+geom_tile(aes(fill=year))
ggplot(dff,aes(collcode,year))+geom_tile(aes(fill=parents_college))
ggplot(dff,aes(parents_college,year))+geom_tile(aes(fill=y_collcode))


# q80 distribution by college*year and change lable angle
yc_q80<-ggplot(df1,aes(y_collcode,fill=q80))+geom_bar(position="fill")
yc_q80 + theme(axis.text.x=element_text(angle=90,hjust=1))



##########heatmap



