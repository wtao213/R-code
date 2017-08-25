##########useful_skills analysis
filename <- "C:/Users/TaoWa/Desktop/by_client_with_sequences.csv"
df1<- read.csv("C:/Users/TaoWa/Desktop/by_client_with_sequences.csv")


library(ggplot2)
library(tidyverse)
library(DT)
library(forcats)
library(scales)


df1$collcode_y<-as.character(paste(df1$collcode, df1$year, sep = "_"))
df1$y_collcode<-as.character(paste(df1$year, df1$collcode, sep = "_"))

dff<-
  df1%>%
  rename(parents_college = q80)%>%
  mutate(parents_college = fct_recode(parents_college, 
                                    "yes" = "1", 
                                    "no" = "2", 
                                    "missing" = ".a", 
                                    "multiple" = ".b" ))%>%
  rename(gender = q72)%>%
  mutate(gender = fct_recode(gender, 
                           "female" = "1", 
                           "male" = "2", 
                           "uncertain" = "3", 
                           "missing" = ".a", 
                           "multiple" = ".b" ))%>%
  rename(coop = q79)%>%
  mutate(coop = fct_recode(coop, 
                         "yes" = "1", 
                         "no" = "2", 
                         "unknown" = "3", 
                         "missing" = ".a", 
                         "multiple" = ".b"  ))%>%
  mutate(useful_skills = fct_recode(q13s, 
                         "Very Dissatisfied" = "1", 
                         "Dissatisfied" = "2", 
                         "Neither Satisfied nor Dissatisfied" = "3", 
                         "Satisfied" = "4", 
                         "Very Satisfied" = "5", 
                         "missing" = ".a", 
                         "multiple" = ".b"))%>%
  mutate(q13s = fct_recode(q13s, 
                                  "Very Dissatisfied" = "1", 
                                  "Dissatisfied" = "2", 
                                  "Neither Satisfied nor Dissatisfied" = "3", 
                                  "Satisfied" = "4", 
                                  "Very Satisfied" = "5", 
                                  "missing" = ".a", 
                                  "multiple" = ".b"))
## group dataset dff to get the var percentage of useful_skills for 4, 5
##############################################################################################  
## method 1
g_dff <-
  dff %>%
  group_by(collcode, year) %>%
  mutate(is_satisfied = (useful_skills %in% c("Satisfied", "Very Satisfied"))) %>%
  summarise(n = n(), 
            num_satisfied = sum(is_satisfied), 
            pct_satisfied = sum(is_satisfied) / n())

## method 2
g2_dff %>%
  group_by(collcode, year) %>%
  summarize(num_satisfied = sum(useful_skills %in% c("Satisfied", "Very Satisfied")))



################################################################
## reorder the plot
## method 1:
p3<-ggplot(dff,aes(x=reorder(collcode,q13s=="Satisfied",.desc=TRUE),fill=q13s))+geom_bar(position="fill")# position adjustments plot for q13s in each college
## method 2:
p3<-ggplot(dff,aes(x=reorder(collcode,q13s %in% c("Satisfied", "Very Satisfied"),.desc=TRUE),fill=q13s))+geom_bar(position="fill")


## for q13a and year

t_useful_skills <- 
  table(dff$useful_skills, dff$year) %>%
  as.data.frame() %>%
  rename(useful_skills = Var1, year = Var2, surveys = Freq) 

s_useful_skills <- 
  t_useful_skills %>%
  spread(key = year, value = surveys) %>%
  mutate(total = `13/14` + `14/15` + `15/16`) %>%
  arrange(desc(total))

table(dff$useful_skills) # overall distribution for useful_skills
ggplot(dff, aes(useful_skills))+geom_bar()# total distubution for useful_skills

s_useful_skills%>%datatable()

ggplot(dff, aes(year, fill = useful_skills))+geom_bar(position = "fill")# plot for year, parents' education

## college *useful_skills
# c_useful_skills<-table(dff$collcode, dff$useful_skills) #useful_skills *college


c_useful_skills<-
  table(dff$collcode, dff$useful_skills) %>%
  as.data.frame() %>%
  rename(collcode  = Var1, useful_skills = Var2)

s_useful_skills<-
  c_useful_skills %>%
  spread(key = collcode, value = Freq)

ggplot(dff, aes(useful_skills, collcode))+geom_jitter()# plot for college, parents' education

ggplot(dff, aes(collcode, fill = useful_skills))+geom_bar(position = "fill")# position adjustments plot for coop in each college

ggplot(dff, aes(useful_skills, fill = collcode))+geom_bar(position = "fill")# position adjustments plot for each college' coop.

### gender * parents'education
g_useful_skills<-table(dff$gender, dff$useful_skills) #q72*q79

g_useful_skills<-
  table(dff$gender, dff$useful_skills) %>%
  as.data.frame() %>%
  rename(gender = Var1, useful_skills =  Var2)

sg_useful_skills<-
  g_useful_skills %>%
  spread(key = useful_skills, value = Freq)

sg_useful_skills%>%datatable()

ggplot(dff, aes(gender, fill = useful_skills))+geom_bar(position = "fill")

ggplot(dff, aes(useful_skills, fill = gender))+geom_bar(position = "fill")
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

slice(s_trade, 1:10)%>% datatable() #cut the top 10 trades in total

#subset observations
top<-c("309A", "310S", "306A", "403A", "310T", "620C", "433A", "313A", "308A", "415A")
top_data<-filter(df1, trade%in%top)

ggplot(top_data, aes(q13s, fill = trade))+geom_bar(position = "fill") # coop distribution by top trades
ggplot(top_data, aes(trade, fill = q13s))+geom_bar(position = "fill") # coop distribution for top trades


########## college*year*useful_skills
cy_useful_skills<-
  table(dff$collcode_y, dff$useful_skills)%>%
  as.data.frame()%>%
  rename(collcode_y = Var1, useful_skills = Var2)

cy_useful_skills%>%datatable()

##create new variable combine year and collcode


yc_parents<-
  table(dff$y_collcode, dff$useful_skills)%>%
  as.data.frame()%>%
  rename(y_collcode = Var1, useful_skills = Var2)

# college response nubmer change by year
ggplot(dff, aes(collcode, fill = y_collcode))+geom_bar(position = "fill")

# q80 distribution by college*year and change lable angle
yc_q79<-ggplot(dff, aes(y_collcode, fill = useful_skills))+geom_bar(position = "fill")
yc_q79 + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(dff, aes(useful_skills, year))+geom_tile(aes(fill = y_collcode))

# useful_skills distribution by college*year and change lable angle
yc_q13<-ggplot(dff, aes(collcode_y, fill = useful_skills))+geom_bar(position = "fill")
yc_q13 + theme(axis.text.x = element_text(angle = 90, hjust = 1))

p8<-ggplot(dff, aes(useful_skills, year))+geom_tile(aes(fill = y_collcode))
p8 + theme(axis.text.x = element_text(angle = 45, hjust = 1))


p9<-ggplot(dff, aes(useful_skills, collcode))+geom_tile(aes(fill = year)) # highest response year of each college for each response rate
p9 + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p10<-ggplot(dff, aes(collcode, year))+geom_tile(aes(fill = useful_skills)) # highest response rate of each college by year
p10 + theme(axis.text.x = element_text(angle = 45, hjust = 1))



#############What I want is the most like the last graph in the file you just sent. But what I want is:
#### x = year
#### y = college
### z (color) = % of the responses that were either 4 or 5

### you'll have to create a calculated column to use as the color variable - percent satisfied. 
ggplot(dff, aes(year, collcode))+geom_tile(aes(fill = useful_skills)) # highest response rate of each college by year
p10 + theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### heat map
# heat map
g_dff <-
  dff %>%
  group_by(collcode, year) %>%
  mutate(is_satisfied = (q13s %in% c("Satisfied", "Very Satisfied"))) %>%
  summarise(n = n(), 
            num_satisfied = sum(is_satisfied), 
            pct_satisfied = sum(is_satisfied) / n())%>%
            mutate(p_satisfied =  paste(round(100*pct_satisfied, 2), "%", sep = "")) #column with percentage

p11<-
  ggplot(g_dff, aes(year, collcode)) +
  geom_tile(aes(fill = pct_satisfied)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "white",  high = "coral") + 
  labs(title="Graph 1:Satisfied rate by college and year", subtitle="made by xxx", caption="removing 1,7, 10 due to....")

print(p11)

#add value on it, size 2 is front size, round(, 2) 2 is the digital number we want
p12<-ggplot(g_dff, aes(year, collcode))+geom_tile(aes(fill = pct_satisfied))+geom_text(aes(label = round(pct_satisfied, 2), size = 2))

## change cell sieze: ggplot(g_dff, aes(year, collcode))+geom_tile(aes(fill = pct_satisfied, width = 0.5, height = 1))
##   try 2:  ggplot(g_dff, aes(year, collcode))+geom_tile(aes(fill = pct_satisfied))+geom_bin2d(binwidth = c(0.5, 0.5))

g_dff <-
  dff %>%
  group_by(collcode, year) %>%
  mutate(not_satisfied = (q13s %in% c("Dissatisfied", "Very Dissatisfied"))) %>%
  summarise(n = n(), 
            num_dsatisfied = sum(not_satisfied), 
            pct_dsatisfied = sum(not_satisfied) / n()) %>%
            mutate(p_dsatisfied =  percent(pct_dsatisfied))

p12 <-
  ggplot(g_dff, aes(year, collcode)) + 
  geom_tile(aes(fill = pct_dsatisfied)) + 
  geom_text(aes(label = percent(round(pct_dsatisfied, 2)), size = 2)) + #add label of each cell
  scale_fill_gradient(low = "white",  high = "coral") +                 #change color
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +            #change lable direction
  ggtitle("Graph 2:Dissatisfied rate by college and year") +            #add title
  labs(subtitle="made by xxx", caption="removing 1,7, 10 due to....")                           #add caption, but can also add title, x-axis,y-axis
  
  
print(p12)

# library(plotly)
# ggplotly(p12)

#p_dsatisfied =  paste(round(100*pct_dsatisfied, 2), "%", sep = "" change to percentage
#as.numeric(sub("%", "", g_dff$p_dsatisfied)) change percentage to numeric
percent(g_dff$pct_dsatisfied)
is.numeric(g_dff$pct_dsatisfied)
