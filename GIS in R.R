library(ggplot2)
library(leaflet)
library(ggmap)
library(tidyverse)

df1<- read.csv("C:/Users/TaoWa/Desktop/by_client_with_sequences.csv")

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
                         "multiple" = ".b"  ))%>%
  mutate(useful_skills=fct_recode(q13s,
                                  "Very Dissatisfied"="1",
                                  "Dissatisfied"="2",
                                  "Neither Satisfied nor Dissatisfied"="3",
                                  "Satisfied"="4",
                                  "Very Satisfied"="5",
                                  "missing" = ".a", 
                                  "multiple" = ".b"))%>%
  mutate(q13s=fct_recode(q13s,
                         "Very Dissatisfied"="1",
                         "Dissatisfied"="2",
                         "Neither Satisfied nor Dissatisfied"="3",
                         "Satisfied"="4",
                         "Very Satisfied"="5",
                         "missing" = ".a", 
                         "multiple" = ".b")) %>%
  mutate(q24s=fct_recode(q24s,
                         "Very Dissatisfied"="1",
                         "Dissatisfied"="2",
                         "Neither Satisfied nor Dissatisfied"="3",
                         "Satisfied"="4",
                         "Very Satisfied"="5",
                         "missing" = ".a", 
                         "multiple" = ".b"))%>%
  mutate(q39s=fct_recode(q39s,
                         "Very Dissatisfied"="1",
                         "Dissatisfied"="2",
                         "Neither Satisfied nor Dissatisfied"="3",
                         "Satisfied"="4",
                         "Very Satisfied"="5",
                         "missing" = ".a", 
                         "multiple" = ".b"))%>%
  mutate(q49s=fct_recode(q49s,
                         "Very Dissatisfied"="1",
                         "Dissatisfied"="2",
                         "Neither Satisfied nor Dissatisfied"="3",
                         "Satisfied"="4",
                         "Very Satisfied"="5",
                         "missing" = ".a", 
                         "multiple" = ".b"))

## transfer postal type from integer to character, so that we can use geocode
dff$postal<-as.character(dff$postal)

## group the data set by postcode, and count the number of each postcode
loglat <-
  dff %>%
  group_by(postal) %>%
  summarise(count= n())

## delete the 44th observatrion with na in postal
loglat <- loglat[-44,]

## find the log and lat of each post code
loglat$cologlat<- geocode(loglat$postal)

###########################################################################


## show the university of toronto via leaflet
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-79.39566, lat=43.66289, popup="University of Toronto")
m 


## show the postal of data set via leaflet
leaflet(loglat) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~cologlat[,1], ~cologlat[,2], popup=~postal, label= ~postal)


## marker clusters, showas the distribution of 43 postcode 
fulldata <- inner_join(dff, loglat, by = "postal")

leaflet(fulldata) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~cologlat[,1], ~cologlat[,2], popup=~postal, label= ~postal, clusterOptions = markerClusterOptions() )


## the code to check longitude/latitude:  geocode("university of toronto")
## string can be an address, zip code, or proper name, return the longitude/latitude coordinate for the center


#########################################################################
##  using ggmap to plot the graph
typeof(geocode("University of Waterloo"))

mylocation <- "University of Toronto"
myMap <- get_map(location=mylocation, 
                 source="google", 
                 maptype= "roadmap",
                 crop = FALSE,zoom = 10) 


 ggmap(myMap) + geom_point( data = dff, aes(x = long, y = lat),
             alpha= .5, color = "darkred", size=3)


 
 ## another way to do so

myMap <- get_map(location= "toronto", 
                 source="google", 
                 maptype= "roadmap",
                 crop = FALSE,zoom = 10) 

ggmap(myMap) + geom_point( data = loglat, aes(x = cologlat[,1], y = cologlat[,2]),
                           alpha= .5, color = "blue", size=3)




