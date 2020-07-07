
###########################################################
##  want to combine customer info with neibouhood info together
## start date: 2020-july-03
##

library(ggplot2)
library(leaflet)
library(ggmap)
library(tidyverse)
library(dplyr)


library(grid)
library(geojsonio)
library(rmapshaper)
library(rgdal)
library(tidyverse)
library(spdplyr)
library(sf)

##########
## https://blog.exploratory.io/making-maps-for-canadas-provisions-and-census-divisions-in-r-c189b88ccd8a
## get the canada division bound !!!!!!!

# https://cengel.github.io/R-spatial/mapping.html example
canada <- geojsonio::geojson_read("C:/Users/012790/Desktop/GIS/canada_divisions.geojson", what = "sp")
df<- read.csv("C:/Users/012790/Desktop/GIS/cus_355k_0703.csv")

canada$CDNAME
canada$CDUID

m <- leaflet(canada) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m %>% addPolygons()

m %>% addPolygons(data = canada, color = 'green', opacity = 0.4,popup=paste("Stratum:",canada@data$CDNAME,"<br>"))



m <- leaflet(canada) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
    addPolygons()

addProviderTiles("OpenStreetMap.Mapnik") %>%


bins <- c(0,50, 100,150,200,250,300)
pal <- colorBin("YlOrRd", domain = canada$rmapshaperid, bins = bins)


###############################
# this is kind of final version, polygan plus base map

m %>% addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(
  fillColor = ~pal(rmapshaperid),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.3)%>%
  addLegend(pal = pal, values = ~rmapshaperid, opacity = 0.5, title = NULL,
            position = "bottomright")

################################
# get the bounds zip
# https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R


map.getBounds();

#
zipsInBounds <- reactive({
  if (is.null(input$map_bounds))
    return(zipdata[FALSE,])
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  
  subset(zipdata,
         latitude >= latRng[1] & latitude <= latRng[2] &
           longitude >= lngRng[1] & longitude <= lngRng[2])
})





## leaflet by heat map??
leaflet(quakes) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addWebGLHeatmap(lng=~long, lat=~lat, intensity = ~mag, size=60000)




#####################################################
## newest version of customer info



leaflet(df) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~LONGITUDE, ~LATITUDE, popup=~rank_ver_3_0, label= ~rank_ver_3_0, clusterOptions = markerClusterOptions())




#########################################
## data manipulate

a <- canada@data

a$CDUID <-as.numeric(a$CDUID)

remove(a)
remove(canada)
# , row.names = FALSE
write.csv(a,"C:/Users/012790/Desktop/GIS/sp293.csv", row.names = FALSE)



# read data frame and merge
sp <- read.csv("C:/Users/012790/Desktop/GIS/prcd_293_v2.csv")

sp$CODE <- as.character(sp$CODE)

#merge sp and a, just use spatial dataframe as normal dataframe!
canada2 <- merge(canada, sp, by.x = "CDUID", by.y = "CODE")

# try out on map
m <- leaflet(canada2) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

# look at property WSPRIMREAVG min:89768.01 max: 1080000.0
bins <- c(80000,200000,400000,600000,800000,900000,1100000)
pal <- colorBin("YlOrRd", domain = canada2$WSPRIMREAVG, bins = bins)


m %>% addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(
    fillColor = ~pal(WSPRIMREAVG),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.5)%>%
  addLegend(pal = pal, values = ~WSPRIMREAVG, opacity = 0.5, title = NULL,
            position = "bottomright")



## add existing customer info
addMarkers(~LONGITUDE, ~LATITUDE, popup=~rank_ver_3_0, label= ~rank_ver_3_0, clusterOptions = markerClusterOptions())



m <- leaflet() %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))%>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(data=canada2,
    fillColor = ~pal(WSPRIMREAVG),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.5)%>%
  addLegend(data=canada2,pal = pal, values = ~WSPRIMREAVG, opacity = 0.5, title = NULL,
            position = "bottomright") %>%
  addMarkers(data=df,~LONGITUDE, ~LATITUDE, popup=~rank_ver_3_0, label= ~rank_ver_3_0, clusterOptions = markerClusterOptions())

m