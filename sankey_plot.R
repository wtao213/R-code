#############################
## 2020-01-08
##


install.packages("dplyr")
install.packages("networkD3")

library(networkD3)
library(dplyr)


nodes = data.frame("name" = 
                     c("SD-t0", 
                       "WM-t0", 
                       "Multi-class-t0", 
                       "SD-t1", 
                       "WM-t1", 
                       "Multi-class-t1",
                       "SD-t3", 
                       "WM-t3", 
                       "Multi-class-t3",
                       "SD-t6", 
                       "WM-t6", 
                       "Multi-class-t6"))

##links of the transation, source, target and value

links = as.data.frame(matrix(c(
  0,3,1027,
  0,4,324,
  0,5,957,
  1,3,404,
  1,4,6444,
  1,5,1101,
  2,3,13,
  2,4,23,
  2,5,443,
  3,6,778,
  3,7,63,
  3,8,381,
  4,6,61,
  4,7,4722,
  4,8,353,
  5,6,123,
  5,7,61,
  5,8,1850,
  6,9,498,
  6,10,56,
  6,11,367,
  7,9,52,
  7,10,4422,
  7,11,224,
  8,9,123,
  8,10,63,
  8,11,2381
),
byrow = TRUE, ncol = 3))

names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)



#################################################
## version 2
## add color to  plots


###############################################################
## this version gouped color, and let them show up in turns
##

  
  nodes$group <- gsub(" ", "-", nodes$name)
  color_scale <- 
    "d3.scaleOrdinal()
     .domain(['SD-t0', 'WM-t0',  'Multi-class-t0', 
              'SD-t1', 'WM-t1',  'Multi-class-t1',
              'SD-t3', 'WM-t3',  'Multi-class-t3',
              'SD-t6', 'WM-t6',  'Multi-class-t6'])
     .range(['#a1d99b', '#9ecae1', '#bcbddc', '#a1d99b', '#9ecae1', '#bcbddc','#a1d99b', '#9ecae1', '#bcbddc', '#a1d99b', '#9ecae1', '#bcbddc']);
  "
## this version gouped color, and let them show up in turns
  sankeyNetwork(Links = links, Nodes = nodes,
                               Source = "source", Target = "target",
                               Value = "value", NodeID = "name",
                              fontSize= 12, nodeWidth = 30,iterations=0
                              ,NodeGroup ="group"
                              ,colourScale = color_scale) 
  
  
  
  
  
##########################################################
## find version to check the link color
## still try color link  , and we have 27 links
## on top of node groups, add lind groups
  
 
  
  links$group <- as.factor(c("1","1","1","2","2","2","3","3","3",
                             "1","1","1","2","2","2","3","3","3",
                             "1","1","1","2","2","2","3","3","3"))
  
  nodes$group <- gsub(" ", "-", nodes$name)

  color_scale <- 
    "d3.scaleOrdinal()
     .domain(['SD-t0', 'WM-t0',  'Multi-class-t0', 
              'SD-t1', 'WM-t1',  'Multi-class-t1',
              'SD-t3', 'WM-t3',  'Multi-class-t3',
              'SD-t6', 'WM-t6',  'Multi-class-t6','1','2','3'])
     .range(['#a1d99b', '#9ecae1', '#bcbddc', '#a1d99b', '#9ecae1', '#bcbddc','#a1d99b', '#9ecae1', '#bcbddc',
             '#a1d99b', '#9ecae1', '#bcbddc','#e5f5e0', '#deebf7', '#efedf5']);
  "   
 
  color_scale <- 
    "d3.scaleOrdinal()
     .domain(['SD-t0', 'WM-t0',  'Multi-class-t0', 
              'SD-t1', 'WM-t1',  'Multi-class-t1',
              'SD-t3', 'WM-t3',  'Multi-class-t3',
              'SD-t6', 'WM-t6',  'Multi-class-t6','1','2','3'])
     .range(['#31a354', '#3182bd', '#756bb1', '#31a354', '#3182bd', '#756bb1','#31a354', '#3182bd', '#756bb1',
             '#31a354', '#3182bd', '#756bb1','#a1d99b', '#9ecae1', '#bcbddc']);"

   
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30,iterations=0
              ,colourScale = color_scale,LinkGroup = "group")

