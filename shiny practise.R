filename <- "C:/Users/TaoWa/Desktop/by_client_with_sequences.csv"
df1<- read.csv("C:/Users/TaoWa/Desktop/by_client_with_sequences.csv")


library(ggplot2)
library(tidyverse)
library(DT)
library(forcats)
library(scales)

library(shiny)


## quick and simple example
#ui<-fluidPage("Shiny Test")

ui<-fluidPage(
  sliderInput(inputId="num",
                           label="Choose a number",
                           value=25,min=1,max=100),
  plotOutput("hist"))

server<- function(input, output){
  output$hist<-renderPlot({
    title<-"100 random normal values"
    hist(rnorm(input$num), main=title)
    })
}

shinyApp(server=server, ui=ui)

