filename <- "C:/Users/TaoWa/Desktop/by_client_with_sequences.csv"
df1<- read.csv("C:/Users/TaoWa/Desktop/by_client_with_sequences.csv")


library(ggplot2)
library(tidyverse)
library(DT)
library(forcats)
library(scales)

library(shiny)

system.file("examples", package="shiny")

runExample("03_reactivity")
