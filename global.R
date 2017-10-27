library(shiny)
library(data.table)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(shinythemes)

noise = fread("noise2.csv", stringsAsFactors = F)