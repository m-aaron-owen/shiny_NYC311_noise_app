library(shiny)
library(data.table)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(shinythemes)

noise = fread("noise_.csv", stringsAsFactors = F)