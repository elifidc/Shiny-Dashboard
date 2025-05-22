# app.R
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(lubridate)
library(scales)
library(RColorBrewer)

# app.R


source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)