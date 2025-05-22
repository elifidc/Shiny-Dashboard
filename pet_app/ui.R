library(DT)
library(lubridate)
library(scales)


library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(lubridate)
library(scales)
library(RColorBrewer)



default_end <- max(df_clean$`Intake Date`, na.rm = TRUE)
default_start <- default_end %m-% months(6)


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Animal Shelter Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Intake Analysis", tabName = "intake", icon = icon("arrow-right")),
      menuItem("Outcome Analysis", tabName = "outcome", icon = icon("arrow-left")),
      menuItem("Geographic Analysis", tabName = "geo", icon = icon("map"))
    ),
    
    conditionalPanel(
      condition = "input.tabs != 'overview'",
      selectInput(
        inputId = "animalType",
        label = "Animal Type:",
        choices = NULL
      ),
      dateRangeInput(
        inputId = "dateRange",
        label = strong("📅 Filter by Date Range"),
        start = Sys.Date() - 90,
        end = Sys.Date(),
        format = "yyyy-mm-dd",
        width = "100%"
      )
    )
  ),
  dashboardBody(
    tabItems(
      
      # OVERVIEW TAB
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("totalAnimals", width = 3),
          valueBoxOutput("avgStay", width = 3),
          valueBoxOutput("currentInShelter", width = 3),
          valueBoxOutput("overdueAnimals", width = 3)
        ),
        fluidRow(
          valueBoxOutput("recentAdoptionBox", width = 3),
          valueBoxOutput("catCountBox", width = 3),
          valueBoxOutput("dogCountBox", width = 3),
          valueBoxOutput("adoptionsInRange", width = 3)
        ),
        fluidRow(
          box(
            title = tags$div("Distribution of Current Animals in Shelter",
                             style = "text-align:center; font-size:20px; font-weight:bold;"),
            width = 12,
            plotlyOutput("animalTypeBar")
          )
        ),
        fluidRow(
          box(
            title = "Distribution of Current Shelter Stays",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("durationInShelterPlot", height = 300)
          ),
          box(
            title = "Recent Outcome Types (Last 30 Days)", 
            status = "primary",                             
            solidHeader = TRUE,                             
            width = 6,
            plotlyOutput("outcomeLollipop")
          )
        )
      ),
      
      # INTAKE TAB
      tabItem(
        tabName = "intake",
        fluidRow(
          valueBoxOutput("totalIntakes", width = 3),
          valueBoxOutput("avgDailyIntakes", width = 3),
          valueBoxOutput("mostCommonIntakeType", width = 3),
          valueBoxOutput("mostCommonCondition", width = 3)
        ),
        fluidRow(
          box(title = "Monthly Intake Trends", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("monthlyIntakePlot", height = "300px"))
        ),
        fluidRow(
          box(status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("groupedConditionsPlot", height = "300px")),
          box(status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("intakeTypePlot", height = "300px"))
        )
      ),
      
      # OUTCOME TAB
      tabItem(
        tabName = "outcome",
        fluidRow(
          valueBoxOutput("quickOutcomes", width = 4),
          valueBoxOutput("successRateBox", width = 4),
          valueBoxOutput("failureRateBox", width = 4)
        ),
        fluidRow(
          box(
            title = "Outcome Type Breakdown",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("outcomeByAnimal", height = "450px")
          )
        ),
        fluidRow(
          box(title = "Monthly Outcome Trends", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("monthlyOutcomePlot", height = 300))
        ),
        fluidRow(
          box(title = "Length of Stay by Animal Type", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("losByType", height = 300))
          # (commented out plot was here)
        )
      ),
      
      # GEO TAB
      tabItem(
        tabName = "geo",
        fluidRow(
          box(title = "Intake Locations", status = "primary", solidHeader = TRUE, width = 12,
              leafletOutput("intakeMap", height = 600))
        ),
        fluidRow(
          box(title = "Intake Density by Jurisdiction", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("jurisdictionHeat", height = 300))
        )
      )
      
    )  # END of tabItems()
  )    # END of dashboardBody()
)
  