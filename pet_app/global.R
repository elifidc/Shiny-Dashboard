library(shiny)
library(shinydashboard)
library(tidyverse)     
library(plotly)
library(leaflet)
library(DT)
library(lubridate)
library(scales)
library(RColorBrewer)



df_clean <- read_csv("animal-shelter-intakes-and-outcomes.csv") %>%
  mutate(
    `Intake Condition` = str_squish(str_to_title(`Intake Condition`)),
    `Animal Type` = str_to_title(`Animal Type`),
    `Outcome Type` = str_to_title(`Outcome Type`),
    `Intake Type` = str_to_title(`Intake Type`),
    intake_month = floor_date(`Intake Date`, "month"),
    outcome_month = floor_date(`Outcome Date`, "month"),
    age_at_intake = if_else(!is.na(DOB), interval(DOB, `Intake Date`) / years(1), NA_real_),
    is_alive = was_outcome_alive == 1,
    `Outcome Grouped` = case_when(
      `Outcome Type` %in% c("Rescue", "Adoption", "Return To Rescue", "Foster To Adopt") ~ "Adoption/Rescue",
      TRUE ~ `Outcome Type`
    )
  ) %>%
  mutate(`Animal Type Grouped` = case_when(
    `Animal Type` %in% c("Cat", "Dog", "Rabbit", "Bird", "Wild", "Reptile") ~ `Animal Type`,
    TRUE ~ "Other"
  ))
