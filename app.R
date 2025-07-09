# Load shiny and here package ####
library(shiny)
library(here)

# Load UI and server functions ####
source(
  file = here::here("scripts/app/ui.R")
)

source(
  file = here::here("scripts/app/server.R")
)

# Sourcing prevalence function ####
source(file = here::here("R/prevalence.R"))

# Running app ####
shiny::shinyApp(ui = ui(), server = server)
