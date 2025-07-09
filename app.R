# Load shiny package
library(shiny)
library(here)


# Importing data ####
load(file = here::here("data/app/ui.rda"))
load(file = here::here("data/app/server.rda"))

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
