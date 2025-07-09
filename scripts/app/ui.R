ui <- function() {
  # Loading UI data ###
  load(file = here::here("data/app/ui.rda"))

  # UI ####
  shiny::pageWithSidebar(
    headerPanel = shiny::headerPanel(
      title = paste0(
        "Prevalence of Cystic Fibrosis (CF) carriers",
        " given CF-related conditions"
      )
    ),
    sidebarPanel = shiny::sidebarPanel(
      # Input for sex ####
      shiny::selectInput(
        inputId = "sex",
        label = "What is the sex of the possible carriers?",
        choices = sex_choices,
        selected = "Sex not specified",
        multiple = FALSE
      ),

      # Input for age ####
      shiny::selectInput(
        inputId = "age",
        label = "What is the age of the possible carriers?",
        choices = age_choices,
        selected = "Age not specified",
        multiple = FALSE
      ),

      # Input for calculation of prevalence ####
      shiny::selectInput(
        inputId = "calc",
        label = "How should the prevalence be calculated?",
        choices = calc_choices,
        selected = "individual",
        multiple = FALSE
      ),

      # Input for diagnoses ####
      shiny::selectInput(
        inputId = "dx",
        label = "What CF-related conditions do the patients have?",
        choices = dx_choices,
        selected = "all",
        multiple = TRUE
      ),

      # Input for multiple visits given a diagnosis
      shiny::selectInput(
        inputId = "multi_dx",
        label = paste0(
          "Which CF–related condition has prompted multiple clinical visits",
          " for this patient?"
        ),
        choices = multi_dx_choices,
        selected = "no_multi",
        multiple = FALSE
      ),

      # Input for multiple visit count ####
      shiny::selectInput(
        inputId = "multi_dx_count",
        label = paste0(
          "How many clinical visits has the patient had for the specified",
          "CF–related condition?"
        ),
        choices = multi_dx_count_choices,
        selected = "1",
        multiple = FALSE
      ),

      # Input for population prevalence ####
      shiny::numericInput(
        inputId = "pop_prev",
        label = "What is the population prevalence of being a CF carrier?",
        min = 0,
        max = 1,
        value = 0.03,
        step = 0.01
      )
    ),
    mainPanel = shiny::mainPanel(
      shiny::tableOutput(outputId = "view")
    )
  )
}
