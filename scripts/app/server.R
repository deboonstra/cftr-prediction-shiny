server <- function(input, output) {
  output$view <- shiny::renderTable(
    expr = {
      # Loading server data ###
      load(file = here::here("data/app/server.rda"))

      # Switching input intersect to logical ####
      if (input$calc == "union") {
        intersect_value <- FALSE
      } else {
        intersect_value <- TRUE
      }

      if (!is.null(input$dx)) {
        # Getting diagnoses ####
        if (!("all" %in% input$dx)) {
          dx <- input$dx
        } else {
          dx <- c(paste0("dx_0", 1:9), paste0("dx_", 10:62))
        }

        # Subsetting condition names data ####
        cftr_dx_conds <- cftr_dx_labels$condition[cftr_dx_labels$label %in% dx]

        # Subsetting data based on age and sex ####
        if (input$sex != "Sex not specified") {
          if (input$age != "Age not specified") {
            dat <- subset(
              x = cftr_dx_inds,
              subset = (sex == input$sex) & (age_grps == input$age)
            )
          } else {
            dat <- subset(x = cftr_dx_inds, subset = sex == input$sex)
          }
        } else if (input$sex == "Sex not specified") {
          if (input$age != "Age not specified") {
            dat <- subset(
              x = cftr_dx_inds,
              subset = age_grps == input$age
            )
          } else {
            dat <- cftr_dx_inds
          }
        }

        # Subsetting data based on the number of multiple visits ####
        multi_dx_count <- as.integer(input$multi_dx_count)
        if (input$multi_dx != "no_multi") {
          w <- which(x = cftr_dx_counts[, input$multi_dx] >= multi_dx_count)
          dat_counts <- cftr_dx_counts[w, ]
          dat_counts <- subset(x = dat_counts, select = c(patient_id))
          dat <- merge(
            x = dat,
            y = dat_counts,
            by = "patient_id",
            all.y = TRUE
          )
        } else {
          dat <- dat
        }

        # Calculating prevalence ####
        if (input$calc != "individual") {
          out <- round(
            x = prevalence(
              data = dat,
              dx = dx,
              intersect = intersect_value,
              p = input$pop_prev
            ),
            digits = 3
          )
        } else {
          out <- lapply(
            X = dx,
            FUN = function(x) {
              round(
                x = prevalence(
                  data = dat,
                  dx = x,
                  p = input$pop_prev
                ),
                digits = 3
              )
            }
          )
          out <- do.call(what = "rbind", args = out)
        }

        # Adding condition names to data ####
        if (input$calc == "individual") {
          out <- cbind(
            data.frame(
              cftr_dx_conds = cftr_dx_conds
            ),
            out
          )
          out <- out[
            order(out$dx_cf_bias_adj_prev_patient, decreasing = TRUE),
          ]
        } else {
          out <- cbind(
            data.frame(
              cftr_dx_conds = paste0(cftr_dx_conds, collapse = "; \n")
            ),
            out
          )
        }
      } else {
        out <- data.frame(
          matrix(
            data = "",
            nrow = 1,
            ncol = 6,
            dimnames = list(NULL, letters[1:6])
          )
        )
      }

      # Column names ####
      colnames(out) <- c(
        "CF-related conditions",
        "Number of patients with conditions",
        "Prevalence of patients with conditions",
        "Number of CF carries with conditions",
        "Prevalence of CF carriers with conditions",
        "Bias-adjusted prevelance of CF carriers with conditions"
      )

      # Sending out prevalence
      out
    }
  )
}
