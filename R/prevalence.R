# The purpose of this script file is to create a function that calculates the
# prevalence of CF carriers based on diagnoses.

# Creating function ####
prevalence <- function(data, dx, intersect = TRUE, p = 0.03) {
  # Checking parameter values ####
  if (!is.character(x = dx)) {
    stop("dx must be a character vector containing a list diagnoses labels.")
  }

  if (!is.logical(x = intersect)) {
    stop("intersect must be a logical value.")
  }

  if (!is.numeric(x = p) || p > 1) {
    stop("p must be a numeric value less than or equal to 1.")
  }

  # Preparing data for prevalence calculation ####

  ## Data for prevalence calculation ####
  dx_dat <- subset(x = data, select = dx)
  carrier <- data$carrier

  ## Calculating row sums to identify patients ####
  dx_rowsum <- rowSums(x = dx_dat)

  ## Identifying patients with diagnoses given intersection parameter ####
  if (intersect) {
    dx_exposure <- as.numeric(dx_rowsum == length(dx))
  } else {
    dx_exposure <- as.numeric(dx_rowsum >= 1)
  }

  ## Changing numeric values to factors to display contigency table ####
  dx_exposure <- factor(
    x = dx_exposure,
    levels = c(1, 0),
    labels = c("DX present", "DX absent")
  )

  carrier <- factor(
    x = carrier,
    levels = c(1, 0),
    labels = c("Carrier", "Control")
  )

  # Creating a contingency table of diagnoses exposure and carrier status ####
  tab <- stats::ftable(x = dx_exposure, y = carrier)

  ## Calculating row and column sums of the contigency table ####
  ## These vaules will allow one to calculate the prevalence of carriers given
  ## a set of conditions are present or prevalence of the conditions given
  ## carrier status.
  r_sums <- rowSums(x = tab)
  c_sums <- colSums(x = tab)

  # Calculating prevalence of carriers given conditions are present ####

  ## Number of patients with the conditions ####
  dx_no_patient <- r_sums[1]

  ## Prevalence of patietns with conditions ####
  dx_prev_patient <- dx_no_patient / sum(r_sums)

  ## Number of carriers with conditions ####
  dx_cf_no_patient <- tab[1, 1]

  ## Prevalence of carrier with conditions ####
  dx_cf_prev_patient <- dx_cf_no_patient / dx_no_patient

  ## Calculating bias-adjusted prevalence of carriers with conditions ####
  ## This bias-adjustment will be based on Bayes' rule. So, I will code this
  ## in terms of numerator and denominator of Bayes' rule.

  ### Numerator ####
  numerator <- (tab[1, 1] / c_sums[1]) * p

  ### Denominator ####
  denominator <- numerator + ((tab[1, 2] / c_sums[2]) * (1 - p))

  ### Bias-adjusted prevalence of carriers with conditions ####
  dx_cf_bias_adj_prev_patient <- numerator / denominator

  # Creating outputs object ####
  out <- data.frame(
    dx_no_patient = dx_no_patient,
    dx_prev_patient = dx_prev_patient,
    dx_cf_no_patient = dx_cf_no_patient,
    dx_cf_prev_patient = dx_cf_prev_patient,
    dx_cf_bias_adj_prev_patient = dx_cf_bias_adj_prev_patient
  )

  # Returning output object ####
  return(out)
}
