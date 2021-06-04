library(magrittr)

seir <- ""
source("data_prep.R")

run_scenario <-
  function(cv,
           mod_day = 10,
           mod_length = 10,
           mod_multiplier = 2, add_days = 0) {
    scenario_df <- cv$data
    cm_array <- cv$cm_array
    if (add_days > 0) {
      max_date <- as.Date(max(scenario_df$Date), format = "%m/%d/%Y")
      days <- seq(max_date + 1, max_date + add_days, by = "days")

      beta <- rep(scenario_df$beta[dim(scenario_df)[1]], add_days)
      scenario_df <- scenario_df %>%
        add_row(County = rep("Wake", add_days), Date = days, beta = beta)
      cm_array <- c(cm_array, rep(cm_array[length(cm_array)], add_days))
    }

    beta <- scenario_df$beta
    beta[mod_day:(mod_day + mod_length)] <-
      beta[mod_day:(mod_day + mod_length)] * mod_multiplier

    # Rebind
    beta[which(beta > 1)] <- 1
    beta[which(beta < 0)] <- 0

    scenario_df$beta <- beta

    min_date <- as.Date(min(scenario_df$Date), format = "%m/%d/%Y")
    max_date <- as.Date(max(scenario_df$Date), format = "%m/%d/%Y")
    days <- as.integer(max_date - min_date)

    pop <- cv$Population
    cm <- cv$starting_cases
    alpha <- cv$alpha
    gamma <- cv$gamma
    seir_values <- seir(days, pop, cm, alpha, gamma, beta)
    p <- seir_values[["predicted"]]

    scenario_df$scenario_infections <- p
    scenario_df$scenario_infections_cum <- cumsum(p)
    scenario_df$scenario_cases <- scenario_df$scenario_infections / cm_array
    scenario_df$scenario_cases_cum <- cumsum(scenario_df$scenario_cases)
    scenario_df$susceptible <- seir_values[["susceptible"]]
    scenario_df$infectious <- seir_values[["infectious"]]
    scenario_df$si <- scenario_df$susceptible * scenario_df$infectious

    return(scenario_df)
  }

prepare_analysis_df <- function(cv, days = 300, length = 1, cm = 1) {
  analysis_df <- dplyr::as_tibble(data.frame("day" = c(1:days)))

  base_scenario <- run_scenario(cv, 1, 1, 1)

  cum_cases <- tail(base_scenario$cum_cases, 1)
  c_red <- c()
  cum_infections <- tail(base_scenario$cum_infections, 1)
  i_red <- c()
  peak_i <- max(base_scenario$scenario_infections)
  peak_red_i <- c()
  peak_c <- max(base_scenario$scenario_cases)
  peak_red_c <- c()

  for (i in 1:days) {
    run <- run_scenario(cv, i, length, cm)
    peak_red_c <- c(peak_red_c, peak_c - max(run$scenario_cases))
    peak_red_i <- c(peak_red_i, peak_i - max(run$scenario_infections))
    c_red <- c(c_red, cum_cases - tail(run$scenario_cases_cum, 1))
    i_red <- c(i_red, cum_infections - tail(run$scenario_infections_cum, 1))
  }

  analysis_df$cases_reduction <- c_red
  analysis_df$infections_reduction <- i_red
  analysis_df$peak_reduction_cases <- peak_red_c
  analysis_df$peak_reduction_infections <- peak_red_i

  base_scenario <- run_scenario(cv, 1, 1, 1)
  analysis_df$susceptible <- base_scenario$susceptible[1:days]
  analysis_df$si <- base_scenario$si[1:days]
  analysis_df$beta <- base_scenario$beta[1:days]
  return(analysis_df)
}