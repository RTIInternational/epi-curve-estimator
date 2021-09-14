library("lubridate")
library("tidyverse")
library("magrittr")

print(getwd())
source("graphics.R")

# ----- Raw Data
county_data <- tibble(read.csv("data/covid19_cases.csv", header = T)) %>%
  select(-c(County.FIPS))
county_pop <- tibble(read.csv("data/population.csv", header = T))

# --- Rename columns, update the date column, fix the county name
county_data <- county_data %>%
  rename(CumTot = Total.Confirmed.Cases, Cases = New.Cases) %>%
  mutate(Date = ymd(county_data$Date)) %>%
  mutate(County = str_trim(as.character(county_data$County), side = c("right")))

# --- Create an "Epidemic Day" column, order the df, and add population
d1 <- min(county_data$Date)
county_data <- county_data %>%
  mutate(Day = as.integer(county_data$Date - d1 + 1), .after = "Date") %>%
  arrange(county_data$County, county_data$Date) %>%
  select(-c(CumTot))
# Fix names
colnames(county_data) <-
  c("Region", "Date", "Day", "cum_cases", "cases")

# --- Unique County Names
tt2 <- unique(county_data$Region)


# ----- Our Main Function
calc_beta <-
  function(cases_data,
           population,
           region = "Wake",
           cm_start = 10,
           cm_end = 4,
           tlat = 5,
           tinf = 6) {
    values <- list()
    # ----- Grab the region: Drop days before first case
    df <- cases_data %>%
      filter(Region == region) %>%
      filter(cum_cases > 0)
    values["Population"] <- population
    values["Region"] <- region

    # ----- Parameters
    alpha <- 1 / tlat
    gamma <- 1 / tinf
    min_date <- as.Date(min(df$Date), format = "%m/%d/%Y")
    max_date <- as.Date(max(df$Date), format = "%m/%d/%Y")
    days <- as.integer(max_date - min_date)

    values$alpha <- alpha
    values$gamma <- gamma

    # ----- Setup the case multiplier array
    cm_array <- rep(cm_start, 90)
    step <- abs(cm_start - cm_end) / 179
    if (cm_end < cm_start) {
      temp_array <- rev(seq(cm_end, cm_start, step))
    } else {
      temp_array <- seq(cm_start, cm_end, step)
    }
    cm_array <- c(cm_array, temp_array)
    if (max(df$Day) - length(cm_array) > 0) {
      cm_array <- c(cm_array, rep(cm_end, max(df$Day) - length(cm_array)))
    } else {
      cm_array <- cm_array[1:max(df$Day)]
    }
    cm_array <- cm_array[min(df$Day):max(df$Day)]

    values[["cm_array"]] <- cm_array

    # ----- Estimate the Infections
    df$infections <- df$cases * cm_array

    # ----- Smooth out the infections (too many days of counties reported 0)
    m1 <- smooth.spline(df$infections, cv = TRUE)

    df$smoothed_infections <- m1$y

    # ----- Calculate the growth rate, re, and beta
    df <- df %>%
      mutate(
        smoothed_infections =
          ifelse(smoothed_infections <= 0, 0, smoothed_infections)
      ) %>%
      mutate(lag_infections = lag(smoothed_infections, 7, 1)) %>%
      mutate(growth_r = ((smoothed_infections / lag_infections)**(1 / 7)) - 1)
    df <- df %>%
      mutate(growth_r = ifelse(is.na(growth_r), 0, growth_r)) %>%
      mutate(cum_infections = cumsum(smoothed_infections)) %>%
      mutate(re = (1 + growth_r * tlat) * (1 + growth_r * tinf)) %>%
      mutate(beta = (re / tinf) / (1 - cum_infections / population))

    # --- Bound beta between 0 and 1
    df[which(df$beta > 1), "beta"] <- 1
    df[which(df$beta < 0), "beta"] <- 0

    # ----- SEIR MODELING ------------------------------------------------------
    min_error <- 10**10
    cms <- c(seq(.1, 10, .1), seq(10, 200, 2.5))

    for (cm in cms) {
      seir_values <- seir(days, population, cm, alpha, gamma, df$beta)
      p <- seir_values[["predicted"]]
      error <- mean((df$smoothed_infections - p)**2)
      if (error < min_error) {
        min_error <- error
        preds <- p
        selected_cm <- cm
      }
    }
    df$pred_infections <- preds
    df$cum_pred_infections <- cumsum(preds)
    df$pred_cases <- df$pred_infections / cm_array
    df$cum_pred_cases <- cumsum(df$pred_cases)
    values[["data"]] <- df
    values[["starting_cases"]] <- selected_cm

    return(values)
  }


seir <- function(days, population, cm, alpha, gamma, beta_array) {
  # ----- Set Initial Compartments
  s <- rep(1, days + 1)
  e <- rep(0, days + 1)
  i <- rep(0, days + 1)
  p <- rep(0, 1)

  # --- Set original "infections" as the case multiplier
  e[1] <- cm / population
  i[1] <- cm / population

  # ----- Run SEIR
  for (j in 1:days) {
    s[j + 1] <- s[j] - beta_array[j] * s[j] * i[j]
    e[j + 1] <- e[j] + beta_array[j] * s[j] * i[j] - alpha * e[j]
    i[j + 1] <- i[j] + alpha * e[j] - gamma * i[j]
    p <- c(p, alpha * e[j] * population)
  }
  return_list <- list()
  return_list[["predicted"]] <- p
  return_list[["susceptible"]] <- s
  return_list[["infectious"]] <- i
  return_list[["infected"]] <- i * population
  return(return_list)
}


# surge <-
#   function(county = "Wake",
#            start_date = "2020-01-01",
#            beta = .2,
#            cm_start = 10,
#            cm_end = 4,
#            tlat = 5,
#            tinf = 6) {
#     values <- list()
#     # ----- Grab the County: Drop days before first case
#     df <- county_data %>%
#       filter(County == county)
#     start_date <- as.Date(start_date)
#     dates <- seq(start_date, (start_date + 365), by = "days")
#     df <- data.frame(dates)
#     df["County"] <- county
#     df["beta"] <- beta
#   }