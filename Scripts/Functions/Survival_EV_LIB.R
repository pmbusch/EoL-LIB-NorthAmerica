## Survival Curve based on Logistic Distribution
# Calculates dynamics for each region based on survival curves
# Results in detailed outflows of EVs and LIBs additional requirements,
# as well as LIB outflows to EVs, SSPS and recycling
## PBH January 2024

source("Scripts/01-ModelParameters.R")


# Function to get flows (numbers of cars,EV,LIB) depending on the
# vehicle and battery starting age
# Discretized by year using a Logistic Distribution
# n vehicles: vehicles currently on stock,
f.getOutflows <- function(
  n_veh = 1,
  EV_age,
  LIB_age,
  mean_ev = p.mean_ev,
  sd_ev = p.sd_ev,
  mean_lib = p.mean_lib,
  sd_lib = p.sd_lib,
  maxEV_age = 30,
  maxLIB_age = 30,
  dist.Age = "Logistic"
) {
  # get fraction year to year of survival, based on CDF ratios
  # represent proportion that survives year to year

  if (dist.Age == "Normal") {
    y1 = (1 - pnorm(EV_age + 1, mean = mean_ev, sd = sd_ev)) / (1 - pnorm(EV_age, mean = mean_ev, sd = sd_ev))
    y2 = (1 - pnorm(LIB_age + 1, mean = mean_lib, sd = sd_lib)) / (1 - pnorm(LIB_age, mean = mean_lib, sd = sd_lib))
  } else {
    # Logistic
    y1 = (1 - plogis(EV_age + 1, mean_ev, sd_ev * sqrt(3) / pi)) / # CONVERT SCALE TO Stand Dev.
      (1 - plogis(EV_age, mean_ev, sd_ev * sqrt(3) / pi))
    y2 = (1 - plogis(LIB_age + 1, mean_lib, sd_lib * sqrt(3) / pi)) /
      (1 - plogis(LIB_age, mean_lib, sd_lib * sqrt(3) / pi))
  }

  # max age
  if (EV_age >= maxEV_age) {
    y1 = 0
  }
  if (LIB_age >= maxLIB_age) {
    y2 = 0
  }

  # independent events to get proportions into 4 cases
  ret <- tibble(
    both_fail = (1 - y1) * (1 - y2) * n_veh,
    ev_fail = (1 - y1) * y2 * n_veh,
    lib_fail = y1 * (1 - y2) * n_veh,
    none = y1 * y2 * n_veh
  )

  return(ret)
}

# Test - vehicle of age 12, with original battery
# f.getOutflows(100,EV_age = 12,LIB_age = 12)
