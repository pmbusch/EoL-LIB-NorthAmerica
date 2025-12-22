# Key model parameters, can be overriden for scenarios
# PBH July 2025

# LIBs
p.share_repurpose <- 0.5 # Share of LIB available that goes to repurpose (instead of recycling)
p.ev_age_newLib <- 8 #
p.max_reuse_lib <- 0 # Share of LIBs that goes towards reuse (in another EV)
p.max_ev_age_replacement <- 12 # Max EV age to get a new LIB (replacement)
p.max_lib_age_secondlife <- 12 # Max LIB age to be use in another EV (reuse)
p.max_lib_age_repurp <- 12 # Max LIB age to be use in Stationary Storage (repurpose)
p.degradation <- 0.02 # LIB capacity degradation per year

# Lifetime - years
p.mean_ev <- 17
p.sd_ev <- 4
p.mean_lib <- 15
p.sd_lib <- 4

p.mean_lib_repurp <- 20 # LIB for repurposing

# large vehicles
p.mean_lib_large <- 8
large_veh <- c("Vans", "Buses", "Medium trucks", "Heavy trucks")


# Production scrap - downstream and midstream
p.scrap_down <- tibble(Year = 2025:2050, scrap_down = c(2.34, 2.17, 2.01, 1.84, 1.68, 1.51, rep(1.51, 20)) / 100)
p.scrap_mid <- tibble(Year = 2025:2050, scrap_mid = c(4.38, 4.07, 3.76, 3.45, 3.14, 2.83, rep(2.83, 20)) / 100)

# EoF
