# Key model parameters, can be overriden for scenarios
# PBH July 2025


# LIBs
p.share_repurpose <- 0.5 # Share of LIB available that goes to repurpose (instead of recycling)
p.ev_age_newLib <- 8 #
p.max_reuse_lib <- 0.5 # Share of LIBs that goes towards reuse (in another EV)
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
large_veh <- c("Buses","Medium trucks","Heavy trucks")


# Production scrap
p.scrap <- 0.06 # BatPac


# EoF