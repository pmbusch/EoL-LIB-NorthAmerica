# Cathode weight based on 2024 chemistry
# PBH July 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")


chem <- read.csv("Inputs/Battery/chem2024.csv")


# weight cathode fraction
# Batpac 5.2
batpac <- tibble(
  chemistry = c("NMC 811", "NMC 622", "NMC 111", "NMC 90", "LFP", "NCA", "LMO/LTO"),
  cathode_we_frac = c(0.26, 0.28, 0.30, 0.26, 0.30, 0.27, 0.32), # %
  energy_dens = 100 / c(499, 523, 551, 488, 666, 511, 1073)
) # kwh per kg (all batteries are 100kWh)
batpac


# Combine with chemistry to get average
unique(chem$chemistry)

# special situations - HOMOLOGY
chem <- chem %>%
  mutate(
    chemistry = case_when(
      chemistry %in% c("NMC 532", "NMC 721") ~ "NMC 622",
      chemistry %in% c("NMC", "NMC / LTO", "NK") ~ "NMC 111",
      T ~ chemistry
    )
  )


# Figure for SI
head(chem)

chem |>
  group_by(Country, Vehicle, chemistry) |>
  reframe(share = sum(share)) |>
  mutate(
    Vehicle = Vehicle |>
      str_replace("Cars", "Light Duty Vehicles") |>
      str_replace("Vans", "Light Commercial Vehicles") |>
      factor(levels = c("Light Duty Vehicles", "Light Commercial Vehicles", "Buses", "Medium trucks", "Heavy trucks"))
  ) |>
  ggplot(aes(Country, share, fill = chemistry)) +
  geom_col(col="black",linewidth=0.5) +
  ggforce::facet_col(facets = vars(Vehicle), scales = "free", space = "free") +
  coord_flip(expand = F) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "", y = "2024 Cathode Battery Chemistry Share", fill = "Battery Chemistry") +
  theme(legend.position = "bottom", axis.text.x = element_text(hjust = 1))

ggsave("Figures/Inputs/batchem.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7 * 2)

params <- chem %>%
  left_join(batpac) %>%
  group_by(Country, Vehicle) %>%
  reframe(
    energy_dens = sum(energy_dens * share),
    cathode_we_frac = sum(cathode_we_frac * share),
    share = sum(share)
  ) %>%
  ungroup()
params$share <- NULL

# Add for CE LMO
params <- rbind(
  params,
  tibble(
    Vehicle = "Consumer Electronics",
    Country = c("Mexico", "Canada", "United States"),
    energy_dens = filter(batpac, chemistry == "LMO/LTO")$energy_dens,
    cathode_we_frac = filter(batpac, chemistry == "LMO/LTO")$cathode_we_frac
  )
)


# Add for stationary storage: LFP
params <- rbind(
  params,
  tibble(
    Vehicle = "Stationary Storage",
    Country = c("Mexico", "Canada", "United States"),
    energy_dens = filter(batpac, chemistry == "LFP")$energy_dens,
    cathode_we_frac = filter(batpac, chemistry == "LFP")$cathode_we_frac
  )
)


# Save
write.csv(params, "Inputs/battery_weight.csv", row.names = F)

# Scenarios for NMC 811 and LFP
params_nmc <- params |>
  mutate(
    energy_dens = if_else(
      Vehicle %in% c("Cars", "Vans", "Buses", "Medium trucks", "Heavy trucks"),
      filter(batpac, chemistry == "NMC 811")$energy_dens,
      energy_dens
    ),
    cathode_we_frac = if_else(
      Vehicle %in% c("Cars", "Vans", "Buses", "Medium trucks", "Heavy trucks"),
      filter(batpac, chemistry == "NMC 811")$cathode_we_frac,
      cathode_we_frac
    )
  )
write.csv(params_nmc, "Inputs/battery_weight_nmc.csv", row.names = F)

params_lfp <- params |>
  mutate(
    energy_dens = if_else(
      Vehicle %in% c("Cars", "Vans", "Buses", "Medium trucks", "Heavy trucks"),
      filter(batpac, chemistry == "LFP")$energy_dens,
      energy_dens
    ),
    cathode_we_frac = if_else(
      Vehicle %in% c("Cars", "Vans", "Buses", "Medium trucks", "Heavy trucks"),
      filter(batpac, chemistry == "LFP")$cathode_we_frac,
      cathode_we_frac
    )
  )
write.csv(params_lfp, "Inputs/battery_weight_lfp.csv", row.names = F)

# EoF
