# Figure of Quantiles of battery size, by country

library(tidyverse)

bat <- rbind(read.csv("Inputs/Battery/bat_quantiles.csv"), read.csv("Inputs/Battery/bat_quantiles_MDV.csv"))
head(bat)

bat <- bat %>%
  mutate(Vehicle = if_else(Vehicle == "Vans", "LCV", Vehicle)) %>%
  mutate(
    Sales_Country = Sales_Country %>% str_replace("USA", "United States"),
    Country = factor(Sales_Country, levels = rev(c("United States", "Mexico", "Canada"))),
    Vehicle = factor(Vehicle, levels = c("Heavy trucks", "Medium trucks", "Buses", "LCV", "Cars"))
  )


ggplot(bat, aes(mean_kwh_veh, Vehicle, col = Country)) +
  geom_point(position=position_dodge(width=0.4), size=1) +
  geom_linerange(
    aes(xmin = low_kwh_veh, xmax = high_kwh_veh),
    position = position_dodge(width = 0.4),
    linewidth = 0.3
  ) +
  # coord_flip()+
  scale_color_manual(values = c("United States" = "#3C3B6EFF", "Mexico" = "#006847FF", "Canada" = "#B22222FF")) +
  guides(col = guide_legend(reverse = TRUE)) +
  labs(x = "", y = "Battery size per vehicle [kWh]", col = "") +
  theme(legend.position = c(0.8, 0.8))


ggsave(
  "Figures/Inputs/BatSizeQuantiles.png",
  ggplot2::last_plot(),
  units = "cm",
  dpi = 600,
  width = 8.7 * 1.5,
  height = 8.7
)

# EoF
