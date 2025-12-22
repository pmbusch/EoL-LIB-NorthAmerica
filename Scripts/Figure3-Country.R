# Export effect
# PBH August 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/Run Model/01-ModelParameters.R")

# DATA -----

# Trade
df <- read.csv("Results/Feedstock/Momentum__reuse0.csv")
# No Trade, to get net imports
df2 <- read.csv("Results/Feedstock/Momentum__reuse0Notrade.csv")

df <- rbind(mutate(df, case = "Trade"), mutate(df2, case = "NoTrade"))
rm(df2)

# limit scrap
df <- df %>%
  mutate(ratio_cap = case_when(!str_detect(Flow, "LIB_scrap") ~ 1, ratio_cap > 1 ~ 1, T ~ ratio_cap)) |>
  mutate(kwh = kwh * ratio_cap, battery_kg = battery_kg * ratio_cap, blackMass_kg = blackMass_kg * ratio_cap)

# net imports
head(df)
df <- df %>% dplyr::select(-ratio_cap, -kwh)

dif_df <- df %>%
  pivot_wider(names_from = case, values_from = c(blackMass_kg, battery_kg)) %>%
  mutate(
    blackMass_kg = blackMass_kg_Trade - blackMass_kg_NoTrade,
    battery_kg = battery_kg_Trade - battery_kg_NoTrade
  ) %>%
  filter(abs(blackMass_kg) + abs(battery_kg) > 0) %>%
  dplyr::select(-blackMass_kg_Trade, -blackMass_kg_NoTrade, -battery_kg_Trade, -battery_kg_NoTrade) %>%
  mutate(Vehicle = "Used LDV Trade")

# add to No Trade scenario
df <- df %>% filter(case == "NoTrade") %>% dplyr::select(-case) %>% rbind(dif_df)

# all scrap is scrap
df <- df %>% mutate(Vehicle = if_else(str_detect(Flow, "scrap"), "Production Scrap", Vehicle))

## Capacity ----
url_drive <- "Inputs/Original/"
cap <- read_excel(
  paste0(url_drive, "NA Recycling facilities 12.9.25.xlsx"),
  sheet = "US and CA cleaned data",
  range = "X13:AB29"
)
# expand to 2050
cap_2032 <- cap %>% filter(Year == 2032)
for (i in 2033:2050) {
  aux <- cap_2032 %>% mutate(Year = i)
  cap <- rbind(cap, aux)
}
rm(aux, cap_2032)

head(cap)
cap <- cap %>%
  dplyr::select(-`Refining Capacity No Delay`) |>
  pivot_longer(c(-Year, -Country), names_to = "Stage", values_to = "tons") %>%
  mutate(
    Stage = if_else(
      str_detect(Stage, "processing"),
      "Pre-processing (ktons of battery)",
      "Refining (ktons of black mass)"
    )
  ) %>%
  mutate(Country = str_replace(Country, "US", "United States")) %>%
  mutate(Country = factor(Country, levels = c("United States", "Mexico", "Canada")))
cap <- cap %>% mutate(ktons = tons / 1e3)

# add mexico
mex_cap <- expand.grid(
  Year = 2025:2050,
  Stage = c("Pre-processing (ktons of battery)", "Refining (ktons of black mass)")
) %>%
  mutate(Country = "Mexico", tons = 0, ktons = 0)
cap <- rbind(cap, mex_cap)

# FIGURE -----

## Main ----

# SAME COLORS AS FIG 2
veh_levels <- MetBrewer::met.brewer("Signac", n = 12)
names(veh_levels) <- c(
  "Production Scrap",
  "Consumer Electronics",
  "Stationary Storage",
  "LIB Replacement - SS",
  "LIB Replacement - LDV",
  "LIB Replacement - HDV",
  "Heavy trucks",
  "Medium trucks",
  "Buses",
  "Light Commercial Vehicles",
  "Light Duty Vehicles",
  "Used LDV Trade"
)


unique(df$Flow)
data_fig <- df %>%
  # filter(Year %in% c(2025,2030,2035,2040,2045,2050)) %>%
  pivot_longer(c(battery_kg, blackMass_kg), names_to = "Stage", values_to = "ktons") %>%
  mutate(ktons = ktons / 1e6) %>%
  mutate(
    Vehicle = case_when(
      Vehicle %in% c("Cars") ~ "Light Duty Vehicles",
      Vehicle %in% c("Vans") ~ "Light Commercial Vehicles",
      T ~ Vehicle
    )
  ) %>%
  mutate(
    Stage = if_else(str_detect(Stage, "battery"), "Pre-processing (ktons of battery)", "Refining (ktons of black mass)")
  ) %>%
  group_by(Vehicle, Country, Year, Stage) %>%
  reframe(ktons = sum(ktons)) %>%
  ungroup() %>%
  mutate(Vehicle = factor(Vehicle, levels = names(veh_levels))) %>%
  mutate(Country = factor(Country, levels = c("United States", "Mexico", "Canada")))

# add total north america
data_fig_NA <- data_fig %>%
  group_by(Vehicle, Year, Stage) %>%
  reframe(ktons = sum(ktons)) %>%
  ungroup() %>%
  mutate(Country = "North America")
data_fig <- rbind(data_fig, data_fig_NA) %>%
  mutate(Country = factor(Country, levels = c("North America", "United States", "Mexico", "Canada")))

cap_NA <- cap %>%
  group_by(Year, Stage) %>%
  reframe(tons = sum(tons), ktons = sum(ktons)) %>%
  ungroup() %>%
  mutate(Country = "North America")
cap <- rbind(cap, cap_NA) %>%
  mutate(Country = factor(Country, levels = c("North America", "United States", "Mexico", "Canada")))

# retiring feedstock by country numbers
data_fig %>%
  filter(Year == 2050) %>%
  group_by(Country, Stage) %>%
  mutate(share = ktons / sum(ktons)) %>%
  filter(Vehicle == "Used LDV Trade")


p <- ggplot(data_fig, aes(Year, ktons)) +
  geom_col(aes(fill=Vehicle),col="black",linewidth=0.1) +
  geom_line(data = cap, linewidth = 0.5) +
  # geom_hline(yintercept = 0,col="black",linetype="dashed")+
  # facet_grid(Country ~ Stage, scales = "free") +
  ggh4x::facet_grid2(vars(Country), vars(Stage), scales = "free_y", independent = "y") +
  scale_x_continuous(breaks = c(2025, 2030, 2040, 2050), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(values = veh_levels) +
  labs(x = "", y = "", fill = "") +
  theme(panel.spacing = unit(0.5, "cm"), legend.text = element_text(size = 7))
p


write.csv(data_fig, "Results/Data Figures/Fig3.csv", row.names = FALSE)

## Zoom versions -----
p_zoom_base <- ggplot(
  filter(data_fig, Year <= 2030, Country == "United States", str_detect(Stage, "Pre-processing")),
  aes(Year, ktons)
) +
  geom_col(aes(fill=Vehicle),col="black",linewidth=0.01) +
  coord_cartesian(expand = F, ylim = c(-10, 1400)) +
  scale_x_continuous(breaks = c(2025, 2030), minor_breaks = 2025:2030) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 500, 1000)) +
  scale_fill_manual(values = veh_levels) +
  labs(x = "", y = "", fill = "") +
  guides(x = guide_axis(minor.ticks = TRUE)) +
  theme(legend.position = "none", plot.background = element_rect(fill = "transparent", color = NA))

# NA
p_zoom0a <- p_zoom_base +
  coord_cartesian(expand = F, ylim = c(0, 1050)) +
  geom_line(data = filter(cap, Year <= 2030, Country == "North America", str_detect(Stage, "Pre-processing")))
p_zoom0b <- p_zoom_base %+%
  coord_cartesian(expand = F, ylim = c(0, 450)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 200, 400)) +
  filter(data_fig, Year <= 2030, Country == "North America", str_detect(Stage, "Refining")) +
  geom_line(data = filter(cap, Year <= 2030, Country == "North America", str_detect(Stage, "Refining")))

# USA
p_zoom1a <- p_zoom_base +
  coord_cartesian(expand = F, ylim = c(0, 1050)) +
  geom_line(data = filter(cap, Year <= 2030, Country == "United States", str_detect(Stage, "Pre-processing")))
p_zoom1b <- p_zoom_base %+%
  filter(data_fig, Year <= 2030, Country == "United States", str_detect(Stage, "Refining")) +
  coord_cartesian(expand = F, ylim = c(0, 350)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 100, 200, 300)) +
  geom_line(data = filter(cap, Year <= 2030, Country == "United States", str_detect(Stage, "Refining")))

# Mex
p_zoom2a <- p_zoom_base %+%
  filter(data_fig, Year <= 2030, Country == "Mexico", str_detect(Stage, "processing")) +
  coord_cartesian(expand = F, ylim = c(0, 70)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 25, 50)) +
  geom_line(data = filter(cap, Year <= 2030, Country == "Mexico", str_detect(Stage, "Pre-processing")))

p_zoom2b <- p_zoom_base %+%
  filter(data_fig, Year <= 2030, Country == "Mexico", str_detect(Stage, "Refining")) +
  coord_cartesian(expand = F, ylim = c(0, 22)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 10, 20)) +
  geom_line(data = filter(cap, Year <= 2030, Country == "Mexico", str_detect(Stage, "Refining")))

# Can
p_zoom3a <- p_zoom_base %+%
  filter(data_fig, Year <= 2030, Country == "Canada", str_detect(Stage, "processing")) +
  coord_cartesian(expand = F, ylim = c(-2, 350)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 100, 200, 300)) +
  geom_line(data = filter(cap, Year <= 2030, Country == "Canada", str_detect(Stage, "Pre-processing")))

p_zoom3b <- p_zoom_base %+%
  filter(data_fig, Year <= 2030, Country == "Canada", str_detect(Stage, "Refining")) +
  coord_cartesian(expand = F, ylim = c(-2, 75)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 20, 40, 60)) +
  geom_line(data = filter(cap, Year <= 2030, Country == "Canada", str_detect(Stage, "Refining")))

## Combine ----

library(cowplot)
library(grid)
ggdraw() +
  draw_plot(p) +
  draw_plot(p_zoom0a, x = 0.0617, y = 0.815, width = 0.21, height = 0.14) +
  draw_plot(p_zoom0b, x = 0.412, y = 0.815, width = 0.21, height = 0.14) +
  draw_plot(p_zoom1a, x = 0.0617, y = 0.584, width = 0.21, height = 0.14) +
  draw_plot(p_zoom1b, x = 0.412, y = 0.584, width = 0.21, height = 0.14) +
  draw_plot(p_zoom2a, x = 0.0617, y = 0.35, width = 0.21, height = 0.14) +
  draw_plot(p_zoom2b, x = 0.412, y = 0.35, width = 0.21, height = 0.14) +
  draw_plot(p_zoom3a, x = 0.0617, y = 0.12, width = 0.21, height = 0.14) +
  draw_plot(p_zoom3b, x = 0.412, y = 0.12, width = 0.21, height = 0.14) +
  draw_label("Recycling Capacity", x = 0.515, y = 0.83, size = 8, color = "black") +
  draw_grob(segmentsGrob(
    x0 = unit(0.5, "npc"),
    y0 = unit(0.815, "npc"),
    x1 = unit(0.5, "npc"),
    y1 = unit(0.79, "npc"),
    gp = gpar(col = "black"),
    arrow = arrow(length = unit(0.2, "cm"))
  ))

ggsave("Figures/Fig3.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 8.7 * 1.8)

pdf("Figures/pdf/Fig3.pdf", width = 18 / 2.54, height = 8.7 * 1.8 / 2.54)
ggplot2::last_plot()
dev.off()

# EoF
