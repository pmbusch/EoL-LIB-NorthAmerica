# Recycling Capacity Comparison
# PBH July 2025

# LOAD DATA -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-ModelParameters.R")

url_drive <- "Inputs/Original/"


## Capacity -----
# in metric tons
cap <- read_excel(
  paste0(url_drive, "NA Recycling facilities 12.9.25.xlsx"),
  sheet = "US and CA cleaned data",
  range = "X11:AB23"
)
# expand to 2050
cap_2030 <- cap %>% filter(Year == 2030)
for (i in 2031:2050) {
  aux <- cap_2030 %>% mutate(Year = i)
  cap <- rbind(cap, aux)
}
rm(aux, cap_2030)

cap <- cap %>%
  pivot_longer(c(-Year, -Country), names_to = "Stage", values_to = "tons") %>%
  mutate(Stage = str_remove(Stage, " Capacity") %>% str_replace("Refining", "Refining (black mass)")) %>%
  mutate(type = "Capacity") %>%
  mutate(Country = str_replace(Country, "US", "United States"))

cap <- cap %>% mutate(ktons = tons / 1e3)
cap_total <- cap %>% group_by(Stage, Year, type) %>% reframe(ktons = sum(ktons)) %>% ungroup()

## Recycling outflows ----
(runs <- list.files("Results/Feedstock/", recursive = F))

# Read all results and put them in the same dataframe!
df_all <- do.call(
  rbind,
  lapply(runs, function(i) {
    transform(read.csv(file.path("Results/Feedstock", i)), Scenario = i)
  })
)
unique(df_all$Scenario)

# Extract scens
df_all <- df_all %>%
  mutate(
    Sales = str_extract(Scenario, "Baseline|Momentum|Ambitious"),
    Size = str_extract(Scenario, "Small|Large"),
    Lifetime = str_extract(Scenario, "Short|Long"),
    eol = str_extract(Scenario, "Repurposing|Recycling"),
    Reuse = str_extract(Scenario, "reuse..") %>% str_replace("reuse0.", "reuse0")
  )
df_all <- df_all %>%
  mutate(
    Scenario = str_remove(Scenario, "\\.csv"),
    Size = if_else(is.na(Size), "Reference", Size),
    Lifetime = if_else(is.na(Lifetime), "Reference", Lifetime),
    eol = if_else(is.na(eol), "Reference", eol)
  )
unique(df_all$Sales)
unique(df_all$Size)
unique(df_all$Lifetime)
unique(df_all$eol)
unique(df_all$Reuse)

# consider scrap and max production
df_all <- df_all %>%
  mutate(ratio_cap = case_when(Flow != "LIB_scrap" ~ 1, ratio_cap > 1 ~ 1, T ~ ratio_cap)) %>%
  mutate(kwh = kwh * ratio_cap, battery_kg = battery_kg * ratio_cap, blackMass_kg = blackMass_kg * ratio_cap)

# add scenario 18% scrap
df_scrap <- df_all %>%
  filter(Scenario == "Momentum__reuse0") %>%
  # scale it according to original scrap rate used
  mutate(adj = if_else(Flow == "LIB_scrap", 0.18 / p.scrap, 1)) %>%
  mutate(kwh = kwh * adj, battery_kg = battery_kg * adj, blackMass_kg = blackMass_kg * adj, adj = NULL) %>%
  mutate(Scenario = "Scrap18")

df_all <- rbind(df_all, df_scrap)

df_all <- df_all %>%
  group_by(Scenario, Country, Sales, Size, Lifetime, eol, Reuse, Year) %>%
  # to ktons
  reframe(`Pre-processing` = sum(battery_kg) / 1e6, `Refining (black mass)` = sum(blackMass_kg) / 1e6) %>%
  ungroup() %>%
  pivot_longer(c(`Pre-processing`, `Refining (black mass)`), names_to = "Stage", values_to = "ktons") %>%
  mutate(type = "Feedstock")


# Pick key scenarios - with their name
scens_selected <- c(
  "Reference" = "Momentum__reuse0",
  "Ambitious" = "Ambitious__reuse0", # Sales
  "Large LIB" = "Momentum__reuse0Large",
  "Small LIB" = "Momentum__reuse0Small", # LIB Size
  "Long life" = "Momentum_Long_reuse0",
  "Short life" = "Momentum_Short_reuse0", # Lifetime
  "Recycling" = "Momentum__reuse0Recycling",
  "Repurposing" = "Momentum__reuse0Repurposing", # Eol
  "50% reuse" = "Momentum__reuse50", # reuse
  "18% Scrap" = "Scrap18"
)

# names of stages
df_all <- df_all %>%
  mutate(
    Stage = if_else(
      str_detect(Stage, "processing"),
      "Pre-processing (ktons of battery)",
      "Refining (ktons of black mass)"
    )
  )

cap <- cap %>%
  mutate(
    Stage = if_else(
      str_detect(Stage, "processing"),
      "Pre-processing (ktons of battery)",
      "Refining (ktons of black mass)"
    )
  )

cap_total <- cap_total %>%
  mutate(
    Stage = if_else(
      str_detect(Stage, "processing"),
      "Pre-processing (ktons of battery)",
      "Refining (ktons of black mass)"
    )
  )


# FIG 3 -----
year_limit <- 2050

## a) Cap vs Feedstock -----

df_scen <- df_all %>%
  filter(Year <= year_limit) %>%
  filter(Scenario %in% scens_selected) %>%
  group_by(Stage, Scenario, Country, Sales, Size, Lifetime, eol, Reuse, Year) %>%
  reframe(ktons = sum(ktons)) %>%
  ungroup() %>%
  left_join(tibble(Scenario = scens_selected, scen_name = names(scens_selected)))

cap <- cap %>% dplyr::select(Year, Country, Stage, type, ktons)


data_fig <- df_scen %>%
  filter(Country != "Exports") %>%
  filter(scen_name == "Reference") %>%
  mutate(type = "Feedstock") %>%
  dplyr::select(Year, Country, Stage, type, ktons) %>%
  rbind(cap) %>%
  group_by(Stage, Year, type) %>%
  reframe(ktons = sum(ktons)) %>%
  ungroup()

max_v <- max(data_fig$ktons)

# data for feedstcok
data_fig %>%
  filter(Year %in% c(2025:2030, 2035, 2040, 2045, 2050)) %>%
  filter(type == "Feedstock") %>%
  mutate(ktons = ktons * 1e3) %>%
  pivot_wider(names_from = Stage, values_from = ktons)


pa <- ggplot(data_fig, aes(Year, ktons)) +
  geom_col(aes(fill=type),position = "dodge") +
  facet_wrap(~Stage) +
  coord_cartesian(expand = F, ylim = c(0, max_v * 1.05), xlim = c(2024.5, year_limit + .5)) +
  scale_x_continuous(breaks = c(2025, 2030, 2040, 2050)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Capacity" = "#B22222", "Feedstock" = "#006400")) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = "", y = "", tag = "(a)", fill = "", title = "North America Battery Feedstock and Processing Capacity") +
  theme(
    plot.tag = element_text(face = "bold"),
    legend.position = c(0.7, 0.45),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    panel.spacing = unit(0.5, "cm")
  )
pa

# zoom versions
pa_zoom <- ggplot(filter(data_fig, Year <= 2030, str_detect(Stage, "Pre-processing")), aes(Year, ktons)) +
  geom_col(aes(fill=type),position = "dodge") +
  coord_cartesian(expand = F, ylim = c(0, 1100)) +
  scale_x_continuous(breaks = c(2025, 2030), minor_breaks = 2025:2030) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 500, 1000)) +
  scale_fill_manual(values = c("Capacity" = "#B22222", "Feedstock" = "#006400")) +
  labs(x = "", y = "", fill = "") +
  guides(x = guide_axis(minor.ticks = TRUE)) +
  theme(legend.position = "none", plot.background = element_rect(fill = "transparent", color = NA))

pa_zoom2 <- pa_zoom %+% filter(data_fig, Year <= 2030, str_detect(Stage, "Refining"))


## b) net by scenarios -----

data_fig2 <- df_scen %>%
  filter(Country != "Exports") %>%
  group_by(Scenario, scen_name, Year, Stage) %>%
  reframe(ktons = sum(ktons)) %>%
  ungroup() %>%
  left_join(mutate(cap_total, type = NULL, capacity = ktons, ktons = NULL)) %>%
  mutate(net = capacity - ktons)


annotations <- tibble(
  Stage = "Pre-processing (ktons of battery)",
  x = c(2044, 2044),
  y = c(800, -800),
  label = c("Overcapacity", "Undercapacity")
)


color_scale <- c(
  "Momentum__reuse0" = "black",
  "Ambitious__reuse0" = "#E31A1C",
  "Momentum__reuse0Small" = "#A6CEE3",
  "Momentum__reuse0Large" = "#1F78B4",
  "Momentum_Long_reuse0" = "#B2DF8A",
  "Momentum_Short_reuse0" = "#33A02C",
  "Momentum__reuse0Recycling" = "#FDBF6F",
  "Momentum__reuse0Repurposing" = "#FF7F00",
  "Scrap18" = "#6A3D9A"
)

pb <- ggplot(data_fig2, aes(Year, net, col = Scenario)) +
  geom_line() +
  geom_text_repel(
    data = . %>% group_by(Scenario) %>% slice_max(Year) %>% filter(str_detect(Stage, "Pre-processing")),
    aes(label = scen_name, col = Scenario),
    direction = "y",
    segment.size = 0.1,
    nudge_x = 5,
    hjust = -0.1,
    size = 8 * 5 / 14 * 0.8
  ) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
  geom_text(data=annotations,aes(x=x,y=y,label=label),fontface="italic",
            inherit.aes = F,col="black",size=9*5/14 * 0.8) +
  facet_wrap(~Stage, scales = "free_x") +
  # coord_cartesian(expand = F,xlim = c(2025,year_limit+4),
  #                 ylim=range(data_fig2$net)*1.05)+
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(breaks = c(2025, 2030, 2040, 2050)) +
  scale_color_manual(values = color_scale) +
  labs(x = "", y = "", col = "", title = "Net Processing Capacity", tag = "(b)") +
  theme(panel.spacing = unit(0.5, "cm"), plot.tag = element_text(face = "bold"), legend.position = "none")
pb


pb_zoom <- ggplot(
  filter(data_fig2, Year <= 2030, str_detect(Stage, "Pre-processing")),
  aes(Year, net, col = Scenario)
) +
  geom_line() +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
  coord_cartesian(expand = F, xlim = c(2025, 2030), ylim = c(-700, 500)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(breaks = c(2025, 2030), minor_breaks = 2025:2030) +
  scale_color_manual(values = color_scale) +
  labs(x = "", y = "", col = "") +
  guides(x = guide_axis(minor.ticks = TRUE)) +
  theme(legend.position = "none", plot.background = element_rect(fill = "transparent", color = NA))


pb_zoom2 <- pb_zoom %+% filter(data_fig2, Year <= 2030, str_detect(Stage, "Refining"))


## c) By country -----

data_fig3 <- df_scen %>%
  filter(Country != "Exports") %>%
  filter(scen_name == "Reference") %>%
  group_by(Country, Year, Stage) %>%
  reframe(ktons = sum(ktons)) %>%
  ungroup() %>%
  left_join(mutate(cap, type = NULL, capacity = ktons, ktons = NULL)) %>%
  mutate(capacity = if_else(is.na(capacity), 0, capacity)) %>%
  mutate(net = capacity - ktons)

annotations <- tibble(Country = c("United States", "Mexico"))


pc <- ggplot(data_fig3, aes(Year, net, col = Country, group = Country)) +
  geom_line() +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
  # facet_grid(Country~Stage,scales="free_y")+
  facet_wrap(~Stage) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(2025, 2030, 2040, 2050)) +
  geom_text(x=2045,size=8*5/14 * 0.8,y=-6000,label="United States",angle=-35,col="#3C3B6EFF",data= . %>% filter(Stage=="Pre-processing")) +
  geom_text(x=2048,size=8*5/14 * 0.8,y=-450,label="Mexico",col="#006847FF",data= . %>% filter(Stage=="Pre-processing")) +
  geom_text(x=2048,size=8*5/14 * 0.8,y=-2000,label="Canada",col="#B22222FF",data= . %>% filter(Stage=="Pre-processing")) +
  labs(x = "", y = "", tag = "(c)", title = "Net Processing Capacity by Country") +
  theme(plot.tag = element_text(face = "bold"), panel.spacing = unit(0.5, "cm"), legend.position = "none")
pc

pc_zoom <- ggplot(
  filter(data_fig3, Year <= 2030, str_detect(Stage, "Pre-processing")),
  aes(Year, net, col = Country, group = Country)
) +
  geom_line() +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
  coord_cartesian(expand = F, xlim = c(2025, 2030), ylim = c(-100, 400)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(2025, 2030), minor_breaks = 2025:2030) +
  labs(x = "", y = "", col = "") +
  guides(x = guide_axis(minor.ticks = TRUE)) +
  theme(legend.position = "none", plot.background = element_rect(fill = "transparent", color = NA))

pc_zoom2 <- pc_zoom %+% filter(data_fig3, Year <= 2030, str_detect(Stage, "Refining"))


## Combine -----

pa1 <- pa + theme(plot.margin = margin(0, 5, -5, 5)) #trbl
pb1 <- pb + theme(plot.margin = margin(-5, 5, -5, 5))
pc1 <- pc + theme(plot.margin = margin(-5, 5, -5, 5))

library(cowplot)
pa1_inset <- ggdraw() +
  draw_plot(pa1) +
  draw_plot(pa_zoom, x = 0.11, y = 0.33, width = 0.25, height = 0.38) +
  draw_plot(pa_zoom2, x = 0.56, y = 0.33, width = 0.25, height = 0.38)


pb1_inset <- ggdraw() +
  draw_plot(pb1) +
  draw_plot(pb_zoom, x = 0.11, y = 0.08, width = 0.25, height = 0.35) +
  draw_plot(pb_zoom2, x = 0.56, y = 0.08, width = 0.25, height = 0.35)


pc1_inset <- ggdraw() +
  draw_plot(pc1) +
  draw_plot(pc_zoom, x = 0.11, y = 0.08, width = 0.25, height = 0.45) +
  draw_plot(pc_zoom2, x = 0.56, y = 0.08, width = 0.25, height = 0.45)


plot_grid(pa1_inset, pb1_inset, pc1_inset, ncol = 1, rel_heights = c(1, 1.3, 1), align = "v", axis = "lr")

ggsave("Figures/Fig4.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7 * 2)

# OLD FIGURE ----

df_scen <- df_all %>%
  # filter(Stage=="Pre-processing") %>%
  filter(Scenario %in% scens_selected) %>%
  group_by(Stage, Scenario, Sales, Size, Lifetime, eol, Reuse, Year) %>%
  reframe(ktons = sum(ktons)) %>%
  ungroup() %>%
  left_join(tibble(Scenario = scens_selected, scen_name = names(scens_selected)))

# Figure
# year_limit <- 2035
# year_limit <- 2050
year_limit <- 2040

data_fig <- df_scen %>% filter(Year <= year_limit)
max_v <- max(data_fig$ktons)

annotations <- tibble(
  Stage = c("Pre-processing", "Refining (black mass)"),
  x = c(2038, 2030),
  y = c(1000, 1600),
  label = c("North America Recycling Capacity", "North America Recycling Capacity")
)


ggplot(data_fig, aes(Year, ktons)) +
  geom_col(data=filter(cap_total,Year<=year_limit),fill="darkred",alpha=0.8) +
  geom_line(aes(group = Scenario, col = Scenario), linewidth = 0.5) +
  geom_text_repel(
    data = . %>% group_by(Scenario) %>% slice_max(Year),
    aes(label = scen_name, col = Scenario),
    direction = "y",
    segment.size = 0.1,
    hjust = -0.1,
    size = 8 * 5 / 14 * 0.8
  ) +
  geom_text(data=annotations,aes(x=x,y=y,label=label),
            inherit.aes = F,col="darkred",size=10*5/14 * 0.8) +
  facet_wrap(~Stage) +
  coord_cartesian(expand = F, xlim = c(2025, year_limit + 2), ylim = c(0, max_v * 1.05)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(breaks = seq(2025, 2050, 5)) +
  scale_color_manual(
    values = c(
      "Momentum__reuse0" = "black",
      "Ambitious__reuse0" = "#E31A1C",
      "Momentum__reuse0Small" = "#A6CEE3",
      "Momentum__reuse0Large" = "#1F78B4",
      "Momentum_Long_reuse0" = "#B2DF8A",
      "Momentum_Short_reuse0" = "#33A02C",
      "Momentum__reuse0Recycling" = "#FDBF6F",
      "Momentum__reuse0Repurposing" = "#FF7F00",
      "Momentum__reuse30" = "#CAB2D6",
      "Momentum__reuse60" = "#6A3D9A"
    )
  ) +
  # scale_fill_manual(values = c("Capacity" = "#1F77B4", "Feedstock" = "#D62728"))+
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = "", y = "", title = "Batttery outflows and recycling capacity (thousand metric tons)", fill = "") +
  theme(legend.position = "none", panel.spacing.x = unit(1, "cm"))

ggsave("Figures/CapacityComparison.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# By country -----
year_limit = 2035
data_fig_b <- df_all %>%
  filter(Stage == "Pre-processing") %>%
  filter(Scenario %in% scens_selected) %>%
  filter(Year == year_limit) %>%
  left_join(tibble(Scenario = scens_selected, scen_name = names(scens_selected)))


# categories
data_fig_b <- data_fig_b %>%
  mutate(
    x_lab = case_when(
      Scenario %in% c("Baseline__reuse0", "Momentum__reuse0") ~ "Reference",
      Size != "Reference" ~ "LIB Size",
      Lifetime != "Reference" ~ "LIB Lifetime",
      (eol != "Reference" | str_detect(Scenario, "reuse50")) ~ "End-of-Life"
    ) %>%
      factor(levels = c("Reference", "LIB Size", "LIB Lifetime", "End-of-Life"))
  )

ggplot(data_fig_b, aes(x_lab, ktons)) +
  geom_col(data=filter(data_fig_b,Scenario=="Momentum__reuse0"),
           alpha=0.8) +
  geom_point(data=filter(data_fig_b,Scenario!="Momentum__reuse0"),
             aes(col=x_lab),size=0.5) +
  stat_summary(
    data = filter(data_fig_b, Scenario != "Momentum__reuse0"),
    aes(col = x_lab),
    linewidth = 0.3,
    geom = "linerange",
    fun.min = min,
    fun.max = max
  ) +
  geom_text_repel(
    data = filter(data_fig_b, Scenario != "Momentum__reuse0", Country == "United States"),
    aes(label = scen_name, col = x_lab),
    direction = "y",
    nudge_x = 0.2,
    segment.size = 0.1,
    size = 6 * 5 / 14 * 0.8
  ) +
  facet_wrap(~Country, ncol = 1, scales = "free_y") +
  scale_color_manual(
    values = c("Reference" = "#E31A1C", "LIB Size" = "#1F78B4", "LIB Lifetime" = "#33A02C", "End-of-Life" = "#FF7F00")
  ) +
  labs(x = "", y = "", title = paste0(year_limit, " battery recycling feedstock (thousand metric tons)")) +
  theme(legend.position = "none")

ggsave("Figures/Feedstock.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 1.2, height = 8.7)

# EoL
