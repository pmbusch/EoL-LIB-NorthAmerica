# Load Consumer Electronics input
# Source: RMI Dashboard - Baseline scenario. https://rmi.org/battery-circular-economy-initiative/resources/dashboards/?dashboard=1
# Report only states that consumer electronics comes from RMI estimates, no mention of chemisty, only that has cobalt
# PBH July  2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")


url_drive <- "Inputs/Original/"
# Global demand in GWh
ce <- read_excel(paste0(url_drive, "Consumer electronics forecast.xlsx"), sheet = "Sheet1", range = "C2:AC3")
ce <- ce %>%
  mutate(x = 1) %>%
  pivot_longer(c(-x), names_to = "Year", values_to = "ce_gwh") %>%
  mutate(x = NULL, Year = as.numeric(Year))


# Distribute it to country by GDP 2024
# Source World Development Indicators - https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.CD#
# Note: only countries, no aggregated regions
gdp <- read_excel(
  "Inputs/Original/P_Data_Extract_From_World_Development_Indicators.xlsx",
  sheet = "Data",
  range = "A1:P218"
)
names(gdp)
gdp <- gdp %>%
  rename(Country = `Country Name`, gdp = `2024 [YR2024]`) %>%
  dplyr::select(Country, gdp) %>%
  mutate(gdp = as.numeric(gdp))

gdp <- gdp %>% mutate(gdp_share = gdp / sum(gdp, na.rm = T))

# remove non-countries

gdp <- gdp %>% filter(Country %in% c("United States", "Canada", "Mexico")) %>% mutate(gdp = NULL)
gdp

# dissagregate by gdp
ce <- ce %>% cross_join(gdp) %>% mutate(ce_gwh = ce_gwh * gdp_share, gdp_share = NULL)

# Failure distribution
# Weibull for US Epa mean 4 (Laptops)

# max 9 years
survival_curve <- dweibull(1:9, shape = 4.54, scale = 4.38)
plot(1:9, survival_curve)
sum(survival_curve)

# get LIB outflows based on survival curve
recyc <- ce %>%
  cross_join(tibble(offset = 1:9, p = survival_curve)) %>%
  mutate(Year = Year + offset, ce_gwh_recyc = ce_gwh * p) %>%
  group_by(Country, Year) %>%
  reframe(ce_gwh_recyc = sum(ce_gwh_recyc)) %>%
  ungroup() %>%
  arrange(Country, Year) %>%
  filter(Year < 2051)

# Create stock
ce <- ce %>% left_join(recyc)

ce <- ce %>%
  mutate(ce_gwh_recyc = if_else(is.na(ce_gwh_recyc), 0, ce_gwh_recyc)) %>%
  group_by(Country) %>%
  mutate(ce_gwh_stock = ce_gwh - ce_gwh_recyc) %>%
  mutate(ce_gwh_stock = cumsum(ce_gwh_stock)) %>%
  ungroup() %>%
  filter(Year > 2024)

write.csv(ce, "Inputs/consumerElectronics.csv", row.names = F)

# EoF
