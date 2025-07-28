# Load ICCT Vehicle Sales data - Filter to North America
# Output: Car Sales by country, year
# PBH July  2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Data from ICCT - Roadmap v2.6
icct <- readxl::read_excel("Inputs/Original/2025_sales_data_updated_roadmapv2.6.xlsx")
(names(icct) <- names(icct) %>% str_replace_all(" ","_") %>%  # correct names
  str_remove_all("\\(|\\)") %>% 
  str_replace("CY","Year"))

# Filter to North America
icct <- icct %>% filter(Country %in% c("Mexico","Canada","United States"))


# Process data ------
unique(icct$Powertrain)
unique(icct$Scenario)
unique(icct$Vehicle)
range(icct$Year)

# Filter BEV, Ambitious scenario for now
icct <- icct %>% 
  filter(Scenario %in% c("Baseline 2024","Momentum")) %>% 
  filter(Powertrain %in% c("BEV")) %>% 
  filter(Year>2024) %>% 
  filter(Vehicle %in% c("Cars","Vans","Medium trucks","Heavy trucks","Buses")) %>% 
  mutate(Scenario=str_remove(Scenario," 2024"))

unique(icct$Vehicle)
unique(icct$Country)
unique(icct$Scenario)

## Add historical sales - From EV Volumes, prior to 2024
historical_sales <- read.csv("Inputs/Battery/historical_EVsales.csv")
range(historical_sales$Year)
historical_sales <- historical_sales %>% 
  rename(Sales=unit,Country=Sales_Country) %>% 
  mutate(Country=if_else(Country=="USA","United States",Country)) %>% 
  dplyr::select(Vehicle,Year,Country,Sales)
# Heavy duty
historical_HDV <- read.csv("Inputs/Battery/historical_EV_HDV_sales.csv")
range(historical_HDV$Year)
historical_HDV <- historical_HDV %>% 
  rename(Sales=sales,Country=Sales_Country) %>% 
  mutate(Country=if_else(Country=="USA","United States",Country)) %>% 
  dplyr::select(Vehicle,Year,Country,Sales)

unique(icct$Scenario)
icct <- icct %>% 
  dplyr::select(Scenario,Vehicle,Year,Country,Sales) %>% 
  rbind(mutate(historical_sales,Scenario="Baseline")) %>%
  rbind(mutate(historical_sales,Scenario="Momentum")) %>%
  rbind(mutate(historical_HDV,Scenario="Baseline")) %>% 
  rbind(mutate(historical_HDV,Scenario="Momentum")) %>% 
  arrange(Year) %>% arrange(Country)

# complete all data
icct <- icct %>% complete(Scenario,Vehicle,Country, Year, fill = list(Sales = 0))
nrow(icct) # 1080, 3*36*5*2

write.csv(icct,"Inputs/EV_Sales.csv",row.names = F)

icct %>% 
  filter(Scenario=="Momentum") %>% 
  filter(Year %in% c(2025,2050)) %>% 
  group_by(Country,Vehicle,Year) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% # to millions
  pivot_wider(names_from = Country, values_from = Sales)


# EoF