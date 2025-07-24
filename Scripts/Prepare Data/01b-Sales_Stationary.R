## Stationary storage based on forecast of solar and wind generation capacity
# Method originally from https://doi.org/10.1016/j.resconrec.2025.108377


source("Scripts/00-Libraries.R", encoding = "UTF-8")

# USA -----

# EIA Table 56 renewable forecast for US Grid
# Source: EIA Annual Energy Outlook
# https://www.eia.gov/outlooks/aeo/tables_ref.php
df_ren <- read.csv("Inputs/Original/EIA_Table56.csv")
nrow(df_ren) 


#Dimensions
range(df_ren$period) # 2023 to 2050
unique(df_ren$regionName) # 25
unique(df_ren$history);unique(df_ren$tableName)
unique(df_ren$seriesId)
unique(df_ren$scenario)

## Filter data ---------
# filter by generation data
df_ren <- df_ren %>% 
  filter(substr(seriesId,0,4)=="cap_") %>% 
  filter(str_detect(seriesName,"Electric Power"))
unique(df_ren$seriesName)
unique(df_ren$unit) # GW

# solar and wind
df_ren <- df_ren %>% 
  filter(str_detect(seriesName,"Wind|Solar"))

# total GW
unique(df_ren$scenario)
head(df_ren)
usa <- df_ren %>% 
  filter(scenario=="ref2025") %>% 
  filter(regionName=="United States") %>% 
  group_by(period) %>% 
  reframe(gw=sum(value)) %>% ungroup()

usa$Country <- "United States"

# Mexico ---------- 
# Based on https://energia.conahcyt.mx/planeas/electricidad/capacidad-generacion and https://www.cenace.gob.mx/Docs/16_MARCOREGULATORIO/Prodecen/20%202024-2038%20Cap%C3%ADtulos%201%20al%206.pdf
# Figure plotted and data to scale
url_drive <- "H:/.shortcut-targets-by-id/1CWiPbqLa53GMwIlw6QXl5kVdUX6nm-Sa/North America Battery Retirements and Recycling Capacity Research/Data/"
# Global demand in GWh
mex <- read_excel(paste0(url_drive,"Mexico_ElectricityCap_PROSEDEN.xlsx"),
                 sheet="Sheet1",range = "G5:V7")
mex <- mex %>% 
  pivot_longer(c(-Type), names_to = "Year", values_to = "gw") %>% 
  mutate(gw=gw/1e3,Year=as.numeric(Year))
  
# extrapolate based on last 10 years slope (linear)
slope_wind <- ((mex[15,3]-mex[6,3])/10) %>% .$gw
slope_solar <- ((mex[30,3]-mex[21,3])/10) %>% .$gw


mex_ext <- mex %>% filter(Year==2038) %>% 
  mutate(Year=NULL) %>% 
  cross_join(tibble(Year=2039:2050)) %>% 
  mutate(gw=gw+if_else(Type=="Wind",slope_wind,slope_solar)*(Year-2038))


mex <- rbind(mex,mex_ext) %>% arrange(Type,Year)

mex <- mex %>% group_by(Year) %>% reframe(gw=sum(gw)) %>% ungroup()

mex$Country <- "Mexico"

# Canada -----
# Source: Canada’s Energy Future 2023: Energy Supply and Demand Projections to 2050
# https://open.canada.ca/data/en/dataset/7643c948-d661-4d90-ab91-e9ac732fc737

can <- read.csv(paste0(url_drive,"Canada_electricity-capacity-2023.csv"))
head(can)
unique(can$Variable)
can <- can %>% 
  filter(Scenario=="Current Measures",Region=="Canada") %>% 
  filter(str_detect(Variable,"Solar|Wind"))

can <- can %>% 
  group_by(Year) %>% 
  reframe(gw=sum(Value)/1e3) %>% ungroup() # MW to GW

can <- can %>% filter(Year>2024)

can$Country <- "Canada"


df <- usa %>% rename(Year=period) %>% 
  rbind(can) %>% rbind(mex)

# Convert to battery kwh ----

# from stock to annual addition of capacity
df <- df %>% 
  arrange(Country, Year) %>%  
  group_by(Country) %>%       
  mutate(gw = gw - lag(gw, default = 0)) %>% ungroup()


# Key parameters
sg <- 0.2 # storage to generation ratio - 20%
sd <- 4 # duration of storage, in hours

df <- df %>% 
  group_by(Country,Year) %>% 
  reframe(gw=sum(gw)) %>% ungroup() %>% 
  mutate(gwh=gw*sg*sd,
         gw=NULL)

df %>% filter(Year%%5==0) %>% 
  pivot_wider(names_from = Country, values_from = gwh)


write.csv(df,"Inputs/StationaryStorage.csv",row.names = F)


## EoF