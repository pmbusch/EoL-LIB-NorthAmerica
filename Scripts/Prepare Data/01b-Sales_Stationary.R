## Stationary storage based on forecast of solar and wind generation capacity
# Method originally from https://doi.org/10.1016/j.resconrec.2025.108377
# Add historical capacity data from 2015, to build stock


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
  rename(Year=period) %>% 
  group_by(Year) %>% 
  reframe(gw=sum(value)) %>% ungroup()

usa$Country <- "United States"

# Historical
# Source: https://www.eia.gov/electricity/data/state/
# EIA-860 Annual Electric Generator Report (released: 9/21/2024)
# Existing Nameplate and Net Summer Capacity by Energy Source, Producer Type and State
url_drive <- "H:/.shortcut-targets-by-id/1CWiPbqLa53GMwIlw6QXl5kVdUX6nm-Sa/North America Battery Retirements and Recycling Capacity Research/Data/"
usa_hist <- read_excel(paste0(url_drive,"USA_existcapacity_annual.xlsx"),
                       skip = 1)
head(usa_hist)
usa_hist <- usa_hist %>% 
  filter(Year>2014) %>% 
  filter(`State Code`=="US") %>% 
  filter(!str_detect(`Producer Type`,"Total")) %>% 
  filter(str_detect(`Fuel Source`,"Wind|Solar"))

# aggregate
usa_hist <- usa_hist %>% 
  group_by(Year) %>% 
  reframe(gw=sum(`Nameplate Capacity (Megawatts)`)/1e3) %>% ungroup() %>% 
  mutate(Country="United States")

usa <- rbind(usa,usa_hist)
rm(usa_hist)

# Mexico ---------- 
# Based on https://energia.conahcyt.mx/planeas/electricidad/capacidad-generacion and https://www.cenace.gob.mx/Docs/16_MARCOREGULATORIO/Prodecen/20%202024-2038%20Cap%C3%ADtulos%201%20al%206.pdf
# Figure plotted and data to scale
# Global demand in GWh - forecast and historical
mex <- read_excel(paste0(url_drive,"Mexico_ElectricityCap_PROSEDEN.xlsx"),
                 sheet="Forecast",range = "G5:AG7")

mex <- mex %>% 
  pivot_longer(c(-Type), names_to = "Year", values_to = "gw") %>% 
  mutate(gw=gw/1e3,Year=as.numeric(Year)) %>% 
  filter(Year>2014) %>% 
  arrange(Type,Year)
  
# extrapolate based on last 10 years slope (linear)
slope_solar <- ((mex[24,3]-mex[15,3])/10) %>% .$gw
slope_wind <- ((mex[48,3]-mex[39,3])/10) %>% .$gw

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

range(can$Year)
can <- can %>% filter(Year>2014)

can$Country <- "Canada"

df <- rbind(usa,can,mex)

# from stock to annual addition of capacity
df <- df %>% 
  arrange(Country, Year) %>%  
  group_by(Country) %>%  
  mutate(gw_stock=gw) %>% 
  mutate(gw = gw - lag(gw, default = 0)) %>% ungroup() %>% 
  mutate(gw=if_else(gw<0,0,gw))

# Figure -----

df %>%
  pivot_longer(c(gw,gw_stock), names_to = "key", values_to = "value") %>% 
  mutate(key=if_else(key=="gw","Added year-to-year capacity (GW)",
                     "Total installed capacity (GW)")) %>% 
  ggplot(aes(Year,value,fill=Country))+
  geom_col(position = "stack")+
  facet_wrap(~key,ncol=1,scales="free_y")+
  labs(x="",y="",title="Solar and Wind Electricity Capacity (GW)")

ggsave("Figures/Inputs/gw_capacity.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)



# Convert to battery kwh ----



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