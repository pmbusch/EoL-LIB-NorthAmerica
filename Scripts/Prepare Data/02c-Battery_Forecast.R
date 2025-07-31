# Battery Forecast scenarios
# PBH July 2025


source("Scripts/00-Libraries.R", encoding = "UTF-8")


# LDV -------------

bat <- read.csv("Inputs/Battery/battery_country.csv")
head(bat)
bat <- bat %>% rename(Country=Sales_Country) %>% 
  dplyr::select(Vehicle,Year,Country,chemistry,kwh_veh)

bat <- bat %>% mutate(Country=str_replace(Country,"USA","United States"))

# chemistry share of 2024 (for cathode weight and energy density)
chem <- bat %>% 
  filter(Year==2024) %>% 
  group_by(Vehicle,Country) %>% 
  mutate(share=kwh_veh/sum(kwh_veh)) %>% ungroup()

# no chemistry for modeling
bat <- bat %>% 
  group_by(Vehicle,Year,Country) %>% reframe(kwh_veh=sum(kwh_veh)) %>% ungroup()

# repeat 2024 as constant for baseline scenarios
bat_2024 <- bat %>% filter(Year==2024)
for (i in 2025:2050){
  aux <- bat_2024 %>% mutate(Year=i)
  bat <- rbind(bat,aux)
  rm(aux)
}
rm(i)


# HDV ----
bat_HDV <- read.csv("Inputs/Battery/HDV_battery_country.csv")
head(bat_HDV)
bat_HDV <- bat_HDV %>% 
  rename(Country=Sales_Country,chemistry=Cathode_Chemistry) %>% 
  dplyr::select(Vehicle,Year,Country,chemistry,kwh_veh)

bat_HDV <- bat_HDV %>% mutate(Country=str_replace(Country,"USA","United States"))

chem_HDV <- bat_HDV %>% 
  filter(Year==2024) %>% 
  group_by(Vehicle,Country) %>% 
  mutate(share=kwh_veh/sum(kwh_veh)) %>% ungroup()

chem <- rbind(chem,chem_HDV)
write.csv(chem,"Inputs/Battery/chem2024.csv",row.names = F)

# no chemistry for modeling
bat_HDV <- bat_HDV %>% 
  group_by(Vehicle,Year,Country) %>% reframe(kwh_veh=sum(kwh_veh)) %>% ungroup()

# repeat 2024 as constant - reference scenario

bat_HDV_2024 <- bat_HDV %>% filter(Year==2024)
for (i in 2025:2050){
  aux <- bat_HDV_2024 %>% mutate(Year=i)
  bat_HDV <- rbind(bat_HDV,aux)
  rm(aux)
}
rm(i)

# Join -----
x <- rbind(bat,bat_HDV)
write.csv(x,"Inputs/batterySize.csv",row.names = F)

# Low and High Scenarios ------

# load data
bat_scen <- rbind(read.csv("Inputs/Battery/bat_quantiles.csv"),
                  read.csv("Inputs/Battery/bat_quantiles_MDV.csv"))

# transition linearly towards 2035
goal_year <- 2035 # year that range/cap is achieved

# slope
slope_bat <- bat_scen %>%
  rename(Country=Sales_Country) %>% 
  mutate(Country=str_replace(Country,"USA","United States")) %>% 
  mutate(slope_low=(low_kwh_veh-mean_kwh_veh)/(goal_year-2024),
         slope_high=(high_kwh_veh-mean_kwh_veh)/(goal_year-2024))
  
# linearly until 2035 - Low
bat_low <- x %>% 
  left_join(slope_bat) %>% 
  mutate(kwh_veh=case_when(
    Year<2025 ~ kwh_veh,
    Year<=goal_year ~ mean_kwh_veh+(Year-2024)*slope_low,
    T ~ low_kwh_veh)) %>% 
    dplyr::select(-high_kwh_veh,-slope_high,-slope_low,-mean_kwh_veh,-low_kwh_veh) %>% 
  arrange(Country,Vehicle,Year)
write.csv(bat_low,"Inputs/batterySize_small.csv",row.names = F)

# linearly until 2035 - High
bat_high <- x %>% 
  left_join(slope_bat) %>% 
  mutate(kwh_veh=case_when(
    Year<2025 ~ kwh_veh,
    Year<=goal_year ~ mean_kwh_veh+(Year-2024)*slope_high,
    T ~ high_kwh_veh)) %>% 
  dplyr::select(-high_kwh_veh,-slope_high,-slope_low,-mean_kwh_veh,-low_kwh_veh) %>% 
  arrange(Country,Vehicle,Year)
write.csv(bat_high,"Inputs/batterySize_large.csv",row.names = F)


# Figure ---------

# to run figure comment part where I remove chemistry detail
library(khroma)
library(scico)

x %>% 
  # mutate(chemistry=if_else(str_detect(chemistry,"LTO|LMO|LMP"),"LTO/LMO/LMP",chemistry)) %>% 
  # mutate(chemistry=if_else(str_detect(chemistry,"721"),"NMC 811",chemistry)) %>% 
  group_by(Vehicle,Year,Country,chemistry) %>% reframe(kwh_veh=sum(kwh_veh)) %>% ungroup() %>% 
  ggplot(aes(Year,kwh_veh,fill=chemistry))+
  geom_col(position = "stack",col="black",linewidth=0.1)+
  facet_grid(Vehicle~Country,scales="free_y")+
  coord_cartesian(expand = F)+
  scale_fill_manual(values = scico(14, palette = "batlow", direction = 1)) +
  labs(x="",y="",title="Battery size [kWh/veh]",fill="Chemistry")

ggsave("Figures/Inputs/BatterySize.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7*1.5)



# EoF