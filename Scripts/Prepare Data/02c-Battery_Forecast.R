# Battery Forecast scenarios
# PBH July 2025


source("Scripts/00-Libraries.R", encoding = "UTF-8")


# LDV -------------

bat <- read.csv("Inputs/Battery/battery_country.csv")
head(bat)
bat <- bat %>% rename(Country=Sales_Country) %>% 
  dplyr::select(Vehicle,Year,Country,chemistry,kwh_veh)

# repeat 2024 as constant - FOR NOW

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

# repeat 2024 as constant - FOR NOW

bat_HDV_2024 <- bat_HDV %>% filter(Year==2024)
for (i in 2025:2050){
  aux <- bat_HDV_2024 %>% mutate(Year=i)
  bat_HDV <- rbind(bat_HDV,aux)
  rm(aux)
}
rm(i)

# Join -----
x <- rbind(bat,bat_HDV)
unique(x$chemistry)
write.csv(x,"Inputs/batterySize.csv",row.names = F)

# Figure ---------
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