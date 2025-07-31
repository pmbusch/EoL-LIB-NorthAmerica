# Battery Capacity - Historical Sales and Average battery capacity
# Source of Data: EV Volumes
# PBH August 2023 - Updated July 2025


# LOAD DATA -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# 2024 EV Volumes data - Not possible to Share
bat <- read.csv("Inputs/Original/BatteryInstallation-Tracker-May_2025_Data.csv")
(names(bat) <- names(bat) %>% str_remove("&") %>% str_replace_all(" |-|\\.","_") %>% 
    str_replace_all("__","_") %>% str_remove("Delivered_"))

# Filter north america
bat <- bat %>% filter(Sales_Country %in% c("Mexico","Canada","USA"))

# to long format
bat <- bat %>% 
  dplyr::select(-OEM_Group,-Brand,-Make_Model,-Architecture,
                -LCV_Details,-Fast_Charging,-Battery_kWh,-ED_Wh_kg_Cell,
                -Cell_Type,-Cell_Supplier,-Cathode_Supplier) %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid,Sales_Region,Sales_Sub_Region,Sales_Country, Global_Segment,
                 Vehicle_Production_Region,Vehicle_Production_Country,
                 Propulsion,Cathode_Chemistry,Cathode_Mix), names_to = "key", values_to = "value")
unique(bat$key)  

# get total EV registrations and MWh
bat <- bat %>% 
  mutate(type=str_extract(key,"Mwh|Reg"),
         year=str_extract(key,paste0(2013:2025,collapse="|")),
         month=substr(key, nchar(key)-4, nchar(key))) %>% 
  filter(year<2025,value>0)
unique(bat$year)  
unique(bat$type)
range(bat$value)

# only BEV
bat <- bat %>% filter(Propulsion=="BEV")

table(bat$Global_Segment)
# remove QC (quadricycles), MC (microcars), SS (sport specialty), PUPs
# clasify LCV as Vans (ICCT-light commercial vehicles), rest as cars
bat <- bat %>% 
  filter(!(Global_Segment %in% c("PUP-D","PUP-E","QC","SS","MC"))) %>% 
  mutate(Vehicle=if_else(Global_Segment=="LCV","Vans","Cars"))
table(bat$Vehicle,bat$Global_Segment)

# Distribution avg size -----------
head(bat)
# sum over years
dist <- bat %>% 
  group_by(Sales_Country, Vehicle,type,year,rowid) %>% 
  reframe(value=sum(value)) %>% ungroup()
dist <- dist %>%  
  pivot_wider(names_from = type, values_from = value) %>% 
  filter(!is.na(Mwh))

dist <- dist %>% mutate(kwh_veh=Mwh*1e3/Reg)

# histogram
ggplot(dist,aes(kwh_veh,fill=Sales_Country))+
  # geom_histogram()+
  geom_density(alpha=.5)+
  facet_grid(Vehicle~Sales_Country)

library(ggridges)
dist %>% 
  filter(Vehicle=="Cars"|year!=2020) %>% 
  ggplot(aes(kwh_veh,fill=Sales_Country,y=year,group=year,weight=Reg))+
  # geom_density_ridges(alpha=.8)+
  geom_density_ridges(
    aes(point_size = Reg),
    jittered_points = TRUE,
    linewidth=0.1,
    position = position_points_jitter(width = 0.01, height = 0),
    # point_shape = '|', 
    point_alpha = 0.7, alpha = 0.7)+
  facet_grid(Vehicle~Sales_Country,scales="free_y",space="free_y")+
  scale_point_size_continuous(range=c(0.1,5))+
  labs(x="Battery size [kWh per vehicle]",y="",fill="",caption="Weighted by sales")+
  theme(legend.position = "none")

ggsave("Figures/Inputs/EVVol_dist.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7*1.5)

# same average as later
bat_scen <- dist %>% 
  filter(year==2024) %>% 
  group_by(Sales_Country,Vehicle) %>% 
  reframe(mean_kwh_veh=weighted.mean(kwh_veh,Reg),
          low_kwh_veh=Hmisc::wtd.quantile(kwh_veh,weights=Reg,probs = 0.1),
          high_kwh_veh=Hmisc::wtd.quantile(kwh_veh,weights=Reg,probs = 0.9))
bat_scen
write.csv(bat_scen,"Inputs/Battery/bat_quantiles.csv",row.names = F)


# Summarise ----------
# summarise by year, COUNTRY, MIX
bat <- bat %>% 
  group_by(Sales_Country, Vehicle,
           Cathode_Chemistry,Cathode_Mix,type,year) %>% 
  reframe(value=sum(value))
bat <- bat %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  rename(MWh=Mwh,unit=Reg)

# check
head(bat)
bat %>% filter(year==2018) %>% pull(MWh) %>% sum() # 18322

# DATA WRANGLING -----

## Aggregate Chemistry -----
bat %>% filter(year==2024) %>% 
  group_by(Vehicle,Cathode_Chemistry) %>% summarise(x=sum(MWh,na.rm=T)) %>%
  arrange(desc(x)) %>% ungroup() %>% mutate(perc=x/sum(x)*100)
bat %>% filter(year==2024) %>% 
  group_by(Vehicle,Cathode_Mix) %>% summarise(x=sum(MWh,na.rm=T)) %>%
  arrange(desc(x)) %>% ungroup() %>% mutate(perc=x/sum(x)*100)
# NMC ratios: 721, 622, 811, 532, 111
# Others NMCA 89-4-4-3 (used by Tesla only) https://evreporter.com/nmca-cathode-for-lithium-ion-batteries/

# Mix aggregation
bat %>% group_by(Cathode_Chemistry,Cathode_Mix) %>% tally() %>% arrange(desc(n))
bat <- bat %>% 
  mutate(mix=case_when(
    str_detect(Cathode_Mix,"NMC 111") & Cathode_Chemistry!="LMO" ~ " 111",
    str_detect(Cathode_Mix,"NMC 721") ~ " 721",
    str_detect(Cathode_Mix,"NMC 532|NMC532|NMC 523") ~ " 532", # I assume 523 is a typo error
    str_detect(Cathode_Mix,"NMC 622|NMC622") ~ " 622",
    str_detect(Cathode_Mix,"NMC 442") ~ " 442",
    str_detect(Cathode_Mix,"NMC 811") ~ " 811",
    str_detect(Cathode_Mix,"NMCA 89:04:04:03") ~ "A 89:4:4:3",
    T ~ ""))
bat %>% group_by(Cathode_Chemistry,Cathode_Mix,mix) %>% tally() %>% arrange(desc(n))

bat <- bat %>% mutate(chemistry=paste0(Cathode_Chemistry,mix))

# share of ratios of NMC
bat %>% 
  filter(chemistry %in% c("NMC","NMC 111","NMC 811","NMC 622","NMC 532")) %>% 
                          # "NMC 721","NMCA 89:4:4:3","NMC 442")) %>%
  filter(year==2024) %>% 
  group_by(Vehicle,Sales_Country,chemistry) %>% 
  summarise(MWh=sum(MWh)) %>% ungroup() %>% 
  group_by(Vehicle,Sales_Country) %>% 
  mutate(perc=MWh/sum(MWh)) %>% 
  ggplot(aes(Sales_Country,perc,fill=chemistry))+
  geom_col()+
  coord_flip()+
  facet_wrap(~Vehicle,nrow=1)

# Almost equal for North America ....
nmc_share <- bat %>% 
  filter(chemistry %in% c("NMC 111","NMC 811","NMC 622","NMC 532")) %>%
  # filter(year==2024) %>% 
  group_by(year,chemistry) %>% 
  summarise(MWh=sum(MWh,na.rm=T)) %>% ungroup() %>% 
  group_by(year) %>% 
  mutate(perc=MWh/sum(MWh)) %>% ungroup()

# other share - TBA
other_share <- bat %>% 
  filter(!(chemistry %in% c("NMC","tba","LTO","LMO","LMP"))) %>% 
  # filter(year==2024) %>% 
  group_by(year,chemistry) %>% 
  summarise(MWh=sum(MWh,na.rm=T)) %>% ungroup() %>%
  group_by(year) %>% 
  mutate(perc=MWh/sum(MWh)) %>% ungroup()

table(bat$chemistry)
# combine into 4 groups
bat <- bat %>% 
  mutate(chemistry_group=case_when(
    chemistry=="LFP" ~ "LFP",
    chemistry=="NCA" ~ "NCA",
    chemistry %in% c("LMO","LTO") ~ "LTO/LMO",
    chemistry %in% c("NMC","NMC 111") ~ "NMC Low Ni",
    chemistry %in% c("NMC 532","NMC 622","NMC 721","NMC 811") ~ "NMC High Ni",
    T ~ "remove"))
table(bat$chemistry_group,bat$chemistry)

## Flat years -----
bat <- bat %>% mutate(unit = if_else(is.na(unit), 0, unit))

# Checks
bat %>% group_by(year) %>% summarise(MWh=sum(MWh),unit=sum(unit,na.rm = T)) %>% 
  mutate(kWh_veh=MWh*1e3/unit)

## orders -----

chem_order <- bat %>% group_by(chemistry) %>% 
  summarise(MWh=sum(MWh),unit=sum(unit,na.rm = T)) %>% 
  mutate(kWh_veh=MWh*1e3/unit) %>% arrange(kWh_veh) %>% pull(chemistry)
bat <- bat %>% mutate(chemistry=factor(chemistry,levels=chem_order))

## Dimensions -----
bat$Sales_Country %>% unique() # 3
bat$year %>% unique()
bat$chemistry %>% unique()

# Distribute NMC and Other as averages
nmc_share
bat_nmc <- bat %>% filter(chemistry=="NMC") %>% rename(x=chemistry)

bat_nmc <- bat_nmc %>% 
  filter(year>2014) %>% 
  filter(MWh>0, unit>0) %>% 
  left_join(mutate(nmc_share,x="NMC",MWh=NULL),relationship = "many-to-many") %>% 
  mutate(MWh=MWh*perc,unit=unit*perc,
         perc=NULL,x=NULL)

# add back to database
bat <- bat %>% filter(chemistry!="NMC") %>% rbind(bat_nmc) %>% filter(year>2014)
rm(bat_nmc)

# others share - same method
other_share
unique(bat$chemistry)
bat_other <- bat %>% filter(chemistry=="tba") %>% rename(x=chemistry)
bat_other <- bat_other %>% 
  filter(MWh>0, unit>0) %>% 
  left_join(mutate(nmc_share,x="tba",MWh=NULL),relationship = "many-to-many") %>% 
  mutate(MWh=MWh*perc,unit=unit*perc,
         perc=NULL,x=NULL)
bat <- bat %>% filter(chemistry!="tba") %>% rbind(bat_other)
rm(bat_other)

bat$chemistry %>% unique()

# Aggregate data -----

# Country level ----------
bat_country <- bat %>% 
  filter(year>2014) %>% 
  group_by(Vehicle,year,Sales_Country,chemistry) %>% 
  summarise(MWh=sum(MWh,na.rm=T),
            unit=sum(unit,na.rm=T)) %>% ungroup()

# kWh per vehicle
# Note: chemistry is taking as weighted average by units
bat_country <- bat_country %>% 
  filter(MWh>0&unit>0) %>% 
  group_by(Vehicle,year,Sales_Country) %>% 
  mutate(share_units=unit/sum(unit)) %>% # share of units by chemistry
  mutate(kwh_veh=sum(MWh)*1e3/sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh=kwh_veh*share_units) %>% 
  group_by(Vehicle,year,Sales_Country) %>% 
  mutate(kwh_veh_total=sum(kwh_veh)) %>% ungroup()

bat_country <- bat_country %>% rename(Year=year)

bat_country %>% filter(Year==2024) %>% 
  group_by(Sales_Country,Vehicle) %>% reframe(x=mean(kwh_veh_total))

# SAVE DATA -----------

# historical sales
sales <- bat %>% rename(Year=year) %>% 
  group_by(Vehicle,Year,Sales_Country) %>% 
  reframe(unit=sum(unit)) %>% ungroup()
write.csv(sales,"Inputs/Battery/historical_EVsales.csv",row.names=F)

write.csv(bat_country,"Inputs/Battery/battery_country.csv",row.names = F)


bat_country_size <- bat_country %>% 
  group_by(Vehicle,Year,Sales_Country) %>% 
  reframe(MWh=sum(MWh),unit=sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh_total=MWh/unit*1e3)
write.csv(bat_country_size,"Inputs/Battery/battery_size_country.csv",row.names = F)

## Save Chem Share ----------
bat_country_chem <- bat_country %>% 
  group_by(Vehicle,Year,Sales_Country,chemistry) %>% 
  reframe(MWh=sum(MWh)) %>% ungroup() %>%  
  group_by(Vehicle,Year,Sales_Country) %>% 
  mutate(chem_share_mwh=MWh/sum(MWh))
write.csv(bat_country_chem,"Inputs/Battery/battery_chem_country.csv",row.names = F)

# Figures -------------

## Sales over time ----
sales %>% 
  mutate(unit=unit/1e3) %>% 
  mutate(key=paste0(Sales_Country,Vehicle)) %>% 
  ggplot(aes(Year,unit,col=Sales_Country,
             group = key))+
  geom_line(aes(linetype=Vehicle))+
  geom_point()+
  coord_cartesian(expand = F)+
  labs(x="",y="",title="Vehicle Sales [thousands]",col="")
ggsave("Figures/Inputs/Historical_EVSales.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

## Battery size over time ----
bat_country_size %>% 
  mutate(key=paste0(Sales_Country,Vehicle)) %>% 
  ggplot(aes(Year,kwh_veh_total,col=Sales_Country,
             group = key))+
  geom_line(aes(linetype=Vehicle))+
  geom_point()+
  coord_cartesian(expand = F)+
  labs(x="",y="",title="Avg. Battery Size [kWh/veh]",col="")

ggsave("Figures/Inputs/LibSize.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

## Chem share over time ----
aux <- unique(bat_country_chem$chemistry) %>% as.character() %>% sort()
bat_country_chem %>% 
  mutate(chemistry=factor(chemistry,levels=aux)) %>% 
  ggplot(aes(Year,chem_share_mwh,fill=chemistry))+
  # geom_area()+
  geom_col()+
  facet_grid(Vehicle~Sales_Country)+
  coord_cartesian(expand = F)+
  scale_x_discrete(breaks = seq(2016,2024,2))+
  scale_y_continuous(labels=scales::percent)+
  labs(x="",y="",title="Cathode Chemistry Share",fill="")

ggsave("Figures/Inputs/LibChem.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

# EoF