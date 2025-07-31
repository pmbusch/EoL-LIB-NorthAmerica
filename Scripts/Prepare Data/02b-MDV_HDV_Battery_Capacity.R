# Battery Capacity - Historical Sales and Average battery capacity
# Source of Data: EV Volumes
# PBH  July 2025

# LOAD DATA -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# 2024 EV Volumes data - Not possible to Share
bat <- read_excel("Inputs/Original/MHCV-MHBC-2025-06-06b-Mailout.xlsx",sheet="DATA")
(names(bat) <- names(bat) %>% str_remove_all("&|'") %>% str_replace_all(" |-|\\.","_") %>% 
    str_replace_all("__","_") %>% str_remove("Delivered_"))

# Filter north america
bat <- bat %>% filter(Sales_Country %in% c("Mexico","Canada","USA")) %>% 
  filter(Propulsion=="BEV")

# to long format
bat <- bat %>% 
  dplyr::select(Sales_Country,Brand,Global_Segment,`Estd_Average_Battery_Capacity_[kWh]`,
                Primary_Application,Cathode_Chemistry,
                `2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,
                `2017`,`2018`,`2019`,`2020`,`2021`,`2022`,`2023`,`2024`) %>%
  rowid_to_column() %>% 
  pivot_longer(-c(rowid,Sales_Country,Brand,Global_Segment,`Estd_Average_Battery_Capacity_[kWh]`,
                  Primary_Application,Cathode_Chemistry), 
               names_to = "year", values_to = "sales")
unique(bat$year)  

bat <- bat %>% 
  mutate(year=as.numeric(year)) %>% 
  filter(year<2025,year>2014,sales>0)
unique(bat$year)  
range(bat$sales)


table(bat$Global_Segment)
# AGGREGATE SEGMENTS
bat <- bat %>% 
  filter(!str_detect(Global_Segment,"Robo")) %>% 
  mutate(Vehicle=case_when(
    str_detect(Global_Segment,"Bus") ~ "Buses",
    Global_Segment=="Truck - Medium" ~ "Medium trucks",
    Global_Segment=="Truck - Heavy" ~ "Heavy trucks"))
table(bat$Vehicle,bat$Global_Segment)

# distribtion ---------

dist <- bat %>% rename(kwh_veh=`Estd_Average_Battery_Capacity_[kWh]`)

# histogram
ggplot(dist,aes(kwh_veh,fill=Sales_Country))+
  # geom_histogram()+
  geom_density(alpha=.5)+
  facet_grid(Vehicle~Sales_Country)

library(ggridges)
dist %>% 
  ggplot(aes(kwh_veh,fill=Sales_Country,y=year,group=year,weight=sales))+
  # geom_density_ridges(alpha=.8)+
  geom_density_ridges(
    aes(point_size=sales),
    jittered_points = TRUE,
    linewidth=0.1,
    position = position_points_jitter(width = 0.01, height = 0),
    # point_shape = '|', 
    point_alpha = 0.7, alpha = 0.7)+
  facet_grid(Vehicle~Sales_Country,scales="free_y",space="free_y")+
  scale_point_size_continuous(range=c(0.1,5))+
  labs(x="Battery size [kWh per vehicle]",y="",fill="",caption="Weighted by sales")+
  theme(legend.position = "none")

ggsave("Figures/Inputs/EVVol_dist_MDV.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7*1.5)


bat_scen <- dist %>% 
  filter(year==2024) %>% 
  group_by(Sales_Country,Vehicle) %>% 
  reframe(mean_kwh_veh=weighted.mean(kwh_veh,sales),
          low_kwh_veh=Hmisc::wtd.quantile(kwh_veh,weights=sales,probs = 0.1),
          high_kwh_veh=Hmisc::wtd.quantile(kwh_veh,weights=sales,probs = 0.9))
bat_scen
write.csv(bat_scen,"Inputs/Battery/bat_quantiles_MDV.csv",row.names = F)


# summarise by year, COUNTRY, MIX -----
bat <- bat %>% 
  group_by(year,Sales_Country, Vehicle,Cathode_Chemistry) %>% 
  reframe(kwh_veh=weighted.mean(`Estd_Average_Battery_Capacity_[kWh]`,sales),
          sales=sum(sales)) %>% ungroup()

# check
head(bat)
bat %>% filter(year==2024) %>% pull(sales) %>% sum() # 5788

# divide total kwh by chemistry
# eg. take weighted average of battery size for all chemistry, then 
# distribute based on share of chemistry kWh
# this method ensures same kWh for all chemistries
bat <- bat %>% 
  mutate(kwh_total=kwh_veh*sales) %>% 
  group_by(year,Sales_Country,Vehicle) %>% 
  mutate(kwh_avg=weighted.mean(kwh_veh,sales),
         kwh_share=kwh_total/sum(kwh_total)) %>% ungroup() %>% 
  mutate(kwh_veh_chem=kwh_share*kwh_avg,
         kwh_total=NULL,kwh_share=NULL)
  
# checks
# (291*35+440*9)/(35+9) # avg size
# 322*44*.28 # share of NMC 2015
# 9*440 # equal

bat <- bat %>% 
  rename(Year=year) %>% 
  rename(kwh_veh_total=kwh_avg) %>% 
  mutate(kwh_veh=kwh_veh_chem,kwh_veh_chem=NULL)

# SAVE DATA -----------

# historical sales
sales <- bat %>% 
  group_by(Vehicle,Year,Sales_Country) %>% 
  reframe(sales=sum(sales)) %>% ungroup()
write.csv(sales,"Inputs/Battery/historical_EV_HDV_sales.csv",row.names=F)

write.csv(bat,"Inputs/Battery/HDV_battery_country.csv",row.names = F)


bat_country_size <- bat %>% 
  group_by(Vehicle,Year,Sales_Country) %>% 
  reframe(kwh_veh_total=mean(kwh_veh_total)) %>% ungroup()
write.csv(bat_country_size,"Inputs/Battery/HDV_battery_size_country.csv",row.names = F)

## Save Chem Share ----------
bat_country_chem <- bat %>% 
  group_by(Vehicle,Year,Sales_Country) %>% 
  mutate(chem_share_mwh=kwh_veh/sum(kwh_veh)) %>% ungroup()
write.csv(bat_country_chem,"Inputs/Battery/HDV_battery_chem_country.csv",row.names = F)

# Figures -------------

## Sales over time ----
sales %>% 
  mutate(sales=sales/1e3) %>% 
  mutate(key=paste0(Sales_Country,Vehicle)) %>% 
  ggplot(aes(Year,sales,col=Sales_Country,
             group = key))+
  geom_line(aes(linetype=Vehicle))+
  geom_point()+
  coord_cartesian(expand = F)+
  labs(x="",y="",title="Vehicle Sales [thousands]",col="")
ggsave("Figures/Inputs/Historical_HDV_EVSales.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

## Battery size over time ----
bat_country_size %>% 
  mutate(key=paste0(Sales_Country,Vehicle)) %>% 
  ggplot(aes(Year,kwh_veh_total,col=Sales_Country,
             group = key))+
  geom_line(aes(linetype=Vehicle))+
  geom_point()+
  ylim(0,600)+
  scale_x_discrete(breaks = seq(2016,2024,2))+
  coord_cartesian(expand = F)+
  labs(x="",y="",title="Avg. Battery Size [kWh/veh]",col="")

ggsave("Figures/Inputs/HDV_LibSize.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

## Chem share over time ----
bat_country_chem %>% 
  ggplot(aes(Year,chem_share_mwh,fill=Cathode_Chemistry))+
  # geom_area()+
  geom_col()+
  facet_grid(Vehicle~Sales_Country)+
  coord_cartesian(expand = F)+
  scale_x_discrete(breaks = seq(2016,2024,2))+
  scale_y_continuous(labels=scales::percent)+
  labs(x="",y="",title="Cathode Chemistry Share",fill="")

ggsave("Figures/Inputs/HDV_LibChem.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

# EoF