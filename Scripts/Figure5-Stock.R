# Stock vs Flow analysis
# PBH August 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-ModelParameters.R")

# LOAD DATA -----

## Results - Flows for recycling -----
df <- read.csv("Results/Feedstock/Momentum__reuse0.csv")
df2 <- read.csv("Results/Production/Momentum__reuse0.csv")

df <- rbind(
  mutate(df,FlowType="LIB Recycling Feedstock",blackMass_kg=NULL),
  mutate(df2,FlowType="LIB Production Requirement"))
rm(df2)

df <- df %>% 
  filter(str_detect(FlowType,"Recycling")) %>% 
  group_by(FlowType,Country,Year,Vehicle) %>% 
  reframe(gwh=sum(kwh)/1e6) %>% ungroup()
head(df)


## Stock of Products ----
ev <- read.csv("Inputs/Stocks/Momentum__reuse0.csv",stringsAsFactors = FALSE)
ev <- ev %>% dplyr::select(Vehicle,Country,Year,EV_Stock)
bat <- read.csv("Inputs/BatterySize.csv")
ev <- ev %>% left_join(bat) %>% 
  filter(EV_Stock>0) %>% 
  mutate(gwh=EV_Stock*kwh_veh/1e6) %>% 
  mutate(EV_Stock=NULL,kwh_veh=NULL)

ss <- read.csv("Inputs/StationaryStorage.csv")
ss <- ss %>% dplyr::select(Country,Year,gwh_stock) %>% 
  rename(gwh=gwh_stock) %>% mutate(Vehicle="Stationary Storage")
ce <- read.csv("Inputs/consumerElectronics.csv")
ce <- ce %>% dplyr::select(Year,Country,ce_gwh_stock) %>% 
  rename(gwh=ce_gwh_stock) %>% mutate(Vehicle="Consumer Electronics")

stock <- rbind(ev,ss,ce) %>% 
  mutate(FlowType="Stock")

## Join data ----
df <- rbind(df,stock)


# FIGURE -----

df %>% 
  filter(Year %in% c(2030,2035,2040,2050)) %>% 
  mutate(x=1) %>% 
  ggplot(aes(x,gwh,fill=Vehicle))+
  geom_col()+
  facet_wrap(Year~FlowType,scales="free_y",ncol=2)+
  labs(x="",y="")
  
  


# EoF