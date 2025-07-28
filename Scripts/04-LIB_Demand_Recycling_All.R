# Main inflow and outflow model 
# PBH July 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-ModelParameters.R")

# Load Data --------

# Electric Vehilces
ev <- read.csv("Results/Intermediate/EV.csv")

# Stationary Storage
ss <- read.csv("Results/Intermediate/ss.csv")

# Consumer Electronics
ce <- read.csv("Inputs/consumerElectronics.csv")

# data format
ce <- ce %>% 
  mutate(Vehicle="Consumer Electronics") %>%
  pivot_longer(c(ce_gwh,ce_gwh_recyc), 
               names_to = "Flow", values_to = "kwh") %>% 
  mutate(kwh=kwh*1e6,
         Flow=if_else(Flow=="ce_gwh","EVSales","LIB_recycling"))


# Prod. Scrap -----
# add all sectors together
df <- rbind(ev,ss,ce)

unique(df$Flow)
df <- df %>% 
  mutate(FlowType=if_else(Flow=="LIB_recycling","LIB Recycling","LIB Production"))

prod <- df %>% 
  filter(str_detect(FlowType,"Production")) %>% 
  group_by(Vehicle,Country,Year) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup()

# Check with current NA capacity - TO DO
prod <- prod %>% 
  mutate(kwh=kwh*p.scrap) %>% 
  mutate(Flow="LIB_scrap") %>% 
  mutate(FlowType="LIB Recycling")

df <- rbind(df,prod)

# to Black Mass -----

# weight cathode fraction
cathode_fraction <- 0.3 # do it by chemistry

# energy density  
energy_density <- 100/500 # kWh per kg, do it by chemistry

# black mass
df <- df %>% 
  mutate(blackMass_kg=kwh/energy_density*cathode_fraction,
         battery_kg=kwh/energy_density)


write.csv(df,"Results/Momentum.csv",row.names = F)

# Figure ----

# rename flow
levels_flow <- c("New EV sales","Additional LIB\n(Replacement)",
                 "LIB Recycling","LIB Scrap")
df <- df %>% 
  mutate(flow_label=case_when(
    Flow=="EVSales" ~ levels_flow[1],
    Flow=="addLIB" ~ levels_flow[2],
    Flow=="LIB_recycling" ~ levels_flow[3],
    Flow=="LIB_scrap" ~ levels_flow[4],
    T ~ "NA") %>% factor(levels=levels_flow))

df %>% 
  mutate(gwh=kwh/1e6) %>% 
  ggplot(aes(Year,gwh,fill=Vehicle))+
  geom_col(position="stack",col="black",linewidth=0.1)+
  facet_grid(Country~flow_label,scales="free_y") +
  scale_x_continuous(breaks = c(2025,2030,2040,2050))+
  labs(x="",y="",title="GWh of Batteries")


ggsave("Figures/flows_EV.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7*1.5)


# EoF