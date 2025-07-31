# Main inflow and outflow model 
# PBH July 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-ModelParameters.R")

df <- read.csv("Results/Feedstock/Momentum__reuse0.csv")
df2 <- read.csv("Results/Production/Momentum__reuse0.csv")

df <- rbind(
  mutate(df,FlowType="LIB Recycling",blackMass_kg=NULL),
  mutate(df2,FlowType="LIB Production"))

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

# Figure 2 -----

df %>% 
  # filter(FlowType=="LIB Recycling") %>% 
  # mutate(Vehicle=Country) %>% 
  group_by(FlowType,Year,Vehicle) %>% 
  reframe(gwh=sum(kwh)/1e6) %>% ungroup() %>% 
  ggplot(aes(Year,gwh,fill=Vehicle))+
  facet_wrap(~FlowType)+
  geom_col(position="stack",col="black",linewidth=0.1)+
  scale_x_continuous(breaks = c(2025,2030,2040,2050))+
  scale_y_continuous(labels = scales::label_comma())+
  labs(x="",y="",title="GWh of Batteries")

ggsave("Figures/Fig2.png", ggplot2::last_plot(),units="cm",
       dpi=600,width=8.7*2,height=8.7)

# EoF