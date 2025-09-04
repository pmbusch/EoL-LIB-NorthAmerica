# Main inflow and outflow model 
# PBH July 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-ModelParameters.R")

df <- read.csv("Results/Feedstock/Momentum__reuse0.csv")
df2 <- read.csv("Results/Production/Momentum__reuse0.csv")

df <- rbind(
  mutate(df,FlowType="LIB Recycling Feedstock",blackMass_kg=NULL),
  mutate(df2,FlowType="LIB Production Requirement"))

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

# Prepare data
data_fig0 <- df %>% 
  mutate(ratio_cap=case_when(
    Flow!="LIB_scrap" ~ 1,
    ratio_cap>1 ~ 1,
    T ~ ratio_cap)) %>% 
  mutate(kwh=kwh*ratio_cap) %>%
  mutate(Vehicle=case_when(
    Flow=="LIB_scrap" ~ "Production Scrap",
    Flow=="addLIB" & Vehicle %in% c("Cars","Vans") ~ "LIB Replacement - LDV",
    Flow=="addLIB" & Vehicle %in% c("Stationary Storage") ~ "LIB Replacement - SS",
    Flow=="addLIB" ~ "LIB Replacement - HDV",
    Country=="Exports" ~ "Vehicle Exports",
    T ~ Vehicle))

veh_levels <- MetBrewer::met.brewer("Signac", n = 12)
names(veh_levels) <- c("Production Scrap",
                "Consumer Electronics","Stationary Storage",
                "LIB Replacement - SS",
                "LIB Replacement - LDV","LIB Replacement - HDV",
                "Heavy trucks","Medium trucks","Buses",
                "Vans","Cars","Vehicle Exports")

data_fig <- data_fig0 %>% 
  group_by(FlowType,Year,Vehicle) %>% 
  reframe(gwh=sum(kwh)/1e6) %>% ungroup() %>% 
  mutate(gwh=if_else(str_detect(Vehicle,"Exports"),-gwh,gwh)) %>% 
  mutate(Vehicle=factor(Vehicle,
                        levels=names(veh_levels)))

p1 <- ggplot(data_fig,aes(Year,gwh,fill=Vehicle))+
  facet_wrap(~FlowType)+
  geom_col(position="stack",col="black",linewidth=0.1)+
  scale_x_continuous(breaks = c(2025,2030,2040,2050))+
  scale_y_continuous(labels = scales::label_comma())+
  scale_fill_manual(values = veh_levels)+
  coord_cartesian(expand = F,ylim = c(-100,2600))+
  labs(x="",y="",title="GWh of Lithium-ion Batteries",fill="",tag="(a)")+
  theme(panel.spacing = unit(0.5, "cm"),
        plot.tag = element_text(face = "bold"))

p1


# production by year
data_fig %>% 
  filter(Year %in% c(2025:2030,2035,2040,2045,2050)) %>% 
  filter(str_detect(FlowType,"Production")) %>% 
  group_by(Year) %>% 
  reframe(gwh=sum(gwh))


# level year
data_fig %>% 
  filter(Year %in% c(2025:2030,2035,2040,2045,2050)) %>% 
  filter(str_detect(FlowType,"Recycling")) %>% 
  group_by(Year) %>% 
  reframe(gwh=sum(gwh))
  

# majorirty of feedstock
data_fig %>% 
  filter(Year %in% c(2025,2030,2031,2032,2033,2034,2035)) %>% 
  filter(str_detect(FlowType,"Recycling")) %>% 
  group_by(Year,Vehicle) %>% 
  reframe(gwh=sum(gwh)) %>% ungroup() %>% 
  pivot_wider(names_from = Year, values_from = gwh)


# zoom to distribution by vehicle type
data_fig3 <- data_fig %>% 
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(str_detect(FlowType,"Recycling")) %>% 
  filter(!str_detect(Vehicle,"Exports")) %>% 
  group_by(Year,Vehicle) %>% 
  reframe(gwh=sum(gwh)) %>% ungroup() %>% 
  group_by(Year) %>% 
  mutate(share=gwh/sum(gwh))

p3 <- ggplot(data_fig3,aes(Year,share,fill=Vehicle))+
  geom_col(position="stack",col="black",linewidth=0.1,width = 9.5)+
  scale_y_continuous(labels=scales::percent)+
  scale_x_reverse(breaks=c(2030,2040,2050))+
  scale_fill_manual(values = veh_levels)+
  coord_flip(expand = F)+
  guides(fill= guide_legend(reverse = TRUE))+
  labs(x="",y="",title="Share of Battery Recycling Feedstock by Type",fill="",tag="(b)")+
  theme(plot.tag = element_text(face = "bold"),
        axis.text.x = element_text(hjust=1),
        legend.box.spacing = unit(0, "pt"),
        legend.key.height = unit(8, "pt"),
        legend.position = "none")
p3

# share of recycling feedstock by country
data_fig2 <- data_fig0 %>% 
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(str_detect(FlowType,"Recycling")) %>% 
  group_by(FlowType,Year,Country) %>% 
  reframe(gwh=sum(kwh)/1e6) %>% ungroup() %>% 
  group_by(FlowType,Year) %>% 
  mutate(share=gwh/sum(gwh)) %>% 
  mutate(Country=factor(Country,
                        levels=rev(c("United States","Canada","Mexico","Exports"))))

p2 <- ggplot(data_fig2,aes(Year,share,fill=Country))+
  geom_col(position="stack",col="black",linewidth=0.1,width = 9.5)+
  scale_y_continuous(labels=scales::percent)+
  scale_x_reverse(breaks=c(2030,2040,2050))+
  # scale_fill_manual(values = MetBrewer::met.brewer("Cassatt1", n = 4))+
  scale_fill_manual(values = c("United States" = "#3C3B6EFF", 
                               "Mexico" = "#006847FF", 
                               "Canada" = "#B22222FF", "Exports" = "#999999FF"))+

  coord_flip(expand = F)+
  guides(fill= guide_legend(reverse = TRUE))+
  labs(x="",y="",title="Share of Battery Recycling Feedstock by Country",fill="",tag="(c)")+
  theme(plot.tag = element_text(face = "bold"),
        axis.text.x = element_text(hjust=1),
        legend.box.spacing = unit(0, "pt"),
        legend.key.height = unit(8, "pt"),
        legend.position = "bottom")
p2
cowplot::plot_grid(p1,p3,p2,ncol=1,rel_heights = c(0.55,0.2,0.25))

ggsave("Figures/Fig2.png", ggplot2::last_plot(),units="cm",
       dpi=600,width=8.7*2,height=8.7*1.75)


# EoF