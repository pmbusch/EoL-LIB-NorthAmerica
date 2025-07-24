# Recycling Capacity Comparison
# PBH July 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")


url_drive <- "H:/.shortcut-targets-by-id/1CWiPbqLa53GMwIlw6QXl5kVdUX6nm-Sa/North America Battery Retirements and Recycling Capacity Research/Data/"

# in metric tons
cap <- read_excel(paste0(url_drive,"NA Recycling facilities.xlsx"),
                         sheet="US and CA cleaned data",range="P2:R8")
# expand to 2050
cap_2030 <- cap %>% filter(Year==2030)
for (i in 2031:2050){
  aux <- cap_2030 %>% mutate(Year=i)
  cap <- rbind(cap,aux)
}
rm(aux,cap_2030)

cap <- cap %>% 
  pivot_longer(c(-Year), names_to = "Stage", values_to = "tons") %>% 
  mutate(Stage=str_remove(Stage," Capacity")) %>% 
  mutate(type="Capacity")
  
# Recycling outflows ----
rec <- read.csv("Results/Base.csv")

rec <- rec %>% 
  filter(FlowType=="LIB Recycling") %>% 
  group_by(Year) %>% 
  reframe(`Pre-processing`=sum(battery_kg)/1e3,
          Refining=sum(blackMass_kg)/1e3) %>% ungroup() %>% 
  pivot_longer(c(-Year), names_to = "Stage", values_to = "tons") %>% 
  mutate(type="Feedstock")

  
# Join them
df <- rbind(cap,rec) %>% 
  mutate(ktons=tons/1e3)

# Figure
df %>% 
  ggplot(aes(Year,ktons,fill=type))+
  geom_col(position = "dodge")+
  facet_wrap(~Stage)+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels = scales::label_comma())+
  scale_x_continuous(breaks = seq(2025,2050,5))+
  scale_fill_manual(values = c("Capacity" = "#1F77B4", "Feedstock" = "#D62728"))+
  guides(fill = guide_legend(ncol = 2))+
  labs(x="",y="",title="Thousand Metric tons",fill="")+
  theme(legend.position = c(0.8,0.4),
        panel.spacing.x = unit(1, "cm"))
  
ggsave("Figures/CapacityComparison.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)


# EoL