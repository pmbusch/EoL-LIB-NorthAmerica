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
  
cap <- cap %>% mutate(ktons=tons/1e3)

# Recycling outflows ----
(runs <- list.files("Results/Feedstock/",recursive = F))

# Read all results and put them in the same dataframe!
df_all <- do.call(rbind, lapply(runs, function(i) 
  transform(read.csv(file.path("Results/Feedstock", i)), 
            Scenario = i)))
unique(df_all$Scenario)

# Extract scens
df_all <- df_all %>% 
  mutate(Sales=str_extract(Scenario,"Baseline|Momentum"),
         Size=str_extract(Scenario,"Small|Large"),
         Lifetime=str_extract(Scenario,"Short|Long"),
         eol=str_extract(Scenario,"Repurposing|Recycling"),
         Reuse=str_extract(Scenario,"reuse..") %>% str_replace("reuse0.","reuse0"))
df_all <- df_all %>% 
  mutate(Scenario=str_remove(Scenario,"\\.csv"),
         Size=if_else(is.na(Size),"Reference",Size),
         Lifetime=if_else(is.na(Lifetime),"Reference",Lifetime),
         eol=if_else(is.na(eol),"Reference",eol))
unique(df_all$Sales)
unique(df_all$Size)
unique(df_all$Lifetime)
unique(df_all$eol)
unique(df_all$Reuse)

df_all <- df_all %>% 
  group_by(Scenario,Country,Sales,Size,Lifetime,eol,Reuse,Year) %>%
  # to ktons
  reframe(`Pre-processing`=sum(battery_kg)/1e6,
          Refining=sum(blackMass_kg)/1e6) %>% ungroup() %>% 
  pivot_longer(c(`Pre-processing`,Refining), names_to = "Stage", values_to = "ktons") %>% 
  mutate(type="Feedstock")


# Pick key scenarios - with their name
scens_selected <- c("Reference"="Momentum__reuse0",
                    "Lower EV Sales"="Baseline__reuse0", # Sales
                    "Large LIB"="Momentum__reuse0Large","Small LIB"="Momentum__reuse0Small", # LIB Size
                    "Long life"="Momentum_Long_reuse0","Short life"="Momentum_Short_reuse0", # Lifetime
                    "Recycling"="Momentum__reuse0Recycling","Repurposing"="Momentum__reuse0Repurposing", # Eol
                    "50% reuse"="Momentum__reuse50") # reuse


df_scen <- df_all %>% 
  filter(Stage=="Pre-processing") %>% 
  filter(Scenario %in% scens_selected) %>% 
  group_by(Scenario,Sales,Size,Lifetime,eol,Reuse,Year) %>%
  reframe(ktons=sum(ktons)) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,scen_name=names(scens_selected)))
  
# Figure
year_limit <- 2035
year_limit <- 2050
year_limit <- 2040

data_fig <- df_scen %>% 
  filter(Year<=year_limit)
max_v <- max(data_fig$ktons)

ggplot(data_fig,aes(Year,ktons))+
  geom_col(data=filter(cap,Year<=year_limit,Stage=="Pre-processing"),fill="darkred",alpha=0.8)+
  geom_line(aes(group=Scenario,col=Scenario),linewidth=0.5)+
  geom_text_repel(data = . %>% group_by(Scenario) %>% slice_max(Year), 
                  aes(label = scen_name,col=Scenario),direction = "y", 
                  segment.size=0.1,
                  hjust = -0.1,size=6*5/14 * 0.8)+
  annotate("text",x=2038,y=1000,label="North America Recycling Capacity",
           col="darkred",size=8*5/14 * 0.8)+
  # facet_wrap(~Stage)+
  coord_cartesian(expand = F,xlim = c(2025,year_limit+2),ylim=c(0,max_v*1.05))+
  scale_y_continuous(labels = scales::label_comma())+
  scale_x_continuous(breaks = seq(2025,2050,5))+
  scale_color_manual(values = c("Momentum__reuse0"="black",
                               "Baseline__reuse0"="#E31A1C",
                               "Momentum__reuse0Small"="#A6CEE3","Momentum__reuse0Large"="#1F78B4",
                               "Momentum_Long_reuse0"="#B2DF8A","Momentum_Short_reuse0"="#33A02C",
                               "Momentum__reuse0Recycling"="#FDBF6F","Momentum__reuse0Repurposing"="#FF7F00",
                               "Momentum__reuse30"="#CAB2D6","Momentum__reuse60"="#6A3D9A"))+
  # scale_fill_manual(values = c("Capacity" = "#1F77B4", "Feedstock" = "#D62728"))+
  guides(fill = guide_legend(ncol = 2))+
  labs(x="",y="",title="Batttery outflows and recycling capacity (thousand metric tons)",fill="")+
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"))
  
ggsave("Figures/CapacityComparison.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

# 2nd figure by country


# Heatmap figure ----------

year_limit=2035
# join to get deficit towards 2040
data_fig <- df_all %>%
  filter(Stage=="Pre-processing") %>% 
  group_by(Scenario,Sales,Size,Lifetime,eol,Reuse,Year) %>%
  reframe(ktons=sum(ktons)) %>% ungroup() %>% 
  mutate(type=NULL) %>% 
  filter(Year==year_limit) %>% 
  left_join(filter(cap,Year==year_limit) %>% rename(cap=ktons)) %>% 
  mutate(deficit=ktons-cap)
  
# categorical levels
data_fig <- data_fig %>% 
  mutate(Reuse=paste0(str_remove(Reuse,"reuse"),"%")) %>% 
  mutate(Lifetime=factor(paste0(Lifetime,if_else(Lifetime=="Reference",""," lifetime")),
                         levels = c("Short lifetime","Reference","Long lifetime"))) %>% 
  mutate(Size=factor(paste0(Size,if_else(Size=="Reference",""," LIB")),
                         levels = c("Small LIB","Reference","Large LIB"))) %>% 
  mutate(eol=factor(eol,levels=rev(c("Reference","Repurposing","Recycling"))))


# scale transformation
data_fig$deficit_trans <- sign(data_fig$deficit) * sqrt(abs(data_fig$deficit))
breaks_orig <- pretty(data_fig$deficit, n = 5)
breaks_sqrt <- sign(breaks_orig) * sqrt(abs(breaks_orig))

range_deficit <- range(data_fig$deficit)
# round to 100
range_deficit <- sign(range_deficit) * ceiling(abs(range_deficit) / 100) * 100

library(cowplot)
p1 <- 
  ggplot(filter(data_fig, Sales == "Baseline"), 
             aes(x = Reuse, y = eol, fill = deficit)) +
  geom_tile(color = "grey80") +
  facet_grid(Size ~ Lifetime) +
  # scale_fill_gradient2(low = scales::alpha("#20215c", 0.9),
  #                      mid = "white",
  #                      high = scales::alpha("#570e12", 0.9),
  #                      labels = scales::label_comma(),
  #                      limits=range_deficit,
  #                      breaks = seq(range_deficit[1],range_deficit[2], by = 500),
  #                      # breaks = breaks_sqrt, # scale transformation
  #                      # labels = scales::label_comma()(breaks_orig),
  #                      midpoint = 0) + 
  scico::scale_fill_scico(palette = "vik",midpoint = 0,
                         limits=range_deficit,
                         breaks = seq(range_deficit[1],range_deficit[2], by = 500))+
  labs(x="Reuse %",y="End-of-Life Scenario",
       fill=paste0(year_limit," Recycling Capacity Deficit (ktons of battery)")) +
  theme_minimal(base_size = 9)+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9),
        panel.spacing.x= unit(0.1, "lines"),
        panel.spacing.y = unit(0.2, "lines"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 7,angle = 90,vjust=0),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,barwidth = unit(0.7, "npc")))
p1

p2 <- p1 %+% filter(data_fig, Sales == "Momentum")+
  theme(legend.position = "none",plot.margin = margin(5, 5, 5, -15))+labs(y="")
p2

# Combine plots
# Extract legend from one plot
legend_shared <- get_plot_component(p1,"guide-box-bottom",return_all = T)
p1 <- p1+theme(legend.position = "none",plot.margin = margin(5, 2, 5, 5))

# Centered title
p1_with_title <- ggdraw() +
  draw_label("Baseline Sales", size = 11, x = 0.6, y = 1, hjust = 0.5, vjust = 1) +
  draw_plot(p1, y = 0, height = 1)

p2_with_title <- ggdraw() +
  draw_label("Momentum Sales", size = 11, x = 0.6, y = 1, hjust = 0.5, vjust = 1) +
  draw_plot(p2, y = 0, height = 1)


plot_grid(
  plot_grid(p1_with_title, p2_with_title, ncol = 2, align = "hv", labels = NULL),
  legend_shared,ncol = 1, rel_heights = c(0.8,0.2))

ggsave("Figures/Fig4.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=18,height=8.7)


# EoL