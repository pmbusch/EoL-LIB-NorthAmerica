# Heatmap figure 
# Recycling Capacity Comparison
# PBH July 2025

# LOAD DATA -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-ModelParameters.R")

url_drive <- "H:/.shortcut-targets-by-id/1CWiPbqLa53GMwIlw6QXl5kVdUX6nm-Sa/North America Battery Retirements and Recycling Capacity Research/Data/"

## Capacity -----
# in metric tons
cap <- read_excel(paste0(url_drive,"NA Recycling facilities.xlsx"),
                  sheet="US and CA cleaned data",range="S10:V22")
# expand to 2050
cap_2030 <- cap %>% filter(Year==2030)
for (i in 2031:2050){
  aux <- cap_2030 %>% mutate(Year=i)
  cap <- rbind(cap,aux)
}
rm(aux,cap_2030)

cap <- cap %>% 
  pivot_longer(c(-Year,-Country), names_to = "Stage", values_to = "tons") %>% 
  mutate(Stage=str_remove(Stage," Capacity") %>% 
           str_replace("Refining","Refining (black mass)")) %>% 
  mutate(type="Capacity") %>% 
  mutate(Country=str_replace(Country,"US","United States"))

cap <- cap %>% mutate(ktons=tons/1e3)
cap_total <- cap %>% group_by(Stage,Year,type) %>% 
  reframe(ktons=sum(ktons)) %>% ungroup()

## Recycling outflows ----
(runs <- list.files("Results/Feedstock/",recursive = F))

# Read all results and put them in the same dataframe!
df_all <- do.call(rbind, lapply(runs, function(i) 
  transform(read.csv(file.path("Results/Feedstock", i)), 
            Scenario = i)))
unique(df_all$Scenario)

# Extract scens
df_all <- df_all %>% 
  mutate(Sales=str_extract(Scenario,"Baseline|Momentum|Ambitious"),
         Size=str_extract(Scenario,"Small|Large"),
         Lifetime=str_extract(Scenario,"Short|Long"),
         eol=str_extract(Scenario,"Repurposing|Recycling"),
         Reuse=str_extract(Scenario,"reuse..") %>% str_replace("reuse0.","reuse0"),
         Trade=str_extract(Scenario,"NoTrade"))
df_all <- df_all %>% 
  mutate(Scenario=str_remove(Scenario,"\\.csv"),
         Size=if_else(is.na(Size),"Reference",Size),
         Lifetime=if_else(is.na(Lifetime),"Reference",Lifetime),
         eol=if_else(is.na(eol),"Reference",eol),
         Trade=if_else(is.na(Trade),"Reference",Trade))
unique(df_all$Sales)
unique(df_all$Size)
unique(df_all$Lifetime)
unique(df_all$eol)
unique(df_all$Reuse)
unique(df_all$Trade)

df_all <- df_all %>% filter(Trade=="Reference")

# put reuse scenario with EoL
df_all <- df_all %>% 
  mutate(eol=if_else(eol=="Reference" & Reuse=="reuse50",
                     "50% Reuse",eol))
unique(df_all$eol)
# No reuse showing
df_all <- df_all %>% filter(Reuse=="reuse0"|eol=="50% Reuse")

# consider scrap and max production
df_all$Scrap <- ""
df_all$ScrapScen <- "All "
df_scrap_limited <- df_all %>% 
  mutate(ratio_cap=case_when(
    Flow!="LIB_scrap" ~ 1,
    ratio_cap>1 ~ 1,
    T ~ ratio_cap)) %>% 
  mutate(kwh=kwh*ratio_cap,
         battery_kg=battery_kg*ratio_cap,
         blackMass_kg=blackMass_kg*ratio_cap) %>% 
  mutate(ScrapScen="")

nrow(df_all)/1e6
scrap_scens <- seq(0.02,0.24,0.04) # 6 scenarios

# Multiple DF to include senarios
df_orig <- df_all
df_orig2 <- df_scrap_limited
for (s in scrap_scens){
  df_aux <- rbind(df_orig,df_orig2)
  df_aux <- df_aux %>% 
  # scale it according to original scrap rate used
  mutate(adj=if_else(Flow=="LIB_scrap",s/p.scrap,1)) %>% 
  mutate(kwh=kwh*adj,
         battery_kg=battery_kg*adj,
         blackMass_kg=blackMass_kg*adj,
         adj=NULL) %>% 
  mutate(Scrap=paste0(round(s*100,0),"%"))
  # JOIN
  df_all <- rbind(df_all,df_aux)
}
rm(s,df_aux)
df_all <- df_all %>% filter(Scrap!="")
nrow(df_all)/1e6

# remove ALL productions scenario
df_all <- df_all %>% filter(ScrapScen=="")

df_all <- df_all %>% 
  mutate(Scrap=paste0(ScrapScen,Scrap)) %>% 
  group_by(Scenario,Country,Sales,Size,Lifetime,eol,Reuse,Scrap,Year) %>%
  # to ktons
  reframe(`Pre-processing`=sum(battery_kg)/1e6,
          `Refining (black mass)`=sum(blackMass_kg)/1e6) %>% ungroup() %>% 
  pivot_longer(c(`Pre-processing`,`Refining (black mass)`), 
               names_to = "Stage", values_to = "ktons") %>% 
  mutate(type="Feedstock")


# Loop or debug
# year_limit=2035
for (year_limit in seq(2025,2050,5)){

  # join to get deficit towards target year
  data_fig <- df_all %>%
    filter(Stage=="Pre-processing") %>% 
    group_by(Scenario,Sales,Size,Lifetime,eol,Reuse,Scrap,Year) %>%
    reframe(ktons=sum(ktons)) %>% ungroup() %>% 
    mutate(type=NULL) %>% 
    filter(Year==year_limit) %>% 
    left_join(filter(cap_total,Year==year_limit) %>% rename(cap=ktons)) %>% 
    mutate(deficit=ktons-cap)
  
  # categorical levels
  data_fig <- data_fig %>% 
    mutate(Reuse=paste0(str_remove(Reuse,"reuse"),"%")) %>% 
    mutate(Lifetime=factor(paste0(Lifetime,if_else(Lifetime=="Reference",""," lifetime")),
                           levels = c("Short lifetime","Reference","Long lifetime"))) %>% 
    mutate(Size=factor(paste0(Size,if_else(Size=="Reference",""," LIB")),
                       levels = c("Small LIB","Reference","Large LIB"))) %>% 
    mutate(eol=factor(eol,levels=rev(c("Reference","Repurposing","Recycling","50% Reuse")))) %>% 
    mutate(Scrap=factor(Scrap,
                        levels=c("2%","6%","10%","14%","18%","22%",
                                 "All 2%","All 6%","All 10%","All 14%","All 18%","All 22%")))
  
  # scale transformation
  data_fig$deficit_trans <- sign(data_fig$deficit) * sqrt(abs(data_fig$deficit))
  breaks_orig <- pretty(data_fig$deficit, n = 5)
  breaks_sqrt <- sign(breaks_orig) * sqrt(abs(breaks_orig))
  
  # range_deficit <- range(data_fig$deficit_trans)
  range_deficit <- range(data_fig$deficit)
  # round to 100
  range_deficit <- sign(range_deficit) * ceiling(abs(range_deficit) / 100) * 100
  
  # labels
  data_fig <- data_fig %>% 
    mutate(lab_def=paste0(round(deficit/1e3,1),"M"))
  
  library(cowplot)
  data1 <- filter(data_fig, Sales == "Momentum",Stage=="Pre-processing")
  p1a <- ggplot(data1,aes(x = Scrap, y = eol, fill = deficit)) +
    geom_tile(color = "grey80") +
    # geom_text(aes(label=lab_def),size=6*5/14 * 0.8)+
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
    scico::scale_fill_scico(palette = "vik",
                            midpoint = 0,
                            limits=range_deficit,
                            breaks = seq(range_deficit[1],range_deficit[2], by = 500))+
    labs(x="Scrap %",y="End-of-Life Scenario",
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
  p1a
  
  p1b <- p1a %+% filter(data_fig, Sales == "Momentum",Stage=="Refining (black mass)")+
    theme(legend.position = "none",plot.margin = margin(5, 5, 5, -15))+labs(y="",x="")
  p1b
  
  p2a <- p1a %+% filter(data_fig, Sales == "Ambitious",Stage=="Pre-processing")+
    theme(legend.position = "none",plot.margin = margin(5, 2, 5, 5))
  p2a
  
  p2b <- p1a %+% filter(data_fig, Sales == "Ambitious",Stage=="Refining (black mass)")+
    theme(legend.position = "none",plot.margin = margin(5, 5, 5, -15))+labs(y="")
  p2b
  
  
  # Combine plots
  # Extract legend from one plot
  legend_shared <- get_plot_component(p1a,"guide-box-bottom",return_all = T)
  p1a <- p1a+theme(legend.position = "none",plot.margin = margin(5, 2, 5, 5))+labs(x="")
  
  # Centered title
  p1a_with_title <- ggdraw() +
    draw_label("Pre-processing & Momentum Sales", size = 11, x = 0.6, y = 1, hjust = 0.5, vjust = 1) +
    draw_plot(p1a, y = 0, height = 1)
  
  p1b_with_title <- ggdraw() +
    draw_label("Refining & Momentum Sales", size = 11, x = 0.6, y = 1, hjust = 0.5, vjust = 1) +
    draw_plot(p1b, y = 0, height = 1)
  
  p2a_with_title <- ggdraw() +
    draw_label("Pre-processing & Ambitious Sales", size = 11, x = 0.6, y = 1, hjust = 0.5, vjust = 1) +
    draw_plot(p2a, y = 0, height = 1)
  
  p2b_with_title <- ggdraw() +
    draw_label("Refining & Ambitious Sales", size = 11, x = 0.6, y = 1, hjust = 0.5, vjust = 1) +
    draw_plot(p2b, y = 0, height = 1)
  
  plot_grid(
    plot_grid(p1a_with_title,p1b_with_title, p2a_with_title,p2b_with_title, ncol = 2, align = "hv", labels = NULL),
    legend_shared,ncol = 1, rel_heights = c(0.85,0.15))
  
  ggsave(paste0("Figures/Fig4",year_limit,".png"), ggplot2::last_plot(),
         units="cm",dpi=600,width=18,height=8.7*2)

  }


# EoF