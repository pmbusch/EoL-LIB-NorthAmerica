# Script to run scenarios 
# Scenarios for LIB size and for EoL Strategy
# Note that Scenarios for lifetime and sales are already runned in the EV stock model
# PBH July 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-ModelParameters.R")

# Consumer Electronics
# No scenario for CE
ce <- read.csv("Inputs/consumerElectronics.csv")

# data format
ce <- ce %>% 
  mutate(Vehicle="Consumer Electronics") %>%
  pivot_longer(c(ce_gwh,ce_gwh_recyc), 
               names_to = "Flow", values_to = "kwh") %>% 
  mutate(kwh=kwh*1e6,
         Flow=if_else(Flow=="ce_gwh","EVSales","LIB_recycling"))

# second hand trade
trade <- read.csv("Inputs/EV_2handTrade.csv")

# cathode weight
params <- read.csv("Inputs/battery_weight.csv")
# Add cathode weight for exports..
params <- rbind(params,
                mutate(filter(params,Country=="United States"),Country="Exports"))


# Current LIB production capacity
cap_prod <- rbind(
  tibble(Country="United States",
         Year=2025:2050,
         cap_ghw=c(1193,1324,1477,1826,1826,1829,rep(1829,20))),
  tibble(Country="Mexico",
         Year=2025:2050,
         cap_ghw=c(9.44,9.44,rep(29.49,24))))
  

# Iterate over
lib_scens <- c("","Small","Large")
eol_scens <- c("","Repurposing","Recycling")
trade_scens <- c("","NoTrade")
p.share.orig <- p.share_repurpose

p.mean_lib.orig <- p.mean_lib
p.mean_lib_large.orig <- p.mean_lib_large

# Need to iterate over all files of stock - each one is a scenario of lifetime and sales
(runs <- list.files("Inputs/Stocks",recursive = F))

# TO DEBUG
# b=""
# e=""
# tra=""
# i="Momentum__reuse0.csv"

for (i in runs){
  cat("\n",i)
  # Batteries
  for (b in lib_scens){
    cat("\n  ",b,"   ")
    # End of Life
    for (e in eol_scens){
      cat("",e,"-")
      
      for (tra in trade_scens){
        
        considerTrade <- if(tra=="NoTrade") F else T
        
        # re-read inputs in each iteration
        stock <- read.csv(paste0("Inputs/Stocks/",i),stringsAsFactors = FALSE)
        
        if (b=="Small"){
          bat <- read.csv("Inputs/BatterySize_small.csv")
        } else if (b=="Large") {
          bat <- read.csv("Inputs/BatterySize_large.csv")
        } else {
          bat <- read.csv("Inputs/BatterySize.csv")
        }
        
        if (e=="Repurposing"){
          p.share_repurpose <- 0.8
        } else if (e=="Recycling"){
          p.share_repurpose <- 0
        } else {
          p.share_repurpose <- p.share.orig # original share
        }
        
        # LIB mean lifetime in EVs
        if (str_detect(i,"Short")){
          p.mean_lib <- 10
          p.mean_lib_large <- 5
        } else if (str_detect(i,"Long")) {
          p.mean_lib <- 20
          p.mean_lib_large <- 11
        } else {
          p.mean_lib <- p.mean_lib.orig
          p.mean_lib_large <- p.mean_lib_large.orig
        }
        
        
        # Run EV script
        source("Scripts/02-EV_LIB_Demand_Recycling.R")
        
        # Run SS script
        source("Scripts/03-StationaryStorageStock.R")
        
        # Variables created in the scripts above
        # ev <- read.csv("Results/Intermediate/EV.csv") # Electric Vehicles
        # ss <- read.csv("Results/Intermediate/ss.csv") # Stationary Storage
        
        # Join
        df <- rbind(ev,ss,ce) %>% 
          group_by(Vehicle,Country,Year,Flow) %>% 
          reframe(kwh=sum(kwh)) %>% ungroup()
        
        # Production Scrap
        unique(df$Flow)
        df <- df %>% 
          mutate(FlowType=if_else(Flow=="LIB_recycling","LIB Recycling","LIB Production"))
        
        prod <- df %>% 
          filter(str_detect(FlowType,"Production")) %>% 
          group_by(Vehicle,Country,Year) %>% 
          reframe(kwh=sum(kwh)) %>% ungroup()
        
        # Check with current North America capacity
        # ratio cap/prod
        prod_country <- prod %>% group_by(Year,Country) %>% 
          reframe(gwh=sum(kwh)/1e6) %>% ungroup() %>% 
          left_join(cap_prod) %>% 
          mutate(cap_ghw=if_else(is.na(cap_ghw),0,cap_ghw)) %>% 
          mutate(ratio_cap=cap_ghw/gwh)
        
        # save ratio for later, for now assumed production 100% in NA
        prod <- prod %>% 
          left_join(dplyr::select(prod_country,Year,Country,ratio_cap)) %>% 
          mutate(kwh=kwh*p.scrap) %>% 
          mutate(Flow="LIB_scrap") %>% 
          mutate(FlowType="LIB Recycling")
        
        df <- rbind(mutate(df,ratio_cap=1),prod)
        
        
        # to Black Mass -----
        
        # black mass
        df <- df %>% 
          left_join(params) %>% 
          mutate(blackMass_kg=kwh/energy_dens*cathode_we_frac,
                 battery_kg=kwh/energy_dens,
                 energy_dens=NULL,cathode_we_frac=NULL)
        
        # Save results
        # production results
        prod_save <- df %>% filter(FlowType=="LIB Production") %>% mutate(FlowType=NULL,blackMass_kg=NULL)
        name_aux <- str_remove(i,"\\.csv")
        write.csv(prod_save,paste0("Results/Production/",name_aux,b,e,tra,".csv"),row.names = F)
        # recycling results
        df <- df %>% filter(FlowType=="LIB Recycling") %>% mutate(FlowType=NULL)
        write.csv(df,paste0("Results/Feedstock/",name_aux,b,e,tra,".csv"),row.names = F)
      }
    }
  }
}



# EoF