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



# Iterate over
lib_scens <- c("","Small","Large")
eol_scens <- c("","Repurposing","Recycling")
p.share.orig=p.share_repurpose

# Need to iterate over all files of stock - each one is a scenario of lifetime and sales
(runs <- list.files("Inputs/Stocks",recursive = F))

# TO DEBUG
# b=""
# e=""
# i="Momentum__reuse0.csv"

for (i in runs){
  cat("\n",i)
  # Batteries
  for (b in lib_scens){
    cat("\n  ",b,"   ")
    # End of Life
    for (e in eol_scens){
      cat("",e,"-")

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
      
      # Check with current NA capacity - TO DO
      prod <- prod %>% 
        mutate(kwh=kwh*p.scrap) %>% 
        mutate(Flow="LIB_scrap") %>% 
        mutate(FlowType="LIB Recycling")
      
      df <- rbind(df,prod)
      
            
      # to Black Mass -----
      params <- read.csv("Inputs/battery_weight.csv")
      
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
      write.csv(prod_save,paste0("Results/Production/",name_aux,b,e,".csv"),row.names = F)
      # recycling results
      df <- df %>% filter(FlowType=="LIB Recycling") %>% mutate(FlowType=NULL)
      write.csv(df,paste0("Results/Feedstock/",name_aux,b,e,".csv"),row.names = F)
    }
  }
}


# EoF