# Stock of Stationary Storage
# PBH July 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-ModelParameters.R")

# Load data ----
# Demand
ss <- read.csv("Inputs/StationaryStorage.csv")
# LIBs suitable for repurp
ss_vector <- read.csv("Results/Intermediate/LIB_repurpose_available.csv")

# Calculations -----

# Consider share for LIB repurpose and degradation in EVs
ss_vector <- ss_vector %>% 
  mutate(kwh=kwh*p.share_repurpose) %>% 
  mutate(kwh_useful=kwh*(1-p.degradation*age))

## Max Age ----
# Max age to repurpose, rest to recycling
ss_recyc <- ss_vector %>% 
  filter(Year>2024) %>% 
  filter(age>p.max_lib_age_repurp) %>% 
  group_by(Country,Year) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup() %>% 
  mutate(Flow="LIB_recycling")

ss_vector <- ss_vector %>% filter(age<=p.max_lib_age_repurp)


# Main STOCK loop ----
ss <- ss %>% mutate(kwh=gwh*1e6,gwh=NULL) 
range(ss$Year)


# Get survival ratio from year to year
# get fraction year to year of survival, based on CDF ratios
# represent proportion that survives year to year
surv_ratio <- c()
for (a in 0:30){
  surv_ratio <- rbind(surv_ratio,
                      (1-plogis(a+1, p.mean_lib, p.sd_lib*sqrt(3)/pi))/
                        (1-plogis(a, p.mean_lib, p.sd_lib*sqrt(3)/pi)))
}
rm(a)
surv_ratio <- surv_ratio[,1]

out.recyc <- c()
out.fails <- c()
out.add <- c()
out.new <- c()
for (ct in unique(ss$Country)){
  cat(ct," \n")
  ss_ct <- ss %>% filter(Country==ct) %>% arrange(Year)
  repurp <- ss_vector %>% filter(Country==ct) %>% arrange(Year,age)
  
  # stock at zero - from 0 to 30 years
  stock <- matrix(0,nrow=31)
  stock <- stock[,1]
  
  for (y in 2015:2050){
    
    ss_year <- ss_ct %>% filter(Year==y) %>% pull(kwh)
    if (y>2024){ # repurpose after 2025
  
      repurp_year <- repurp %>% filter(Year==y) %>% 
        complete(age=1:12,fill=list(kwh=0,Year=y,Country=ct,kwh_useful=0))
    
      # Join and check Max repurpose - depending on LIB grid storage demand
      cum <- cumsum(repurp_year$kwh_useful)
      assigned <- ifelse(cum <= ss_year, repurp_year$kwh_useful, 
                         pmax(0, ss_year - c(0, head(cum, -1))))      
      
      
      # to recycling or stock
      repurp_year$assigned <- assigned
      repurp_year <- repurp_year %>% 
        mutate(ratio=if_else(kwh_useful>0,
                             assigned/kwh_useful,0), # physical vs useful (with degradation),
                 kwh_recyc=kwh*(1-ratio), # back to physical kWh
               assigned=NULL,kwh_useful=NULL,ratio=NULL)
      
      # unallocated to recycling
      out.recyc <- rbind(out.recyc,
                         tibble(Year=y,Country=ct,
                                kwh=sum(repurp_year$kwh_recyc),Flow="LIB_recycling"))
      
      # physical qty for stock
      repurp_stock <- repurp_year$kwh
      
    }
    else {
        assigned <- rep(0,12)
      }
    
    # reduce primary demand
    ss_year <- ss_year-sum(assigned)
    
    # assign new bat capacity to first row
    stock[1] <- stock[1]+ss_year
    
    if(y>2024){
      # add extra batteries of age 1 to 12 from repurposing
      stock[2:13] <- stock[2:13]+repurp_stock
      
    }
    
    # Get failure rates towards next year
    fails <- stock*(1-surv_ratio)
    new_stock <- stock*surv_ratio
    new_stock[-1] <- new_stock[1:(length(new_stock) - 1)]
    new_stock[1] <- 0
    
    # new batteries needed due to replacement
    new_lib <- sum(fails)
    # add stock of batteries
    new_stock[1] <- new_stock[1]+new_lib
    
    stock <- new_stock
    
    # Save results by year
    out.new <- rbind(out.new,tibble(Year=y,Country=ct,
                                    kwh=ss_year,Flow="EVSales"))
    out.fails <- rbind(out.fails,tibble(Year=y,Country=ct,
                                        kwh=new_lib,Flow="LIB_recycling"))
    out.add <- rbind(out.add,tibble(Year=y,Country=ct,
                                    kwh=new_lib,Flow="addLIB"))
    
  }
}

# joib all
df <- rbind(out.new,out.add,out.fails,out.recyc) %>% 
  mutate(Vehicle="Stationary Storage")

# ADD lib to repurp not used
df <- rbind(df,mutate(ss_recyc,Vehicle="Cars")) # all to EVs for now

df <- df %>% 
  filter(Year>2024) %>% 
  group_by(Vehicle,Country,Flow,Year) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup()


write.csv(df,"Results/Intermediate/ss.csv",row.names = F)

# EoF