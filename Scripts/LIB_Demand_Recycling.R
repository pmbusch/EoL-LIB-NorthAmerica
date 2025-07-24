# Main Module: Estimates battery (LIB) requirements and recycling outflows
# Inputs pre-processed in other scripts

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-ModelParameters.R")

# Load Inputs -----

## Electric Vehicles --------

# Stocks of EVs and LIBs
stock <- read.csv("Inputs/Stocks/Base.csv",stringsAsFactors = FALSE)
# Convert strings to vectors by year - Process the list column back into a list
stock <- stock %>%
  mutate(LIB_recycling_vector = str_split(LIB_recycling_vector,"\\|") %>% lapply(as.numeric),
         LIB_Available_vector = str_split(LIB_Available_vector,"\\|") %>% lapply(as.numeric),
         add_LIB_vector = str_split(add_LIB_vector,"\\|") %>% lapply(as.numeric),
         EV_Stock_vector = str_split(EV_Stock_vector,"\\|") %>% lapply(as.numeric))
stock <- stock %>% filter(Year>2024)


# Battery Size
bat <- read.csv("Inputs/BatterySize.csv")
bat <- bat %>% mutate(Country=str_replace(Country,"USA","United States"))

# no chemistry - FOR NOW
bat <- bat %>% 
  group_by(Vehicle,Year,Country) %>% reframe(kwh_veh=sum(kwh_veh)) %>% ungroup()


## Stationary Storage ------
ss <- read.csv("Inputs/StationaryStorage.csv")


## Consumer Electronics -----
ce <- read.csv("Inputs/consumerElectronics.csv")

# data format
ce <- ce %>% 
  mutate(Vehicle="Consumer Electronics") %>%
  pivot_longer(c(ce_gwh,ce_gwh_recyc), 
               names_to = "Flow", values_to = "kwh") %>% 
  mutate(kwh=kwh*1e6,
         Flow=if_else(Flow=="ce_gwh","EVSales","LIB_recycling"))
  

# Calculations -----

# Note: Vector dot-product allows to estimate battery size based on age fleet with more speed
# Each vector is of size 30 (0 to 30 years)

## EVs and LIBs -----

# Additional column vector by age 
bat <- bat %>%
  group_by(Vehicle,Country) %>%
  arrange(Vehicle,Country, desc(Year)) %>%
  mutate(kwh_veh_vector = map(Year, ~{
    vals <- kwh_veh[Year <= .x]
    vals <- head(vals, 30)
    length(vals) <- 30
    replace_na(vals, 0)
  })) %>% 
  filter(Year>2024)


df <- stock %>% 
  left_join(bat)

# Estimate kWh associated with new sales, replacement, recycling, and availability
# Note that LIB for replacement, recycling, ... depend on the age of each vehicle (year it was introduced to the market), so we are using vector by age multiplication
df <- df %>% 
  mutate(kWh_EVSales=Sales*kwh_veh) %>% 
  # dot-product
  rowwise() %>%
  mutate(kWh_addLIB = sum(as.numeric(add_LIB_vector) * as.numeric(kwh_veh_vector)),
         kWh_LIB_recycling = sum(as.numeric(LIB_recycling_vector) * as.numeric(kwh_veh_vector)),
         kWh_LIB_available = sum(as.numeric(LIB_Available_vector) * as.numeric(kwh_veh_vector)),
         # vector for repurpose age
         kWh_available=list(as.numeric(LIB_Available_vector) * as.numeric(kwh_veh_vector))) %>%
  ungroup() %>% 
  dplyr::select(-add_LIB_vector,-LIB_Available_vector,-LIB_recycling_vector,-kwh_veh_vector,-EV_Stock_vector)

# LIB vector for repurposing - to long format
ss_vector <- df %>% 
  dplyr::select(Vehicle,Country,Year,kWh_available) %>% 
  unnest_longer(kWh_available, indices_to = "age")  %>% 
  group_by(Country,Year,age) %>% reframe(kwh=sum(kWh_available)) %>% ungroup() %>% 
  filter(kwh>0)

# care only for flows of kWh
df <- df %>% 
  dplyr::select(Vehicle,Country,Year,kWh_EVSales,kWh_addLIB,kWh_LIB_recycling,kWh_LIB_available) %>%
  pivot_longer(c(-Vehicle,-Country,-Year), names_to = "Flow", values_to = "kwh") %>% 
  mutate(Flow=str_remove(Flow,"kWh_"))


## Stationary -----

## Substract batteries for repurposing
lib_repurp <- df %>% 
  filter(str_detect(Flow,"available")) %>% 
  mutate(kwh=kwh*p.share_repurpose,
         Flow=NULL) %>% 
  group_by(Country,Year) %>% reframe(kwh_repurp=sum(kwh)) %>% ungroup()

# are they the same? YES
lib_repurp
ss_vector %>% mutate(kwh=kwh*p.share_repurpose) %>% 
  group_by(Country,Year) %>% reframe(kwh=sum(kwh)) %>% ungroup()

# Consider degradation for LIB repurpose
ss_vector <- ss_vector %>% 
  mutate(kwh=kwh*p.share_repurpose) %>% 
  mutate(kwh_useful=kwh*(1-p.degradation*age))

lib_repurp <- ss_vector %>% 
  group_by(Country,Year) %>% 
  reframe(kwh_repurp=sum(kwh),kwh_useful=sum(kwh_useful)) %>% ungroup()

# Join and check Max repurpose - depending on LIB grid storage demand
ss <- ss %>% 
  filter(Year>2024) %>% 
  mutate(kwh=gwh*1e6) %>% 
  left_join(lib_repurp) %>% 
  mutate(allocate=pmin(kwh,kwh_useful)) %>% 
  mutate(ratio=allocate/kwh_useful, # physical vs useful (with degradation)
         kwh=kwh-allocate,
         kwh_repurp=kwh_repurp*(1-ratio),
         allocate=NULL,kwh_useful=NULL)

# Ratio of physical vs useful
ratios <- ss %>% dplyr::select(Country,Year,ratio)
ss$ratio <- NULL


# additional batteries back to recycling
lib_repurp <- df %>% 
  filter(str_detect(Flow,"available")) %>% 
  mutate(kwh=kwh*p.share_repurpose,
         Flow=NULL) %>% 
  group_by(Vehicle,Country,Year) %>% reframe(kwh_repurp=sum(kwh)) %>% ungroup()

# get total back
lib_repurp_back <- ss %>% mutate(kwh=NULL,gwh=NULL) %>% 
  filter(kwh_repurp>0) %>% rename(kwh=kwh_repurp)

# ratio back - distribute equally among vehicles TYPES
lib_repurp_back <- lib_repurp %>% 
  group_by(Country,Year) %>% reframe(kwh_repurp=sum(kwh_repurp)) %>% ungroup() %>% 
  left_join(lib_repurp_back) %>% 
  mutate(kwh=if_else(is.na(kwh),0,kwh)) %>% 
  mutate(back=kwh/kwh_repurp)

lib_repurp <- lib_repurp %>% 
  left_join(dplyr::select(lib_repurp_back,Country,Year,back)) %>% 
  mutate(kwh=kwh_repurp*back,back=NULL,kwh_repurp=NULL)

# rest can go to recycling
lib_recycling <- df %>%
  filter(str_detect(Flow,"available|recycling")) %>% 
  mutate(kwh=if_else(str_detect(Flow,"available"),
                     kwh*(1-p.share_repurpose),
                     kwh)) %>% 
  group_by(Vehicle,Country,Year) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup() %>% 
  rbind(lib_repurp) %>% 
  group_by(Vehicle,Country,Year) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup() %>% 
  mutate(Flow="LIB_recycling")
lib_recycling

# add to resutls
df <- df %>% 
  filter(!str_detect(Flow,"available|recycling")) %>% 
  rbind(lib_recycling)
  
# format data - LIBs required due to initial installation
ss <- ss %>% mutate(kwh_repurp=NULL)
ss_prod <- ss %>% 
  mutate(Vehicle="Stationary Storage",Flow="EVSales",gwh=NULL)

## SS Extra LIBs -----
# get extra batteries due to degradation and failures (outflows)

# first add batteries coming from EVs (repurpose)
ss <- ss %>% mutate(gwh=NULL,age=1)

# repurposed batteries with age - Added as phyisical kWh, easier to account for recycling
ss_addition <- ss_vector %>% 
  left_join(ratios) %>% 
  mutate(kwh=kwh*ratio,ratio=NULL,kwh_useful=NULL)
  
ss <- rbind(ss,ss_addition)

# add extra battery reqs. based on degradation
head(ss)
ss_deg <- ss %>%
  cross_join(tibble(offset=1:30,d=p.degradation)) %>% 
  filter(age<=offset) %>% # batteries from repurpose continue to degrade
  mutate(Year=Year+offset-1,
         kwh=kwh*d) %>% 
  group_by(Country,Year) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup() %>% 
  arrange(Country,Year) %>% 
  filter(Year<2051)

# add extra battery installations each year to account for degradation
ss <- ss %>% 
  rbind(mutate(ss_deg,age=1)) %>% # INSTALLED AT YEAR ZERO 
  group_by(Country,Year,age) %>% reframe(kwh=sum(kwh)) %>% ungroup()

# failure of batteries in SS
survival_curve <- dlogis(1:30, p.mean_lib, p.sd_lib*sqrt(3)/pi)
sum(survival_curve)
plot(1:30,survival_curve) # like a normal curve (Bell shape)

# get LIB outflows based on survival curve (age 0)
recyc <- ss %>%
  cross_join(tibble(offset=1:30,p=survival_curve)) %>% 
  filter(age<=offset) %>% # account for older batteries from repurposing
  mutate(Year=Year+offset,
         kwh=kwh*p) %>% 
  group_by(Country,Year) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup() %>% 
  arrange(Country,Year) %>% 
  filter(Year<2051) %>% 
  mutate(Flow="LIB_recycling",
         Vehicle="Stationary Storage")

head(ss_prod)
head(recyc)
ss_deg <- ss_deg %>% mutate(Vehicle="Stationary Storage",Flow="deg")

ss <- rbind(ss_prod,recyc,ss_deg,
            mutate(recyc,Flow="addLIB")) # batteries that failed are added as new batteries

# MAYBE a loop is required in this part, as new batteries can also fail....

# Prod. Scrap -----
# add all sectors together
df <- rbind(df,ce) %>% rbind(ss)

df <- df %>% 
  mutate(FlowType=if_else(Flow=="LIB_recycling","LIB Recycling","LIB Production"))

prod <- df %>% 
  filter(str_detect(FlowType,"Production")) %>% 
  group_by(Vehicle,Country,Year) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup()

# Check with current NA capacity
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


write.csv(df,"Results/Base.csv",row.names = F)

# Figure ----

# rename flow
levels_flow <- c("New EV sales","Additional LIB\n(Replacement)",
                 "Additional LIB\n(Degradation)",
                 "LIB Available\n(repurpose or recyling)","LIB Recycling","LIB Scrap")
df <- df %>% 
  mutate(flow_label=case_when(
    Flow=="EVSales" ~ levels_flow[1],
    Flow=="addLIB" ~ levels_flow[2],
    Flow=="deg" ~ levels_flow[3],
    Flow=="LIB_available" ~ levels_flow[4],
    Flow=="LIB_recycling" ~ levels_flow[5],
    Flow=="LIB_scrap" ~ levels_flow[6],
    T ~ "NA") %>% factor(levels=levels_flow))

df %>% 
  mutate(gwh=kwh/1e6) %>% 
  ggplot(aes(Year,gwh,fill=Vehicle))+
  geom_col(position="stack",col="black",linewidth=0.1)+
  facet_grid(Country~flow_label,scales="free_y") +
  labs(x="",y="",title="GWh of Batteries")


ggsave("Figures/flows_EV.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7*1.5)

# EoF