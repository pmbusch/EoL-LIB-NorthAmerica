# Estimates battery (LIB) requirements for EVs and recycling outflows
# Inputs pre-processed in other scripts

source("Scripts/00-Libraries.R", encoding = "UTF-8")
# source("Scripts/01-ModelParameters.R") # uncomment to debug

# Load Inputs -----

## Electric Vehicles --------

# Stocks of EVs and LIBs

# Uncomment to debug only
#stock <- read.csv("Inputs/Stocks/Momentum.csv",stringsAsFactors = FALSE)

# Convert strings to vectors by year - Process the list column back into a list
stock <- stock %>%
  mutate(LIB_recycling_vector = str_split(LIB_recycling_vector,"\\|") %>% lapply(as.numeric),
         LIB_Available_vector = str_split(LIB_Available_vector,"\\|") %>% lapply(as.numeric),
         add_LIB_vector = str_split(add_LIB_vector,"\\|") %>% lapply(as.numeric),
         EV_Stock_vector = str_split(EV_Stock_vector,"\\|") %>% lapply(as.numeric))
stock <- stock %>% filter(Year>2024)


# Battery Size
# UNCOMMENT TO DEBUG
# bat <- read.csv("Inputs/BatterySize.csv")


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


ev <- stock %>% 
  left_join(bat)

# Estimate kWh associated with new sales, replacement, recycling, and availability
# Note that LIB for replacement, recycling, ... depend on the age of each vehicle (year it was introduced to the market), so we are using vector by age multiplication
ev <- ev %>% 
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
ss_vector <- ev %>% 
  dplyr::select(Vehicle,Country,Year,kWh_available) %>% 
  unnest_longer(kWh_available, indices_to = "age")  %>% 
  group_by(Country,Year,age) %>% reframe(kwh=sum(kWh_available)) %>% ungroup() %>% 
  filter(kwh>0)

# save to use in SS file
# UNCOMMENT TO DEBUG
# write.csv(ss_vector,"Results/Intermediate/LIB_repurpose_available.csv",row.names = F)

# care only for flows of kWh
ev <- ev %>% 
  dplyr::select(Vehicle,Country,Year,kWh_EVSales,kWh_addLIB,kWh_LIB_recycling,kWh_LIB_available) %>%
  pivot_longer(c(-Vehicle,-Country,-Year), names_to = "Flow", values_to = "kwh") %>% 
  mutate(Flow=str_remove(Flow,"kWh_"))

# Estimate recycling
lib_recycling <- ev %>%
  filter(str_detect(Flow,"available|recycling")) %>% 
  mutate(kwh=if_else(str_detect(Flow,"available"),
                     kwh*(1-p.share_repurpose),
                     kwh)) %>% 
  group_by(Vehicle,Country,Year) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup() %>% 
  mutate(Flow="LIB_recycling")
lib_recycling

# add to resutls
ev <- ev %>% 
  filter(!str_detect(Flow,"available|recycling")) %>% 
  rbind(lib_recycling)
  
# UNCOMMENT TO DEBUG
# write.csv(ev,"Results/Intermediate/EV.csv",row.names = F)

# EoF