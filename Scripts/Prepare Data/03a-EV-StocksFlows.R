# Estimate future EV stock and flows using the surival curves
# Estimate EV stock, additional LIB requirements, LIBs for recycling and LIBs for availability


source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Function
source("Scripts/Functions/StockFlows_EV_LIB.R")


# Sales data, with historical data


sales <- read.csv("Inputs/EV_Sales.csv")

# iterate over each country/state and vehicle type

stock=c()
for (st in unique(sales$Country)){
  cat("\n",st,": ")
  for (v in unique(sales$Vehicle)){
    cat(v,", ")
    sales_aux <- sales %>% filter(Vehicle==v,Country==st)
    stock_iter <- f.LIB_EV_Stock(sales_aux,
                               ev_age_newLib=8,
                               max_reuse_lib=0.5,
                               max_ev_age_replacement=16, 
                               max_lib_age_secondlife=12, 
                               start_year=2015 )
    stock <- rbind(stock,stock_iter)
    rm(stock_iter,sales_aux)
  }
}
rm(st,v)

# Save results
#  vector variables as strings
stock <- stock %>%
  rowwise() %>%
  mutate_if(is.list, ~paste(unlist(.), collapse = '|')) 
write.csv(stock,"Inputs/Stocks/Base.csv",row.names = F)


# EoF