# Estimate future EV stock and flows using the surival curves
# Estimate EV stock, additional LIB requirements, LIBs for recycling and LIBs for availability


source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Function
source("Scripts/Functions/StockFlows_EV_LIB.R")


# Sales data, with historical data


sales <- read.csv("Inputs/EV_Sales.csv")

# Loop for scenarios - Each One is different file

for (s in unique(sales$Scenario)){
  
  sales_scen <- sales %>% filter(Scenario==s)
  
  # iterate over each country/state and vehicle type
  stock=c()
  for (st in unique(sales_scen$Country)){
    cat("\n",st,": ")
    for (v in unique(sales_scen$Vehicle)){
      cat(v,", ")
      sales_aux <- sales_scen %>% filter(Vehicle==v,Country==st)
      stock_iter <- f.LIB_EV_Stock(sales_aux,
                                 ev_age_newLib=p.ev_age_newLib,
                                 max_reuse_lib=p.max_reuse_lib,
                                 max_ev_age_replacement=p.max_ev_age_replacement, 
                                 max_lib_age_secondlife=p.max_lib_age_secondlife, 
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
  write.csv(stock,paste0("Inputs/Stocks/",s,".csv"),row.names = F)
  
}



# EoF