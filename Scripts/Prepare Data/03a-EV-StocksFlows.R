# Estimate future EV stock and flows using the surival curves
# Estimate EV stock, additional LIB requirements, LIBs for recycling and LIBs for availability

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Function
source("Scripts/Functions/StockFlows_EV_LIB.R")

# Sales data, with historical data
sales <- read.csv("Inputs/EV_Sales.csv")


# Lifetime scenarios
# blank is for reference
lifetime_scen <- c("","Short","Long")
p.life.orig=p.mean_lib
p.life.orig.large=p.mean_lib_large

# reuse rates
reuse_scens <- seq(0,0.5,0.1)

# Loop for scenarios - Each One is different file
# Runtime: less than 1 minute per scenario loop
# Estimate for 2x3x6 ~ 40 minutes
# Debug
# s="Baseline"
# s="Momentum"
# lif=""
# re=0
for (s in unique(sales$Scenario)){
  cat("\n ",s,": ")
  for(lif in lifetime_scen){
    cat(" ",lif,",")
    for (re in reuse_scens){
      
      sales_scen <- sales %>% filter(Scenario==s)
      

      
      # iterate over each country/state and vehicle type
      stock=c()
      for (st in unique(sales_scen$Country)){
        cat("\n",st,": ")
        for (v in unique(sales_scen$Vehicle)){
          cat(v,", ")
          
          # LIB lifetime
          if(lif=="Short"){
            p.mean_lib= if(v %in% large_veh) 5 else 10 #  years
          }
          else if(lif=="Long"){
            p.mean_lib=if(v %in% large_veh) 11 else 20 # years
          } else {
            p.mean_lib=if(v %in% large_veh) p.life.orig.large else p.life.orig # back to original
          }
          
          
          sales_aux <- sales_scen %>% filter(Vehicle==v,Country==st)
          stock_iter <- f.LIB_EV_Stock(sales_aux,
                                       ev_age_newLib=p.ev_age_newLib,
                                       max_reuse_lib=re,
                                       max_ev_age_replacement=p.max_ev_age_replacement, 
                                       max_lib_age_secondlife=p.max_lib_age_secondlife, 
                                       start_year=2015)
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
      write.csv(stock,paste0("Inputs/Stocks/",s,"_",lif,
                             "_reuse",round(re*100,0),".csv"),row.names = F)
      
      rm(stock,sales_scen)
      
    }
  }  
}



# EoF