# Function to get stocks and outflows of EVs and LIBs using product-component failure method
# Inputs: Inflows (forecast of sales) and lifetime assumptions (scrappage curves)
# No trade between state, function works for single state (or regional aggregation)
## PBH July 2025


library(tidyverse)
source("Scripts/Functions/Survival_EV_LIB.R")

# Function to get stock and outflows for EVs and LIBs, based on parameters
# In Survival_EV_LIB the surival function is defined as logistic
f.LIB_EV_Stock <- function(
    inflows, # Sales vector, indexed from start year to 2050
    ev_age_newLib=8,# year were a new battery is needed, after that an old battery will be sufficient
    # 8 years assuming a warranty over this period
    max_reuse_lib=0.5,
    max_ev_age_replacement=16, # Max age when an EV gets a battery, either 2-hand or new
    max_lib_age_secondlife=12, # Max age of LIB to be used in an EV
    start_year=2015 # historical to build stock at 2025 
  ){
  
  
  # Survival part with matrices - vehicle age and battery age
  # For the same country - 30 years max life assumed
  matrix_data <- matrix(0, nrow = 31, ncol = 31)
  rownames(matrix_data) <-paste0("EV_",0:30) # ROWS are EV
  colnames(matrix_data) <- paste0("LIB_",0:30) # COLS are Battery
  
  # Loop through years
  x <- inflows
  x$Year %>% range()
  x$add_LIB <-x$LIB_Available <- x$LIB_recycling <- x$LIB_reuse_EV <- x$EV_Stock <- 0
  x$add_LIB_vector <-x$LIB_Available_vector <- x$LIB_recycling_vector <- x$EV_Stock_vector <- c()
  
  for (y in start_year:2050){
    
    # if (y==2043){break} # debug
    
    # Assign new sales to top left cuadrant (0,0)
    matrix_data[1, 1] <- x$Sales[y-start_year+1]
    
    # clear stock of 10 or less batteries or EVs
    matrix_data[matrix_data < 10] <- 0
    
    # Get new matrix of EV stock with ages, LIBs in good use 
    new_matrix <- matrix_ev <- matrix_lib <- matrix_both <- matrix(0, nrow = 31, ncol = 31)
    rownames(new_matrix) <-paste0("EV_",0:30) # ROWS are EV
    colnames(new_matrix) <- paste0("LIB_",0:30) # COLS are Battery
    
    
    for (i in 1:31) { # EV
      for (j in 1:31) { # LIB
        if (matrix_data[i, j] != 0) {
          result <- f.getOutflows(matrix_data[i, j],
                                  EV_age = i-1,LIB_age = j-1) # age is minus 1 for the index
          if (i!=31 & j!=31){ # to avoid border case
            new_matrix[i + 1, j + 1] <- result$none # move 1 age for both EV and LIB
            matrix_ev[i+1,j+1] <- result$lib_fail # EVs that need LIB
            matrix_lib[i+1,j+1] <- result$ev_fail # LIBs available to use
            matrix_both[i+1,j+1] <- result$both_fail # LIB failed for recycling
            
          } else if (j==31 & i!=31){ # BATTERIES TOO OLD
            matrix_ev[i+1,j] <- result$lib_fail # EVs that need LIB, no LIBs available as they died
          } else if (j!=31 & i==31){ # EV TOO OLD
            matrix_lib[i,j+1] <- result$ev_fail # LIBs available to use, no EV at border
          }
        }
      }
    }
    
    # get vector of outflows of EV and outflows of LIBs
    ev_need <- rowSums(matrix_ev)
    
    # Above certain age simply no LIB required, THEY DIED
    ev_need[(max_ev_age_replacement+1):31] <- 0
    
    # LIB ready for end life recycling, when the LIB failed
    # move to the left to allow for delay in other part of the code
    lib_failed <- colSums(matrix_ev)[-1] + colSums(matrix_both)[-1]
    
    lib_available <- colSums(matrix_lib)
    
    # assigning old batteries to second-life in EVs
    lib_to_EV <- lib_available*max_reuse_lib
    # limit age of LIB for EV
    lib_to_EV[(max_lib_age_secondlife+1):31] <- 0
    
    lib_available <- lib_available-lib_to_EV
    
    # first match year to year with offset of years - 8 years
    ev_need <- c(ev_need,rep(0,ev_age_newLib))
    lib_to_EV <- c(rep(0,ev_age_newLib),lib_to_EV)
    allocation <- pmin(ev_need,lib_to_EV)
    
    ev_need <- ev_need - allocation
    lib_to_EV <- lib_to_EV - allocation
    
    # remove offsets
    ev_need <- ev_need[1:31]
    lib_to_EV <- lib_to_EV[-(1:ev_age_newLib)]
    allocation <- allocation[-(1:ev_age_newLib)]
    
    # update new_matrix with stock of EVs and old batteries
    for (i in 1:(31-ev_age_newLib)){
      new_matrix[i+ev_age_newLib,i] <- new_matrix[i+ev_age_newLib,i]+allocation[i]
    }
    
    allocation <- sum(allocation)
    
    # do rest of allocation with LOOP
    start_bat <- 1
    for (i in 31:1) { # start with old
      if (i<=ev_age_newLib){
        # new_matrix[i,0] <- ev_need[i] # new battery DUPLICATED
      } else {
        for (j in start_bat:31) {
          allocated <- min(ev_need[i], lib_to_EV[j])
          ev_need[i] <- ev_need[i] - allocated
          lib_to_EV[j] <- lib_to_EV[j] - allocated
          # update new_matrix with stock of EVs and old batteries
          new_matrix[i,j] <- new_matrix[i,j]+allocated
          allocation <- allocation+allocated
          start_bat <- j
          if (ev_need[i] == 0) { break }
        }
      }
    }
    
    # add remaining batteries back to pool
    lib_available <- lib_available+lib_to_EV
    
    # add EVs with new batteries to stock - note, no other battery with 0 age
    new_matrix[,1] <-  ev_need
    
    # assign numbers for Year - totals and vector
    x$add_LIB[y-start_year+1] <- round(sum(ev_need),0) # additional new LIBs required
    x$add_LIB_vector[y-start_year+1] <- list(round(ev_need[-1],0)) 
    # LIBs in good condition for SSPS or recycling
    x$LIB_Available[y-start_year+1] <- round(sum(lib_available),0)  
    x$LIB_Available_vector[y-start_year+1] <- list(round(lib_available[-1],0))  
    # LIBs that failed but available to recycle
    x$LIB_recycling[y-start_year+1] <- round(sum(lib_failed),0)
    x$LIB_recycling_vector[y-start_year+1] <- list(round(lib_failed,0))
    x$LIB_reuse_EV[y-start_year+1] <- round(allocation,0)
    x$EV_Stock[y-start_year+1] <- round(sum(new_matrix),0)
    x$EV_Stock_vector[y-start_year+1] <- list(unname(round(rowSums(new_matrix)[-1],0)))
    
    
    # end for loop, next year
    matrix_data <- new_matrix
    
    # keep balance of removed EV Sales from stock
    
    rm(new_matrix,matrix_ev,matrix_lib,lib_to_EV,lib_available,allocated,allocation,start_bat)
    
  }
  rm(i,j)

  return(x)
   
}


# To test
# sales <- read.csv("Inputs/EV_Sales.csv")
# sales_usa <- sales %>% filter(Vehicle=="Cars",Country=="United States",Scenario=="Ambitious")
# {
#   start_time <- proc.time()
#   stocks <- f.LIB_EV_Stock(sales_usa)
#   end_time <- proc.time() # Capture the ending time
#   cat("Running time: ",end_time - start_time,"\n") # 3 seconds per run
# }
# 
# stocks[36,]$EV_Stock/370/1e6 # 0.65 EVs per capita



# EoF