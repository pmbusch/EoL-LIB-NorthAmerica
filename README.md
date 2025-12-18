# Future battery recycling in North America: Capacity expansion under shifting demand and circularity strategies

Replications materials for Busch et al. (*submitted*). 

The following code and data allows for the reproduction of all the tables, figures and calculations made in the article, both in the main body and supplementary information.

If you identify any error in the source code or have any further suggestions please contact Pablo Busch at pmbuschh@gmail.com.

# Organization

* **Inputs**: Data inputs used in the analysis. 
* **Figures**: Figures included in the analysis. 
* **Scripts**: All code to process the data, run models and create figures. Each script starts with a description of the file purpose. Through the file there are several explanatory  comments.  
* **Results**: Aggregated results stored to recreate tables and figures.

# Instructions

The repository is ~100Mb fully unzipped. Downloading and unzipping everything should take less than 5 minutes on a normal computer.

Users can run all the code for replication using the "EoL-LIB-NorthAmerica.Rproj.Rproj" file, or by setting their own working directory and running scripts independently.

This GitHub contains organization notes in each folder describing the, and each scripts is properly docummented.

## Runtime

Users can either used the uploaded model results to replicate figures, or run new instances to generate results for the material flow analysis model.

Please note that each survival EV and LIB modeling takes 1 minute per scenario, resulting in ~35 minutes for all scenarios. These generates a key input for the material flow analysis (MFA) model.

Each run for the MFA model takes less than 5 seconds, so running all ~600 scenarios takes around ~30 minutes.

# Software required

The script code was developed with **R** software version 4.5.1. 

The R code requires the following packages: *tidyverse*, *readr*,*readxl*,*ggplot2*,*data.table*,*dplyr*,*gridExtra*,*reshape2*,*scales*,*RColorBrewer*,*sf*,*ggrepel*. All libraries can be installed with the following command: 

```
install.packages(c("tidyverse","readr","readxl","ggplot2","data.table","dplyr","gridExtra","reshape2","scales","RColorBrewer","sf","ggrepel"), dependencies = T)
```

The model has only been tested using OS Windows 10 and 11, but it should work on Mac and Linux as well using **R**.

# License
This project is covered under the **MIT License**
