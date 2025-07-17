# Load Consumer Electronics input
# PBH July  2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")

url_drive <- "H:/.shortcut-targets-by-id/1CWiPbqLa53GMwIlw6QXl5kVdUX6nm-Sa/North America Battery Retirements and Recycling Capacity Research/Data/"
ce <- read_excel(paste0(url_drive,"Consumer electronics forecast.xlsx"),
                 sheet="")


# EoF