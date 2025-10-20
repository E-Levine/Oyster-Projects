###Habitat suitability models
##Accessory data
#
#Shorebirds
#
#
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               writexl, readxl, openxlsx, 
               install = TRUE) 
##Set up
Site_code <- c("Site")
Version <- c("version")
#
#
source("HSM Code/Functions/HSM_accessories.R")
#
#####Shorebird data####
#
#
load_shorebird_data(Counties = c("Citrus", "Levy", "Dixie"), SiteCode = Site_code, VersionNumber = Version)
#
clean_shorebird_data(IBNB = "Yes")
#