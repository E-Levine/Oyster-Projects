##Sex ratio and reproduction of Loxahatchee oytsers
#
#Working project: LHarmon, ELevine
#
#
#Load packages, install as needed
#if (!require("remotes")) install.packages("remotes")
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               ggpubr, ggpattern, scales, biostat,
               rstatix, broom, #Summary stats
               zoo, lubridate, forecast, #Dates and times
               readxl, #Reading excel files
               car, emmeans, multcomp, #Basic analyses
               lmPerm, stats,  
               install = TRUE)
#
#
#
#####Load data files####
#
##Load repro data file
Repro_data_raw <- read_excel("Data/LX Combined Raw Data 2020-2024.xlsx", sheet = "ReproMSX", #File name and sheet name
                             skip = 3, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                             na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                             .name_repair = "unique")
#check data types 
glimpse(Repro_data_raw)
#update data types/values as needed

#
Molluscs_WQ_Raw 
#
#
#END OF SECTION
#
#
