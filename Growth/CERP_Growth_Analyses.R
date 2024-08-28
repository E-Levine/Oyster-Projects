##CERP Growth Comparisons
#
#CERP Cage Data 2015-current
#
#
#Load packages, install as needed
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               ggpubr,
               rstatix, #Summary stats
               zoo, lubridate, #Dates and times
               readxl, #Reading excel files
               car, emmeans, multcomp, #Basic analyses
               install = TRUE)
#
#
#
#
####Load Files####
#Reading in Excel files, adding station information to dfs.
#
##Station information
Locations_raw <- read_excel("../Data/Growth_database_2024_08_26.xlsx", sheet = "FixedLocation", #File name and sheet name
                          skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                          na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "unique")
head(Locations_raw)
(Locations <- Locations_raw %>% dplyr::select(FixedLocationID:StationNumber))
#
###Water quality
Cage_WQ_raw <- read_excel("../Data/Growth_database_2024_08_26.xlsx", sheet = "SampleEventWQ", #File name and sheet name
                     skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                     na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                     .name_repair = "unique")
#Check data and column names
head(Cage_WQ_raw)
#Remove unneeded columns and add in station info
(Cage_WQ <- Cage_WQ_raw %>% 
    dplyr::select(SampleEventWQID:DissolvedOxygen, PercentDissolvedOxygen, pH:TurbidityYSI, CollectionTime, Comments) %>%
    mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>%
    left_join(Locations))
#
###Cage Counts
Cage_counts_raw <- read_excel("../Data/Growth_database_2024_08_26.xlsx", sheet = "CageCount", #File name and sheet name
                          skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                          na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "unique")
head(Cage_counts_raw)
(Cage_counts <- Cage_counts_raw %>% dplyr::select(CageCountID:DaysDeployed, Comments) %>%
    mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>%
    left_join(Locations))
#
###Cage SHS
Cage_SH_raw <- read_excel("../Data/Growth_database_2024_08_26.xlsx", sheet = "CageSH", #File name and sheet name
                      skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                      na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                      .name_repair = "unique")
head(Cage_SH_raw)
(Cage_SH <- Cage_SH_raw %>% dplyr::select(ShellHeightID:ShellHeight, Comments) %>%
  mutate(FixedLocationID = substring(CageCountID, 19, 22)) %>%
  left_join(Locations))
#
#
#
#
####Cage data####
#
#Nu live, dead, missing. Percentage live, pct mortality, pct unknown
#Same with lower limit based on deployed SH
#How many times oysters smaller on ret, ave number smaller