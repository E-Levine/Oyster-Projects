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
#Reading in Excel files
#
###Water quality
Cage_WQ_raw <- read_excel("../Data/Growth_database_2024_08_26.xlsx", sheet = "SampleEventWQ", #File name and sheet name
                     skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                     na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                     .name_repair = "unique")
#Check data and column names
head(Dage_WQ_raw)
#Remove uneeded columns
(Cage_WQ <- WQ_raw %>% dplyr::select(SampleEventWQID:DissolvedOxygen, PercentDissolvedOxygen, pH:TurbidityYSI, CollectionTime, Comments))
#
###Cage Counts
Cage_counts_raw <- read_excel("../Data/Growth_database_2024_08_26.xlsx", sheet = "CageCount", #File name and sheet name
                          skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                          na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "unique")
head(Cage_counts_raw)
(Cage_counts <- Cage_counts_raw %>% dplyr::select(CageCountID:DaysDeployed, Comments))
#
###Cage SHS
Cage_SH <- read_excel("../Data/Growth_database_2024_08_26.xlsx", sheet = "CageSH", #File name and sheet name
                      skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                      na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                      .name_repair = "unique")
head(Cage_SH)
