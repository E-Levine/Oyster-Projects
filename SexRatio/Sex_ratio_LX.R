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
#Clean data:
Repro_df <- Repro_data_raw %>% 
  #remove unneeded columns
  dplyr::select(-'Repro Stage (2005-2016)', -'MSX Present', -'MSX Stage', -Helper) %>% 
  #rename columns to make easier to work with
  rename(Sample_num = 'Sample Number', SH = 'SH (mm)', Stage = 'Repro Stage (2017-current)', Parasite = 'Other Parasite', Bad_Slide = 'Bad Slide', M_F = 'Male and Female') %>%
  #update data types/values as needed
  mutate(Date = as.Date(Date, origin = "1899-12-30"),
         across(c(Site, Station, Sex, Stage), as.factor))
#
glimpse(Repro_df)
#
#Load Molluscs's WQ data
Molluscs_WQ_Raw <- read_excel("Data/LX Combined Raw Data 2020-2024.xlsx", sheet = "2020-2024 LX WQ", #File name and sheet name
                              skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                              na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                              .name_repair = "unique")
#check data types
glimpse(Molluscs_WQ_Raw)
#
MollWQ_df <- Molluscs_WQ_Raw %>% 
  #remove unneeded columns
  dplyr::select(-SampleEventWQID, -'Chl a (ug/L)') %>% 
  #rename columns to make easier to work with
  rename(Depth = 'Depth (m)', Temp = 'Temp (oC)', Salinity = 'Salinity (ppt)', DO_mgL = 'DO (mg/L)', DO_Pct = 'DO %', Secchi = 'Secchi (m)', Turb_P = 'Tubidity Probe (NTU)', Turb_H = 'Tubidity Handheld (NTU)') %>%
  #update data types/values as needed
  mutate(across(c(Site, Station), as.factor))
#
glimpse(MollWQ_df)
#
#
#
#
#END OF SECTION
#
#

#####Figure formatting - for consistent figures####
#
#Basic background to work with
basetheme <- theme_bw()+
  theme(axis.title.x = element_text(size = 12, face = "bold", color = "black"), axis.text.x = element_text(size = 11, margin = unit(c(0.5, 0.5, 0, 0.5), "cm")),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"), axis.text.y = element_text(size = 11, margin = unit(c(0, 0.5, 0, 0), "cm")),
        panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"))
#
axistheme <- theme(axis.ticks.length = unit(-0.15, "cm"), 
                   axis.text.x = element_text(color = "black", margin = unit(c(0.25, 0.5, 0, 0.5), "cm")), 
                   axis.text.y = element_text(color = "black", margin = unit(c(0, 0.25, 0, 0), "cm")))
#
facettheme <- theme(strip.text = element_text(size = 12, color = "black", face = "bold"))
#
#
##Colors to sites
SiteColor <- c("#009E73", "#E69F00")
names(SiteColor) <- levels(Repro_df$Site)
#
#
#
#END OF SECTION
#
#
