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
Locations_raw <- read_excel("Growth_database_2024_09_25.xlsx", sheet = "FixedLocations", #File name and sheet name
                          skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                          na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "unique")
head(Locations_raw)
(Locations <- Locations_raw %>% mutate(Site = as.factor(paste0(Estuary, SectionName))))
#
###Water quality
Cage_WQ_raw <- read_excel("Growth_database_2024_09_25.xlsx", sheet = "SampleEventWQ", #File name and sheet name
                     skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                     na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                     .name_repair = "unique")
#Check data and column names
head(Cage_WQ_raw)
#Remove unneeded columns and add in station info
(Cage_WQ <- Cage_WQ_raw %>% 
    dplyr::select(SampleEventWQID:DissolvedOxygen, PercentDissolvedOxygen, pH:TurbidityYSI, CollectionTime, Comments) %>%
    mutate(MonYr = as.yearmon(as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d")),
           FixedLocationID = substring(SampleEventID, 19, 22)) %>%
    left_join(Locations))
#
###Cage Counts
Cage_counts_raw <- read_excel("Growth_database_2024_09_25.xlsx", sheet = "CageCount_Dead", #File name and sheet name
                          skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                          na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "unique")
head(Cage_counts_raw)
(Cage_counts <- Cage_counts_raw %>% dplyr::select(CageCountID:DaysDeployed, Dead, Comments) %>%
    mutate(MonYr = as.yearmon(as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d")),
           FixedLocationID = substring(SampleEventID, 19, 22)) %>%
    left_join(Locations) %>%
    #Determine dead counts per cage
    left_join(Cage_counts %>% dplyr::select(CageCountID, Dead) %>%
                mutate(Red = as.integer(substr(Dead, 3,4)),
                       Brown = as.integer(substr(Dead, 9, 10)),
                       Yellow = as.integer(substr(Dead, 15, 16))) %>%
                mutate(DeadCount = case_when(substr(CageCountID, 26, 28) == "R_R" ~ Red,
                                             substr(CageCountID, 26, 28) == "R_B" ~ Brown,
                                             substr(CageCountID, 26, 28) == "R_Y" ~ Yellow,
                                             TRUE ~ NA)) %>%
                dplyr::select(CageCountID, DeadCount)) %>%
    mutate(Missing = 30 - TotalCount - DeadCount) %>% #Number of unaccounted for oysters
    mutate(Missing = case_when(Missing == 30 ~ NA, TRUE ~ Missing), #If missing = 30 then cage missing or flipped, remove from numbers
           TotalRet = as.integer(case_when(TotalCount == 30 ~ NA,
                                           TotalCount < 30 ~ TotalCount + DeadCount,
                                           TRUE ~ NA))))
#
###Cage SHS
Cage_SH_raw <- read_excel("../Data/Growth_database_2024_08_26.xlsx", sheet = "CageSH", #File name and sheet name
                      skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                      na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                      .name_repair = "unique")
head(Cage_SH_raw)
(Cage_SH <- Cage_SH_raw %>% dplyr::select(ShellHeightID:ShellHeight, Comments) %>%
  mutate(MonYr = as.yearmon(as.Date(substring(CageCountID, 8, 15), format = "%Y%m%d")),
         FixedLocationID = substring(CageCountID, 19, 22)) %>%
  left_join(Locations))
#
#
#
#
####Figure formatting for consistent figures#####
#
#Basic background to work with
basetheme <- theme_bw()+
  theme(axis.title.x = element_text(size = 12, face = "bold", color = "black"), axis.text.x = element_text(size = 11, margin = unit(c(0.5, 0.5, 0, 0.5), "cm")),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"), axis.text.y = element_text(size = 11, margin = unit(c(0, 0.5, 0, 0), "cm")),
        panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"))
#
axistheme <- theme(axis.ticks.length = unit(-0.15, "cm"), 
                   axis.text.x = element_text(color = "black", margin = unit(c(0.5, 0.5, 0, 0.5), "cm")), 
                   axis.text.y = element_text(color = "black", margin = unit(c(0, 0.5, 0, 0), "cm")))
##Colors to sites
SiteColor <- c("#56B4E9", "#009E73", "#E69F00", "#CC79A7")
names(SiteColor) <- levels(Locations$Site)
#
#
####Cage data####
#
#Average monthly/all number live, dead, and missing retrieved 
Counts_monthly <- Cage_counts %>% group_by(FixedLocationID, Site, MonYr, DataType) %>% 
  summarise(Count = mean(TotalCount, na.rm = T),
            Dead = mean(DeadCount, na.rm = T),
            Missing = mean(Missing, na.rm = T))
Counts_monthly %>% filter(DataType == "Retrieved") %>% group_by(Site) %>% 
  summarise(MeanRet = mean(Count, na.rm = T),
            MeanDead = mean(Dead, na.rm = T),
            MeanMissing = mean(Missing, na.rm = T))
#
ggarrange(
#Live counts
Counts_monthly %>% filter(DataType == "Retrieved") %>%
  ggplot(aes(MonYr, Count, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = SiteColor)+
  basetheme + axistheme,
#Dead counts
Counts_monthly %>% filter(DataType == "Retrieved") %>%
  ggplot(aes(MonYr, Dead, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = SiteColor)+
  basetheme + axistheme,
#Missing counts
Counts_monthly %>% filter(DataType == "Retrieved") %>%
  ggplot(aes(MonYr, Missing, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = SiteColor)+
  basetheme + axistheme,
nrow = 3, ncol = 1)
#
#Percentage live, pct mortality, pct unknown



#Same with lower limit based on deployed SH
#How many times oysters smaller on ret, ave number smaller