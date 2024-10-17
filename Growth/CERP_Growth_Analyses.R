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
(Cage_counts <- Cage_counts_raw %>% dplyr::select(CageCountID, CageColor, DataType, TotalCount) %>%
    #Get deployed and retrieved counts
    mutate(CageCountID = substr(CageCountID, 1, 22),
           MonYr = as.yearmon(as.Date(substring(CageCountID, 8, 15), format = "%Y%m%d")),
           FixedLocationID = substring(CageCountID, 19, 22)) %>% 
    spread(DataType, TotalCount) %>% rename(DepCount = Deployed, LiveCount = Retrieved) %>%
    #Determine dead counts per cage
    left_join(Cage_counts_raw %>% dplyr::select(CageCountID, CageColor, DataType, Dead) %>%
                mutate(CageCountID = substr(CageCountID, 1, 22)) %>% 
                spread(DataType, Dead) %>% rename("DeadCount" = Retrieved) %>%
                dplyr::select(-Deployed)) %>%
    mutate(RetTotal = LiveCount + DeadCount,
           MissCount = DepCount - RetTotal,
           Pct_Dead = 100-((LiveCount/DepCount)*100),
           Pct_DeadCounts = DeadCount/DepCount*100) %>% left_join(Locations))
#
###Cage SHS
Cage_SH_raw <- read_excel("Growth_database_2024_09_25.xlsx", sheet = "CageSH", #File name and sheet name
                      skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                      na = c("", "Z", "z", "NA"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                      .name_repair = "unique")
head(Cage_SH_raw)
(Cage_SH <- Cage_SH_raw %>% dplyr::select(ShellHeightID:ShellHeight, Comments) %>%
  mutate(ShellHeight = as.numeric(ShellHeight),
         DataType = case_when(substr(CageCountID, 26, 26) == "D" ~ "Dep",
                              substr(CageCountID, 26, 26) == "R" ~ "Ret",
                              TRUE ~ NA),
         CageColor = case_when(substr(CageCountID, 28, 28) == "R" ~ "Red",
                               substr(CageCountID, 28, 28) == "B" ~ "Brown",
                               substr(CageCountID, 28, 28) == "Y" ~ "Yellow",
                               TRUE ~ NA),
         MonYr = as.yearmon(as.Date(substring(CageCountID, 8, 15), format = "%Y%m%d")),
         FixedLocationID = substring(CageCountID, 19, 22),
         CageCountID = substr(CageCountID, 1, 22)) %>%
  left_join(Locations))
#
#END OF SECTION
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
#
#
####Cage data####
#
#Average monthly/all number live, dead, and missing retrieved 
(Counts_monthly <- Cage_counts %>% group_by(FixedLocationID, Site, MonYr, CageCountID) %>% 
  summarise(Live = mean(LiveCount, na.rm = T),
            Dead = mean(DeadCount, na.rm = T),
            Missing = mean(MissCount, na.rm = T)))
Counts_monthly %>% group_by(Site) %>% 
  summarise(MeanLive = mean(Live, na.rm = T),
            MeanDead = mean(Dead, na.rm = T),
            MeanMissing = mean(Missing, na.rm = T))
#
ggarrange(
#Live counts
Counts_monthly %>% 
  ggplot(aes(MonYr, Live, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = SiteColor)+
  basetheme + axistheme,
#Dead counts
Counts_monthly %>% 
  ggplot(aes(MonYr, Dead, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = SiteColor)+
  basetheme + axistheme,
#Missing counts
Counts_monthly %>% 
  ggplot(aes(MonYr, Missing, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = SiteColor)+
  basetheme + axistheme,
nrow = 3, ncol = 1)
#

Counts_monthly %>% 
  ggplot(aes(MonYr, Live, color = Site))+
  geom_point()+
  geom_line(aes(group = Site))+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#
Counts_monthly %>% 
  ggplot(aes(MonYr, Dead, color = Site))+
  geom_point()+
  geom_line(aes(group = Site))+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#
Counts_monthly %>% 
  ggplot(aes(MonYr, Missing, color = Site))+
  geom_point()+
  geom_line(aes(group = Site))+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#
Counts_monthly %>% dplyr::select(-Missing) %>% #Missing is inverse of Live
  gather(Type, Count, -FixedLocationID, -Site, -MonYr, -CageCountID) %>%
  ggplot(aes(MonYr, Count, color = Type))+
  geom_point()+
  geom_line(aes(group = Type))+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#
#Percentage live, pct mortality, pct unknown



#Same with lower limit based on deployed SH
#
#
####Cage Shell Heights####
#
(ShellHeights <- Cage_SH %>% 
  group_by(MonYr, CageCountID, DataType, Site, CageColor) %>%
  summarise(MinSH = min(ShellHeight),
            MaxSH = max(ShellHeight),
            MeanSH = mean(ShellHeight, na.rm = TRUE)) %>% 
  mutate(across(all_of(c("MinSH", "MaxSH")), ~ replace(., . == Inf | . == -Inf, NA))) %>% gather(Meas, Size, -MonYr, -CageCountID, -Site, -DataType, -CageColor) %>% mutate(Measurement = paste(DataType, Meas, sep = "_")) %>%
  ungroup() %>% dplyr::select(-DataType, -Meas) %>%
  spread(Measurement, Size) %>%
   mutate(Min_growth = Ret_MinSH - Dep_MinSH,
          Max_growth = Ret_MaxSH - Dep_MaxSH, 
          Mean_growth = Ret_MeanSH - Dep_MeanSH))
#
(SH_summ <- ShellHeights %>% group_by(MonYr, Site, CageCountID) %>%  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)))
#
SH_summ %>%
  ggplot(aes(MonYr, Mean_growth_mean, group = 1))+
  geom_line()+
  geom_line(aes(MonYr, Min_growth_mean, group = 1), color = "red")+
  geom_line(aes(MonYr, Max_growth_mean, group = 1), color = "blue")+
  geom_hline(yintercept = 0, linetype = "dotted")+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#How many times oysters smaller on ret, ave number smaller