##CERP Growth Comparisons
#
#CERP Cage Data 2015-current
#
#
#Load packages, install as needed
#if (!require("remotes")) install.packages("remotes")
#remotes::install_github("GegznaV/biostat")
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
#
####Load Files####
#Reading in Excel files, adding station information to dfs.
#
##Station information
Locations_raw <- read_excel("Data/Growth_database_2025_02.xlsx", sheet = "FixedLocations", #File name and sheet name
                            skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                            na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                            .name_repair = "unique")
head(Locations_raw)
(Locations <- Locations_raw %>% mutate(Site = as.factor(paste0(Estuary, SectionName))))
#
###Water quality
Cage_WQ_raw <- read_excel("Data/Growth_database_2025_02.xlsx", sheet = "SampleEventWQ", #File name and sheet name
                          skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                          na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "unique")
#Check data and column names
glimpse(Cage_WQ_raw)
#Remove unneeded columns and add in station info
(Cage_WQ <- Cage_WQ_raw %>% 
    dplyr::select(SampleEventWQID:DissolvedOxygen, PercentDissolvedOxygen, pH:TurbidityYSI, CollectionTime, Comments) %>%
    rename("SampleEventID_Coll" = SampleEventID) %>%
    mutate(SampleEventID = str_replace_all(SampleEventID_Coll, "COLL", "CAGE"),
           MonYr = as.yearmon(as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d")),
           FixedLocationID = substring(SampleEventID, 19, 22)) %>%
    left_join(Locations) %>% filter(FixedLocationID %in% Locations$FixedLocationID & MonYr < as.yearmon(as.Date("2025-01-01", format = "%Y-%m-%d"))))
#
#CRE
CR_WQ_raw <- read_excel("Data/CR_Portal_selected_Cage_2015_2024.xlsx", #File name and sheet name
                        skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                        na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                        .name_repair = "unique")
glimpse(CR_WQ_raw)
(CR_WQ <- CR_WQ_raw %>% dplyr::select(MonitoringLocationIdentifier, Estuary, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, CharacteristicName, ResultMeasureValue, Result_Unit) %>%
    mutate(MonYr = as.yearmon(as.Date(ActivityStartDate, format = "%Y-%m-%d"))) %>%
    group_by(MonYr, MonitoringLocationIdentifier, Estuary, LatitudeMeasure, LongitudeMeasure, CharacteristicName, Result_Unit) %>%
    summarise(MeanValue = mean(ResultMeasureValue, na.rm = T)) %>%
    mutate(FixedLocationID = case_when(grepl(paste(c("WQX-CES10SUR", "WQX-CES07"), collapse = "|"), MonitoringLocationIdentifier) ~ "0231",
                                       grepl(paste(c("WQX-62-SEAS500", "WQX-PI-02", "WQX-PI-01", "WQX-GSD0108", "4607", "CES08", "NSF04", "CLEW10"), collapse = "|"), MonitoringLocationIdentifier) ~ "0232", 
                                       TRUE ~ NA)) %>%
    left_join(Locations))
CRE_WQ <- CR_WQ %>% subset(FixedLocationID == "0231") %>% subset(MonYr > as.yearmon("03-31-2019", format = "%m-%d-%Y"))
CRW_WQ <- CR_WQ %>% subset(FixedLocationID == "0232") %>% subset(MonYr > as.yearmon("12-31-2017", format = "%m-%d-%Y"))
#
#LXN
LXN_WQ_raw <- read_excel("Data/LX_Portal_selected_Cage_2015_2024.xlsx", #File name and sheet name
                         skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                         na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                         .name_repair = "unique")
glimpse(LXN_WQ_raw)
(LXN_WQ <- LXN_WQ_raw %>% dplyr::select(MonitoringLocationIdentifier, Estuary, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, CharacteristicName, ResultMeasureValue, Result_Unit) %>%
    mutate(MonYr = as.yearmon(as.Date(ActivityStartDate, format = "%Y-%m-%d"))) %>%
    group_by(MonYr, MonitoringLocationIdentifier, Estuary, LatitudeMeasure, LongitudeMeasure, CharacteristicName, Result_Unit) %>%
    summarise(MeanValue = mean(ResultMeasureValue, na.rm = T)) %>%
    mutate(FixedLocationID = "0243") %>%
    left_join(Locations) %>%
    subset(MonYr > as.yearmon("01-31-2015", format = "%m-%d-%Y")))
#
#SLC
SLC_WQ_raw <- read_excel("Data/SL_Portal_selected_Cage_2015_2024.xlsx", #File name and sheet name
                         skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                         na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                         .name_repair = "unique")
glimpse(SLC_WQ_raw)
(SLC_WQ <- SLC_WQ_raw %>% dplyr::select(MonitoringLocationIdentifier, Estuary, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, CharacteristicName, ResultMeasureValue, Result_Unit) %>%
    mutate(MonYr = as.yearmon(as.Date(ActivityStartDate, format = "%Y-%m-%d"))) %>%
    group_by(MonYr, MonitoringLocationIdentifier, Estuary, LatitudeMeasure, LongitudeMeasure, CharacteristicName, Result_Unit) %>%
    summarise(MeanValue = mean(ResultMeasureValue, na.rm = T)) %>%
    mutate(FixedLocationID = "0255") %>%
    left_join(Locations) %>%
    subset(MonYr > as.yearmon("01-31-2015", format = "%m-%d-%Y")))
#
#
#
#
#
###Cage Counts
Cage_counts_raw <- read_excel("Data/Growth_database_2025_02.xlsx", sheet = "CageCount", #File name and sheet name
                              skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                              na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                              .name_repair = "unique")
glimpse(Cage_counts_raw)
(Cage_counts <- Cage_counts_raw %>% dplyr::select(CageCountID, CageColor, DataType, TotalCount, DaysDeployed) %>%
    #Get deployed and retrieved counts
    mutate(CageCountID = substr(CageCountID, 1, 22),
           MonYr = as.yearmon(as.Date(substring(CageCountID, 8, 15), format = "%Y%m%d")),
           FixedLocationID = substring(CageCountID, 19, 22)) %>% 
    spread(DataType, TotalCount) %>% rename(DepCount = Deployed, LiveCount = Retrieved) %>%
    #Determine dead counts per cage
    left_join(Cage_counts_raw %>% dplyr::select(CageCountID, CageColor, DataType, Dead) %>%
                mutate(CageCountID = substr(CageCountID, 1, 22), Dead = as.numeric(Dead)) %>% 
                spread(DataType, Dead) %>% rename("DeadCount" = Retrieved) %>%
                dplyr::select(-Deployed)) %>%
    mutate(RetTotal = LiveCount + DeadCount,
           MissCount = DepCount - RetTotal,
           DeadRate = 1-(LiveCount/DepCount),
           DeadCountRate = (DeadCount/DepCount)) %>% left_join(Locations) %>% 
    filter(MonYr < as.yearmon(as.Date("2025-01-01", format = "%Y-%m-%d"))))
#
###Cage SHS
Cage_SH_raw <- read_excel("Data/Growth_database_2025_02.xlsx", sheet = "CageSH", #File name and sheet name
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
           CageCountID = substr(CageCountID, 1, 22)) %>% filter(MonYr < as.yearmon(as.Date("2025-01-01", format = "%Y-%m-%d"))) %>% 
    left_join(Locations) %>% left_join(Cage_counts %>% dplyr::select(CageCountID, DaysDeployed) %>% unique()) %>% unique())
#
#
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
preztheme <- theme_bw()+
  theme(axis.title.x = element_text(size = 20, face = "bold", color = "black"), axis.text.x = element_text(size = 18, margin = unit(c(0.5, 0.5, 0, 0.5), "cm")),
        axis.title.y = element_text(size = 20, face = "bold", color = "black"), axis.text.y = element_text(size = 18, margin = unit(c(0, 0.5, 0, 0), "cm")),
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
SiteColor <- c("#56B4E9", "#009E73", "#E69F00", "#CC79A7")
names(SiteColor) <- levels(Locations$Site)
#
#END OF SECTION
#
#
#
####Data checks, WQ combination####
#
t <- Cage_counts %>% group_by(Site, MonYr) %>% tally() %>% spread(Site, n)
t <- Cage_counts %>% group_by(Site, MonYr) %>% summarise(meanRet = round(mean(LiveCount, na.rm = T),1)) %>% spread(Site, meanRet)
t <- Cage_SH %>% group_by(Site, MonYr) %>% tally() %>% spread(Site, n)
rm(t)
#
unique(CRE_WQ$CharacteristicName)
(CRE_WQ_all <- full_join(Cage_WQ %>% filter(Site == "CRE") %>% dplyr::select(-SampleEventID_Coll, -SectionName, -SampleEventID, -CollectionTime) %>%
                           dplyr::select(MonYr, Estuary, StationNumber, Site, FixedLocationID, SampleEventWQID, everything()) %>%
                           mutate(PercentDissolvedOxygen = as.numeric(PercentDissolvedOxygen), pH = as.numeric(pH)) %>% rename("StationID" = SampleEventWQID),
                         CRE_WQ %>% ungroup() %>% dplyr::select(-SectionName, -LatitudeMeasure, -LongitudeMeasure, -Result_Unit) %>%
                           dplyr::select(MonYr, Estuary, StationNumber, Site, FixedLocationID, MonitoringLocationIdentifier, everything()) %>%
                           mutate(CharacteristicName = case_when(CharacteristicName == "Depth, Secchi disk depth" ~ "Secchi",
                                                                 CharacteristicName == "Temperature, water" ~ "Temperature",
                                                                 CharacteristicName == "Dissolved oxygen (DO)" ~ "DissolvedOxygen",
                                                                 CharacteristicName == "Dissolved oxygen saturation" ~ "PercentDissolvedOxygen",
                                                                 CharacteristicName == "Total suspended solids" ~ "TSS",
                                                                 TRUE ~ CharacteristicName)) %>%
                           filter(CharacteristicName != "Chlorophyll a, corrected for pheophytin" & CharacteristicName != "Specific conductance") %>% 
                           spread(CharacteristicName, MeanValue) %>% rename("StationID" = MonitoringLocationIdentifier)))
(CRW_WQ_all <- full_join(Cage_WQ %>% filter(Site == "CRW") %>% dplyr::select(-SampleEventID_Coll, -SectionName, -SampleEventID, -CollectionTime) %>%
                           dplyr::select(MonYr, Estuary, StationNumber, Site, FixedLocationID, SampleEventWQID, everything()) %>%
                           mutate(PercentDissolvedOxygen = as.numeric(PercentDissolvedOxygen), pH = as.numeric(pH)) %>% rename("StationID" = SampleEventWQID),
                         CRW_WQ %>% ungroup() %>% dplyr::select(-SectionName, -LatitudeMeasure, -LongitudeMeasure, -Result_Unit) %>%
                           dplyr::select(MonYr, Estuary, StationNumber, Site, FixedLocationID, MonitoringLocationIdentifier, everything()) %>%
                           mutate(CharacteristicName = case_when(CharacteristicName == "Depth, Secchi disk depth" ~ "Secchi",
                                                                 CharacteristicName == "Temperature, water" ~ "Temperature",
                                                                 CharacteristicName == "Dissolved oxygen (DO)" ~ "DissolvedOxygen",
                                                                 CharacteristicName == "Dissolved oxygen saturation" ~ "PercentDissolvedOxygen",
                                                                 CharacteristicName == "Total suspended solids" ~ "TSS",
                                                                 TRUE ~ CharacteristicName)) %>%
                           filter(CharacteristicName != "Chlorophyll a, corrected for pheophytin" & CharacteristicName != "Specific conductance") %>% 
                           spread(CharacteristicName, MeanValue) %>% rename("StationID" = MonitoringLocationIdentifier)))

#
unique(LXN_WQ$CharacteristicName)
(LXN_WQ_all <- full_join(Cage_WQ %>% filter(Site == "LXN") %>% dplyr::select(-SampleEventID_Coll, -SectionName, -SampleEventID, -CollectionTime) %>%
                           dplyr::select(MonYr, Estuary, StationNumber, Site, FixedLocationID, SampleEventWQID, everything()) %>%
                           mutate(PercentDissolvedOxygen = as.numeric(PercentDissolvedOxygen), pH = as.numeric(pH)) %>% rename("StationID" = SampleEventWQID),
                         LXN_WQ %>% ungroup() %>% dplyr::select(-SectionName, -LatitudeMeasure, -LongitudeMeasure, -Result_Unit) %>%
                           dplyr::select(MonYr, Estuary, StationNumber, Site, FixedLocationID, MonitoringLocationIdentifier, everything()) %>%
                           mutate(CharacteristicName = case_when(CharacteristicName == "Depth, Secchi disk depth" ~ "Secchi",
                                                                 CharacteristicName == "Temperature, water" ~ "Temperature",
                                                                 CharacteristicName == "Dissolved oxygen (DO)" ~ "DissolvedOxygen",
                                                                 CharacteristicName == "Dissolved oxygen saturation" ~ "PercentDissolvedOxygen",
                                                                 CharacteristicName == "Total suspended solids" ~ "TSS",
                                                                 TRUE ~ CharacteristicName)) %>%
                           filter(CharacteristicName != "Chlorophyll a, corrected for pheophytin" & CharacteristicName != "Specific conductance") %>% 
                           spread(CharacteristicName, MeanValue) %>% rename("StationID" = MonitoringLocationIdentifier)))
#
#
unique(SLC_WQ$CharacteristicName)
(SLC_WQ_all <- full_join(Cage_WQ %>% filter(Site == "SLC") %>% dplyr::select(-SampleEventID_Coll, -SectionName, -SampleEventID, -CollectionTime) %>%
                           dplyr::select(MonYr, Estuary, StationNumber, Site, FixedLocationID, SampleEventWQID, everything()) %>%
                           mutate(PercentDissolvedOxygen = as.numeric(PercentDissolvedOxygen), pH = as.numeric(pH)) %>% rename("StationID" = SampleEventWQID),
                         SLC_WQ %>% ungroup() %>% dplyr::select(-SectionName, -LatitudeMeasure, -LongitudeMeasure, -Result_Unit) %>%
                           dplyr::select(MonYr, Estuary, StationNumber, Site, FixedLocationID, MonitoringLocationIdentifier, everything()) %>%
                           mutate(CharacteristicName = case_when(CharacteristicName == "Depth, Secchi disk depth" ~ "Secchi",
                                                                 CharacteristicName == "Temperature, water" ~ "Temperature",
                                                                 CharacteristicName == "Dissolved oxygen (DO)" ~ "DissolvedOxygen",
                                                                 CharacteristicName == "Dissolved oxygen saturation" ~ "PercentDissolvedOxygen",
                                                                 CharacteristicName == "Total suspended solids" ~ "TSS",
                                                                 TRUE ~ CharacteristicName)) %>%
                           filter(CharacteristicName != "Chlorophyll a, corrected for pheophytin" & CharacteristicName != "Specific conductance") %>% 
                           spread(CharacteristicName, MeanValue) %>% rename("StationID" = MonitoringLocationIdentifier)))
#
glimpse(CRE_WQ_all)
glimpse(CRW_WQ_all)
glimpse(LXN_WQ_all)
glimpse(SLC_WQ_all)
#
#END OF SECTION
#
#
#
####Overall - summary####
#
##Dep, Ret, Growth summary by cage 
(ShellHeights <- Cage_SH %>% 
   group_by(MonYr, CageCountID, DataType, Site, CageColor, DaysDeployed) %>%
   summarise(MinSH = min(ShellHeight),
             MaxSH = max(ShellHeight),
             MeanSH = mean(ShellHeight, na.rm = TRUE)) %>% 
   mutate(across(all_of(c("MinSH", "MaxSH")), ~ replace(., . == Inf | . == -Inf, NA))) %>% gather(Meas, Size, -MonYr, -CageCountID, -Site, -DataType, -CageColor, -DaysDeployed) %>% mutate(Measurement = paste(DataType, Meas, sep = "_")) %>%
   ungroup() %>% dplyr::select(-DataType, -Meas) %>%
   spread(Measurement, Size) %>%
   mutate(Min_growth = Ret_MinSH - Dep_MinSH,
          Max_growth = Ret_MaxSH - Dep_MaxSH, 
          Mean_growth = Ret_MeanSH - Dep_MeanSH,
          Growth_rate = Mean_growth/DaysDeployed,
          Month_rate = Mean_growth/(DaysDeployed/28)))
#
##Summary by site/station per MonYr and by Site overall
(SH_summ <- ShellHeights %>% group_by(MonYr, Site, CageCountID, DaysDeployed) %>%  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)) %>%
    mutate(Year = as.factor(format(MonYr, "%Y"))))
(SH_Site_summ <- ShellHeights %>% group_by(Site) %>%  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)))
#
##Mean SH by Site
ShellHeights %>% group_by(Site) %>%
  ggplot(aes(Site, Dep_MeanSH))+
  geom_point()+
  geom_boxplot()+
  scale_y_continuous(expand = c(0,0), limits = c(0,85))+
  ggtitle("Cage data through December 2024")+
  basetheme + axistheme
#
SH_summ %>% group_by(Site, Year) %>% summarize(DepMean = mean(Dep_MeanSH_mean, na.rm = T)) %>%
  ggplot(aes(Year, DepMean))+
  geom_line(aes(group = 1))+
  lemon::facet_rep_grid(Site~.)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 70))
#
#
#Compare deployed shell heights among Sites
set.seed(54321)
Site_Dep_SHs <- aovp(Dep_MeanSH_mean  ~ Site, data = SH_summ, perm = "", nperm = 10000)
(Site_Dep_SH_summ <- summary(Site_Dep_SHs))
Site_Dep_SH_tidy <- tidy(Site_Dep_SHs)
names(Site_Dep_SH_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Site_Dep_SH_tidy
#
##Pairwise comparisons - Sites deployed
(Site_dep_pair <- as.data.frame(SH_summ) %>% pairwise_t_test(Dep_MeanSH_mean ~ Site, p.adjust.method = "holm"))
Site_dep_tab <- dplyr::select(Site_dep_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select("Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Site_dep_letters <- make_cld(Site_dep_tab) %>% dplyr::select(-c("spaced_cld")) %>% rename(Site = group, Letters = cld)
(Site_dep_comps <- merge(SH_summ %>% group_by(Site) %>% rstatix::get_summary_stats(Dep_MeanSH_mean , type = "mean_sd") %>% 
                           dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Site_dep_letters, by = "Site"))
#
ShellHeights %>% group_by(Site) %>%
  ggplot(aes(Site, Dep_MeanSH))+
  geom_point()+
  geom_boxplot(fill = SiteColor)+
  scale_y_continuous("Depolyed shell height (mm)", expand = c(0,0), limits = c(0,100))+
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(85, 62, 69, 75), label = c("a", "d", "c", "b"), fontface = "bold", size = 7)+
  ggtitle("Cage data through December 2024") +
  preztheme + axistheme + theme(plot.title = element_text(margin = margin(0, 0, 15, 0)))
###Presentation fig: Site_dep_SH_ave -- 1000
#
#
#
###Compare ret shell heights among Sites
set.seed(54321)
Site_Ret_SHs <- aovp(Ret_MeanSH_mean  ~ Site, data = SH_summ, perm = "", nperm = 10000)
(Site_Ret_SH_summ <- summary(Site_Ret_SHs))
Site_Ret_SH_tidy <- tidy(Site_Ret_SHs)
names(Site_Ret_SH_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Site_Ret_SH_tidy
#
##Pairwise comparisons - Sites deployed
(Site_ret_pair <- as.data.frame(SH_summ) %>% pairwise_t_test(Ret_MeanSH_mean ~ Site, p.adjust.method = "holm"))
Site_ret_tab <- dplyr::select(Site_ret_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select("Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Site_ret_letters <- make_cld(Site_ret_tab) %>% dplyr::select(-c("spaced_cld")) %>% rename(Site = group, Letters = cld)
(Site_ret_comps <- merge(SH_summ %>% group_by(Site) %>% rstatix::get_summary_stats(Ret_MeanSH_mean , type = "mean_sd") %>% 
                           dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Site_ret_letters, by = "Site"))
#
ShellHeights %>% group_by(Site) %>%
  ggplot(aes(Site, Ret_MeanSH))+
  geom_point()+
  #geom_boxplot(fill = SiteColor)+
  geom_boxplot_pattern(color = "black", fill = SiteColor, linewidth = 0.75, pattern = "weave")+
  scale_y_continuous("Retrieved shell height (mm)", expand = c(0,0), limits = c(0,100))+
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(90, 72, 74, 77), label = c("a", "c", "b", "b"), fontface = "bold", size = 7)+
  ggtitle("Cage data through Dec 2024")+
  preztheme + axistheme + theme(plot.title = element_text(margin = margin(0, 0, 15, 0)))
#
###Presentation fig: Site_ret_SH_ave -- 1000
#
##Plot of deployed and retrieved together
ShellHeights %>% group_by(Site) %>% dplyr::select(MonYr:CageColor, Dep_MeanSH, Ret_MeanSH) %>%
  gather("Type", "MeanSH", -MonYr, -CageCountID, -Site, -CageColor) %>%
  mutate(Type = as.factor(substr(Type, 1, 3))) %>%
  ggplot(aes(Site, MeanSH, fill = Site, pattern = Type)) +
  geom_boxplot_pattern(color = "black",  fill = c("#56B4E9", "#56B4E9", "#009E73", "#009E73", "#E69F00", "#E69F00", "#CC79A7", "#CC79A7"), linewidth = 0.75, pattern_key_scale_factor=.25)+ scale_pattern_manual(values = c("none", "weave"))+
  scale_y_continuous("Mean shell height (mm)", expand = c(0,0), limits = c(0,100))+
  preztheme + axistheme + theme(legend.position = "top", legend.text = element_text(size = 14), legend.title = element_text(size = 15)) +
  guides(fill = "none", pattern = guide_legend(override.aes = list(size = 14, pattern_density = 0.5)))
#
###Presentation fig: Site_SH_ave -- 1000
#
#
#
#
#
#
##Mean growth by Site
ShellHeights %>% group_by(Site) %>%
  ggplot(aes(Site, Growth_rate))+
  geom_point()+
  geom_boxplot()+
  scale_y_continuous("Growth rate (mm/day)", expand = c(0,0), limits = c(0,1))+
  ggtitle("Cage data  through Dec 2024")+
  basetheme + axistheme
#
#Compare growth rate (mm/day) among Sites
set.seed(54321)
Site_Growth <- aovp(Growth_rate_mean  ~ Site, data = SH_summ, perm = "", nperm = 10000)
(Site_Growth_summ <- summary(Site_Growth))
Site_Growth_tidy <- tidy(Site_Growth)
names(Site_Growth_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Site_Growth_tidy
#
##Pairwise comparisons - Sites deployed
(Site_grow_pair <- as.data.frame(SH_summ) %>% pairwise_t_test(Growth_rate_mean ~ Site, p.adjust.method = "holm"))
Site_grow_tab <- dplyr::select(Site_grow_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select("Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Site_grow_letters <- make_cld(Site_grow_tab) %>% dplyr::select(-c("spaced_cld")) %>% rename(Site = group, Letters = cld)
(Site_grow_comps <- merge(SH_summ %>% group_by(Site) %>% rstatix::get_summary_stats(Growth_rate_mean , type = "mean_sd") %>% 
                            dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Site_grow_letters, by = "Site"))
#
ShellHeights %>% group_by(Site) %>%
  ggplot(aes(Site, Growth_rate))+
  geom_point()+
  geom_boxplot(fill = SiteColor)+
  scale_y_continuous("Growth rate (mm/day)", expand = c(0,0), limits = c(0,1.25))+
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(1.19, 0.83, 0.89, 0.8), label = c("a", "c", "b", "c"), fontface = "bold", size = 5)+
  ggtitle("Cage data through Dec 2024")+
  preztheme + axistheme
#
###Presentation fig: Site_growth_daily -- 1000
#
#
#
##Mean monthly growth by Site
ShellHeights %>% group_by(Site) %>%
  ggplot(aes(Site, Month_rate))+
  geom_point()+
  geom_boxplot()+
  scale_y_continuous("Growth rate (mm/month)", expand = c(0,0), limits = c(0,40))+
  ggtitle("Cage data through Dec 2024")+
  basetheme + axistheme
#
#Compare growth rate (mm/day) among Sites
set.seed(54321)
Site_Growth_month <- aovp(Month_rate_mean  ~ Site, data = SH_summ, perm = "", nperm = 10000)
(Site_Growth_month_summ <- summary(Site_Growth_month))
Site_Growth_month_tidy <- tidy(Site_Growth_month)
names(Site_Growth_month_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Site_Growth_month_tidy
#
##Pairwise comparisons - Sites deployed
(Site_grow_month_pair <- as.data.frame(SH_summ) %>% pairwise_t_test(Month_rate_mean ~ Site, p.adjust.method = "holm"))
Site_grow_month_tab <- dplyr::select(Site_grow_month_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select("Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Site_grow_month_letters <- make_cld(Site_grow_month_tab) %>% dplyr::select(-c("spaced_cld")) %>% rename(Site = group, Letters = cld)
(Site_grow_month_comps <- merge(SH_summ %>% group_by(Site) %>% rstatix::get_summary_stats(Month_rate_mean , type = "mean_sd") %>% 
                                  dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Site_grow_month_letters, by = "Site"))
#
ShellHeights %>% group_by(Site) %>%
  ggplot(aes(Site, Month_rate))+
  geom_point()+
  geom_boxplot(fill = SiteColor)+
  scale_y_continuous("Growth rate (mm/month)", expand = c(0,0), limits = c(0,40))+
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(34, 23, 25, 22), label = c("a", "c", "b", "c"), fontface = "bold", size = 7)+
  ggtitle("Cage data through Dec 2024")+
  preztheme + axistheme  + theme(plot.title = element_text(margin = margin(0, 0, 15, 0)))
#
###Presentation fig: Site_growth_monthly -- 1000
#
#
#
#
#
###Mortality by Site
#
(Counts_cages <- Cage_counts %>% group_by(MonYr, Site, CageCountID, CageColor) %>% 
    summarise(DepCount = DepCount,
              AssumeDead = DepCount-LiveCount,
              DeadCount = DeadCount,
              DeadRate = mean(DeadRate, na.rm = T),
              DeadCountRate = mean(DeadCountRate, na.rm = T),
              MissPct = (MissCount/DepCount)*100) %>% 
    mutate(Year = as.factor(format(MonYr, "%Y")), Month = as.factor(format(MonYr, "%m"))))
#
##Summary by site/station per MonYr and by Site overall
(Dead_summ <- Counts_cages %>% group_by(MonYr, Year, Site, CageCountID) %>%  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)))
(Dead_Site_summ <- Counts_cages %>% group_by(Site) %>%  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)))
#
##Mean percentages by Site
ggarrange(
  Counts_cages %>% group_by(Site) %>%
    ggplot(aes(Site, DeadRate))+
    geom_point()+
    geom_boxplot()+
    scale_y_continuous("Mean mortality rate", expand = c(0,0), limits = c(0,1))+
    ggtitle("Cage data through Dec 2024")+
    basetheme + axistheme,
  Counts_cages %>% group_by(Site) %>%
    ggplot(aes(Site, DeadCountRate))+
    geom_point()+
    geom_boxplot()+
    scale_y_continuous("Mean dead count rate", expand = c(0,0), limits = c(0,1))+
    ggtitle("Cage data through Dec 2024")+
    basetheme + axistheme
)
#
#
#Compare percent mortality among Sites
set.seed(54321)
Mort_mod <- aovp(DeadRate_mean  ~ Site, data = Dead_summ, perm = "", nperm = 10000)
Mort_mod_tidy <- tidy(Mort_mod)
names(Mort_mod_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Mort_mod_tidy
#
##Pairwise comparisons - Sites deployed
(Mort_mod_pair <- as.data.frame(Dead_summ) %>% pairwise_t_test(DeadRate_mean ~ Site, p.adjust.method = "holm"))
Mort_mod_tab <- dplyr::select(Mort_mod_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select("Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Mort_mod_letters <- make_cld(Mort_mod_tab) %>% dplyr::select(-c("spaced_cld")) %>% rename(Site = group, Letters = cld)
(Mort_mod_comps <- merge(Dead_summ %>% group_by(Site) %>% rstatix::get_summary_stats(DeadRate_mean , type = "mean_sd") %>% 
                           dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Mort_mod_letters, by = "Site"))
#
Counts_cages %>% group_by(Site) %>%
  ggplot(aes(Site, DeadRate))+
  geom_point()+
  #geom_jitter(width = 0.15)+
  geom_boxplot(fill = SiteColor)+
  scale_y_continuous("Mean percent mortality", expand = c(0,0), labels = percent_format(), limits = c(0,1.15), breaks = seq(0, 1, by = 0.2))+
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(1.1, 1.1, 1.1, 1.1), label = c("ab", "a", "b", "b"), fontface = "bold", size = 5)+
  ggtitle("Cage data through Dec 2024")+
  preztheme + axistheme + theme(plot.title = element_text(margin = margin(0, 0, 15, 0)))
#
###Presentation fig: Site_mortality -- 1000
#
##Abstract
ggarrange(
  Counts_cages %>% group_by(Site) %>%
    ggplot(aes(Site, DeadRate*100, fill = Site))+
    geom_boxplot(linewidth = 1)+
    geom_jitter(width = 0.15)+
    scale_y_continuous("Mean mortality (survivorship)", expand = c(0,0), limits = c(0,100))+
    #ggtitle("Cage data  Feb 2005 - Sept 2024")+
    preztheme + axistheme + facettheme + theme(legend.position = "none", axis.text.x = element_text(size = 12)),
  Counts_cages %>% group_by(Site) %>%
    ggplot(aes(Site, DeadCountRate*100, fill = Site))+
    geom_boxplot(linewidth = 1)+
    geom_jitter(width = 0.15)+
    scale_y_continuous("Mean mortality (dead count)", expand = c(0,0), limits = c(0,50))+
    #ggtitle("Cage data  Feb 2005 - Sept 2024")+
    preztheme + axistheme + facettheme + theme(legend.position = "none", axis.text.x = element_text(size = 12))
)
#
#
#
#Compare dead rate among Sites
set.seed(54321)
Dead_mod <- aovp(DeadCountRate_mean  ~ Site, data = Dead_summ, perm = "", nperm = 10000)
Dead_mod_tidy <- tidy(Dead_mod)
names(Dead_mod_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Dead_mod_tidy
#
(Dead_mod_pair <- as.data.frame(Dead_summ) %>% pairwise_t_test(DeadCountRate_mean ~ Site, p.adjust.method = "holm"))
Dead_mod_tab <- dplyr::select(Dead_mod_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select("Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Dead_mod_letters <- make_cld(Dead_mod_tab) %>% dplyr::select(-c("spaced_cld")) %>% rename(Site = group, Letters = cld)
(Dead_mod_comps <- merge(Dead_summ %>% group_by(Site) %>% rstatix::get_summary_stats(DeadCountRate_mean , type = "mean_sd") %>% 
                           dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Dead_mod_letters, by = "Site"))
#
Counts_cages %>% group_by(Site) %>%
  ggplot(aes(Site, DeadCountRate))+
  geom_point()+
  #geom_jitter(width = 0.15)+
  geom_boxplot(fill = SiteColor)+
  scale_y_continuous("Mean percent dead", expand = c(0,0), limits = c(0,0.5), labels = percent_format())+
  ggtitle("Cage data through Dec 2024")+
  preztheme + axistheme + theme(plot.title = element_text(margin = margin(0, 0, 15, 0)))
#
###Presentation fig: Site_deadCount -- 1000
#
#
#
#
#
##END OF SECTION
#
#
#
####Shell Height - Annual changes####
#
###Change in size (dep SH) over time?
(DepSHs <- SH_summ %>% mutate(Year = as.factor(format(MonYr, "%Y"))) %>% dplyr::select(MonYr, Year, Site, CageCountID, Dep_MeanSH_mean) %>% rename(Dep_SH = Dep_MeanSH_mean))
ggarrange(
  DepSHs %>%
    ggplot(aes(MonYr, Dep_SH, group = 1))+
    geom_line()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  DepSHs %>% group_by(Site, Year) %>% summarise(meanSH = mean(Dep_SH, na.rm = T)) %>%
    ggplot(aes(Year, meanSH, group = 1))+
    geom_line()+
    lemon::facet_rep_grid(Site~.)+
    basetheme +axistheme
)
#
##Permutation based ANOVA - Year for each site growth rate
set.seed(54321)
Dep_SH_LXN <- aovp(Dep_SH ~ Year, data = DepSHs %>% filter(Site == "LXN"), perm = "",  nperm = 10000)
Dep_SH_SLC <- aovp(Dep_SH ~ Year, data = DepSHs %>% filter(Site == "SLC"), perm = "",  nperm = 10000)
Dep_SH_CRE <- aovp(Dep_SH ~ Year, data = DepSHs %>% filter(Site == "CRE"), perm = "",  nperm = 10000)
Dep_SH_CRW <- aovp(Dep_SH ~ Year, data = DepSHs %>% filter(Site == "CRW" & Year != "2017"), perm = "",  nperm = 10000)
#
(Annual_dep_tidy <- rbind(rbind(tidy(Dep_SH_LXN) %>% mutate(Site = "LXN"), tidy(Dep_SH_SLC) %>% mutate(Site = "SLC")), 
                          rbind(tidy(Dep_SH_CRE) %>% mutate(Site = "CRE"), tidy(Dep_SH_CRW) %>% mutate(Site = "CRW"))) %>% dplyr::select(Site, everything()))
names(Annual_dep_tidy) <- c("Site", "Factors", "df", "SS", "MS", "F", "Pr")

(Annual_dep_tab <- rbind(rbind(as.data.frame(DepSHs) %>% filter(Site == "LXN") %>% pairwise_t_test(Dep_SH ~ Year, p.adjust.method = "holm")%>% 
                                 dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "LXN", Comparison = paste(group1, group2, sep = "-")) %>%
                                 dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj), 
                               as.data.frame(DepSHs) %>% filter(Site == "SLC") %>% pairwise_t_test(Dep_SH ~ Year, p.adjust.method = "holm")%>% 
                                 dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "SLC", Comparison = paste(group1, group2, sep = "-")) %>%
                                 dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)),
                         rbind(as.data.frame(DepSHs) %>% filter(Site == "CRE") %>% pairwise_t_test(Dep_SH ~ Year, p.adjust.method = "holm")%>% 
                                 dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "CRE", Comparison = paste(group1, group2, sep = "-")) %>%
                                 dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj), 
                               as.data.frame(DepSHs) %>% filter(Site == "CRW" & Year != "2017") %>% pairwise_t_test(Dep_SH ~ Year, p.adjust.method = "holm")%>% 
                                 dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "CRW", Comparison = paste(group1, group2, sep = "-")) %>%
                                 dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj))))
#
#
(Annual_dep_comps <- merge(DepSHs %>% group_by(Site, Year) %>% rstatix::get_summary_stats(Dep_SH , type = "mean_sd") %>% dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                           rbind(rbind(make_cld(Annual_dep_tab %>% filter(Site == "LXN")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "LXN") %>% rename(Year = group, Letters = cld),
                                       make_cld(Annual_dep_tab %>% filter(Site == "SLC")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "SLC") %>% rename(Year = group, Letters = cld)),
                                 rbind(make_cld(Annual_dep_tab %>% filter(Site == "CRE")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "CRE") %>% rename(Year = group, Letters = cld),
                                       make_cld(Annual_dep_tab %>% filter(Site == "CRW")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "CRW") %>% rename(Year = group, Letters = cld)))) %>%
    arrange(Site, Year))
#
Annual_dep_comps %>% 
  ggplot(aes(Year, mean, group = 1))+
  geom_point(aes(color = Site), size = 6)+
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, size = 1)+
  geom_line(aes(color = Site), size = 1.5)+
  lemon::facet_rep_grid(Site~.)+
  #geom_text(aes(y = upper+10, label = Letters), size = 5) +
  scale_y_continuous("Mean deployed shell height (mm)", expand = c(0,0), limits= c(0, 75), breaks = seq(0, 75, by = 25))+
  scale_color_manual(values = SiteColor)+
  preztheme + axistheme + theme(legend.position = "none") + facettheme
#
###Presentation fig: Site_dep_SH_annual -- 1000
#
#Abstract - saving at 700
Annual_dep_comps %>% 
  ggplot(aes(Year, mean, group = 1, color = Site))+
  geom_point(size = 5)+
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, linewidth = 1)+
  geom_line(linewidth = 1.2)+
  lemon::facet_rep_grid(Site~.)+
  scale_color_manual(values = SiteColor)+
  scale_y_continuous("Mean shell height (mm)", expand = c(0,0), limits= c(0, 90), breaks = seq(0, 90, by = 30))+
  preztheme + axistheme + facettheme + theme(legend.position = "none", panel.spacing.y = unit(0, "lines"), axis.text.x = element_text(angle = 25))
#
Annual_dep_tab %>% filter(Site == "CRE" & p.adjust < 0.05)
Annual_dep_tab %>% filter(Site == "CRW" & p.adjust < 0.05)
Annual_dep_tab %>% filter(Site == "LXN" & p.adjust < 0.05)
Annual_dep_tab %>% filter(Site == "SLC" & p.adjust < 0.05)
#
#
###De-meaning
DepSH_demean <- left_join(DepSHs %>% group_by(Site, Year) %>% summarise(AnnualMean_DepSH = mean(Dep_SH, na.rm = T)), #annual within group means
                          DepSHs %>% group_by(Site) %>% summarise(AllMean_DepSH = mean(Dep_SH, na.rm = T))) %>% #group means
  mutate(Demeaning = AnnualMean_DepSH - AllMean_DepSH) 

DepSH_demean %>%
  ggplot(aes(Year, Demeaning, fill = Site))+
  geom_col()+
  scale_fill_manual(values = SiteColor)+
  lemon::facet_rep_grid(Site~.)+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth =1)+
  scale_y_continuous("Difference from mean shell height", limits = c(-8, 8), expand = c(0,0), breaks = seq(-8, 8, by = 4))+
  preztheme + theme(legend.position = "none", panel.spacing.y = unit(1, "lines")) + facettheme 
#
###Presentation fig: Site_dep_SH_annual_demean -- 1000
#
#
#
#END OF SECTION
#
#
####Growth rates - Annual changes####
#
#mm/day growth by CageColor - "raw replicate data"
(Cage_growth_raw <- left_join(ShellHeights %>% dplyr::select(MonYr:CageColor, Dep_MeanSH, Ret_MeanSH, Mean_growth),
                              Cage_counts_raw %>% mutate(CageCountID = substr(CageCountID, 1, 22)) %>% 
                                filter(DataType == "Retrieved") %>% dplyr::select(CageCountID, CageColor, TotalCount, DaysDeployed)) %>%
   mutate(Year = as.factor(format(MonYr, "%Y")), Month = as.factor(format(MonYr, "%m")), 
          Site = as.factor(Site), mm_day = Mean_growth/DaysDeployed, mm_mon = Mean_growth/(DaysDeployed/28)))
(Cage_growth <- Cage_growth_raw %>% group_by(MonYr, Year, Month, CageCountID, Site) %>%
    summarise(MeanDep = mean(Dep_MeanSH, na.rm = T), MeanRet = mean(Ret_MeanSH, na.rm = T), 
              MeanCount = mean(TotalCount, na.rm = T), MeanGrowth = mean(Mean_growth, na.rm = T), DaysDeployed = mean(DaysDeployed),
              MeanDaily = MeanGrowth/DaysDeployed, MeanMonth = MeanGrowth/(DaysDeployed/28)))
#
#Visualize 
ggarrange(
  Cage_growth %>% group_by(MonYr, Site) %>% summarise(Mean_growth = mean(MeanGrowth, na.rm = T)) %>%
    ggplot(aes(MonYr, Mean_growth, group = 1))+
    geom_line()+
    geom_smooth()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Cage_growth %>% group_by(Year, Site) %>% summarise(Mean_growth = mean(MeanGrowth, na.rm = T)) %>%
    ggplot(aes(Year, Mean_growth, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Cage_growth %>% group_by(Month, Site) %>% summarise(Mean_growth = mean(MeanGrowth, na.rm = T)) %>%
    ggplot(aes(Month, Mean_growth, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  nrow = 1, ncol=3)
#
#
#
###Change in growth rate (mm/day) over time?
(GrowthRates <- SH_summ %>% dplyr::select(MonYr, Year, Site, CageCountID, Growth_rate_mean) %>% rename(Growth_rate = Growth_rate_mean))
ggarrange(
  GrowthRates %>%
    ggplot(aes(MonYr, Growth_rate, group = 1))+
    geom_line()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  GrowthRates %>% group_by(Site, Year) %>% summarise(meanRate = mean(Growth_rate, na.rm = T)) %>%
    ggplot(aes(Year, meanRate, group = 1))+
    geom_line()+
    lemon::facet_rep_grid(Site~.)+
    basetheme +axistheme)
#
##Permutation based ANOVA - Year for each site growth rate
set.seed(54321)
Rate_LXN <- aovp(Growth_rate ~ Year, data = GrowthRates %>% filter(Site == "LXN"), perm = "",  nperm = 10000)
Rate_SLC <- aovp(Growth_rate ~ Year, data = GrowthRates %>% filter(Site == "SLC"), perm = "",  nperm = 10000)
Rate_CRE <- aovp(Growth_rate ~ Year, data = GrowthRates %>% filter(Site == "CRE"), perm = "",  nperm = 10000)
Rate_CRW <- aovp(Growth_rate ~ Year, data = GrowthRates %>% filter(Site == "CRW" & Year != "2017"), perm = "",  nperm = 10000)
#
(Annual_grow_tidy <- rbind(rbind(tidy(Rate_LXN) %>% mutate(Site = "LXN"), tidy(Rate_SLC) %>% mutate(Site = "SLC")), 
                           rbind(tidy(Rate_CRE) %>% mutate(Site = "CRE"), tidy(Rate_CRW) %>% mutate(Site = "CRW"))) %>% dplyr::select(Site, everything()))
names(Annual_grow_tidy) <- c("Site", "Factors", "df", "SS", "MS", "F", "Pr")

(Annual_grow_tab <- rbind(as.data.frame(GrowthRates) %>% filter(Site == "LXN") %>% pairwise_t_test(Growth_rate ~ Year, p.adjust.method = "holm")%>% 
                            dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "LXN", Comparison = paste(group1, group2, sep = "-")) %>%
                            dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj), 
                          as.data.frame(GrowthRates) %>% filter(Site == "SLC") %>% pairwise_t_test(Growth_rate ~ Year, p.adjust.method = "holm")%>% 
                            dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "SLC", Comparison = paste(group1, group2, sep = "-")) %>%
                            dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)))
#
#
(Annual_grow_comps <- left_join(GrowthRates %>% group_by(Site, Year) %>% rstatix::get_summary_stats(Growth_rate , type = "mean_sd") %>% dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                                rbind(make_cld(Annual_grow_tab %>% filter(Site == "LXN")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "LXN") %>% rename(Year = group, Letters = cld),
                                      make_cld(Annual_grow_tab %>% filter(Site == "SLC")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "SLC") %>% rename(Year = group, Letters = cld))) %>%
    arrange(Site, Year))
#
Annual_grow_comps %>% 
  ggplot(aes(Year, mean, group = 1))+
  geom_point(aes(color = Site), size = 4)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, size = 1)+
  geom_line()+
  lemon::facet_rep_grid(Site~.)+
  geom_text(aes(y = upper+0.1, label = Letters), size = 5) +
  scale_y_continuous("Mean growth rate (mm/day)", expand = c(0,0), limits= c(-0.2, 0.6), breaks = seq(-0.2, 0.6, by = 0.2))+
  scale_color_manual(values = SiteColor)+
  geom_hline(yintercept = 0, linetype = "dotted")+
  preztheme + axistheme + facettheme + theme(legend.position = "none")
#
###Presentation fig: Site_growth_mmday_annual -- 1000
#
#Abstract
Annual_grow_comps %>% 
  ggplot(aes(Year, mean, group = 1, color = Site))+
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, linewidth = 1)+
  geom_line(linewidth = 1)+
  lemon::facet_rep_grid(Site~.)+
  scale_color_manual(values = SiteColor)+
  scale_y_continuous("Mean growth rate (mm/day)", expand = c(0,0), limits= c(-0.2, 0.6), breaks = seq(-0.2, 0.6, by = 0.2))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  preztheme + axistheme + facettheme + theme(legend.position = "none", panel.spacing.y = unit(1.25, "lines"), 
                                             axis.text.x = element_text(angle = 32), axis.text.y = element_text(size = 12))
#
Annual_grow_tab %>% filter(Site == "LXN" & p.adjust < 0.06)
Annual_grow_tab %>% filter(Site == "SLC" & p.adjust < 0.06)
#
#
###De-meaning
(Growth_demean <- left_join(GrowthRates %>% group_by(Site, Year) %>% summarise(AnnualMean_Growth = mean(Growth_rate, na.rm = T)), #annual within group means
                            GrowthRates %>% group_by(Site) %>% summarise(AllMean_Growth = mean(Growth_rate, na.rm = T))) %>% #group means
    mutate(Demeaning = AnnualMean_Growth - AllMean_Growth))

Growth_demean %>%
  ggplot(aes(Year, Demeaning, fill = Site))+
  geom_col()+
  scale_fill_manual(values = SiteColor)+
  lemon::facet_rep_grid(Site~.)+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  scale_y_continuous("Mean growth rate (mm/day) difference", limits = c(-0.20, 0.20), expand = c(0,0), breaks = seq(-0.20, 0.20, by = 0.10))+
  preztheme + facettheme + theme(legend.position = "none", panel.spacing.y = unit(0, "lines"), axis.text.x = element_text(angle = 25))
#
###Presentation fig: Site_growth_mmday_annual_demean -- 1000
#
#
#
#
#
#
###Change in growth rate (mm/month) over time?
(GrowthMonth <- SH_summ %>% dplyr::select(MonYr, Year, Site, CageCountID, Month_rate_mean) %>% rename(Growth_rate = Month_rate_mean))
ggarrange(
  GrowthMonth %>%
    ggplot(aes(MonYr, Growth_rate, group = 1))+
    geom_line()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  GrowthMonth %>% group_by(Site, Year) %>% summarise(meanRate = mean(Growth_rate, na.rm = T)) %>%
    ggplot(aes(Year, meanRate, group = 1))+
    geom_line()+
    lemon::facet_rep_grid(Site~.)+
    basetheme +axistheme)
#
##Permutation based ANOVA - Year for each site growth rate
set.seed(54321)
MonthRate_LXN <- aovp(Growth_rate ~ Year, data = GrowthMonth %>% filter(Site == "LXN"), perm = "",  nperm = 10000)
MonthRate_SLC <- aovp(Growth_rate ~ Year, data = GrowthMonth %>% filter(Site == "SLC"), perm = "",  nperm = 10000)
MonthRate_CRE <- aovp(Growth_rate ~ Year, data = GrowthMonth %>% filter(Site == "CRE"), perm = "",  nperm = 10000)
MonthRate_CRW <- aovp(Growth_rate ~ Year, data = GrowthMonth %>% filter(Site == "CRW" & Year != "2017"), perm = "",  nperm = 10000)
#
(Annual_growMon_tidy <- rbind(rbind(tidy(MonthRate_LXN) %>% mutate(Site = "LXN"), tidy(MonthRate_SLC) %>% mutate(Site = "SLC")), 
                              rbind(tidy(MonthRate_CRE) %>% mutate(Site = "CRE"), tidy(MonthRate_CRW) %>% mutate(Site = "CRW"))) %>% dplyr::select(Site, everything()))
names(Annual_growMon_tidy) <- c("Site", "Factors", "df", "SS", "MS", "F", "Pr")

(Annual_growMon_tab <- rbind(as.data.frame(GrowthMonth) %>% filter(Site == "LXN") %>% pairwise_t_test(Growth_rate ~ Year, p.adjust.method = "holm")%>% 
                               dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "LXN", Comparison = paste(group1, group2, sep = "-")) %>%
                               dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj), 
                             as.data.frame(GrowthMonth) %>% filter(Site == "SLC") %>% pairwise_t_test(Growth_rate ~ Year, p.adjust.method = "holm")%>% 
                               dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "SLC", Comparison = paste(group1, group2, sep = "-")) %>%
                               dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)))
#
#
(Annual_growMon_comps <- left_join(GrowthMonth %>% group_by(Site, Year) %>% rstatix::get_summary_stats(Growth_rate , type = "mean_sd") %>% dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                                   rbind(make_cld(Annual_growMon_tab %>% filter(Site == "LXN")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "LXN") %>% rename(Year = group, Letters = cld),
                                         make_cld(Annual_growMon_tab %>% filter(Site == "SLC")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "SLC") %>% rename(Year = group, Letters = cld))) %>%
    arrange(Site, Year))
#
Annual_growMon_comps %>% 
  ggplot(aes(Year, mean, group = 1))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, size = 1)+
  geom_point(aes(color = Site), size = 5)+
  geom_line(aes(color = Site), size = 1)+
  lemon::facet_rep_grid(Site~.)+
  geom_text(aes(y = upper+3, label = Letters), size = 5) +
  scale_y_continuous("Mean growth rate (mm/month)", expand = c(0,0), limits= c(-5, 20), breaks = seq(-5, 20, by = 5))+
  scale_color_manual(values = SiteColor)+
  geom_hline(yintercept = 0, linetype = "dotted")+
  preztheme + axistheme + facettheme + theme(legend.position = "none")
#
#
Annual_growMon_comps %>% 
  ggplot(aes(Year, mean, group = 1))+
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, size = 1)+
  geom_point(aes(color = Site), size = 6)+
  geom_line(aes(color = Site), size = 1.5)+
  lemon::facet_rep_grid(Site~.)+
  #geom_text(aes(y = upper+3, label = Letters), size = 5) +
  scale_y_continuous("Mean growth rate (mm/month)", expand = c(0,0), limits= c(0, 15), breaks = seq(0, 15, by = 5))+
  scale_color_manual(values = SiteColor)+
  geom_hline(yintercept = 0, linetype = "dotted")+
  preztheme + axistheme + facettheme + theme(legend.position = "none", panel.spacing.y = unit(1, "lines"))
#
###Presentation fig: Site_growth_mmmonth_annual -- 1000
#
#
###De-meaning
(GrowthMon_demean <- left_join(GrowthMonth %>% group_by(Site, Year) %>% summarise(AnnualMean_Growth = mean(Growth_rate, na.rm = T)), #annual within group means
                               GrowthMonth %>% group_by(Site) %>% summarise(AllMean_Growth = mean(Growth_rate, na.rm = T))) %>% #group means
    mutate(Demeaning = AnnualMean_Growth - AllMean_Growth))

GrowthMon_demean %>%
  ggplot(aes(Year, Demeaning, fill = Site))+
  geom_col()+
  scale_fill_manual(values = SiteColor)+
  lemon::facet_rep_grid(Site~.)+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  scale_y_continuous("Difference mean growth rate (mm/month)", limits = c(-4, 4), expand = c(0,0), breaks = seq(-4, 4, by = 2))+
  preztheme + theme(legend.position = "none", panel.spacing.y = unit(1, "lines")) + facettheme
#
###Presentation fig: Site_growth_mmmonth_annual_demean -- 1000
#
#END OF SECTION
#
#
#
####Pct mortality - Annual changes####
#
###Comparisons within Site among Years
ggarrange(
  Counts_cages %>%
    ggplot(aes(MonYr, DeadRate, group = 1))+
    geom_line()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Counts_cages %>% group_by(Site, Year) %>% summarise(meanMort = mean(DeadRate, na.rm = T)) %>%
    ggplot(aes(Year, meanMort, group = 1))+
    geom_line()+
    lemon::facet_rep_grid(Site~.)+
    basetheme +axistheme
)
#
##Permu1tation based ANOVA - Year for each site percent mortality
set.seed(54321)
PctMort_LXN <- aovp(DeadRate ~ Year, data = Counts_cages %>% filter(Site == "LXN"), perm = "",  nperm = 10000)
PctMort_SLC <- aovp(DeadRate ~ Year, data = Counts_cages %>% filter(Site == "SLC"), perm = "",  nperm = 10000)
PctMort_CRE <- aovp(DeadRate ~ Year, data = Counts_cages %>% filter(Site == "CRE" & Year != "2017" & Year != "2018"), perm = "",  nperm = 10000)
PctMort_CRW <- aovp(DeadRate ~ Year, data = Counts_cages %>% filter(Site == "CRW" & Year != "2017"), perm = "",  nperm = 10000)
#
(Annual_PctMort_tidy <- rbind(rbind(tidy(PctMort_LXN) %>% mutate(Site = "LXN"), tidy(PctMort_SLC) %>% mutate(Site = "SLC")), 
                              rbind(tidy(PctMort_CRE) %>% mutate(Site = "CRE"), tidy(PctMort_CRW) %>% mutate(Site = "CRW"))) %>% dplyr::select(Site, everything()))
names(Annual_PctMort_tidy) <- c("Site", "Factors", "df", "SS", "MS", "F", "Pr")

(Annual_PctMort_tab <- rbind(rbind(as.data.frame(Counts_cages) %>% filter(Site == "LXN") %>% pairwise_t_test(DeadRate ~ Year, p.adjust.method = "holm")%>% 
                                     dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "LXN", Comparison = paste(group1, group2, sep = "-")) %>%
                                     dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj), 
                                   as.data.frame(Counts_cages) %>% filter(Site == "SLC") %>% pairwise_t_test(DeadRate ~ Year, p.adjust.method = "holm")%>% 
                                     dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "SLC", Comparison = paste(group1, group2, sep = "-")) %>%
                                     dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)),
                             rbind(as.data.frame(Counts_cages) %>% filter(Site == "CRE" & Year != "2017" & Year != "2018") %>% pairwise_t_test(DeadRate ~ Year, p.adjust.method = "holm")%>% 
                                     dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "CRE", Comparison = paste(group1, group2, sep = "-")) %>%
                                     dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj), 
                                   as.data.frame(Counts_cages) %>% filter(Site == "CRW" & Year != "2017") %>% pairwise_t_test(DeadRate ~ Year, p.adjust.method = "holm")%>% 
                                     dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "CRW", Comparison = paste(group1, group2, sep = "-")) %>%
                                     dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj))))
#
#
(Annual_PctMort_comps <- left_join(Counts_cages %>% group_by(Site, Year) %>% rstatix::get_summary_stats(DeadRate , type = "mean_sd") %>% dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                                   rbind(rbind(make_cld(Annual_PctMort_tab %>% filter(Site == "LXN")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "LXN") %>% rename(Year = group, Letters = cld),
                                               make_cld(Annual_PctMort_tab %>% filter(Site == "SLC")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "SLC") %>% rename(Year = group, Letters = cld)),
                                         rbind(make_cld(Annual_PctMort_tab %>% filter(Site == "CRE")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "CRE", cld = NA) %>% rename(Year = group, Letters = cld),
                                               make_cld(Annual_PctMort_tab %>% filter(Site == "CRW")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "CRW") %>% rename(Year = group, Letters = cld)))) %>%
    arrange(Site, Year))
#
Annual_PctMort_comps %>% 
  ggplot(aes(Year, mean, group = 1))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, linewidth = 0.75)+
  geom_line()+
  geom_point(aes(color = Site), size = 4)+
  lemon::facet_rep_grid(Site~.)+
  geom_text(aes(y = upper+0.2, label = Letters), size = 5) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_y_continuous("Mean mortality (survivorship)", expand = c(0,0), limits= c(-0.1, 1.5))+
  scale_color_manual(values = SiteColor)+
  preztheme + axistheme + facettheme + theme(legend.position = "none")
#
Annual_PctMort_comps %>% 
  ggplot(aes(Year, mean, group = 1))+
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, linewidth = 0.75)+
  geom_line(aes(color = Site), size = 1.5)+ 
  geom_point(aes(color = Site), size = 6)+
  lemon::facet_rep_grid(Site~.)+
  #geom_text(aes(y = upper+0.2, label = Letters), size = 5) +
  #geom_hline(yintercept = 0, linetype = "dashed")+
  scale_y_continuous("Mean mortality rate", expand = c(0,0), limits= c(0, 1.1))+
  scale_color_manual(values = SiteColor)+
  preztheme + axistheme + facettheme + theme(legend.position = "none", panel.spacing.y = unit(1, "lines"))
###Presentation fig: Site_mortality_annual -- 1000
#
###De-meaning
(DeadRate_demean <- left_join(Counts_cages %>% group_by(Site, Year) %>% summarise(AnnualMean_DeadRate = mean(DeadRate, na.rm = T)), #annual within group means
                              Counts_cages %>% group_by(Site) %>% summarise(AllMean_DeadRate = mean(DeadRate, na.rm = T))) %>% #group means
    mutate(Demeaning = AnnualMean_DeadRate - AllMean_DeadRate))

DeadRate_demean %>%
  ggplot(aes(Year, Demeaning, fill = Site))+
  geom_col()+
  scale_fill_manual(values = SiteColor)+
  lemon::facet_rep_grid(Site~.)+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  scale_y_continuous("Difference from mean mortality rate", limits = c(-0.4, 0.4), expand = c(0,0), breaks = seq(-0.4, 0.4, by = 0.2))+
  preztheme + facettheme + theme(legend.position = "none", panel.spacing.y = unit(1, "lines"))
#
###Presentation fig: Site_mortality_annual_demean -- 1000
#
#
#
#
#
#
##Permutation based ANOVA - Year for each site dead counts
set.seed(54321)
PctDead_LXN <- aovp(DeadCountRate ~ Year, data = Counts_cages %>% filter(Site == "LXN"), perm = "",  nperm = 10000)
PctDead_SLC <- aovp(DeadCountRate ~ Year, data = Counts_cages %>% filter(Site == "SLC"), perm = "",  nperm = 10000)
PctDead_CRE <- aovp(DeadCountRate ~ Year, data = Counts_cages %>% filter(Site == "CRE" & Year != "2017" & Year != "2018"), perm = "",  nperm = 10000)
PctDead_CRW <- aovp(DeadCountRate ~ Year, data = Counts_cages %>% filter(Site == "CRW" & Year != "2017"), perm = "",  nperm = 10000)
#
(Annual_PctDead_tidy <- rbind(rbind(tidy(PctDead_LXN) %>% mutate(Site = "LXN"), tidy(PctDead_SLC) %>% mutate(Site = "SLC")), 
                              rbind(tidy(PctDead_CRE) %>% mutate(Site = "CRE"), tidy(PctDead_CRW) %>% mutate(Site = "CRW"))) %>% dplyr::select(Site, everything()))
names(Annual_PctDead_tidy) <- c("Site", "Factors", "df", "SS", "MS", "F", "Pr")

(Annual_PctDead_tab <- rbind(rbind(as.data.frame(Counts_cages) %>% filter(Site == "LXN") %>% pairwise_t_test(DeadCountRate ~ Year, p.adjust.method = "holm")%>% 
                                     dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "LXN", Comparison = paste(group1, group2, sep = "-")) %>%
                                     dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj), 
                                   as.data.frame(Counts_cages) %>% filter(Site == "SLC") %>% pairwise_t_test(DeadCountRate ~ Year, p.adjust.method = "holm")%>% 
                                     dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "SLC", Comparison = paste(group1, group2, sep = "-")) %>%
                                     dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)),
                             rbind(as.data.frame(Counts_cages) %>% filter(Site == "CRE" & Year != "2017" & Year != "2018") %>% pairwise_t_test(DeadCountRate ~ Year, p.adjust.method = "holm")%>% 
                                     dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "CRE", Comparison = paste(group1, group2, sep = "-")) %>%
                                     dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj), 
                                   as.data.frame(Counts_cages) %>% filter(Site == "CRW" & Year != "2017") %>% pairwise_t_test(DeadCountRate ~ Year, p.adjust.method = "holm")%>% 
                                     dplyr::select(c("group1", "group2", "p", "p.adj")) %>% mutate(Site = "CRW", Comparison = paste(group1, group2, sep = "-")) %>%
                                     dplyr::select("Site", "Comparison", everything()) %>% dplyr::select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj))))
#
#
(Annual_PctDead_comps <- left_join(Counts_cages %>% group_by(Site, Year) %>% rstatix::get_summary_stats(DeadCountRate , type = "mean_sd") %>% dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                                   rbind(rbind(make_cld(Annual_PctDead_tab %>% filter(Site == "LXN")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "LXN") %>% rename(Year = group, Letters = cld),
                                               make_cld(Annual_PctDead_tab %>% filter(Site == "SLC")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "SLC") %>% rename(Year = group, Letters = cld)),
                                         rbind(make_cld(Annual_PctDead_tab %>% filter(Site == "CRE")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "CRE", cld = NA) %>% rename(Year = group, Letters = cld),
                                               make_cld(Annual_PctDead_tab %>% filter(Site == "CRW")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "CRW") %>% rename(Year = group, Letters = cld)))) %>%
    arrange(Site, Year))
#
Annual_PctDead_comps %>% 
  ggplot(aes(Year, mean, group = 1))+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, linewidth = 0.75)+
  geom_point(aes(color = Site), size = 4)+
  lemon::facet_rep_grid(Site~.)+
  geom_text(aes(y = upper+0.1, label = Letters), size = 5) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_y_continuous("Mean mortality (dead counts)", expand = c(0,0), limits= c(-0.1, 0.4))+
  scale_color_manual(values = SiteColor)+
  preztheme + axistheme + facettheme + theme(legend.position = "none")
#
Annual_PctDead_comps %>% 
  ggplot(aes(Year, mean, group = 1))+
  geom_line(aes(color = Site), size = 1.5)+
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, linewidth = 0.75)+
  geom_point(aes(color = Site), size = 6)+
  lemon::facet_rep_grid(Site~.)+
  #geom_text(aes(y = upper+0.1, label = Letters), size = 5) +
  #geom_hline(yintercept = 0, linetype = "dashed")+
  scale_y_continuous("Mean dead rate", expand = c(0,0), limits= c(0, 0.15), breaks = seq(0, 0.15, 0.05))+
  scale_color_manual(values = SiteColor)+
  preztheme + axistheme + facettheme + theme(legend.position = "none", panel.spacing.y = unit(1, "lines"))
#
###Presentation fig: Site_mortality_dead_annual -- 1000
#
###De-meaning
(DeadCount_demean <- left_join(Counts_cages %>% group_by(Site, Year) %>% summarise(AnnualMean_DeadCount = mean(DeadCountRate, na.rm = T)), #annual within group means
                               Counts_cages %>% group_by(Site) %>% summarise(AllMean_DeadCount = mean(DeadCountRate, na.rm = T))) %>% #group means
    mutate(Demeaning = AnnualMean_DeadCount - AllMean_DeadCount))

DeadCount_demean %>%
  ggplot(aes(Year, Demeaning, fill = Site))+
  geom_col()+
  scale_fill_manual(values = SiteColor)+
  lemon::facet_rep_grid(Site~.)+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  scale_y_continuous("Difference from mean dead rate", limits = c(-0.05, 0.1), expand = c(0,0), breaks = seq(-0.05, 0.1, by = 0.05))+
  preztheme + facettheme + theme(legend.position = "none", panel.spacing.y = unit(1, "lines"))
# 
###Presentation fig: Site_mortality_count_annual_demean -- 1000
#
#
###END OF SECTION
#
#
#
####Water quality - all estuaries####
#
###Sites are different enough to consider water quality individually but considering as one data set first
#Seasonal trends are know so detrending data
#
###Detrend each parameter - additive
detrending <- function(df, param){
  temp <- df %>% ungroup() %>% dplyr::select(c("MonYr", param))
  #temp$MonYr <- as.yearmon(temp$MonYr, format = "%m/%Y")
  temp <- na.interp(as.ts(read.zoo(temp, FUN = as.yearmon)))
  temp %>% decompose("additive") -> decompTemp
  tempAdj <- temp-decompTemp$seasonal
  return(tempAdj)
}
#
(All_WQ_clean <- rbind(CRE_WQ_all %>% group_by(MonYr, Site) %>% summarise(across(c(Temperature, Salinity, DissolvedOxygen, pH), mean, na.rm = T)) %>% ungroup(), 
                       CRW_WQ_all %>% group_by(MonYr, Site) %>% summarise(across(c(Temperature, Salinity, DissolvedOxygen, pH), mean, na.rm = T)) %>% ungroup()) %>% 
    rbind(LXN_WQ_all %>% group_by(MonYr, Site) %>% summarise(across(c(Temperature, Salinity, DissolvedOxygen, pH), mean, na.rm = T)) %>% ungroup()) %>% 
    rbind(SLC_WQ_all %>% group_by(MonYr, Site) %>% summarise(across(c(Temperature, Salinity, DissolvedOxygen, pH), mean, na.rm = T)) %>% ungroup()))
#
(AllWQ_adj <- data.frame(MonYr = unique(All_WQ_clean$MonYr),
                        TempAdj = detrending(All_WQ_clean %>% group_by(MonYr) %>% summarise(Temperature = mean(Temperature, na.rm = T)), "Temperature"),
                        SalAdj  = detrending(All_WQ_clean %>% group_by(MonYr) %>% summarise(Salinity = mean(Salinity, na.rm = T)), "Salinity"),
                        DOAdj = detrending(All_WQ_clean %>% group_by(MonYr) %>% summarise(DissolvedOxygen = mean(DissolvedOxygen, na.rm = T)), "DissolvedOxygen"),
                        pHAdj = detrending(All_WQ_clean %>% group_by(MonYr) %>% summarise(pH = mean(pH, na.rm = T)), "pH")))
#
All_WQ_clean %>% rename("DO" = DissolvedOxygen) %>% gather(Parameter, Value, -MonYr, -Site) %>%
  ggplot(aes(MonYr, Value))+
  geom_line(aes(group = Site, color = Site), size = 2.5)+
  lemon::facet_rep_grid(Parameter~., scales = "free_y", switch = "y")+
  scale_color_manual(values = SiteColor)+ xlab("Year")+
  scale_x_yearmon(expand = c(0.01,0), limits = c(as.yearmon("Jul 2015", format = "%b %Y"), as.yearmon("Dec 2024", format = "%b %Y")))+
  preztheme + facettheme + theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12), panel.spacing.y = unit(-1, "lines"))
#
###Presentation fig: All_WQ_patterns -- 1000
#
Alldata <- left_join(AllWQ_adj, GrowthMonth %>% group_by(MonYr) %>% summarise(MeanMonthly = mean(Growth_rate, na.rm = T))) %>% 
  drop_na() %>% mutate(Year = as.integer(format(MonYr, "%Y")))
##Vizualize data##
#Get summary mean/sd
Alldata %>% group_by(Year) %>% rstatix::get_summary_stats(MeanMonthly, type = "mean_sd")
ggboxplot(Alldata, x = "Year", y = "MeanMonthly")
Alldata %>% ggplot(aes(x = MeanMonthly))+ geom_histogram(aes(y = ..count..)) #Has zeros, add constant for comparison
Alldata <- Alldata %>% mutate(MeanMonthly1 = MeanMonthly + 1)
Alldata %>% ggplot(aes(x = MeanMonthly1))+ geom_histogram(aes(y = ..count..)) #Has zeros, add constant for comparison
#
#
##Initial MLR - all dat, growth
set.seed(54321)
All_growth <- lm(MeanMonthly1 ~ ., data = Alldata %>% dplyr::select(-MonYr, -MeanMonthly))
All_growth_tab <- tidy(All_growth)
names(All_growth_tab) <- c("term", "Est.", "SE", "t", "p-value")
All_growth_sum <- glance(All_growth) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(All_growth_sum) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
All_growth_tab; All_growth_sum
##AIC - Model selection for final model - including YEAR
All_growth_step <- stepAIC(All_growth, direction = "backward")
set.seed(54321)
All_growth_final <- update(All_growth, .~. -SalAdj, data = Alldata %>% dplyr::select(-MonYr, -MeanMonthly))
tidy(All_growth_final)
glance(All_growth_final) %>% dplyr::select(r.squared:df, deviance:df.residual)
#
(All_growth_modeldata <- cbind(data.frame(Growth_p = predict(All_growth_final, (Alldata %>% dplyr::select(-MonYr, -MeanMonthly)))-1, Year = Alldata$Year),
                        predict(All_growth_final, interval = "confidence")-1) %>% dplyr::select(-fit) %>% dplyr::select(Year, Growth_p, everything()) %>%
    group_by(Year) %>% dplyr::summarise(Growth_p = mean(Growth_p, na.rm = T), Growth_lwr = mean(lwr), Growth_upr = mean(upr)))
#
ggplot(Alldata %>% group_by(Year) %>% summarise(mean = mean(MeanMonthly, na.rm = T)))+
  geom_point(aes(Year, mean, color = "Mean"), size = 4)+
  geom_line(data = All_growth_modeldata, aes(Year, Growth_p, color = "Predict"), size = 1.25)+
  geom_line(data = All_growth_modeldata, aes(Year, Growth_lwr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  geom_line(data = All_growth_modeldata, aes(Year, Growth_upr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  preztheme + theme(legend.position = c(0.85, 0.92), legend.text = element_text(size = 16))+ axistheme +
  ylab("Mean growth rate (mm/month)")+ 
  scale_x_continuous(expand = c(0.1,0), limits = c(2015, 2024), breaks = seq(2015, 2024, 1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,8)) +
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#
###Presentation fig: Growth_model_annual -- 1000
#
#
###Predictions###
#
#predictions: WQ stay same as average of previous 5 years, or temp increase by 0.006C per year (0.06C/decade)
predAll_same <- data.frame()
#data.frame(Year = 2025:2030, Alldata %>% filter(Year > 2019) %>% summarise(across(c(TempAdj, SalAdj, DOAdj, pHAdj), list(mean = mean), na.rm = T))
           for (year in 2025:2030) {
             summarized_data <- data.frame(Year = 2025:2030, Alldata %>% filter(Year > 2019) %>% summarise(across(c(TempAdj, SalAdj, DOAdj, pHAdj), list(mean = mean), na.rm = T)))
             # Create a temporary data frame for the current year
             temp_data <- summarized_data %>% mutate(Year = year)  # Add the Year column
             # Bind the temporary data frame to the new data frame
             predAll_same <- rbind(predAll_same, temp_data)
           }
(predAll_same <- predAll_same %>% distinct() %>% rename_with(~ gsub("_mean", "", .)))           #df of no change
(predAll_inc <- predAll_same %>% mutate(TempAdj = TempAdj + (row_number()-1)*0.006)) #df of increased change

(predAll_same <- left_join(predAll_same,
  cbind(data.frame(Growth_p = predict(All_growth_final, predAll_same)-1, Year = predAll_same$Year),
      predict(All_growth_final, predAll_same, interval = "confidence")-1) %>% dplyr::select(-fit) %>% dplyr::select(Year, Growth_p, everything()) %>%
  group_by(Year) %>% dplyr::summarise(Growth_p = mean(Growth_p, na.rm = T), Growth_lwr = mean(lwr), Growth_upr = mean(upr))))
#
(predAll_inc <- left_join(predAll_inc,
                           cbind(data.frame(Growth_p = predict(All_growth_final, predAll_inc)-1, Year = predAll_inc$Year),
                                 predict(All_growth_final, predAll_inc, interval = "confidence")-1) %>% dplyr::select(-fit) %>% dplyr::select(Year, Growth_p, everything()) %>%
                             group_by(Year) %>% dplyr::summarise(Growth_p = mean(Growth_p, na.rm = T), Growth_lwr = mean(lwr), Growth_upr = mean(upr))))
#
ggplot(Alldata %>% group_by(Year) %>% summarise(mean = mean(MeanMonthly, na.rm = T)))+
  geom_point(aes(Year, mean, color = "Mean"))+
  geom_line(data = All_growth_modeldata, aes(Year, Growth_p, color = "Predict"))+
  geom_line(data = All_growth_modeldata, aes(Year, Growth_lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = All_growth_modeldata, aes(Year, Growth_upr, color = "95% CI"), linetype = "dashed")+
  geom_point(data = predAll_same, aes(Year, Growth_p), color = "blue")+
  geom_point(data = predAll_inc, aes(Year, Growth_p), color = "green")+
preztheme + theme(legend.position = c(0.899, 0.91))+ axistheme +
  ylab("Mean growth rate (mm/month)")+ 
  scale_x_continuous(expand = c(0.1,0), limits = c(2015, 2030), breaks = seq(2015, 2030, 1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,8)) +
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))

####Water quality - CRE####
#
##Working within estuary
#
###Detrend each parameter - additive
detrending <- function(df, param){
  temp <- df %>% ungroup() %>% dplyr::select(c("MonYr", param))
  #temp$MonYr <- as.yearmon(temp$MonYr, format = "%m/%Y")
  temp <- na.interp(as.ts(read.zoo(temp, FUN = as.yearmon)))
  temp %>% decompose("additive") -> decompTemp
  tempAdj <- temp-decompTemp$seasonal
  return(tempAdj)
}
#
(CREWQ_adj <- data.frame(MonYr = as.yearmon(time(detrending((All_WQ_clean %>% filter(Site == "CRE")) %>% group_by(MonYr) %>% summarise(Temperature = mean(Temperature, na.rm = T)), "Temperature"))),
                         TempAdj = detrending((All_WQ_clean %>% filter(Site == "CRE")) %>% group_by(MonYr) %>% summarise(Temperature = mean(Temperature, na.rm = T)), "Temperature"),
                         SalAdj  = detrending((All_WQ_clean %>% filter(Site == "CRE")) %>% group_by(MonYr) %>% summarise(Salinity = mean(Salinity, na.rm = T)), "Salinity"),
                         DOAdj = detrending((All_WQ_clean %>% filter(Site == "CRE")) %>% group_by(MonYr) %>% summarise(DissolvedOxygen = mean(DissolvedOxygen, na.rm = T)), "DissolvedOxygen"),
                         pHAdj = detrending((All_WQ_clean %>% filter(Site == "CRE")) %>% group_by(MonYr) %>% summarise(pH = mean(pH, na.rm = T)), "pH")))
#
CREdata <- left_join(CREWQ_adj, GrowthMonth %>% filter(Site == "CRE") %>% group_by(MonYr) %>% summarise(MeanMonthly = mean(Growth_rate, na.rm = T))) %>% 
  drop_na() %>% mutate(Year = as.integer(format(MonYr, "%Y")))
##Vizualize data##
#Get summary mean/sd
CREdata %>% group_by(Year) %>% rstatix::get_summary_stats(MeanMonthly, type = "mean_sd")
ggboxplot(CREdata, x = "Year", y = "MeanMonthly")
CREdata %>% ggplot(aes(x = MeanMonthly))+ geom_histogram(aes(y = ..count..)) #Has negatives, add constant for comparison
CREdata <- CREdata %>% mutate(MeanMonthly1 = MeanMonthly + 2)
CREdata %>% ggplot(aes(x = MeanMonthly1))+ geom_histogram(aes(y = ..count..)) #Has negatives, add constant for comparison
#
#
##Initial MLR - all dat, growth
set.seed(54321)
CRE_growth <- lm(MeanMonthly1 ~ ., data = CREdata %>% dplyr::select(-MonYr, -MeanMonthly))
CRE_growth_tab <- tidy(CRE_growth)
names(CRE_growth_tab) <- c("term", "Est.", "SE", "t", "p-value")
CRE_growth_sum <- glance(CRE_growth) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(CRE_growth_sum) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
CRE_growth_tab; CRE_growth_sum
##AIC - Model selection for final model - including YEAR
CRE_growth_step <- stepAIC(CRE_growth, direction = "backward")
set.seed(54321)
CRE_growth_final <- update(CRE_growth, .~. -TempAdj -SalAdj -DOAdj -pHAdj, data = CREdata %>% dplyr::select(-MonYr, -MeanMonthly))
tidy(CRE_growth_final)
glance(CRE_growth_final) %>% dplyr::select(r.squared:df, deviance:df.residual)
#R2 is higher with all parameters
(CRE_growth_modeldata <- cbind(data.frame(Growth_p = predict(CRE_growth, (CREdata %>% dplyr::select(-MonYr, -MeanMonthly)))-2, Year = CREdata$Year),
                               predict(CRE_growth, interval = "confidence")-2) %>% dplyr::select(-fit) %>% dplyr::select(Year, Growth_p, everything()) %>%
    group_by(Year) %>% dplyr::summarise(Growth_p = mean(Growth_p, na.rm = T), Growth_lwr = mean(lwr), Growth_upr = mean(upr)))
#
ggplot(CREdata %>% group_by(Year) %>% summarise(mean = mean(MeanMonthly, na.rm = T)))+
  geom_point(aes(Year, mean, color = "Mean"), size = 4)+
  geom_line(data = CRE_growth_modeldata, aes(Year, Growth_p, color = "Predict"), size = 1.25)+
  geom_line(data = CRE_growth_modeldata, aes(Year, Growth_lwr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  geom_line(data = CRE_growth_modeldata, aes(Year, Growth_upr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  preztheme + theme(legend.position = c(0.85, 0.92), legend.text = element_text(size = 16))+ axistheme +
  ylab("Mean growth rate (mm/month)")+ 
  scale_x_continuous(expand = c(0.1,0), limits = c(2019, 2024), breaks = seq(2019, 2024, 1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#
###Presentation fig: Growth_model_CRE -- 1000
#
#


####Water quality - CRW####
#
##Working within estuary
#
(CRWWQ_adj <- data.frame(MonYr = as.yearmon(time(detrending((All_WQ_clean %>% filter(Site == "CRW")) %>% group_by(MonYr) %>% summarise(Temperature = mean(Temperature, na.rm = T)), "Temperature"))),
                         TempAdj = detrending((All_WQ_clean %>% filter(Site == "CRW")) %>% group_by(MonYr) %>% summarise(Temperature = mean(Temperature, na.rm = T)), "Temperature"),
                         SalAdj  = detrending((All_WQ_clean %>% filter(Site == "CRW")) %>% group_by(MonYr) %>% summarise(Salinity = mean(Salinity, na.rm = T)), "Salinity"),
                         DOAdj = detrending((All_WQ_clean %>% filter(Site == "CRW")) %>% group_by(MonYr) %>% summarise(DissolvedOxygen = mean(DissolvedOxygen, na.rm = T)), "DissolvedOxygen"),
                         pHAdj = detrending((All_WQ_clean %>% filter(Site == "CRW")) %>% group_by(MonYr) %>% summarise(pH = mean(pH, na.rm = T)), "pH")))
#
CRWdata <- left_join(CRWWQ_adj, GrowthMonth %>% filter(Site == "CRW") %>% group_by(MonYr) %>% summarise(MeanMonthly = mean(Growth_rate, na.rm = T))) %>% 
  drop_na() %>% mutate(Year = as.integer(format(MonYr, "%Y")))
##Vizualize data##
#Get summary mean/sd
CRWdata %>% group_by(Year) %>% rstatix::get_summary_stats(MeanMonthly, type = "mean_sd")
ggboxplot(CRWdata, x = "Year", y = "MeanMonthly")
CRWdata %>% ggplot(aes(x = MeanMonthly))+ geom_histogram(aes(y = ..count..)) #Has negatives, add constant for comparison
CRWdata <- CRWdata %>% mutate(MeanMonthly1 = MeanMonthly + 6)
CRWdata %>% ggplot(aes(x = MeanMonthly1))+ geom_histogram(aes(y = ..count..)) #Has negatives, add constant for comparison
#
#
##Initial MLR - all dat, growth
set.seed(54321)
CRW_growth <- lm(MeanMonthly1 ~ ., data = CRWdata %>% dplyr::select(-MonYr, -MeanMonthly))
CRW_growth_tab <- tidy(CRW_growth)
names(CRW_growth_tab) <- c("term", "Est.", "SE", "t", "p-value")
CRW_growth_sum <- glance(CRW_growth) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(CRW_growth_sum) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
CRW_growth_tab; CRW_growth_sum
##AIC - Model selection for final model - including YEAR
CRW_growth_step <- stepAIC(CRW_growth, direction = "backward")
set.seed(54321)
CRW_growth_final <- update(CRW_growth, .~. -TempAdj -SalAdj, data = CRWdata %>% dplyr::select(-MonYr, -MeanMonthly))
tidy(CRW_growth_final)
glance(CRW_growth_final) %>% dplyr::select(r.squared:df, deviance:df.residual)
#
(CRW_growth_modeldata <- cbind(data.frame(Growth_p = predict(CRW_growth, (CRWdata %>% dplyr::select(-MonYr, -MeanMonthly)))-6, Year = CRWdata$Year),
                               predict(CRW_growth, interval = "confidence")-6) %>% dplyr::select(-fit) %>% dplyr::select(Year, Growth_p, everything()) %>%
    group_by(Year) %>% dplyr::summarise(Growth_p = mean(Growth_p, na.rm = T), Growth_lwr = mean(lwr), Growth_upr = mean(upr)))
#
ggplot(CRWdata %>% group_by(Year) %>% summarise(mean = mean(MeanMonthly, na.rm = T)))+
  geom_point(aes(Year, mean, color = "Mean"), size = 4)+
  geom_line(data = CRW_growth_modeldata, aes(Year, Growth_p, color = "Predict"), size = 1.25)+
  geom_line(data = CRW_growth_modeldata, aes(Year, Growth_lwr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  geom_line(data = CRW_growth_modeldata, aes(Year, Growth_upr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  preztheme + theme(legend.position = c(0.85, 0.92), legend.text = element_text(size = 16))+ axistheme +
  ylab("Mean growth rate (mm/month)")+ 
  scale_x_continuous(expand = c(0.1,0), limits = c(2018, 2024), breaks = seq(2018, 2024, 1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,10)) +
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#
###Presentation fig: Growth_model_CRW -- 1000
#
#
#
#


####Water quality - LXN####
#
##Working within estuary
#
(LXNWQ_adj <- data.frame(MonYr = as.yearmon(time(detrending((All_WQ_clean %>% filter(Site == "LXN")) %>% group_by(MonYr) %>% summarise(Temperature = mean(Temperature, na.rm = T)), "Temperature"))),
                         TempAdj = detrending((All_WQ_clean %>% filter(Site == "LXN")) %>% group_by(MonYr) %>% summarise(Temperature = mean(Temperature, na.rm = T)), "Temperature"),
                         SalAdj  = detrending((All_WQ_clean %>% filter(Site == "LXN")) %>% group_by(MonYr) %>% summarise(Salinity = mean(Salinity, na.rm = T)), "Salinity"),
                         DOAdj = detrending((All_WQ_clean %>% filter(Site == "LXN")) %>% group_by(MonYr) %>% summarise(DissolvedOxygen = mean(DissolvedOxygen, na.rm = T)), "DissolvedOxygen"),
                         pHAdj = detrending((All_WQ_clean %>% filter(Site == "LXN")) %>% group_by(MonYr) %>% summarise(pH = mean(pH, na.rm = T)), "pH")))
#
LXNdata <- left_join(LXNWQ_adj, GrowthMonth %>% filter(Site == "LXN") %>% group_by(MonYr) %>% summarise(MeanMonthly = mean(Growth_rate, na.rm = T))) %>% 
  drop_na() %>% mutate(Year = as.integer(format(MonYr, "%Y")))
##Vizualize data##
#Get summary mean/sd
LXNdata %>% group_by(Year) %>% rstatix::get_summary_stats(MeanMonthly, type = "mean_sd")
ggboxplot(LXNdata, x = "Year", y = "MeanMonthly")
LXNdata %>% ggplot(aes(x = MeanMonthly))+ geom_histogram(aes(y = ..count..)) #
#
#
##Initial MLR - all dat, growth
set.seed(54321)
LXN_growth <- lm(MeanMonthly ~ ., data = LXNdata %>% dplyr::select(-MonYr))
LXN_growth_tab <- tidy(LXN_growth)
names(LXN_growth_tab) <- c("term", "Est.", "SE", "t", "p-value")
LXN_growth_sum <- glance(LXN_growth) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(LXN_growth_sum) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
LXN_growth_tab; LXN_growth_sum
##AIC - Model selection for final model - including YEAR
LXN_growth_step <- stepAIC(LXN_growth, direction = "backward")
set.seed(54321)
LXN_growth_final <- update(LXN_growth, .~. -TempAdj -DOAdj, data = LXNdata %>% dplyr::select(-MonYr))
tidy(LXN_growth_final)
glance(LXN_growth_final) %>% dplyr::select(r.squared:df, deviance:df.residual)
#
(LXN_growth_modeldata <- cbind(data.frame(Growth_p = predict(LXN_growth, (LXNdata %>% dplyr::select(-MonYr))), Year = LXNdata$Year),
                               predict(LXN_growth, interval = "confidence")) %>% dplyr::select(-fit) %>% dplyr::select(Year, Growth_p, everything()) %>%
    group_by(Year) %>% dplyr::summarise(Growth_p = mean(Growth_p, na.rm = T), Growth_lwr = mean(lwr), Growth_upr = mean(upr)))
#
ggplot(LXNdata %>% group_by(Year) %>% summarise(mean = mean(MeanMonthly, na.rm = T)))+
  geom_point(aes(Year, mean, color = "Mean"), size = 4)+
  geom_line(data = LXN_growth_modeldata, aes(Year, Growth_p, color = "Predict"), size = 1.25)+
  geom_line(data = LXN_growth_modeldata, aes(Year, Growth_lwr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  geom_line(data = LXN_growth_modeldata, aes(Year, Growth_upr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  preztheme + theme(legend.position = c(0.85, 0.92), legend.text = element_text(size = 16))+ axistheme +
  ylab("Mean growth rate (mm/month)")+ 
  scale_x_continuous(expand = c(0.1,0), limits = c(2015, 2024), breaks = seq(2015, 2024, 1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,10)) +
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#
###Presentation fig: Growth_model_LXN -- 1000
#
#
#
#
####Water quality - SLC####
#
##Working within estuary
#
(SLCWQ_adj <- data.frame(MonYr = as.yearmon(time(detrending((All_WQ_clean %>% filter(Site == "SLC")) %>% group_by(MonYr) %>% summarise(Temperature = mean(Temperature, na.rm = T)), "Temperature"))),
                         TempAdj = detrending((All_WQ_clean %>% filter(Site == "SLC")) %>% group_by(MonYr) %>% summarise(Temperature = mean(Temperature, na.rm = T)), "Temperature"),
                         SalAdj  = detrending((All_WQ_clean %>% filter(Site == "SLC")) %>% group_by(MonYr) %>% summarise(Salinity = mean(Salinity, na.rm = T)), "Salinity"),
                         DOAdj = detrending((All_WQ_clean %>% filter(Site == "SLC")) %>% group_by(MonYr) %>% summarise(DissolvedOxygen = mean(DissolvedOxygen, na.rm = T)), "DissolvedOxygen"),
                         pHAdj = detrending((All_WQ_clean %>% filter(Site == "SLC")) %>% group_by(MonYr) %>% summarise(pH = mean(pH, na.rm = T)), "pH")))
#
SLCdata <- left_join(SLCWQ_adj, GrowthMonth %>% filter(Site == "SLC") %>% group_by(MonYr) %>% summarise(MeanMonthly = mean(Growth_rate, na.rm = T))) %>% 
  drop_na() %>% mutate(Year = as.integer(format(MonYr, "%Y")))
##Vizualize data##
#Get summary mean/sd
SLCdata %>% group_by(Year) %>% rstatix::get_summary_stats(MeanMonthly, type = "mean_sd")
ggboxplot(SLCdata, x = "Year", y = "MeanMonthly")
SLCdata %>% ggplot(aes(x = MeanMonthly))+ geom_histogram(aes(y = ..count..)) #
SLCdata <- SLCdata %>% mutate(MeanMonthly1 = MeanMonthly + 5)
SLCdata %>% ggplot(aes(x = MeanMonthly1))+ geom_histogram(aes(y = ..count..)) #Has negatives, add constant for comparison

#
#
##Initial MLR - all dat, growth
set.seed(54321)
SLC_growth <- lm(MeanMonthly1 ~ ., data = SLCdata %>% dplyr::select(-MonYr, -MeanMonthly))
SLC_growth_tab <- tidy(SLC_growth)
names(SLC_growth_tab) <- c("term", "Est.", "SE", "t", "p-value")
SLC_growth_sum <- glance(SLC_growth) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(SLC_growth_sum) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
SLC_growth_tab; SLC_growth_sum
##AIC - Model selection for final model - including YEAR
SLC_growth_step <- stepAIC(SLC_growth, direction = "backward")
set.seed(54321)
SLC_growth_final <- update(SLC_growth, .~. -TempAdj -SalAdj -DOAdj -pHAdj, data = SLCdata %>% dplyr::select(-MonYr, -MeanMonthly))
tidy(SLC_growth_final)
glance(SLC_growth_final) %>% dplyr::select(r.squared:df, deviance:df.residual)
#
(SLC_growth_modeldata <- cbind(data.frame(Growth_p = predict(SLC_growth, (SLCdata %>% dplyr::select(-MonYr)))-5, Year = SLCdata$Year),
                               predict(SLC_growth, interval = "confidence")-5) %>% dplyr::select(-fit) %>% dplyr::select(Year, Growth_p, everything()) %>%
    group_by(Year) %>% dplyr::summarise(Growth_p = mean(Growth_p, na.rm = T), Growth_lwr = mean(lwr), Growth_upr = mean(upr)))
#
ggplot(SLCdata %>% group_by(Year) %>% summarise(mean = mean(MeanMonthly, na.rm = T)))+
  geom_point(aes(Year, mean, color = "Mean"), size = 4)+
  geom_line(data = SLC_growth_modeldata, aes(Year, Growth_p, color = "Predict"), size = 1.25)+
  geom_line(data = SLC_growth_modeldata, aes(Year, Growth_lwr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  geom_line(data = SLC_growth_modeldata, aes(Year, Growth_upr, color = "95% CI"), linetype = "dashed", size = 1.25)+
  preztheme + theme(legend.position = c(0.85, 0.92), legend.text = element_text(size = 16))+ axistheme +
  ylab("Mean growth rate (mm/month)")+ 
  scale_x_continuous(expand = c(0.1,0), limits = c(2015, 2024), breaks = seq(2015, 2024, 1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,10)) +
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#
###Presentation fig: Growth_model_SLC -- 1000
#
#
#
#