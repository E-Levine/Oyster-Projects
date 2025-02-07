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
               rstatix, #Summary stats
               zoo, lubridate, forecast, #Dates and times
               readxl, #Reading excel files
               car, emmeans, multcomp, #Basic analyses
               lmPerm,  
               install = TRUE)
#
#
#
#
####Load Files####
#Reading in Excel files, adding station information to dfs.
#
##Station information
Locations_raw <- read_excel("Growth_database_2025_02.xlsx", sheet = "FixedLocations", #File name and sheet name
                            skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                            na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                            .name_repair = "unique")
head(Locations_raw)
(Locations <- Locations_raw %>% mutate(Site = as.factor(paste0(Estuary, SectionName))))
#
###Water quality
Cage_WQ_raw <- read_excel("Growth_database_2025_02.xlsx", sheet = "SampleEventWQ", #File name and sheet name
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
    left_join(Locations) %>% filter(FixedLocationID %in% Locations$FixedLocationID))
#
#CRE
CR_WQ_raw <- read_excel("CR_Portal_selected_Cage_2015_2024.xlsx", #File name and sheet name
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
  left_join(Locations) %>%
    subset(MonYr > as.yearmon("12-31-2014", format = "%m-%d-%Y")))
CRE_WQ <- CR_WQ %>% subset(FixedLocationID == "0231")
CRW_WQ <- CR_WQ %>% subset(FixedLocationID == "0232")
#
#LXN
LXN_WQ_raw <- read_excel("LX_Portal_selected_Cage_2015_2024.xlsx", #File name and sheet name
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
    subset(MonYr > as.yearmon("12-31-2014", format = "%m-%d-%Y")))
#
#SLC
SLC_WQ_raw <- read_excel("SL_Portal_selected_Cage_2015_2024.xlsx", #File name and sheet name
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
    subset(MonYr > as.yearmon("12-31-2014", format = "%m-%d-%Y")))
#
#
#
#
#
###Cage Counts
Cage_counts_raw <- read_excel("Growth_database_2025_02.xlsx", sheet = "CageCount", #File name and sheet name
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
           DeadCountRate = (DeadCount/DepCount)) %>% left_join(Locations))
#
###Cage SHS
Cage_SH_raw <- read_excel("Growth_database_2025_02.xlsx", sheet = "CageSH", #File name and sheet name
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
  theme(axis.title.x = element_text(size = 18, face = "bold", color = "black"), axis.text.x = element_text(size = 16, margin = unit(c(0.5, 0.5, 0, 0.5), "cm")),
        axis.title.y = element_text(size = 18, face = "bold", color = "black"), axis.text.y = element_text(size = 16, margin = unit(c(0, 0.5, 0, 0), "cm")),
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
  ggtitle("Cage data  Feb 2005 - Sept 2024")+
  basetheme + axistheme
#
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
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(85, 62, 69, 75), label = c("a", "b", "c", "d"), fontface = "bold", size = 5)+
  ggtitle("Cage data  Feb 2005 - Sept 2024")+
  preztheme + axistheme  
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
  geom_boxplot(fill = SiteColor)+
  scale_y_continuous("Retrieved shell height (mm)", expand = c(0,0), limits = c(0,100))+
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(86, 72, 74, 76), label = c("a", "b", "c", "c"), fontface = "bold", size = 5)+
  ggtitle("Cage data  Feb 2005 - Sept 2024")+
  preztheme + axistheme
#
###Presentation fig: Site_ret_SH_ave -- 1000
#
##Plot of deployed and retrieved together
ShellHeights %>% group_by(Site) %>% dplyr::select(MonYr:CageColor, Dep_MeanSH, Ret_MeanSH) %>%
  gather("Type", "MeanSH", -MonYr, -CageCountID, -Site, -CageColor) %>%
  mutate(Type = as.factor(substr(Type, 1, 3))) %>%
  ggplot(aes(Site, MeanSH, fill = Site, pattern = Type)) +
  #geom_point(aes(alpha = interaction(Site, Type)), position = "dodge") +
  geom_boxplot_pattern(color = "black", linewidth = 0.75)+ scale_pattern_manual(values = c("none", "weave"))+
  scale_y_continuous("Shell height (mm)", expand = c(0,0), limits = c(0,100))+
  preztheme + axistheme + theme(legend.position = "none")
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
  ggtitle("Cage data  Feb 2005 - Sept 2024")+
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
  scale_y_continuous("Growth rate (mm/day)", expand = c(0,0), limits = c(0,1))+
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(0.82, 0.83, 0.89, 0.8), label = c("a", "b", "c", "b"), fontface = "bold", size = 5)+
  ggtitle("Cage data  Feb 2005 - Sept 2024")+
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
  scale_y_continuous("Growth rate (mm/month)", expand = c(0,0), limits = c(0,25))+
  ggtitle("Cage data  Feb 2005 - Sept 2024")+
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
  scale_y_continuous("Growth rate (mm/month)", expand = c(0,0), limits = c(0,25))+
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(23, 23, 24, 22), label = c("a", "b", "c", "b"), fontface = "bold", size = 5)+
  ggtitle("Cage data  Feb 2005 - Sept 2024")+
  preztheme + axistheme
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
    summarise(DeadRate = mean(DeadRate, na.rm = T),
              DeadCountRate = mean(DeadCountRate, na.rm = T),
              MissPct = (MissCount/DepCount)*100) %>% 
              mutate(Comparison = case_when(round(DeadRate,3) == round(DeadCountRate,3) ~ 0, 
                                            round(DeadRate,3) > round(DeadCountRate,3) ~ 1, 
                                            round(DeadRate,3) < round(DeadCountRate,3) ~ -1, TRUE ~ NA)) %>%
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
    ggtitle("Cage data  Feb 2005 - Sept 2024")+
    basetheme + axistheme,
  Counts_cages %>% group_by(Site) %>%
    ggplot(aes(Site, DeadCountRate))+
    geom_point()+
    geom_boxplot()+
    scale_y_continuous("Mean dead count rate", expand = c(0,0), limits = c(0,1))+
    ggtitle("Cage data  Feb 2005 - Sept 2024")+
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
  geom_boxplot(fill = SiteColor)+
  geom_jitter(width = 0.15)+
  scale_y_continuous("Mean mortality rate", expand = c(0,0), limits = c(0,1.15), breaks = seq(0, 1, by = 0.2))+
  annotate("text", x = c("CRE", "CRW", "LXN", "SLC"), y = c(1.1, 1.1, 1.1, 1.1), label = c("a", "a", "a", "b"), fontface = "bold", size = 5)+
  ggtitle("Cage data  Feb 2005 - Sept 2024")+
  preztheme + axistheme
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
  ggplot(aes(Site, DeadRate))+
  geom_point()+
  geom_boxplot()+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  ggtitle("Cage data  Feb 2005 - Sept 2024")+
  basetheme + axistheme
#
#
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
  geom_point(aes(color = Site), size = 4)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, size = 1)+
  geom_line()+
  lemon::facet_rep_grid(Site~.)+
  geom_text(aes(y = upper+10, label = Letters), size = 5) +
  scale_y_continuous("Mean deployed shell height (mm)", expand = c(0,0), limits= c(0, 90), breaks = seq(0, 90, by = 30))+
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
          Site = as.factor(Site), mm_day = Mean_growth/DaysDeployed))
(Cage_growth <- Cage_growth_raw %>% group_by(MonYr, Year, Month, CageCountID, Site) %>%
    summarise(MeanDep = mean(Dep_MeanSH, na.rm = T), MeanRet = mean(Ret_MeanSH, na.rm = T), 
              MeanCount = mean(TotalCount, na.rm = T), MeanGrowth = mean(Mean_growth, na.rm = T), DaysDeployed = mean(DaysDeployed),
              MeanDaily = MeanGrowth/DaysDeployed))
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
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25)+
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
##Permutation based ANOVA - Year for each site percent mortality
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
                                         rbind(make_cld(Annual_PctMort_tab %>% filter(Site == "CRE")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "CRE") %>% rename(Year = group, Letters = cld),
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
###Presentation fig: Site_mortality_annual -- 1000
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
names(Annual_PctMort_tidy) <- c("Site", "Factors", "df", "SS", "MS", "F", "Pr")

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
                                         rbind(make_cld(Annual_PctDead_tab %>% filter(Site == "CRE")) %>% dplyr::select(-c("spaced_cld")) %>% mutate(Site = "CRE") %>% rename(Year = group, Letters = cld),
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
###Presentation fig: Site_mortality_dead_annual -- 1000
#
#
#
###END OF SECTION
#
#
#
####Extra code####
#From Cage Shell Heights
SH_summ %>%
  ggplot(aes(MonYr, Mean_growth_mean, group = 1))+
  geom_line()+
  geom_line(aes(MonYr, Min_growth_mean, group = 1), color = "red")+
  geom_line(aes(MonYr, Max_growth_mean, group = 1), color = "blue")+
  geom_hline(yintercept = 0, linetype = "dotted")+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
SH_summ %>% 
  ggplot(aes(MonYr, Dep_MeanSH_mean, group = 1))+
  geom_line()+
  geom_line(aes(MonYr, Dep_MinSH_mean, group = 1), color = "red")+
  geom_line(aes(MonYr, Dep_MaxSH_mean, group = 1), color = "blue")+
  geom_hline(data = SH_Site_summ, aes(yintercept = Dep_MeanSH_mean), linetype = "dashed")+
  geom_hline(data = SH_Site_summ, aes(yintercept = Dep_MinSH_mean), linetype = "dashed", color = "red")+
  geom_hline(data = SH_Site_summ, aes(yintercept = Dep_MaxSH_mean), linetype = "dashed", color = "blue")+
  lemon::facet_rep_grid(Site~.)+
  basetheme + axistheme
#
##From Growth - monthly comparisons
###Does each site have different variability among Months?
ggarrange(Cage_growth %>% ggplot(aes(x = MeanGrowth)) + geom_histogram(), #normal but negative so add 10 to all values to make non-negative, continuous, normal dist
          Cage_growth %>% mutate(Growth1 = MeanGrowth + 10) %>% ggplot(aes(x = Growth1)) + geom_histogram(), nrow = 2)
(Growth_final$Growth_1 <- Growth_final$MeanGrowth + 10)
#
##Permutation based ANOVA - Month, Site##
set.seed(54321)
Growth_mon <- aovp(Growth_1 ~ Month * Site, data = Growth_final, perm = "",  nperm = 10000)
(Growth_mon_summ <- summary(Growth_mon))
Growth_mon_tidy <- tidy(Growth_mon)
names(Growth_mon_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
Growth_mon_tidy
#Significant difference among Months within Sites - detrend each Site by month -- additive since pattern is theoretically the same each time period (i.e. year)
#
Growth_final %>% group_by(Month, Site) %>% summarise(meanGrowth = mean(MeanGrowth, na.rm = T), se = sd(MeanGrowth, na.rm = T)/sqrt(length(MeanGrowth))) %>%
  ggplot(aes(Month, meanGrowth, group = 1, color = Site)) + 
  geom_line() +
  geom_errorbar(aes(ymin = meanGrowth - se, ymax = meanGrowth+se), width = 0.25)+
  geom_hline(data = Growth_final %>% group_by(Site) %>% summarise(Mean = mean(MeanGrowth)), aes(yintercept = Mean), linetype = "dashed")+
  lemon::facet_rep_grid(Site~.)+
  scale_y_continuous(limits = c(0, 15), expand = c(0,0))+
  basetheme + axistheme
#
###Detrend each parameter - additive - function "detrending"
detrending <- function(df, param){
  temp <- df %>% ungroup() %>% dplyr::select(c("MonYr", all_of(param)))
  #temp$MonYr <- as.yearmon(temp$MonYr, format = "%m/%Y")
  temp <- na.interp(as.ts(read.zoo(temp, FUN = as.yearmon)))
  temp %>% decompose("additive") -> decompTemp
  tempAdj <- temp-decompTemp$seasonal #Removes seasonal component, leaves trend and random components in final output values
  return(tempAdj)
}
#
(LXN_de <- detrending(Cage_growth %>% filter(Site == "LXN"), "MeanGrowth"))
(SLC_de <- detrending(Cage_growth %>% filter(Site == "SLC"), "MeanGrowth"))
(CRE_de <- detrending(Cage_growth %>% filter(Site == "CRE"), "MeanGrowth"))
(CRW_de <- detrending(Cage_growth %>% filter(Site == "CRW"), "MeanGrowth"))
(LXN_da <- detrending(Cage_growth %>% filter(Site == "LXN"), "MeanDaily"))
(SLC_da <- detrending(Cage_growth %>% filter(Site == "SLC"), "MeanDaily"))
(CRE_da <- detrending(Cage_growth %>% filter(Site == "CRE"), "MeanDaily"))
(CRW_da <- detrending(Cage_growth %>% filter(Site == "CRW"), "MeanDaily"))
#
#Get dataframe of detrended data and add to growth data for final data frame
(Growth_detrended <- left_join(
  left_join(data.frame(MonYr = as.yearmon(time(LXN_de)), LXN_de, SLC_de),
            data.frame(MonYr = as.yearmon(time(CRE_de)), CRE_de)),
  data.frame(MonYr = as.yearmon(time(CRW_de)), CRW_de)) %>% rename(LXN = LXN_de, SLC = SLC_de, CRE = CRE_de, CRW = CRW_de) %>%
    gather("Site", "Growth_de", -MonYr))
(Daily_detrended <- left_join(
  left_join(data.frame(MonYr = as.yearmon(time(LXN_de)), LXN_da, SLC_da),
            data.frame(MonYr = as.yearmon(time(CRE_de)), CRE_da)),
  data.frame(MonYr = as.yearmon(time(CRW_da)), CRW_da)) %>% rename(LXN = LXN_da, SLC = SLC_da, CRE = CRE_da, CRW = CRW_da) %>%
    gather("Site", "Growth_da", -MonYr))
(Growth_final <- left_join(Cage_growth, Growth_detrended) %>% left_join(Daily_detrended) %>% mutate(Site = as.factor(Site)))
#
ggarrange(
  Growth_final %>% 
    ggplot(aes(MonYr, MeanGrowth, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Growth_final %>% 
    ggplot(aes(MonYr, Growth_de, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Growth_final %>% 
    ggplot(aes(MonYr, Growth_da, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  nrow = 1, ncol = 3)
#
ggarrange(
  Growth_final %>% 
    ggplot(aes(MonYr, Growth_de, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Growth_final %>% group_by(Site, Year) %>% summarise(MeanDe = mean(Growth_de, na.rm = T)) %>%
    ggplot(aes(Year, MeanDe, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  Growth_final %>% group_by(Site, Month) %>% summarise(MeanDe = mean(Growth_de, na.rm = T)) %>% 
    ggplot(aes(Month, MeanDe, group = 1))+
    geom_line()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth()+
    lemon::facet_rep_grid(Site~.)+
    basetheme + axistheme,
  nrow = 1, ncol = 3)
#
(Growth_final$Growth_de1 <- Growth_final$Growth_de + 10)
#
#