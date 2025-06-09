##Sex ratio and reproduction of Loxahatchee oytsers
#
#Working project: LHarmon, ELevine
#
#
#Load packages, install as needed
#if (!require("remotes")) install.packages("remotes")
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               ggpubr, scales, lemon,
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
                             na = c("", "Z", "z", " ", "NAN", "na"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
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
  mutate(across(c(Site, Station, Sex, Stage), as.factor),
         Sex = as.factor(case_when(is.na(Sex) ~ "Z", TRUE ~ Sex)),
         Year = as.factor(format(Date, "%Y")),
         Month = as.factor(format(Date, "%b")),
         MonYr = as.yearmon(format(Date, "%b %Y")),
         SH_Bin = cut(SH, breaks = seq(0, 5*ceiling(max(SH, na.rm = T)/5), by = 5))) %>%
  filter(Bad_Slide == "No" & M_F == "No")
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
  mutate(across(c(Site, Station), as.factor),
         Year = as.factor(format(Date, "%Y")),
         Month = as.factor(format(Date, "%b")),
         MonYr = as.yearmon(format(Date, "%b %Y")))
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
##Colors to sex
SexZColor <- c("#CC79A7", "#56B4E9", "#E69F00")
names(SexZColor) <- levels(Repro_df$Sex)
SexColor <- c("#CC79A7", "#56B4E9")
names(SexColor) <- c("F", "M")
#
#
#
#END OF SECTION
#
#

#####Review of data - overall summary tables and figures####
#
###All estuary data - LXN + LXS
#
(Repro_stations_noZ <- left_join(
  Repro_df %>% group_by(Year, Month, MonYr, Site, Station, Sex) %>% filter(Sex != "Z") %>% summarise(Count = n()),
  Repro_df %>% group_by(Year, Month, MonYr, Site, Station) %>% filter(Sex != "Z") %>% summarise(Total = n())) %>%
  mutate(Ratio = Count/Total, Prop = Ratio * 100))
#
##What is the average ratio of males:females?
(LX_ratios <- left_join(
  #Group by sex and get count of each (including Zs)
  Repro_stations %>% group_by(Sex) %>% summarise(All_Count = n()) %>% 
    mutate(All_Total = sum(All_Count), All_Ratio = All_Count/All_Total),
  #Group by sex and get count of each (excluding Zs)
  Repro_stations %>% filter(Sex != "Z") %>% group_by(Sex) %>% summarise(MF_Count = n()) %>% 
    mutate(MF_Total = sum(MF_Count), MF_Ratio = MF_Count/MF_Total)))
#
ggarrange(
  LX_ratios %>% 
    ggplot(aes(Sex, All_Ratio, fill = Sex))+
    geom_bar(stat = "identity", fill = SexZColor) +
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
    scale_fill_manual(values = SexColor)+
    basetheme,
  LX_ratios %>% filter(Sex != "Z") %>%
    ggplot(aes(Sex, MF_Ratio, fill = Sex))+
    geom_bar(stat = "identity", fill = SexColor) +
    scale_x_discrete(labels = c("F", "M", ""))+
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
    scale_fill_manual(values = SexColor)+
    basetheme)
#
#
###Has the ratio changed over time? - not including Zs for now
(LX_annual_ratios <- left_join(Repro_stations %>% filter(Sex != "Z") %>% group_by(Sex, Year) %>% summarise(Count = n()),
                               Repro_stations %>% filter(Sex != "Z") %>% group_by(Year) %>% summarise(Total = n())) %>% 
    mutate(Ratio = Count/Total))
#
LX_annual_ratios %>%
  ggplot(aes(Year, Ratio, fill = Sex))+
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = SexColor)+
  scale_y_continuous(expand = c(0,0))+
  basetheme
#
#
##Plotting all Months/Years
left_join(Repro_stations %>% filter(Sex != "Z") %>% group_by(Sex, MonYr) %>% summarise(Count = n()),
          Repro_stations %>% filter(Sex != "Z") %>% group_by(MonYr) %>% summarise(Total = n())) %>% 
  mutate(Ratio = Count/Total) %>%
  ggplot(aes(MonYr, Ratio, fill = Sex))+
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = SexColor)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  basetheme
#
#
###How do months differ in M:F?
(Monthly_ratios <- left_join(
  Repro_stations_noZ %>% filter(Sex != "Z" & Site == "LX-N") %>% group_by(Sex, MonYr) %>% summarise(Count = n()),
  Repro_stations_noZ %>% filter(Sex != "Z" & Site == "LX-N") %>% group_by(MonYr) %>% summarise(Total = n()))  %>%
  mutate(Ratio = Count/Total,
         Prop = Ratio * 100))
Repro_stations_noZ %>% group_by(MonYr, Sex) %>% summarise(Ave = mean(Ratio))

Monthly_ratios %>%
  ggplot(aes(Month, Ratio, fill = Sex))+
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = SexColor)+
  scale_y_continuous(expand = c(0,0))+
  basetheme
#
#
#
#
#
#
####What is the average ratio of males:females within each site? - LXN vs LXS
(Site_ratios <- left_join(
  #Group by sex and get count of each (including Zs)
  Repro_df %>% group_by(Site, Sex) %>% summarise(All_Count = n()) %>% 
    mutate(All_Total = sum(All_Count), All_Ratio = All_Count/All_Total),
  #Group by sex and get count of each (excluding Zs)
  Repro_df %>% filter(Sex != "Z") %>% group_by(Site, Sex) %>% summarise(MF_Count = n()) %>% 
    mutate(MF_Total = sum(MF_Count), MF_Ratio = MF_Count/MF_Total)))
#
ggarrange(
  Site_ratios %>% 
    ggplot(aes(Sex, All_Ratio, fill = Sex))+
    geom_bar(stat = "identity", fill = c(SexZColor, SexZColor)) +
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) +  
    facet_rep_grid(.~Site)+
    basetheme + facettheme,
  Site_ratios %>% filter(Sex != "Z") %>%
    ggplot(aes(Sex, MF_Ratio, fill = Sex))+
    geom_bar(stat = "identity", fill = c(SexColor,SexColor)) +
    scale_x_discrete(labels = c("F", "M", ""))+
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) +  
    facet_rep_grid(.~Site)+
    basetheme + facettheme)
#Very similar among sites when just looking at M and F - compare to be sure
(Site_cont_tab <- table((Repro_df %>% subset(Sex != "Z") %>% droplevels())$Site, (Repro_df %>% subset(Sex != "Z") %>% droplevels())$Sex)) #Create a contingency table
chisq.test(Site_cont_tab) #Perform Chi-squared test
#p = 0.5799 - fail to reject null that they are the same >> LXN M:F = LXS M:F
#
(SiteZ_cont_tab <- table(Repro_df$Site, Repro_df$Sex)) #Create a contingency table
chisq.test(SiteZ_cont_tab) #Perform Chi-squared test
#p = 0.002 - reject null when Zs are include >> LXN M:F:Z != LXS M:F:Z
#
#
#
#
#END OF SECTION
#
#
#####Ratios by shell heights -  summary tables and figures####
#
###All estuary data - LXN + LXS
#
##What is the average ratio of males:females per SH bin?
(LX_SH_Bin_ratios <- left_join(
  #Group by sex and get count of each (including Zs)
  left_join(Repro_df %>% group_by(Sex, SH_Bin) %>% summarise(All_Count = n()), Repro_df %>% group_by(SH_Bin) %>% summarise(All_Total = n())) %>% 
    mutate(All_Ratio = All_Count/All_Total),
  #Group by sex and get count of each (excluding Zs)
  left_join(Repro_df %>% filter(Sex != "Z") %>% group_by(Sex, SH_Bin) %>% summarise(MF_Count = n()), Repro_df %>% filter(Sex != "Z") %>% group_by(SH_Bin) %>% summarise(MF_Total = n())) %>% 
    mutate(MF_Ratio = MF_Count/MF_Total)))
#
#Compare ratios
ggarrange(
  LX_SH_Bin_ratios %>% 
    ggplot(aes(SH_Bin, All_Ratio, fill = Sex))+
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
    scale_fill_manual(values = SexZColor)+
    basetheme,
  LX_SH_Bin_ratios %>% filter(Sex != "Z") %>%
    ggplot(aes(SH_Bin, MF_Ratio, fill = Sex))+
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = SexColor)+
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
    basetheme,
  nrow = 2)
#
#
#
Repro_df %>% mutate(Sex = factor(Sex, ordered = TRUE, levels = c("Z", "M", "F"))) %>%
  ggplot()+
  geom_jitter(aes(SH, Sex, color = Sex), height = 0.1)+
  geom_point(data = Repro_df %>% mutate(Sex = factor(Sex, ordered = TRUE, levels = c("Z", "M", "F"))) %>% group_by(Sex) %>% summarise(SH = mean(SH)),
             aes(SH, Sex), color = "black", size = 10)+
  scale_x_continuous(expand = c(0,0), limits = c(0, 100))+
  scale_y_discrete(expand = c(0,0.5))+
  basetheme
