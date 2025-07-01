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
                             na = c("", "NULL", " ", "NAN", "na"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
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
         Month = factor(format(Date, "%b"), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         MonYr = as.yearmon(format(Date, "%b %Y")),
         SH_Bin = cut(SH, breaks = seq(0, 5*ceiling(max(SH, na.rm = T)/5), by = 5))) %>%
  filter(Bad_Slide == "No" & M_F == "No") %>% 
  mutate(Stage = as.factor(case_when(grepl("first spawn", Comments) ~ "0", TRUE ~ Stage)))
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
##Colors to stage
StageColor <- c("#666666", "#993333", "#E69F00", "#009E73", "#56B4E9", "#FFFFFF")
names(StageColor) <- levels(Repro_df$Stage)
#
#
#
#END OF SECTION
#
#
#
####Compile cleaned data for summary and analyses####
#
##Cleaned data for ratio analyes:
(Ratio_clean_df <- Repro_df %>% 
   #Remove Zs and group by stations and Sex
   filter(Sex != "Z") %>% group_by(Year, Month, Site, Station, Sex) %>% droplevels() %>%
   #Get count of each M and F sample
   summarise(Count = n(), .groups = 'drop') %>%
   #Add in missing counts (when M or F were 0)
   complete(Year, Month, Site, Station, Sex, fill = list(Count = 0)) %>%
   #Add in Total number of samples
   left_join(Repro_df %>% 
                #Remove Zs and group by stations
                filter(Sex != "Z") %>% group_by(Year, Month, Site, Station) %>% droplevels() %>%
                summarise(Total = n(), .groups = 'drop')) %>%
   #Removing 4-5/2020 since no samples collected
   filter(!(Year == '2020' & Month == 'Apr') & !(Year == '2020' & Month == 'May')) & 
   !(Year == '2025' & Month %in% c("Jan","Feb", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
   #Calculate ratios
   mutate(Ratio = Count/Total))
#
glimpse(Ratio_clean_df)
#
#
##END OF SECTION
#
#
####Review of data - overall summary tables and figures####
#
####What is the average ratio of males:females overall within each site? - LXN vs LXS
(Site_ratios <- Ratio_clean_df %>% 
  #Get count per Sex
  group_by(Site, Sex) %>% summarise(MeanRatio = mean(Ratio, na.rm = T)))
#
Site_ratios %>% 
    ggplot(aes(Sex, MeanRatio))+
    geom_bar(stat = "identity", fill = c(SexColor, SexColor)) +
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) +  
    facet_rep_grid(.~Site)+
    basetheme + facettheme
#
#Very similar among sites when just looking at M and F - compare to be sure
(Site_cont_tab <- table((Ratio_clean_df %>% filter(Count >0) %>% droplevels())$Site, (Ratio_clean_df %>% filter(Count >0) %>% droplevels())$Sex)) #Create a contingency table
chisq.test(Site_cont_tab) #Perform Chi-squared test
#p = 0.8167 - fail to reject null that they are the same >> LXN M:F = LXS M:F
#p = 0.8833 - fail to reject null that they are the same >> LXN M:F = LXS M:F (2020-2024 data)
#
#
####What is the average ratio of males:females per year? - LXN + LXS
(Annual_ratios <- Ratio_clean_df %>% 
    #Get count per Sex
    group_by(Year, Sex) %>% summarise(MeanRatio = mean(Ratio, na.rm = T)))
#
Annual_ratios %>% filter(Year != "2025") %>%
  ggplot(aes(Year, MeanRatio, fill = Sex))+
  geom_bar(stat = "identity") +
  #geom_line(aes(group = Sex, color = Sex), linewidth = 1)+ geom_point(aes(color = Sex), size = 4)+ 
  scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
  scale_fill_manual(values = SexColor)+ scale_color_manual(values = SexColor)+
  basetheme + facettheme
#
#
#
####What is the average ratio of males:females per month? - LXN + LXS
(Monthly_ratios <- Ratio_clean_df %>% filter(Year != "2025") %>%
    #Get count per Sex
    group_by(Month, Sex) %>% summarise(MeanRatio = mean(Ratio, na.rm = T)))
#
Monthly_ratios %>% 
  ggplot(aes(Month, MeanRatio, fill = Sex))+
  geom_bar(stat = "identity") +
  #geom_line(aes(group = Sex, color = Sex), linewidth = 1)+ geom_point(aes(color = Sex), size = 4)+ 
  scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
  scale_fill_manual(values = SexColor)+ scale_color_manual(values = SexColor)+
  basetheme + facettheme
#
#
#
####What is the average ratio of males:females overtime (MonYr)? - LXN + LXS
(MonYr_ratios <- Ratio_clean_df %>% filter(Year != "2025") %>%
    mutate(MonYr = as.yearmon(paste(Month, Year))) %>%
    #Get count per Sex
    group_by(MonYr, Sex) %>% summarise(MeanRatio = mean(Ratio, na.rm = T)))
#
MonYr_ratios %>% 
  ggplot(aes(MonYr, MeanRatio, fill = Sex))+
  #geom_bar(stat = "identity") +
  geom_line(aes(group = Sex, color = Sex), linewidth = 1)+ geom_point(aes(color = Sex), size = 4)+ 
  scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
  scale_x_continuous(expand = c(0,0))+
  scale_fill_manual(values = SexColor)+ scale_color_manual(values = SexColor)+
  basetheme + facettheme
#