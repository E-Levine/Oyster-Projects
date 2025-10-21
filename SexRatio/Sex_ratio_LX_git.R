##Sex ratio and reproduction of Loxahatchee oysters
#
#Working project: LHarmon, ELWilliams
#Working with all data 2020-09/2025
#
#
#Load packages, install as needed
#if (!require("remotes")) install.packages("remotes")
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               ggpubr, scales, lemon, ggnewscale, #ploting
               rstatix, broom, #Summary stats
               zoo, lubridate, forecast, #Dates and times
               readxl, #Reading excel files
               car, emmeans, multcomp, #Basic analyses
               lmPerm, stats, glmmTMB, AICcmodavg, DHARMa, performance,
               install = TRUE)
#
#
#
#####Load data files####
#
###Dermo data
Dermo_raw <- read_excel("Data/LX Combined Raw Data.xlsx", sheet = "Database_dermo", #File name and sheet name
                        skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                        na = c("", "NULL", " ", "NAN", "na"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                        .name_repair = "unique") 
#Add 5 mm SH bins
Dermo_df <- Dermo_raw %>% 
  mutate(Year = substr(SampleEventID, 8, 11),
         SH_Bin = cut(ShellHeight, breaks = seq(0, 5*ceiling(max(ShellHeight, na.rm = T)/5), by = 5)))
#
#
###Repro data
Repro_data_raw <- read_excel("Data/LX Combined Raw Data.xlsx", sheet = "Database_repro", #File name and sheet name
                             skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                             na = c("", "NULL", " ", "NAN", "na"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                             .name_repair = "unique")
#check data types 
glimpse(Repro_data_raw)
#Clean data:
Repro_df_raw <- Repro_data_raw %>% 
  mutate(Date = as.Date(substr(SampleEventID, 8, 16), format = "%Y%m%d")) %>%
  #update data types/values as needed
  mutate(Site = as.factor(substr(OysterID, 1, 3)),
         Station = as.factor(substr(SampleEventID, 19, 22)),
         Sample_num = as.factor(substr(OysterID, 10, 11)),
         Sex = as.factor(case_when(is.na(Sex) ~ "Z", TRUE ~ Sex)),
         M_F = as.factor(case_when(Sex == "M/F" ~ "Yes", TRUE ~ "No")),
         Year = as.factor(format(Date, "%Y")),
         Month = factor(format(Date, "%b"), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         MonYr = as.yearmon(format(Date, "%b %Y"))) %>%
  #Add SH data
  left_join((Dermo_df %>% dplyr::select(OysterID, ShellHeight, SH_Bin))) %>%
  #Reorder columns
  dplyr::select(Date, Year:MonYr, Site, Station, Sample_num, OysterID, SampleEventID, ShellHeight, SH_Bin, M_F, everything())
#
#Get data to work with:
Repro_df <- Repro_df_raw %>% 
  mutate(BadSlide = case_when(Sex == "Z" & ReproStage == "Z" ~ "Y", 
                              Sex != "Z" ~ "N", 
                              TRUE ~ BadSlide)) %>% 
  filter(M_F == "No") %>% 
  mutate(Sex = as.factor(case_when(Sex == "M/F" ~ "Z", TRUE ~ Sex)),
         ReproStage = as.factor(case_when(grepl("first spawn", Comments) ~ "0", TRUE ~ ReproStage)))
#across(c(Site, Station, Sex, Stage, Sample_num), as.factor),
glimpse(Repro_df)
#
#
###FWRI WQ data
Molluscs_WQ_Raw <- read_excel("Data/LX Combined Raw Data.xlsx", sheet = "Database_WQ", #File name and sheet name
                              skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                              na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                              .name_repair = "unique")
#check data types
glimpse(Molluscs_WQ_Raw)
#
MollWQ_df <- Molluscs_WQ_Raw %>% 
  #Add Station columns
  mutate(Station = as.factor(substr(SampleEventID, 19, 22))) %>%
  #Add Site, Year, Month, MonYr info
  left_join(Repro_df %>% dplyr::select(SampleEventID, Date, Year, Month, MonYr, Site)) %>%
  #Reorder columns
  dplyr::select(Date:Site, Station, everything())
#
glimpse(MollWQ_df)
#
#
##Load portal WQ file -need to update data
Portal_WQ_Raw <- read_excel("Data/LX_Portal_combined_filtered_2020_082023.xlsx", sheet = "Sheet1", #File name and sheet name
                              skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                              na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                              .name_repair = "unique")
#check data types
glimpse(Portal_WQ_Raw)
#Clean data
Portal_WQ <- Portal_WQ_Raw %>% 
  dplyr::select(MonitoringLocationIdentifier:Estuary, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, CharacteristicName:ResultMeasureValue) %>%
  subset(Estuary == "LX" & (CharacteristicName == "Salinity"|CharacteristicName == "Temperature, water")) %>% 
  pivot_wider(names_from = CharacteristicName, values_from = ResultMeasureValue, values_fn = sum) %>%
  rename("Temp" = `Temperature, water`, "Station" = MonitoringLocationIdentifier, "Lat" = LatitudeMeasure, "Long" = LongitudeMeasure) 
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
XCate <- theme(axis.title.x = element_blank(),
               axis.text.x = element_text(color = "black", size = 14, family = "serif",
                                          margin = unit(c(0.5, 0.5, 0, 0.5), "cm")),
               plot.margin = margin(0.25, 0.5, 0.25, 0.25, "cm"))
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
SexColorC <- c("#CC79A7", "#56B4E9")
names(SexColorC) <- c("1", "0")
#
##Colors to stage
StageColor <- c("#666666", "#993333", "#E69F00", "#009E73", "#56B4E9", "#FFFFFF")
names(StageColor) <- levels(Repro_df$ReproStage)
#
#
#
#END OF SECTION
#
#
#
####Summary info, ratio df####
#
##How many M/F all data 2020 - current
Repro_df_raw %>% group_by(Sex) %>%
  summarise(Count = n()) %>%
  mutate(Total = as.numeric(paste(Repro_df_raw %>% summarise(Count = n())))) %>%
  mutate(Proportion = Count/Total)
#
##Check sample coverage
Repro_df %>% group_by(Site, MonYr) %>%
  summarise(Count = n()) %>%
  ggplot(aes(MonYr, Count))+
  geom_col()+
  lemon::facet_rep_grid(Site~.)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 60))+
  basetheme + axistheme
##Need to check missing data - check data sheets.
#
#
#
##Cleaned data for ratio analyses (w/o Z):
(Ratio_clean_df <- Repro_df %>%  
   #Remove Zs and group by stations and Sex
   filter(Sex != "Z") %>% group_by(Year, Month, Site, Station, Sex) %>% droplevels() %>%
   #Get count of each M and F sample
   summarise(Count = n(), .groups = 'drop') %>%
   #Add in missing counts (when M or F were 0) 
   mutate(SiteStation = as.factor(paste0(Site, Station))) %>% 
   complete(Year, Month, SiteStation, Sex, fill = list(Count = 0)) %>%
   mutate(Site = case_when(is.na(Site) ~ substr(SiteStation, 1, 3), TRUE ~ Site),
          Station = case_when(is.na(Station) ~ substr(SiteStation, 4, 7), TRUE ~ Station)) %>%
   #Add in Total number of samples
   left_join(Repro_df %>% 
                #Remove Zs and group by stations
                filter(Sex != "Z") %>% group_by(Year, Month, Site, Station) %>% droplevels() %>%
                summarise(Total = n(), .groups = 'drop')) %>%
   #Removing 4-5/2020 since no samples collected
   filter(!(Year == '2020' & Month == 'Apr') & !(Year == '2020' & Month == 'May')) %>% # & 
            #!(Year == '2025' & Month %in% c("Jan","Feb", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
   #Calculate ratios
   mutate(Ratio = Count/Total))
#
#
glimpse(Ratio_clean_df)
#
#
##Cleaned data for ratio analyses (w/ Z):
(Ratio_Z_df <- Repro_df %>%  
    #Group by stations and Sex
    group_by(Year, Month, Site, Station, Sex) %>% droplevels() %>%
    #Get count of each M and F sample
    summarise(Count = n(), .groups = 'drop') %>%
    #Add in missing counts (when M or F were 0) 
    mutate(SiteStation = as.factor(paste0(Site, Station))) %>% 
    complete(Year, Month, SiteStation, Sex, fill = list(Count = 0)) %>%
    mutate(Site = case_when(is.na(Site) ~ substr(SiteStation, 1, 3), TRUE ~ Site),
           Station = case_when(is.na(Station) ~ substr(SiteStation, 4, 7), TRUE ~ Station)) %>%
    #Add in Total number of samples
    left_join(Repro_df %>% 
                #Group by stations
                group_by(Year, Month, Site, Station) %>% droplevels() %>%
                summarise(Total = n(), .groups = 'drop')) %>%
    #Removing 4-5/2020 since no samples collected
    filter(!(Year == '2020' & Month == 'Apr') & !(Year == '2020' & Month == 'May')) %>% # & 
    #!(Year == '2025' & Month %in% c("Jan","Feb", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
    #Calculate ratios
    mutate(Ratio = Count/Total))
#
#
##END OF SECTION
#
#
####Site comparisons####
#
####What is the average ratio of males:females overall within each site? - LXN vs LXS
(Site_ratios <- #Ratio_clean_df %>% #Get count per Sex group_by(Site, Sex) %>% summarise(MeanRatio = mean(Ratio, na.rm = T)))
   Ratio_clean_df %>%
   #Get count per Year and Sex
   group_by(Site, Sex) %>% 
   summarise(Count = sum(Count, na.rm = T)) %>% 
   pivot_wider(names_from = Sex, values_from = Count) %>% 
   mutate(Total = sum(F, M), Ratio_F = F/Total, Ratio_M = M/Total) %>%
   pivot_longer(cols = c(Ratio_M, Ratio_F), names_to = c("Column", "Sex"), names_sep = "_", values_to = "Ratio") %>%
   mutate(Sex = factor(Sex, levels = c("F", "M"))) %>%
   dplyr::select(-c("F", "M", "Column")))
#
Site_ratios %>% 
    ggplot(aes(Sex, Ratio))+
    geom_bar(stat = "identity", fill = c(rev(SexColor), rev(SexColor))) +
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) +  
    facet_rep_grid(.~Site)+
    basetheme + facettheme#
#Very similar among sites when just looking at M and F - compare to be sure
(Site_cont_tab <- table((Ratio_clean_df %>% filter(Count >0) %>% droplevels())$Site, (Ratio_clean_df %>% filter(Count >0) %>% droplevels())$Sex)) #Create a contingency table
chisq.test(Site_cont_tab) #Perform Chi-squared test
#p = 0.8167 - fail to reject null that they are the same >> LXN M:F = LXS M:F
#p = 0.8833 - fail to reject null that they are the same >> LXN M:F = LXS M:F (2020-2024 data)
#p = 0.8639 - fail to reject null that they are the same >> LXN M:F = LXS M:F (2020-08/2025 data)
#
###END OF SECTION
#
####Year comparisons#####
#
####What is the average ratio of males:females per year? - LXN + LXS
(Annual_ratios <- Ratio_clean_df %>%
    #Get count per Year and Sex
   group_by(Year, Sex) %>% 
   summarise(Count = sum(Count, na.rm = T)) %>% 
   pivot_wider(names_from = Sex, values_from = Count) %>% 
   mutate(Total = sum(F, M), Ratio_F = F/Total, Ratio_M = M/Total) %>%
   pivot_longer(cols = c(Ratio_M, Ratio_F), names_to = c("Column", "Sex"), names_sep = "_", values_to = "Ratio") %>%
   dplyr::select(-c("F", "M", "Column")))
#
Annual_ratios %>% #filter(Year != "2025") %>%
  ggplot(aes(Year, Ratio, fill = Sex))+
  geom_bar(stat = "identity") +
  #geom_hline(aes(yintercept = 0), linetype = "dashed")+
  #geom_line(aes(group = Sex, color = Sex), linewidth = 1)+ geom_point(aes(color = Sex), size = 4)+ geom_errorbar(aes(color = Sex, ymin = mean-se, ymax = mean+se))+ scale_color_manual(values = SexColor)+
  scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
  scale_fill_manual(values = SexColor)+ 
  basetheme + facettheme
#
#
#INCLUDING Zs
####What is the average ratio of males:females per year? - LXN + LXS
(Annual_Z_ratios <- Ratio_Z_df %>%
    #Get count per Sex
    group_by(Year, Sex) %>% get_summary_stats(Ratio, show = c("mean", "sd", "se")))
#
Annual_Z_ratios %>% dplyr::select(Year, Sex, mean)
#
Annual_Z_ratios %>% #filter(Year != "2025") %>%
  ggplot(aes(Year, mean, fill = Sex))+
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  #geom_line(aes(group = Sex, color = Sex), linewidth = 1)+ geom_point(aes(color = Sex), size = 4)+ geom_errorbar(aes(color = Sex, ymin = mean-se, ymax = mean+se))+ scale_color_manual(values = SexColor)+
  scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1.01)) + 
  scale_fill_manual(values = SexZColor)+ 
  basetheme + facettheme
#
#
##WORKING IDEA, see GLM for analyses:
#
#Beta regression:
library(betareg)
annual_F_model <- betareg(mean ~ Year, data = Annual_ratios %>% filter(Sex == "F" & Year != "2025"),
                          control = betareg.control(maxit = 1000))
summary(annual_F_model)
#No sig diff among years.
## each month = 1 smaple
(f_proportions <- Ratio_clean_df %>% 
  #Get count per Sex
  group_by(Year, Month, Sex) %>% get_summary_stats(Ratio, show = c("mean", "sd", "se")) %>% 
    filter(Sex == "F" & Year != "2025") %>% dplyr::select(Year, Month, mean) %>% 
    pivot_wider(names_from = Month, values_from = mean) %>% arrange(Year))
#
library(vegan)
distance_matrix <- vegdist(temp_df[-c(1)], method = "bray", na.rm = TRUE)
F_annaul_perm <- adonis2(f_proportions[-1] ~ c("2020", "2021", "2022", "2023", "2024"), method = "bray", permutations = 999, na.rm = TRUE)
#
#
##END OF SECTION
#
#
####Month comparisons#####
#
#What is the average ratio of males:females per month? - LXN + LXS
(Monthly_ratios <- Ratio_clean_df %>% #filter(Year != "2025") %>% #Get count per Sex group_by(Month, Sex) %>% get_summary_stats(Ratio, show = c("mean", "sd", "se")))
   #Get count per Year and Sex
   group_by(Month, Sex) %>% 
   summarise(Count = sum(Count, na.rm = T)) %>% 
   pivot_wider(names_from = Sex, values_from = Count) %>% 
   mutate(Total = sum(F, M), Ratio_F = F/Total, Ratio_M = M/Total) %>%
   pivot_longer(cols = c(Ratio_M, Ratio_F), names_to = c("Column", "Sex"), names_sep = "_", values_to = "Ratio") %>%
   dplyr::select(-c("F", "M", "Column")))
#
Monthly_ratios %>% 
  ggplot(aes(Month, Ratio, fill = Sex))+
  #geom_bar(stat = "identity") +
  geom_line(aes(group = Sex, color = Sex), linewidth = 1)+ geom_point(aes(color = Sex), size = 4)+ scale_color_manual(values = SexColor)+
  scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
  scale_fill_manual(values = SexColor)+ 
  basetheme + facettheme
#
##Monthly ratio per year:
ggarrange(
Ratio_clean_df %>% #filter(Year != "2025") %>%
  #Get count per Sex
  group_by(Year, Month, Sex) %>% get_summary_stats(Ratio, show = c("mean", "sd", "se")) %>%
  ggplot(aes(Month, mean, fill = Sex))+
  #geom_bar(stat = "identity") +
  geom_line(aes(group = Sex, color = Sex), linewidth = 1)+ geom_point(aes(color = Sex), size = 4)+ geom_errorbar(aes(color = Sex, ymin = mean-se, ymax = mean+se)) + scale_color_manual(values = SexColor)+
  scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
  lemon::facet_rep_grid(Year~.)+
  scale_fill_manual(values = SexColor)+ 
  basetheme + facettheme,
#
Repro_df %>% filter(Sex != "Z") %>%
  group_by(Year, Month, Sex) %>% get_summary_stats(ShellHeight, show = c("mean", "sd", "se")) %>%
  ggplot(aes(Month, mean, fill = Sex))+
  geom_line(aes(group = Sex, color = Sex), linewidth = 1)+ geom_point(aes(color = Sex), size = 4)+ geom_errorbar(aes(color = Sex, ymin = mean-se, ymax = mean+se)) + scale_color_manual(values = SexColor)+
  geom_hline(data = Repro_df %>% filter(Sex != "Z") %>%
               group_by(Year) %>% get_summary_stats(ShellHeight, show = c("mean", "sd", "se")), 
             aes(yintercept = mean), linetype = "dashed")+
  lemon::facet_rep_grid(Year~.)+
  scale_fill_manual(values = SexColor)+ scale_y_continuous("Mean shell height (mm)")+
  basetheme + facettheme
)
#
####What is the average ratio of males:females overtime (MonYr)? - LXN + LXS
(MonYr_ratios <- Ratio_clean_df %>% 
    filter(Year != "2025") %>% mutate(MonYr = as.yearmon(paste(Month, Year))) %>% #Get count per Sex group_by(MonYr, Sex) %>% summarise(MeanRatio = mean(Ratio, na.rm = T)))
    #Get count per Year and Sex
    group_by(MonYr, Sex) %>% 
    summarise(Count = sum(Count, na.rm = T)) %>% 
    pivot_wider(names_from = Sex, values_from = Count) %>% 
    mutate(Total = sum(F, M), Ratio_F = F/Total, Ratio_M = M/Total) %>%
    pivot_longer(cols = c(Ratio_M, Ratio_F), names_to = c("Column", "Sex"), names_sep = "_", values_to = "Ratio") %>%
    dplyr::select(-c("F", "M", "Column")))
#
MonYr_ratios %>% 
  ggplot(aes(MonYr, Ratio, fill = Sex))+
  #geom_bar(stat = "identity") +
  geom_line(aes(group = Sex, color = Sex), linewidth = 1)+ geom_point(aes(color = Sex), size = 4)+ 
  scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
  scale_x_continuous(expand = c(0,0))+
  scale_fill_manual(values = SexColor)+ scale_color_manual(values = SexColor)+
  basetheme + facettheme
#
#
#
##END OF SECTION
#
####GLM sex####
#
##Get all data together - not currently using Portal WQ
(Repro_WQ <- left_join(Repro_df %>% filter(!grepl("no slide", Comments, ignore.case = TRUE)) %>%
                         dplyr::select(Date:SH_Bin, Sex, ReproStage),
                      MollWQ_df %>% dplyr::select(Date:Salinity), relationship = "many-to-many") %>%
  mutate(Season = case_when(month(Date) >= 1 & month(Date) <= 3 ~ "Spring",
                            month(Date) >= 4 & month(Date) <= 6 ~ "Summer",
                            month(Date) >= 7 & month(Date) <= 9 ~ "Fall",
                            month(Date) >= 10 & month(Date) <= 12 ~ "Winter",
                            TRUE ~ "Invalid")))
##Scaling data
(Repro_WQ2 <- Repro_WQ %>%
    #Nuerical
  mutate(Year_scaled = scale(as.numeric(Year))[,1],
         SH_scaled = scale(ShellHeight)[,1],
         Salinity_scaled = scale(Salinity)[,1],
         Temp_scaled = scale(Temperature)[,1]) %>%
    #Factors
  mutate(Site = factor(Site, levels = c("LXN", "LXS")),
         Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")),
         Month = factor(Month, levels = 1:12, labels = month.abb),
         Year_numeric = as.numeric(Year),  # For linear trends
         Station = factor(Station)) %>%
    mutate(Sex_class = ifelse(Sex == "M", 0, 1)) %>%
    #Remove Z Sex and oysters without shell heights
    subset(Sex != "Z") %>% subset(!is.na(ShellHeight)))
#
#
###Initial models
#All possible independent variables: Year, Season, Site, Station, ShellHeight, Salinity, Temperature#
#Starting with Site and ShellHeight
#
#### 1-Site/Section ONLY as a fixed effect
model.Site <- glmmTMB(Sex_class ~ Site, family = binomial, data = Repro_WQ2,
                   control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#
#### 2-ShellHeight ONLY as a fixed effect
model.SH <- glmmTMB(Sex_class ~ SH_scaled, family = binomial, data = Repro_WQ2,
                      control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#
#### 3-Site/Section and ShellHeight as a fixed effect
model.S_SH <- glmmTMB(Sex_class ~ Site + SH_scaled, family = binomial, data = Repro_WQ2,
                      control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#
AIC_models1 <- list('1-SiteOnly' = model.Site, 
                    '2-ShellHeightOnly' = model.SH, 
                    '3-Site+ShellHeight' = model.S_SH)
aictab(AIC_models1) #Want lowest AIC with most change
####KEY FINDINGS:
##Shell height is highly important (as expected)
##Including Site doesn't improve the model - sites are similar
#
###Best model - model 5 - evaluate model
Sex_best <- model.SH
### Assess goodness-of-fit
simulateResiduals(Sex_best, n = 250, refit = FALSE, plot = TRUE) #Want no deviation in QQ
recalculateResiduals(simulateResiduals(Sex_best, n = 250, refit = FALSE, plot = TRUE), group = Repro_WQ2$Site) #Doesn't change much. Not needed.
#REF: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
#REF: https://jonesor.github.io/BB852_Book/extending-use-cases-of-glm.html
## Calculate R-squared equivalent for count models. Closer to 1 is better
r2(Sex_best) #0.217 - more variability present than model accounts for
## Look at estimates
summary(Sex_best)
###Key findings 
##Sex_class ~ SH_scaled
## 1. Model fits the data pretty well with good residuals.
## 2. Tjur's R-squared is used here for a binary outcome. A value of 0.217 indicates a weak to moderate ability to discriminate between the 2 outcomes. Thus, there's lots of room for improvement.
## 3. Larger oysters more likely to be female (1)
#
#
#
##Consider adding Year and Season:
#### 1-Baseline: ShellHeight as a fixed effect
#model.SH <- glmmTMB(Sex_class ~ SH_scaled, family = binomial, data = Repro_WQ2,
#                    control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#### 2-Season and ShellHeight as a fixed effect
model.Sea_SH <- glmmTMB(Sex_class ~ Season + SH_scaled, family = binomial, data = Repro_WQ2,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#### 3-Year (factor, non-linear temporal) and ShellHeight as a fixed effect
model.Yr_SH <- glmmTMB(Sex_class ~ Year + SH_scaled, family = binomial, data = Repro_WQ2,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#### 4-Year (linear trend) and ShellHeight as a fixed effect
model.YrN_SH <- glmmTMB(Sex_class ~ Year_numeric + SH_scaled, family = binomial, data = Repro_WQ2,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#### 5-Season, Year (factor, non-linear temporal) and ShellHeight as a fixed effect
model.Yr_Sea_SH <- glmmTMB(Sex_class ~ Year + Season + SH_scaled, family = binomial, data = Repro_WQ2,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#### 6-Season, Year (linear trend) and ShellHeight as a fixed effect
model.YrN_Sea_SH <- glmmTMB(Sex_class ~ Year_numeric + Season + SH_scaled, family = binomial, data = Repro_WQ2,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#### 7-Season * Year (linear trend) interaction and ShellHeight as a fixed effect
model.YrNSea_SH <- glmmTMB(Sex_class ~ Year_numeric * Season + SH_scaled, family = binomial, data = Repro_WQ2,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#
AIC_models2 <- list('1-ShellHeightOnly' = Sex_best, 
                    '2-SH + Season' = model.Sea_SH, 
                    '3-SH + Year' = model.Yr_SH,
                    '4-SH + YearScaled' = model.YrN_SH,
                    '5-SH + Year + Season' = model.Yr_Sea_SH,
                    '6-SH + YearScaled + Season' = model.YrN_Sea_SH,
                    '7-SH + YearScaled * Season' = model.YrNSea_SH)
aictab(AIC_models2) #Want lowest AIC with most change
#
### Key findings from model selection
##1. Both Season AND Year are important (5 >> 2 or 3 alone)
##2. Non-linear year effects may be important (5 > 6: factor vs linear year) - may change once samples are completed for 2025
##3. Seasonal patterns are mostly consistent across years (5, 6, 7 not too different)
##4. Temporal factors explain a lot of the variation (ΔAICc = 1.36 vs baseline)")
#
###Best model - model 5 - evaluate model
Sex_best2 <- model.Yr_Sea_SH
### Assess goodness-of-fit
simulateResiduals(Sex_best2, n = 250, refit = FALSE, plot = TRUE) #Want no deviation in QQ
## Calculate R-squared equivalent for count models. Closer to 1 is better
r2(Sex_best2) #0.244 - more variability present than model accounts for
## Look at estimates
summary(Sex_best2)
###Key findings 
##Sex_class ~ SH_scaled + Year + Season
## 1. Model fits the data pretty well with good residuals. Tjur's R-squared value of 0.244 indicates a weak to moderate ability to discriminate between the 2 outcomes. Thus, there's lots of room for improvement, though better than the initial model.
## 2. Larger oysters still more likely to be female (1)
## 3. There appear to be some statistical differences between the Seasons and the Years. Closer inspection of those results to follow
## 4. Differences in more recent years... 
#
##Years
#Comparisons
emm_Year <- emmeans(Sex_best2, ~ Year, type= "response")
(pairs_Year <- contrast(emm_Year, method = "pairwise", adjust = "tukey"))
pairs_Year %>% as.data.frame() %>% dplyr::select(-c("df", "null"))
# contrast odds.ratio         SE    z.ratio      p.value
#1  Year2020 / Year2021  1.1291497 0.10809861  1.2687678 8.021435e-01
#2  Year2020 / Year2022  0.7067313 0.12135742 -2.0213826 3.299778e-01
#3  Year2020 / Year2023  1.1850023 0.11254311  1.7872960 4.739616e-01
#4  Year2020 / Year2024  1.5257773 0.13990686  4.6076868 5.973637e-05***
#5  Year2020 / Year2025  2.0633619 0.17335419  8.6214740 5.284662e-14***
#6  Year2021 / Year2022  0.6258969 0.10533679 -2.7841770 5.998352e-02
#7  Year2021 / Year2023  1.0494643 0.09305386  0.5445012 9.943063e-01
#8  Year2021 / Year2024  1.3512622 0.11504772  3.5357743 5.445643e-03**
#9  Year2021 / Year2025  1.8273590 0.13823605  7.9694346 1.053602e-13***
#10 Year2022 / Year2023  1.6767366 0.28123879  3.0814395 2.517028e-02*
#11 Year2022 / Year2024  2.1589213 0.35844524  4.6353652 5.232682e-05***
#12 Year2022 / Year2025  2.9195846 0.46924216  6.6664165 3.930899e-10***
#13 Year2023 / Year2024  1.2875733 0.10846970  3.0003412 3.224376e-02*
#14 Year2023 / Year2025  1.7412303 0.12721513  7.5908605 5.138112e-13***
#15 Year2024 / Year2025  1.3523349 0.09442791  4.3226502 2.232098e-04***
#Yearly mean (M = 0, F = 1)
(Year_mean <- left_join(Repro_WQ2 %>% group_by(Year) %>% get_summary_stats(Sex_class, show = c("mean", "sd", "se")), 
                        cld(emm_Year, alpha = 0.05, adjust = "sidak", Letters = letters) %>% 
                          dplyr::select(-"df") %>% rename("Group" = .group) %>% mutate(Group = gsub(" ", "", Group))))
#Years overall
Year_mean %>% 
  ggplot(aes(Year, mean, fill = Group))+
  geom_col()+ geom_errorbar(aes(ymin = mean, ymax = mean+se), width = 0.5)+
  scale_y_continuous("Proportion female", expand = c(0,0), limits = c(0, 1))+
  #scale_fill_grey()+
  basetheme + axistheme
#Year by SH - not fair comparison since collecting more, but interesting to look at
Repro_WQ2 %>%
  group_by(SH_Bin, Year, Sex) %>%
  summarise(Count = n()) %>%
  ggplot(aes(SH_Bin, Count, fill = Sex))+
  geom_col()+
  lemon::facet_rep_grid(Year~.)+ 
  scale_fill_manual(values = SexColor)+
  basetheme+ axistheme + facettheme
#
#
##Seasons
#Comparisons
emm_Season <- emmeans(Sex_best2, ~ Season, type= "response")
pairs_Season <- contrast(emm_Season, method = "pairwise", adjust = "tukey")
pairs_Season %>% as.data.frame() %>% dplyr::select(-c("df", "null"))
# contrast odds.ratio         SE    z.ratio      p.value
#1 Spring / Summer  1.4870931 0.09041524  6.5267005 4.033369e-10
#2   Spring / Fall  0.8720784 0.05552699 -2.1497036 1.375983e-01
#3 Spring / Winter  0.8617906 0.07201428 -1.7799975 2.829480e-01
#4   Summer / Fall  0.5864316 0.03272192 -9.5647846 3.141931e-14
#5 Summer / Winter  0.5795136 0.04572027 -6.9151604 2.806821e-11
#6   Fall / Winter  0.9882032 0.07804000 -0.1502683 9.987915e-01
#Seasonal mean (M = 0, F = 1)
(Season_mean <- left_join(Repro_WQ2 %>% group_by(Season) %>% get_summary_stats(Sex_class, show = c("mean", "sd", "se")), 
  cld(emm_Season, alpha = 0.05, adjust = "sidak", Letters = letters) %>% 
    dplyr::select(-"df") %>% rename("Group" = .group) %>% mutate(Group = gsub(" ", "", Group))))
#Seasons overall
Season_mean %>% 
  ggplot(aes(Season, mean, fill = Group))+
  geom_col()+ geom_errorbar(aes(ymin = mean, ymax = mean+se), width = 0.5)+
  scale_y_continuous("Proportion female", expand = c(0,0), limits = c(0, 1))+
  scale_fill_grey(start = 0, end = 0.5)+
  basetheme + axistheme
#Seasons by SH
Repro_WQ2 %>%
  group_by(SH_Bin, Season, Sex) %>%
  summarise(Count = n()) %>%
  ggplot(aes(SH_Bin, Count, fill = Sex))+
    geom_col()+
    lemon::facet_rep_grid(Season~.)+ 
  scale_fill_manual(values = SexColor)+
  basetheme+ axistheme + facettheme
#
###Shell heights
# Get the scaling parameters from the original data
SH_mean <- attr(scale(Repro_WQ2$ShellHeight), "scaled:center")
SH_sd <- attr(scale(Repro_WQ2$ShellHeight), "scaled:scale")
#Get the coefficient and odds ratio
(SH_coeff <- (summary(Sex_best2)$coefficients$cond %>% as.data.frame())["SH_scaled",]) #The coefficient for SH_scaled is 1.0128791 (SE = 0.02952, z = 34.314, p < 0.001)
(SH_odds <- as.numeric(paste(exp(SH_coeff))))
#
#Create a sequence of ORIGINAL Shell_height values, then scale them for emmeans
original_seq <- seq(min(Repro_WQ2$ShellHeight), max(Repro_WQ2$ShellHeight), by = 0.5)  # Example sequence on original scale
scaled_seq <- (original_seq - SH_mean) / SH_sd 
#Use emmeans for post hoc
emmeans_data <- expand.grid(SH_scaled = scaled_seq,
                            Year = levels(Repro_WQ2$Year), Season = levels(Repro_WQ2$Season))
emmeans_output <- emmeans(Sex_best2, ~ SH_scaled + Year + Season, at = list(SH_scaled = scaled_seq, Year = levels(Repro_WQ2$Year), Season = levels(Repro_WQ2$Season)), data = emmeans_data)
emmeans_summary <- summary(emmeans_output, type = "response")  # On response scale (probabilities)
#Add the original scale back to the emmeans output for easier interpretation
emmeans_summary$SH_original <- rep(original_seq, each = length(levels(Repro_WQ2$Year)) * length(levels(Repro_WQ2$Season)))  # Match the order
head(emmeans_summary)  #Check data is all there
#
# Create a dataframe for predictions using original scale, but scale for prediction
SH_newdata <- data.frame(SH_original = seq(min(Repro_WQ2$ShellHeight), max(Repro_WQ2$ShellHeight), length.out = 100))
# Scale the new data
SH_newdata$SH_scaled <- (SH_newdata$SH_original - SH_mean) / SH_sd
SH_newdata <- SH_newdata %>% 
  tidyr::expand_grid(Year = levels(Repro_WQ2$Year), Season = levels(Repro_WQ2$Season)) # Add Year and Season combinations
# Get predictions from the model using the scaled values
pred_SH_df <- data.frame(SH_newdata, 
                         Predictions = predict(Sex_best2, newdata = SH_newdata, type = "response", se.fit = TRUE)) %>%
  rename(Predictions = Predictions.fit, SE = Predictions.se.fit) %>%
  mutate(Lower_CI = Predictions - 1.96*SE, Upper_CI = Predictions + 1.96*SE)
#
# Plot the data and the fitted curves (individual)
Repro_WQ2 %>%
  ggplot(aes(x = ShellHeight, y = Sex_class)) +  # Use original Shell_height for data points
  geom_point(aes(color = as.factor(Sex_class)), alpha = 0.6, position = position_jitter(height = 0.05)) +  # Raw data
  scale_color_manual("Sex", labels = c("Male", "Female"), values = SexColorC)+ new_scale_color()+
  geom_line(data = pred_SH_df, aes(x = SH_original, y = Predictions, color = Year), size = 1) +  # Fitted curve
  #lemon::facet_rep_grid(Season~.) +
  basetheme + axistheme + facettheme
#
##Averaged
(pred_SH_df_ave <- pred_SH_df %>% group_by(SH_original, SH_scaled) %>%
    summarise(meanPred = mean(Predictions), meanSE = mean(SE), meanLower = mean(Lower_CI),meanUpper = mean(Upper_CI)))
#
ggplot() +  # Use original Shell_height for data points
  geom_point(data = Repro_WQ2, aes(x = ShellHeight, y = Sex_class, color = as.factor(Sex_class)), alpha = 0.6, position = position_jitter(height = 0.05)) +  # Raw data
  scale_color_manual("Sex", labels = c("Male", "Female"), values = SexColorC)+ 
  geom_ribbon(data = pred_SH_df_ave, aes(x = SH_original, ymin = meanLower, ymax = meanUpper), alpha = 0.4) +  # SE shading
  geom_line(data = pred_SH_df_ave, aes(x = SH_original, y = meanPred), size = 2) +  # Fitted curve
  scale_x_continuous(limits = c(0, 110), expand = c(0,0))+
  basetheme + axistheme + facettheme + theme(legend.position = "none")
#
#
#
##END OF SECTION
#
#####Shell heights -  summary tables and figures####
#
###All estuary data - LXN + LXS
##Samples per SH bin
Dermo_df %>% group_by(SH_Bin) %>% summarise(Count = n())
##Sample counts
Dermo_df %>% group_by(SH_Bin) %>% summarise(Total = n()) %>%
  ggplot(aes(SH_Bin, Total))+
  geom_bar(stat = "identity", position = "identity")+
  scale_y_continuous("Total samples", expand = c(0, 0), limits = c(0, 400))+
  ggtitle("Number of samples through Sept 2025")+
  basetheme + theme(axis.text.x = element_text(angle = 60, vjust = 0.85))
#
#Number of repro samples
Repro_df %>% group_by(SH_Bin) %>% summarise(Total = n()) %>%
  ggplot(aes(SH_Bin, Total))+
  geom_bar(stat = "identity", position = "identity")+
  scale_y_continuous("Total samples", expand = c(0, 0), limits = c(0, 400))+
  ggtitle("Number of samples through August 2025")+
  basetheme + theme(axis.text.x = element_text(angle = 60, vjust = 0.85))
#Focus on SH extremes
Dermo_df %>% group_by(SH_Bin) %>% summarise(Total = n()) %>%
  ggplot(aes(SH_Bin, Total))+
  geom_bar(stat = "identity", position = "identity")+
  scale_y_continuous("Total samples", expand = c(0, 0))+
  coord_cartesian(ylim = c(0, 40))+
  ggtitle("Number of samples through September 2025")+
  basetheme + theme(axis.text.x = element_text(angle = 60, vjust = 0.85))
#
#
#
#
##What is the average ratio of males:females per SH bin?
(LX_SH_Bin_ratios <- left_join(
  #Group by sex and get count of each (including Zs)
  left_join(Repro_df %>% group_by(Sex, SH_Bin) %>% summarise(All_Count = n()), 
            Repro_df %>% group_by(SH_Bin) %>% summarise(All_Total = n())) %>% 
    mutate(All_Ratio = All_Count/All_Total),
  #Group by sex and get count of each (excluding Zs)
  left_join(Repro_df %>% filter(Sex != "Z") %>% group_by(Sex, SH_Bin) %>% summarise(MF_Count = n()), 
            Repro_df %>% filter(Sex != "Z") %>% group_by(SH_Bin) %>% summarise(MF_Total = n())) %>% 
    mutate(MF_Ratio = MF_Count/MF_Total)))
#
(Female_ratio <- left_join(Repro_df %>% filter(Sex == "F") %>% group_by(SH_Bin) %>% summarise(Female_count = n()), 
                           Repro_df %>% filter(Sex != "Z") %>% group_by(SH_Bin) %>% summarise(Total = n())) %>% 
    mutate(Ratio = Female_count/Total))
#
#
#Compare ratios
ggarrange(
  LX_SH_Bin_ratios %>% 
    ggplot(aes(SH_Bin, All_Ratio, fill = Sex))+
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
    scale_fill_manual(values = SexZColor)+
    basetheme + theme(axis.text.x = element_text(angle = 60, vjust = 0.85)),
  LX_SH_Bin_ratios %>% filter(Sex != "Z") %>%
    ggplot(aes(SH_Bin, MF_Ratio, fill = Sex))+
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = SexColor)+
    scale_y_continuous("Ratio", expand = c(0, 0), limits = c(0, 1)) + 
    basetheme + theme(axis.text.x = element_text(angle = 60, vjust = 0.85)),
  nrow = 2)
#
Female_ratio %>% 
  ggplot(aes(SH_Bin, Ratio))+
  geom_point(color = "#CC79A7", size = 3)+
  geom_line(group = 1, color = "#CC79A7", linewidth = 1.25)+
  scale_y_continuous("Female ratio", expand = c(0, 0), limits = c(0,1.01))+
  basetheme + theme(axis.text.x = element_text(angle = 60, vjust = 0.85))
#
##Mean SH per Sex:
Repro_df %>% mutate(Sex = factor(Sex, ordered = TRUE, levels = c("Z", "M", "F"))) %>%
  mutate(ShellHeight = case_when(Sex == "Z" & ReproStage == 0 ~ ShellHeight, Sex == "Z" & ReproStage != "0" ~ NA, TRUE ~ ShellHeight)) %>%
  ggplot()+
  geom_jitter(aes(ShellHeight, Sex, color = Sex), height = 0.1)+
  geom_point(data = Repro_df %>% mutate(Sex = factor(Sex, ordered = TRUE, levels = c("Z", "M", "F")), ShellHeight = case_when(Sex == "Z" & ReproStage == 0 ~ ShellHeight, Sex == "Z" & ReproStage != "0" ~ NA, TRUE ~ ShellHeight)) %>% group_by(Sex) %>% summarise(ShellHeight = mean(ShellHeight, na.rm = T)),
             aes(ShellHeight, Sex), color = "black", size = 10)+
  scale_x_continuous(expand = c(0,0), limits = c(0, 100))+
  scale_y_discrete(expand = c(0,0.5))+
  basetheme
#
#
#
##END OF SECTION
#
####Maturity####
#
##Data set up
Mature_df <- Repro_df %>% filter(!grepl("no slide", Comments, ignore.case = TRUE)) %>%
  dplyr::select(Date:SH_Bin, Sex, ReproStage, Comments) %>% 
  filter(!is.na(ShellHeight)) %>%
  #Get maturity (0/1)
  mutate(Mature = case_when(grepl("0=", Comments) ~ 0,
                            grepl("0 = ", Comments) ~ 0,
                            ReproStage == 0 ~ 0, 
                            TRUE ~ 1)) %>%
  #MF with "U" for unknowns
  mutate(MF_Final = case_when(Sex == "M" ~ "M", Sex == "F" ~ "F", TRUE ~ "U"))
#

####Size at maturity figure
#Need histo file, "Species code", Proportion maturity, Type, Extra
#Type 1 = all M and F in one, extra == NA; 
#Type 2 = either Male or Female, must specify extra == "M" or "F"; 
#Type 3 = Male and female with individual propMature, specify extra == 1 for 1 plot, extra == 2 for faceted
#showU - show undetermined as gray on plot? Yes/No
matureSL <- function(df, proportionMature, Type, extra, showU = "Yes"){
  #Functions needed
  round10 <- function(x){10*ceiling(x/10)} #Rounds x up to nearest 10
  round5 <- function(x){5*ceiling(x/5)} #Rounds x up to nearest 5
  
  #df to work with
  mat_all <- df %>%  mutate(Mature = as.factor(Mature), MF_Final = as.factor(MF_Final)) %>% 
    dplyr::select(ShellHeight, SH_Bin, MF_Final, Mature) %>% drop_na()
  mat_M <- mat_all %>% filter(MF_Final != "F") %>% mutate(MF_Final = replace(MF_Final, MF_Final == "U", "M"))
  mat_MU <- mat_all %>% filter(MF_Final != "F") 
  mat_F <- mat_all %>% filter(MF_Final != "M") %>% mutate(MF_Final = replace(MF_Final, MF_Final == "U", "F"))
  mat_FU <- mat_all %>% filter(MF_Final != "M") 
  
  ##Color to sex - no U
  Sex <- c("M" = "Male", "F" = "Female")#, "U" = "Undetermined")
  color_og <- c("#009E73", "#E69F00")#, "#CCCCCC")
  #Map color to Sex
  names(color_og) <- c("F","M")#,"U")
  MFCol <- scale_color_manual(name = "", labels = Sex, values = color_og) 
  ##Color to sex - with U
  SexU <- c("M" = "Male", "F" = "Female", "U" = "Undetermined")
  color_ogU <- c("#009E73", "#E69F00", "#CCCCCC")
  #Map color to Sex
  names(color_ogU) <- c("F","M","U")
  MFUCol <- scale_color_manual(name = "", labels = SexU, values = color_ogU) 
  
  ###ALL
  #Fit model
  lrSL <- glm(Mature ~ ShellHeight, family = binomial, data = mat_all)
  output <- data.frame(ShellHeight = seq(0, round10(max(mat_all$ShellHeight)), 5))
  output$Mature <- predict(lrSL, newdata = output, type = "response")
  #Get x where y = 0.5
  LD50 <- MASS::dose.p(lrSL, p = proportionMature)
  
  ###Males
  lrSLM <- glm(Mature ~ ShellHeight, family = binomial, data = mat_M)
  outputM <- data.frame(ShellHeight = seq(0, round10(max(mat_M$ShellHeight)), 5))
  outputM$Mature <- predict(lrSLM, newdata = outputM, type = "response")
  #Get x where y = p
  LD50M <- MASS::dose.p(lrSLM, p = proportionMature)
  
  ###Females
  lrSLF <- glm(Mature ~ ShellHeight, family = binomial, data = mat_F)
  outputF <- data.frame(ShellHeight = seq(0, round10(max(mat_F$ShellHeight)), 5))
  outputF$Mature <- predict(lrSLF, newdata = outputF, type = "response")
  #Get x where y = p
  LD50F <- MASS::dose.p(lrSLF, p = proportionMature)
  ##Plots
  All <- mat_all %>%
    ggplot(aes(ShellHeight, as.numeric(Mature)-1))+
    geom_point(aes(color = MF_Final), size = 3, alpha = 0.6)+
    stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
                method.args = list(family = binomial), size = 1.25)+
    basetheme + XCate + MFCol +
    geom_vline(xintercept = LD50[[1]],linetype = "dashed", color = "black", size = 1)+
    scale_x_continuous(name = "Shell length (mm)", expand = c(0,0), limits = c(0, round10(max(mat_all$ShellHeight))), breaks = seq(0, round10(max(mat_all$ShellHeight)), by = 10))+
    scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))
  Single <- mat_all %>% subset(MF_Final == extra | MF_Final == "U") %>%
    ggplot(aes(ShellHeight, as.numeric(Mature)-1))+
    geom_point(aes(color = MF_Final), size = 3, alpha = 0.6)+
    stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
                method.args = list(family = binomial), size = 1.25)+
    basetheme + XCate + MFCol +
    geom_vline(xintercept = ifelse(extra == "M", LD50M[[1]], LD50F[[1]]),linetype = "dashed", color = "black", size = 1)+
    scale_x_continuous(name = "Shell length (mm)", expand = c(0,0), limits = c(0, round10(max(mat_all$ShellHeight))), breaks = seq(0, round10(max(mat_all$ShellHeight)), by = 10))+
    scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))
  Both <- mat_all %>%
    ggplot(aes(ShellHeight, as.numeric(Mature)-1))+
    geom_point(aes(color = MF_Final), size = 3, alpha = 0.6)+
    stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
                method.args = list(family = binomial), size = 1.25)+
    basetheme + XCate + MFCol +
    geom_vline(xintercept = LD50[[1]],linetype = "dashed", color = "black", size = 1)+
    geom_vline(xintercept = LD50M[[1]],linetype = "dashed", color = "#E69F00", size = 1)+
    geom_vline(xintercept = LD50F[[1]],linetype = "dashed", color = "#009E73", size = 1)+
    scale_x_continuous(name = "Shell length (mm)", expand = c(0,0), limits = c(0, round10(max(mat_all$ShellHeight))), breaks = seq(0, round10(max(mat_all$ShellHeight)), by = 10))+
    scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))
  if(showU == "No"){
    Facet <- rbind(mat_M, mat_F) %>%
      ggplot(aes(ShellHeight, as.numeric(Mature)-1))+
      geom_point(aes(color = MF_Final), size = 3, alpha = 0.6)+
      stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
                  method.args = list(family = binomial), size = 1.25)+
      lemon::facet_rep_grid(MF_Final~., labeller = labeller(MF_Final = Sex))+
      basetheme + XCate + MFCol + facettheme+ theme(legend.position = "none")+
      geom_vline(data = filter(mat_all, MF_Final == "M"), aes(xintercept = LD50M[[1]]),linetype = "dashed", color = "black", size = 1)+
      geom_vline(data = filter(mat_all, MF_Final == "F"), aes(xintercept = LD50F[[1]]),linetype = "dashed", color = "black", size = 1)+
      scale_x_continuous(name = "Shell length (mm)", expand = c(0,0), limits = c(0, round10(max(mat_all$ShellHeight))), breaks = seq(0, round10(max(mat_all$ShellHeight)), by = 10))+
      scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))
  } else {
    Facet <- rbind(mat_M, mat_F) %>%
      ggplot(aes(ShellHeight, as.numeric(Mature)-1))+
      geom_point(aes(color = MF_Final, alpha = Mature), size = 3.5)+
      stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
                  method.args = list(family = binomial), size = 1.25)+
      lemon::facet_rep_grid(MF_Final~., labeller = labeller(MF_Final = Sex))+
      basetheme + XCate + MFUCol + facettheme+ theme(legend.position = "none")+
      scale_alpha_manual(values = c(0.2, 0.6))+
      geom_vline(data = filter(mat_all, MF_Final == "M"), aes(xintercept = LD50M[[1]]),linetype = "dashed", color = "black", size = 1)+
      geom_vline(data = filter(mat_all, MF_Final == "F"), aes(xintercept = LD50F[[1]]),linetype = "dashed", color = "black", size = 1)+
      scale_x_continuous(name = "Shell length (mm)", expand = c(0,0), limits = c(0, round10(max(mat_all$ShellHeight))), breaks = seq(0, round10(max(mat_all$ShellHeight)), by = 10))+
      scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))
  }
  #OUTPUT
  ifelse(Type == 1,
         return(list(All, paste("All:", proportionMature,"=",LD50[1],",", "SE =",attr(LD50, "SE")))),
         ifelse(Type == 2,
                return(list(Single, ifelse(extra == "M", 
                                           paste("Males:", proportionMature,"=",LD50M[1],",", "SE =",attr(LD50M, "SE")), 
                                           paste("Females:", proportionMature,"=",LD50F[1],",", "SE =",attr(LD50F, "SE"))))),
                ifelse(Type == 3,
                       if(extra == 1){return(list(Both, 
                                                  paste("All:", proportionMature,"=",LD50[1],",", "SE =",attr(LD50, "SE")), 
                                                  paste("Males:", proportionMature,"=",LD50M[1],",", "SE =",attr(LD50M, "SE")), 
                                                  paste("Females", proportionMature,"=",LD50F[1],",", "SE =",attr(LD50F, "SE"))))}
                       else if(extra == 2){return(list(Facet, 
                                                       paste("Males:", proportionMature,"=",LD50M[1],",", "SE =",attr(LD50M, "SE")), 
                                                       paste("Females", proportionMature,"=",LD50F[1],",", "SE =",attr(LD50F, "SE"))))}
                       else{print("1 or 2 plots?")},
                       print("Need more info"))))
  
}
#
matureSL(Mature_df, 0.5, 3, 2, "Yes")
#[[2]]
#[1] "Males: 0.5 = 10.656656694755 , SE = 1.22395283539628"
#[[3]]
#[1] "Females 0.5 = 12.1867129245564 , SE = 1.17555024958039"
#
#
##END OF SECTION
#
####Helpers####
#
##Write data to Excel
#Ratio data
writexl::write_xlsx(Ratio_clean_df, paste0("Data/Ratio_data_",Sys.Date(), ".xlsx"),col_names = TRUE)
