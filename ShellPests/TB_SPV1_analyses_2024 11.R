##Tampa Bay shell pest project analyses
#
#WQ data
#Shell pests
#Condition Index
#
#
#Load packages, install as needed
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               ggpubr, scales,
               rstatix, lme4, corrplot, vegan, lmPerm, DHARMa, #Summary stats, correlations
               zoo, lubridate, #Dates and times
               readxl, #Reading excel files
               car, emmeans, multcomp, multcompView, broom.mixed, ggeffects, #Basic analyses
               blorr, DescTools, sjPlot, 
               MuMIn, glmmTMB, lme4,
               install = TRUE)
#
#
#
#
####Load Files####
#Reading in Excel files
#
###Water qualilty
TB_WQ_raw <- read_excel("Data/TB_WQ_2018_2021.xlsx", sheet = "Sheet1", #File name and sheet name
                    skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                    na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "unique")
#Check data and column names
head(TB_WQ_raw)
#Simplify column names
colnames(TB_WQ_raw) <- c("Date", "Site", "Station", "Time", "Depth", "Temperature", "Salinity", "pH", "DO_mgl", "DO_Pct", "Secchi", "TProbe", "THach", "Quali")
head(TB_WQ_raw)
#
#Portal data
Portal_WQ_raw <- read_excel("Data/TB_SPV1_final_2018_2022.xlsx", sheet = "Sheet1", #File name and sheet name
                    skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                    na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "unique") %>%
  subset(SampleDate < "2022-01-01")
#Check data and column names
head(Portal_WQ_raw)
#
#
#
###Condition index
TB_CI_raw <- read_excel("Data/TB_CI_2018_2021.xlsx", sheet = "Sheet1", #File name and sheet name
                    skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                    na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "unique")
head(TB_CI_raw)
#Simplify column names
colnames(TB_CI_raw) <- c("Date", "Site", "Sample_Number", "Station", "SH", "SL", "SW", "TW", "TarePan", "TissueWW", "ShellWW", "TissueDW", "ShellDW", "FinalTisse", "CI", "CI_Hanley", "New_Sample_Number")
head(TB_CI_raw)
#
#
#
###Shell pests
TB_SP_raw <- read_excel("Data/TB_ShellPests_2018_2021.xlsx", sheet = "Sheet1", #File name and sheet name
                    skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                    na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "universal")
#Check data and column names
head(TB_SP_raw)
#
#Convert Month to factor
TB_SP_raw <- TB_SP_raw %>% mutate(Month = as.factor(substr(Date, start = 6, stop = 7)))
#
#
#
##Reference dfs for factor columns
Reef_Type <- data.frame("Station" = c(1, 2, 3, 4, 5),
                        "Station_Name" = c("Pinellas Point", "Skyway", "Fort Desoto", "Gulfport", "Weedon Island"),
                        "Type" = c("Inter", "Inter", "Sub", "Inter", "Sub"))
#
Seasons <- data.frame("Month" = as.factor(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")),
                      "Month_Abb" = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                      "Season" = c("Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Fall", "Fall", "Fall"))
#
Seasons_c <- data.frame("Month" = as.factor(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")),
                        "Season_c" = c("Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Spring", "Summer", "Summer", "Fall", "Fall", "Fall"))
#
#
#
#
####Data cleaning####
#
###Water quality
#Check column data types, change as needed
glimpse(TB_WQ_raw)
#Add Year, Month, Season columns, add reef and station information
TB_WQ <- TB_WQ_raw %>% mutate(Year = as.factor(format(Date, "%Y")),
                          Month = as.factor(format(Date, "%m"))) %>% 
  left_join(Seasons) %>% left_join(Seasons_c) %>%
  left_join(Reef_Type) %>%
  mutate_at(c("Site", "Station", "Month", "Month_Abb", "Season", "Station_Name", "Type"), as.factor) %>% #Change columns to factor type 
  dplyr::select(Year, Month, Season, Season_c, everything(.)) #Reorder columns
head(TB_WQ)
#
glimpse(Portal_WQ_raw)
##Get mean daily values to work with.
Portal_WQ <- Portal_WQ_raw %>% mutate(Month = as.factor(Month)) %>%
  dplyr::select(-Result_Unit, -Buffer, -LocationName, -StationName, -Latitude, -Longitude, -Season) %>% #Remove unnecessary columns
  left_join(Seasons) %>% left_join(Seasons_c) %>%
  group_by(Year, Month, Season, Season_c, Estuary, Station, SampleDate, Parameter) %>% #Group by all columns needed in output
  summarise(Mean_Result = mean(Result_Value, na.rm = T)) %>% #Calculate mean daily value 
  mutate_at(c("Year", "Month", "Season", "Season_c", "Estuary", "Station"), as.factor)
#
head(Portal_WQ)
#
#Combine portal data with our data
TB_WQ_df <- rbind(TB_WQ %>% left_join(Seasons) %>% left_join(Seasons_c) %>% 
                    dplyr::select(-Time, -Date, -Depth, -Station_Name, -Type, -Month_Abb, -Secchi, -TProbe, -THach, -Quali) %>% #Remove unneeded columns from our data
                    pivot_longer(cols = -c(Year, Month, Season, Season_c, Site, Station), names_to = "Parameter", values_to = "Mean_Result"),
                    ungroup(Portal_WQ) %>% rename(Site = Estuary) %>% dplyr::select(-SampleDate)) %>% #Rename Portal columns to join data
  group_by(Year, Month, Season, Season_c, Site, Station, Parameter) %>% summarise(Mean_Result = mean(Mean_Result, na.rm = T)) %>% #Determine mean daily value for all parameters and data
  pivot_wider(names_from = Parameter, values_from = Mean_Result) #Spread parameters into columns
#
#
#
#
###Condition index
glimpse(TB_CI_raw)
#Add columns for Year, Month, and SizeClass then Season and Reef Type info
TB_CI <- TB_CI_raw %>% mutate(Year = as.factor(format(Date, "%Y")),
                          Month = as.factor(format(Date, "%m")),
                          SizeClass = case_when(SH < 25 ~ "S",
                                                SH > 75 ~ "L",
                                                TRUE ~ "A")) %>%
  left_join(Seasons) %>% left_join(Seasons_c) %>%
  left_join(Reef_Type) %>%
  mutate_at(c("Year", "Month", "Site", "Station", "Month_Abb", "Season", "Season_c", "Station_Name", "Type", "SizeClass"), as.factor)
head(TB_CI)
#
#
#
#
###Shell pests
glimpse(TB_SP_raw)
#Add columns - add column for combined load (Poly+Clio)
TB_SP <- TB_SP_raw %>% mutate(SizeClass = case_when(Height < 25 ~ "S",
                                                Height > 75 ~ "L",
                                                TRUE ~ "A") ,
                          Shell_Side = ifelse(grepl("E", Location), "External", "Internal"),
                          Shell_Pos = ifelse(grepl("B", Location), "Bottom", "Top"),
                          Pct_Affected = Pct.Polydora + Pct.Cliona) %>%
  left_join(Seasons) %>% left_join(Seasons_c) %>%
  left_join(Reef_Type) %>%
  mutate_at(c("Year", "Month", "Site", "Station", "Shell_Side", "Shell_Pos", "Month_Abb", "Season", "Season_c", "Station_Name", "Type"), as.factor)
head(TB_SP)
#
#Summarize data by Shell_Side, Shell_Pos, and overall
TB_SP_df <- 
  rbind(TB_SP %>%  
          group_by(Date, Year, Month, Site, Station, Sample.Number, Shell_Side, New_Sample_Number) %>%
          summarise(Pct.Polydora = mean(Pct.Polydora, na.rm = T),
                    Pct.Cliona = mean(Pct.Cliona, na.rm = T),
                    Pct_Affected = mean(Pct_Affected, na.rm = T),
                    Poly_Prev = case_when(Pct.Polydora > 0 ~ 1, TRUE ~ 0),
                    Cliona_Prev = case_when(Pct.Cliona > 0 ~ 1, TRUE ~ 0),
                    Richness = Poly_Prev + Cliona_Prev) %>%
          rename(Measurement = Shell_Side),
        #
        TB_SP %>%  mutate(Shell_Pos = recode_factor(Shell_Pos, 'Top' = "Top", 'Bottom' = "Bot")) %>%
          group_by(Date, Year, Month, Site, Station, Sample.Number, Shell_Pos, New_Sample_Number) %>%
          summarise(Pct.Polydora = mean(Pct.Polydora, na.rm = T),
                    Pct.Cliona = mean(Pct.Cliona, na.rm = T),
                    Pct_Affected = mean(Pct_Affected, na.rm = T),
                    Poly_Prev = case_when(Pct.Polydora > 0 ~ 1, TRUE ~ 0),
                    Cliona_Prev = case_when(Pct.Cliona > 0 ~ 1, TRUE ~ 0),
                    Richness = Poly_Prev + Cliona_Prev) %>%
          rename(Measurement = Shell_Pos)) %>%
  #
  rbind(TB_SP %>%  
          group_by(Date, Year, Month, Site, Station, Sample.Number, New_Sample_Number) %>%
          summarise(Pct.Polydora = mean(Pct.Polydora, na.rm = T),
                    Pct.Cliona = mean(Pct.Cliona, na.rm = T),
                    Pct_Affected = mean(Pct_Affected, na.rm = T),
                    Poly_Prev = case_when(Pct.Polydora > 0 ~ 1, TRUE ~ 0),
                    Cliona_Prev = case_when(Pct.Cliona > 0 ~ 1, TRUE ~ 0),
                    Richness = Poly_Prev + Cliona_Prev) %>% 
          mutate(Measurement = as.factor("All"))) %>%
  arrange(Sample.Number)
#
head(TB_SP_df)
#
#
#Combine shell pest and condition data by sample ID
(Combined_df <- full_join(TB_SP_df %>% dplyr::select(Date:Station, Measurement:Richness), TB_CI %>% dplyr::select(Date, Site, Station:Type)))
#
#
#
#
#
####Overall summary for comparisons####
#
#Each station all years and all TB all years - WQ - just Temp, Sal, pH, and DO for now
(TB_WQ_summ <- rbind(TB_WQ_df %>% ungroup() %>% dplyr::select(Station, Temperature, Salinity, pH, DO_mgl, DO_Pct) %>%
   pivot_longer(cols = -Station, names_to = "Parameter", values_to = "Value") %>% 
   group_by(Station, Parameter) %>%
   summarise(n = n(),
             mean = mean(Value, na.rm = T),
             se = sd(Value, na.rm = T)/sqrt(n), 
             min = min(Value, na.rm = T),
             max = max(Value, na.rm = T)),
   TB_WQ_df %>% ungroup() %>% dplyr::select(Site, Temperature, Salinity, pH, DO_mgl, DO_Pct) %>%
     rename(Station = Site) %>%
     pivot_longer(cols = -Station, names_to = "Parameter", values_to = "Value") %>% 
     group_by(Station, Parameter) %>%
     summarise(n = n(),
               mean = mean(Value, na.rm = T),
               se = sd(Value, na.rm = T)/sqrt(n), 
               min = min(Value, na.rm = T),
               max = max(Value, na.rm = T))))
#
#
#Each station all years and All TB, all years - Sizes
(TB_Size_summ <- rbind(TB_CI %>% group_by(Station, SizeClass) %>%
                        summarise(n = n()) %>% pivot_wider(names_from = SizeClass, values_from = n),
                      TB_CI %>% group_by(Site, SizeClass) %>%
                        summarise(n = n()) %>% rename(Station = Site) %>% pivot_wider(names_from = SizeClass, values_from = n)))
#
(TB_CI_summ <- rbind(TB_CI %>% group_by(Station) %>%
                       summarise(n = n(),
                                 mean = mean(CI, na.rm = T),
                                 se = sd(CI, na.rm = T)/sqrt(n), 
                                 min = min(CI, na.rm = T),
                                 max = max(CI, na.rm = T)),
                     TB_CI %>% group_by(Site) %>%
                       summarise(n = n(),
                                 mean = mean(CI, na.rm = T),
                                 se = sd(CI, na.rm = T)/sqrt(n), 
                                 min = min(CI, na.rm = T),
                                 max = max(CI, na.rm = T)) %>% rename(Station = Site)))
#CI_Hanley version
(TB_CI_H_summ <- rbind(TB_CI %>% group_by(Station) %>%
                       summarise(n = n(),
                                 mean = mean(CI_Hanley, na.rm = T),
                                 se = sd(CI_Hanley, na.rm = T)/sqrt(n), 
                                 min = min(CI_Hanley, na.rm = T),
                                 max = max(CI_Hanley, na.rm = T)),
                     TB_CI %>% group_by(Site) %>%
                       summarise(n = n(),
                                 mean = mean(CI_Hanley, na.rm = T),
                                 se = sd(CI_Hanley, na.rm = T)/sqrt(n), 
                                 min = min(CI_Hanley, na.rm = T),
                                 max = max(CI_Hanley, na.rm = T)) %>% rename(Station = Site)))
#
#
#
#Each station all years and all TB all years - Pests - Pcts and Richness
TB_Pest_summ <- rbind(TB_SP_df %>% ungroup() %>% dplyr::select(-Date, -Year, -Month, -Site, -Sample.Number, -New_Sample_Number) %>%
                        pivot_longer(cols = -c(Station, Measurement), names_to = "Parameter", values_to = "Value") %>% 
                        group_by(Station, Measurement, Parameter) %>%
                        summarise(n = n(),
                                  mean = mean(Value, na.rm = T),
                                  se = sd(Value, na.rm = T)/sqrt(n), 
                                  min = min(Value, na.rm = T),
                                  max = max(Value, na.rm = T)),
                      TB_SP_df %>% ungroup() %>% dplyr::select(-Date, -Year, -Month, -Station, -Sample.Number, -New_Sample_Number) %>%
                        pivot_longer(cols = -c(Site, Measurement), names_to = "Parameter", values_to = "Value") %>% 
                        group_by(Site, Measurement, Parameter) %>%
                        summarise(n = n(),
                                  mean = mean(Value, na.rm = T),
                                  se = sd(Value, na.rm = T)/sqrt(n), 
                                  min = min(Value, na.rm = T),
                                  max = max(Value, na.rm = T)) %>%
                        rename(Station = Site)) %>%
  arrange(Measurement, Station)
#
head(TB_Pest_summ)
#
#
##Pest Prevalence
#How many surfaces have Polydora and/or Cliona?

#Join count of infected oyster/shell sides, total count of samples, and determine prevalence
(Side_Presence <- left_join(TB_SP %>%  
                             group_by(Date, Year, Month, Site, Station, Sample.Number, Shell_Side) %>%
                             #Determine presence/absence of Polydora or Cliona per sample and shell side (External/Internal)
                             summarise(Poly_Pres = case_when(Pct.Polydora > 0 ~ 1, TRUE ~ 0),
                                       Cliona_Pres = case_when(Pct.Cliona > 0 ~ 1, TRUE ~ 0),
                                       Presence = case_when(Pct.Polydora > 0 | Pct.Cliona > 0 ~ 1, TRUE ~ 0)) %>%
                             rename(Measurement = Shell_Side) %>%
                             ungroup() %>% group_by(Date, Year, Month, Site, Station, Sample.Number, Measurement) %>%
                             #Determine overall presence per station per Month/Year per shell side per sample
                             summarise(Poly_Presence = max(Poly_Pres),
                                       Cliona_Presence = max(Cliona_Pres),
                                       Presence = max(Presence)) %>%
                             ungroup() %>% group_by(Date, Year, Month, Site, Station, Measurement) %>%
                             #Get final count of how many oysters per station have Polydora or Cliona on their external or internal surfaces 
                             summarise(Poly_Presence = sum(Poly_Presence),
                                       Cliona_Presence = sum(Cliona_Presence),
                                       Presence = sum(Presence)),
                           #Total number of samples per station (should be 5 unless samples missing)
                           TB_SP %>%  
                             drop_na(Shell_Side) %>% dplyr::select(Date:Site, Station, Sample.Number) %>% unique() %>% #Remove rows of missing data, select rows to count by
                             group_by(Date, Year, Month, Site, Station) %>% #Group by and count number of samples
                             summarise(Total = n())) %>%
  #Calculate final prevalences and limit df to desired columns
  mutate(Pres_Poly = Poly_Presence/Total,
         Pres_Clio = Cliona_Presence/Total,
         Pres_Pests = Presence/Total) %>%
  dplyr::select(Date:Measurement, Pres_Poly:Pres_Pests))
#
(Shell_Presence <- left_join(TB_SP %>%  
                              group_by(Date, Year, Month, Site, Station, Sample.Number, Shell_Pos) %>%
                              #Determine presence/absence of Polydora or Cliona per sample and shell side (External/Internal)
                              summarise(Poly_Pres = case_when(Pct.Polydora > 0 ~ 1, TRUE ~ 0),
                                        Cliona_Pres = case_when(Pct.Cliona > 0 ~ 1, TRUE ~ 0),
                                        Presence = case_when(Pct.Polydora > 0 | Pct.Cliona > 0 ~ 1, TRUE ~ 0)) %>%
                              rename(Measurement = Shell_Pos) %>%
                              ungroup() %>% group_by(Date, Year, Month, Site, Station, Sample.Number, Measurement) %>%
                              #Determine overall presence per station per Month/Year per shell side per sample
                              summarise(Poly_Presence = max(Poly_Pres),
                                        Cliona_Presence = max(Cliona_Pres),
                                        Presence = max(Presence)) %>%
                              ungroup() %>% group_by(Date, Year, Month, Site, Station, Measurement) %>%
                              #Get final count of how many oysters per station have Polydora or Cliona on their external or internal surfaces 
                              summarise(Poly_Presence = sum(Poly_Presence),
                                        Cliona_Presence = sum(Cliona_Presence),
                                        Presence = sum(Presence)),
                            #Total number of samples per station (should be 5 unless samples missing)
                            TB_SP %>%  
                              drop_na(Shell_Pos) %>% dplyr::select(Date:Site, Station, Sample.Number) %>% unique() %>% #Remove rows of missing data, select rows to count by
                              group_by(Date, Year, Month, Site, Station) %>% #Group by and count number of samples
                              summarise(Total = n())) %>%
    #Calculate final prevalences and limit df to desired columns
    mutate(Pres_Poly = Poly_Presence/Total,
           Pres_Clio = Cliona_Presence/Total,
           Pres_Pests = Presence/Total) %>%
    dplyr::select(Date:Measurement, Pres_Poly:Pres_Pests))
#
Shell_Presence %>% dplyr::select(-Measurement) %>%
  pivot_longer(cols = -c(Date, Year, Month, Site, Station), names_to = "Type", values_to = "Prevalence") %>%
  group_by(Site, Station, Type) %>%
  mutate(Type = as.factor(Type)) %>%
  summarise(n = n(), 
            Mean = mean(Prevalence), 
            SE = sd(Prevalence, na.rm = T)/sqrt(n)) %>%
  ggplot(aes(Station, Mean, fill = Type))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, position = position_dodge(width = 0.9))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.25))+ theme_classic()
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
FacetTheme <- theme(strip.text.y = element_text(face = "bold", size = 12),
                 strip.background = element_rect(fill = "#CCCCCC"),
                 panel.spacing = unit(0.75, "lines"),
                 strip.text.x = element_text(face = "bold", size = 12))
#
#
#
#
#
####Bay Summary Questions (Q1-Q4)####
#
###Q1: WQ trends - overall min/max/mean; annual min/max/mean figures :: Summary data found in "TB_WQ_summ" df/sheet
#Overall
TB_WQ_df %>% ungroup() %>% mutate(MonYr = as.yearmon(paste(Year, Month, sep = "-"))) %>% 
  dplyr::select(MonYr, Temperature, Salinity, pH, DO_mgl, DO_Pct) %>%
  pivot_longer(cols = -MonYr, names_to = "Parameter", values_to = "Value") %>% 
  group_by(MonYr, Parameter) %>%
  summarise(n = n(),
            mean = mean(Value, na.rm = T),
            se = sd(Value, na.rm = T)/sqrt(n)) %>%
  ggplot(aes(MonYr, mean, group = 1))+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.02)+
  geom_line(linewidth = 0.75)+
  lemon::facet_rep_grid(Parameter~., scales = "free_y")+
  basetheme +
  scale_x_yearmon("", expand = c(0,0), n = 12)+
  scale_y_continuous("Mean parameter value")
#
#Annual
TB_WQ_df %>% ungroup() %>% 
  dplyr::select(Year, Temperature, Salinity, pH, DO_mgl, DO_Pct) %>%
  pivot_longer(cols = -Year, names_to = "Parameter", values_to = "Value") %>% 
  group_by(Year, Parameter) %>%
  summarise(n = n(),
            mean = mean(Value, na.rm = T),
            se = sd(Value, na.rm = T)/sqrt(n),
            min = min(Value, na.rm = T),
            max = max(Value, na.rm = T)) %>%
  ggplot(aes(Year, mean, group = 1))+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.02)+
  geom_line(linewidth = 0.75)+
  lemon::facet_rep_grid(Parameter~., scales = "free_y")+
  basetheme +
  scale_x_discrete(expand = c(0.05,0))+
  scale_y_continuous("Mean parameter value")
#
###Any sig differences among years?
#Permutation ANOVA - temperature
set.seed(4321)
Overall_temp <- aovp(Temperature ~ Year, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Overall_temp) #NO sig diff among years
#
#Permutation ANOVA - salinity
set.seed(4321)
Overall_salinity <- aovp(Salinity ~ Year, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Overall_salinity) #NO sig diff among years
#
#Permutation ANOVA - pH
set.seed(4321)
Overall_pH <- aovp(pH ~ Year, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Overall_pH) #NO sig diff among years (alpha set to 0.05 so p would need to be less than 0.05 to be sig)
#
#Permutation ANOVA - DO_mgl
set.seed(4321)
Overall_DOmgl <- aovp(DO_mgl ~ Year, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Overall_DOmgl) #NO sig diff among years
#
#No significant difference among years in mean parameter values for any of the parameters.
#
#
#
#
#
#
#
###Q2: WQ - which months most similar? -comparing all month values
(TB_WQ_Months <- TB_WQ_df %>% ungroup() %>% 
    dplyr::select(Month, Temperature, Salinity, pH, DO_mgl) %>% #drop_na())
    group_by(Month) %>% 
    mutate(Temperature = mean(Temperature, na.rm = T),
           Salinity = mean(Salinity, na.rm = T),
           pH = mean(pH, na.rm = T),
           DO_mgl = mean(DO_mgl, na.rm = T)) %>% distinct())
#
ggboxplot(TB_WQ_df, x = "Month", y = "Temperature")
gghistogram(TB_WQ_df, x = "Temperature") #Slightly skewed, okay for permANOVA
#
#Permutation ANOVA - temperature
set.seed(4321)
Month_temp <- aovp(Temperature ~ Month, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Month_temp)
(Month_temp_p <- rstatix::pairwise_t_test(Temperature ~ Month, data = ungroup(TB_WQ_df), p.adjust.method = "holm") %>%
  dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Month_temp_means <- merge(TB_WQ_df %>% group_by(Month) %>% rstatix::get_summary_stats(Temperature, show = c("n", "mean", "sd", "min", "max")) %>% 
        dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
      biostat::make_cld(Month_temp_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Month = group, Letters = cld)))
ggplot(Month_temp_means, aes(Month, mean, color = Letters))+
  geom_point(aes(shape = Letters), size = 3)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  basetheme
Month_temp_p %>% filter(p.adjust < 0.05)  %>% arrange(Comparison) %>% print(n = 57)
#Temperatures are similar in 02/03/11; 09 <= 06/08 <= 07; 05/10
#
#Permutation ANOVA - salinity 
set.seed(4321)
Month_sal <- aovp(Salinity ~ Month, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Month_sal)
(Month_sal_p <- rstatix::pairwise_t_test(Salinity ~ Month, data = ungroup(TB_WQ_df), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Month_sal_means <- merge(TB_WQ_df %>% group_by(Month) %>% rstatix::get_summary_stats(Salinity, show = c("n", "mean", "sd", "min", "max")) %>% 
                             dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                           biostat::make_cld(Month_sal_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Month = group, Letters = cld)))
ggplot(Month_sal_means, aes(Month, mean, color = Letters))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  basetheme
Month_sal_p %>% filter(p.adjust < 0.05) %>% arrange(Comparison) %>% print(n = 20)
#Salinity is lowest in 09 highest in 04/05/06/07
#
#Permutation ANOVA - pH 
set.seed(4321)
Month_pH <- aovp(pH ~ Month, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Month_pH)
(Month_pH_p <- rstatix::pairwise_t_test(pH ~ Month, data = ungroup(TB_WQ_df), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Month_pH_means <- merge(TB_WQ_df %>% group_by(Month) %>% rstatix::get_summary_stats(pH, show = c("n", "mean", "sd", "min", "max")) %>% 
                            dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                          biostat::make_cld(Month_pH_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Month = group, Letters = cld)))
ggplot(Month_pH_means, aes(Month, mean, color = Letters))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  basetheme
Month_pH_p %>% filter(p.adjust < 0.05) %>% arrange(Comparison) %>% print(n = 57)
#pH is lowest in 08, higest in 03
#
#Permutation ANOVA - DOmgL 
set.seed(4321)
Month_DO <- aovp(DO_mgl ~ Month, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Month_DO)
(Month_DO_p <- rstatix::pairwise_t_test(DO_mgl ~ Month, data = ungroup(TB_WQ_df), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Month_DO_means <- merge(TB_WQ_df %>% group_by(Month) %>% rstatix::get_summary_stats(DO_mgl, show = c("n", "mean", "sd", "min", "max")) %>% 
                            dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                          biostat::make_cld(Month_DO_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Month = group, Letters = cld)))
ggplot(Month_DO_means, aes(Month, mean, color = Letters))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  basetheme
Month_DO_p %>% filter(p.adjust < 0.05) %>% arrange(Comparison) %>% print(n = 57)
#DO lower in Summer months and higher in Spring months
#
#
#
#
#
#
#
###Q3: WQ - which stations most similar? -comparing all station values
(TB_WQ_Stations <- TB_WQ_df %>% ungroup() %>% 
    dplyr::select(Station, Temperature, Salinity, pH, DO_mgl) %>% #drop_na())
    group_by(Station) %>% 
    mutate(Temperature = mean(Temperature, na.rm = T),
           Salinity = mean(Salinity, na.rm = T),
           pH = mean(pH, na.rm = T),
           DO_mgl = mean(DO_mgl, na.rm = T)) %>% distinct())
#
ggboxplot(TB_WQ_df, x = "Station", y = "Temperature")
ggboxplot(TB_WQ_df, x = "Station", y = "Salinity")
#
#Permutation ANOVA - temperature
set.seed(4321)
Station_temp <- aovp(Temperature ~ Station, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Station_temp)
(Station_temp_p <- rstatix::pairwise_t_test(Temperature ~ Station, data = ungroup(TB_WQ_df), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Station_temp_means <- merge(TB_WQ_df %>% group_by(Station) %>% rstatix::get_summary_stats(Temperature, show = c("n", "mean", "sd", "min", "max")) %>% 
                             dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                           biostat::make_cld(Station_temp_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Station = group, Letters = cld)))
ggplot(Station_temp_means, aes(Station, mean, color = Letters))+
  geom_point(aes(shape = Letters), size = 3)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  basetheme
#No sig diff in temps among stations
#
#Permutation ANOVA - salinity 
set.seed(4321)
Station_sal <- aovp(Salinity ~ Station, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Station_sal)
(Station_sal_p <- rstatix::pairwise_t_test(Salinity ~ Station, data = ungroup(TB_WQ_df), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Station_sal_means <- merge(TB_WQ_df %>% group_by(Station) %>% rstatix::get_summary_stats(Salinity, show = c("n", "mean", "sd", "min", "max")) %>% 
                            dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                          biostat::make_cld(Station_sal_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Station = group, Letters = cld)))
ggplot(Station_sal_means, aes(Station, mean, color = Letters))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  basetheme
Station_sal_p %>% filter(p.adjust < 0.05) %>% print(n = 57)
#Salinity 5 < 1 < 2/3/4
#
#Permutation ANOVA - pH 
set.seed(4321)
Station_pH <- aovp(pH ~ Station, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Station_pH)
(Station_pH_p <- rstatix::pairwise_t_test(pH ~ Station, data = ungroup(TB_WQ_df), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Station_pH_means <- merge(TB_WQ_df %>% group_by(Station) %>% rstatix::get_summary_stats(pH, show = c("n", "mean", "sd", "min", "max")) %>% 
                              dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                            biostat::make_cld(Station_pH_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Station = group, Letters = cld)))
ggplot(Station_pH_means, aes(Station, mean, color = Letters))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  basetheme
Station_pH_p %>% filter(p.adjust < 0.05) %>% arrange(Comparison) %>% print(n = 57)
#5 < 1/4 < 2/3
#
#Permutation ANOVA - DO 
set.seed(4321)
Station_DO <- aovp(DO_mgl ~ Station, data = ungroup(TB_WQ_df), perm = "", nperm = 10000)
summary(Station_DO)
(Station_DO_p <- rstatix::pairwise_t_test(DO_mgl ~ Station, data = ungroup(TB_WQ_df), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Station_DO_means <- merge(TB_WQ_df %>% group_by(Station) %>% rstatix::get_summary_stats(DO_mgl, show = c("n", "mean", "sd", "min", "max")) %>% 
                             dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                           biostat::make_cld(Station_DO_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Station = group, Letters = cld)))
ggplot(Station_DO_means, aes(Station, mean, color = Letters))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  basetheme
Station_DO_p %>% filter(p.adjust < 0.05) %>% arrange(Comparison) %>% print(n = 57)
#
#
#
#
#
###Q4: Oysters among stations - SH, TW, CI
(Oyster_Stations <- TB_CI %>% ungroup() %>% 
    dplyr::select(Station, SH, TW, CI, CI_Hanley) %>% 
    group_by(Station) %>% 
    summarise(SHeight = mean(SH, na.rm = T),
              TWeight = mean(TW, na.rm = T),
              CIndex = mean(CI, na.rm = T),
              CI_H = mean(CI_Hanley, na.rm = T))) 
#
ggarrange(ggboxplot(TB_CI, x = "Station", y = "SH"),
          ggboxplot(TB_CI, x = "Station", y = "TW"),
          ggboxplot(TB_CI, x = "Station", y = "CI"),
          ggboxplot(TB_CI, x = "Station", y = "CI_Hanley"))
#
#
#Permutation ANOVA - SH
set.seed(4321)
Station_SH <- aovp(SH ~ Station, data = ungroup(TB_CI), perm = "", nperm = 10000)
summary(Station_SH)
(Station_SH_p <- rstatix::pairwise_t_test(SH ~ Station, data = ungroup(TB_CI), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Station_SH_means <- merge(TB_CI %>% group_by(Station) %>% rstatix::get_summary_stats(SH, show = c("n", "mean", "sd", "min", "max")) %>% 
                               dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                             biostat::make_cld(Station_SH_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Station = group, Letters = cld)))
ggplot(Station_SH_means, aes(Station, mean, color = Letters))+
  geom_jitter(data = (TB_CI %>% rename(mean = SH)), aes(Station, mean), color = "black", alpha = 0.2, width = 0.15)+
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 1.5)+
  scale_y_continuous("Shell height (mm)")+
  basetheme
Station_SH_p %>% filter(p.adjust < 0.05) %>% arrange(Comparison) %>% print(n = 57)
#1 < 4 < 2 < 3 < 5
#
#Permutation ANOVA - TW
set.seed(4321)
Station_TW <- aovp(TW ~ Station, data = ungroup(TB_CI), perm = "", nperm = 10000)
summary(Station_TW)
(Station_TW_p <- rstatix::pairwise_t_test(TW ~ Station, data = ungroup(TB_CI), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Station_TW_means <- merge(TB_CI %>% group_by(Station) %>% rstatix::get_summary_stats(TW, show = c("n", "mean", "sd", "min", "max")) %>% 
                             dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                           biostat::make_cld(Station_TW_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Station = group, Letters = cld)))
ggplot(Station_TW_means, aes(Station, mean, color = Letters))+
  geom_jitter(data = (TB_CI %>% rename(mean = TW)), aes(Station, mean), color = "black", alpha = 0.2, width = 0.15)+
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 1.5)+
  scale_y_continuous("Total weight (g)")+
  basetheme
Station_TW_p %>% filter(p.adjust < 0.05) %>% arrange(Comparison) %>% print(n = 57)
#1 < 4 < 2 < 3 < 5 
#SH and TW co-linear but good to confirm
#
#Permutation ANOVA - CI
set.seed(4321)
Station_CI <- aovp(CI ~ Station, data = ungroup(TB_CI), perm = "", nperm = 10000)
summary(Station_CI)
(Station_CI_p <- rstatix::pairwise_t_test(CI ~ Station, data = ungroup(TB_CI), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Station_CI_means <- merge(TB_CI %>% group_by(Station) %>% rstatix::get_summary_stats(CI, show = c("n", "mean", "sd", "min", "max")) %>% 
                             dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                           biostat::make_cld(Station_CI_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Station = group, Letters = cld)))
ggplot(Station_CI_means, aes(Station, mean, color = Letters))+
  geom_jitter(data = (TB_CI %>% rename(mean = CI)), aes(Station, mean), color = "black", alpha = 0.2, width = 0.15)+
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 1.5)+
  scale_y_continuous("Condition index")+
  basetheme
Station_CI_p %>% filter(p.adjust < 0.05) %>% arrange(Comparison) %>% print(n = 57)
#3 <= 1/2 <= 4 < 5
#
#Permutation ANOVA - CI_Hanley
set.seed(4321)
Station_CIH <- aovp(CI_Hanley ~ Station, data = ungroup(TB_CI), perm = "", nperm = 10000)
summary(Station_CIH)
(Station_CIH_p <- rstatix::pairwise_t_test(CI_Hanley ~ Station, data = ungroup(TB_CI), p.adjust.method = "holm") %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Station_CIH_means <- merge(TB_CI %>% group_by(Station) %>% rstatix::get_summary_stats(CI_Hanley, show = c("n", "mean", "sd", "min", "max")) %>% 
                             dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                           biostat::make_cld(Station_CIH_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Station = group, Letters = cld)))
ggplot(Station_CIH_means, aes(Station, mean, color = Letters))+
  geom_jitter(data = (TB_CI %>% rename(mean = CI_Hanley)), aes(Station, mean), color = "black", alpha = 0.2, width = 0.15)+
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 1.5)+
  scale_y_continuous("Condition index (Hanley)")+
  basetheme
Station_CIH_p %>% filter(p.adjust < 0.05) %>% arrange(Comparison) %>% print(n = 57)
#2/3/5 < 4 < 1
#
#
#
#
#
#
####Pest Summary Questions (Q5-Q8)####
#
###Q5:Does Polydora and Cliona differ in parasite prevalence in TB Oysters? 
#
#Check amount of correlation between Poly and Clio
t0 <- TB_SP_df %>% subset(Measurement == "All") %>% dplyr::select(Date:Station, New_Sample_Number, Pct.Polydora, Pct.Cliona, Poly_Prev:Richness) 
t0 %>% ggplot(aes(Pct.Polydora, Pct.Cliona))+ geom_point()
cor(t0$Pct.Polydora, t0$Pct.Cliona, method = "spearman") #Mildly correlated - 0.619931
#
#Data frame of presence/absence for Polydora and Cliona for each sample
(t1 <- t0 %>% ungroup() %>% dplyr::select(Date:New_Sample_Number, Poly_Prev, Cliona_Prev) %>% 
    rename("Polydora" = Poly_Prev, "Cliona" = Cliona_Prev) %>% 
    gather("Type", "Prev", -Date, -Year, -Month, -Site, -Station, -New_Sample_Number))
#
#Compare models and select best
set.seed(54321)
Pest_model0 <- glmer(Prev ~ Type + (1|Station), family = "binomial", data = t1, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_model1 <- glmer(Prev ~ Type + (1|New_Sample_Number), family = "binomial", data = t1, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_model2 <- glmer(Prev ~ Type + (1|Station) + (1|New_Sample_Number), family = "binomial", data = t1, control=glmerControl(optCtrl=list(maxfun=10000)))
AIC(Pest_model0, Pest_model1, Pest_model2) #model 2 is best
#
#Model summary
Pest_model <- Pest_model2
rm(Pest_model0, Pest_model1, Pest_model2)
summary(Pest_model)
#Get means and Letters distinguishing significantly different groups:
(Pest_model_emm <- emmeans(Pest_model, ~Type, type = "response", adjust = "holm")) 
(Pest_p_means <- merge(t1 %>% group_by(Type) %>% rstatix::get_summary_stats(Prev, show = c("n", "mean", "sd", "min", "max")) %>% 
                         dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                       multcomp::cld(Pest_model_emm, alpha = 0.05, decreasing = TRUE, Letters = c(letters)) %>% data.frame() %>%
                         rename(Model_mean = prob, Letters = .group, Lower = asymp.LCL, Upper = asymp.UCL) %>% dplyr::select(Type, Model_mean, Lower:Letters) %>%
                         mutate(Letters = gsub(" ", "", Letters))))
##Check assumptions/model fit
plot(Pest_model, Type ~ resid(.))
testDispersion(Pest_model) #
P_mod1_res <- simulateResiduals(Pest_model)
plot(P_mod1_res, quantreg = T)
testZeroInflation(P_mod1_res)
testCategorical(P_mod1_res, catPred = t1$Type)
#Report results
Anova(Pest_model)
tidy(Pest_model)  %>% filter(effect == "fixed") %>% dplyr::select(-group, -effect)
Pest_p_means
contrast(Pest_model_emm, method = "pairwise") %>% data.frame() %>% arrange(contrast) %>% filter(p.value < 0.05) %>% dplyr::select(-df, -null)
#
Pest_p_means %>%
  ggplot(aes(Type, Model_mean, fill = Type))+
  geom_errorbar(aes(ymin = Model_mean, ymax = Upper), width = 0.5, position = position_dodge(0.9))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(y = Upper+0.03, label = Letters), position = position_dodge(0.9))+
  scale_fill_grey(start = 0.2, end = 0.7)+
  scale_x_discrete("Type")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.05)) + basetheme + 
  theme(legend.position = "none")
#
ggpredict(Pest_model, terms = c("Type"))
ggpredict(Pest_model, terms = c("Type")) |> plot(ci_style = "errorbar", dot_size = 4, line_size = 1) + 
  ggtitle("Predicted probabilities of pest prevelence")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.05)) +
  scale_color_grey(start = 0.2, end = 0.7) + basetheme
#
#
#
#
#
#
#
#
#
#
#
#
#
###Q6: Does Polydora or Cliona differ in parasite prevalence impact among shell surfaces in TB Oysters? - want # infected out of total oysters per sample (1 sample = 1 year/month/station)
#
#Check amount of correlation between Poly and Clio
t2 <- TB_SP_df %>% subset(Measurement == "External" | Measurement == "Internal") %>% 
  dplyr::select(Date:Station, New_Sample_Number, Pct.Polydora, Pct.Cliona, Poly_Prev:Richness) %>% droplevels(.)
t2 %>% ggplot(aes(Pct.Polydora, Pct.Cliona))+ geom_point()
cor(t2$Pct.Polydora, t2$Pct.Cliona, method = "spearman") #Mildly correlated - 0.5336638
#
#Data frame of presence/absence for Polydora and Cliona for each sample for Cliona External and Internal shell surfaces
(t3 <- t2 %>% ungroup() %>% dplyr::select(Date:New_Sample_Number, Measurement, Poly_Prev, Cliona_Prev) %>% 
    rename("Polydora" = Poly_Prev, "Cliona" = Cliona_Prev) %>% 
    gather("Type", "Prev", -Date, -Year, -Month, -Site, -Station, -New_Sample_Number, -Measurement))
#
ggarrange(t3 %>% ggplot(aes(Measurement, Prev, shape = Type))+ geom_jitter(size = 3, alpha = 0.5, width = 0.25, height = 0.25),
          t3 %>% ggplot(aes(Measurement, Prev, shape = Type, color = Station))+ geom_jitter(size = 3, alpha = 0.5, width = 0.25, height = 0.25))
#
#Compare models and select best
set.seed(54321)
Pest_surface0 <- glmer(Prev ~ Type + Measurement + (1|Station), family = "binomial", data = t3, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_surface1 <- glmer(Prev ~ Type + Measurement + (1|New_Sample_Number), family = "binomial", data = t3, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_surface2 <- glmer(Prev ~ Type * Measurement + (1|Station), family = "binomial", data = t3, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_surface3 <- glmer(Prev ~ Type * Measurement + (1|New_Sample_Number), family = "binomial", data = t3, control=glmerControl(optCtrl=list(maxfun=10000)))
AIC(Pest_surface0, Pest_surface1, Pest_surface2, Pest_surface3) #model 2 is best
#
#Model summary
Pest_surface <- Pest_surface2
rm(Pest_surface0, Pest_surface1, Pest_surface2, Pest_surface3)
summary(Pest_surface)
#Get means and Letters distinguishing significantly different groups:
(Pest_surface_emm <- emmeans(Pest_surface, ~Measurement*Type, type = "response", adjust = "holm")) 
(Pest_surface_means <- merge(t3 %>% group_by(Type, Measurement) %>% rstatix::get_summary_stats(Prev, show = c("n", "mean", "sd", "min", "max")) %>% 
                         dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                       multcomp::cld(Pest_surface_emm, alpha = 0.05, decreasing = TRUE, Letters = c(letters)) %>% data.frame() %>%
                         rename(Model_mean = prob, Letters = .group, Lower = asymp.LCL, Upper = asymp.UCL) %>% dplyr::select(Type, Measurement, Model_mean, Lower:Letters) %>%
                         mutate(Letters = gsub(" ", "", Letters))))
##Check assumptions/model fit
plot(Pest_surface, Type ~ resid(.))
testDispersion(Pest_surface) 
P_surface_res <- simulateResiduals(Pest_surface)
plot(P_surface_res, quantreg = T)
testZeroInflation(P_surface_res)
testCategorical(P_surface_res, catPred = t3$Type)
testCategorical(P_surface_res, catPred = t3$Measurement)
#Report results
Anova(Pest_surface)
tidy(Pest_surface)  %>% filter(effect == "fixed") %>% dplyr::select(-group, -effect) %>% mutate(term = gsub("Measurement", "Meas-", term))
Pest_surface_means
contrast(Pest_surface_emm, method = "pairwise") %>% data.frame() %>% arrange(contrast) %>% filter(p.value < 0.05) %>% dplyr::select(-df, -null) 
#
Pest_surface_means %>%
  ggplot(aes(Type, Model_mean, fill = Measurement))+
  geom_errorbar(aes(ymin = Model_mean, ymax = Upper), width = 0.5, position = position_dodge(0.9))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(y = Upper+0.03, label = Letters), position = position_dodge(0.9))+
  scale_fill_grey(start = 0.2, end = 0.7)+
  scale_x_discrete("Type")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.05)) + basetheme 
#
ggpredict(Pest_surface, terms = c("Measurement", "Type"))
ggpredict(Pest_surface, terms = c("Type", "Measurement")) |> plot(ci_style = "errorbar", dot_size = 4, line_size = 1, dodge = -0.55) + 
  ggtitle("Predicted probabilities of pest prevelence")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.05)) +
  scale_color_grey(start = 0.2, end = 0.7) + basetheme
#
#
#
#
#
#
#
#
#
#
###Q7: Does Polydora or Cliona differ in parasite prevalence impact among shell position in TB Oysters? - want # infected out of total oysters per sample (1 sample = 1 year/month/station)
#
#Check amount of correlation between Poly and Clio
t4 <- TB_SP_df %>% subset(Measurement == "Top" | Measurement == "Bot") %>% 
  dplyr::select(Date:Station, New_Sample_Number, Pct.Polydora, Pct.Cliona, Poly_Prev:Richness) %>% droplevels(.)
t4 %>% ggplot(aes(Pct.Polydora, Pct.Cliona))+ geom_point()
cor(t4$Pct.Polydora, t4$Pct.Cliona, method = "spearman") #Mildly correlated - 0.4045071
#
#Data frame of presence/absence for Polydora and Cliona for each sample for Cliona External and Internal shell surfaces
(t5 <- t4 %>% ungroup() %>% dplyr::select(Date:New_Sample_Number, Measurement, Poly_Prev, Cliona_Prev) %>% 
    rename("Polydora" = Poly_Prev, "Cliona" = Cliona_Prev) %>% 
    gather("Type", "Prev", -Date, -Year, -Month, -Site, -Station, -New_Sample_Number, -Measurement))
#
ggarrange(t5 %>% ggplot(aes(Measurement, Prev, shape = Type))+ geom_jitter(size = 3, alpha = 0.5, width = 0.25, height = 0.25),
          t5 %>% ggplot(aes(Measurement, Prev, shape = Type, color = Station))+ geom_jitter(size = 3, alpha = 0.5, width = 0.25, height = 0.25))
#
#Compare models and select best
set.seed(54321)
Pest_pos0 <- glmer(Prev ~ Type + Measurement + (1|Station), family = "binomial", data = t5, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_pos1 <- glmer(Prev ~ Type + Measurement + (1|New_Sample_Number), family = "binomial", data = t5, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_pos2 <- glmer(Prev ~ Type * Measurement + (1|Station), family = "binomial", data = t5, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_pos3 <- glmer(Prev ~ Type * Measurement + (1|New_Sample_Number), family = "binomial", data = t5, control=glmerControl(optCtrl=list(maxfun=10000)))
AIC(Pest_pos0, Pest_pos1, Pest_pos2, Pest_pos3) #model 2 is best
#
#Model summary
Pest_position <- Pest_pos2
rm(Pest_pos0, Pest_pos1, Pest_pos2, Pest_pos3)
summary(Pest_position)
#Get means and Letters distinguishing significantly different groups:
(Pest_pos_emm <- emmeans(Pest_position, ~Measurement*Type, type = "response", adjust = "holm")) 
(Pest_pos_means <- merge(t5 %>% group_by(Type, Measurement) %>% rstatix::get_summary_stats(Prev, show = c("n", "mean", "sd", "min", "max")) %>% 
                               dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                             multcomp::cld(Pest_pos_emm, alpha = 0.05, decreasing = TRUE, Letters = c(letters)) %>% data.frame() %>%
                               rename(Model_mean = prob, Letters = .group, Lower = asymp.LCL, Upper = asymp.UCL) %>% dplyr::select(Type, Measurement, Model_mean, Lower:Letters) %>%
                               mutate(Letters = gsub(" ", "", Letters))))
##Check assumptions/model fit
plot(Pest_position, Type ~ resid(.))
testDispersion(Pest_position) 
P_pos_res <- simulateResiduals(Pest_position)
plot(P_pos_res, quantreg = T)
testZeroInflation(P_pos_res)
testCategorical(P_pos_res, catPred = t3$Type)
testCategorical(P_pos_res, catPred = t3$Measurement)
#Report results
Anova(Pest_position)
tidy(Pest_position)  %>% filter(effect == "fixed") %>% dplyr::select(-group, -effect) %>% mutate(term = gsub("Measurement", "Meas-", term))
Pest_pos_means
contrast(Pest_pos_emm, method = "pairwise") %>% data.frame() %>% arrange(contrast) %>% filter(p.value < 0.05) %>% dplyr::select(-df, -null) 
#
Pest_pos_means %>%
  ggplot(aes(Type, Model_mean, fill = Measurement))+
  geom_errorbar(aes(ymin = Model_mean, ymax = Upper), width = 0.5, position = position_dodge(0.9))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(y = Upper+0.03, label = Letters), position = position_dodge(0.9))+
  scale_fill_grey(start = 0.2, end = 0.7)+
  scale_x_discrete("Type")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.05)) + basetheme 
#
ggpredict(Pest_position, terms = c("Measurement", "Type"))
ggpredict(Pest_position, terms = c("Type", "Measurement")) |> plot(ci_style = "errorbar", dot_size = 4, line_size = 1, dodge = -0.55) + 
  ggtitle("Predicted probabilities of pest prevelence")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.05)) +
  scale_color_grey(start = 0.2, end = 0.7) + basetheme
#
#
#
#
#
#
#
#
#
###Q8: Does Polydora and Cliona differ in parasite prevalence in TB Oysters among stations?
#
head(t1)
#Compare models and select best
set.seed(54321)
Pest_stations0 <- glmer(Prev ~ Type + (1|Station), family = "binomial", data = t1, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_stations1 <- glmer(Prev ~ Type + Station + (1|New_Sample_Number), family = "binomial", data = t1, control=glmerControl(optCtrl=list(maxfun=10000)))
Pest_stations2 <- glmer(Prev ~ Type * Station + (1|New_Sample_Number), family = "binomial", data = t1, control=glmerControl(optCtrl=list(maxfun=10000)))
AIC(Pest_stations0, Pest_stations1, Pest_stations2) #model2 is best
#
#Model summary
Pest_stations_model <- Pest_stations2
summary(Pest_stations_model)
#Get means and Letters distinguishing significantly different groups:
(Pest_stations_emm <- emmeans(Pest_stations_model, ~Type*Station, type = "response", adjust = "holm")) 
(Pest_stations_p_means <- merge(t1 %>% group_by(Type, Station) %>% rstatix::get_summary_stats(Prev, show = c("n", "mean", "sd", "min", "max")) %>% 
                         dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                       multcomp::cld(Pest_stations_emm, alpha = 0.05, decreasing = TRUE, Letters = c(letters)) %>% data.frame() %>%
                         rename(Model_mean = prob, Letters = .group, Lower = asymp.LCL, Upper = asymp.UCL) %>% dplyr::select(Station, Type, Model_mean, Lower:Letters) %>%
                         mutate(Letters = gsub(" ", "", Letters))))
##Check assumptions/model fit
plot(Pest_stations_model, Type ~ resid(.))
testDispersion(Pest_stations_model) #
P_sta_mod1_res <- simulateResiduals(Pest_stations_model)
plot(P_sta_mod1_res, quantreg = T)
testZeroInflation(P_sta_mod1_res)
testCategorical(P_sta_mod1_res, catPred = t1$Type)
testCategorical(P_sta_mod1_res, catPred = t1$Station)
#Report results
Anova(Pest_stations_model)
tidy(Pest_stations_model)  %>% filter(effect == "fixed") %>% dplyr::select(-group, -effect)
Pest_stations_p_means
contrast(Pest_stations_emm, method = "pairwise") %>% data.frame() %>% arrange(contrast) %>% filter(p.value < 0.05) %>% dplyr::select(-df, -null)
contrast(Pest_stations_emm, method = "pairwise") %>% data.frame() %>% arrange(contrast) %>% filter(p.value >= 0.05) %>% dplyr::select(-df, -null)
#
Pest_stations_p_means %>%
  ggplot(aes(Type, Model_mean, fill = Station))+
  geom_errorbar(aes(ymin = Model_mean, ymax = Upper), width = 0.5, position = position_dodge(0.9))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(y = Upper+0.03, label = Letters), position = position_dodge(0.9))+
  scale_fill_grey(start = 0.2, end = 0.7)+
  scale_x_discrete("Type")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.05)) + basetheme + 
  theme(legend.position = "none")
#
ggpredict(Pest_stations_model, terms = c("Type", "Station")) |> plot(ci_style = "errorbar", dot_size = 4, line_size = 1) + 
  ggtitle("Predicted probabilities of pest prevelence")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.05)) +
  scale_color_grey(start = 0.2, end = 0.7) + basetheme
ggpredict(Pest_stations_model, terms = c("Type", "Station"))
#
#
#
#
#
#
#
#
####Trends (Q9-Q12)####
#
##Q9: What is the relationship between Polydora or Cliona with CI?
(c1 <- Combined_df %>% ungroup() %>% filter(Measurement == "All") %>% 
  mutate(Type = as.factor(case_when(Poly_Prev == 0 & Cliona_Prev == 1 ~ "Cliona",
                          Poly_Prev == 1 & Cliona_Prev == 0 ~ "Polydora",
                          Poly_Prev == 1 & Cliona_Prev == 1 ~ "Both",
                          TRUE ~ "Neither"))) %>% 
   mutate(Type = fct_relevel(Type, "Polydora", "Cliona", "Both", "Neither"), Station = as.integer(Station)) %>%
  dplyr::select(Date:New_Sample_Number, Type, CI, CI_Hanley))
#
##We know CI is different among Station (see Q4) but we're just interested in the overall comparison. 
#
#Dependent = CI value 
#Independent = Type
ggboxplot(c1, x = "Type", y = "CI_Hanley", fill = "Station")
ggboxplot(c1, x = "Type", y = "CI", fill = "Station")
#
#Permutation ANOVA - CI
set.seed(4321)
Type_CI <- aovp(CI ~ Type * Station, data = ungroup(c1), perm = "", nperm = 10000)
summary(Type_CI) 
#
(Type_CI_p <- emmeans_test(c1, CI ~ Type, covariate = Station, p.adjust.method = "holm") %>% 
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>% 
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Type_CI_means <- merge(c1 %>% group_by(Type) %>% rstatix::get_summary_stats(CI, show = c("n", "mean", "sd", "min", "max")) %>% 
                          transform(lower = mean-sd, upper = mean+sd),
                        get_emmeans(emmeans_test(c1, CI ~ Type, covariate = Station, p.adjust.method = "holm")) %>% dplyr::select(-c(Station, method))) %>%
    mutate(Letters = c("b", "ab", "b", "a")))
#Report results
summary(Type_CI)
Type_CI_means
Type_CI_p %>% arrange(Comparison)
#
Type_CI_means %>%
  ggplot(aes(Type, emmean, fill = Letters))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5, position = position_dodge(0.9))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(y = conf.high+0.3, label = Letters), position = position_dodge(0.9))+
  scale_fill_grey(start = 0.2, end = 0.7)+
  scale_x_discrete("Type")+
  scale_y_continuous("Average condition of oysters (emmeans)", expand = c(0,0), limits = c(0,8)) + basetheme + 
  theme(legend.position = "none")
#
#
#
#Permutation ANOVA - CI_Hanley
set.seed(4321)
Type_CI_H <- aovp(CI_Hanley ~ Type * Station, data = c1, perm = "", nperm = 10000)
summary(Type_CI_H) 
#
(Type_CI_H_p <- emmeans_test(c1, CI_Hanley ~ Type, covariate = Station, p.adjust.method = "holm") %>% 
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>% 
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Type_CI_H_means <- merge(c1 %>% group_by(Type) %>% rstatix::get_summary_stats(CI_Hanley, show = c("n", "mean", "sd", "min", "max")) %>% 
                          transform(lower = mean-sd, upper = mean+sd),
                        get_emmeans(emmeans_test(c1, CI_Hanley ~ Type, covariate = Station, p.adjust.method = "holm")) %>% dplyr::select(-c(Station, method))) %>%
    mutate(Letters = c("a", "a", "b", "a")))

#
#Report results
summary(Type_CI_H)
Type_CI_H_means
Type_CI_H_p %>% arrange(Comparison)
#
Type_CI_H_means %>%
  ggplot(aes(Type, emmean, fill = Letters))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5, position = position_dodge(0.9))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(y = conf.high+0.7, label = Letters), position = position_dodge(0.9))+
  scale_fill_grey(start = 0.2, end = 0.7)+
  scale_x_discrete("Type")+
  scale_y_continuous("Average condition of oysters", expand = c(0,0), limits = c(0,30)) + basetheme + 
  theme(legend.position = "none")
#
#
#
#
#
#
#
#
##Q10: What is the relationship between Polydora and Cliona proportion affected, and between percentage affected? (Wilcoxon rank sum test)
#
(c2 <- Combined_df %>% ungroup() %>% filter(Measurement == "All") %>% 
  mutate(Type = as.factor(case_when(Poly_Prev == 0 & Cliona_Prev == 1 ~ "Cliona",
                                    Poly_Prev == 1 & Cliona_Prev == 0 ~ "Polydora",
                                    Poly_Prev == 1 & Cliona_Prev == 1 ~ "Both",
                                    TRUE ~ "Neither"))) %>% 
  mutate(Type = fct_relevel(Type, "Polydora", "Cliona", "Both", "Neither")) %>%
  dplyr::select(Year:Station, Type))
#
#Select and reformat data
(Trends <- left_join(c2 %>% ungroup() %>% group_by(Year, Month, Site, Station, Type) %>% 
                       summarise(Count = n()), 
                     c2 %>% ungroup() %>% group_by(Year, Month, Site, Station) %>% summarise(Total = n())) %>% 
    mutate(Prop = Count/Total))
#Summarize
Trends %>% group_by(Type) %>% get_summary_stats(Prop, type = c("full")) %>% dplyr::select(-iqr, -mad, -ci)
#Visualize data
Trends %>% 
  ggplot(aes(Type, Prop))+
  geom_boxplot(fill = "#999999", size = 1, outlier.shape = 18, outlier.size = 5)+
  geom_jitter(width = 0.2, alpha = 0.4)+
  scale_y_continuous("Proportion", expand = c(0,0), limits = c(0,1.1))+
  basetheme
#
#Wilcoxon test and significance
(Trend_WC <- left_join(Trends %>% ungroup() %>% dplyr::select(Type, Prop) %>%
  wilcox_test(Prop ~ Type, p.adjust.method = "holm") %>% add_significance(),
  Trends %>% ungroup() %>% dplyr::select(Type, Prop) %>% wilcox_effsize(Prop ~ Type)) %>%
    dplyr::select(-.y., -p) %>% rename("effect_size" = effsize))
#
#
#
(Pct_trends <- TB_SP_df %>% subset(Measurement == "All") %>%
  mutate(Type = as.factor(case_when(Pct.Polydora == 0 & Pct.Cliona > 0 ~ "Cliona",
                          Pct.Polydora > 0 & Pct.Cliona == 0 ~ "Polydora",
                          Pct.Polydora > 0 & Pct.Cliona > 0 ~ "Both",
                          TRUE ~ "None"))) %>% filter(Type != "None") %>% droplevels() %>%
  mutate(Type = fct_relevel(Type, "Polydora", "Cliona", "Both"),
         Pct = as.numeric(case_when(Type == "Polydora" ~ Pct.Polydora,
                                    Type == "Cliona" ~ Pct.Cliona,
                                    Type == "Both" ~ Pct_Affected,
                                    TRUE ~ NA))) %>% ungroup() %>% dplyr::select(Type, Pct))
Pct_trends %>% group_by(Type) %>% get_summary_stats(Pct, type = c("full")) %>% dplyr::select(-iqr, -mad, -ci)
#
#Visualize data
Pct_trends %>% 
  ggplot(aes(Type, Pct))+
  geom_boxplot(fill = "#999999", size = 1, outlier.shape = 18, outlier.size = 5)+
  geom_jitter(width = 0.2, alpha = 0.4)+
  scale_y_continuous("Percentage", expand = c(0,0), limits = c(0,100))+
  basetheme
#
#Wilcoxon test and significance
(Pct_trend_WC <- left_join(Pct_trends %>% ungroup() %>% dplyr::select(Type, Pct) %>%
                             wilcox_test(Pct ~ Type, p.adjust.method = "holm") %>% add_significance(),
                         Pct_trends %>% wilcox_effsize(Pct ~ Type)) %>%
    dplyr::select(-.y., -p) %>% rename("effect_size" = effsize))
#
#
#
#What is the relationship between Polydora and Cliona with percent of shell affect if we control for CI?
#CI = ratio between shell weight and tissue weight
#Need CI data and Pct affected for each sample
#need data frame with all Pct Cliona and all Pct Polydora (no "Both")
(c3 <- full_join(TB_SP_df %>% subset(Measurement == "All") %>% dplyr::select(Year:Station, New_Sample_Number:Pct.Cliona) %>% 
                   gather("Type", "Percent", Pct.Polydora:Pct.Cliona) %>% mutate(Type = str_remove(Type, "Pct.")), 
                 TB_CI %>% dplyr::select(Site:TW, CI, CI_Hanley, New_Sample_Number:Station_Name)) %>%
    drop_na(Percent) %>% mutate(CI_Hanley_i = as.integer(CI_Hanley)) %>% ungroup())
#
ggscatter(c3, x = "CI_Hanley", y = "Percent", color = "Type", add = "reg.line") + 
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Type))
#
#Permutation ANCOVA
set.seed(4321)
Pest_CI_test <- aovp(Percent ~ Type * CI_Hanley, data = c3,  perm = "", nperm = 10000, center = TRUE)
summary(Pest_CI_test)
plot(Pest_CI_test$residuals)
plot(c3$Percent, c3$CI_Hanley)
#
(Pest_CI_p <- rstatix::pairwise_t_test(Percent ~ Type, data = c3, p.adjust.method = "holm", paired = TRUE) %>%
    dplyr::select(group1, group2, p, p.adj) %>% mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
    dplyr::select(Comparison, everything(), -group1, -group2) %>% rename(p.value = p, p.adjust = p.adj))   #Move 'Comparison' to front and drop grp1 & grp2
(Pest_CI_means <- merge(c3 %>% group_by(Type) %>% rstatix::get_summary_stats(Percent, show = c("n", "mean", "sd", "min", "max")) %>% 
                          transform(lower = mean-sd, upper = mean+sd),
                        biostat::make_cld(Pest_CI_p) %>% dplyr::select(-c(spaced_cld)) %>% rename(Type = group, Letters = cld)))

#
ggplot(Pest_CI_means, aes(Type, mean, color = Letters))+
  #geom_jitter(data = (c3 %>% rename(mean = CI_Hanley)), aes(Station, mean), color = "black", alpha = 0.2, width = 0.15)+
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 1.5)+
  scale_y_continuous("Mean percent of shell affected")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  basetheme

ggplot()+
  geom_boxplot(data = c3, aes(Type, Percent), fill = "#999999", size = 1, outlier.shape = 18, outlier.size = 5)+
  scale_y_continuous("Percentage", expand = c(0,0), limits = c(0,100))+
  basetheme
#
#
#
#
#
#
#
#
#
##Q11:Has the amount of Polydora or Cliona at each station changed over time?
#
head(Trends)
Trends2 <- Trends %>% ungroup() %>% complete(Year, Type, Month, Station, fill = list(Site = "TB", Count = 0, Total = 5, Prop = 0)) %>%
  mutate(Prop = case_when(Year == "2020" & Month == "04" ~ NA, Year == "2020" & Month == "05" ~ NA, TRUE ~ Prop)) 
#Comparing within each station, nested by pest Type (to compare within each pest type)
#
###Polydora
set.seed(4321)
Poly_mod <- aovp(Prop ~ Year * Station, data = ungroup(Trends2 %>% filter(Type == "Polydora")), perm = "", nperm = 10000)
summary(Poly_mod)
tidy(Poly_mod)
#
(Poly_p <- rbind(TukeyHSD(Poly_mod, Year = "Tukey")$'Year' %>% as.data.frame(), TukeyHSD(Poly_mod, Year = "Tukey")$'Station' %>% as.data.frame()) %>% 
  rownames_to_column("Comp") %>% rename(lower = lwr, upper = upr, p.adjust = 'p adj'))
(Poly_means <- left_join(rbind(Trends2 %>% filter(Type == "Polydora") %>% group_by(Year) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% rename(Level = Year),
Trends2 %>% filter(Type == "Polydora") %>% group_by(Station) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% rename(Level = Station)),
rbind((multcompLetters4(Poly_mod, TukeyHSD(Poly_mod, Year = "Tukey"))$`Year` %>% as.data.frame.list())  %>% dplyr::select(Letters), 
      (multcompLetters4(Poly_mod, TukeyHSD(Poly_mod, Station = "Tukey"))$`Station` %>% as.data.frame.list()) %>% dplyr::select(Letters)) %>% 
  rownames_to_column("Level")))
#
(Poly_int_p <- TukeyHSD(Poly_mod, Year = "Tukey")$'Year:Station' %>% as.data.frame() %>% rownames_to_column("Comp") %>% 
    rename(lower = lwr, upper = upr, p.adjust = 'p adj'))
(Poly_int_means <- merge(Trends2 %>% filter(Type == "Polydora") %>% group_by(Year, Station) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% 
                       mutate(Comp = paste(Year, Station, sep  = ":")),
                     (multcompLetters4(Poly_mod, TukeyHSD(Poly_mod, Year = "Tukey"))$`Year:Station` %>% as.data.frame.list()) %>% 
                       dplyr::select(Letters) %>% rownames_to_column("Comp")))
#
Poly_p %>% filter(p.adjust < 0.05) %>% arrange(Comp)
Poly_int_p %>% filter(p.adjust < 0.05) %>% arrange(Comp)
#
Poly_int_means %>%
  ggplot(aes(Year, mean, color = Station))+
  geom_point(size = 3)+ geom_line(aes(group = Station), linewidth = 1)+
  scale_fill_grey(start = 0.2, end = 0.7)+
  basetheme+ 
  scale_y_continuous("Average proportion", expand = c(0,0), limits = c(0, 0.5))
#
#
###Cliona
set.seed(4321)
Clio_mod <- aovp(Prop ~ Year * Station, data = ungroup(Trends2 %>% filter(Type == "Cliona")), perm = "", nperm = 10000)
summary(Clio_mod)
tidy(Clio_mod)
#
(Clio_p <- rbind(TukeyHSD(Clio_mod, Year = "Tukey")$'Year' %>% as.data.frame(), TukeyHSD(Clio_mod, Year = "Tukey")$'Station' %>% as.data.frame()) %>% 
    rownames_to_column("Comp") %>% rename(lower = lwr, upper = upr, p.adjust = 'p adj'))
(Clio_means <- left_join(rbind(Trends2 %>% filter(Type == "Cliona") %>% group_by(Year) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% rename(Level = Year),
                               Trends2 %>% filter(Type == "Cliona") %>% group_by(Station) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% rename(Level = Station)),
                         rbind((multcompLetters4(Clio_mod, TukeyHSD(Clio_mod, Year = "Tukey"))$`Year` %>% as.data.frame.list())  %>% dplyr::select(Letters), 
                               (multcompLetters4(Clio_mod, TukeyHSD(Clio_mod, Station = "Tukey"))$`Station` %>% as.data.frame.list()) %>% dplyr::select(Letters)) %>% 
                           rownames_to_column("Level")))
#
(Clio_int_p <- TukeyHSD(Clio_mod, Year = "Tukey")$'Year:Station' %>% as.data.frame() %>% rownames_to_column("Comp") %>% 
    rename(lower = lwr, upper = upr, p.adjust = 'p adj'))
(Clio_int_means <- merge(Trends2 %>% filter(Type == "Cliona") %>% group_by(Year, Station) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% 
                           mutate(Comp = paste(Year, Station, sep  = ":")),
                         (multcompLetters4(Clio_mod, TukeyHSD(Clio_mod, Year = "Tukey"))$`Year:Station` %>% as.data.frame.list()) %>% 
                           dplyr::select(Letters) %>% rownames_to_column("Comp")))
#
Clio_p %>% filter(p.adjust < 0.05) %>% arrange(Comp)
Clio_int_p %>% filter(p.adjust < 0.05) %>% arrange(Comp)
#
Clio_int_means %>%
  ggplot(aes(Year, mean, color = Station))+
  geom_point(size = 3)+ geom_line(aes(group = Station), linewidth = 1)+
  scale_fill_grey(start = 0.2, end = 0.7)+
  basetheme+ 
  scale_y_continuous("Average proportion", expand = c(0,0), limits = c(0, 0.5))
#
#
###Both
set.seed(4321)
Both_mod <- aovp(Prop ~ Year * Station, data = ungroup(Trends2 %>% filter(Type == "Both")), perm = "", nperm = 10000)
summary(Both_mod)
tidy(Both_mod)
#
(Both_p <- rbind(TukeyHSD(Both_mod, Year = "Tukey")$'Year' %>% as.data.frame(), TukeyHSD(Both_mod, Year = "Tukey")$'Station' %>% as.data.frame()) %>% 
    rownames_to_column("Comp") %>% rename(lower = lwr, upper = upr, p.adjust = 'p adj'))
(Both_means <- left_join(rbind(Trends2 %>% filter(Type == "Both") %>% group_by(Year) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% rename(Level = Year),
                               Trends2 %>% filter(Type == "Both") %>% group_by(Station) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% rename(Level = Station)),
                         rbind((multcompLetters4(Both_mod, TukeyHSD(Both_mod, Year = "Tukey"))$`Year` %>% as.data.frame.list())  %>% dplyr::select(Letters), 
                               (multcompLetters4(Both_mod, TukeyHSD(Both_mod, Station = "Tukey"))$`Station` %>% as.data.frame.list()) %>% dplyr::select(Letters)) %>% 
                           rownames_to_column("Level")))
#
(Both_int_p <- TukeyHSD(Both_mod, Year = "Tukey")$'Year:Station' %>% as.data.frame() %>% rownames_to_column("Comp") %>% 
    rename(lower = lwr, upper = upr, p.adjust = 'p adj'))
(Both_int_means <- merge(Trends2 %>% filter(Type == "Both") %>% group_by(Year, Station) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% 
                           mutate(Comp = paste(Year, Station, sep  = ":")),
                         (multcompLetters4(Both_mod, TukeyHSD(Both_mod, Year = "Tukey"))$`Year:Station` %>% as.data.frame.list()) %>% 
                           dplyr::select(Letters) %>% rownames_to_column("Comp")))
#
Both_p %>% filter(p.adjust < 0.05) %>% arrange(Comp)
Both_int_p %>% filter(p.adjust < 0.05) %>% arrange(Comp)
#
Both_int_means %>%
  ggplot(aes(Year, mean, color = Station))+
  geom_point(size = 3)+ geom_line(aes(group = Station), linewidth = 1)+
  scale_fill_grey(start = 0.2, end = 0.7)+
  basetheme+ 
  scale_y_continuous("Average proportion", expand = c(0,0), limits = c(0, 1))
#
#
#
#
#
#
#
#
#
#
#
##Q12: #Which WQ parameters best explain the trend observed in Polydora or Cliona? (glm)
#Remove station since it's a confounding factor - want to look at each WQ param on its own
#Grouping months by season according to clustering analyses of 2002-2023 data.
(Trends_WQ <- full_join(Trends2 %>% dplyr::select(-Site), 
                        ungroup(TB_WQ_df) %>% dplyr::select(Year, Season_c, Month, Station, DO_mgl, Salinity, Temperature, Turbidity, pH)) %>%
    mutate(Season_c = as.factor(Season_c),
           MonthYear = interaction(Month, Year), MonthSta = interaction(Month, Station)) %>% droplevels(.)) #continuous Month for covariate
#
##Polydora
#
#initial MLR
(Poly_df <- Trends_WQ[complete.cases(Trends_WQ),] %>% filter(Type == "Polydora") %>% dplyr::select(-Type)) 
Poly_df %>% ggplot(aes(x = Prop))+ geom_histogram(aes(y = ..count..)) #skewed/proportional needs transformation
mean(Poly_df$Prop == 0) #Proportion of 0s = 0.563981
cor(Poly_df[,c(8:12)]) #Temp and DO
#Scale continuous variables
Poly_df$Sal_s <- scale(Poly_df$Salinity)[,1]
Poly_df$Temp_s <- scale(Poly_df$Temperature)[,1]
Poly_df$Turb_s <- scale(Poly_df$Turbidity)[,1]
Poly_df$pH_s <- scale(Poly_df$pH)[,1]
head(Poly_df)
#
#
set.seed(4321)
fullPoly <- glmer(cbind(Count, Total-Count) ~ Year + Season_c + Sal_s + Temp_s + Turb_s + pH_s + (1|MonthSta), 
                  data = Poly_df, family = binomial, glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
summary(fullPoly)
testDispersion(simulateResiduals(fullPoly, n = 5000, plot = T)) #mild
testZeroInflation(simulateResiduals(fullPoly, n = 5000, plot = T))
performance::check_collinearity(fullPoly) #Check model 
#
(Polystep <- drop1(fullPoly, test = "Chisq")) #Test for significant factors
#Try model without Season (VIF > 3) and model based on ChiSq test
set.seed(4321)
fullPoly2 <- glmer(cbind(Count, Total-Count) ~ Year + Sal_s + Temp_s + Turb_s + pH_s + (1|MonthSta), 
                   data = Poly_df, family = binomial, glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
set.seed(4321)
fullPoly3 <- glmer(cbind(Count, Total-Count) ~ Year + pH_s + (1|MonthSta), 
                   data = Poly_df, family = binomial, glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
#
model.sel(fullPoly, fullPoly2, fullPoly3) #Full model better since lower AICc
#
Poly_rawMeansSDs <- Poly_df %>% summarise(mnTemp = mean(Temperature), sdTemp = sd(Temperature), 
                                          mnSal = mean(Salinity), sdSal = sd(Salinity), mnpH = mean(pH), sdpH = sd(pH))
#
predPoly <- expand.grid(Year = unique(Poly_df$Year), pH_s = seq(min(Poly_df$pH_s), max(Poly_df$pH_s), length.out = 80), 
                        Temp_s = c(min(Poly_df$Temp_s), 0, max(Poly_df$Temp_s)), Sal_s = c(min(Poly_df$Sal_s), 0, max(Poly_df$Sal_s)), MonthSta = unique(Poly_df$MonthSta))
## Back-transform the standardized means for plotting
predPoly$pH <- predPoly$pH_s*Poly_rawMeansSDs$sdpH + Poly_rawMeansSDs$mnpH
predPoly$Temp <- predPoly$Temp_s*Poly_rawMeansSDs$sdTemp + Poly_rawMeansSDs$mnTemp
predPoly$Sal <- predPoly$Sal_s*Poly_rawMeansSDs$sdSal + Poly_rawMeansSDs$mnSal
#
# Make predictions to the pH data frame using the best-fitting model
predicted_Poly <- predict(fullPoly3, newdata = predPoly, type = "link", se.fit = T)
preddsT <- data.frame(predPoly, fit = predicted_Poly$fit, se.fit = predicted_Poly$se.fit)

# Exponentiate predictions to go from log to real scale. It's not valid to assume normality of the variable after exponentiating, so we calculate lower and upper 95% CLs at the same time.
preddsT$mean <- exp(preddsT$fit)
preddsT$lwr <- exp(preddsT$fit - 1.96*preddsT$se.fit)
preddsT$upr <- exp(preddsT$fit + 1.96*preddsT$se.fit)
#
min(preddsT$Temp); mean(preddsT$Temp); max(preddsT$Temp)
min(preddsT$Sal); mean(preddsT$Sal); max(preddsT$Sal)
# Convert Temp and Sal to factors to make labeling easier in the plot 
preddsT$Temp <- factor(preddsT$Temp, labels = c("min: 13.1", "mean: 23.3", "max: 31.9"))
preddsT$Sal <- factor(preddsT$Sal, labels = c("min: 14.8", "mean: 26.2", "max: 35.06"))
#
#Plot annual predicted based on pH and min/mean/max Temps and Salinity
preddsT %>% group_by(Year, pH_s, Temp_s, Sal_s, pH, Temp, Sal) %>% 
  summarise(fit = mean(fit), se.fit = mean(se.fit), mean = mean(mean), lwr = mean(lwr), upr = mean(upr)) %>%
  ggplot(aes(pH, mean, color = Year))+
  geom_point()+
  lemon::facet_rep_grid(Sal~Temp)
#
predicted_Poly <- data.frame(predW = predict(fullPoly, Poly_df, type = "response"), Year = Poly_df$Year) #%>%
#mutate(predW_bt = predW.fit*(pi/2), predW_sefit_bt = predW.se.fit*(pi/2)) %>% unique(.)
(modelPolyWQ <- predicted_Poly %>% dplyr::select(Year, predW.fit) %>% 
    group_by(Year) %>% dplyr::summarise(predW = mean(predW.fit, na.rm = T), se = mean(predW.se.fit)))#, 
#predW_bt = mean(predW_bt), se_bt = mean(predW_sefit_bt)))
#https://campus.datacamp.com/courses/hierarchical-and-mixed-effects-models-in-r/linear-mixed-effect-models?ex=5
#
summary(fullPoly)
confint(fullPoly)
#
fullPoly_tab <- tidy(fullPoly)
names(fullPoly_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullPoly_tab
#
summary(fullPoly)
#
#Updated model - keep lcose variables to double check
fullPoly2 <- update(fullPoly, .~. -Season -Salinity -Turbidity, data = Poly_df)
summary(fullPoly2) #DO and Temp still not significant so remove from model
fullPoly3 <- update(fullPoly2, .~. -DO_mgl -Temperature, data = Poly_df)
###Reporting final model
fullPoly3
fullPoly3_sum <- summary(fullPoly3)
#MLR table with test values
fullPoly3_tab <- tidy(fullPoly3)
names(fullPoly3_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullPoly3_sum_tab <- glance(fullPoly3) 
names(fullPoly3_sum_tab) <- c("Null_Dev", "df_null", "LL", "AIC", "BIC", "Deviance", "df_redis", "n")
fullPoly3_tab
#McFadden's R2:1-deviance/null.deviance)
summary(fullPoly3)
1-(72.234/76.719) #0.05846009
#
predicted_Poly <- data.frame(predW = predict(fullPoly3, Poly_df, type = "response", se.fit = TRUE), Year = Poly_df$Year) #%>%
  #mutate(predW_bt = predW.fit*(pi/2), predW_sefit_bt = predW.se.fit*(pi/2)) %>% unique(.)
(modelPolyWQ <- predicted_Poly %>% dplyr::select(Year, predW.fit, everything()) %>% dplyr::select(-predW.residual.scale) %>%
    group_by(Year) %>% dplyr::summarise(predW = mean(predW.fit, na.rm = T), se = mean(predW.se.fit)))#, 
                                        #predW_bt = mean(predW_bt), se_bt = mean(predW_sefit_bt)))
#
Poly_df %>% group_by(Year) %>% summarise(AveProp = mean(Prop, na.rm = T)) %>%
  ggplot()+
  geom_point(aes(Year, AveProp, color = "Mean"))+
  geom_line(data = modelPolyWQ, aes(Year, predW, color = "Predict", group = 1))+
  geom_line(data = modelPolyWQ, aes(Year, predW-se, color = "95% CI", group = 1), linetype = "dashed")+
  geom_line(data = modelPolyWQ, aes(Year, predW+se, color = "95% CI", group = 1), linetype = "dashed")+
  basetheme + theme(legend.position = c(0.899, 0.91))+ 
  ylab("Average proportion affected")+ 
  scale_x_discrete(expand = c(0.02,0.1)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,0.4)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#
Poly_df %>% group_by(Year) %>% summarise(AvepH = mean(pH, na.rm = T)) %>%
  ggplot()+
  geom_point(aes(Year, AvepH), color = "darkblue")+
  geom_line(aes(Year, AvepH, group = 1))+
  scale_y_continuous("Mean annual pH", expand = c(0,0), limits = c(7, 9))+
  basetheme
#
####MLogisticR 
head(Poly_df)
levels(Poly_df$Year); levels(Poly_df$Month); levels(Poly_df$Season)
#
Poly_model <- glm(Prop ~ Year + Season + DO_mgl + Salinity + Temperature + Turbidity + pH, data = Poly_df, family = binomial, weights = Total)
(Polystep_2 <- drop1(Poly_model, test = "F"))
Poly_model2 <- update(Poly_model, .~. -Season -Salinity -Turbidity -Temperature -DO_mgl, data = Poly_df)
exp(cbind(OR = coef(Poly_model2), confint(Poly_model2))) #OR results and CIs
#
blorr::blr_model_fit_stats(Poly_model2) # Gives various fit statistics
blorr::blr_test_hosmer_lemeshow(Poly_model2) # Hosmer Lemeshow gof test
blorr::blr_roc_curve(blorr::blr_gains_table(Poly_model2)) # ROC curve
DescTools::Cstat(Poly_model2) # C-Statistic (concordance statistic)) # Gives various fit statistics (model  performance)
DescTools::VIF(Poly_model2) #3 or below is okay
sjPlot::tab_model(Poly_mod2)
#
#
#
##Cliona
#
#initial MLR
(Clio_df <- Trends_WQ[complete.cases(Trends_WQ),] %>% filter(Type == "Cliona") %>% dplyr::select(-Type, -Station)) 
Clio_df %>% ggplot(aes(x = Prop))+ geom_histogram(aes(y = ..count..)) #skewed/proportional needs transformation
#
set.seed(4321)
fullClio <- glm(Prop ~ Year + Season + DO_mgl + Salinity + Temperature + Turbidity + pH, data = Clio_df, family = quasibinomial)
summary(fullClio)
fullClio_tab <- tidy(fullClio)
names(fullClio_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullClio_tab
#
summary(fullClio)
(Cliostep <- drop1(fullClio, test = "F"))
#
#Updated model
fullClio2 <- update(fullClio, .~. -Season -DO_mgl -Turbidity - Temperature, data = Clio_df)
#
###Reporting final model
fullClio2
fullClio2_sum <- summary(fullClio2)
#MLR table with test values
fullClio2_tab <- tidy(fullClio2)
names(fullClio2_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullClio2_sum_tab <- glance(fullClio2) 
names(fullClio2_sum_tab) <- c("Null_Dev", "df_null", "LL", "AIC", "BIC", "Deviance", "df_redis", "n")
fullClio2_tab
#McFadden's R2:1-deviance/null.deviance)
summary(fullClio2)
1-(59.138/83.060) #0.2880087
#
predicted_Clio <- data.frame(predW = predict(fullClio2, Clio_df, type = "response", se.fit = TRUE), Year = Clio_df$Year)
(modelClioWQ <- predicted_Clio %>% dplyr::select(Year, predW.fit, everything()) %>% dplyr::select(-predW.residual.scale) %>%
    group_by(Year) %>% dplyr::summarise(predW = mean(predW.fit, na.rm = T), se = mean(predW.se.fit)))
#
Clio_df %>% group_by(Year) %>% summarise(AveProp = mean(Prop, na.rm = T)) %>%
  ggplot()+
  geom_point(aes(Year, AveProp, color = "Mean"))+
  geom_line(data = modelClioWQ, aes(Year, predW, color = "Predict", group = 1))+
  geom_line(data = modelClioWQ, aes(Year, predW-se, color = "95% CI", group = 1), linetype = "dashed")+
  geom_line(data = modelClioWQ, aes(Year, predW+se, color = "95% CI", group = 1), linetype = "dashed")+
  basetheme + theme(legend.position = c(0.899, 0.91))+ 
  ylab("Average proportion affected")+ 
  scale_x_discrete(expand = c(0,0.1)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.025,0.4)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#
ggarrange(Clio_df %>% group_by(Year) %>% summarise(AveSal = mean(Salinity, na.rm = T)) %>%
            ggplot()+
            geom_point(aes(Year, AveSal), color = "darkblue")+
            geom_line(aes(Year, AveSal, group = 1))+
            scale_y_continuous("Mean annual salinity", expand = c(0,0), limits = c(20, 35))+
            basetheme,
          Clio_df %>% group_by(Year) %>% summarise(AveTemp = mean(Temperature, na.rm = T)) %>%
            ggplot()+
            geom_point(aes(Year, AveTemp), color = "darkblue")+
            geom_line(aes(Year, AveTemp, group = 1))+
            scale_y_continuous("Mean annual temperaure", expand = c(0,0), limits = c(20, 30))+
            basetheme,
          Clio_df %>% group_by(Year) %>% summarise(AvepH = mean(pH, na.rm = T)) %>%
            ggplot()+
            geom_point(aes(Year, AvepH), color = "darkblue")+
            geom_line(aes(Year, AvepH, group = 1))+
            scale_y_continuous("Mean annual pH", expand = c(0,0), limits = c(7, 9))+
            basetheme,
          nrow = 2, ncol = 2)
#
#
#
#
##Both
#initial MLR
(Both_df <- Trends_WQ[complete.cases(Trends_WQ),] %>% filter(Type == "Both") %>% dplyr::select(-Type, -Station)) 
Both_df %>% ggplot(aes(x = Prop))+ geom_histogram(aes(y = ..count..)) #skewed/proportional needs transformation
#
set.seed(4321)
fullBoth <- glm(Prop ~ Year + DO_mgl + Salinity + Temperature + Turbidity + pH, data = Both_df, family = quasibinomial)
summary(fullBoth)
fullBoth_tab <- tidy(fullBoth)
names(fullBoth_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullBoth_tab
#
summary(fullBoth)
(Bothstep <- drop1(fullBoth, test = "F"))
#
#Updated model
fullBoth2 <- update(fullBoth, .~. -DO_mgl -Temperature -pH, data = Both_df)
#
###Reporting final model
fullBoth2
fullBoth2_sum <- summary(fullBoth2)
#MLR table with test values
fullBoth2_tab <- tidy(fullBoth2)
names(fullBoth2_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullBoth2_sum_tab <- glance(fullBoth2) 
names(fullBoth2_sum_tab) <- c("Null_Dev", "df_null", "LL", "AIC", "BIC", "Deviance", "df_redis", "n")
fullBoth2_tab
#
predicted_Both <- data.frame(predW = predict(fullBoth2, Both_df, type = "response", se.fit = TRUE), Year = Both_df$Year)
(modelBothWQ <- predicted_Both %>% dplyr::select(Year, predW.fit, everything()) %>% dplyr::select(-predW.residual.scale) %>%
    group_by(Year) %>% dplyr::summarise(predW = mean(predW.fit, na.rm = T), se = mean(predW.se.fit)))
#
ggarrange(Both_df %>% group_by(Year) %>% summarise(AveProp = mean(Prop, na.rm = T)) %>%
    ggplot()+
      geom_point(aes(Year, AveProp, color = "Mean"))+
      geom_line(data = modelBothWQ, aes(Year, predW, color = "Predict", group = 1))+
      geom_line(data = modelBothWQ, aes(Year, predW-se, color = "95% CI", group = 1), linetype = "dashed")+
      geom_line(data = modelBothWQ, aes(Year, predW+se, color = "95% CI", group = 1), linetype = "dashed")+
      basetheme + theme(legend.position = c(0.899, 0.91))+ 
      ylab("Average proportion affected")+ 
      scale_x_discrete(expand = c(0,0.1)) + 
      scale_y_continuous(expand = c(0,0), limits = c(0,0.8)) +
      geom_hline(yintercept = 0, linetype = "dotted")+
      scale_color_manual(name = "",
                         breaks = c("Mean", "Predict", "95% CI"),
                         values = c("#000000", "#FF0000", "#999999"),
                         labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                         guide = guide_legend(override.aes = list(
                           linetype = c("blank", "solid", "dashed"),
                           shape = c(19, NA, NA)))),
    Both_df %>% group_by(Year) %>% summarise(AveSal = mean(Salinity, na.rm = T)) %>%
      ggplot()+
      geom_point(aes(Year, AveSal), color = "darkblue")+
      geom_line(aes(Year, AveSal, group = 1))+
      scale_y_continuous("Mean annual salinity", expand = c(0,0), limits = c(20, 35))+
      basetheme,
    nrow = 1, ncol = 2)
#
#
#
#
#
#####Additional comparisons####
#
#Comparison of minimum and maximum WQ parameters among years.
aovp(minTemp ~ Year, data = ungroup(TB_WQ_df %>% group_by(Year, Month, Date) %>% summarise(minTemp = min(Temperature))), 
     perm = "", nperm = 10000) %>% summary()

