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
               ggpubr,
               rstatix, corrplot, vegan, lmPerm, #Summary stats, correlations
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
#
###Condition index
TB_CI_raw <- read_excel("Data/TB_CI_2018_2021.xlsx", sheet = "Sheet1", #File name and sheet name
                    skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                    na = c("", "Z", "z"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "unique")
head(TB_CI_raw)
#Simplify column names
colnames(TB_CI_raw) <- c("Date", "Site", "Sample_Number", "Station", "SH", "SL", "SW", "TW", "TarePan", "TissueWW", "ShellWW", "TissueDW", "ShellDW", "FinalTisse", "CI", "CI_Hanley")
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
  left_join(Seasons) %>% 
  left_join(Reef_Type) %>%
  mutate_at(c("Site", "Station", "Month", "Month_Abb", "Season", "Station_Name", "Type"), as.factor) %>% #Change columns to factor type 
  dplyr::select(Year, Month, Season, everything(.)) #Reorder columns
head(TB_WQ)
#
glimpse(Portal_WQ_raw)
##Get mean daily values to work with.
Portal_WQ <- Portal_WQ_raw %>% mutate(Month = as.factor(Month)) %>%
  dplyr::select(-Result_Unit, -Buffer, -LocationName, -StationName, -Latitude, -Longitude) %>% #Remove unnecessary columns
  group_by(Year, Month, Season, Estuary, Station, SampleDate, Parameter) %>% #Group by all columns needed in output
  summarise(Mean_Result = mean(Result_Value, na.rm = T)) %>% #Calculate mean daily value 
  mutate_at(c("Year", "Month", "Season", "Estuary", "Station"), as.factor)
#
head(Portal_WQ)
#
#Combine portal data with our data
TB_WQ_df <- rbind(TB_WQ %>% dplyr::select(-Time, -Depth, -Station_Name, -Type, -Month_Abb, -Secchi, -TProbe, -THach, -Quali) %>% #Remove unneeded columns from our data
                    pivot_longer(cols = -c(Year, Month, Season, Date, Site, Station), names_to = "Parameter", values_to = "Mean_Result"), #Reorganize into 2 columns to join by
                  Portal_WQ %>% rename(Site = Estuary, Date = SampleDate)) %>% #Rename Portal columns to join data
  group_by(Year, Month, Season, Date, Site, Station, Parameter) %>% summarise(Mean_Result = mean(Mean_Result, na.rm = T)) %>% #Determine mean daily value for all parameters and data
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
  left_join(Seasons) %>% 
  left_join(Reef_Type) %>%
  mutate_at(c("Year", "Month", "Site", "Station", "Month_Abb", "Season", "Station_Name", "Type", "SizeClass"), as.factor)
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
  left_join(Seasons) %>% 
  left_join(Reef_Type) %>%
  mutate_at(c("Year", "Month", "Site", "Station", "Shell_Side", "Shell_Pos", "Month_Abb", "Season", "Station_Name", "Type"), as.factor)
head(TB_SP)
#
#Summarize data by Shell_Side, Shell_Pos, and overall
TB_SP_df <- 
  rbind(TB_SP %>%  
          group_by(Date, Year, Month, Site, Station, Sample.Number, Shell_Side) %>%
          summarise(Pct.Polydora = mean(Pct.Polydora, na.rm = T),
                    Pct.Cliona = mean(Pct.Cliona, na.rm = T),
                    Pct_Affected = mean(Pct_Affected, na.rm = T),
                    Poly_Prev = case_when(Pct.Polydora > 0 ~ 1, TRUE ~ 0),
                    Cliona_Prev = case_when(Pct.Cliona > 0 ~ 1, TRUE ~ 0),
                    Richness = Poly_Prev + Cliona_Prev) %>%
          rename(Measurement = Shell_Side),
        #
        TB_SP %>%  mutate(Shell_Pos = recode_factor(Shell_Pos, 'Top' = "Top", 'Bottom' = "Bot")) %>%
          group_by(Date, Year, Month, Site, Station, Sample.Number, Shell_Pos) %>%
          summarise(Pct.Polydora = mean(Pct.Polydora, na.rm = T),
                    Pct.Cliona = mean(Pct.Cliona, na.rm = T),
                    Pct_Affected = mean(Pct_Affected, na.rm = T),
                    Poly_Prev = case_when(Pct.Polydora > 0 ~ 1, TRUE ~ 0),
                    Cliona_Prev = case_when(Pct.Cliona > 0 ~ 1, TRUE ~ 0),
                    Richness = Poly_Prev + Cliona_Prev) %>%
          rename(Measurement = Shell_Pos)) %>%
  #
  rbind(TB_SP %>%  
          group_by(Date, Year, Month, Site, Station, Sample.Number) %>%
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
#
#
#
#Each station all years and all TB all years - Pests - Pcts and Richness
TB_Pest_summ <- rbind(TB_SP_df %>% ungroup() %>% dplyr::select(-Date, -Year, -Month, -Site, -Sample.Number) %>%
                        pivot_longer(cols = -c(Station, Measurement), names_to = "Parameter", values_to = "Value") %>% 
                        group_by(Station, Measurement, Parameter) %>%
                        summarise(n = n(),
                                  mean = mean(Value, na.rm = T),
                                  se = sd(Value, na.rm = T)/sqrt(n), 
                                  min = min(Value, na.rm = T),
                                  max = max(Value, na.rm = T)),
                      TB_SP_df %>% ungroup() %>% dplyr::select(-Date, -Year, -Month, -Station, -Sample.Number) %>%
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
#
#
####Bay Summary Questions (Q1-Q4)####
#
###Q1: WQ trends - overall min/max/mean; annual min/max/mean figures :: Summary data found in "TB_WQ_summ" df/sheet
#Overall
TB_WQ_df %>% ungroup() %>% mutate(MonYr = as.yearmon(Date)) %>% 
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
###Any sig differences?
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
gghistogram(TB_WQ_df, x = "Temperature")
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
Month_temp_p %>% filter(p.adjust < 0.05) %>% print(n = 57)
#Temperatures are similar in 02/03/11; 06/08; 05/10

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
Month_sal_p %>% filter(p.adjust < 0.05) %>% print(n = 57)

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
Month_pH_p %>% filter(p.adjust < 0.05) %>% print(n = 57)

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
Month_DO_p %>% filter(p.adjust < 0.05) %>% print(n = 57)
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
#Temperatures are similar

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
#Salinity 5 < 1 <= 2/3/4

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
Station_pH_p %>% filter(p.adjust < 0.05) %>% print(n = 57)

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
Station_DO_p %>% filter(p.adjust < 0.05) %>% print(n = 57)
#
#
#
#
###Q4: Oysters among stations - SH, TW, CI
(Oyster_Stations <- TB_CI %>% ungroup() %>% 
    dplyr::select(Station, SH, TW, CI) %>% 
    group_by(Station) %>% 
    summarise(SHeight = mean(SH, na.rm = T),
              TWeight = mean(TW, na.rm = T),
              CIndex = mean(CI, na.rm = T))) 
#
ggarrange(ggboxplot(TB_CI, x = "Station", y = "SH"),
          ggboxplot(TB_CI, x = "Station", y = "TW"),
          ggboxplot(TB_CI, x = "Station", y = "CI"))
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
Station_SH_p %>% filter(p.adjust < 0.05) %>% print(n = 57)
#
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
Station_TW_p %>% filter(p.adjust < 0.05) %>% print(n = 57)
#
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
Station_CI_p %>% filter(p.adjust < 0.05) %>% print(n = 57)
#
#
#
#
#
####Pest Summary Questions (Q5-Q8)####
#
###Q5:Does Polydora and Cliona differ in parasite prevalence in TB Oysters? 
#want # infected out of total oysters per sample (1 sample = 1 year/month/station) - need to consider oysters with both separately from each pest
(t1 <- left_join(TB_SP_df %>% subset(Measurement == "All") %>% 
                   mutate(Type = as.factor(case_when(Poly_Prev == 1 & Cliona_Prev == 0 ~ "Polydora",
                                                     Poly_Prev == 0 & Cliona_Prev == 1 ~ "Cliona",
                                                     Poly_Prev == 1 & Cliona_Prev == 1 ~ "Both",
                                                     TRUE ~ "None"))) %>% 
                   filter(Type != "None") %>%
                   group_by(Year, Month, Station, Type) %>% 
                   summarise(Count = n()),
                 TB_SP_df %>% subset(Measurement == "All") %>% 
                   group_by(Year, Month, Station) %>% 
                   summarise(Total = n())) %>%
   mutate(Prop = Count/Total, Type = factor(Type, levels = c("Polydora", "Cliona", "Both"))))#
#
set.seed(54321)
Pest_model <- glm(Prop ~ Type, family = quasibinomial, data = t1) #quasi - [0,1]
summary(Pest_model) #Check model
Anova(Pest_model, test = "F") #Significant difference among pest "Types"
#
#Get means and Letters distinguishing significantly different groups:
(Pest_model_emm <- emmeans(Pest_model, ~Type, type = "response")) #Polydora = Cliona < Both = None
(Pest_p_means <- merge(t1 %>% group_by(Type) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% 
                         dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                       multcomp::cld(Pest_model_emm, alpha = 0.05, decreasing = TRUE, Letters = c(letters)) %>%
                         rename(Letters = .group, Lower = asymp.LCL, Upper = asymp.UCL) %>% dplyr::select(Type, Lower:Letters)))
#
(Pest_p <- pairs(Pest_model_emm, type = "response", adjust = "holm") %>% as.data.frame() %>% dplyr::select(-c(df, null)))
#
#Figure of means with letters
Pest_means %>%
  ggplot(aes(Type, meanProp, fill = Letters))+
  geom_errorbar(aes(ymin = meanProp, ymax = meanProp+se), width = 0.5)+
  geom_bar(stat = "identity")+
  geom_text(aes(Type, y = meanProp+se+0.03, label = Letters))+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_x_discrete("")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1)) + basetheme + 
  theme(legend.position = "none")
#
Pest_p %>% filter(p.adjust < 0.05)
#
#
#
#
#
###Q6: Does Polydora or Cliona differ in parasite prevalence impact among shell surfaces in TB Oysters? - want # infected out of total oysters per sample (1 sample = 1 year/month/station)
#
(t2 <- left_join(TB_SP_df %>% subset(Measurement == "External" | Measurement == "Internal") %>%
    mutate(Type = as.factor(case_when(Poly_Prev == 1 & Cliona_Prev == 0 ~ "Polydora",
                                      Poly_Prev == 0 & Cliona_Prev == 1 ~ "Cliona",
                                      Poly_Prev == 1 & Cliona_Prev == 1 ~ "Both",
                                      TRUE ~ "None"))) %>%
      filter(Type != "None") %>%
      group_by(Year, Month, Station, Measurement, Type) %>% #Grouping factors
      summarise(Count = n()), #Total number of oysters per Type
    TB_SP_df %>% subset(Measurement == "External" | Measurement == "Internal") %>% 
      group_by(Year, Month, Station, Measurement) %>%
      summarise(Total = n())) %>%
    mutate(Prop = Count/Total, Type = factor(Type, levels = c("Polydora", "Cliona", "Both"))))
#
set.seed(54321)
Side_model <- glm(Prop ~ Type * Measurement, family = quasibinomial, data = t2)
summary(Side_model) #Check model
Anova(Side_model, test = "F") #Significant difference between Polydora and Cliona, between shell sides p < 0.01
#
#Get means and Letters distinguishing significantly different groups:
(Side_model_emm <- emmeans(Side_model, ~Measurement*Type, type = "response")) 
(Side_p_means <- merge(t2 %>% group_by(Type, Measurement) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% 
                         dplyr::select(-c("variable")),
                       multcomp::cld(Side_model_emm, alpha = 0.05, decreasing = TRUE, Letters = c(letters)) %>%
                         rename(Letters = .group, Lower = asymp.LCL, Upper = asymp.UCL) %>% dplyr::select(Measurement, Type, SE, Lower:Letters) %>%
                         mutate(Measurement = as.factor(Measurement), Type = as.factor(Type))))

(Side_p <- pairs(Side_model_emm, type = "response", adjust = "holm") %>% as.data.frame() %>% dplyr::select(-c(df, null))) #Both more often on external (both p <= 0.005)
Side_p %>% filter(p.value < 0.05)
#
#
#Figure of means with letters
Side_p_means %>%
  ggplot(aes(Type, mean, fill = Measurement))+
  geom_errorbar(aes(ymin = mean, ymax = Upper), width = 0.5, stat = "identity", position = position_dodge(0.75))+
  geom_col(position = "dodge", width = 0.75)+
  geom_text(aes(Type, y = Upper+0.03, label = Letters), position = position_dodge(0.75))+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_x_discrete("")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1)) + basetheme  
  theme(legend.position = "none")
#
#
#
#
#
#
###Q7: Does Polydora or Cliona differ in parasite prevalence impact among shell position in TB Oysters? - want # infected out of total oysters per sample (1 sample = 1 year/month/station)
#
(t3 <- left_join(TB_SP_df %>% subset(Measurement == "Top" | Measurement == "Bot") %>%
                     mutate(Type = as.factor(case_when(Poly_Prev == 1 & Cliona_Prev == 0 ~ "Polydora",
                                                       Poly_Prev == 0 & Cliona_Prev == 1 ~ "Cliona",
                                                       Poly_Prev == 1 & Cliona_Prev == 1 ~ "Both",
                                                       TRUE ~ "None"))) %>%
                     filter(Type != "None") %>%
                     group_by(Year, Month, Station, Measurement, Type) %>% #Grouping factors
                     summarise(Count = n()), #Total number of oysters per Type
                   TB_SP_df %>% subset(Measurement == "Top" | Measurement == "Bot") %>% 
                     group_by(Year, Month, Station, Measurement) %>%
                     summarise(Total = n())) %>%
      mutate(Prop = Count/Total, Type = factor(Type, levels = c("Polydora", "Cliona", "Both"))))
#
Position_model <- glm(Prop ~ Type * Measurement, family = quasibinomial, data = t3)
summary(Position_model) #Check model
Anova(Position_model, test = "F") #Significant difference between Polydora and Cliona and between shell positions p < 0.001
#
#Get means and Letters distinguishing significantly different groups:
(Position_model_emm <- emmeans(Position_model, ~Measurement*Type, type = "response")) 
(Position_p_means <- merge(t3 %>% group_by(Type, Measurement) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% 
                         dplyr::select(-c("variable")),
                       multcomp::cld(Position_model_emm, alpha = 0.05, decreasing = TRUE, Letters = c(letters)) %>%
                         rename(Letters = .group, Lower = asymp.LCL, Upper = asymp.UCL) %>% dplyr::select(Measurement, Type, SE, Lower:Letters) %>%
                         mutate(Measurement = as.factor(Measurement), Type = as.factor(Type))))

(Position_p <- pairs(Position_model_emm, type = "response", adjust = "holm") %>% as.data.frame() %>% dplyr::select(-c(df, null))) #Both more often on external (both p <= 0.005)
Position_p %>% filter(p.value < 0.05)
#
#Figure of means with letters
Position_p_means %>%
  ggplot(aes(Type, mean, fill = Measurement))+
  geom_errorbar(aes(ymin = mean, ymax = Upper), width = 0.5, stat = "identity", position = position_dodge(0.75))+
  geom_col(position = "dodge", width = 0.75)+
  geom_text(aes(Type, y = Upper+0.03, label = Letters), position = position_dodge(0.75))+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_x_discrete("")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1)) + basetheme  
#
#
#
#
#
#
###Q8: Does Polydora and Cliona differ in parasite prevalence in TB Oysters among stations?
#Interested in comparing within pest species differences (rather than Pest*Station)
set.seed(54321)
Pest_model_2 <- glm(Prop ~ Type * Station, family = quasibinomial, data = t1)
summary(Pest_model_2) #Check model
Anova(Pest_model_2, test = "F") 
#
#Get means and Letters distinguishing significantly different groups:
(Pest_model_2_emm <- emmeans(Pest_model_2, ~Station*Type, type = "response")) 
(Pest2_p_means <- merge(t1 %>% group_by(Type, Station) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% 
                             dplyr::select(-c("variable")),
                           multcomp::cld(Pest_model_2_emm, alpha = 0.05, decreasing = TRUE, Letters = c(letters)) %>%
                             rename(Letters = .group, Lower = asymp.LCL, Upper = asymp.UCL) %>% dplyr::select(Station, Type, SE, Lower:Letters) %>%
                             mutate(Station = as.factor(Station), Type = as.factor(Type))))

(Pest2_p <- pairs(Pest_model_2_emm, type = "response", adjust = "holm") %>% as.data.frame() %>% dplyr::select(-c(df, null))) #
Pest2_p_means %>% arrange(Type) %>% dplyr::select(Type, everything())
Pest2_p %>% filter(p.value < 0.05)
#
#Figure of differences among stations
Pest2_p_means %>%
  ggplot(aes(Type, mean, fill = Station))+
  geom_col(position = "dodge", width = 0.75, color = "black")+
  geom_errorbar(aes(ymin = mean, ymax = Upper), width = 0.5, stat = "identity", position = position_dodge(0.75))+
  geom_text(aes(Type, y = Upper+0.03, label = Letters), position = position_dodge(0.75))+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_x_discrete("")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.02)) + basetheme
#
#
#Curiosity. What about just Cliona & Polydora?
set.seed(54321)
Pest_model_2b <- glm(Prop ~ Type * Station, family = quasibinomial, data = (t1 %>% filter(Type != "Both")))
summary(Pest_model_2b) #Check model
Anova(Pest_model_2b, test = "F") 
#Get means and Letters distinguishing significantly different groups:
(Pest_model_2b_emm <- emmeans(Pest_model_2b, ~Station*Type, type = "response")) 
(Pest2b_p_means <- merge(t1 %>% filter(Type != "Both") %>% group_by(Type, Station) %>% rstatix::get_summary_stats(Prop, show = c("n", "mean", "sd", "min", "max")) %>% 
                          dplyr::select(-c("variable")),
                        multcomp::cld(Pest_model_2b_emm, alpha = 0.05, decreasing = TRUE, Letters = c(letters)) %>%
                          rename(Letters = .group, Lower = asymp.LCL, Upper = asymp.UCL) %>% dplyr::select(Station, Type, SE, Lower:Letters) %>%
                          mutate(Station = as.factor(Station), Type = as.factor(Type))))

(Pest2b_p <- pairs(Pest_model_2b_emm, type = "response", adjust = "holm") %>% as.data.frame() %>% dplyr::select(-c(df, null))) #
Pest2b_p_means %>% arrange(Type) %>% dplyr::select(Type, everything())
Pest2b_p %>% filter(p.value < 0.05)
#
#Figure of differences among stations
Pest2b_p_means %>%
  ggplot(aes(Type, mean, fill = Station))+
  geom_col(position = "dodge", width = 0.75, color = "black")+
  geom_errorbar(aes(ymin = mean, ymax = Upper), width = 0.5, stat = "identity", position = position_dodge(0.75))+
  geom_text(aes(Type, y = Upper+0.03, label = Letters), position = position_dodge(0.75))+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_x_discrete("")+
  scale_y_continuous("Average proportion of oysters", expand = c(0,0), limits = c(0,1.02)) + basetheme
#
#
#
####Trends (Q9-Q12)####
#
##What is the relationship between Polydora and Cliona percent affected? (Correlation)
#
head(t1)
cor()
#
#
#
#
#
##What is the relationship between Polydora or Cliona with CI? (Correlation)
##Has the amount of Polydora or Cliona at each station changed over time? (glm)
##Which WQ parameters best explain the trend observed in Polydora or Cliona? (glm)