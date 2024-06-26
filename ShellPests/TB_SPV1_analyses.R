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
#
#
#
##Reference dfs for factor columns
Reef_Type <- data.frame("Station" = c(1, 2, 3, 4, 5),
                        "Station_Name" = c("Pinellas Point", "Skyway", "Fort Desoto", "Gulfport", "Weedon Island"),
                        "Type" = c("Inter", "Inter", "Sub", "Inter", "Sub"))
#
Seasons <- data.frame("Month" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
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
#Add Year, Month, Season columns, add reef and station information, change Secchi to penetration
TB_WQ <- TB_WQ_raw %>% mutate(Year = as.factor(format(Date, "%Y")),
                          Month = as.numeric(format(Date, "%m"))) %>% 
  left_join(Seasons) %>% 
  left_join(Reef_Type) %>%
  mutate_at(c("Site", "Station", "Month", "Month_Abb", "Season", "Station_Name", "Type"), as.factor) %>% #Change columns to factor type 
  dplyr::select(Year, Month, Season, everything(.)) #Reorder columns
head(TB_WQ)
#
glimpse(Portal_WQ_raw)
##Get mean daily values to work with.
Portal_WQ <- Portal_WQ_raw %>% dplyr::select(-Result_Unit, -Buffer, -LocationName, -StationName, -Latitude, -Longitude) %>% #Remove unnecessary columns
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
                          Month = as.numeric(format(Date, "%m")),
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
####Beginning questions####
#
##Does Polydora and Cliona differ in parasite prevalence in TB Oysters? - want # infected out of total oysters per sample (1 sample = 1 year/month/station)
(t1 <- TB_SP_df %>% subset(Measurement == "All") %>% 
   group_by(Year, Month, Station) %>% #Grouping factors
   summarise(nT = n(), #Total number of oysters
             Polydora = sum(Poly_Prev), #Number of oysters with Polydora
             Cliona = sum(Cliona_Prev)) %>%  #Number of oysters with Cliona
   pivot_longer(cols = c(Polydora, Cliona), names_to = "Type", values_to = "nI") %>% #Restructure data for analyses
   mutate(Prop = nI/nT)) #Calculate proporiton infected
#
#
Pest_model <- glm(cbind(nI, nT) ~ Type, family = binomial, data = t1)
summary(Pest_model) #Check model
confint(Pest_model)
Anova(Pest_model) #Significant difference between Polydora and Cliona p = 0.03 (< 0.05)
#X2 1 = 4.63 p = 0.03
(Pest_model_emm <- emmeans(Pest_model, ~Type, type = "response")) #Polydora averages higher than Cliona
pairs(Pest_model_emm, adjust = "tukey")  
#
#Get means and Letters distinguishing significantly different groups:
(Pest_means <- left_join(t1 %>% group_by(Type) %>% 
                           summarise(n = n(),
                                     meanProp = mean(Prop),
                                     se = sd(Prop, na.rm = T)/sqrt(n)),
                         data.frame(cld(object = Pest_model_emm, adjust = "sidak", Letters = letters, alpha = 0.05))) %>%
    rename(Letters = .group)) %>% mutate(Letters =  gsub("[[:space:]]", "", Letters)) %>%  dplyr::select(-df)
#Figure of means with letters
Pest_means %>%
  ggplot(aes(Type, meanProp, fill = Letters))+
  geom_errorbar(aes(ymin = meanProp, ymax = meanProp+se), width = 0.5)+
  geom_bar(stat = "identity")+
  geom_text(aes(Type, y = meanProp+se+0.03, label = Letters))+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + basetheme + 
  theme(legend.position = "none")
#
#
#
#
#
#
##Does Polydora or Cliona differ in parasite prevalence impact among shell surfaces in TB Oysters? - want # infected out of total oysters per sample (1 sample = 1 year/month/station)
#
(t2 <- TB_SP_df %>% subset(Measurement == "External" | Measurement == "Internal") %>% 
  group_by(Year, Month, Station, Measurement) %>% #Grouping factors
  summarise(nT = n(), #Total number of oysters
            Polydora = sum(Poly_Prev), #Number of oysters with Polydora
            Cliona = sum(Cliona_Prev)) %>%  #Number of oysters with Cliona
  pivot_longer(cols = c(Polydora, Cliona), names_to = "Type", values_to = "nI") %>% #Restructure data for analyses
  mutate(Prop = nI/nT)) #Calculate proporiton infected
#
Side_model <- glm(cbind(nI, nT) ~ Type * Measurement, family = binomial, data = t2)
summary(Side_model) #Check model
confint(Side_model)
Anova(Side_model) #Significant difference between Polydora and Cliona and between shell sides p = 0.03 (< 0.05)
#X2 1 = 10.13 p = 0.001
(Side_model_emm <- emmeans(Side_model, ~Measurement*Type, type = "response")) #Polydora averages higher than Cliona
pairs(Side_model_emm, adjust = "tukey") #Both more often on external (both p <= 0.005)  
#
#Get means and Letters distinguishing significantly different groups:
(Side_means <- left_join(t2 %>% group_by(Type, Measurement) %>% 
                           summarise(n = n(),
                                     meanProp = mean(Prop),
                                     se = sd(Prop, na.rm = T)/sqrt(n)),
                         data.frame(cld(object = Side_model_emm, adjust = "sidak", Letters = letters, alpha = 0.05))) %>%
    rename(Letters = .group)) %>% mutate(Letters =  gsub("[[:space:]]", "", Letters)) %>%  dplyr::select(-df)
#Figure of means with letters
Side_means %>%
  ggplot(aes(Type, meanProp, fill = Measurement))+
  geom_errorbar(aes(ymin = meanProp, ymax = meanProp+se), width = 0.5, stat = "identity", position = position_dodge(0.75))+
  geom_col(position = "dodge", width = 0.75)+
  geom_text(aes(Type, y = meanProp+se+0.03, label = Letters), position = position_dodge(0.75))+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + basetheme  
  theme(legend.position = "none")
#
#
#
#
#
#
##Does Polydora or Cliona differ in parasite prevalence impact among shell position in TB Oysters? - want # infected out of total oysters per sample (1 sample = 1 year/month/station)
#
(t3 <- TB_SP_df %>% subset(Measurement == "Top" | Measurement == "Bot") %>% 
    group_by(Year, Month, Station, Measurement) %>% #Grouping factors
    summarise(nT = n(), #Total number of oysters
              Polydora = sum(Poly_Prev), #Number of oysters with Polydora
              Cliona = sum(Cliona_Prev)) %>%  #Number of oysters with Cliona
    pivot_longer(cols = c(Polydora, Cliona), names_to = "Type", values_to = "nI") %>% #Restructure data for analyses
    mutate(Prop = nI/nT)) #Calculate proporiton infected
#
Position_model <- glm(cbind(nI, nT) ~ Type * Measurement, family = binomial, data = t3)
summary(Position_model) #Check model
confint(Position_model)
Anova(Position_model) #Significant difference between Polydora and Cliona and between shell positions p < 0.001
#X2 1 = 63.48 p < 0.001
(Position_model_emm <- emmeans(Position_model, ~Measurement*Type, type = "response")) 
pairs(Position_model_emm, adjust = "tukey") #Cliona less on top than bottom and less on top than Polydora on either shell (all p < 0.001) 
#
#Get means and Letters distinguishing significantly different groups:
(Position_means <- left_join(t3 %>% group_by(Type, Measurement) %>% 
                           summarise(n = n(),
                                     meanProp = mean(Prop),
                                     se = sd(Prop, na.rm = T)/sqrt(n)),
                         data.frame(cld(object = Position_model_emm, adjust = "sidak", Letters = letters, alpha = 0.05))) %>%
    rename(Letters = .group)) %>% mutate(Letters =  gsub("[[:space:]]", "", Letters)) %>%  dplyr::select(-df)
#Figure of means with letters
Position_means %>%
  ggplot(aes(Type, meanProp, fill = Measurement))+
  geom_errorbar(aes(ymin = meanProp, ymax = meanProp+se), width = 0.5, stat = "identity", position = position_dodge(0.75))+
  geom_col(position = "dodge", width = 0.75)+
  geom_text(aes(Type, y = meanProp+se+0.03, label = Letters), position = position_dodge(0.75))+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + basetheme  
#
#
#
#
#
#
##Does Polydora and Cliona differ in parasite prevalence in TB Oysters among stations? - want # infected out of total oysters per sample (1 sample = 1 year/month/station)
#Interested in comparing within pest species differences (rather than Pest*Station)
Pest_model_2 <- glm(cbind(nI, nT) ~ Type * Station, family = binomial, data = t1)
summary(Pest_model_2) #Check model
confint(Pest_model_2)
Anova(Pest_model_2) #Significant difference between Polydora and Cliona among Stations
#X2, 4 = 26.22, <0.001
(Pest_model_2_emm <- emmeans(Pest_model_2, ~Station|Type, type = "response")) #Get differences among stations within pest type
pairs(Pest_model_2_emm, adjust = "sidak")
#
#Get means and Letters distinguishing significantly different groups:
(Pest_station_means <- left_join(t1 %>% group_by(Type, Station) %>% 
                           summarise(n = n(),
                                     meanProp = mean(Prop),
                                     se = sd(Prop, na.rm = T)/sqrt(n)),
                         data.frame(cld(object = Pest_model_2_emm, adjust = "sidak", Letters = letters, alpha = 0.05))) %>%
    rename(Letters = .group) %>% mutate(Letters =  gsub("[[:space:]]", "", Letters), Height = (meanProp + se + 0.02))) %>%
  dplyr::select(-df)
#Figure of differences among stations
Pest_station_means %>%
  ggplot(aes(Type, meanProp, fill = Station))+
  geom_col(position = "dodge", width = 0.75, color = "black")+
  geom_errorbar(aes(ymin = meanProp, ymax = meanProp+se), width = 0.5, stat = "identity", position = position_dodge(0.75))+
  geom_text(aes(Type, y = Height, label = Letters), position = position_dodge(0.75))+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1.02)) + basetheme
#
#
#
#
#Since Cliona and Polydora differ among stations, Station will be included in all models.
#
##What is the relationship between Polydora and Cliona percent affected? (Correlation)
##What is the relationship between Polydora or Cliona with CI? (Correlation)
##Has the amount of Polydora or Cliona at each station changed over time? (glm)
##Which WQ parameters best explain the trend observed in Polydora or Cliona? (glm)