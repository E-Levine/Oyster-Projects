###Figures for Oyster presentation 2026 CECM
#
#
#Packages
if (!require("pacman")) {install.packages("pacman")}
#p_unlock()
pacman::p_load(odbc, DBI, dbplyr, 
               tidyverse, dplyr,  stringr, #DF manipulation
               DT, openxlsx,         #Excel
               lubridate, zoo,         #Dates
               knitr, scales, gt, gtExtras, ggpubr, ggpattern, magick, 
               flextable, lmPerm, broom, rstatix, biostat, rcompanion,
               install = TRUE)
#
Author <- c("EL Williams") #Change to your name
Database <- "Oysters_26-01-28"  #Set the local database to use
Server = "localhost\\ERICALOCALSQL" #Set the local Server to use
#
##Sites of interest
Estuaries <- c("SL", "LX", "CR")
##Data types of interest: Recruitment, Collection/Dissection. Sediment Trap, Semi-Annual Survey, Cage Growth and Mortality, Survey, Shell Budget, Wave
DataTypes <- c("Survey")
DataTypeCodes <- c("SRVY")
#Dates of interest
Start_date <- as.Date("2010-01-01")
End_date <- as.Date("2025-12-31")
#
#
#
###Database download####
# Connect to Local database server and pull all necessary data, then close connection 
con <- dbConnect(odbc(),
                 Driver = "SQL Server", 
                 Server = Server,
                 Database = Database,
                 Authentication = "ActiveDirectoryIntegrated")
#
FixedLocations_raw <- tbl(con,in_schema("dbo", "FixedLocations")) %>%  collect()
FixedLocations_raw <- FixedLocations_raw %>% dplyr::select(FixedLocationID:StationNumber, EstuaryLongName) %>% mutate(StationName = gsub("-", "", StationName))
#
hsdbTripInfo <- tbl(con,in_schema("hsdb", "TripInfo")) %>%  collect() %>% filter(TripDate > Start_date & TripDate < End_date)
dboTripInfo <- tbl(con,in_schema("dbo", "TripInfo")) %>%  collect() %>% filter(TripDate > Start_date & TripDate < End_date)
#
hsdbSampleEvent <- tbl(con,in_schema("hsdb", "SampleEvent")) %>%   collect() 
dboSampleEvent <- tbl(con,in_schema("dbo", "SampleEvent")) %>%   collect() 
#
hsdbSampleEventWQ <- tbl(con,in_schema("hsdb", "SampleEventWQ")) %>%   collect()
dboSampleEventWQ <- tbl(con,in_schema("dbo", "SampleEventWQ")) %>%   collect()
#
hsdbSRVY <- tbl(con,in_schema("hsdb", "SurveyQuadrat")) %>%  collect() 
dboSRVY <- tbl(con,in_schema("dbo", "SurveyQuadrat")) %>%  collect() 
#
DBI::dbDisconnect(con)
#
#
#
#
####Data filtering/cleaning####
#
FixedLocations <- FixedLocations_raw %>% filter(Estuary %in% Estuaries)
#
(TripInfo <- rbind(hsdbTripInfo %>% 
                     filter(substring(TripID,1,2) %in% Estuaries & TripType %in% DataTypes) %>%  
                     arrange(TripID),
                   dboTripInfo %>% 
                     filter(substring(TripID,1,2) %in% Estuaries & TripType %in% DataTypes) %>%  
                     arrange(TripID)) %>%
    dplyr::select(TripID:DataStatus, Comments))
rm(hsdbTripInfo, dboTripInfo)        
#
#
(SampleEvent <- rbind(hsdbSampleEvent %>% 
                        mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), 
                               FixedLocationID = substring(SampleEventID, 19, 22), 
                               DataType = substring(SampleEventID,3,6)) %>%
                        filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate > Start_date & TripDate < End_date & DataType %in% DataTypeCodes),
                      dboSampleEvent %>% 
                        mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), 
                               FixedLocationID = substring(SampleEventID, 19, 22), 
                               DataType = substring(SampleEventID,3,6)) %>%
                        filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate > Start_date & TripDate < End_date & DataType %in% DataTypeCodes)) %>%
    left_join(FixedLocations) %>% 
    dplyr::select(TripDate, DataType, Estuary, SectionName, StationName, StationNumber, SampleEventID:FixedLocationID, DataStatus, Comments, EstuaryLongName) %>% arrange(TripDate))
rm(hsdbSampleEvent, dboSampleEvent)
#
#
(SampleEventWQ <- rbind(hsdbSampleEventWQ %>% 
                          mutate(TripDate = as.Date(substring(SampleEventWQID, 8, 15), format = "%Y%m%d"), 
                                 FixedLocationID = substring(SampleEventWQID, 19, 22)) %>%
                          filter(substring(SampleEventWQID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date),
                        dboSampleEventWQ %>% 
                          mutate(TripDate = as.Date(substring(SampleEventWQID, 8, 15), format = "%Y%m%d"), 
                                 FixedLocationID = substring(SampleEventWQID, 19, 22)) %>%
                          filter(substring(SampleEventWQID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date)) %>%
    left_join(FixedLocations, by = c("FixedLocationID")) %>%
    dplyr::select(TripDate, Estuary, SectionName, StationName, StationNumber, SampleEventWQID:TurbidityHach, PercentDissolvedOxygen, CollectionTime, FixedLocationID, EstuaryLongName) %>% arrange(TripDate))
rm(hsdbSampleEventWQ, dboSampleEventWQ)
#
#
(Survey <- rbind(hsdbSRVY %>% 
        mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), 
               FixedLocationID = substring(QuadratID, 19, 22)) %>%
        filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date),
        dboSRVY %>% 
          mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), 
                 FixedLocationID = substring(QuadratID, 19, 22)) %>%
          filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date))%>%
  left_join(FixedLocations) %>% 
  mutate(Year = format(TripDate, "%Y"), 
         Month = format(TripDate, "%m"),
         TotalLive = NumLive*4) %>%
  dplyr::select(TripDate, Estuary, SectionName, StationName, StationNumber, QuadratNumber, TotalLive, Comments, Year, Month, FixedLocationID, EstuaryLongName))
rm(hsdbSRVY, dboSRVY, con)
#
#
#
####Formatting####
#
#
Site_names <- c("SLC" = "St. Lucie-Central", 
                "LXN" = "Loxahatchee-North", 
                "LXS" = "Loxahatchee-South", 
                "CRE" = "Caloosahatchee-East", 
                "CRW" = "Caloosahatchee-West")
#
Month_abbs <- c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr", "05" = "May", "06" = "Jun",
                "07" = "Jul", "08" = "Aug", "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")
#
Base <- theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 15, color = "black", family = "sans"),
        axis.text.x = element_text(size = 14, color = "black", 
                                   margin = unit(c(0.4, 0.5, 0, 0.5), "cm"), family = "sans"),
        axis.text.y = element_text(size = 14, color = "black", 
                                   margin = unit(c(0, 0.4, 0, 0), "cm"), family = "sans"),
        axis.ticks.length = unit(-0.15, "cm"), plot.margin = margin(0.25, 0.5, 0.25, 0.25, "cm"))
#
Prez <- theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(size = 24, color = "black", family = "sans"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 23, color = "black", 
                                   margin = unit(c(0.4, 0.5, 0, 0.5), "cm"), family = "sans"),
        axis.text.y = element_text(size = 23, color = "black", 
                                   margin = unit(c(0, 0.4, 0, 0), "cm"), family = "sans"),
        axis.ticks.length = unit(-0.15, "cm"), plot.margin = margin(0.25, 0.5, 0.25, 0.25, "cm"))
#
theme_f <- theme(strip.text.y = element_text(color = "black", size = 11, family = "sans", face = "bold"),
                 strip.background = element_rect(fill = "#999999"),
                 panel.spacing = unit(-2, "lines"),
                 strip.text.x = element_text(size = 13, face = "bold", family = "sans"),
                     panel.border = element_rect(color = "black", fill = NA))
#
adaptive_breaks <- function(limits) {
  #  Get the default neat breaks
  default_breaks <- scales::breaks_pretty(n = 12)(limits)
  n_breaks <- length(default_breaks)
  # Apply conditional filtering rules
  if (n_breaks >= 12) {
    # Keep every 2nd break to cut 12 down
    return(default_breaks[seq(1, n_breaks, by = 4)]) 
  } else if (n_breaks >= 9) {
    # Keep every 2nd break to cut 9 down
    return(default_breaks[seq(1, n_breaks, by = 3)]) 
  #} else if (n_breaks >= 6) {
    # Keep every 2nd break to cut 6 down
    #return(default_breaks[seq(1, n_breaks, by = 2)]) 
  } else {
    # If less than 6 breaks, use all defaults
    return(default_breaks)
  }
}#
#
#
####Survey data####
#
Sites <- paste(c("SLC", "LXN", "LXS", "CRE", "CRW"), collapse = "|")
#
Survey_df <- Survey %>% 
  # Update Sites and Add survey codes
  mutate(Site = case_when(Estuary == "CR" ~ paste0(Estuary, SectionName),
                          TRUE ~ substr(StationName, 1 , 3)),
         SurvCode = as.factor(case_when(
           Month  == "03" ~ 1,
           Month == "09" ~ 2,
           TRUE ~ NA))) %>%
  # Limit to desired Sites
  filter(str_detect(StationName, Sites) | Estuary == "CR") %>%
  # Remove random stations
  filter(!str_detect(FixedLocationID, "^[A-Za-z]")) %>%
  # Limit to spring and fall
  filter(!is.na(SurvCode))
  
#
(Long_mean <- Survey_df %>%
  group_by(Site) %>%
  summarise(AveLive = mean(TotalLive, na.rm = T)))
#
Annual_means <- Survey_df %>%
  group_by(Site, Year) %>%
  summarise(MeanLive = mean(TotalLive, na.rm = T),
            SDLive = sd(TotalLive, na.rm = T),
            SELive = sd(TotalLive) / sqrt(length(TotalLive))) %>%
  left_join(Long_mean) %>%
  mutate(Diff = case_when(AveLive > MeanLive ~ "Low",
                          AveLive < MeanLive ~ "High",
                          TRUE ~ NA))
#
#
#
####Survey plot####
#
#
Annual_means %>%
  ggplot(aes(Year, MeanLive))+
  geom_bar(stat = "identity", fill = "#999999")+
  geom_errorbar(aes(ymin = MeanLive-SELive, ymax = MeanLive + SELive), width = 0.3)+
  geom_hline(aes(yintercept = AveLive), linetype = "dashed", linewidth = 1, color = "#333333")+
  lemon::facet_rep_wrap(.~Site, scales = "free_y", ncol = 1, labeller = as_labeller(Site_names))+
  scale_y_continuous(expression("Live oysters/m"^2), expand = expansion(mult = c(0, 0.05)), limits = c(0, NA), breaks = pretty_breaks(n = 4))+
  #scale_fill_manual(values = c("#3CB371", "#F88379"))+
  Base + theme_f +
  theme(axis.text.x = element_text(angle = 45, size = 14))
#740*845
#
#