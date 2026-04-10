## Data cleaning
#
#
## Load raw data and clean
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
Data_type <- c("SRVY")
Initials <- c("ELW")
#
#
##### Load data files ####
#
## Trip Info data
Trip_raw <- read_excel("Data/Raw data/TripInfo_raw_shared.xlsx", sheet = "TripInfo", #File name and sheet name
                        skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                        na = c("", "NULL", " ", "NAN", "na"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                        .name_repair = "unique") 
#
## Sample Event
Sample_raw <- read_excel("Data/Raw data/SampleEvent_raw_shared.xlsx", sheet = "SampleEvent", #File name and sheet name
                       skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                       na = c("", "NULL", " ", "NAN", "na"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                       .name_repair = "unique") 
#
## Fixed Locations
FLID_raw <- read_excel("Data/Raw data/FixedLocations_raw_shared.xlsx", sheet = "FixedLocations", #File name and sheet name
                       skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                       na = c("", "NULL", " ", "NAN", "na"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                       .name_repair = "unique") 
#
## Survey
Quads_raw <- read_excel("Data/Raw data/SurveyQuadrat_raw.xlsx", sheet = "SurveyQuadrat_raw", #File name and sheet name
                       skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                       na = c("", "NULL", " ", "NAN", "na"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                       .name_repair = "unique") 
#
## Survey SHs
SHs_raw <- read_excel("Data/Raw data/SurveySH_raw.xlsx", sheet = "SurveySH_raw", #File name and sheet name
                        skip = 0, col_names = TRUE,  #How many rows to skip at top; are column names to be used
                        na = c("", "NULL", " ", "NAN", "na"), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                        .name_repair = "unique") 
#
#
#
#
#### Clean data files #####
#
## Station information
(FLIDs <- FLID_raw %>%
  # Limit to desired columns to add to other data
  dplyr::select("FLID" = FixedLocationID,
                Estuary,
                SectionName,
                StationNumber,
                StartDate,
                EndDate) %>%
   mutate(StartDate = as.Date(StartDate),
          EndDate = as.Date(EndDate)))
#
#
#
## Survey counts
(Quads_c <- Quads_raw %>%
    # Correct column types
    mutate(across(c("NumLive", "NumDead", "TotalVolume", "TotalWeight", "NumLegal"), as.numeric)) %>%
    # Remove row number column
    dplyr::select(-`...1`)  %>%
    # Create Year, Month, Site, Station columns
    mutate(Date = as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d"),
           Year = substr(SampleEventID, 8, 11),
           Month = substr(SampleEventID, 12, 13),
           Estuary = substr(SampleEventID, 1, 2),
           FLID = substr(SampleEventID, 19, 22)) %>%
    # Calculate total live/dead per m2. NOTE: 2005-2007 used 1m2, 2008+ used 1/4m2
    mutate(TotalLive = as.numeric(case_when(Year == "2005" | Year == "2006" | Year == "2007" ~ NumLive, TRUE ~ NumLive*4)),
           TotalDead = as.numeric(case_when(Year == "2005" | Year == "2006" | Year == "2007" ~ NumDead, TRUE ~ NumDead*4))) %>%
    #Add DeadRatio 
    mutate(Total = NumLive+NumDead,
           DeadRatio = as.numeric(case_when(Total == 0 ~ 0, TRUE ~ NumDead/Total))) %>%
    # Column types
    mutate(across(c("Year", "Month", "Estuary", "QuadratNumber"), as.factor)) %>% 
    # Reorganize columns
    dplyr::select(Date, Year, Month, Estuary, FLID, TotalLive, TotalDead, everything()))
#
# Add FLID info to Quads
(Quads_df <- left_join(Quads_c, 
                      FLIDs))

#### Data summary ####
#
## Estuary annual summary
(Annual_summ <- Quads_df %>%
   # Group by all columns needed in data output
   group_by(Year, Estuary) %>%
   summarise(Samples = n(),
             meanLive = mean(TotalLive, na.rm = TRUE),
             minLive = min(TotalLive, na.rm = TRUE),
             maxLive = max(TotalLive, na.rm = TRUE),
             meanDead = mean(TotalDead, na.rm = TRUE),
             minDead = min(TotalDead, na.rm = TRUE),
             maxDead = max(TotalDead, na.rm = TRUE),
             meanDeadRatio = mean(DeadRatio, na.rm = TRUE),
             minDeadRatio = min(DeadRatio, na.rm = TRUE),
             maxDeadRatio = max(DeadRatio, na.rm = TRUE)))
#
#
#
#### Figure formatting ####
# margin(t = 10, l = 2, r = 2)
#
basetheme <- theme_bw()+
  theme(axis.title.x = element_text(size = 12, face = "bold", color = "black"), 
        axis.text.x = element_text(size = 11, margin = margin(t=0.5, l=0.5, r=0, b=0.15, unit = "cm")),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"), 
        axis.text.y = element_text(size = 11, margin = margin(t=0, l=0.25, r=0, b=0, unit = "cm")),
        panel.grid = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"))
#
#
#
#### Figures ####
#
Annual_summ %>%
  filter(Estuary == "SL") %>%
  ggplot(aes(Year, meanLive))+
  geom_bar(stat = "identity")+
  scale_x_discrete(expand = c(0.05,0))+
  scale_y_continuous("Average live oysters per m2", expand = c(0,0), limits = c(0, 400))+
  basetheme + 
  theme(axis.text.x = element_text(angle = 45))
#
Annual_summ %>%
  filter(Estuary == "SL") %>%
  ggplot(aes(Year, meanDeadRatio))+
  geom_bar(stat = "identity")+
  scale_x_discrete(expand = c(0.05,0))+
  scale_y_continuous("Average DeadRatio", expand = c(0,0), limits = c(0,1))+
  basetheme + 
  theme(axis.text.x = element_text(angle = 45))
#
#
#