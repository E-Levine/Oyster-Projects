###Figures for Palm Beach County meeting February 2025
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
Author <- c("E Levine") #Change to your name
Database <- "Oysters_25-01-27"  #Set the local database to use
Server = "localhost\\ERICALOCALSQL" #Set the local Server to use
#
##Sites of interest
Estuaries <- c("LW")
##Data types of interest: Recruitment, Collection/Dissection. Sediment Trap, Semi-Annual Survey, Cage Growth and Mortality, Survey, Shell Budget, Wave
DataTypes <- c("Recruitment", "Collection/Dissection", "Sediment Trap")
DataTypeCodes <- c("RCRT", "COLL", "SDTP")
#Dates of interest
Start_date <- as.Date("2000-01-01")
End_date <- as.Date("2024-12-31")
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
hsdbDermo <- tbl(con,in_schema("hsdb", "Dermo")) %>%  collect() 
dboDermo <- tbl(con,in_schema("dbo", "Dermo")) %>%  collect() 
#
hsdbRepro <- tbl(con,in_schema("hsdb", "Repro")) %>%  collect() 
dboRepro <- tbl(con,in_schema("dbo", "Repro")) %>%  collect() 
#
hsdbRCRT <- tbl(con,in_schema("hsdb", "Recruitment")) %>%  collect() 
dboRCRT <- tbl(con,in_schema("dbo", "Recruitment")) %>%  collect() 
#
hsdbSDTP <- tbl(con,in_schema("hsdb", "SedimentTrap")) %>%  collect() 
dboSDTP <- tbl(con,in_schema("dbo", "SedimentTrap")) %>%  collect() 
#
DBI::dbDisconnect(con)
#
#
####Data filtering/cleaning####
#
FixedLocations <- FixedLocations_raw %>% filter(Estuary %in% Estuaries)
#
(TripInfo <- rbind(hsdbTripInfo %>% filter(substring(TripID,1,2) %in% Estuaries & TripType %in% DataTypes) %>%  arrange(TripID),
                  dboTripInfo %>% filter(substring(TripID,1,2) %in% Estuaries & TripType %in% DataTypes) %>%  arrange(TripID)) %>%
  dplyr::select(TripID:DataStatus, Comments))
rm(hsdbTripInfo, dboTripInfo)        
#
#
(SampleEvent <- rbind(hsdbSampleEvent %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22), DataType = substring(SampleEventID,3,6)) %>%
                       filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate > Start_date & TripDate < End_date & DataType %in% DataTypeCodes),
                     dboSampleEvent %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22), DataType = substring(SampleEventID,3,6)) %>%
                       filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate > Start_date & TripDate < End_date & DataType %in% DataTypeCodes)) %>%
    left_join(FixedLocations) %>% 
    dplyr::select(TripDate, DataType, Estuary, SectionName, StationName, StationNumber, SampleEventID:FixedLocationID, DataStatus, Comments, EstuaryLongName) %>% arrange(TripDate))
rm(hsdbSampleEvent, dboSampleEvent)
#
#
(SampleEventWQ <- rbind(hsdbSampleEventWQ %>% mutate(TripDate = as.Date(substring(SampleEventWQID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventWQID, 19, 22)) %>%
                         filter(substring(SampleEventWQID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date),
                       dboSampleEventWQ %>% mutate(TripDate = as.Date(substring(SampleEventWQID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventWQID, 19, 22)) %>%
                         filter(substring(SampleEventWQID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date)) %>%
    left_join(FixedLocations, by = c("FixedLocationID")) %>%
  dplyr::select(TripDate, Estuary, SectionName, StationName, StationNumber, SampleEventWQID:TurbidityHach, PercentDissolvedOxygen, CollectionTime, FixedLocationID, EstuaryLongName) %>% arrange(TripDate))
rm(hsdbSampleEventWQ, dboSampleEventWQ)
#
#
(Dermo <- rbind(hsdbDermo %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22), DermoMantle = as.numeric(DermoMantle), DermoGill = as.numeric(DermoGill)) %>%
                 dplyr::select(-OldSampleNumber) %>% filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date),
               dboDermo %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22), DermoMantle = as.numeric(DermoMantle), DermoGill = as.numeric(DermoGill)) %>%
                 filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date)) %>%
  left_join(FixedLocations) %>% mutate(Year = format(TripDate, "%Y"), Month = format(TripDate, "%m"), 
                                       MeanDermo = rowMeans(.[, c("DermoMantle", "DermoGill")], na.rm = TRUE), DermoSum = as.numeric(ifelse(rowSums(select(., contains("Dermo"))) >0, 1, 0))) %>% 
  dplyr::select(TripDate, Estuary, SectionName, StationName, StationNumber, OysterID:ShellHeight, DermoMantle:DermoGill, MeanDermo, DermoSum, Comments, Year, Month, FixedLocationID, EstuaryLongName) %>% arrange(TripDate))
rm(hsdbDermo, dboDermo)
#
#
(Repro <- rbind(hsdbRepro %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22)) %>% 
                  dplyr::select(-OldSampleNumber) %>% filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date),
                dboRepro %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22)) %>% 
                  filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date)) %>%
    left_join(FixedLocations) %>% mutate_at(c("SectionName", "StationName", "StationNumber", "Sex", "FixedLocationID", "ReproStage"), as.factor) %>%
    mutate(OldSample = as.integer(str_extract(Comments, "(?<=OldStage=).*?(?=;|$)"))) %>% #Extract the Old Stage number
  mutate(ReproStage = as.factor(case_when(Parasite == "Buceph" ~ "8", #Set Buceph to Stage = 8
                                Sex == "M/F" ~ "M/F", #Set M/F to Stage = M/F
                                BadSlide == "Y" ~ NA, #Remove bad slides
                                OldSample == 0 | OldSample == 10 ~ "1",
                                OldSample > 0 &  OldSample < 5 ~ "2",
                                OldSample > 4 &  OldSample < 7 ~ "3",
                                OldSample > 6 &  OldSample < 10 ~ "4",
                                BadSlide == "N" & is.na(OldSample) & is.na(ReproStage) ~ NA,
                                TRUE ~ ReproStage))) %>%
    dplyr::select(TripDate, Estuary, SectionName, StationName, StationNumber, OysterID:BadSlide, Comments, FixedLocationID, EstuaryLongName) %>% arrange(TripDate))
rm(hsdbRepro, dboRepro)
#
#
(Sediment <- rbind(hsdbSDTP %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22), NumDays = as.numeric(interval(DeployedDate, TripDate), "days")) %>%
                     filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date),
                   hsdbSDTP %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22), NumDays = as.numeric(interval(DeployedDate, TripDate), "days")) %>%
                     filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date)) %>%
    left_join(FixedLocations) %>%
    dplyr::select(TripDate, Estuary, SectionName, StationName, StationNumber, CupSampleID:NumOtherBiota, TareCrucible:CrucibleDW, Comments, FixedLocationID, EstuaryLongName) %>% arrange(TripDate))
rm(hsdbSDTP, dboSDTP)
#
#
(Recruitment <- rbind(hsdbRCRT %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(ShellID, 19, 22), NumDays = as.numeric(interval(DeployedDate, TripDate), "days")) %>%
                        filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date),
                      dboRCRT %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(ShellID, 19, 22), NumDays = as.numeric(interval(DeployedDate, TripDate), "days")) %>%
                        filter(substring(SampleEventID,1,2) %in% Estuaries & TripDate >= Start_date & TripDate <= End_date)) %>%
    left_join(FixedLocations) %>% 
    mutate(DeployedDate = as.Date(DeployedDate, format = "%Y-%m-%d"), Year = format(TripDate, "%Y"), Month = format(TripDate, "%m"),
           BottomCount = case_when(ShellPosition == 1|ShellPosition == 6|ShellPosition == 7|ShellPosition == 12 ~ NA, TRUE ~ NumBottom),  RcrtRate = BottomCount/(as.numeric(TripDate-DeployedDate)/28)) %>%
    dplyr::select(TripDate, Estuary, SectionName, StationName, StationNumber, BottomCount, RcrtRate, ShellID:NumBottom, Comments, Year, Month, FixedLocationID, EstuaryLongName))
rm(hsdbRCRT, dboRCRT, con)
#
#
#
####Formatting####
#
Month_abbs <- c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr", "05" = "May", "06" = "Jun",
                "07" = "Jul", "08" = "Aug", "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")
#
#Map color to Stage
Stages <- c("1" = "Developing", "2" = "Ripe/Spawning", "3" = "Spent/Recycling", "4" = "Indifferent", "8" = "Buceph", "M/F" = "M/F", "Z" = "Z")
cbPalette <- c("#D55E00", "#E69F00", "#009E73", "#56B4E9", "#9966FF", "#333333", "#666666")
names(cbPalette) <- levels(Repro$ReproStage)
StaFill <- scale_fill_manual(name = "Stage", labels = Stages, values = cbPalette, na.value = "#999999")
StaColor <- scale_color_manual(name = "Stage", labels = Stages, values = cbPalette, na.value = "#999999")
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
theme_f <- theme(strip.text.y = element_text(color = "black", size = 15, family = "sans", face = "bold"),
                 strip.background = element_rect(fill = "#999999"),
                 panel.spacing = unit(0.75, "lines"),
                 strip.text.x = element_text(size = 13, face = "bold", family = "sans"))
#
#
#
####Repro figure####
#
#
head(Repro)
Repro_c <- Repro %>% filter(!is.na(ReproStage) & ReproStage != "M/F" & ReproStage != "8") %>% droplevels() %>% #limit to only determined repro stages
  mutate(Year = as.factor(format(TripDate, "%Y")), Month = as.factor(format(TripDate, "%m"))) #Add year and month columns
#
(Stage_counts <- left_join(Repro_c %>% group_by(Year, Month, ReproStage) %>% summarise(Count = n()), #Determine counts per stage, total number
                          Repro_c %>% group_by(Year, Month) %>% summarise(Total = n())) %>%
    mutate(Prop = Count/Total) %>% ungroup() %>% complete(ReproStage, nesting(Year, Month), fill = list(Count = 0, Total = 0, Prop = 0))) #Determine proportion per stage, complete any missing data with 0s
#
#Data frames of Month*Years with 33%, 50%, or 66% ripe samples
(Ripe_0.33 <- Stage_counts %>% group_by(Year, Month, ReproStage) %>%
    summarise(meanProp3 = round(mean(Prop), 3)) %>% subset(ReproStage == 2) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp3 > 0.333 & meanProp3 < 0.5)) 
#
(Ripe_0.5 <- Stage_counts %>% group_by(Year, Month, ReproStage) %>%
    summarise(meanProp5 = round(mean(Prop), 3)) %>% subset(ReproStage == 2) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>% print(n = Inf) %>%#Arrange and group by Year
    filter(meanProp5 > 0.5 & meanProp5 < 0.66))
#
(Ripe_0.66 <- Stage_counts %>% group_by(Year, Month, ReproStage) %>%
    summarise(meanProp3 = round(mean(Prop), 3)) %>% subset(ReproStage == 2) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp3 > 0.66))
#
Stage_counts %>% group_by(Year, Month, ReproStage) %>%
  summarise(meanProp = round(mean(Prop), 3)) %>% subset(ReproStage == 2) %>%
  ggplot()+
  geom_rect(data = Ripe_0.33, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "darkblue", alpha = 0.5)+
  geom_rect(data = Ripe_0.5, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "orange", alpha = 0.5)+
  geom_rect(data = Ripe_0.66, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.5)+
  geom_rect(data = data.frame(Year = as.factor(seq(2005, 2024, by = 1)), Month = as.factor("01")), aes(xmin = 0.5, xmax = 1, ymin = -Inf, ymax = Inf), fill = "white")+
  geom_rect(data = data.frame(Year = as.factor(seq(2005, 2024, by = 1)), Month = as.factor("12")), aes(xmin = 12, xmax = 12.5, ymin = -Inf, ymax = Inf), fill = "white")+
  geom_text(aes(label = Year), x = 0.75, y = 0.55, size = 6)+
  geom_line(aes(Month, meanProp, group = ReproStage), linewidth = 1.25)+
  lemon::facet_rep_grid(Year~.)+
  Prez + theme(panel.spacing.y = unit(0.05, "lines"), strip.text = element_blank(),
                         axis.title = element_text(size = 20, color = "black", family = "sans"),
                         axis.text.x = element_text(size = 18, color = "black", family = "sans", margin = unit(c(0, 0, -0.2, 0), "cm"), vjust = 0),
                         axis.text.y = element_text(size = 9, color = "black", family = "sans", margin = unit(c(0, 0.1, 0, 0.2), "cm")))+
  scale_x_discrete("", expand = c(0,0.5), labels = Month_abbs)+
  scale_y_continuous("Average proportion in the ripe phase", expand = c(0,0), limits = c(0,1.0), breaks = c(0, 0.5, 1.0))
#LW_Repro_Ripe phase @1400
#
#Data frames of Month*Years with 33%, 50%, or 66% developing samples
(Dev_0.33 <- Stage_counts %>% group_by(Year, Month, ReproStage) %>%
    summarise(meanProp3 = round(mean(Prop), 3)) %>% subset(ReproStage == 1) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp3 > 0.333 & meanProp3 < 0.5)) 
#
(Dev_0.5 <- Stage_counts %>% group_by(Year, Month, ReproStage) %>%
    summarise(meanProp5 = round(mean(Prop), 3)) %>% subset(ReproStage == 1) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>% print(n = Inf) %>%#Arrange and group by Year
    filter(meanProp5 > 0.5 & meanProp5 < 0.66))
#
(Dev_0.66 <- Stage_counts %>% group_by(Year, Month, ReproStage) %>%
    summarise(meanProp3 = round(mean(Prop), 3)) %>% subset(ReproStage == 1) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp3 > 0.66))
#
Stage_counts %>% group_by(Year, Month, ReproStage) %>%
  summarise(meanProp = round(mean(Prop), 3)) %>% subset(ReproStage == 1) %>%
  ggplot()+
  geom_rect(data = Dev_0.33, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "darkblue", alpha = 0.5)+
  geom_rect(data = Dev_0.5, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "orange", alpha = 0.5)+
  geom_rect(data = Dev_0.66, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.5)+
  geom_rect(data = data.frame(Year = as.factor(seq(2005, 2024, by = 1)), Month = as.factor("01")), aes(xmin = 0.5, xmax = 1, ymin = -Inf, ymax = Inf), fill = "white")+
  geom_rect(data = data.frame(Year = as.factor(seq(2005, 2024, by = 1)), Month = as.factor("12")), aes(xmin = 12, xmax = 12.5, ymin = -Inf, ymax = Inf), fill = "white")+
  geom_text(aes(label = Year), x = 0.75, y = 0.55, size = 6)+
  geom_line(aes(Month, meanProp, group = ReproStage), linewidth = 1.25)+
  lemon::facet_rep_grid(Year~.)+
  Prez + theme(panel.spacing.y = unit(0.05, "lines"), strip.text = element_blank(),
               axis.title = element_text(size = 20, color = "black", family = "sans"),
               axis.text.x = element_text(size = 18, color = "black", family = "sans", margin = unit(c(0, 0, -0.2, 0), "cm"), vjust = 0),
               axis.text.y = element_text(size = 9, color = "black", family = "sans", margin = unit(c(0, 0.1, 0, 0.2), "cm")))+
  scale_x_discrete("", expand = c(0,0.5), labels = Month_abbs)+
  scale_y_continuous("Average proportion in the developing phase", expand = c(0,0), limits = c(0,1.0), breaks = c(0, 0.5, 1.0))
#LW_Repro_Developing phase @1400
####Recruitment figure####
#
#
head(Recruitment)
(MeanRcrt <- Recruitment  %>% group_by(TripDate, Estuary, StationName) %>% summarise(MeanRcrt = mean(RcrtRate, na.rm = T)))
(AnnualMeanRcrt  <- Recruitment %>% group_by(Year) %>% summarise(MeanRcrt = mean(RcrtRate, na.rm = T), SDRcrt = sd(RcrtRate, na.rm = T)))

AnnualMeanRcrt %>%
  ggplot(aes(Year, MeanRcrt))+
  geom_errorbar(aes(ymin = MeanRcrt, ymax = MeanRcrt + SDRcrt))+
  geom_col() +
  scale_x_discrete(expand = c(0.05, 0))+
  scale_y_continuous("Mean spat/shell", limits = c(0, 31), expand = c(0,0))+
  Prez + theme(axis.title.x = element_blank(), axis.text.x = element_text(vjust = 0.8, size = 17, angle = 20))
#PBC_Rcrt_Annual
Recruitment %>% group_by(Year, SectionName) %>% 
  summarise(MeanRcrt = mean(RcrtRate, na.rm = T), SDRcrt = sd(RcrtRate, na.rm = T)) %>%
  ggplot(aes(Year, MeanRcrt, fill = SectionName))+
  geom_errorbar(aes(ymin = MeanRcrt, ymax = MeanRcrt + SDRcrt), position = position_dodge())+
  geom_col(position = position_dodge()) +
  scale_x_discrete(expand = c(0.05, 0))+
  scale_y_continuous("Mean spat/shell", limits = c(0, 31), expand = c(0,0))+
  Prez + theme(axis.title.x = element_blank(), axis.text.x = element_text(vjust = 0.8, size = 17, angle = 20))
#PBT_Rcrt_Annual_Site
Recruitment %>% group_by(Year, Month) %>% 
  summarise(MeanRcrt = mean(RcrtRate, na.rm = T), SDRcrt = sd(RcrtRate, na.rm = T)) %>%
  ggplot(aes(Month, MeanRcrt))+
  geom_point() + geom_line(aes(Month, MeanRcrt, group = Year), linewidth = 1.25)+
  lemon::facet_rep_grid(Year~.)+
  scale_x_discrete(expand = c(0.025, 0))
#Months don't show much
#
#
#
#
####Dermo figures####
#
(MeanDermo <- Dermo %>% group_by(TripDate, Year, Month, SectionName, StationName) %>% #Calculate Mean, SD, and Pct dermo
  summarise(MeanInt = mean(MeanDermo, na.rm = T), SDInt = sd(MeanDermo, na.rm = T), Pct = (sum(DermoSum, na.rm = T)/n())*100) %>%
  mutate(Pct = case_when(is.na(MeanInt) ~ NA, TRUE ~ Pct)))
DermoInt <- Dermo %>% group_by(Year, StationName) %>% summarise(MeanInt = mean(MeanDermo, na.rm = T), SDInt = sd(MeanDermo, na.rm = T)) 
DermoPct <- MeanDermo %>% group_by(Year, StationName) %>% summarise(MeanPct = mean(Pct, na.rm = T)) 

 DermoInt %>%
  ggplot(aes(Year, MeanInt))+
  geom_errorbar(aes(ymin = MeanInt, ymax = MeanInt + SDInt), width = 0.3)+
  geom_col(aes(fill = StationName))+
  lemon::facet_rep_grid(StationName ~.)+
  scale_x_discrete(expand = c(0.05,0))
  scale_y_continuous("Mean Dermo Infection Intensity", expand = c(0,0), limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.4))+
  Prez + theme_f + theme(legend.position = "none")

DermoPct %>%
  ggplot(aes(Year, MeanPct))+
  geom_col(aes(fill = StationName))+
  scale_x_discrete(expand = c(0.05,0))+
  scale_y_continuous("Mean Dermo Infection Prevalence", expand = c(0,0), limits = c(0, 100))+
  Prez
#
#
#
#
####All Sites Repro figure####
#
No_Estuary <-  c("SS", "WC", "BB", "ML", "SR", "PE", "SA")
#
(Repro_AllSites <- rbind(hsdbRepro %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22)) %>% 
                  dplyr::select(-OldSampleNumber) %>% filter(!substring(SampleEventID,1,2) %in% No_Estuary & TripDate >= Start_date & TripDate <= End_date),
                dboRepro %>% mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), FixedLocationID = substring(SampleEventID, 19, 22)) %>% 
                  filter(!substring(SampleEventID,1,2) %in% No_Estuary & TripDate >= Start_date & TripDate <= End_date)) %>%
   left_join(FixedLocations_raw) %>% mutate_at(c("SectionName", "StationName", "StationNumber", "Sex", "FixedLocationID", "ReproStage"), as.factor) %>%
   mutate(OldSample = as.integer(str_extract(Comments, "(?<=OldStage=).*?(?=;|$)"))) %>% #Extract the Old Stage number
   mutate(ReproStage = as.factor(case_when(Parasite == "Buceph" ~ "8", #Set Buceph to Stage = 8
                                           Sex == "M/F" ~ "M/F", #Set M/F to Stage = M/F
                                           BadSlide == "Y" ~ NA, #Remove bad slides
                                           OldSample == 0 | OldSample == 10 ~ "1",
                                           OldSample > 0 &  OldSample < 5 ~ "2",
                                           OldSample > 4 &  OldSample < 7 ~ "3",
                                           OldSample > 6 &  OldSample < 10 ~ "4",
                                           BadSlide == "N" & is.na(OldSample) & is.na(ReproStage) ~ NA,
                                           TRUE ~ ReproStage))) %>%
   dplyr::select(TripDate, Estuary, SectionName, StationName, StationNumber, OysterID:BadSlide, Comments, FixedLocationID, EstuaryLongName) %>% arrange(TripDate))
#
Repro_All_c <- Repro_AllSites %>% filter(!is.na(ReproStage) & ReproStage != "M/F" & ReproStage != "8") %>% droplevels() %>% #limit to only determined repro stages
  mutate(Year = as.factor(format(TripDate, "%Y")), Month = as.factor(format(TripDate, "%m"))) #Add year and month columns
#
(AllStage_counts <- left_join(Repro_All_c %>% group_by(Estuary, Year, Month, ReproStage) %>% summarise(Count = n()), #Determine counts per stage, total number
                           Repro_All_c %>% group_by(Estuary, Year, Month) %>% summarise(Total = n())) %>%
    mutate(Prop = Count/Total) %>% ungroup() %>% complete(ReproStage, nesting(Estuary, Year, Month), fill = list(Count = 0, Total = 0, Prop = 0)) %>% #Determine proportion per stage, complete any missing data with 0s
drop_na(Estuary))
#
#Data frames of Month*Years with 33%, 50%, or 66% ripe samples
(All_Ripe_0.33 <- AllStage_counts %>% group_by(Estuary, Year, Month, ReproStage) %>%
    summarise(meanProp3 = round(mean(Prop), 3)) %>% subset(ReproStage == 2) %>% ungroup() %>%
    arrange(Estuary, Year) %>% group_by(Estuary, Year) %>%#Arrange and group by Year
    filter(meanProp3 > 0.333 & meanProp3 < 0.5)) 
#
(All_Ripe_0.5 <- AllStage_counts %>% group_by(Estuary, Year, Month, ReproStage) %>%
    summarise(meanProp5 = round(mean(Prop), 3)) %>% subset(ReproStage == 2) %>% ungroup() %>%
    arrange(Estuary, Year) %>% group_by(Estuary, Year) %>% print(n = Inf) %>%#Arrange and group by Year
    filter(meanProp5 > 0.5 & meanProp5 < 0.66))
#
(All_Ripe_0.66 <- AllStage_counts %>% group_by(Estuary, Year, Month, ReproStage) %>%
    summarise(meanProp3 = round(mean(Prop), 3)) %>% subset(ReproStage == 2) %>% ungroup() %>%
    arrange(Estuary, Year) %>% group_by(Estuary, Year) %>%#Arrange and group by Year
    filter(meanProp3 > 0.66))
#
AllStage_counts %>% group_by(Estuary, Year, Month, ReproStage) %>%
  summarise(meanProp = round(mean(Prop), 3)) %>% subset(ReproStage == 2) %>%
  ggplot()+
  geom_rect(data = All_Ripe_0.33 , aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "darkblue", alpha = 0.5)+
  geom_rect(data = All_Ripe_0.5, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "orange", alpha = 0.5)+
  geom_rect(data = All_Ripe_0.66, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.5)+
  #geom_rect(data = data.frame(Year = as.factor(seq(2005, 2024, by = 1)), Month = as.factor("01")), aes(xmin = 0.5, xmax = 1, ymin = -Inf, ymax = Inf), fill = "white")+
  #geom_rect(data = data.frame(Year = as.factor(seq(2005, 2024, by = 1)), Month = as.factor("12")), aes(xmin = 12, xmax = 12.5, ymin = -Inf, ymax = Inf), fill = "white")+
  geom_text(aes(label = Year), x = 0.75, y = 0.55, size = 6)+
  geom_line(aes(Month, meanProp, group = ReproStage), linewidth = 1.25)+
  lemon::facet_rep_grid(Year~Estuary)+ theme_f+
  theme(panel.spacing.y = unit(0.05, "lines"), strip.text = element_blank(),
               axis.title = element_text(size = 20, color = "black", family = "sans"),
               axis.text.x = element_text(size = 14, color = "black", family = "sans", margin = unit(c(0, 0, -0.2, 0), "cm"), vjust = 0, angle = 45),
               axis.text.y = element_text(size = 9, color = "black", family = "sans", margin = unit(c(0, 0.1, 0, 0.2), "cm")))+
  scale_x_discrete("", expand = c(0,0.5), labels = Month_abbs)+
  scale_y_continuous("Average proportion in the ripe phase", expand = c(0,0), limits = c(0,1.0), breaks = c(0, 0.5, 1.0))
#LW_Repro_Ripe phase @1400
#
#Data frames of Month*Years with 33%, 50%, or 66% developing samples
(AllDev_0.33 <- AllStage_counts %>% group_by(Estuary, Year, Month, ReproStage) %>%
    summarise(meanProp3 = round(mean(Prop), 3)) %>% subset(ReproStage == 1) %>% ungroup() %>%
    arrange(Estuary, Year) %>% group_by(Estuary, Year) %>%#Arrange and group by Year
    filter(meanProp3 > 0.333 & meanProp3 < 0.5)) 
#
(AllDev_0.5 <- AllStage_counts %>% group_by(Estuary, Year, Month, ReproStage) %>%
    summarise(meanProp5 = round(mean(Prop), 3)) %>% subset(ReproStage == 1) %>% ungroup() %>%
    arrange(Estuary, Year) %>% group_by(Estuary, Year) %>% print(n = Inf) %>%#Arrange and group by Year
    filter(meanProp5 > 0.5 & meanProp5 < 0.66))
#
(AllDev_0.66 <- AllStage_counts %>% group_by(Estuary, Year, Month, ReproStage) %>%
    summarise(meanProp3 = round(mean(Prop), 3)) %>% subset(ReproStage == 1) %>% ungroup() %>%
    arrange(Estuary, Year) %>% group_by(Estuary, Year) %>%#Arrange and group by Year
    filter(meanProp3 > 0.66))
#
AllStage_counts %>% group_by(Estuary, Year, Month, ReproStage) %>%
  summarise(meanProp = round(mean(Prop), 3)) %>% subset(ReproStage == 1) %>%
  ggplot()+
  geom_rect(data = AllDev_0.33, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "darkblue", alpha = 0.5)+
  geom_rect(data = AllDev_0.5, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "orange", alpha = 0.5)+
  geom_rect(data = AllDev_0.66, aes(xmin = as.numeric(Month)-0.5, xmax = as.numeric(Month)+0.5, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.5)+
  #geom_rect(data = data.frame(Year = as.factor(seq(2005, 2024, by = 1)), Month = as.factor("01")), aes(xmin = 0.5, xmax = 1, ymin = -Inf, ymax = Inf), fill = "white")+
  #geom_rect(data = data.frame(Year = as.factor(seq(2005, 2024, by = 1)), Month = as.factor("12")), aes(xmin = 12, xmax = 12.5, ymin = -Inf, ymax = Inf), fill = "white")+
  geom_text(aes(label = Year), x = 0.75, y = 0.55, size = 6)+
  geom_line(aes(Month, meanProp, group = ReproStage), linewidth = 1.25)+
  lemon::facet_rep_grid(Year~.)+
  lemon::facet_rep_grid(Year~Estuary)+ theme_f+
  theme(panel.spacing.y = unit(0.05, "lines"), strip.text = element_blank(),
               axis.title = element_text(size = 20, color = "black", family = "sans"),
               axis.text.x = element_text(size = 18, color = "black", family = "sans", margin = unit(c(0, 0, -0.2, 0), "cm"), vjust = 0),
               axis.text.y = element_text(size = 9, color = "black", family = "sans", margin = unit(c(0, 0.1, 0, 0.2), "cm")))+
  scale_x_discrete("", expand = c(0,0.5), labels = Month_abbs)+
  scale_y_continuous("Average proportion in the developing phase", expand = c(0,0), limits = c(0,1.0), breaks = c(0, 0.5, 1.0))
#LW_Repro_Developing phase @1400