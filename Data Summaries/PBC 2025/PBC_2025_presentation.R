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
#Collecting all data for site.
#
###Database download####
# Connect to Local database server and pull all necessary data, then close connection 
con <- dbConnect(odbc(),
                 Driver = "SQL Server", 
                 Server = Server,
                 Database = Database,
                 Authentication = "ActiveDirectoryIntegrated")
#
FixedLocations <- tbl(con,in_schema("dbo", "FixedLocations")) %>%  collect() %>% filter(Estuary %in% Estuaries)
#
hsdbTripInfo <- tbl(con,in_schema("hsdb", "TripInfo")) %>%  collect() 
dboTripInfo <- tbl(con,in_schema("dbo", "TripInfo")) %>%  collect() 
#
hsdbSampleEvent <- tbl(con,in_schema("hsdb", "SampleEvent")) %>%   collect()
dboSampleEvent <- tbl(con,in_schema("dbo", "SampleEvent")) %>%   collect()
#
hsdbSampleEventWQ <- tbl(con,in_schema("hsdb", "SampleEventWQ")) %>%   collect()
dboSampleEventWQ <- tbl(con,in_schema("dbo", "SampleEventWQ")) %>%   collect()
#
hsdbRCRT <- tbl(con,in_schema("hsdb", "SedimentTrap")) %>%  collect() 
dboRCRT <- tbl(con,in_schema("dbo", "SedimentTrap")) %>%  collect() 
#
DBI::dbDisconnect(con)
#
#
####Data filtering/cleaning####
#
TripInfo 
hsdbTripInfo %>% 
  filter(substring(TripID,1,2) %in% Estuaries)  %>%  arrange(TripID)
         