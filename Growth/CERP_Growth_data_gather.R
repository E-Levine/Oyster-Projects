##CERP Growth data gather from database
#
#
#
##Parameters
Database = "Oysters_24-09-25"
Server = "localhost\\ERICALOCALSQL"
EstuaryCode = c("SL", "LX", "CR") #two-letter Estuary code of interest
#
#
#Load packages, install as needed
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               odbc, DBI, dbplyr, lubridate,
               rMR, scales, knitr, openxlsx,
               install = TRUE)
#
#
#
####Connect and pull tables####
#
con <- dbConnect(odbc(),
                 Driver = "SQL Server", 
                 Server = Server,
                 Database = Database,
                 Authentication = "ActiveDirectoryIntegrated")
#
dboFixedLocations <- tbl(con,in_schema("dbo", "FixedLocations")) %>%
  collect() %>% 
  filter(Cage == "Y" & Estuary %in% EstuaryCode)

hsdbTripInfo <- tbl(con,in_schema("hsdb", "TripInfo")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

dboTripInfo <- tbl(con,in_schema("dbo", "TripInfo")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

hsdbSampleEvent <- tbl(con,in_schema("hsdb", "SampleEvent")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

dboSampleEvent <- tbl(con,in_schema("dbo", "SampleEvent")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

hsdbSampleEventWQ <- tbl(con,in_schema("hsdb", "SampleEventWQ")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

dboSampleEventWQ <- tbl(con,in_schema("dbo", "SampleEventWQ")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

hsdbCageCount <- tbl(con,in_schema("hsdb", "CageCount")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

dboCageCount <- tbl(con,in_schema("dbo", "CageCount")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

hsdbCageSH <- tbl(con,in_schema("hsdb", "CageSH")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

dboCageSH <- tbl(con,in_schema("dbo", "CageSH")) %>%
  collect() %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")

DBI::dbDisconnect(con)
#
#
#
####Clean data tables####
#
head(dboFixedLocations)
(FixedLocations <- dboFixedLocations %>% 
  mutate(StationNumber = as.numeric(StationNumber)) %>% 
  select(FixedLocationID,
         Estuary,
         SectionName, 
         StationNumber) %>% 
  distinct())
#
head(dboTripInfo)
(TripInfo <- rbind(dboTripInfo, hsdbTripInfo) %>% filter(substring(TripType, 1, 4) == "Cage"))
#
head(dboCageCount)
(CageCount <- rbind(dboCageCount %>% dplyr::select(CageCountID:DaysDeployed, Comments) %>%
                     mutate(DeployedDate = as.Date(DeployedDate),
                            RetrievedDate = as.Date(RetrievedDate),
                            CageColor = as.factor(CageColor),
                            DataType = as.factor(DataType)),
                   hsdbCageCount %>% dplyr::select(CageCountID:DaysDeployed, Comments) %>%
                     mutate(DeployedDate = as.Date(DeployedDate),
                            RetrievedDate = as.Date(RetrievedDate),
                            CageColor = as.factor(CageColor),
                            DataType = as.factor(DataType))))
#
head(dboCageSH)
(CageSH <- rbind(dboCageSH %>% dplyr::select(ShellHeightID:ShellHeight, Comments),
                hsdbCageSH %>% dplyr::select(ShellHeightID:ShellHeight, Comments)))
#
head(dboSampleEvent)
(SampleEvent <- rbind(dboSampleEvent %>% dplyr::select(SampleEventID:FixedLocationID, Comments),
                     hsdbSampleEvent %>% dplyr::select(SampleEventID:FixedLocationID, Comments)) %>%
    filter(SampleEventID %in% (CageCount$SampleEventID)))
#
head(dboSampleEventWQ)
(SampleEventWQ <- rbind(dboSampleEventWQ %>% dplyr::select(SampleEventWQID:TurbidityHach, CollectionTime, PercentDissolvedOxygen, Comments),
                        hsdbSampleEventWQ %>% dplyr::select(SampleEventWQID:TurbidityHach, CollectionTime, PercentDissolvedOxygen, Comments)) %>%
    filter(SampleEventID %in% (CageCount$SampleEventID%>% str_replace("CAGE", "COLL"))))
#
rm(dboFixedLocations, dboTripInfo, hsdbTripInfo, dboCageCount, hsdbCageCount, dboCageSH, hsdbCageSH, dboSampleEvent, hsdbSampleEvent, dboSampleEventWQ, hsdbSampleEventWQ)
#
write.xlsx(list("FixedLocations" = FixedLocations, "TripInfo" = TripInfo, "SampleEvent" = SampleEvent,
                "SampleEventWQ" = SampleEventWQ, "CageCount" = CageCount, "CageSH" = CageSH), 
           file = "Growth_database_2024_09_25.xlsx")
