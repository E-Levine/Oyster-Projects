##Functions for HSM accessory data
#
##Shorebirds
#
## Load shorebird data:
#Counties = list of county names
load_shorebird_data <- function(Counties, SiteCode, VersionNumber){
  data_dir <- paste0("Data layers/Shorebirds/")
  output_name <- paste0(SiteCode, "_", VersionNumber, "_shorebird_data")  
  #
  solitary_rawdata <- data.frame()  # For "Solitary summary" sheet
  colonial_rawdata <- data.frame()  # For "Colonial summary" sheet
  search_rawdata <- NULL
  #
  #Iterate over counties listed:
  for(County in Counties){
    # Build match pattern:
    pattern <- paste0("^Shorebirds_", County, ".*\\.xlsx$")
    # List all matching shape files
    data_files <- list.files(path = data_dir, pattern = pattern, full.names = TRUE)
    file_name <- sub("^.*/", "", data_files)
    #
    for (file in data_files) {
      # Load the "SearchCriteria" sheet 
      if ("SearchCriteria" %in% excel_sheets(file)) {
        search_data <- readxl::read_excel(file, sheet = "SearchCriteria") %>% drop_na(`Search Option`) %>% filter(!str_detect(`Search Option`, "Created"))
        # Data manipulation: Move last value in 'Search Option' column to the same row as 'Data Analysis Note:' value in 'Search Criteria' column
        # Find the row where 'Search Criteria' contains "Data Analysis Note:"
        note_row <- which(grepl("Data Analysis Note:", search_data$'Search Option'))
        if (length(note_row) > 0) {
          # Get the last value from 'Search Option' column
          last_option <- tail(search_data$'Search Option', 1)
          # Append it to the 'Search Criteria' in that row
          search_data$'Search Criteria Selected'[note_row] <- last_option
          # Remove the last row from search_data (assuming it was the source of the moved value)
          if (nrow(search_data) > 1) {
            search_data <- search_data[-nrow(search_data), ]
          }
        }
        names(search_data)[names(search_data) == "Search Criteria Selected"] <- paste0(County, "_", "Search Criteria Selected")
        if (is.null(search_rawdata)) {
          # For the first matching sheet, load the format directly
          search_rawdata <- search_data
        } else {
          # For subsequent sheets, full_join based on common columns
          search_rawdata <- full_join(search_rawdata, search_data, by = intersect(names(search_rawdata), names(search_data)))
        }
      }
      # Load the "Solitary summary" sheet and append to solitary_rawdata
      if ("SolitarySummary" %in% excel_sheets(file)) {
        solitary_data <- readxl::read_excel(file, sheet = "SolitarySummary")
        solitary_rawdata <- rbind(solitary_rawdata, solitary_data)
      }
      # Load the "Colonial summary" sheet and append to colonial_rawdata
      if ("ColonialSummary" %in% excel_sheets(file)) {
        colonial_data <- readxl::read_excel(file, sheet = "ColonialSummary")
        # Check if the first value (assuming first cell) is "No data returned"
        if (nrow(colonial_data) > 0 && ncol(colonial_data) > 0 && colonial_data[1, 1] == "No data returned") {
          # Skip appending if condition is met (do nothing)
        } else {
          colonial_rawdata <- rbind(colonial_rawdata, colonial_data)
        }
      }
      #
    }
    if (length(data_files) == 0) {
      message(paste0("No data file found for county: ", County))
    } else {
      #Print list of files loaded:
      message(paste("File loaded for:", County, "::", paste(file_name, collapse = "\n")))
    }
  }
    #Return final data to global
    Solitary_nests_raw <<- solitary_rawdata
    Colonial_nests_raw <<- colonial_rawdata
    Search_raw <<- search_rawdata
    #
}
#
#
## Function to split coordinates
split_coordinates <- function(coord_string){
  # Split the input string by spaces to get individual coordinate pairs
  coord_pairs <- strsplit(coord_string, " ")[[1]]
  # Split each pair by comma and convert to numeric
  lon_lat <- lapply(coord_pairs, function(pair) {
    parts <- strsplit(pair, ",")[[1]]
    as.numeric(parts)
  })
  # Create a data frame with lon and lat columns
  df <- data.frame(
    lon = sapply(lon_lat, `[`, 1),
    lat = sapply(lon_lat, `[`, 2)
  )
  # Add point ID
  df$PointID = 1:nrow(df)
  #
  return(df)
}
#split_coordinates(Colonial_nests_raw$ColonyFootprintCoordinates)
#
## Clean shorebird data, limit to IBNB if specified.
clean_shorebird_data <- function(IBNB = "No"){
  #
  # Solitary data:
  if(exists("Solitary_nests_raw", envir = globalenv())){
    # Limitation to IBNB
    if(IBNB == "Yes"){
      Solitary_nests <<- Solitary_nests_raw %>% 
        dplyr::select(SurveyYear, LocationID, County, Longitude, Latitude, 
                      SiteName, Species, LastVisit, LastReportedStatus) %>% 
        filter(tolower(Species) == "snowy plover" | tolower(Species) == "american oystercatcher" | tolower(Species) == "black skimmer" | tolower(Species) == "least tern")
    } else if(IBNB == "No"){
      # All data
      Solitary_nests <<- Solitary_nests_raw %>% 
        dplyr::select(SurveyYear, LocationID, County, Longitude, Latitude, 
                      SiteName, Species, LastVisit, LastReportedStatus)
    } else {
      message("Please specify if data should be limited to imperiled beach-nesting bird species: Yes/No")
    }
  } else {
    message("No raw solitary nest data found")
  }
  #
  # Colonial data
  if(exists("Colonial_nests_raw", envir = globalenv())){
    if(IBNB == "Yes"){
      # Clean data
      Colonial_nests_t <- Colonial_nests_raw %>%
        dplyr::select(SurveyYear, LocationID, County, Longitude, Latitude, SiteName, Species, LastVisit, ColonyFootprintCoordinates) %>%
        dplyr::rename("coords" = ColonyFootprintCoordinates) %>%
        filter(tolower(Species) == "snowy plover" | tolower(Species) == "american oystercatcher" | tolower(Species) == "black skimmer" | tolower(Species) == "least tern")
      #
      # Split coordinate points
      parsed_list <- lapply(seq_len(nrow(Colonial_nests_t)), function(i) {
        # Split coordinates
        result <- split_coordinates(Colonial_nests_t$coords[i])
        # Add the other columns to each parsed data frame
        other_cols <- Colonial_nests_t[1, -which(names(Colonial_nests_t) == "coords"), drop = FALSE]
        result <- cbind(other_cols, result)
        return(result)
      })
      #
      Colonial_nests <<- do.call(rbind, parsed_list) %>% as_tibble()
    } else if(IBNB == "No"){
      # Clean data
      Colonial_nests_t <- Colonial_nests_raw %>%
        dplyr::select(SurveyYear, LocationID, County, Longitude, Latitude, SiteName, Species, LastVisit, ColonyFootprintCoordinates) %>%
        dplyr::rename("coords" = ColonyFootprintCoordinates)
      #
      # Split coordiant points
      parsed_list <- lapply(seq_len(nrow(Colonial_nests_t)), function(i) {
        # Split coordinates
        result <- split_coordinates(Colonial_nests_t$coords[i])
        # Add the other columns to each parsed data frame
        other_cols <- Colonial_nests_t[1, -which(names(Colonial_nests_t) == "coords"), drop = FALSE]
        result <- cbind(other_cols, result)
        return(result)
      })
      #
      Colonial_nests <<- do.call(rbind, parsed_list) %>% as_tibble()
    } else {
      message("Please specify if data should be limited to imperiled beach-nesting bird species: Yes/No")
    }
    #
  } else {
    message("No raw colonial nest data found")
  }
}
#
#
save_shorebird_data <- function(SiteCode, VersionNumber){
  # Establish path and file name:
  file_path <- paste0(SiteCode, "_", VersionNumber, "/Output/Data files/")
  file_name <- paste0(SiteCode, "_", VersionNumber, "_Shorebird_data")
  #
  # Create the workbook with sheets for each data frame:
  wb <- createWorkbook()
  #
  addWorksheet(wb, "SearchCriteria")
  writeData(wb, "SearchCriteria", Search_raw)
  #
  addWorksheet(wb, "SolitaryNests")
  writeData(wb, "SolitaryNests", Solitary_nests)
  #
  addWorksheet(wb, "ColonialNests")
  writeData(wb, "ColonialNests", Colonial_nests)
  #
  # Check if the file exists
  if (file.exists(paste0(file_path, file_name, ".xlsx"))) {
    if(interactive()){
      result<- select.list(c("yes", "no"), title = paste0("File:", file_name, "already exists.\n Do you want to overwrite it?: "))
      if(tolower(result) == "no"){
        file_name <- paste0(file_name, "_", Sys.Date())
        saveWorkbook(wb, paste0(file_path, file_name, ".xlsx"), overwrite = TRUE) 
        message("Shorebird data was written to file containing today's date")
      } else {
        saveWorkbook(wb, paste0(file_path, file_name, ".xlsx"), overwrite = TRUE)
      }
    }
  }
  #
  saveWorkbook(wb, paste0(file_path, file_name, ".xlsx"), overwrite = TRUE)
  cat("File written successfully to:", file_name, "\n")
  #
}
#
