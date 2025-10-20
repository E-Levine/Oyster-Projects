##Functions for HSM accessory data
#
##Shorebirds
#
##Load shorebird data:
#Counties = list of county names
load_shorebird_data <- function(Counties, SiteCode, VersionNumber){
  data_dir <- paste0("Data layers/Shorebirds/")
  output_name <- paste0(SiteCode, "_", VersionNumber, "_shorebird_data")  
  #
  solitary_rawdata <- data.frame()  # For "Solitary summary" sheet
  colonial_rawdata <- data.frame()  # For "Colonial summary" sheet
  #
  #Iterate over counties listed:
  for(County in Counties){
    # Build match pattern:
    pattern <- paste0("^Shorebirds_", County, ".*\\.xlsx$")
    # List all matching shapefiles
    data_files <- list.files(path = data_dir, pattern = pattern, full.names = TRUE)
    file_name <- sub("^.*/", "", data_files)
    #
    for (file in data_files) {
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
          # Skip appending if condition is met
          next
        } else {
          colonial_rawdata <- rbind(colonial_rawdata, colonial_data)
        }
      }
    }
    #Return final data to global
    Solitary_nests_raw <<- solitary_rawdata
    Colonial_nests_raw <<- colonial_rawdata
    #
    if (length(data_files) == 0) {
      message(paste0("No data file found for county: ", County))
    } else {
      #Print list of files loaded:
      message(paste("File loaded for:", County, "::", paste(file_name, collapse = "\n")))
    }
  }
}
#
#
##Function to split coordinates
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
##Clean shorebird data, limit to IBNB if specified.
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