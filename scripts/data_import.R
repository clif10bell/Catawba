## The script is used for importing Catawba water quality data.

install.packages('officer', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(here)
library(ggplot2)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(httr)
library(jsonlite)
library(readr)
library(knitr)
library(kableExtra)
library(tibble)
library(gridExtra)
library(patchwork)
library(readxl)
library(flextable)
library(officer)

# -------------------------------------------------------------------------

# Importing cyanotoxin data from DES. Doing this first so that it can be pre-processed
# with the other data

file_path <- here("data", "Data_rqst_HABs_Cliff_Bell.xlsx")
Microcystin <- read_excel(file_path, sheet = "Routine_microcystins_data")
cyanotox_complaint <- read_excel(file_path, sheet = "Algal Blooms-complaint sampling")

# Rename columns for consistently with other data frames
Microcystin <- Microcystin %>%
  rename(Microcystin = `Microcystins concentration (ug/L)`)

Microcystin <- Microcystin %>%
  rename(ActivityStartDate = Date)

Microcystin <- Microcystin %>%
  rename(StationID = Station)

#Set BDL values to 0.004 ug/L
Microcystin <- Microcystin %>%
  mutate(Microcystin = ifelse(Microcystin == "BDL", 0.004, Microcystin)) %>%
  mutate(Microcystin = as.numeric(Microcystin))

# Eliminate the Comments column
Microcystin <- Microcystin %>% select(-Comments)

# Add columns for consistency with other data frames

Microcystin <- Microcystin %>%
  # Add ActivityStartTime (blank) after ActivityStartDate
  mutate(ActivityStartTime = "") %>%
  # Add Units ("ug/L") after ActivityStartTime
  mutate(Units = "ug/L") %>%
  # Add Depth_m (0.3) after Microcystin
  mutate(Depth_m = 0.3) %>%
  # Add Source ("DES") after Depth_m
  mutate(Source = "DES") %>%
  # Reorder columns to desired sequence
  select(StationID, ActivityStartDate, ActivityStartTime, Units, Microcystin, Depth_m, Source)

# Convert the dates to a character mdy format. They will be converted back to POSIXct.
# Only doing this so the file dates can be processed with the same function as used
# for the other water quality data frames

Microcystin$ActivityStartDate <- format(Microcystin$ActivityStartDate, "%m-%d-%Y")

#--------------------------------------------------------------------------
# Now import water quality data

# Define a parameter list for reading the csv files

param_list <- c("alkalinity", "ammonia", "chla", "BOD5", "DO", "organiccarbon", "Hardness", 
                "nitrate", "ph", "secchi", "Temp", "TKN", "TN",
                "TP", "TSS", "turbidity")

#Identify the location of the files

data_location <- here("data")

# The following function reads the water quality files for individual parameters. 

read_csv_files <- function(folder_name, name_list) {
  data_list <- list()
  for (name in name_list) {
    file_path <- file.path(folder_name, paste0(name, ".csv"))
    data <- read.csv(file_path, header = TRUE)
    # Rename the problematic column
    names(data)[names(data) == "X...StationID"] <- "StationID"
    data_list[[name]] <- data
  }
  return(data_list)
}

# Apply the function to read in the data files
data_list <- read_csv_files(data_location, param_list)

# Add the microcystin data to the data_list
data_list$Microcystin <- Microcystin

# Read the lake stations key file and use it to update all the parameter-specific
# data frames with information on waterbody, station category, etc.

lake_stations_key <- read.csv(here("data", "lake_stations_key.csv"))

# Add station CW-207A to the station key

# Find the row with StationID "CW-207"
row_to_duplicate <- lake_stations_key[lake_stations_key$StationID == "CW-207", ]
new_row <- row_to_duplicate
new_row$StationID <- "CW-207A"
new_row$Monitoring_Name <- "CW-207A"
lake_stations_key <- rbind(lake_stations_key, new_row)

# Add the station location information to the WQ data frames

data_list <- lapply(data_list, function(df) {
  left_join(df, lake_stations_key, by = "StationID")
})

# Identify station order upstream to downstream

ordered_station_ids <- lake_stations_key[order(lake_stations_key$Order2), "StationID"]

# Write a function to standardize date formats. This overwrites the ActivityStartDate variable.

standardize_date <- function(df) {
  df$ActivityStartDate <- mdy(df$ActivityStartDate)
  return(df)
}

data_list <- lapply(data_list, standardize_date)

# Assign months and seasons to all rows
process_date_vars <- function(df) {
  # Extract month from ActivityStartDate as an integer
  df$Month <- as.integer(format(df$ActivityStartDate, "%m"))
  
  # Assign calendar season based on month
  df$Season <- ifelse(df$Month %in% c(12, 1, 2), "Winter",
                      ifelse(df$Month %in% c(3, 4, 5), "Spring",
                             ifelse(df$Month %in% c(6, 7, 8), "Summer", "Fall")))
  
  # Assign Season2 based on month
  df$Season2 <- ifelse(df$Month %in% c(4, 5, 6, 7, 8, 9, 10), "Apr-Oct", "Nov-Mar")
  
  return(df)
}

data_list <- lapply(data_list, process_date_vars)

# Rename units variable to simply "Units".

data_list <- lapply(data_list, function(df) {
  rename(df, Units = names(df)[4])
})

# Rename parameter variable within the data frames so that the data frame and the
# parameter have the exact same name. Consistently capitalize.

names(data_list)[1] <- "Alkalinity"
names(data_list)[2] <- "Ammonia_N"
names(data_list)[3] <- "Chla"
names(data_list)[6] <- "Org_Carbon"
names(data_list)[8] <- "Nitrate_N"
names(data_list)[9] <- "pH"
names(data_list)[10] <- "Secchi"
names(data_list)[16] <- "Turbidity"

data_list$Chla <- rename(data_list$Chla, Chla  = chla)
data_list$Ammonia_N <- rename(data_list$Ammonia_N, Ammonia_N = Ammonia)
data_list$BOD5 <- rename(data_list$BOD5, BOD5 = BOD)
data_list$DO <- rename(data_list$DO, DO = dissolved_oxygen)
data_list$Org_Carbon <- rename(data_list$Org_Carbon, Org_Carbon = organic_carbon)
data_list$Hardness <- rename(data_list$Hardness, Hardness = Hardness_Ca_Mg)
data_list$Nitrate_N <- rename(data_list$Nitrate_N, Nitrate_N = NitrateNitrite)
data_list$Secchi <- rename(data_list$Secchi, Secchi = secchi)
data_list$Temp <- rename(data_list$Temp, Temp = temperature)
data_list$TN <- rename(data_list$TN, TN = Total_Nitrogen)
data_list$TP <- rename(data_list$TP, TP = Total_phosphorus)

# Capitalize "Depth_m" for where necessary

data_list$Ammonia_N <- rename(data_list$Ammonia_N, Depth_m = depth_m)
data_list$Chla <- rename(data_list$Chla, Depth_m = depth_m)
data_list$DO <- rename(data_list$DO, Depth_m = depth_m)

# Add column of Depth_m to the Secchi table for consistency with other tables.
# Set the value to 0 only so that scripts that screen out deep data will not
# filter the Secchi data

data_list$Secchi <- add_column(data_list$Secchi, Depth_m = 0, .after = 4)

# list2env(data_list, envir = .GlobalEnv)     #This would extract the data frames from data_list

# Write the data_list to a *.rds file for referencing within R Markdown

saveRDS(data_list, file = here("data", "data_list.rds"))

# -----------------------------------------------

# Import the algal taxa data from 2017

algal_taxa <- read.csv(here("data", "algal_taxa.csv"), header = TRUE)

# Do some variable renaming for consistency with water quality files

algal_taxa <- rename(algal_taxa, StationID = Site)

algal_taxa <- rename(algal_taxa, ActivityStartDate = Date)

algal_taxa$StationID <- gsub("CW-16F", "CW-016F", algal_taxa$StationID)

algal_taxa <- rename(algal_taxa, Algal_Group = ALGALGROUP)

algal_taxa$Algal_Group <- gsub("Blue-Green_Algae", "Cyanobacteria", algal_taxa$Algal_Group, ignore.case = TRUE)

# Add locational information from lake_station_key

algal_taxa <- left_join(algal_taxa, lake_stations_key, by = "StationID")

# Process date and add month/season information

algal_taxa$ActivityStartDate <- mdy(algal_taxa$ActivityStartDate)

algal_taxa$Month <- as.integer(format(algal_taxa$ActivityStartDate, "%m"))

algal_taxa$Season <- ifelse(algal_taxa$Month %in% c(12, 1, 2), "Winter",
                    ifelse(algal_taxa$Month %in% c(3, 4, 5), "Spring",
                           ifelse(algal_taxa$Month %in% c(6, 7, 8), "Summer", "Fall")))

algal_taxa$Season2 <- ifelse(algal_taxa$Month %in% c(4, 5, 6, 7, 8, 9, 10), "Apr-Oct", "Nov-Mar")

# Calculate the percent biovolume of each algal group, add to the data frame

algal_taxa <- algal_taxa %>%
  group_by(StationID, ActivityStartDate) %>%
  mutate(Total_Biovolume = sum(Biovolume)) %>%
  ungroup() %>%
  mutate(Perc_Biovol = (Biovolume / Total_Biovolume) * 100) %>%
  select(-Total_Biovolume)

# Write algal_taxa to an rds file for referencing within R Markdown

saveRDS(algal_taxa, file = here("data", "algal_taxa.rds"))

# Function to allow printing with caption in R Markdown

print_with_caption <- function(plot, caption) {
  print(plot)
  cat(paste0("<p style='text-align: left;'>", caption, "</p>"))
}

