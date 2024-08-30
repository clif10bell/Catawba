# The purpose of this script is to calculate annual geometric means of a parameter.
# The function takes as input a parameter, a water body, and a minimum number of samples for calculating
# the geometric mean. The output of the function is a table of geometric means
# by Stations ID and year. It also calculate the min-max-median geomeans and their
# standard deviation. Samples ares filtered by depth and for the growing season.

annual_geomeans <- function(parameter = "Chla", waterbody = "LAKE WATEREE", n = 5) {
  # Find the correct dataframe in the list
  df <- data_list[[parameter]]
  
  # Filter and process the data
  processed_df <- df %>%
    filter(Waterbody == waterbody,
           Depth_m == 0.3,
           Season2 == "Apr-Oct") %>%
    mutate(Year = year(ActivityStartDate)) %>%
    group_by(Year, StationID) %>%
    summarize(
      Count = sum(!is.na(!!sym(parameter))),
      GeomMean = if_else(Count >= n, exp(mean(log(!!sym(parameter)), na.rm = TRUE)), NA_real_),
      Order2 = first(Order2),  # Ensure Order2 is carried through
      .groups = "drop"
    )
  
  # Calculate additional statistics with NA handling
  station_stats <- processed_df %>%
    group_by(StationID) %>%
    summarize(
      YearsWithData = sum(Count >= n, na.rm = TRUE),
      MinGeomMean = if(all(is.na(GeomMean))) NA_real_ else min(GeomMean, na.rm = TRUE),
      MedianGeomMean = median(GeomMean, na.rm = TRUE),
      MaxGeomMean = if(all(is.na(GeomMean))) NA_real_ else max(GeomMean, na.rm = TRUE),
      SDGeomMean = sd(GeomMean, na.rm = TRUE),
      Order2 = first(Order2)
    ) %>%
    arrange(Order2)
  
  # Pivot the main results and round to one decimal place
  result_table <- processed_df %>%
    select(Year, StationID, GeomMean) %>%
    pivot_wider(names_from = StationID, values_from = GeomMean) %>%
    mutate(across(-Year, ~round(., 1))) %>%
    arrange(Year)
  
  # Reorder columns based on Order2
  station_order <- station_stats %>% arrange(Order2) %>% pull(StationID)
  result_table <- result_table %>%
    select(Year, all_of(station_order))
  
  # Create summary rows with formatted values
  summary_rows <- tibble(
    Year = c("Years with ≥ 5 values", "Min Geom Mean", "Median Geom Mean", 
             "Max Geom Mean", "SD Geom Mean")
  )
  
  for (station in station_order) {
    summary_rows[[station]] <- c(
      as.integer(station_stats$YearsWithData[station_stats$StationID == station]),
      round(station_stats$MinGeomMean[station_stats$StationID == station], 1),
      round(station_stats$MedianGeomMean[station_stats$StationID == station], 1),
      round(station_stats$MaxGeomMean[station_stats$StationID == station], 1),
      round(station_stats$SDGeomMean[station_stats$StationID == station], 1)
    )
  }
  
  # Combine result_table and summary_rows
  final_table <- bind_rows(
    result_table %>% mutate(Year = as.character(Year)),
    summary_rows
  )
  
  # Create flextable
  ft <- flextable(final_table) %>%
    set_header_labels(Year = "Year") %>%
    theme_vanilla() %>%
    autofit()
  
  return(ft)
}

chla_table2 <- annual_geomeans(parameter = "Chla", waterbody = "LAKE WATEREE", n = 5)

chla_table2