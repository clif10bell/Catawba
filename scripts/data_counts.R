# This script tabulates the number of data for each parameter-year-station
# combination. Inputs are the parameter and the water body. It filters the data
# by depth (<= 1 m) and the growing season (Apr-Oct).

count_table <- function(parameter, waterbody = "LAKE WATEREE") {
  # Find the correct dataframe in the list
  df <- data_list[[parameter]]
  
  # Filter the data
  filtered_df <- df %>%
    filter(Waterbody == waterbody,
           Depth_m <= 1,
           Season2 == "Apr-Oct") %>%
    mutate(Year = as.integer(year(ActivityStartDate)))  # Ensure Year is integer
  
  # Get all unique years and sort them
  all_years <- sort(unique(filtered_df$Year))
  
  # Count non-NA values of the parameter
  result_table <- filtered_df %>%
    group_by(StationID, Year) %>%
    summarize(
      count = sum(!is.na(!!sym(parameter))),
      Order2 = first(Order2),  # Preserve Order2 for sorting
      .groups = "drop"
    ) %>%
    # Complete the dataset with all years, filling missing counts with 0
    complete(StationID, Year = all_years, fill = list(count = 0)) %>%
    # Now pivot with years as rows and stations as columns
    pivot_wider(id_cols = Year, names_from = StationID, values_from = count)
  
  # Get the order of stations based on Order2
  station_order <- filtered_df %>%
    group_by(StationID) %>%
    summarize(Order2 = first(Order2), .groups = "drop") %>%
    arrange(Order2) %>%
    pull(StationID)
  
  # Reorder the columns based on station_order
  result_table <- result_table %>%
    select(Year, all_of(station_order))
  
  # Create flextable
  ft <- flextable(result_table) %>%
    set_header_labels(Year = "Year") %>%
    theme_vanilla() %>%
    autofit()
  
  # Format the Year column to prevent commas
  ft <- ft %>%
    colformat_num(j = "Year", big.mark = "")
  
  return(ft)
}

# For Chla in LAKE WATEREE
chla_table <- count_table("Chla", waterbody = "FISHING CREEK RESERVOIR")

chla_table