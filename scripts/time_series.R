# Time series function; user enters parameter and station list. Creates time series plots in
# panels.One must first use the data_import.R script to import and process the data.

time_series <- function(param, station_list) {
  
  df <- data_list[[param]]
  
  if (is.null(df)) {
    return(paste("No data frame named '", param, "' found in data_list.", sep = ""))
  }
  
  # Check if the column exists in the data frame
  if (!(param %in% names(df))) {
    return(paste("No column named '", param, "' found in the data frame.", sep = ""))
  } 
  
  plot_data <- df %>%
    filter(StationID %in% station_list) %>%
    select(StationID, ActivityStartDate, !!sym(param))
           
  ggplot(plot_data, aes(x = ActivityStartDate, y = !!sym(param))) +
    geom_point() +
    facet_wrap(~ StationID, scales = "free_y", ncol = 1) +
    labs(title = paste("Time Series Plot of", param),
         x = "Date",
         y = paste(param, "(", data_list[[param]][1,4], ")")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

time_series("Chla", c("CW-016F", "CW-057"))  #Fishing Creek Res.
time_series("Chla", c("CW-174", "CW-033"))  #Cedar Creek Res.
time_series("Chla", c("CW-231", "CW-208", "CL-089"))  #Lake Wateree

time_series("DO", c("CW-231", "CW-208", "CL-089"))  #Lake Wateree

time_series("DO", c("CL-089"))  #Lake Wateree

