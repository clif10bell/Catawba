# This script presents function for creating a table of the proportions of 
# water quality data that exceed a particular threshold such as a criterion. The
# user enters the parameter name, the threshold, and the reservoir name, and the
# table will display results for all stations in that reservoirs. Only data <= 1 m
# are used, but all seasons are used.

below_table <- function(param = "Chla", threshold = 5, reservoir="LAKE WATEREE") {
  
  df <- data_list[[param]]
  
  result <- df %>%
    filter(Waterbody == reservoir,
           Depth_m <= 1) %>%
    group_by(StationID) %>%
    filter(n() > 5) %>%
    summarise(
      Count = n(),
      Below_Threshold_Count = sum(!!sym(param) < threshold),
      Percentage_Below = round(mean(!!sym(param) < threshold) * 100, 1)  # Rounded to 1 decimal place
    ) %>%
    left_join(df %>% 
                select(StationID, Order2) %>% 
                distinct(),
              by = "StationID") %>%
    arrange(Order2) %>%
    select(-Order2)
  
  formatted_table <- result %>%
    flextable() %>%
    theme_zebra() %>%
    autofit() %>%
    fontsize(size = 10) %>%
    add_header_row(
      values = c("", paste(param, "in", reservoir, "Threshold =", sprintf("%.1f", threshold), df[1,4])),
      colwidths = c(1, 3)
    ) %>%
    align(align = "center", part = "all") %>%
    border_outer() %>%
    border_inner() %>%
    set_table_properties(layout = "autofit")
  
  formatted_table
}

above_table <- function(param = "pH", threshold = 9.0, reservoir="LAKE WATEREE") {
  
  df <- data_list[[param]]
  
  result <- df %>%
    filter(Waterbody == reservoir,
           Depth_m <= 1) %>%
    group_by(StationID) %>%
    filter(n() > 5) %>%
    summarise(
      Count = n(),
      Above_Threshold_Count = sum(!!sym(param) > threshold),
      Percentage_Above = round(mean(!!sym(param) > threshold) * 100, 1)  # Rounded to 1 decimal place
    ) %>%
    left_join(df %>% 
                select(StationID, Order2) %>% 
                distinct(),
              by = "StationID") %>%
    arrange(Order2) %>%
    select(-Order2)
  
  formatted_table <- result %>%
    flextable() %>%
    set_formatter_type(fmt_double = function(x) sprintf("%.1f", x)) %>%
    theme_zebra() %>%
    autofit() %>%
    add_header_row(
      values = c("", paste(param, "in", reservoir, "Threshold =", sprintf("%.1f", threshold), df[1,4])),
      colwidths = c(1, 3)
    ) %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 10) %>%
    border_outer() %>%
    border_inner() %>%
    set_table_properties(layout = "autofit")
  
  formatted_table
}

# below_table(param = "DO", threshold = 5, reservoir = "FISHING CREEK RESERVOIR")
# below_table(param = "DO", threshold = 5, reservoir = "GREAT FALLS RESERVOIR")
# below_table(param = "DO", threshold = 5, reservoir = "CEDAR CREEK RESERVOIR")
# below_table(param = "DO", threshold = 5, reservoir = "LAKE WATEREE")

# above_table(param = "pH", threshold = 8.5, reservoir = "FISHING CREEK RESERVOIR")
# above_table(param = "pH", threshold = 8.5, reservoir = "GREAT FALLS RESERVOIR")
# above_table(param = "pH", threshold = 8.5, reservoir = "CEDAR CREEK RESERVOIR")
# above_table(param = "pH", threshold = 8.5, reservoir = "LAKE WATEREE")

# above_table(param = "Microcystin", threshold = 0.3, reservoir = "FISHING CREEK RESERVOIR")
#above_table(param = "Microcystin", threshold = 0.3, reservoir = "CEDAR CREEK RESERVOIR")
# above_table(param = "Microcystin", threshold = 0.3, reservoir = "LAKE WATEREE")
