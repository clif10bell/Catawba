# This script is for making water quality statistical summary tables. The user
# enters the parameter name and reservoir name.

summary_table <- function(param = "Chla", reservoir = "LAKE WATEREE", threshold = 5) {
  
  df <- data_list[[param]]
  
  result <- df %>%
    filter(Waterbody == reservoir,
           Season2 == "Apr-Oct",
           Depth_m <= 1) %>%
    group_by(StationID) %>%
    filter(n() > threshold) %>%
    summarise(
      Count = n(),
      Max = round(max(!!sym(param), na.rm = TRUE), 1),
      Perc_90th = round(quantile(!!sym(param), 0.90, na.rm = TRUE), 1),
      Perc_75th = round(quantile(!!sym(param), 0.75, na.rm = TRUE), 1),
      Arith_Mean = round(mean(!!sym(param), na.rm = TRUE), 1),
      Geo_Mean = round(exp(mean(log(!!sym(param)), na.rm = TRUE)), 1),
      Median = round(median(!!sym(param), na.rm = TRUE), 1),
      Perc_25th = round(quantile(!!sym(param), 0.25, na.rm = TRUE), 1),
      Perc_10th = round(quantile(!!sym(param), 0.10, na.rm = TRUE), 1),
      Min = round(min(!!sym(param), na.rm = TRUE), 1)
    ) %>%
    left_join(df %>% 
                select(StationID, Order2) %>% 
                distinct(),
              by = "StationID") %>%
    arrange(Order2) %>%
    select(-Order2)
  
  formatted_table <- result %>%
    mutate(across(where(is.numeric), ~ round(., 1))) %>%
    flextable() %>%
    set_caption(caption = paste("All values in", df[1,4], "except Count")) %>%
    theme_zebra() %>%
    autofit() %>%
    add_header_row(
      values = c("", rep(paste(param, "in", reservoir), 10)),
      colwidths = c(1, rep(1, 10))
    ) %>%
    align(align = "center", part = "all") %>%
    set_table_properties(width = 0.8, layout = "autofit") %>%
    fontsize(size = 10, part = "all") %>%
    border_outer()
  
  formatted_table
}

# summary_table("Chla", "FISHING CREEK RESERVOIR")
# summary_table("Chla", "GREAT FALLS RESERVOIR")
# summary_table("Chla", "CEDAR CREEK RESERVOIR")
# summary_table("Chla", "LAKE WATEREE")

# summary_table("DO", "FISHING CREEK RESERVOIR")
# summary_table("DO", "GREAT FALLS RESERVOIR")
# summary_table("DO", "CEDAR CREEK RESERVOIR")
# summary_table("DO", "LAKE WATEREE")

# summary_table("pH", "FISHING CREEK RESERVOIR")
# summary_table("pH", "GREAT FALLS RESERVOIR")
# summary_table("pH", "CEDAR CREEK RESERVOIR")
# summary_table("pH", "LAKE WATEREE")

# summary_table("Microcystin", "FISHING CREEK RESERVOIR")
# summary_table("Microcystin", "CEDAR CREEK RESERVOIR")
# summary_table("Microcystin", "LAKE WATEREE")
