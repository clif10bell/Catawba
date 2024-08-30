# This script calculates the annual seasonal geometric mean of a paramater based on a
# reservoir identified. It only uses Apr-Oct data, <= 1 m, and stations with more
# than a user-specified number of data (default = 5 per year). It then plots the
# 90th percentile pH against those geoemtric means.

perc_vs_geomean <- function(parameter1 = "Chla", parameter2 = "pH", 
                            reservoir = "LAKE WATEREE", min_n1 = 5, min_n2 = 10,
                            regression = NULL) {
  
  # Find the relevant data frames
  df1 <- data_list[[parameter1]]
  df2 <- data_list[[parameter2]]
  
  # Function to process each data frame
  process_df <- function(df, param, min_n) {
    df %>%
      filter(Depth_m <= 1, Season2 == "Apr-Oct", Waterbody == reservoir) %>%
      mutate(Year = year(ActivityStartDate)) %>%
      group_by(StationID, Year) %>%
      filter(n() >= min_n) %>%
      summarise(
        Value = if(param == parameter1) {
          exp(mean(log(!!sym(param)), na.rm = TRUE))  # Geometric mean
        } else {
          quantile(!!sym(param), 0.9, na.rm = TRUE)  # 90th percentile
        },
        .groups = "drop"
      )
  }
  
  # Process both data frames
  results1 <- process_df(df1, parameter1, min_n1)
  results2 <- process_df(df2, parameter2, min_n2)
  
  # Combine results
  combined_results <- inner_join(results1, results2, by = c("StationID", "Year"),
                                 suffix = c("_param1", "_param2"))
  
  # Get station order
  station_order <- df1 %>%
    filter(StationID %in% unique(combined_results$StationID)) %>%
    arrange(Order2) %>%
    pull(StationID) %>%
    unique()
  
  # Create the base plot
  p <- ggplot(combined_results, aes(x = Value_param1, y = Value_param2, color = factor(StationID, levels = station_order))) +
    geom_point() +
    scale_color_discrete(name = "StationID") +
    labs(x = paste("Geometric Mean of", parameter1),
         y = paste("90th Percentile of", parameter2),
         title = paste("Relationship between", parameter1, "and", parameter2),
         subtitle = paste("Reservoir:", reservoir)) +
    theme_minimal()
  
  # Add regression line and equation if specified
  if (!is.null(regression)) {
    if (regression == "linear") {
      model <- lm(Value_param2 ~ Value_param1, data = combined_results)
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "black", 
                           linetype = "dashed", alpha = 0.5)
      
      # Get R-squared and coefficients
      r_squared <- summary(model)$r.squared
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      
      # Add equation and R-squared to the plot
      eq <- paste0("y = ", round(slope, 3), "x + ", round(intercept, 3))
      r2 <- paste0("R² = ", round(r_squared, 3))
      p <- p + annotate("text", x = Inf, y = Inf, label = paste(eq, r2, sep = "\n"), 
                        hjust = 1, vjust = 1, size = 3)
      
    } else if (regression == "exponential") {
      # Fit exponential model
      model <- nls(Value_param2 ~ a * exp(b * Value_param1), 
                   data = combined_results, 
                   start = list(a = 1, b = 0.1))
      
      # Add fitted curve
      p <- p + geom_line(aes(y = predict(model)), color = "black", 
                         linetype = "dashed", alpha = 0.5)
      
      # Get R-squared and coefficients
      fitted_values <- predict(model)
      residuals <- combined_results$Value_param2 - fitted_values
      r_squared <- 1 - sum(residuals^2) / sum((combined_results$Value_param2 - mean(combined_results$Value_param2))^2)
      coefs <- coef(model)
      
      # Add equation and R-squared to the plot
      eq <- paste0("y = ", round(coefs["a"], 3), " * exp(", round(coefs["b"], 3), "x)")
      r2 <- paste0("R² = ", round(r_squared, 3))
      p <- p + annotate("text", x = Inf, y = Inf, label = paste(eq, r2, sep = "\n"), 
                        hjust = 1, vjust = 1, size = 3)
    }
  }
  
  return(p)
}

perc_vs_geomean(parameter1 = "Chla", parameter2 = "pH", 
                reservoir = "FISHING CREEK RESERVOIR", min_n1 = 5, min_n2 = 10)


