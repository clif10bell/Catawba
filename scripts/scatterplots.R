# This script creates scatterplots, and optionally displays a regression line.
# User enters a parameters and station list (which will often be just one station).
# Data are matched by StationID, date, and time to within one hour.

# In the first function, only Depth_m <= 1 are used.

scatter1_old <- function(xparam = "Chla", yparam = "DO", 
                     station_list = c("CW-231", "CL-089", "CW-016F", "LCR-01"),
                     regression = "none") {
  
  df_x <- data_list[[xparam]]
  df_y <- data_list[[yparam]]
  
  merged_df <- inner_join(df_x, df_y, by = c("StationID", "ActivityStartDate"), relationship = "many-to-many")
  
  filtered_data <- merged_df %>%
    filter(Depth_m.x <= 1, Depth_m.y <= 1, StationID %in% station_list)
  
  p <- ggplot(filtered_data, aes(x = !!sym(xparam), y = !!sym(yparam), color = StationID)) +
    geom_point() +
    labs(x = paste(xparam, "(", df_x[1,4], ")"), 
         y = paste(yparam, "(", df_y[1,4], ")"), 
         title = " ",
         color = "Station ID") +
    theme_minimal()
  
  if(regression != "none") {
    if(regression == "linear") {
      # Fit linear model
      model <- lm(paste(yparam, "~", xparam), data = filtered_data)
      eq <- function(x) coef(model)[1] + coef(model)[2] * x
      eq_text <- paste0("y = ", round(coef(model)[1], 3), 
                        " + ", round(coef(model)[2], 3), "x")
    } else if(regression == "exponential") {
      # Fit exponential model
      model <- nls(paste(yparam, "~ a * exp(b *", xparam, ")"), 
                   data = filtered_data, 
                   start = list(a = 1, b = 0.1))
      eq <- function(x) coef(model)[1] * exp(coef(model)[2] * x)
      eq_text <- paste0("y = ", round(coef(model)[1], 3), 
                        " * exp(", round(coef(model)[2], 3), "x)")
    } else {
      stop("Invalid regression type. Choose 'none', 'linear', or 'exponential'.")
    }
    
    # Get R-squared
    r2 <- round(1 - sum(residuals(model)^2) / sum((filtered_data[[yparam]] - mean(filtered_data[[yparam]]))^2), 3)
    
    # Add regression line and equation to plot
    p <- p +
      stat_function(fun = eq, geom = "line", color = "gray50", 
                    linetype = "dashed", size = 0.5, alpha = 0.7) +
      annotate("text", x = min(filtered_data[[xparam]]), 
               y = max(filtered_data[[yparam]]),
               label = paste(eq_text, "\nR² =", r2),
               hjust = 0, vjust = 1, size = 4, color = "gray20")
  }
  
  return(p)
}

#----------------------

scatter1 <- function(xparam = "Chla", yparam = "DO", 
                     station_list = c("CW-231", "CL-089", "CW-016F", "LCR-01"),
                     regression = "none") {
  
  df_x <- data_list[[xparam]]
  df_y <- data_list[[yparam]]
  
  merged_df <- inner_join(df_x, df_y, by = c("StationID", "ActivityStartDate"), relationship = "many-to-many")
  
  filtered_data <- merged_df %>%
    filter(Depth_m.x <= 1, Depth_m.y <= 1, StationID %in% station_list) %>%
    filter(!!sym(xparam) != 0, !!sym(yparam) != 0)  # Exclude zero values
  
  p <- ggplot(filtered_data, aes(x = !!sym(xparam), y = !!sym(yparam), color = StationID)) +
    geom_point() +
    labs(x = paste(xparam, "(", df_x[1,4], ")"), 
         y = paste(yparam, "(", df_y[1,4], ")"), 
         title = " ",
         color = "Station ID") +
    theme_minimal()
  
  if(regression != "none") {
    if(regression == "linear") {
      # Fit linear model
      model <- lm(paste(yparam, "~", xparam), data = filtered_data)
      eq <- function(x) coef(model)[1] + coef(model)[2] * x
      eq_text <- paste0("y = ", round(coef(model)[1], 3), 
                        " + ", round(coef(model)[2], 3), "x")
    } else if(regression == "exponential") {
      # Fit exponential model
      model <- nls(paste(yparam, "~ a * exp(b *", xparam, ")"), 
                   data = filtered_data, 
                   start = list(a = 1, b = 0.1))
      eq <- function(x) coef(model)[1] * exp(coef(model)[2] * x)
      eq_text <- paste0("y = ", round(coef(model)[1], 3), 
                        " * exp(", round(coef(model)[2], 3), "x)")
    } else {
      stop("Invalid regression type. Choose 'none', 'linear', or 'exponential'.")
    }
    
    # Get R-squared
    r2 <- round(1 - sum(residuals(model)^2) / sum((filtered_data[[yparam]] - mean(filtered_data[[yparam]]))^2), 3)
    
    # Add regression line and equation to plot
    p <- p +
      stat_function(fun = eq, geom = "line", color = "gray50", 
                    linetype = "dashed", size = 0.5, alpha = 0.7) +
      annotate("text", x = min(filtered_data[[xparam]]), 
               y = max(filtered_data[[yparam]]),
               label = paste(eq_text, "\nR² =", r2),
               hjust = 0, vjust = 1, size = 4, color = "gray20")
  }
  
  return(p)
}


scatter1("Chla", "DO", c("CW-016F", "LCR-04", "CW-057"), regression = "exponential")   # Fishing Creek Res. Stations
scatter1("Chla", "DO", c("RL-05414", "RL-08062", "RL-06429"), regression = "linear")   # Great Falls Res. Stations
scatter1("Chla", "DO", c("CW-174", "CW-033"),  regression = "linear")              # Cedar Creek Res. Stations
scatter1("Chla", "DO", c("CW-231", "CW-208", "CW-207B", "CL-089"),  regression = "linear")   # Lake Wateree Stations

scatter1("Chla", "pH", c("CW-016F", "LCR-04", "CW-057"), regression = "linear")   # Fishing Creek Res. Stations
scatter1("Chla", "pH", c("RL-05414", "RL-08062", "RL-06429"), regression = "linear")   # Great Falls Res. Stations
scatter1("Chla", "pH", c("CW-174", "CW-033"),  regression = "linear")              # Cedar Creek Res. Stations
scatter1("Chla", "pH", c("CW-231", "CW-208", "CW-207B", "CL-089"),  regression = "linear")   # Lake Wateree Stations

scatter1("Chla", "Secchi", c("CW-016F", "LCR-04", "CW-057"), regression = "linear")   # Fishing Creek Res. Stations
# scatter1("Chla", "Secchi", c("RL-05414", "RL-08062", "RL-06429"), regression = "linear")   # Great Falls Res. Stations
scatter1("Chla", "Secchi", c("CW-174", "CW-033"),  regression = "exponential")              # Cedar Creek Res. Stations
scatter1("Chla", "Secchi", c("CW-231", "CW-208", "CW-207B", "CL-089"),  regression = "linear")   # Lake Wateree Stations

scatter1("Chla", "Microcystin", c("CW-016F", "CW-057", "LCR-04"), regression = "linear")  # Fishing Creek Res. Stations
scatter1("Chla", "Microcystin", c("CW-033", "CW-174"), regression = "linear")  # Cedar Creek Res. Stations
scatter1("Chla", "Microcystin", c("CL-089", "CW-207", "CW-207B", "CW-208", "CW-231", "CW-711", "CW-712", "LCR-02", "LCR-03"), regression = "linear")  # Lake Wateree Stations

#======================================

# The next scatterplot function ("scatterplot2") plots DO vs depth. The user enters
# the station names.

scatter2 <- function(station_list = c("CW-231", "CL-089", "CW-016F", "LCR-01"),
                     xparam = "DO") 
  {
  
  df <- data_list[[xparam]]

  filtered_data <- df %>%
    filter(StationID %in% station_list)
  
#  print(tail(filtered_data))
  
  p <- ggplot(filtered_data, aes(x = !!sym(xparam), y = Depth_m, color = StationID)) +
    geom_point() +
    labs(x = paste(xparam, "(", df[1,4], ")"), 
         y = "Depth (m)", 
         title = " ",
         color = "Station ID") +
    theme_minimal() +
    scale_y_reverse()
  
  
  return(p)
}

scatter2(station_list = c("CW-016F", "LCR-04", "CW-057"))   #Fishing Creek Res.
scatter2(station_list = c("RL-05414", "RL-08062", "RL-06429"))    #Great Falls Res.
scatter2(station_list = c("CW-174", "CW-033"))     #Cedar Creek Res.
scatter2(station_list = c("CW-231", "CW-208", "CW-207B", "CL-089"))   #Lake Wateree
