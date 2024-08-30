# This script makes tables and graphics with the 2017 algal taxa data
# The data are imported using the data_import.R script. There are only four stations
# with data and each was sampled five times in 2017 between late June and mid Oct,
# for a total of 20 unique combinations of StationID and date. The stations are as
# follows, upstream to downstream:

# CW-016F, Fishing Creek Lake, upper reservoir
# CW-057, Fishing Creek Lake, near dam
# CW-207A, Lake Wateree, mid reservoir
# CL-089, Lake Wateree, near dam

# Write function to make stacked bar charts of either cell count or biovolume
# User enters station ID and specifies either count or biovolume

stacked_algae <- function(station_id = "CW-016F", metric = "biovolume", start_date = NULL, end_date = NULL) {
  # Validate metric input
  if (!metric %in% c("count", "biovolume")) {
    stop("Metric must be either 'count' or 'biovolume'")
  }
  
  # Filter data
  filtered_data <- algal_taxa %>%
    filter(StationID == station_id)
  
  # Set y variable and y-axis label based on metric
  if (metric == "count") {
    y_var <- quo(Cells)
    y_label <- "Cell Count (cells per mL)"
  } else {
    y_var <- quo(Biovolume)
    y_label <- "Biovolume (um3 per mL)"
  }
  
  # Create plot
  p <- ggplot(filtered_data, aes(x = ActivityStartDate, y = !!y_var, fill = Algal_Group)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(
      title = paste("Algal Taxa at Station", station_id),
      x = "Date in 2017", 
      y = y_label, 
      fill = "Algal Group"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)  # Center the title
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme(aspect.ratio = 1/2)  # This makes the plot 3 times as wide as it is high
  
  # Add date range if provided
  if (!is.null(start_date) && !is.null(end_date)) {
    p <- p + scale_x_date(limits = c(as.Date(start_date), as.Date(end_date)))
  }
  
  return(p)
}

# This function plots the chla values for the same dates that have algal taxa
# data. Intended to allow one to plot the chla data above the algal taxa data.

plot_chla <- function(StationID, start_date = NULL, end_date = NULL) {
  # Get unique ActivityStartDate values for the given StationID in algal_taxa
  dates <- unique(algal_taxa$ActivityStartDate[algal_taxa$StationID == StationID])
  
  # Filter data_list$Chla for the given StationID, Depth_m <= 1, and matching dates
  chla_data <- data_list$Chla %>%
    filter(StationID == !!StationID,
           Depth_m <= 1,
           ActivityStartDate %in% dates)
  
  # Apply date range filter if provided
  if (!is.null(start_date) && !is.null(end_date)) {
    chla_data <- chla_data %>%
      filter(ActivityStartDate >= as.Date(start_date) & ActivityStartDate <= as.Date(end_date))
  }
  
  # Create the line plot
  p <- ggplot(chla_data, aes(x = ActivityStartDate, y = Chla)) +
    geom_point() +
    labs(title = paste("Chl-a at Station", StationID, ", <= 1 m"),
         x = "Date in 2017",
         y = "Chlorophyll-a (ug/L)") +
    theme_minimal() +
    theme(
      aspect.ratio = 1/5,  # This makes the plot 3 times as wide as it is high
      plot.title = element_text(hjust = 0.5)  # This centers the title
    )
  # Adjust x-axis limits if date range is provided
  if (!is.null(start_date) && !is.null(end_date)) {
    p <- p + scale_x_date(limits = c(as.Date(start_date), as.Date(end_date)))
  }
  
  return(p)
}

# Apply the stacked bar chart and chla functions to the four stations

# Station CW-016F

count_plot_16F <- stacked_algae(station_id = "CW-016F", metric = "count", start_date = "2017-06-01", end_date = "2017-10-31") 
biovolume_plot_16F <- stacked_algae(station_id = "CW-016F", metric = "biovolume", start_date = "2017-06-01", end_date = "2017-10-31")
chla_plot_16F <- plot_chla("CW-016F", start_date = "2017-06-01", end_date = "2017-10-31")

combined_16F_count <- (chla_plot_16F / count_plot_16F) 

combined_16F_biovol <- (chla_plot_16F / biovolume_plot_16F) 

combined_16F_count
combined_16F_biovol

# Station CW-057

count_plot_57 <- stacked_algae(station_id = "CW-057", metric = "count", start_date = "2017-06-01", end_date = "2017-10-31") 
biovolume_plot_57 <- stacked_algae(station_id = "CW-057", metric = "biovolume", start_date = "2017-06-01", end_date = "2017-10-31")
chla_plot_57 <- plot_chla("CW-057", start_date = "2017-06-01", end_date = "2017-10-31")

combined_57_count <- (chla_plot_57 / count_plot_57) 

combined_57_biovol <- (chla_plot_57 / biovolume_plot_57) 

combined_57_count
combined_57_biovol

# Station CW-207

count_plot_207 <- stacked_algae(station_id = "CW-207A", metric = "count", start_date = "2017-06-01", end_date = "2017-10-31") 
biovolume_plot_207 <- stacked_algae(station_id = "CW-207A", metric = "biovolume", start_date = "2017-06-01", end_date = "2017-10-31")
chla_plot_207 <- plot_chla("CW-207A", start_date = "2017-06-01", end_date = "2017-10-31")

combined_207_count <- (chla_plot_207 / count_plot_207) 

combined_207_biovol <- (chla_plot_207 / biovolume_plot_207) 

combined_207_count
combined_207_biovol

# Station CL-89

count_plot_89 <- stacked_algae(station_id = "CL-089", metric = "count", start_date = "2017-06-01", end_date = "2017-10-31") 
biovolume_plot_89 <- stacked_algae(station_id = "CL-089", metric = "biovolume", start_date = "2017-06-01", end_date = "2017-10-31")
chla_plot_89 <- plot_chla("CL-089", start_date = "2017-06-01", end_date = "2017-10-31")

combined_89_count <- (chla_plot_89 / count_plot_89) 

combined_89_biovol <- (chla_plot_89 / biovolume_plot_89) 

combined_89_count
combined_89_biovol

# The next function plots on the blue-green algal counts and also includes a line
# with WHO-based risk thresholds

cyano <- function(station_id = "CW-016F", start_date = NULL, end_date = NULL, threshold1 = 20000, threshold2 = 100000) {
  # Filter data
  filtered_data <- algal_taxa %>%
    filter(StationID == station_id, Algal_Group == "Cyanobacteria")
  
  # Set y variable and y-axis label based on metric
  y_var <- quo(Cells)
  y_label <- "Cyanobacteria Cell Count (cells per mL)"
  
  # Calculate the max y value
  max_y <- max(filtered_data$Cells, 100000)
  
  # Create plot
  p <- ggplot(filtered_data, aes(x = ActivityStartDate, y = !!y_var)) +
    geom_col() +
    geom_hline(aes(yintercept = threshold1), color = "red", linetype = "dashed") +
    annotate("text", x = min(filtered_data$ActivityStartDate), y = threshold1,
             label = "WHO moderate risk threshold",
             vjust = -0.5, hjust = 0, color = "red") +
    geom_hline(aes(yintercept = threshold2), color = "red", linetype = "dashed") +
    annotate("text", x = min(filtered_data$ActivityStartDate), y = threshold2,
             label = "WHO high risk threshold",
             vjust = -0.5, hjust = 0, color = "red") +
    theme_minimal() +
    labs(
      title = paste("Cyanobacteria Counts at Station", station_id),
      x = "Date in 2017", 
      y = y_label
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)  # Center the title
    ) +
    scale_y_continuous(labels = scales::comma) +
    expand_limits(y = max_y * 1.1) +
    theme(aspect.ratio = 1/2)  # This makes the plot 3 times as wide as it is high
  
  # Add date range if provided
  if (!is.null(start_date) && !is.null(end_date)) {
    p <- p + scale_x_date(limits = c(as.Date(start_date), as.Date(end_date)))
  }
  
  return(p)
}

# Apply the function to the stations

# Station CW-016F

cyano_16F <- cyano(station_id = "CW-016F", start_date = "2017-06-01", end_date = "2017-10-31")
cyano_combined_16F <- (chla_plot_16F / cyano_16F)
cyano_combined_16F

cyano_57 <- cyano(station_id = "CW-057", start_date = "2017-06-01", end_date = "2017-10-31")
cyano_combined_57 <- (chla_plot_57 / cyano_57)
cyano_combined_57

cyano_207 <- cyano(station_id = "CW-207A", start_date = "2017-06-01", end_date = "2017-10-31")
cyano_combined_207 <- (chla_plot_207 / cyano_207)
cyano_combined_207

cyano_89 <- cyano(station_id = "CL-089", start_date = "2017-06-01", end_date = "2017-10-31")
cyano_combined_89 <- (chla_plot_89 / cyano_89)
cyano_combined_89

# Next is a scatterplot of cyanobacteria count vs chla by reservoir

# Fishing Creek Reservoir Version

# Step 1: Process data_list$Chla
chla_mean <- data_list$Chla %>%
  filter(Waterbody == "FISHING CREEK RESERVOIR", Depth_m <= 1) %>%
  group_by(StationID, ActivityStartDate) %>%
  summarize(mean_Chla = mean(Chla, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Process algal_taxa
algal_summarized <- algal_taxa %>%
  filter(Waterbody == "FISHING CREEK RESERVOIR", Algal_Group == "Cyanobacteria") %>%
  group_by(StationID, ActivityStartDate) %>%
  summarize(total_Cells = sum(Cells, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Merge the datasets
merged_data <- inner_join(chla_mean, algal_summarized, 
                          by = c("StationID", "ActivityStartDate"))

# Step 4: Create the scatterplot with different colors for each StationID
ggplot(merged_data, aes(x = mean_Chla, y = total_Cells, color = StationID)) +
  geom_point() +
  geom_hline(yintercept = 20000, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 100000, linetype = "dashed", color = "blue") +
  annotate("text", x = max(merged_data$mean_Chla), y = 20000, 
           label = "WHO moderate risk threshold", hjust = 1, vjust = -0.5, color = "red") +
  annotate("text", x = max(merged_data$mean_Chla), y = 100000, 
           label = "WHO high risk threshold", hjust = 1, vjust = -0.5, color = "blue") +
  labs(x = "Mean Chlorophyll-a", y = "Total Cyanobacteria Cells",
       title = "Total Cyanobacteria Cells vs Mean Chlorophyll-a in Fishing Creek Reservoir",
       color = "Station ID") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Lake Wateree Version

# Step 1: Process data_list$Chla
chla_mean <- data_list$Chla %>%
  filter(Waterbody == "LAKE WATEREE", Depth_m <= 1) %>%
  group_by(StationID, ActivityStartDate) %>%
  summarize(mean_Chla = mean(Chla, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Process algal_taxa
algal_summarized <- algal_taxa %>%
  filter(Waterbody == "LAKE WATEREE", Algal_Group == "Cyanobacteria") %>%
  group_by(StationID, ActivityStartDate) %>%
  summarize(total_Cells = sum(Cells, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Merge the datasets
merged_data <- inner_join(chla_mean, algal_summarized, 
                          by = c("StationID", "ActivityStartDate"))

# Step 4: Create the scatterplot with different colors for each StationID
ggplot(merged_data, aes(x = mean_Chla, y = total_Cells, color = StationID)) +
  geom_point() +
  geom_hline(yintercept = 20000, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 100000, linetype = "dashed", color = "blue") +
  annotate("text", x = max(merged_data$mean_Chla), y = 20000, 
           label = "WHO moderate risk threshold", hjust = 1, vjust = -0.5, color = "red") +
  annotate("text", x = max(merged_data$mean_Chla), y = 100000, 
           label = "WHO high risk threshold", hjust = 1, vjust = -0.5, color = "blue") +
  labs(x = "Mean Chlorophyll-a", y = "Total Cyanobacteria Cells",
       title = "Total Cyanobacteria Cells vs Mean Chlorophyll-a in Lake Wateree",
       color = "Station ID") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Next is scatterplots of % cyanobacteria biovolume vs chla

# Fishing Creek Reservoir Version

# Step 1: Process data_list$Chla
chla_mean <- data_list$Chla %>%
  filter(Waterbody == "FISHING CREEK RESERVOIR", Depth_m <= 1) %>%
  group_by(StationID, ActivityStartDate) %>%
  summarize(mean_Chla = mean(Chla, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Process algal_taxa
algal_summarized <- algal_taxa %>%
  filter(Waterbody == "FISHING CREEK RESERVOIR", Algal_Group == "Cyanobacteria") %>%
  group_by(StationID, ActivityStartDate) %>%
  summarize(Sum_Percent_Biovol = sum(Perc_Biovol, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Merge the datasets
merged_data <- inner_join(chla_mean, algal_summarized, 
                          by = c("StationID", "ActivityStartDate"))

# Step 4: Create the scatterplot with different colors for each StationID
ggplot(merged_data, aes(x = mean_Chla, y = Sum_Percent_Biovol, color = StationID)) +
  geom_point() +
  labs(x = "Mean Chlorophyll-a", y = "Percent Cyanobacteria Biovolume",
       title = "Percent Cyanobacteria Biovolume vs Mean Chlorophyll-a in Fishing Creek Reservoir",
       color = "Station ID") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Lake Wateree Version

# Step 1: Process data_list$Chla
chla_mean <- data_list$Chla %>%
  filter(Waterbody == "LAKE WATEREE", Depth_m <= 1) %>%
  group_by(StationID, ActivityStartDate) %>%
  summarize(mean_Chla = mean(Chla, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Process algal_taxa
algal_summarized <- algal_taxa %>%
  filter(Waterbody == "LAKE WATEREE", Algal_Group == "Cyanobacteria") %>%
  group_by(StationID, ActivityStartDate) %>%
  summarize(Sum_Percent_Biovol = sum(Perc_Biovol, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Merge the datasets
merged_data <- inner_join(chla_mean, algal_summarized, 
                          by = c("StationID", "ActivityStartDate"))

# Step 4: Create the scatterplot with different colors for each StationID
ggplot(merged_data, aes(x = mean_Chla, y = Sum_Percent_Biovol, color = StationID)) +
  geom_point() +
  labs(x = "Mean Chlorophyll-a", y = "Percent Cyanobacteria Biovolume",
       title = "Percent Cyanobacteria Biovolume vs Mean Chlorophyll-a in Lake Wateree",
       color = "Station ID") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

