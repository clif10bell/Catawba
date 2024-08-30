# This script is for making water quality box plots, characterizing the
# existing water quality conditions of the Catawba River reservoirs. The function
# makes panel plots by station category, and they are sorted upstream to
# downstream. One must first use the data_import.R script to import and process
# the data. 

# The first boxplot function creates different panels for different lake station
# categories. The data are filters to use only Apr-Oct data and Depth_m <= 1)

boxplots1 <- function(param = "Chla", reservoir = "LAKE WATEREE", min_scale = 0, max_scale = 80, label_height = NULL) {
  
  df <- data_list[[param]]
  
  filtered_data <- df %>%
    filter(Depth_m <= 1, Waterbody == reservoir, Season2 == "Apr-Oct") %>%
    group_by(StationID) %>%
    filter(n() >= 5) %>%      #excluding stations with fewer than 5 values
    ungroup() %>%
    mutate(StationID = factor(StationID, levels = unique(StationID[order(Order2)])))
  
  # If label_height is not provided, set it to 105% of max_scale
  if (is.null(label_height)) {
    label_height <- max_scale * 1.05
  }
  
  ggplot(filtered_data, aes(x = StationID, y = !!sym(param), fill = Category)) +
    geom_boxplot() +
    stat_summary(
      fun.data = function(x) {
        return(data.frame(y = label_height, label = length(x)))
      },
      geom = "text",
      vjust = -0.5,
      size = 3
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Station ID", y = paste(param, "(", df[1,4], ")"), 
         title = paste(param, "in", reservoir, "Apr-Oct")) +
    scale_y_continuous(limits = c(min_scale, label_height * 1.1)) +
    scale_fill_discrete(name = "Category") +
    theme(legend.position = "bottom")
}

boxplots1("Chla", "FISHING CREEK RESERVOIR")
boxplots1("Chla", "GREAT FALLS RESERVOIR")
boxplots1("Chla", "CEDAR CREEK RESERVOIR")
boxplots1("Chla", "LAKE WATEREE")

boxplots1("Microcystin", "FISHING CREEK RESERVOIR", min_scale = 0, max_scale = 0.25)
boxplots1("Microcystin", "CEDAR CREEK RESERVOIR", min_scale = 0, max_scale = 0.4)
boxplots1("Microcystin", "LAKE WATEREE", min_scale = 0, max_scale = 0.65)

# The second boxplot function plot accepts a parameter and a station list. The
# plots are by month for each station. No temporal filtering, but still just
# data collected in the upper meter of the water column.

boxplots2 <- function(param = "Chla", station_list = c("CW-016F", "RL-21226", "RL-17071", "LCR-01"), min_scale = 0, max_scale = 80) {
  
  df <- data_list[[param]]
  
  filtered_data <- df %>%
    filter(Depth_m <= 1, StationID %in% station_list) %>%
    group_by(StationID, Month) %>%
    filter(n() >= 5) %>%      #excluding stations with fewer than 5 values
    ungroup() %>%
    mutate(StationID = factor(StationID, levels = unique(StationID[order(Order2)])))
  
  ggplot(filtered_data, aes(x = factor(Month), y = !!sym(param))) +
    geom_boxplot() +
    facet_wrap(~ StationID, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Month", y = paste(param, "(", df[1,4], ")"), title = paste(param, "by Month and Station")) +
    scale_y_continuous(limits = c(min_scale, max_scale))
}

boxplots2("Chla", c("CW-016F", "LCR-04", "CW-057"))   # Fishing Creek Res. Stations
boxplots2("Chla", c("CW-174", "CW-033"))              # Cedar Creek Res. Stations
boxplots2("Chla", c("CW-231", "CW-208", "CW-207B", "CL-089"))   # Lake Wateree Stations

boxplots2("DO", c("CW-016F", "LCR-04", "CW-057"), max_scale = 20)   # Fishing Creek Res. Stations
boxplots2("DO", c("CW-174", "CW-033"), max_scale = 20)              # Cedar Creek Res. Stations
boxplots2("DO", c("CW-231", "CW-208", "CW-207B", "CL-089"), max_scale = 20)   # Lake Wateree Stations

boxplots2("pH", c("CW-016F", "LCR-04", "CW-057"), min_scale = 5, max_scale = 10)   # Fishing Creek Res. Stations
boxplots2("pH", c("CW-174", "CW-033"), min_scale = 5, max_scale = 10)              # Cedar Creek Res. Stations
boxplots2("pH", c("CW-231", "CW-208", "CW-207B", "CL-089"), min_scale = 5, max_scale = 10)   # Lake Wateree Stations


# =============CODE BELOW REPLACED BY FUNCTIONS ABOVE================

# Fishing Creek Reservoir boxplots

filtered_data <- data_list$Chla %>%
  filter(Depth_m <= 1, Waterbody == "FISHING CREEK RESERVOIR", Season2 == "Apr-Oct") %>%
  group_by(StationID) %>%
  filter(n() >= 5) %>%      #excluding stations with fewer than 5 values
  ungroup() %>%
  mutate(StationID = factor(StationID, levels = unique(StationID[order(Order2)])))

ggplot(filtered_data, aes(x = StationID, y = Chla, fill = Category)) +
  geom_boxplot() +
  facet_wrap(~ Category, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Station ID", y = "Chlorophyll-a (µg/L)", title = "Chl-a in Fishing Creek Reservoir, Apr-Oct") +
  scale_y_continuous(limits = c(0, 80))

# Great Falls Reservoir boxplots

filtered_data <- data_list$Chla %>%
  filter(Depth_m <= 1, Waterbody == "GREAT FALLS RESERVOIR", Season2 == "Apr-Oct") %>%
  group_by(StationID) %>%
  filter(n() >= 5) %>%      #excluding stations with fewer than 5 values
  ungroup() %>%
  mutate(StationID = factor(StationID, levels = unique(StationID[order(Order2)])))

ggplot(filtered_data, aes(x = StationID, y = Chla, fill = Category)) +
  geom_boxplot() +
  facet_wrap(~ Category, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Station ID", y = "Chlorophyll-a (µg/L)", title = "Chl-a in Great Falls Reservoir, Apr-Oct") +
  scale_y_continuous(limits = c(0, 80))

# Cedar Creek Reservoir boxplots

filtered_data <- data_list$Chla %>%
  filter(Depth_m <= 1, Waterbody == "CEDAR CREEK RESERVOIR", Season2 == "Apr-Oct") %>%
  group_by(StationID) %>%
  filter(n() >= 5) %>%      #excluding stations with fewer than 5 values
  ungroup() %>%
  mutate(StationID = factor(StationID, levels = unique(StationID[order(Order2)])))

ggplot(filtered_data, aes(x = StationID, y = Chla, fill = Category)) +
  geom_boxplot() +
  facet_wrap(~ Category, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Station ID", y = "Chlorophyll-a (µg/L)", title = "Chl-a in Cedar Creek Reservoir, Apr-Oct") +
  scale_y_continuous(limits = c(0, 80))

#Lake Wateree boxplots

filtered_data <- data_list$Chla %>%
  filter(Depth_m <= 1, Waterbody == "LAKE WATEREE", Season2 == "Apr-Oct") %>%
  group_by(StationID) %>%
  filter(n() >= 5) %>%      #excluding stations with fewer than 5 values
  ungroup() %>%
  mutate(StationID = factor(StationID, levels = unique(StationID[order(Order2)])))

ggplot(filtered_data, aes(x = StationID, y = Chla, fill = Category)) +
  geom_boxplot() +
  facet_wrap(~ Category, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Station ID", y = "Chlorophyll-a (µg/L)", title = "Chl-a in Lake Wateree, Apr-Oct") +
  scale_y_continuous(limits = c(0, 80))

