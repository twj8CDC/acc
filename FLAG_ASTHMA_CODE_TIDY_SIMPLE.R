# install.packages("MEPS")
# library(devtools)
install_github("e-mitchell/meps_r_pkg/MEPS")
library(MEPS)
library(dplyr)

# Function to read MEPS data
read_meps_data <- function(year, types) {
  data_list <- list()
  for (type in types) {
    data_list[[type]] <- read_MEPS(year = year, type = type)
  }
  return(data_list)
}

# MEPS::read_MEPS(year=2008, type="FYC")

# Function to select conditions related to asthma (ICD-9 code 493)
asthma_conditions <- function(df) {
  df %>%
    filter(grepl("^493", ICD9CODX)) %>%
    mutate(Condition = "Asthma")
}

# Function to merge each event file with the Condition Event Link file and then with the Condition file
merge_conditions <- function(event_df, link_df, condition_df) {
  event_df %>%
    left_join(link_df, by = c("DUPERSID", "EVNTIDX")) %>%
    left_join(condition_df, by = "CONDIDX") %>%
    asthma_conditions()
}

# Function to process data for a single year
process_year <- function(year) {
  data <- read_meps_data(year, c("FYC", "RX", "DV", "IP", "ER", "OP", "OB", "HH", "COND", "CLNK"))
  
  # Rename column for RX dataset
  data$RX <- data$RX %>% rename(EVNTIDX = "LINKIDX")
  
  # Apply merge_conditions function to the relevant datasets
  event_types <- c("RX", "IP", "ER", "OP", "OB", "HH")
  merged_data <- bind_rows(lapply(event_types, function(type) {
    merge_conditions(data[[type]], data$CLNK, data$COND)
  }))
  
  # Extract unique DUPERSID for asthma-related conditions with the year
  asthma_DUPERSID <- merged_data %>%
    filter(!is.na(Condition)) %>%
    distinct(DUPERSID.y) %>%
    mutate(Year = year) %>%
    rename(DUPERSID = DUPERSID.y)
  
  return(asthma_DUPERSID)
}

# data <- read_meps_data(year=2008, type=c("FYC", "RX", "DV", "IP", "ER", "OP", "OB", "HH", "COND", "CLNK"))
# str(data)
# Specify the years of interest
years <- c(2008,2009,2010,2011,2012,2013)

# Process data for all specified years and combine the results
all_years_data <- bind_rows(lapply(years, function(year) {
  process_year(year)
}))

# Display the combined data
all_years_data %>%
  group_by(Year) %>%
  summarize(asthma_counts = n())

str(all_years_data)
