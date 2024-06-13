# Load necessary library
library(readr)
library(dplyr)

# Define the directory containing the CSV files
directory_path <- "G:/.shortcut-targets-by-id/1Hqecl4V9odrCM-fZun8l-1cDSCip00KD/ATELIER_Actividad_Tecnica/WP9/Info_IBERDROLA/FTP/kunsthal/Data from 2024 until May 24"

# List all CSV files in the directory
file_list <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)

# Define the columns that need cleaning and conversion to numeric
numeric_columns <- c("A L1 [A]", "A L2 [A]", "A L3 [A]", "An [A]", "Hz [Hz]",
                     "PF L1", "PF L2", "PF L3", "PF sys", "THD A L1 [%]", 
                     "THD A L2 [%]", "THD A L3 [%]", "THD V L1-N [%]", 
                     "THD V L2-N [%]", "THD V L3-N [%]", "V L1-L2 [V]", 
                     "V L1-N [V]", "V L2-L3 [V]", "V L2-N [V]", "V L3-L1 [V]", 
                     "V L3-N [V]", "V L-L sys [V]", "V L-N sys [V]", 
                     "kVA L1 [kVA]", "kVA L2 [kVA]", "kVA L3 [kVA]", 
                     "kVA sys [kVA]", "kWh AC [kWh]", "kWh (-) AC [kWh]", 
                     "kW L1 [kW]", "kW L2 [kW]", "kW L3 [kW]", "kW sys [kW]", 
                     "kvarh [kvarh]", "kvar L1 [kvar]", "kvar L2 [kvar]", 
                     "kvar L3 [kvar]", "kvar sys [kvar]")

# Function to clean and convert columns to numeric
clean_and_convert <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))
}

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each file, read the data, clean and convert columns, and combine the data
for (file in file_list) {
  # Read the CSV file
  data <- read_delim(file, delim = ";", col_types = cols(.default = "c"))
  
  # Apply the function to all numeric columns
  data[numeric_columns] <- lapply(data[numeric_columns], clean_and_convert)
  
  # Combine the data
  combined_data <- bind_rows(combined_data, data)
}

# Define the path to save the combined CSV file
output_file_path <- "G:/.shortcut-targets-by-id/1Hqecl4V9odrCM-fZun8l-1cDSCip00KD/ATELIER_Actividad_Tecnica/WP9/Info_IBERDROLA/FTP/kunsthal/combined_data.csv"

# Write the combined data to a new CSV file
write_csv(combined_data, output_file_path)
