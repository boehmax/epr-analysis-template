library(dplyr)

process_file <- function(filename) {
  # Remove the file extension if present
  filename <- gsub("\\.DTA$", "", filename)
  
  # Split the filename by the underscore character
  info <- strsplit(filename, "_")[[1]]
  
  # Extract sample name, temperature, power, amplitude (G), sweep width (SW), and scans
  sample <- info[1]
  T_K <- as.numeric(gsub("K.*", "", info[2]))                     # Remove 'K' to get the temperature
  
  # Extract power and check for unit
  power_str <- gsub(".*K", "", info[2])
  if (grepl("mW", power_str)) {
    power <- as.numeric(gsub("mW.*", "", power_str)) * 1000       # Convert mW to uW
  } else {
    power <- as.numeric(gsub("uW.*", "", power_str))              # Extract power in uW
  }
  
  amplitude <- as.numeric(gsub("G.*", "", gsub(".*[mu]W", "", info[2])))  # Extract amplitude from the '15G'
  scan_width <- as.numeric(gsub("SW.*", "", gsub(".*G", "", info[2])))    # Extract sweep width
  scans <- as.numeric(gsub("scans", "", gsub(".*SW", "", info[2])))       # Extract scans number
  
  # Read in the data
  df <- read.csv(paste0("csv/", filename, ".csv")) %>%
    mutate(sample = sample,
           T_K = T_K,
           power_uW = power,
           amplitude_G = amplitude,
           scan_width = scan_width,
           scans = scans)
  
  return(df)
}

# Function to process multiple files
process_multiple_files <- function(filenames) {
  # Apply process_file to each file and combine the results
  df_list <- lapply(filenames, process_file)
  
  # Combine all dataframes into one
  combined_df <- bind_rows(df_list)
  
  return(combined_df)
}

# Example usage
# filenames <- c('MB032_10K20uW15G2600SW16scans', 'MB033_15K25uW20G2700SW20scans')
# all_data <- process_multiple_files(filenames)


extract_dsc_info <- function(file_path) {
  # Read the file
  lines <- readLines(file_path)
  
  # Initialize lists to store data
  descriptor_info <- list()
  standard_param_layer <- list()
  device_specific_layer <- list()
  
  # Helper function to parse key-value pairs
  parse_key_value <- function(line) {
    parts <- strsplit(line, "\\s+", fixed = TRUE)[[1]]
    key <- parts[1]
    value <- paste(parts[-1], collapse = " ")
    return(list(key = key, value = value))
  }
  
  # Helper function to handle specific key-value pairs with spaces
  parse_key_value_with_spaces <- function(line) {
    parts <- strsplit(line, "\\s+", fixed = TRUE)[[1]]
    key <- parts[1]
    value <- paste(parts[-1], collapse = " ")
    return(list(key = key, value = value))
  }
  
  # Parse the file
  current_section <- NULL
  for (line in lines) {
    line <- trimws(line)
    
    if (grepl("^#DESC", line)) {
      current_section <- "descriptor_info"
    } else if (grepl("^#SPL", line)) {
      current_section <- "standard_param_layer"
    } else if (grepl("^#DSL", line)) {
      current_section <- "device_specific_layer"
    } else if (grepl("^\\*", line) || line == "") {
      next
    } else {
      parsed <- parse_key_value_with_spaces(line)
      if (current_section == "descriptor_info") {
        descriptor_info[[parsed$key]] <- parsed$value
      } else if (current_section == "standard_param_layer") {
        standard_param_layer[[parsed$key]] <- parsed$value
      } else if (current_section == "device_specific_layer") {
        device_specific_layer[[parsed$key]] <- parsed$value
      }
    }
  }
  
  # Return the extracted information as a list
  return(list(
    descriptor_info = descriptor_info,
    standard_param_layer = standard_param_layer,
    device_specific_layer = device_specific_layer
  ))
}


# Example usage
#file_path <- "path/to/your/file.DSC"
#dsc_info <- extract_dsc_info(file_path)
#print(dsc_info)
