#' Find the closest value in a numeric vector
#'
#' @param x A numeric vector
#' @param target A numeric value to find the closest match for
#' @return The closest value in `x` to `target`
#' @examples
#' find_closest_value(c(1, 5, 10, 20), 8)  # Returns 10
find_closest_value <- function(x, target) {
  x[which.min(abs(x - target))]
}


#' Compute Power Saturation Data
#'
#' @param data A dataframe containing EPR measurement data
#' @param sample_name A character string specifying the sample name to filter
#' @param power_levels A numeric vector of power values (default: common power levels)
#' @param selection_points A numeric vector of selected field values (G)
#' @return A dataframe with intensity values at different power levels
#' @examples
#' compute_power_saturation(all_data, "MBXXX", c(20, 40, 80), c(2906, 3224))
compute_power_saturation <- function(data, sample_name, 
                                     power_levels = c(20, 40, 80, 320, 640, 1280), 
                                     selection_points) {
  
  power_saturation <- map_df(power_levels, function(p) {
    map_df(selection_points, function(y) {
      data_filtered <- data %>%
        filter(sample == sample_name, power_uW == p)
      
      data_filtered %>%
        filter(Field_G == find_closest_value(data_filtered$Field_G, y))
    })
  })
  power_saturation$sqrt_power <- sqrt(power_saturation$power_uW)
  return(power_saturation)
}



#' Process a Single EPR Data File
#'
#' @param filename A character string representing the file name (without extension)
#' @return A dataframe with extracted metadata and measurement data
#' @examples
#' process_file('MB032_10K20uW15G2600SW16scans')
process_DTAfile <- function(filename) {
  # Remove the .DTA extension if present
  filename <- gsub("\\.DTA$", "", filename)
  
  # Split filename into components
  info <- strsplit(filename, "_")[[1]]
  
  # Extract sample name and temperature
  sample <- info[1]
  T_K <- as.numeric(gsub("K.*", "", info[2]))  # Remove 'K' to get temperature
  
  # Extract power and check unit (mW -> uW conversion)
  power_str <- gsub(".*K", "", info[2])
  power <- ifelse(grepl("mW", power_str), 
                  as.numeric(gsub("mW.*", "", power_str)) * 1000, 
                  as.numeric(gsub("uW.*", "", power_str)))
  
  # Extract amplitude, scan width, and number of scans
  amplitude <- as.numeric(gsub("G.*", "", gsub(".*[mu]W", "", info[2])))
  scan_width <- as.numeric(gsub("SW.*", "", gsub(".*G", "", info[2])))
  scans <- as.numeric(gsub("scans", "", gsub(".*SW", "", info[2])))
  
  # Read and process the CSV file
  df <- read.csv(paste0("csv/", filename, ".csv")) %>%
    mutate(sample = sample,
           T_K = T_K,
           power_uW = power,
           amplitude_G = amplitude,
           scan_width = scan_width,
           scans = scans)
  
  return(df)
}


#' Process Multiple EPR Data Files
#'
#' @param filenames A character vector of file names to process
#' @return A combined dataframe with all processed files
#' @examples
#' filenames <- c('MB032_10K20uW15G2600SW16scans', 'MB033_15K25uW20G2700SW20scans')
#' all_data <- process_multiple_files(filenames)
process_multiple_DTAfiles <- function(filenames) {
  # Apply process_file to each filename and bind results together
  df_list <- lapply(filenames, process_DTAfile)
  combined_df <- bind_rows(df_list)
  
  return(combined_df)
}


#' Extract Information from a DSC File
#'
#' @param file_path A character string representing the path to the DSC file
#' @return A list containing extracted metadata
#' @examples
#' dsc_info <- extract_dsc_info("path/to/file.DSC")
#' print(dsc_info)
extract_dsc_info <- function(file_path) {
  # Read file contents
  lines <- readLines(file_path)
  
  # Initialize storage lists
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
  
  # Determine the current section
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
      parsed <- parse_key_value(line)
      if (current_section == "descriptor_info") {
        descriptor_info[[parsed$key]] <- parsed$value
      } else if (current_section == "standard_param_layer") {
        standard_param_layer[[parsed$key]] <- parsed$value
      } else if (current_section == "device_specific_layer") {
        device_specific_layer[[parsed$key]] <- parsed$value
      }
    }
  }
  
  # Return extracted information as a list
  return(list(
    descriptor_info = descriptor_info,
    standard_param_layer = standard_param_layer,
    device_specific_layer = device_specific_layer
  ))
}
