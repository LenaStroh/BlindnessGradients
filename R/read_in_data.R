rm(list = ls(all = TRUE))
library(data.table)
library(stringr)
library(tidyr)

# Load utility functions
source("R/utils.R")

#' Load and Process Gradient Data
#' 
#' This function loads gradient data files and combines them with demographic
#' information, network assignments, and visual stream classifications.
#' 
#' @param gradient_files Named vector of gradient file paths (e.g., c(gradient_1 = "file1.txt", gradient_2 = "file2.txt"))
#' @param demographic_files List with paths: group, age, sex
#' @param glasser_file Path to Glasser region names file (default: "glasser360NodeNames.txt")
#' @param network_file Path to Yeo network file (default: "yeo_in_glasser.txt")
#' @param normalize Logical; should gradient scores be normalized by gradient? (default: FALSE)
#' @param output_file Path to save the output RDS file (default: NULL, no save)
#' @return A data.table with processed gradient data in long format
load_and_process_gradients <- function(gradient_files,
                                       demographic_files,
                                       glasser_file = "glasser360NodeNames.txt",
                                       network_file = "yeo_in_glasser.txt",
                                       normalize = FALSE,
                                       output_file = NULL) {
  
  # Input validation
  if (!is.character(gradient_files) || length(gradient_files) == 0) {
    stop("gradient_files must be a named character vector with at least one file")
  }
  
  if (is.null(names(gradient_files))) {
    names(gradient_files) <- paste0("gradient_", seq_along(gradient_files))
  }
  
  # Load reference data needed by load_gradient function
  # Note: These are assigned to global environment because load_gradient() expects them
  message("Loading reference data...")
  glasser_names <<- fread(glasser_file, header = FALSE)
  
  # Load demographic data (these are needed as globals for load_gradient)
  message("Loading demographic data...")
  group <<- fread(demographic_files$group, header = FALSE)
  age <<- fread(demographic_files$age, header = FALSE)
  sex <<- fread(demographic_files$sex, header = FALSE)
  site <<- fread(demographic_files$site, header = FALSE)
  
  # Load network information
  message("Loading network data...")
  yeo <- fread(network_file, header = FALSE)
  
  # Load all gradient files using the utility function
  message("Loading gradient files...")
  gradient_list <- list()
  
  for (i in seq_along(gradient_files)) {
    grad_name <- names(gradient_files)[i]
    grad_file <- gradient_files[i]
    
    message(sprintf("  Loading %s from %s", grad_name, grad_file))
    gradient_list[[i]] <- load_gradient(grad_file, grad_name)
  }
  
  # Combine all gradients
  message("Combining gradient data...")
  all_gradients <- rbindlist(gradient_list)
  
  # Reshape to long format
  message("Reshaping to long format...")
  dt_long <- melt(
    all_gradients,
    id.vars = c("subject", "group", "gradient", "age", "sex", "site"),
    variable.name = "roi",
    value.name = "gradient_score"
  )
  
  # Parse hemisphere information
  message("Parsing ROI information...")
  dt_long[, hemi := sapply(strsplit(as.character(roi), "_"), `[`, 2)]
  dt_long[hemi == "L", hemi := "left"]
  dt_long[hemi == "R", hemi := "right"]
  dt_long[, hemi := as.character(hemi)]
  
  # Parse region information
  dt_long[, region := sapply(strsplit(as.character(roi), "_"), `[`, 1)]
  dt_long[, roi := NULL]
  
  # Add Yeo network information
  message("Adding network information...")
  nsubs <- length(unique(dt_long$subject))
  ngrad <- length(unique(dt_long$gradient))
  
  dt_long[, yeo := rep(yeo$V1, each = nsubs * ngrad)]
  
  # Recode Yeo networks with descriptive names
  yeo_mapping <- c(
    "1" = "VN",   # Visual Network
    "2" = "SMN",  # Somatomotor Network
    "3" = "DAN",  # Dorsal Attention Network
    "4" = "VAN",  # Ventral Attention Network
    "5" = "LN",   # Limbic Network
    "6" = "FPN",  # Frontoparietal Network
    "7" = "DMN"   # Default Mode Network
  )
  
  dt_long[, yeo := yeo_mapping[as.character(yeo)]]
  
  # Add visual stream information
  message("Adding visual stream information...")
  evc_regions <- c("V1", "V2", "V3", "V4")
  dorsal_regions <- c("V3A", "V3B", "V6", "V6A", "V7", "IPS1")
  ventral_regions <- c("V8", "VVC", "PIT", "FFC", "VMV1", "VMV2", "VMV3", 
                       "PHA1", "PHA2", "PHA3", "PreS", "ProS", "DVT")
  lateral_occ <- c("V3CD", "LO1", "LO2", "LO3", "V4t", "FST", "MT", "MST", "PH")
  
  dt_long[region %in% evc_regions, stream := "evc"]
  dt_long[region %in% dorsal_regions, stream := "dorsal"]
  dt_long[region %in% ventral_regions, stream := "ventral"]
  dt_long[region %in% lateral_occ, stream := "lateral"]
  
  # Normalize if requested
  if (normalize) {
    message("Normalizing gradient scores (z-scoring by gradient)...")
    dt_long[, gradient_score := scale(gradient_score)[,1], by = gradient]
  }
  
  # Convert to factors
  message("Converting to factors...")
  dt_long[, `:=`(
    subject = as.factor(subject),
    sex = as.factor(sex),
    site = as.factor(site),
    hemi = as.factor(hemi),
    region = as.factor(region),
    yeo = as.factor(yeo),
    stream = as.factor(stream),
    gradient = as.factor(gradient),
    # group = factor(group, levels = c("SC", "CB"))
    group = factor(group, levels = c("SC", "CB", "LB"))
  )]
  
  # Save if output file specified
  if (!is.null(output_file)) {
    message(sprintf("Saving to %s...", output_file))
    saveRDS(dt_long, output_file)
  }
  
  message("Done!")
  message(sprintf("Final dimensions: %d rows x %d columns", nrow(dt_long), ncol(dt_long)))
  message(sprintf("Subjects: %d | Gradients: %d | ROIs: %d", 
                  length(unique(dt_long$subject)),
                  length(unique(dt_long$gradient)),
                  length(unique(paste(dt_long$region, dt_long$hemi)))))
  
  return(dt_long)
}