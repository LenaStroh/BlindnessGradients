rm(list = ls(all = TRUE))
library(data.table)
library(stringr)
library(tidyr)

# ============================================================================
# Utility Functions
# ============================================================================

load_gradient <- function(file, gradient_name, demographics) {
  gradient <- fread(file)
  n <- min(nrow(gradient), nrow(demographics))
  gradient         <- gradient[1:n]
  gradient$group   <- demographics$group[1:n]
  gradient$age     <- demographics$age[1:n]
  gradient$sex     <- demographics$sex[1:n]
  gradient$site    <- demographics$site[1:n]
  gradient[, subject := .I]
  gradient$gradient <- gradient_name
  return(gradient)
}

load_distance <- function(file, gradient_name, demographics) {
  distance <- fread(file)
  n <- min(nrow(distance), nrow(demographics))
  distance         <- distance[1:n]
  distance$group   <- demographics$group[1:n]
  distance$age     <- demographics$age[1:n]
  distance$sex     <- demographics$sex[1:n]
  distance$site    <- demographics$site[1:n]
  distance[, subject := .I]
  distance$gradient <- gradient_name
  return(distance)
}

load_morphometric <- function(file, measure_name, demographics) {
  morpho <- fread(file)
  n <- min(nrow(morpho), nrow(demographics))
  morpho <- morpho[1:n]
  morpho$group   <- demographics$group[1:n]
  morpho$age     <- demographics$age[1:n]
  morpho$sex     <- demographics$sex[1:n]
  morpho$site    <- demographics$site[1:n]
  morpho$etiv    <- demographics$etiv[1:n]
  morpho[, subject := .I]
  morpho$measure <- measure_name
  return(morpho)
}

summarize_gradients <- function(dt_long) {
  dt_long[, .(
    n_subjects = length(unique(subject)),
    n_rois     = length(unique(paste(region, hemi))),
    mean_score = mean(gradient_score, na.rm = TRUE),
    sd_score   = sd(gradient_score,   na.rm = TRUE),
    min_score  = min(gradient_score,  na.rm = TRUE),
    max_score  = max(gradient_score,  na.rm = TRUE),
    n_missing  = sum(is.na(gradient_score))
  ), by = gradient]
}

check_missing_data <- function(dt_long) {
  missing_by_subject <- dt_long[, .(
    n_missing   = sum(is.na(gradient_score)),
    pct_missing = 100 * mean(is.na(gradient_score))
  ), by = .(subject, gradient)][n_missing > 0]
  
  if (nrow(missing_by_subject) == 0) {
    message("No missing data found!")
  } else {
    message(sprintf("Found %d subject-gradient combinations with missing data",
                    nrow(missing_by_subject)))
  }
  return(missing_by_subject)
}

get_network_composition <- function(dt_long) {
  out <- unique(dt_long[, .(region, hemi, yeo)])[, .N, by = yeo]
  setorder(out, -N)
  out
}

get_stream_composition <- function(dt_long) {
  out <- unique(dt_long[!is.na(stream), .(region, hemi, stream)])[, .N, by = stream]
  setorder(out, -N)
  out
}

# ============================================================================
# Main Pipeline Function
# ============================================================================

load_and_process_gradients <- function(gradient_files,
                                       demographic_files,
                                       glasser_file = "glasser360NodeNames.txt",
                                       network_file  = "yeo_in_glasser.txt",
                                       normalize     = FALSE,
                                       filter_expr   = NULL,
                                       output_file   = NULL) {
  
  if (!is.character(gradient_files) || length(gradient_files) == 0)
    stop("gradient_files must be a named character vector with at least one file")
  
  if (is.null(names(gradient_files)))
    names(gradient_files) <- paste0("gradient_", seq_along(gradient_files))
  
  message("Loading reference data...")
  glasser_names <- fread(glasser_file, header = FALSE)
  
  message("Loading demographic data...")
  demographics <- na.omit(fread(demographic_files$demographics))
  
  message("Loading network data...")
  yeo <- fread(network_file, header = FALSE)
  
  message("Loading gradient files...")
  gradient_list <- lapply(seq_along(gradient_files), function(i) {
    message(sprintf("  Loading %s from %s", names(gradient_files)[i], gradient_files[i]))
    load_gradient(gradient_files[i], names(gradient_files)[i], demographics)
  })
  
  message("Combining and reshaping...")
  dt_long <- melt(
    rbindlist(gradient_list),
    id.vars       = c("subject", "group", "gradient", "age", "sex", "site"),
    variable.name = "roi",
    value.name    = "gradient_score"
  )
  
  message("Parsing ROI information...")
  dt_long[, hemi   := fifelse(sapply(strsplit(as.character(roi), "_"), `[`, 2) == "L",
                              "left", "right")]
  dt_long[, region := sapply(strsplit(as.character(roi), "_"), `[`, 1)]
  dt_long[, roi    := NULL]
  
  message("Adding network information...")
  nsubs <- length(unique(dt_long$subject))
  ngrad <- length(unique(dt_long$gradient))
  dt_long[, yeo := rep(yeo$V1, each = nsubs * ngrad)]
  
  yeo_mapping <- c("1"="VN","2"="SMN","3"="DAN","4"="VAN","5"="LN","6"="FPN","7"="DMN")
  dt_long[, yeo := yeo_mapping[as.character(yeo)]]
  
  message("Adding visual stream information...")
  dt_long[region %in% c("V1","V2","V3","V4"),                                      stream := "evc"]
  dt_long[region %in% c("V3A","V3B","V6","V6A","V7","IPS1"),                       stream := "dorsal"]
  dt_long[region %in% c("V8","VVC","PIT","FFC","VMV1","VMV2","VMV3",
                        "PHA1","PHA2","PHA3","PreS","ProS","DVT"),                 stream := "ventral"]
  dt_long[region %in% c("V3CD","LO1","LO2","LO3","V4t","FST","MT","MST","PH"),     stream := "lateral"]
  
  if (normalize) {
    message("Normalizing gradient scores...")
    dt_long[, gradient_score := scale(gradient_score)[,1], by = gradient]
  }
  
  message("Converting to factors...")
  dt_long[, `:=`(
    subject  = as.factor(subject),
    sex      = as.factor(sex),
    site     = as.factor(site),
    hemi     = as.factor(hemi),
    region   = as.factor(region),
    yeo      = factor(yeo, levels = c("VN", "SMN", "DAN", "VAN", "LN", "FPN", "DMN")),
    stream   = as.factor(stream),
    gradient = as.factor(gradient),
    group = factor(group, levels = c("CB", "LB", "SC"))
  )]
  
  # Filter if expression provided
  if (!is.null(filter_expr)) {
    message("Applying filter...")
    dt_long <- dt_long[eval(parse(text = filter_expr))]
    dt_long <- droplevels(dt_long)
  }
  
  if (!is.null(output_file)) {
    message(sprintf("Saving to %s...", output_file))
    saveRDS(dt_long, output_file)
  }
  
  message("Done!")
  message(sprintf("Final: %d rows x %d cols | %d subjects | %d gradients | %d ROIs",
                  nrow(dt_long), ncol(dt_long),
                  length(unique(dt_long$subject)),
                  length(unique(dt_long$gradient)),
                  length(unique(paste(dt_long$region, dt_long$hemi)))))
  return(dt_long)
}

load_and_process_distances <- function(distance_files,
                                       demographic_files,
                                       filter_expr   = NULL,
                                       output_file   = NULL) {
  
  if (!is.character(distance_files) || length(distance_files) == 0)
    stop("distance_files must be a named character vector with at least one file")
  
  if (is.null(names(distance_files)))
    names(distance_files) <- paste0("gradient_", seq_along(distance_files))
  
  message("Loading demographic data...")
  demographics <- na.omit(fread(demographic_files$demographics))
  n_demo <- nrow(demographics)

  message("Loading distance files...")
  distance_list <- lapply(seq_along(distance_files), function(i) {
    message(sprintf("  Loading %s from %s", names(distance_files)[i], distance_files[i]))
    
    dist_peek  <- fread(distance_files[i])
    n_subjects <- nrow(dist_peek)
    
    if (n_subjects > n_demo)
      stop(sprintf(
        "Distance file %s has %d subjects but demographic files only have %d rows",
        distance_files[i], n_subjects, n_demo
      ))
    
    if (n_subjects < n_demo)
      message(sprintf(
        "  Note: demographic files have %d rows but distance file has %d subjects -- using first %d rows",
        n_demo, n_subjects, n_subjects
      ))
    
    load_distance(distance_files[i], names(distance_files)[i],
                  demographics[1:n_subjects])
  })
  
  message("Combining and reshaping...")
  dt_long <- melt(
    rbindlist(distance_list),
    id.vars       = c("subject", "group", "gradient", "age", "sex", "site"),
    variable.name = "roi",
    value.name    = "distance_raw"
  )
  
  message("Averaging distances across ROI quadrants per subject...")
  dt_long <- dt_long[, .(distance = mean(distance_raw, na.rm = TRUE)),
                     by = .(subject, roi, group, age, sex, site, gradient)]
  
  message("Converting to factors...")
  dt_long[, `:=`(
    subject  = as.factor(subject),
    sex      = as.factor(sex),
    site     = as.factor(site),
    roi      = as.factor(roi),
    gradient = as.factor(gradient),
    group    = factor(group, levels = c("CB", "LB", "SC"))
  )]
  
  if (!is.null(filter_expr)) {
    message("Applying filter...")
    dt_long <- dt_long[eval(parse(text = filter_expr))]
    dt_long <- droplevels(dt_long)
  }
  
  if (!is.null(output_file)) {
    message(sprintf("Saving to %s...", output_file))
    saveRDS(dt_long, output_file)
  }
  
  message("Done!")
  message(sprintf("Final: %d rows x %d cols | %d subjects | %d gradients | %d ROIs",
                  nrow(dt_long), ncol(dt_long),
                  length(unique(dt_long$subject)),
                  length(unique(dt_long$gradient)),
                  length(unique(dt_long$roi))))
  # return(dt_long)
}

# LOAD AND PROCESS STRUCTURAL DATA -------------------------------
load_and_process_morphometrics <- function(morphometric_files,
                                           demographic_files,
                                           network_file = "yeo_in_glasser.txt",
                                           normalize    = FALSE,
                                           filter_expr  = NULL,
                                           output_file  = NULL) {
  
  if (!is.character(morphometric_files) || length(morphometric_files) == 0)
    stop("morphometric_files must be a named character vector with at least one file")
  
  if (is.null(names(morphometric_files)))
    names(morphometric_files) <- paste0("measure_", seq_along(morphometric_files))
  
  message("Loading demographic data...")
  demographics <- na.omit(fread(demographic_files$demographics))
  
  message("Loading network data...")
  yeo <- fread(network_file, header = FALSE)
  
  message("Loading morphometric files...")
  morpho_list <- lapply(seq_along(morphometric_files), function(i) {
    message(sprintf("  Loading %s from %s", names(morphometric_files)[i], morphometric_files[i]))
    load_morphometric(morphometric_files[i], names(morphometric_files)[i], demographics)
  })
  
  message("Combining and reshaping...")
  dt_long <- melt(
    rbindlist(morpho_list),
    id.vars       = c("subject", "group", "measure", "age", "sex", "site", "etiv"),
    variable.name = "roi",
    value.name    = "morpho_score"
  )
  
  message("Parsing ROI information...")
  dt_long[, hemi   := fifelse(sapply(strsplit(as.character(roi), "_"), `[`, 2) == "L",
                              "left", "right")]
  dt_long[, region := sapply(strsplit(as.character(roi), "_"), `[`, 1)]
  dt_long[, roi    := NULL]
  
  message("Adding network information...")
  nsubs  <- length(unique(dt_long$subject))
  nmeas  <- length(unique(dt_long$measure))
  dt_long[, yeo := rep(yeo$V1, each = nsubs * nmeas)]
  
  yeo_mapping <- c("1"="VN","2"="SMN","3"="DAN","4"="VAN","5"="LN","6"="FPN","7"="DMN")
  dt_long[, yeo := yeo_mapping[as.character(yeo)]]
  
  message("Adding visual stream information...")
  dt_long[region %in% c("V1","V2","V3","V4"),                                    stream := "evc"]
  dt_long[region %in% c("V3A","V3B","V6","V6A","V7","IPS1"),                     stream := "dorsal"]
  dt_long[region %in% c("V8","VVC","PIT","FFC","VMV1","VMV2","VMV3",
                        "PHA1","PHA2","PHA3","PreS","ProS","DVT"),               stream := "ventral"]
  dt_long[region %in% c("V3CD","LO1","LO2","LO3","V4t","FST","MT","MST","PH"),   stream := "lateral"]
  
  if (normalize) {
    message("Normalizing morphometric scores...")
    dt_long[, morpho_score := scale(morpho_score)[,1], by = measure]
  }
  
  message("Converting to factors...")
  dt_long[, `:=`(
    subject = as.factor(subject),
    sex     = as.factor(sex),
    site    = as.factor(site),
    hemi    = as.factor(hemi),
    region  = as.factor(region),
    yeo     = factor(yeo, levels = c("VN","SMN","DAN","VAN","LN","FPN","DMN")),
    stream  = as.factor(stream),
    measure = as.factor(measure),
    group   = factor(group, levels = c("CB","LB","SC"))
  )]
  
  if (!is.null(filter_expr)) {
    message("Applying filter...")
    dt_long <- dt_long[eval(parse(text = filter_expr))]
    dt_long <- droplevels(dt_long)
  }
  
  if (!is.null(output_file)) {
    message(sprintf("Saving to %s...", output_file))
    saveRDS(dt_long, output_file)
  }
  
  message("Done!")
  message(sprintf("Final: %d rows x %d cols | %d subjects | %d measures | %d ROIs",
                  nrow(dt_long), ncol(dt_long),
                  length(unique(dt_long$subject)),
                  length(unique(dt_long$measure)),
                  length(unique(paste(dt_long$region, dt_long$hemi)))))
  return(dt_long)
}