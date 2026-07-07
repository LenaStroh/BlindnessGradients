rm(list = ls(all = TRUE))

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("R/utils.R")

# CONFIGURATION ######################
setwd("../data_raw/")

gradient_files <- c(
  gradient_1 = "gradient1_lb.txt",
  gradient_2 = "gradient2_lb.txt",
  gradient_3 = "gradient3_lb.txt"
)

distance_files <- c(
  gradient_1 = "min_distance_gradient1.txt",
  gradient_2 = "min_distance_gradient2.txt",
  gradient_3 = "min_distance_gradient3.txt"
)

morpho_files <- c(
  thickness = "thickness.txt",
  volume    = "volume.txt",
  area      = "surface_area.txt",
  curvature = "curvature.txt",
  depth     = "sulcal_depth.txt"
)

demographic_files = list(
  demographics = "demographics_lb.txt"
)

# CREATE DATAFRAMES ###################
load_and_process_gradients(
  gradient_files    = gradient_files,
  demographic_files = demographic_files,
  normalize         = FALSE,
  filter_expr       = "site != 'krakow'",
  output_file       = "../data_tidy/gradients_lb.RDS"
)

load_and_process_distances(
  distance_files    = distance_files,
  demographic_files = demographic_files,
  filter_expr       = "site != 'krakow'",
  output_file       = "../data_tidy/minimum_distances.RDS"
)

morph <- load_and_process_morphometrics(
  morphometric_files = morpho_files,
  demographic_files  = demographic_files,
  normalize          = FALSE,
  output_file        = "../data_tidy/morphometrics.rds"
)

# CREATE DATAFRAMES FOR STRUCTURE FUNCTION COUPLING
sfc <- fread("sfc.txt")
glasser <- fread("glasser360NodeNames.txt", header = FALSE)
sfc$roi <- glasser
sfc[, hemi := sapply(str_split(roi, "_"), `[`, 1)]
sfc[hemi == "Left", hemi := "left"]
sfc[hemi == "Right", hemi := "right"]
sfc[, region := sapply(str_split(roi, "_"), `[`, 2)]

setwd("../data_tidy/")
saveRDS(sfc, "sf_coupling.RDS")

# # READ IN VERTEX-WISE DATA ------------------------------------------------------------------
# library(readxl)
# library(data.table)
# library(reshape2)
# 
# setwd("../data_raw")
# all_sheets <- lapply(
#   excel_sheets("vertexwise_info.xlsx"),
#   function(sheet) as.data.table(read_excel("vertexwise_info.xlsx", sheet = sheet, col_names = FALSE))
# )
# names(all_sheets) <- excel_sheets("vertexwise_info.xlsx")
# 
# # ── 1. Read demographic data 
# demographics <- fread("demographics.txt")  # or read_excel, read.table etc
# setDT(demographics)
# 
# demographics <- demographics[site %in% c("baltimore", "canada")]
# demographics[, age_c := scale(age, center = TRUE, scale = FALSE)]
# 
# # all sheets have 83 rows matching demographics exactly
# subject_ids <- demographics$subject
# # add row number to demographics for positional merging
# demographics[, row_id := .I]
# 
# # read in vertexwise data
# long_data <- rbindlist(lapply(names(all_sheets), function(sheet_name) {
#   
#   dt <- as.data.frame(copy(all_sheets[[sheet_name]]))
#   
#   cat("Processing:", sheet_name, "| nrow:", nrow(dt), "| ncol:", ncol(dt), "\n")
#   
#   if (nrow(dt) == 0 || ncol(dt) == 0) {
#     cat("  --> skipping, empty\n")
#     return(NULL)
#   }
#   
#   if (nrow(dt) != nrow(demographics)) {
#     cat("  --> skipping, row mismatch\n")
#     return(NULL)
#   }
#   
#   parts <- strsplit(sheet_name, "_")[[1]]
#   part1 <- parts[1]
#   part2 <- parts[2]
#   
#   if (grepl("^[Gg]", part1)) {
#     gradient <- part1
#     pc       <- NA_character_
#   } else if (grepl("^[Pp][Cc]", part1)) {
#     pc       <- part1
#     gradient <- NA_character_
#   } else {
#     gradient <- NA_character_
#     pc       <- NA_character_
#   }
#   
#   region <- part2
#   
#   # use row number as merge key
#   dt$row_id <- seq_len(nrow(dt))
#   
#   dt_long <- reshape2::melt(
#     dt,
#     id.vars       = "row_id",
#     variable.name = "vertex",
#     value.name    = "vertex_score"
#   )
#   
#   dt_long$vertex   <- as.character(dt_long$vertex)
#   dt_long$gradient <- gradient
#   dt_long$region   <- region
#   dt_long$pc       <- pc
#   
#   as.data.table(dt_long)
#   
# }), fill = TRUE)
# 
# # merge on row_id — brings in subject, group, age_c, sex, site etc.
# final_data <- merge(long_data, demographics, by = "row_id", all.x = TRUE)
# final_data[, row_id := NULL]
# 
# # sanity check
# cat("Columns:", paste(names(final_data), collapse = ", "), "\n")
# cat("Missing group:", final_data[is.na(group), .N], "\n")
# cat("Missing age_c:", final_data[is.na(age_c), .N], "\n")
# cat("Unique subjects:", final_data[, uniqueN(subject)], "\n")
# print(head(final_data[, .(subject, group, age_c, sex, site, gradient, pc, region, vertex, vertex_score)]))
# 
# # ---- 2. COMBINE ALL SHEETS ----
# # long_data <- rbindlist(long_list, fill = TRUE)
# 
# # ---- 3. SPLIT INTO GRADIENTS & PCs ----
# grad_dt <- long_data[!is.na(gradient)]
# pc_dt   <- long_data[!is.na(pc)]
# 
# # ---- 4. CAST TO WIDE (SEPARATELY) ----
# grad_wide <- dcast(
#   grad_dt,
#   subject + region + vertex ~ gradient,
#   value.var = "gradient_score"
# )
# 
# pc_wide <- dcast(
#   pc_dt,
#   subject + region + vertex ~ pc,
#   value.var = "pc_score"
# )
# 
# # ---- 5. MERGE THEM ----
# final_data <- merge(
#   grad_wide,
#   pc_wide,
#   by = c("subject", "region", "vertex"),
#   all = TRUE
# )
# 
# # ---- 6. ORDER COLUMNS ----
# setcolorder(
#   final_data,
#   c("subject", "region", "vertex",
#     setdiff(names(final_data), c("subject", "region", "vertex")))
# )
# 
# setwd("../data_tidy")
# saveRDS(final_data, "vertex_wise_evc.RDS")
# 
# # ---- DONE ----
# 
# 
# library(readxl)
# library(data.table)
# 
# setwd("../data_raw")
# 
# # ---- 1. READ ALL SHEETS ----
# sheet_names <- excel_sheets("vertexwise_info.xlsx")
# 
# all_sheets <- lapply(
#   sheet_names,
#   function(sheet) as.data.table(
#     read_excel("vertexwise_info.xlsx", sheet = sheet, col_names = FALSE)
#   )
# )
# names(all_sheets) <- sheet_names
# 
# # ---- 2. READ DEMOGRAPHICS ----
# demographics <- fread("demographics.txt")
# setDT(demographics)
# 
# demographics <- demographics[site %in% c("baltimore", "canada")]
# demographics[, age_c := scale(age, center = TRUE, scale = FALSE)]
# 
# # add row index for merging
# demographics[, row_id := .I]
# 
# # ---- 3. PROCESS SHEETS (FAST) ----
# long_data <- rbindlist(lapply(names(all_sheets), function(sheet_name) {
#   
#   dt <- all_sheets[[sheet_name]]
#   
#   cat("Processing:", sheet_name, "| nrow:", nrow(dt), "| ncol:", ncol(dt), "\n")
#   
#   if (nrow(dt) == 0 || ncol(dt) == 0) return(NULL)
#   if (nrow(dt) != nrow(demographics)) {
#     cat("  --> skipping, row mismatch\n")
#     return(NULL)
#   }
#   
#   # ⚡ FAST: set vertex names in-place (no copy)
#   setnames(dt, paste0("V", seq_len(ncol(dt))))
#   
#   # parse sheet name
#   parts  <- strsplit(sheet_name, "_")[[1]]
#   part1  <- parts[1]
#   region <- parts[2]
#   
#   gradient <- if (grepl("^[Gg]", part1)) part1 else NA_character_
#   pc       <- if (grepl("^[Pp][Cc]", part1)) part1 else NA_character_
#   
#   # add row_id for merging
#   dt[, row_id := .I]
#   
#   # ⚡ FAST melt
#   dt_long <- data.table::melt(
#     dt,
#     id.vars = "row_id",
#     variable.name = "vertex",
#     value.name = "vertex_score"
#   )
#   
#   # add metadata
#   dt_long[, `:=`(
#     gradient = gradient,
#     pc       = pc,
#     region   = region
#   )]
#   
#   dt_long
# }), use.names = TRUE, fill = TRUE)
# 
# # ---- 4. MERGE DEMOGRAPHICS ----
# long_data <- merge(long_data, demographics, by = "row_id", all.x = TRUE)
# 
# # remove helper column
# long_data[, row_id := NULL]
# 
# # ---- sanity check ----
# cat("Missing group:", long_data[is.na(group), .N], "\n")
# cat("Unique subjects:", long_data[, uniqueN(subject)], "\n")
# 
# # ---- 5. SPLIT ----
# grad_dt <- long_data[!is.na(gradient)]
# pc_dt   <- long_data[!is.na(pc)]
# 
# # ---- 6. CAST (KEEP DEMOGRAPHICS!) ----
# id_vars <- c("subject", "region", "vertex", "group", "age_c", "sex", "site")
# 
# grad_wide <- dcast(
#   grad_dt,
#   as.formula(paste(paste(id_vars, collapse = " + "), "~ gradient")),
#   value.var = "vertex_score"
# )
# 
# pc_wide <- dcast(
#   pc_dt,
#   as.formula(paste(paste(id_vars, collapse = " + "), "~ pc")),
#   value.var = "vertex_score"
# )
# 
# # ---- 7. MERGE GRAD + PC ----
# final_data <- merge(
#   grad_wide,
#   pc_wide,
#   by = id_vars,
#   all = TRUE
# )
# 
# # ---- 8. CLEAN ORDER ----
# setcolorder(
#   final_data,
#   c(id_vars, setdiff(names(final_data), id_vars))
# )
# 
# # ---- 9. SAVE ----
# setwd("../data_tidy")
# saveRDS(final_data, "vertex_wise_evc.RDS")
# 
# cat("DONE\n")

# EXTRACT VERTEX-WISE INFO FOR ALL ROIS
# library to read matlab data formats into R
library(R.matlab)
library(dplyr)

# ── 1. Load MAT files ──────────────────────────────────────────────────────────
vertices_vis <- readMat("gradients_network_all.mat")
vertices_pca <- readMat("pca_network_all.mat")
demo <- fread(demographic_files$demographics)
demo <- demo[!is.na(age), ]

demo <- demo %>%
  mutate(subject = row_number())

# ── 2. Gradients → long (keep only G1–G3) ─────────────────────────────────────
grad_array <- vertices_vis$gradients.network.all  # [83 x 2527 x 10]

df_gradients <- expand.grid(
  subject  = 1:dim(grad_array)[1],
  vertex   = 1:dim(grad_array)[2],
  gradient = 1:dim(grad_array)[3]
) %>%
  arrange(gradient, vertex, subject) %>%
  mutate(gradient_value = as.vector(grad_array)) %>%
  filter(gradient <= 3) %>%                        # keep G1–G3 only
  mutate(gradient = paste0("G", gradient))

# ── 3. PCA → long ─────────────────────────────────────────────────────────────
pca_array <- vertices_pca$pca.network.all          # [83 x 2527 x 3]

df_pca <- expand.grid(
  subject  = 1:dim(pca_array)[1],
  vertex   = 1:dim(pca_array)[2],
  gradient = 1:dim(pca_array)[3]
) %>%
  arrange(gradient, vertex, subject) %>%
  mutate(pc_score = as.vector(pca_array)) %>%
  mutate(gradient = paste0("G", gradient))

# ── 4. Merge ───────────────────────────────────────────────────────────────────
df_merged <- left_join(df_gradients, df_pca,
                       by = c("subject", "vertex", "gradient"))

# ── 3. Merge ───────────────────────────────────────────────────────────────────
df_final <- df_merged %>%
  left_join(demo, by = "subject")

saveRDS(df_final, "../data_tidy/vertexwise_sfc.RDS")
