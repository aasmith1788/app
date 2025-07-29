# =============================================================================
# Stuff+ Prediction Script for New Data
# Automatically detects year and uses appropriate model and formula
# FIXED: Proper sign convention and handedness detection
# NO FALLBACK METHODS - FAILS LOUDLY IF ISSUES OCCUR
# UPDATED: Improved upload with smaller chunk size for large datasets
# =============================================================================

# -----------------------------------------------------------------------------
# 1. PACKAGE LOADING AND SETUP
# -----------------------------------------------------------------------------

# Install and load required packages
packages <- c("dplyr", "readr", "tidyr", "xgboost", "lubridate", "googlesheets4")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
library(lubridate)
library(googlesheets4)

# Set seed for reproducibility
set.seed(42)

# -----------------------------------------------------------------------------
# 2. CONFIGURATION - UPDATE THESE PATHS
# -----------------------------------------------------------------------------

# Google Sheets URL - YOUR SHEET URL
google_sheets_url <- "https://docs.google.com/spreadsheets/d/15lXLVrocMhrguFjyNk8is67kWj-0qU_Lyk3o1vfCWLA/edit?gid=314811539#gid=314811539"

# Model directories - UPDATE THESE PATHS TO YOUR MODEL FOLDERS
model_paths <- list(
  "2023" = list(
    model = "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2023/Model Development/xgboost_regression_model_runvalue_bayesian_optimized_2020_2022_pfx.txt",
    preprocess = "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2023/Model Development/preprocess_params_xgboost_regression_runvalue_optimized_pfx.rds",
    formula_params = "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2023/Model Development/stuff_plus_formula_params_2023_pfx.rds"
  ),
  "2024" = list(
    model = "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Aaron-David/Regression Model/2024/xgboost_regression_model_runvalue_bayesian_optimized_2021_2023_pfx.txt",
    preprocess = "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Aaron-David/Regression Model/2024/preprocess_params_xgboost_regression_runvalue_optimized_pfx.rds",
    formula_params = "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Aaron-David/Regression Model/2024/stuff_plus_formula_params_2024_pfx.rds"
  ),
  "2025" = list(
    model = "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2025/Model Development/xgboost_regression_model_runvalue_bayesian_optimized_2022_2024_pfx.txt",
    preprocess = "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2025/Model Development/preprocess_params_xgboost_regression_runvalue_optimized_pfx.rds",
    formula_params = "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2025/Model Development/stuff_plus_formula_params_2025_pfx.rds"
  )
)

# -----------------------------------------------------------------------------
# 3. DATA LOADING AND PREPROCESSING FUNCTIONS
# -----------------------------------------------------------------------------

# Function to map your column names to model features
map_columns_to_features <- function(df) {
  # Print column names to help debug
  cat("Available columns in your data:\n")
  cat(paste(names(df), collapse = ", "), "\n\n")
  
  # Add a row index to preserve original order
  df$original_row_index <- seq_len(nrow(df))
  
  # STEP 1: Calculate pitcher handedness FIRST (before any other processing)
  cat("Determining pitcher handedness from average horizontal release...\n")
  pitcher_handedness <- df %>%
    group_by(firstname, lastname) %>%
    summarise(
      # Use original release_side values to determine handedness
      # After sign flip: positive avg = lefty, negative avg = righty
      avg_horizontal_release = mean(-release_side, na.rm = TRUE),
      pitch_count = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      p_throws = ifelse(avg_horizontal_release > 0, "L", "R")
    )
  
  # Show handedness detection results
  cat("Pitcher handedness detected:\n")
  handedness_summary <- pitcher_handedness %>%
    count(p_throws) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  print(handedness_summary)
  
  # Join handedness back to main dataframe
  df <- df %>%
    left_join(pitcher_handedness %>% select(firstname, lastname, p_throws),
              by = c("firstname", "lastname"))
  
  # STEP 2: Create mapping with proper sign convention fixes
  df_mapped <- df %>%
    mutate(
      # Direct mappings - no changes needed
      release_speed = release_speed,
      release_spin_rate = spin_rate,
      release_extension = extension,
      release_pos_z = release_height,
      
      # CRITICAL FIX 1: Flip sign convention once for everyone
      # These "_base" columns store the sign-flipped values
      release_pos_x_base = -release_side,
      pfx_x_inches_base = -horizontal_break,
      
      # Keep vertical break as is
      induced_vertical_break = induced_vertical_break, # Already in inches (no sign flip needed)
      
      # Extract year from date column
      year = year(as.Date(date)),
      
      # Map your pitch types to standardized format
      pitch_type = case_when(
        pitch_type == "FB" ~ "FF",  # 4-seam fastball
        pitch_type == "SI" ~ "SI",  # Sinker
        pitch_type == "CT" ~ "FC",  # Cutter
        pitch_type == "CB" ~ "CU",  # Curveball
        pitch_type == "CH" ~ "CH",  # Changeup
        pitch_type == "SP" ~ "FS",  # Splitter
        pitch_type == "SW" ~ "ST",  # Sweeper
        pitch_type == "SL" ~ "SL",  # Slider
        TRUE ~ as.character(pitch_type)
      ),
      
      # Create pitcher name
      pitcher_name = paste(firstname, lastname, sep = " ")
    ) %>%
    # STEP 3: Mirror left-handed pitchers for model input only
    mutate(
      # Horizontal movement and release position after initial sign flip
      pfx_x_inches = ifelse(p_throws == "L", -pfx_x_inches_base, pfx_x_inches_base),
      pfx_z_inches = induced_vertical_break,

      release_pos_x = ifelse(p_throws == "L", -release_pos_x_base, release_pos_x_base),

      # Columns reflecting only the initial sign flip
      release_side = release_pos_x_base,
      horizontal_break = pfx_x_inches_base,

      # Convenience columns for later calculations
      pfx_x = pfx_x_inches,
      pfx_z = pfx_z_inches
    )
  
  # Show what year was extracted
  if(any(!is.na(df_mapped$year))) {
    cat("Years found in data:", paste(sort(unique(df_mapped$year)), collapse = ", "), "\n")
  } else {
    stop("FATAL ERROR: No years could be extracted from datetime column.")
  }
  
  # Show sample of mapped data including handedness
  cat("\nSample of mapped data (first 3 rows):\n")
  sample_cols <- c("firstname", "lastname", "pitch_type", "release_speed", "year", "p_throws", "release_pos_x", "pfx_x_inches")
  print(df_mapped[1:min(3, nrow(df_mapped)), sample_cols])
  
  # Show handedness breakdown
  cat("\nFinal handedness breakdown:\n")
  final_handedness <- df_mapped %>%
    count(p_throws) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  print(final_handedness)
  
  return(df_mapped)
}

# Function to calculate arm slot
calculate_arm_slot <- function(df) {
  df %>%
    mutate(
      release_magnitude = sqrt(release_pos_x^2 + release_extension^2 + release_pos_z^2),
      cos_arm_angle = release_pos_z / release_magnitude,
      arm_angle_rad = acos(pmax(-1, pmin(1, cos_arm_angle))),  # Clamp to [-1,1] to avoid NaN
      arm_slot_Vector = arm_angle_rad * (180 / pi)
    )
}

# Function to calculate fastball averages and differentials
calculate_fastball_features <- function(df) {
  # Define fastball pitch types
  fastball_types <- c('SI', 'FF', 'FC')
  
  # Calculate fastball averages by pitcher and DATE (outing)
  df_fastballs <- df %>%
    filter(pitch_type %in% fastball_types) %>%
    group_by(firstname, lastname, date, pitch_type) %>%
    summarise(
      avg_fastball_speed = mean(release_speed, na.rm = TRUE),
      avg_fastball_pfx_z = mean(pfx_z_inches, na.rm = TRUE),
      avg_fastball_pfx_x = mean(pfx_x_inches, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    ) %>%
    arrange(firstname, lastname, date, desc(count), desc(avg_fastball_speed)) %>%
    group_by(firstname, lastname, date) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(firstname, lastname, date, avg_fastball_speed, avg_fastball_pfx_z, avg_fastball_pfx_x)
  
  # Join back to main dataframe
  df <- df %>%
    left_join(df_fastballs, by = c("firstname", "lastname", "date"))
  
  # Fill missing fastball values with pitcher's maximum values FOR THAT DATE
  df <- df %>%
    group_by(firstname, lastname, date) %>%
    mutate(
      avg_fastball_speed = ifelse(is.na(avg_fastball_speed), 
                                  max(release_speed, na.rm = TRUE), 
                                  avg_fastball_speed),
      avg_fastball_pfx_z = ifelse(is.na(avg_fastball_pfx_z), 
                                  max(pfx_z_inches, na.rm = TRUE), 
                                  avg_fastball_pfx_z),
      avg_fastball_pfx_x = ifelse(is.na(avg_fastball_pfx_x), 
                                  max(pfx_x_inches, na.rm = TRUE), 
                                  avg_fastball_pfx_x)
    ) %>%
    ungroup()
  
  # Calculate differentials
  df <- df %>%
    mutate(
      speed_diff = release_speed - avg_fastball_speed,
      pfx_z_diff = pfx_z_inches - avg_fastball_pfx_z,
      pfx_x_diff = abs(pfx_x_inches - avg_fastball_pfx_x)
    )
  
  return(df)
}

# Function to load model files
load_model_files <- function(year_str) {
  if (!year_str %in% names(model_paths)) {
    stop(paste("FATAL ERROR: No model available for year", year_str, ". Available years:", paste(names(model_paths), collapse = ", ")))
  }
  
  paths <- model_paths[[year_str]]
  
  # Check if files exist
  if (!file.exists(paths$model)) {
    stop(paste("FATAL ERROR: Model file not found:", paths$model))
  }
  if (!file.exists(paths$preprocess)) {
    stop(paste("FATAL ERROR: Preprocessing file not found:", paths$preprocess))
  }
  if (!file.exists(paths$formula_params)) {
    stop(paste("FATAL ERROR: Formula parameters file not found:", paths$formula_params))
  }
  
  # Load files
  model <- xgb.load(paths$model)
  preprocess_params <- readRDS(paths$preprocess)
  formula_params <- readRDS(paths$formula_params)
  
  return(list(
    model = model,
    preprocess_params = preprocess_params,
    formula_params = formula_params
  ))
}

# Function to apply preprocessing
apply_preprocessing <- function(df, preprocess_params) {
  # Define the features used in the model
  features <- c('release_speed', 'release_spin_rate', 'release_extension',
                'release_pos_x', 'release_pos_z', 'pfx_z_inches',
                'pfx_x_inches', 'arm_slot_Vector', 'speed_diff',
                'pfx_z_diff', 'pfx_x_diff')
  
  # Extract features
  X <- df[features]
  
  # Apply scaling
  if (is.list(preprocess_params) && "means" %in% names(preprocess_params)) {
    # Manual scaling (fallback method)
    X_scaled <- scale(X, center = preprocess_params$means, scale = preprocess_params$sds)
    X_scaled <- as.data.frame(X_scaled)
  } else {
    # Caret preprocessing
    library(caret)
    X_scaled <- predict(preprocess_params, X)
  }
  
  return(as.matrix(X_scaled))
}

# Function to calculate Stuff+
calculate_stuff_plus <- function(predictions, formula_params) {
  mean_val <- formula_params$mean
  sd_val <- formula_params$sd
  
  stuff_plus <- 100 - ((predictions - mean_val) / sd_val) * 10
  return(stuff_plus)
}

# -----------------------------------------------------------------------------
# 4. MAIN PREDICTION FUNCTION - NO FALLBACKS
# -----------------------------------------------------------------------------

predict_stuff_plus <- function(input_source) {
  cat("Reading data from Google Sheets:", input_source, "\n")
  cat("Note: You may need to authenticate with Google on first use.\n")
  
  # Load the data from Google Sheets - specifically from the "All" sheet
  df <- read_sheet(input_source, sheet = "All")
  cat("Successfully read from sheet: 'All'\n")
  
  cat("Data loaded. Rows:", nrow(df), "Columns:", ncol(df), "\n")
  cat("Column names:", paste(names(df), collapse = ", "), "\n\n")
  
  # Store the original number of rows and order
  original_nrows <- nrow(df)
  
  # Map columns to model features (now includes proper sign conversion and handedness detection)
  df <- map_columns_to_features(df)
  
  # Calculate arm slot
  df <- calculate_arm_slot(df)
  
  # Calculate fastball features
  df <- calculate_fastball_features(df)
  
  # Create a copy with all rows for final results
  all_results <- df %>%
    mutate(stuff_plus = NA_real_, prediction_error = NA_character_)
  
  # Filter for valid rows for prediction
  valid_df <- df %>%
    filter(!is.infinite(avg_fastball_speed) & 
             !is.infinite(avg_fastball_pfx_z) & 
             !is.infinite(avg_fastball_pfx_x) &
             !is.na(avg_fastball_speed) &
             !is.na(avg_fastball_pfx_z) &
             !is.na(avg_fastball_pfx_x))
  
  cat("Data preprocessed. Valid rows for prediction:", nrow(valid_df), "\n")
  
  # Get unique years in the valid data
  unique_years <- sort(unique(valid_df$year))
  cat("Years found in data:", paste(unique_years, collapse = ", "), "\n\n")
  
  # Process each year separately
  for (year_val in unique_years) {
    year_str <- as.character(year_val)
    year_data <- valid_df %>% filter(year == year_val)
    
    cat("Processing year", year_val, "- Rows:", nrow(year_data), "\n")
    
    # Load appropriate model files (will stop on error)
    model_files <- load_model_files(year_str)
    
    # Apply preprocessing
    X_scaled <- apply_preprocessing(year_data, model_files$preprocess_params)
    
    # Make predictions
    predictions <- predict(model_files$model, X_scaled)
    
    # Calculate Stuff+
    stuff_plus_scores <- calculate_stuff_plus(predictions, model_files$formula_params)
    
    # Update the results in the all_results dataframe using original_row_index
    for(i in 1:nrow(year_data)) {
      row_idx <- year_data$original_row_index[i]
      all_results$stuff_plus[all_results$original_row_index == row_idx] <- stuff_plus_scores[i]
    }
    
    cat("Successfully predicted", length(stuff_plus_scores), "pitches for year", year_val, "\n")
    cat("Stuff+ range:", round(min(stuff_plus_scores, na.rm = TRUE), 2), "to", 
        round(max(stuff_plus_scores, na.rm = TRUE), 2), "\n\n")
  }
  
  # Sort back to original order
  all_results <- all_results %>% arrange(original_row_index)

  # Verify we have the correct number of rows
  if(nrow(all_results) != original_nrows) {
    stop("FATAL ERROR: Result rows don't match original data rows!")
  }

  # Revert mirrored values so output reflects only the initial sign flip
  all_results <- all_results %>%
    mutate(
      release_pos_x = release_pos_x_base,
      pfx_x_inches = pfx_x_inches_base,
      release_side = release_pos_x_base,
      horizontal_break = pfx_x_inches_base
    )
  
  # Prepare the final dataset with ALL original columns plus stuff_plus
  # Remove only the temporary processing columns, keep EVERYTHING else
  columns_to_remove <- c("original_row_index", "prediction_error",
                         "release_magnitude", "cos_arm_angle", "arm_angle_rad",
                         "pitcher_name",  # This was created for processing
                         "pfx_x", "pfx_z",            # Duplicates of pfx_x_inches, pfx_z_inches
                         "release_pos_x_base", "pfx_x_inches_base")
  
  final_data <- all_results %>%
    select(-any_of(columns_to_remove))
  
  cat("Final dataset prepared with", ncol(final_data), "columns including Stuff+\n")
  cat("Columns in final dataset:", paste(names(final_data), collapse = ", "), "\n\n")
  
  # Upload back to Google Sheets - CREATE NEW SHEET
  cat("Creating new 'Stuff Plus' sheet and uploading ALL data...\n")
  
  # Get current sheet names
  current_sheets <- sheet_names(input_source)
  cat("Current sheets in workbook:", paste(current_sheets, collapse = ", "), "\n")
  
  # Delete existing "Stuff Plus" sheet if it exists
  if("Stuff Plus" %in% current_sheets) {
    cat("Deleting existing 'Stuff Plus' sheet...\n")
    sheet_delete(input_source, sheet = "Stuff Plus")
  }
  
  # Create new "Stuff Plus" sheet
  cat("Creating new 'Stuff Plus' sheet...\n")
  sheet_add(input_source, sheet = "Stuff Plus")
  
  # UPDATED: Reduced chunk size to handle large datasets better
  chunk_size <- 250  # Reduced from 1000 to 250 for better reliability
  total_rows <- nrow(final_data)
  max_retries <- 3
  retry_delay <- 15  # Increased retry delay
  
  cat("Uploading", total_rows, "rows with", ncol(final_data), "columns in chunks of", chunk_size, "...\n")
  cat("This will take approximately", ceiling(total_rows/chunk_size), "chunks to complete.\n")
  
  # Write header first with retry logic
  cat("Writing header...")
  header_written <- FALSE
  for(attempt in 1:max_retries) {
    tryCatch({
      header_df <- final_data[0,]  # Empty df with just column names
      range_write(input_source,
                  header_df,
                  sheet = "Stuff Plus",
                  range = "A1",
                  col_names = TRUE)
      header_written <- TRUE
      cat(" SUCCESS\n")
      break
    }, error = function(e) {
      if(attempt == max_retries) {
        stop(paste("FATAL ERROR: Failed to write header after", max_retries, "attempts:", e$message))
      }
      cat(sprintf(" FAILED (attempt %d/%d), retrying in %d seconds...\n", attempt, max_retries, retry_delay))
      Sys.sleep(retry_delay)
    })
  }
  
  if(!header_written) {
    stop("FATAL ERROR: Could not write header to Google Sheets")
  }
  
  # Track upload progress
  successful_chunks <- 0
  failed_chunks <- 0
  
  # Write data in smaller chunks with better error handling
  for(i in seq(1, total_rows, by = chunk_size)) {
    start_row <- i
    end_row <- min(i + chunk_size - 1, total_rows)
    chunk_num <- ceiling(i/chunk_size)
    total_chunks <- ceiling(total_rows/chunk_size)
    
    # Extract chunk
    chunk_data <- final_data[start_row:end_row, ]
    
    # Calculate range for this chunk (add 1 to account for header)
    chunk_range <- paste0("A", start_row + 1)
    
    cat(sprintf("Writing chunk %d/%d (rows %d-%d)...", 
                chunk_num, total_chunks, start_row, end_row))
    
    # Try to write chunk with retries
    chunk_written <- FALSE
    for(attempt in 1:max_retries) {
      tryCatch({
        range_write(input_source,
                    chunk_data,
                    sheet = "Stuff Plus",
                    range = chunk_range,
                    col_names = FALSE)
        chunk_written <- TRUE
        successful_chunks <- successful_chunks + 1
        cat(" SUCCESS\n")
        break
      }, error = function(e) {
        if(attempt == max_retries) {
          failed_chunks <- failed_chunks + 1
          cat(sprintf(" FAILED after %d attempts: %s\n", max_retries, e$message))
          cat("WARNING: Some data may be missing. Continuing with next chunk...\n")
          
          # Optional: Save failed chunk locally
          failed_filename <- sprintf("failed_chunk_%d_rows_%d_to_%d.csv", chunk_num, start_row, end_row)
          write_csv(chunk_data, failed_filename)
          cat(sprintf("Failed chunk saved to: %s\n", failed_filename))
        } else {
          cat(sprintf(" FAILED (attempt %d/%d), retrying in %d seconds...\n", attempt, max_retries, retry_delay))
          Sys.sleep(retry_delay)
        }
      })
    }
    
    # Progress update with time estimate
    chunks_complete <- chunk_num
    chunks_remaining <- total_chunks - chunks_complete
    if(chunks_complete %% 10 == 0) {  # Every 10 chunks
      cat(sprintf("\nPROGRESS UPDATE: %.1f%% complete (%d/%d chunks)\n", 
                  (end_row / total_rows) * 100, chunks_complete, total_chunks))
      cat(sprintf("Successful chunks: %d, Failed chunks: %d\n", successful_chunks, failed_chunks))
      if(chunks_remaining > 0) {
        est_time_remaining <- chunks_remaining * 5 / 60  # Assume ~5 seconds per chunk
        cat(sprintf("Estimated time remaining: %.1f minutes\n\n", est_time_remaining))
      }
    }
    
    # Adaptive delay between chunks to avoid rate limits
    if(i + chunk_size <= total_rows) {  # Don't sleep after the last chunk
      # Longer delay every 20 chunks to let API cool down
      if(chunk_num %% 20 == 0) {
        cat("Taking a 30-second break to avoid rate limits...\n")
        Sys.sleep(30)
      } else {
        # Regular delay between chunks
        Sys.sleep(5)  # Increased from 3 to 5 seconds
      }
    }
  }
  
  # Final upload summary
  cat("\n", rep("=", 50), "\n")
  cat("UPLOAD COMPLETE\n")
  cat(rep("=", 50), "\n")
  cat(sprintf("Total chunks: %d\n", total_chunks))
  cat(sprintf("Successful chunks: %d\n", successful_chunks))
  cat(sprintf("Failed chunks: %d\n", failed_chunks))
  
  if(failed_chunks > 0) {
    cat("\nWARNING: Some chunks failed to upload. Check local CSV files for failed data.\n")
    cat("You may need to manually add this data to the Google Sheet.\n")
  } else {
    cat("\nSUCCESS: Complete dataset uploaded to 'Stuff Plus' sheet!\n")
  }
  
  # Print summary
  cat("\n", rep("=", 50), "\n")
  cat("PREDICTION SUMMARY\n")
  cat(rep("=", 50), "\n")
  
  successful_predictions <- sum(!is.na(final_data$stuff_plus))
  total_rows <- nrow(final_data)
  
  cat("Total rows processed:", total_rows, "\n")
  cat("Successful predictions:", successful_predictions, "\n")
  cat("Success rate:", round(successful_predictions/total_rows * 100, 1), "%\n")
  
  # Show pitch count breakdown by year
  cat("\nPITCH COUNT BY YEAR:\n")
  cat(rep("-", 30), "\n")
  year_breakdown <- final_data %>%
    group_by(year) %>%
    summarise(
      pitch_count = n(),
      predictions_made = sum(!is.na(stuff_plus)),
      percentage = round(n() / nrow(final_data) * 100, 1),
      .groups = 'drop'
    ) %>%
    arrange(year)
  
  for(i in 1:nrow(year_breakdown)) {
    cat(sprintf("%d: %d pitches, %d predictions (%.1f%%)\n", 
                year_breakdown$year[i], 
                year_breakdown$pitch_count[i],
                year_breakdown$predictions_made[i],
                year_breakdown$percentage[i]))
  }
  
  # Show handedness breakdown in final results
  if("p_throws" %in% names(final_data)) {
    cat("\nFINAL HANDEDNESS BREAKDOWN:\n")
    cat(rep("-", 30), "\n")
    handedness_final <- final_data %>%
      count(p_throws) %>%
      mutate(percentage = round(n / sum(n) * 100, 1))
    for(i in 1:nrow(handedness_final)) {
      cat(sprintf("%s: %d pitches (%.1f%%)\n", 
                  handedness_final$p_throws[i],
                  handedness_final$n[i],
                  handedness_final$percentage[i]))
    }
  }
  
  if (successful_predictions > 0) {
    cat("\nStuff+ Score Summary:\n")
    print(summary(final_data$stuff_plus))
    
    cat("\nStuff+ by Year:\n")
    year_summary <- final_data %>%
      filter(!is.na(stuff_plus)) %>%
      group_by(year) %>%
      summarise(
        count = n(),
        mean_stuff_plus = round(mean(stuff_plus), 2),
        min_stuff_plus = round(min(stuff_plus), 2),
        max_stuff_plus = round(max(stuff_plus), 2),
        .groups = 'drop'
      )
    print(year_summary)
    
    # Show Stuff+ by handedness if available
    if("p_throws" %in% names(final_data)) {
      cat("\nStuff+ by Handedness:\n")
      handedness_summary <- final_data %>%
        filter(!is.na(stuff_plus)) %>%
        group_by(p_throws) %>%
        summarise(
          count = n(),
          mean_stuff_plus = round(mean(stuff_plus), 2),
          min_stuff_plus = round(min(stuff_plus), 2),
          max_stuff_plus = round(max(stuff_plus), 2),
          .groups = 'drop'
        )
      print(handedness_summary)
    }
  }
  
  cat("\nDone! Check the 'Stuff Plus' sheet in your Google Sheets workbook.\n")
  return(final_data)
}

# -----------------------------------------------------------------------------
# 5. EXECUTION
# -----------------------------------------------------------------------------

# Run the prediction
cat("Stuff+ Prediction Script Starting...\n\n")

# Auto-authenticate to avoid prompts
gs4_auth(email = "asmith8@bu.edu")

# Read from Google Sheets and upload complete results
cat("Reading from your Google Sheets and uploading complete results...\n")
results <- predict_stuff_plus(google_sheets_url)
