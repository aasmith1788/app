# combined_ppp_radars_module.R
# CombinedPPPRadars.R converted to Shiny module format

# Load libraries
library(tidyverse)
library(shiny)
library(fmsb)
library(googlesheets4)
library(ggplot2)

# Global definitions
gs4_deauth()

# Google Sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1C4llBzMOXvOg_DQYraNQ3wxcqDZNMTyM2fkA0X2DCi4"

# Metric column names (internal)
metric_columns <- c(
  "bw_kg", "net_peak_vertical_force_n", "jump_height_imp_mom_in_inches_in",
  "p1_concentric_impulse_n_s", "peak_power_w", "rsi_modified_m_s",
  "eccentric_deceleration_impulse_n_s", "eccentric_deceleration_rfd_n_s",
  "eccentric_peak_force_n", "force_at_zero_velocity_n",
  "countermovement_depth_cm", "braking_phase_duration_ms",
  "eccentric_duration_ms", "concentric_peak_force_n",
  "concentric_duration_ms", "concentric_peak_velocity_m_s",
  "concentric_impulse_100ms_n_s", "concentric_impulse_n_s",
  "eccentric_braking_impulse_n_s", "contraction_time_ms",
  "eccentric_braking_rfd_n_s", "PPP_Power_n_m_s", "relative_PPP",
  "relative_eccentric_peak_force",
  "relative_IMTP"
)

# Friendly‐label choices for UI (name = display, value = column)
friendly_choices <- c(
  "Body Weight (kg)"                     = "bw_kg",
  "IMTP Net Peak Vertical Force (N)"     = "net_peak_vertical_force_n",
  "Jump Height (in)"                     = "jump_height_imp_mom_in_inches_in",
  "P1 Concentric Impulse (N·s)"          = "p1_concentric_impulse_n_s",
  "Peak Power (W)"                       = "peak_power_w",
  "MRSI"                                 = "rsi_modified_m_s",
  "Ecc. Decel Impulse (N·s)"             = "eccentric_deceleration_impulse_n_s",
  "Eccentric Decel RFD"                  = "eccentric_deceleration_rfd_n_s",
  "Eccentric Peak Force"                 = "eccentric_peak_force_n",
  "Force at Zero Velocity (N)"           = "force_at_zero_velocity_n",
  "Countermovement Depth (cm)"           = "countermovement_depth_cm",
  "Braking Phase Duration (ms)"          = "braking_phase_duration_ms",
  "Eccentric Duration (ms)"              = "eccentric_duration_ms",
  "Concentric Peak Force (N)"            = "concentric_peak_force_n",
  "Concentric Duration (ms)"             = "concentric_duration_ms",
  "Concentric Peak Velo"                 = "concentric_peak_velocity_m_s",
  "Concentric Impulse @100ms (N·s)"      = "concentric_impulse_100ms_n_s",
  "Concentric Impulse"                   = "concentric_impulse_n_s",
  "Ecc. Braking Impulse (N·s)"           = "eccentric_braking_impulse_n_s",
  "Contraction Time (ms)"                = "contraction_time_ms",
  "Ecc. Braking RFD (N/s)"               = "eccentric_braking_rfd_n_s",
  "PPP Power"                            = "PPP_Power_n_m_s",
  "Relative Premier Power (N*m/s/kg)"    = "relative_PPP",
  "Relative Ecc. Peak Force (N/kg)"      = "relative_eccentric_peak_force",
  "Relative IMTP (N/kg)"                 = "relative_IMTP"
)

# LASSO selected metrics (from your coefficients image) - now used as default
default_selected_metrics <- c(
  "PPP_Power_n_m_s",
  "rsi_modified_m_s",
  "concentric_impulse_n_s",
  "concentric_peak_velocity_m_s",
  "eccentric_peak_force_n",
  "eccentric_deceleration_rfd_n_s"
)

# Power/force variables for log transformation (from standalone script)
power_force_vars <- c("peak_power_w", "avg_power_w", "peak_force_n", "avg_force_n",
                      "impulse_ns", "rfd_max", "rfd_avg")

# ---------------------------------------------------------------------------------
# UI Module
combinedPPPRadarsUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$head(tags$style(HTML("
      body { background: #fff; color: #000; }
      .title { text-align: center; font-size: 24px; font-weight: normal; }
      .score {
        text-align: center;
        font-size: 22px;
        font-weight: normal;
        margin-bottom: 15px;
      }
      .score > div { display: inline-block; }

      /* Sidebar styling - clean gray background */
      .well {
        background-color: #f5f5f5;
        border: none;
        border-radius: 0;
        box-shadow: none;
        border-right: 2px solid #ddd;
        padding: 15px;
      }

      /* Main title styling - integrated into sidebar */
      .main-title {
        background-color: #f5f5f5;
        text-align: center;
        padding: 15px;
        margin: 0;
        font-size: 14px;
        font-weight: normal;
        color: #333;
        border-bottom: 1px solid #ddd;
      }

      /* Table styling */
      .metrics-table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 15px;
      }
      .metrics-table th {
        background-color: #f2f2f2;
        text-align: left;
        padding: 8px;
        border-bottom: 1px solid #ddd;
      }
      .metrics-table td {
        padding: 8px;
        border-bottom: 1px solid #eee;
      }
      .metrics-table tr:nth-child(even) {
        background-color: #f9f9f9;
      }
      /* Download button styling */
      .download-btn {
        margin-top: 20px;
        background-color: #FFDE00;
        color: #000;
        font-weight: bold;
        border: 1px solid #ccc;
      }
      .download-btn:hover {
        background-color: #FFD700;
      }

      /* Remove professional box styling - now just clean sections */
      .filter-section {
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 1px solid #ddd;
      }

      .filter-section:last-child {
        border-bottom: none;
      }

      /* Variable selector box styling */
      .variable-selector-box {
        border: 1px solid #ccc;
        background-color: white;
        max-height: 100px;
        overflow-y: auto;
        margin-bottom: 0;
        border-radius: 3px;
      }

      /* Date selector box styling */
      .date-selector-box {
        border: 1px solid #ccc;
        background-color: white;
        max-height: 80px;
        overflow-y: auto;
        margin-bottom: 0;
        border-radius: 3px;
      }

      /* Selection count bar styling */
      .selection-count-bar {
        padding: 4px 8px;
        background-color: #f0f0f0;
        border: 1px solid #ccc;
        border-top: none;
        font-size: 10px;
        color: #555;
        text-align: left;
        border-radius: 0 0 3px 3px;
      }

      /* Custom checkbox styling to match the professional look */
      .checkbox {
        margin-bottom: 0px;
        padding: 2px 8px;
        font-size: 11px;
      }

      .checkbox input[type='checkbox'] {
        margin-right: 6px;
      }

      .checkbox label {
        font-weight: normal;
        margin-bottom: 0;
      }

      /* Section headers */
      .section-header {
        margin-bottom: 8px;
        font-size: 14px;
        font-weight: bold;
        color: #333;
      }

      .section-subheader {
        font-size: 12px;
        color: #333;
        font-weight: bold;
        display: block;
        margin-bottom: 6px;
      }

      /* Compact form controls */
      .form-group {
        margin-bottom: 8px;
      }

      .form-control {
        font-size: 12px;
        padding: 4px 8px;
      }

      /* Compact hr styling */
      hr {
        margin: 8px 0 6px 0 !important;
        border-color: #ddd;
      }
    "))),
    absolutePanel(
      top = 10, right = 10, fixed = TRUE, draggable = FALSE,
      img(src = "P3_barbell.jpg", height = "100px", width = "120px")
    ),

    sidebarLayout(
      sidebarPanel(
        # Title integrated into sidebar
        div(
          class = "main-title",
          "Premier Pitching Performance: Force & Power"
        ),

        selectInput(ns("select_athlete"), "Select Athlete:",
                    choices = NULL),

        # Force score date selector below athlete selection
        uiOutput(ns("force_score_selector")),

        # NEW: Single unified Radar Chart Variables section with date selection
        div(
          class = "filter-section",
          h4("Radar Chart Variables", class = "section-header"),
          p("Select 3-6 variables for the radar chart:", style = "font-size: 10px; color: #666; margin-bottom: 6px; margin-top: 0;"),

          # Professional dropdown-style container for variables
          div(
            class = "variable-selector-box",
            checkboxGroupInput(
              ns("radar_metrics"),
              NULL,
              choices = friendly_choices,
              selected = default_selected_metrics,
              inline = FALSE
            )
          ),

          # Selection count bar at bottom
          div(
            class = "selection-count-bar",
            textOutput(ns("radar_selection_count"), inline = TRUE)
          ),

          # Validation message
          conditionalPanel(
            condition = paste0("output['", ns("radar_validation"), "']"),
            div(style = "color: red; font-weight: bold; margin-top: 6px; font-size: 10px;",
                textOutput(ns("radar_validation_text")))
          ),

          # Test Dates section (within same box)
          hr(style = "margin: 8px 0 6px 0; border-color: #ddd;"),
          span("Test Dates for Radar:", class = "section-subheader"),

          # Date selector container
          div(
            class = "date-selector-box",
            uiOutput(ns("radar_date_selector"))
          ),

          # Comparison Lines section (within same box)
          hr(style = "margin: 8px 0 6px 0; border-color: #ddd;"),
          span("Comparison Lines:", class = "section-subheader"),
          div(style = "margin-left: 0;",
              checkboxInput(ns("show_hs_avg"), "Show HS Average", value = FALSE),
              checkboxInput(ns("show_college_avg"), "Show College Average", value = FALSE)
          ),

          # Velocity Group Average section (within same box)
          hr(style = "margin: 8px 0 6px 0; border-color: #ddd;"),
          span("Velocity Group Average:", class = "section-subheader"),
          selectInput(ns("select_velocity_group"),"",
                      choices = c("All","<80","80-85","85-90",">90"),
                      selected = "All")
        ),

        # Trend Chart section
        div(
          class = "filter-section",
          h4("Trend Chart", class = "section-header"),
          selectInput(ns("trend_metric"), "",
                      choices = friendly_choices,
                      selected = default_selected_metrics[1])
        ),
        # Data Table Metrics section
        div(
          class = "filter-section",
          h4("Data Table Metrics", class = "section-header"),
          p("Select up to 6 variables for the data table:", style = "font-size: 10px; color: #666; margin-bottom: 6px; margin-top: 0;"),

          # Professional dropdown-style container for data table metrics
          div(
            class = "variable-selector-box",
            checkboxGroupInput(
              ns("data_table_metrics"),
              NULL,
              choices = friendly_choices,
              selected = c("net_peak_vertical_force_n", "peak_power_w", "rsi_modified_m_s",
                           "countermovement_depth_cm", "concentric_peak_velocity_m_s", "eccentric_peak_force_n"),
              inline = FALSE
            )
          ),

          # Selection count bar at bottom
          div(
            class = "selection-count-bar",
            textOutput(ns("data_table_selection_count"), inline = TRUE)
          ),

          # Validation message
          conditionalPanel(
            condition = paste0("output['", ns("data_table_validation"), "']"),
            div(style = "color: red; font-weight: bold; margin-top: 6px; font-size: 10px;",
                textOutput(ns("data_table_validation_text")))
          )
        ),
        hr(),
        # Add download button
        downloadButton(ns("downloadPlot"), "Download Visualization", class = "download-btn btn-block")
      ),

      mainPanel(
        plotOutput(ns("combinedVisualization"), height = "1200px")
      )
    )
  )
}

# ---------------------------------------------------------------------------------
# Server Module
combinedPPPRadarsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Initialize athlete choices
    observe({
      data <- read_sheet(sheet_url, sheet="R_Data_Setup")
      updateSelectInput(
        session,
        "select_athlete",
        choices = sort(unique(data$athlete_name)),
        selected = unique(data$athlete_name)[1]
      )
    })

    # Validation for radar metrics selection (3-6 metrics)
    output$radar_validation <- reactive({
      length(input$radar_metrics) < 3 || length(input$radar_metrics) > 6
    })
    outputOptions(output, "radar_validation", suspendWhenHidden = FALSE)

    # Add the selection count output
    output$radar_selection_count <- renderText({
      count <- length(input$radar_metrics)
      paste0("✓ ", count, " of 6 variables selected")
    })

    # Validation for data table metrics selection (allow 0-6 metrics)
    output$data_table_validation <- reactive({
      length(input$data_table_metrics) > 6
    })
    outputOptions(output, "data_table_validation", suspendWhenHidden = FALSE)

    # Add the data table selection count output
    output$data_table_selection_count <- renderText({
      count <- length(input$data_table_metrics)
      paste0("✓ ", count, " of 6 variables selected")
    })

    output$data_table_validation_text <- renderText({
      if (length(input$data_table_metrics) > 6) {
        "Please select no more than 6 metrics for the data table."
      }
    })

    # 1. Re‑pull Google Sheet every 5 minutes
    data <- reactive({
      gs4_deauth()
      read_sheet(sheet_url, sheet = "R_Data_Setup") %>%
        rename(Test_Date = test_date) %>%
        mutate(Test_Date = as.Date(Test_Date, "%Y-%m-%d"))
    })

    # 2. Load LASSO preprocessing data
    lasso_preprocessing <- reactive({
      gs4_deauth()
      read_sheet(sheet_url, sheet = "LASSO_Preprocessing")
    })

    # 3. Load LASSO coefficients
    lasso_coefficients <- reactive({
      gs4_deauth()
      read_sheet(sheet_url, sheet = "LASSO_Coefficients")
    })

    # 4. Load all velocity prediction rows (multiple dates possible)
    velocity_predictions <- reactive({
      gs4_deauth()
      read_sheet(sheet_url, sheet = "Velo Predictions") %>%
        mutate(test_date = as.Date(test_date))
    })

    # 4. Z‑scores reactive (used for table outputs)
    #    Reference group is 90-92 mph throwers (mirrors Force Dis logic)
    zdata <- reactive({
      df <- data()
      ref <- df %>% filter(max_fb_velo >= 90, max_fb_velo <= 92)
      means <- sapply(ref[metric_columns], mean, na.rm = TRUE)
      sds   <- sapply(ref[metric_columns], sd,  na.rm = TRUE)
      sds[sds == 0] <- 1
      df %>%
        mutate(across(all_of(metric_columns),
                      ~ (. - means[cur_column()]) / sds[cur_column()],
                      .names = "{col}_z"))
    })

    # Percentile data for radar plot
    pctdata <- reactive({
      df <- data()
      df %>%
        mutate(across(all_of(metric_columns),
                      ~ percent_rank(.) * 100,
                      .names = "{col}_pct"))
    })

    # 5. FIXED LASSO-based composite score calculation (matching standalone script)
    lasso_predictions <- reactive({
      df <- data()
      preprocessing <- lasso_preprocessing()
      coefficients <- lasso_coefficients()

      # Prepare data for prediction (same preprocessing as training) - EXACTLY LIKE STANDALONE SCRIPT
      df_clean <- df %>%
        select(-athlete_name, -Test_Date, -velo_group, -level_group, -school)

      # Apply logarithmic transformation to power/force variables - EXACTLY LIKE STANDALONE SCRIPT
      x_all <- df_clean %>% select(-max_fb_velo)
      existing_vars_all <- intersect(power_force_vars, names(x_all))

      cat("Applying log transformation to:", paste(existing_vars_all, collapse = ", "), "\n")

      x_all_transformed <- x_all %>%
        mutate(across(all_of(existing_vars_all), ~ log(.x + 1)))

      # Recreate the preProcess object structure from saved parameters - EXACTLY LIKE STANDALONE SCRIPT
      recreated_preproc <- list(
        mean = setNames(preprocessing$mean, preprocessing$metric),
        std = setNames(preprocessing$std, preprocessing$metric),
        method = list(center = TRUE, scale = TRUE)
      )
      class(recreated_preproc) <- "preProcess"

      # Apply standardization exactly like in training - EXACTLY LIKE STANDALONE SCRIPT
      x_all_scaled <- x_all_transformed
      for(var in names(recreated_preproc$mean)) {
        if(var %in% names(x_all_scaled)) {
          x_all_scaled[[var]] <- (x_all_scaled[[var]] - recreated_preproc$mean[[var]]) / recreated_preproc$std[[var]]
        }
      }

      # Convert to matrix and make predictions using saved coefficients - EXACTLY LIKE STANDALONE SCRIPT
      x_all_matrix <- as.matrix(x_all_scaled)

      # Reconstruct coefficient vector in correct order
      all_features <- colnames(x_all_matrix)
      coef_vector <- rep(0, length(all_features))
      names(coef_vector) <- all_features

      # Fill in coefficients from saved model
      for(i in 1:nrow(coefficients)) {
        feature_name <- coefficients$feature[i]
        if(feature_name != "(Intercept)" && feature_name %in% names(coef_vector)) {
          coef_vector[feature_name] <- coefficients$coefficient[i]
        }
      }

      # Calculate predictions: intercept + X * beta - EXACTLY LIKE STANDALONE SCRIPT
      intercept <- coefficients$coefficient[coefficients$feature == "(Intercept)"]
      predictions <- as.vector(intercept + x_all_matrix %*% coef_vector)

      # Add predictions back to original dataframe
      df_with_predictions <- df %>%
        mutate(lasso_prediction = predictions)

      return(df_with_predictions)
    })



    # Force score date selector UI - single selection, default latest
    output$force_score_selector <- renderUI({
      req(input$select_athlete)
      dates <- velocity_predictions() %>%
        filter(player_name == input$select_athlete) %>%
        pull(test_date) %>% unique() %>% sort(decreasing = TRUE)
      selectInput(session$ns("force_scores_date"), "Force Score:",
                  choices = dates, selected = dates[1])
    })

    # 9. UPDATED: Date selector UI for radar plot only
    output$radar_date_selector <- renderUI({
      req(input$select_athlete)
      dates <- data() %>%
        filter(athlete_name == input$select_athlete) %>%
        pull(Test_Date) %>% unique() %>% sort()
      checkboxGroupInput(session$ns("radar_test_dates"), NULL,
                         choices=dates, selected=dates)
    })

    # 10. UPDATED: COMBINED VISUALIZATION with separate date handling
    create_visualization <- function() {
      req(input$select_athlete, input$radar_test_dates,
          input$data_table_metrics, input$trend_metric,
          input$radar_metrics, input$force_scores_date)

      # Validate radar metrics selection
      if (length(input$radar_metrics) < 3 || length(input$radar_metrics) > 6) {
        # Show error message instead of plot
        plot.new()
        text(0.5, 0.5, "Please select 3-6 metrics for the radar plot",
             cex = 2, col = "red", font = 2)
        return()
      }

      # Validate data table metrics selection (allow 0-6 metrics)
      if (length(input$data_table_metrics) > 6) {
        # Show error message instead of plot
        plot.new()
        text(0.5, 0.5, "Please select no more than 6 metrics for the data table",
             cex = 2, col = "red", font = 2)
        return()
      }

      # --- PART 0: Layout Setup ---
      # Set up a layout with 4 rows for our elements
      layout(matrix(c(1,2,3,4), nrow=4, byrow=TRUE),
             heights=c(0.5,3,2.5,2.5))

      # --- PART 1: Create the Header Banner ---
      # Setup a plot area with no margins for the header
      par(mar=c(0,0,0,0))
      plot.new()

      # Get player name - just the name without any prefix
      player_name <- input$select_athlete
      header_text <- player_name

      # Draw a yellow rectangle across the full width
      rect(0, 0, 1, 1, col="#FFDE00", border=NA)

      # Add the text centered in the banner - larger name with professional font
      text(0.5, 0.5, header_text, cex=3.2, font=2, family="sans")  # Added professional font

      # --- PART 2: Create the Dynamic Radar Plot (using radar_test_dates) ---
      # Use the dynamically selected metrics for radar plot
      radar_metrics <- input$radar_metrics

      # Build the shared min/max boundaries using percentiles
      radar_pcols <- paste0(radar_metrics, "_pct")
      mins  <- rep(0, length(radar_pcols))
      maxs  <- rep(100, length(radar_pcols))
      df_radar <- as.data.frame(rbind(maxs, mins))
      rownames(df_radar) <- c("Max", "Min")

      # Overlay one polygon per selected RADAR test date (not all dates)
      for (d in input$radar_test_dates) {
        athlete_row <- pctdata() %>%
          filter(athlete_name == input$select_athlete, Test_Date == d) %>%
          select(all_of(radar_pcols)) %>%
          unlist(use.names = FALSE)

        df_radar <- rbind(df_radar, athlete_row)
        rownames(df_radar)[nrow(df_radar)] <- format(d, format = "%Y-%m-%d")
      }

      # Add comparison overlays
      if (input$show_hs_avg) {
        avg_hs <- colMeans(pctdata() %>% filter(level_group==1) %>% select(all_of(radar_pcols)), na.rm = TRUE)
        df_radar <- rbind(df_radar, avg_hs); rownames(df_radar)[nrow(df_radar)] <- "HS Average"
      }
      if (input$show_college_avg) {
        avg_co <- colMeans(pctdata() %>% filter(level_group==2) %>% select(all_of(radar_pcols)), na.rm = TRUE)
        df_radar <- rbind(df_radar, avg_co); rownames(df_radar)[nrow(df_radar)] <- "College Average"
      }
      if (input$select_velocity_group != "All") {
        # map dropdown label to numeric code
        vel_code <- switch(input$select_velocity_group,
                           "<80"   = 79,
                           "80-85" = 83,
                           "85-90" = 87,
                           ">90"   = 91)
        avg_velo <- pctdata() %>%
          filter(velo_group == vel_code) %>%
          select(all_of(radar_pcols))
        df_radar <- rbind(df_radar, colMeans(avg_velo, na.rm=TRUE))
        rownames(df_radar)[nrow(df_radar)] <- "Velocity Average"
      }

      # Styling: gold for athlete‐date overlays
      series_names <- rownames(df_radar)[-c(1,2)]
      n_series    <- length(series_names)
      colors      <- rep("#FFD700", n_series)              # all athlete polygons in gold
      fills       <- scales::alpha(colors, .3)
      ltys        <- rep(1, n_series)

      # Overwrite HS/College/Velocity if needed
      for (i in seq_along(series_names)) {
        nm <- series_names[i]
        if (nm == "HS Average" || nm == "College Average") {
          colors[i] <- "#A9A9A9"; fills[i] <- scales::alpha("#A9A9A9", .3)
        }
        if (nm == "Velocity Average") {
          vc <- switch(input$select_velocity_group,
                       "<80"   = "lightblue",
                       "80-85" = "lightgreen",
                       "85-90" = "lightcoral",
                       ">90"   = "orange",
                       "grey")
          colors[i] <- vc; fills[i] <- scales::alpha(vc, .3)
        }
      }

      # Get friendly names for radar axes and create dynamic labels
      radar_friendly_names <- names(friendly_choices)[match(radar_metrics, friendly_choices)]

      # Create shorter, non-overlapping labels for the radar chart dynamically
      formatted_radar_labels <- sapply(radar_friendly_names, function(name) {
        # Remove units and shorten names
        short_name <- gsub("\\s*\\(.*\\)\\s*$", "", name)

        # Create shorter versions of common long names
        short_name <- gsub("IMTP Net Peak Vertical Force", "IMTP Force", short_name)
        short_name <- gsub("Countermovement Depth", "CM Depth", short_name)
        short_name <- gsub("Concentric Peak Velocity", "Peak Velocity", short_name)
        short_name <- gsub("Concentric Peak Velo", "Con Peak Velo", short_name)
        short_name <- gsub("Eccentric Peak Force", "Ecc Peak Force", short_name)
        short_name <- gsub("Ecc\\. Decel Impulse", "Ecc Decel Imp", short_name)
        short_name <- gsub("Ecc\\. Decel RFD", "Ecc Decel RFD", short_name)
        short_name <- gsub("Eccentric Decel RFD", "Ecc Decel RFD", short_name)
        short_name <- gsub("Force at Zero Velocity", "Force @ Zero V", short_name)
        short_name <- gsub("Braking Phase Duration", "Braking Duration", short_name)
        short_name <- gsub("Concentric Peak Force", "Con Peak Force", short_name)
        short_name <- gsub("Concentric Duration", "Con Duration", short_name)
        short_name <- gsub("Concentric Impulse @100ms", "Con Imp @100ms", short_name)
        short_name <- gsub("Concentric Impulse", "Con Impulse", short_name)
        short_name <- gsub("Ecc\\. Braking Impulse", "Ecc Braking Imp", short_name)
        short_name <- gsub("Ecc\\. Braking RFD", "Ecc Braking RFD", short_name)
        short_name <- gsub("Premier Power", "Premier Pwr", short_name)
        short_name <- gsub("PPP Power", "PPP Pwr", short_name)
        short_name <- gsub("Relative Premier Power", "Rel Premier Pwr", short_name)
        short_name <- gsub("Relative Ecc\\. Peak Force", "Rel Ecc Peak", short_name)
        short_name <- gsub("Relative IMTP", "Rel IMTP", short_name)

        # Limit to reasonable length
        if (nchar(short_name) > 15) {
          short_name <- substr(short_name, 1, 15)
        }

        return(short_name)
      })

      # Create the radar plot with standard margins
      par(mar = c(2, 2, 4, 2), family="sans")
      radarchart(df_radar,
                 axistype   = 1,
                 pcol       = colors,
                 pfcol      = fills,
                 plwd       = 3,             # Thick line width
                 plty       = ltys,
                 cglcol     = "grey",
                 cglty      = 1,
                 axislabcol = "grey",
                 vlcex      = 2.0,           # INCREASED from 1.5 to 2.0 for axis labels
                 cglcex     = 1.5,           # ADDED parameter for grid label size
                 calcex     = 1.5,           # ADDED parameter for calibration label size
                 palcex     = 1.5,           # ADDED parameter for polygon area label size
                 vlabels    = formatted_radar_labels
      )

      # Add a legend with larger text
      legend("topright", legend=series_names, col=colors, lty=ltys,
             lwd=3, bty="n", cex=1.8)        # INCREASED from 1.3 to 1.8

      # Add the LASSO score and predicted velocity text in one line with pipe separators
      velo_row <- velocity_predictions() %>%
        filter(player_name == input$select_athlete,
               test_date == input$force_scores_date)

      combined_text <- ""

      if (nrow(velo_row) > 0) {
        combined_text <- sprintf(
          "Force Score: %d | Predicted Velocity: %.1f mph | Velocity: %.1f mph",
          round(velo_row$force_dis),
          velo_row$predicted_velo, velo_row$velo)
      } else {
        combined_text <- "Force Score: N/A | Predicted Velocity: N/A | Velocity: N/A"
      }

      if (combined_text != "") {
        # Place the text just below the yellow header with a tiny gap
        # so it does not overlap the banner
        mtext(combined_text, side=3, line=1.2, cex=1.4, font=1, family="sans")
      }

      # --- PART 3: Create the Trend Plot using ALL dates for the athlete ---
      # UPDATED: Use ALL test dates for the selected athlete, not just radar dates
      df_trend <- data() %>%
        filter(athlete_name == input$select_athlete) %>%
        arrange(Test_Date)  # ensure dates are in order

      y_lab <- names(friendly_choices)[match(input$trend_metric, friendly_choices)]

      par(mar=c(4,6,3,2), family="sans")
      plot(df_trend$Test_Date, df_trend[[input$trend_metric]],
           type="b", pch=16, lwd=2,
           xlab="Test Date",
           ylab=y_lab,
           main=paste0(y_lab, " Over Time"),
           cex.main=2.0, cex.lab=1.8, cex.axis=1.6,
           axes=FALSE)
      axis.Date(1, at = df_trend$Test_Date,
                format = "%Y-%m-%d", cex.axis=1.6)
      axis(2, cex.axis=1.6)
      box()
      grid(lty=2, col="lightgray")

      points(df_trend$Test_Date, df_trend[[input$trend_metric]],
             pch=16, cex=1.8, col="blue")

      # --- PART 4: Create the Metrics Table (using last 3 dates) ---
      # Use the dynamically selected metrics for the data table
      chosen <- input$data_table_metrics

      # UPDATED: Use the last 3 test dates for the data table
      df_metrics <- data() %>%
        filter(athlete_name == input$select_athlete) %>%
        arrange(desc(Test_Date)) %>%
        slice_head(n = 3) %>%
        arrange(Test_Date) %>%
        select(Test_Date, all_of(chosen))

      df_z <- zdata() %>%
        filter(athlete_name == input$select_athlete) %>%
        arrange(desc(Test_Date)) %>%
        slice_head(n = 3) %>%
        arrange(Test_Date) %>%
        select(Test_Date, all_of(paste0(chosen, "_z"))) %>%
        mutate(across(-Test_Date, ~ round(100 + . * 10)))

      colnames(df_z)[-1] <- chosen

      # Format the data specifically for display with shortened year format
      metrics_data <- data.frame(
        Test_Date = format(df_metrics$Test_Date, "%y-%m-%d"),  # Changed from %Y-%m-%d to %y-%m-%d
        Type = "Raw value",
        df_metrics[, -1]
      )

      scores_data <- data.frame(
        Test_Date = format(df_z$Test_Date, "%y-%m-%d"),  # Changed from %Y-%m-%d to %y-%m-%d
        Type = "Composite",
        df_z[, -1]
      )

      combined_data <- rbind(metrics_data, scores_data)

      friendly_names <- names(friendly_choices)[match(chosen, friendly_choices)]
      colnames(combined_data)[-(1:2)] <- friendly_names

      # Setup plot area - expand to take all available space with professional font
      par(mar=c(0,0,3,0), xpd=TRUE, family="sans")
      plot.new()
      # REMOVED: title(main="Current Metrics", cex.main=2.2, font.main=2)  # This line was deleted

      # Get rows and columns
      n_rows <- nrow(combined_data)
      n_cols <- ncol(combined_data)

      # Maximize table width by extending outside normal plot region
      y_pos_top <- 0.95    # Top position of table

      # Use a consistent row height based on having three test dates (six rows)
      # so rows do not stretch when fewer dates are available
      fixed_row_height <- (0.95 - 0.05) / (6 + 2)
      row_height <- fixed_row_height * 1.15  # slightly taller rows

      # Adjust the bottom position so the table sits slightly higher
      # Add a small upward shift so the last row never gets clipped
      y_pos_bottom <- y_pos_top - row_height * (n_rows + 2) + 0.07

      x_pos_left <- 0   # Extend left beyond normal plot region
      x_pos_right <- 1   # Extend right beyond normal plot region

      # Calculate row height and column width - ensure wide columns

      # Make first columns narrower to give more space to data columns
      col_widths <- c(0.12, 0.12)  # Test_Date and Type columns
      remaining_width <- (x_pos_right - x_pos_left) - sum(col_widths)
      if (n_cols > 2) {
        metric_width <- remaining_width / (n_cols - 2)
        col_widths <- c(col_widths, rep(metric_width, n_cols - 2))
      } else {
        # When no metrics are selected, split width evenly
        col_widths <- c(0.5, 0.5)
      }

      # Calculate positions
      col_positions <- c(x_pos_left, x_pos_left + cumsum(col_widths)[-n_cols])
      row_positions <- rev(seq(y_pos_bottom, y_pos_top, length.out = n_rows + 3))

      # Draw the table outline - thicker border
      rect(x_pos_left, y_pos_bottom, x_pos_right, y_pos_top, border = "black", lwd = 2)

      # Make entire table background white first
      rect(x_pos_left, y_pos_bottom, x_pos_right, y_pos_top,
           col = "white", border = NA)

      # Draw header rows with bright yellow background - remove border to avoid double outline
      rect(x_pos_left, row_positions[3], x_pos_right, y_pos_top,
           col = "#FFDE00", border = NA)  # Bright yellow color without outline

      # Draw alternating row backgrounds - white
      for (i in seq(4, length(row_positions), by = 2)) {
        if (i <= length(row_positions)) {
          rect(x_pos_left, row_positions[i], x_pos_right, row_positions[i-1],
               col = "white", border = NA)
        }
      }

      # Draw column separators
      for (j in 1:(n_cols-1)) {
        x_pos <- col_positions[j] + col_widths[j]
        lines(c(x_pos, x_pos), c(y_pos_bottom, y_pos_top), col = "gray60", lwd = 1.5)
      }

      # Draw row separators but SKIP ALL lines that might overlap with text
      last_header_row = 3  # Index of the last header row
      for (i in 3:(length(row_positions)-1)) {
        # Only draw lines between data rows - skip ALL lines that could overlap with headers
        if (i > last_header_row) {
          lines(c(x_pos_left, x_pos_right), c(row_positions[i], row_positions[i]),
                col = "gray60", lwd = 1.5)
        }
      }

      # Column headers for Test Date and Type
      text(col_positions[1] + col_widths[1]/2, row_positions[2] + (row_positions[1] - row_positions[2])/2,
           "Test Date", font = 1, cex = 1.6)

      text(col_positions[2] + col_widths[2]/2, row_positions[2] + (row_positions[1] - row_positions[2])/2,
           "Type", font = 1, cex = 1.6)

      # Header row 1: Categories with shortened text to fit in box
      if (n_cols >= 3) for (j in 3:n_cols) {
        col_center <- col_positions[j] + col_widths[j]/2

        # Get category name from friendly_names - DYNAMICALLY from chosen metrics
        category <- friendly_names[j-2]

        # Don't duplicate units - they'll be shown in the second row
        # Remove units from category name if they exist
        if(grepl("\\(.*\\)", category)) {
          category <- gsub("\\s*\\(.*\\)\\s*$", "", category)
        }

        # Create shorter versions of common long names dynamically
        category <- gsub("IMTP Net Peak Vertical Force", "IMTP Force", category)
        category <- gsub("Countermovement Depth", "CM Depth", category)
        category <- gsub("Concentric Peak Velocity", "Peak Velocity", category)
        category <- gsub("Concentric Peak Velo", "Con Peak Velo", category)
        category <- gsub("Eccentric Peak Force", "Ecc Peak Force", category)
        category <- gsub("Jump Height", "Jump Height", category)
        category <- gsub("Peak Power", "Peak Power", category)
        category <- gsub("RSI Modified", "RSI Modified", category)
        category <- gsub("Body Weight", "Body Weight", category)
        category <- gsub("P1 Concentric Impulse", "P1 Con Impulse", category)
        category <- gsub("Ecc\\. Decel Impulse", "Ecc Decel Imp", category)
        category <- gsub("Ecc\\. Decel RFD", "Ecc Decel RFD", category)
        category <- gsub("Eccentric Decel RFD", "Ecc Decel RFD", category)
        category <- gsub("Force at Zero Velocity", "Force @ Zero V", category)
        category <- gsub("Braking Phase Duration", "Braking Duration", category)
        category <- gsub("Concentric Peak Force", "Con Peak Force", category)
        category <- gsub("Concentric Duration", "Con Duration", category)
        category <- gsub("Concentric Impulse @100ms", "Con Imp @100ms", category)
        category <- gsub("Concentric Impulse", "Con Impulse", category)
        category <- gsub("Ecc\\. Braking Impulse", "Ecc Braking Imp", category)
        category <- gsub("Ecc\\. Braking RFD", "Ecc Braking RFD", category)
        category <- gsub("Premier Power", "Premier Pwr", category)
        category <- gsub("PPP Power", "PPP Pwr", category)
        category <- gsub("Relative Premier Power", "Rel Premier Pwr", category)
        category <- gsub("Relative Ecc\\. Peak Force", "Rel Ecc Peak", category)
        category <- gsub("Relative IMTP", "Rel IMTP", category)

        # Position the text in the top part of the header cell
        text(col_center, row_positions[2] + (row_positions[1] - row_positions[2])/2,
             category, font = 1, cex = 1.4)
      }

      # Header row 2: Units
      if (n_cols >= 3) for (j in 3:n_cols) {
        col_center <- col_positions[j] + col_widths[j]/2

        # Extract units from the friendly name text
        full_name <- friendly_names[j-2]
        unit <- ""

        # Extract units from parentheses if they exist
        if(grepl("\\(.*\\)", full_name)) {
          unit <- gsub(".*\\((.*)\\).*", "\\1", full_name)
          unit <- paste0("(", unit, ")")
        }

        # Position the text in the bottom part of the header cell
        text(col_center, (row_positions[2] + row_positions[3])/2,
             unit, font = 3, cex = 1.4)
      }

      # Draw cell values - with increased font size
      for (i in 1:n_rows) {
        for (j in 1:n_cols) {
          col_center <- col_positions[j] + col_widths[j]/2
          row_center <- (row_positions[i+3] + row_positions[i+2])/2

          # Format values
          if (j > 2) {
            if (combined_data$Type[i] == "Raw value") {
              cell_text <- sprintf("%.1f", as.numeric(combined_data[i,j]))
            } else {
              cell_text <- sprintf("%d", as.numeric(combined_data[i,j]))
            }
          } else {
            cell_text <- as.character(combined_data[i,j])
          }

          # Increased font size for data cells
          text(col_center, row_center, cell_text, cex = 1.9)  # INCREASED from 2.0 to 2.2
        }
      }
    }

    output$combinedVisualization <- renderPlot({
      create_visualization()
    })

    # 11. UPDATED: Download handler for the visualization
    output$downloadPlot <- downloadHandler(
      filename = function() {
        # Create a filename with athlete name and date
        paste0("VALD_LASSO_Assessment_", gsub(" ", "_", input$select_athlete), "_",
               format(Sys.Date(), "%Y%m%d"), ".png")
      },
      content = function(file) {
        # Validate radar metrics selection before download
        if (length(input$radar_metrics) < 3 || length(input$radar_metrics) > 6) {
          return()
        }

        # Open a PNG device with increased width (from 1200 to 1800)
        # and better width-to-height ratio that matches the app display
        png(file, width = 1800, height = 2400, res = 150)
        create_visualization()
        dev.off()
      }
    )
  })
}
