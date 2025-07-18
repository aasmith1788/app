# =====================================================================
# modules/mod_stuffplus.R
# =====================================================================
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(shinyWidgets)

# ----------  UI -------------------------------------------------------
stuffPlusUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Professional CSS styling
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@400;600;700&display=swap');
        
        body {
          font-family: 'Source Sans Pro', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
          color: #333;
          background-color: #fafafa;
        }
        
        .main-container {
          max-width: 1200px;
          margin: 0 auto;
          padding: 20px;
        }
        
        .header-section {
          border-bottom: 1px solid #e2e2e2;
          padding-bottom: 20px;
          margin-bottom: 30px;
        }
        
        .page-title {
          font-size: 32px;
          font-weight: 700;
          color: #121212;
          margin: 0 0 20px 0;
          letter-spacing: -0.5px;
        }
        
        .search-row {
          display: flex;
          gap: 15px;
          align-items: center;
          max-width: 500px;
        }
        
        .search-input-container {
          flex: 1;
        }
        
        .search-input-container input {
          width: 100%;
          padding: 10px 15px;
          border: 1px solid #ccc;
          border-radius: 3px;
          font-size: 16px;
          transition: border-color 0.2s;
        }
        
        .search-input-container input:focus {
          outline: none;
          border-color: #666;
        }
        

        
        .filters-section {
          display: flex;
          gap: 20px;
          margin-bottom: 30px;
          padding-bottom: 20px;
          border-bottom: 1px solid #e2e2e2;
          align-items: flex-end;
        }

        .filter-group {
          flex: 1;
        }
        
        .filter-label {
          font-size: 14px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          color: #666;
          margin-bottom: 10px;
        }

        .player-label {
          font-size: 16px;
          font-weight: 600;
          margin-right: 10px;
          align-self: center;
        }
        
        .selectize-input {
          border: 1px solid #ccc !important;
          border-radius: 3px !important;
          padding: 8px 12px !important;
          min-height: 38px !important;
        }
        
        .selectize-input.focus {
          border-color: #666 !important;
          box-shadow: none !important;
        }
        
        .results-header {
          display: flex;
          justify-content: space-between;
          align-items: baseline;
          margin-bottom: 20px;
        }
        
        .player-name {
          font-size: 24px;
          font-weight: 700;
          color: #121212;
          margin: 0;
        }
        
        .pitch-count {
          font-size: 14px;
          color: #666;
        }
        
        .data-table-container {
          background: white;
          border: 1px solid #e2e2e2;
          border-radius: 3px;
          overflow: hidden;
        }
        
        table.dataTable {
          font-size: 14px;
          border-collapse: collapse;
        }
        
        table.dataTable thead th {
          background-color: #f8f8f8;
          border-bottom: 2px solid #e2e2e2;
          font-weight: 600;
          color: #333;
          padding: 12px 8px;
          text-align: center;
        }
        
        table.dataTable tbody td {
          border-bottom: 1px solid #f0f0f0;
          padding: 10px 8px;
          text-align: center;
        }
        
        table.dataTable tbody tr:hover {
          background-color: #fafafa;
        }
        
        table.dataTable tbody tr:last-child td {
          border-bottom: none;
        }
        
        .no-results {
          text-align: center;
          padding: 60px 20px;
          color: #666;
        }
        
        .no-results h3 {
          font-size: 18px;
          font-weight: 600;
          margin-bottom: 10px;
          color: #333;
        }
        
        /* Hide DataTables default styling */
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          display: none;
        }
      "))
    ),
    
    div(class = "main-container",
        # Header
        div(class = "header-section",
            h1("MLB Stuff Plus Analytics", class = "page-title")
        ),

        div(class = "filters-section",
            div(class = "player-label", "Player 1:"),
            div(class = "filter-group",
                selectizeInput(ns("player_search"), label = NULL,
                               choices = NULL,
                               options = list(
                                 placeholder = "Search pitcher name...",
                                 maxOptions = 1000
                               ))
            ),
            div(class = "filter-group",
                div(class = "filter-label", "Season"),
                uiOutput(ns("year_filter_ui"))
            ),
            div(class = "filter-group",
                div(class = "filter-label", "Game Date Set 1"),
                uiOutput(ns("date_filter_ui1"))
            )
        ),

        # Dynamic content
        uiOutput(ns("dynamic_content"))
    )
  )
}

# ----------  Server ---------------------------------------------------
stuffPlusServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ---- 1. Load and process all data --------------------------------
    csv_2023 <- "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2023/Model Development/test_results_xgboost_regression_runvalue_2023_pfx.csv"
    csv_2024 <- "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2024/Model Development/test_results_xgboost_regression_runvalue_2024_pfx.csv"
    csv_2025 <- "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2025/Model Development/test_results_xgboost_regression_runvalue_2025_pfx.csv"
    
    prm_2023 <- "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2023/Model Development/stuff_plus_formula_parameters_2023_pfx.csv"
    prm_2024 <- "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2024/Model Development/stuff_plus_formula_parameters_2024_pfx.csv"
    prm_2025 <- "C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/Stuff Plus/2025/Model Development/stuff_plus_formula_parameters_2025_pfx.csv"
    
    params <- bind_rows(
      read_csv(prm_2023, show_col_types = FALSE),
      read_csv(prm_2024, show_col_types = FALSE),
      read_csv(prm_2025, show_col_types = FALSE)
    ) %>%
      mutate(year = as.integer(year))
    
    swing_code <- c('foul_bunt', 'foul', 'hit_into_play', 'swinging_strike', 'foul_tip',
                    'swinging_strike_blocked', 'missed_bunt', 'bunt_foul_tip')
    whiff_code <- c('swinging_strike', 'foul_tip', 'swinging_strike_blocked')
    
    all_pitches <- bind_rows(
      read_csv(csv_2023, show_col_types = FALSE),
      read_csv(csv_2024, show_col_types = FALSE),
      read_csv(csv_2025, show_col_types = FALSE)
    ) %>%
      mutate(year = as.integer(year)) %>%
      left_join(params, by = "year") %>%
      mutate(
        stuff_plus = 100 - 10 * (predicted_target - mean_predicted_target) / sd_predicted_target,
        swing = description %in% swing_code,
        whiff = description %in% whiff_code,
        in_zone = zone < 10,
        out_zone = zone > 10,
        chase = zone > 10 & swing == TRUE,
        pfx_z = pfx_z * 12,
        pfx_x = pfx_x * 12,
        pitch_type = as.factor(pitch_type),
        name_parts = strsplit(player_name, ", "),
        formatted_name = sapply(name_parts, function(x) {
          if (length(x) == 2) paste(x[2], x[1]) else x[1]
        }),
        game_date_formatted = format(as.Date(game_date), "%b %d, %Y")
      ) %>%
      select(-mean_predicted_target, -sd_predicted_target, -n_pitches, -name_parts)
    
    # ---- 2. Reactive values -------------------------------------------
    current_player_data <- reactiveVal(NULL)
    
    # ---- 3. Search functionality -------------------------------------
    updateSelectizeInput(session, "player_search",
                         choices = sort(unique(all_pitches$formatted_name)),
                         server = TRUE)
    
    observeEvent(input$player_search, {
      req(input$player_search)
      player_data <- all_pitches %>%
        filter(grepl(input$player_search, formatted_name, ignore.case = TRUE))
      
      if (nrow(player_data) > 0) {
        current_player_data(player_data)
      } else {
        current_player_data(NULL)
      }
    })
    
    output$dynamic_content <- renderUI({
      ns <- session$ns
      
      if (is.null(current_player_data())) {
        if (!is.null(input$player_search) && nchar(input$player_search) > 0) {
          # No results found
          div(class = "no-results",
              h3("No results found"),
              p("Try searching with a different name or check the spelling.")
          )
        } else {
          # Initial state
          div(class = "no-results",
              h3("Search for a pitcher to begin"),
              p("Enter a pitcher's name in the search box above.")
          )
        }
      } else {
        # Player found - show results
        player_data <- current_player_data()
        player_name <- unique(player_data$formatted_name)[1]

        tagList(
          div(class = "results-header",
              h2(player_name, class = "player-name"),
              span(class = "pitch-count",
                   textOutput(ns("pitch_count"), inline = TRUE))
          ),
          uiOutput(ns("season_summary_ui")),
          uiOutput(ns("summary_set1_ui"))
        )
      }
    })
    
    # ---- 5. Season filter --------------------------------------------
    output$year_filter_ui <- renderUI({
      req(current_player_data())
      ns <- session$ns

      years <- sort(unique(current_player_data()$year))

      pickerInput(
        inputId = ns("year_filter"),
        label = NULL,
        choices = years,
        selected = years,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} seasons selected",
          size = 10
        )
      )
    })

    # ---- 6. Game date filters (all available dates) -------------------
    output$date_filter_ui1 <- renderUI({
      req(current_player_data())
      ns <- session$ns
      
      dates <- current_player_data() %>%
        select(game_date, game_date_formatted) %>%
        distinct() %>%
        arrange(desc(game_date))
      
      pickerInput(
        inputId = ns("date_filter1"),
        label = NULL,
        choices = setNames(dates$game_date, dates$game_date_formatted),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = FALSE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} games selected",
          size = 10,
          `live-search` = TRUE,
          `none-selected-text` = "Choose dates"
        )
      )
    })
    
    
    # ---- 6. Pitch count reactive -------------------------------------
    output$pitch_count <- renderText({
      data <- get_season_data()
      paste(format(nrow(data), big.mark = ","), "pitches")
    })
    
    # ---- 7. Data helpers ---------------------------------------------
    get_season_data <- reactive({
      req(current_player_data())
      
      data <- current_player_data()
      
      if (!is.null(input$year_filter) && length(input$year_filter) > 0) {
        data <- data %>% filter(year %in% input$year_filter)
      } else {
        data <- data[0, ]
      }
      data
    })
    
    
    get_date_data1 <- reactive({
      data <- current_player_data()
      if (!is.null(input$date_filter1) && length(input$date_filter1) > 0) {
        data <- data %>% filter(game_date %in% input$date_filter1)
      } else {
        data <- NULL
      }
      data
    })
    
    
    # ---- 8. Summary function ------------------------------------------
    summarize_player_data <- function(data) {
      data %>%
        group_by(pitch_type) %>%
        summarise(
          Count = n(),
          `Velocity` = round(mean(release_speed, na.rm = TRUE), 1),
          `iVB` = round(mean(pfx_z, na.rm = TRUE), 1),
          `HB` = round(mean(pfx_x, na.rm = TRUE), 1),
          `Spin` = round(mean(release_spin_rate, na.rm = TRUE), 0),
          `vRel` = round(mean(release_pos_z, na.rm = TRUE), 1),
          `hRel` = round(mean(release_pos_x, na.rm = TRUE), 1),
          `Extension` = round(mean(release_extension, na.rm = TRUE), 1),
          `Axis` = round(mean(spin_axis, na.rm = TRUE), 0),
          `Stuff+` = round(mean(stuff_plus, na.rm = TRUE), 0),
          `Zone%` = round(mean(in_zone, na.rm = TRUE) * 100, 1),
          `Chase%` = ifelse(sum(out_zone, na.rm = TRUE) > 0,
                            round(sum(chase, na.rm = TRUE) / sum(out_zone, na.rm = TRUE) * 100, 1),
                            0),
          `Whiff%` = ifelse(sum(swing, na.rm = TRUE) > 0,
                            round(sum(whiff, na.rm = TRUE) / sum(swing, na.rm = TRUE) * 100, 1),
                            0),
          .groups = 'drop'
        ) %>%
        arrange(desc(Count)) %>%
        bind_rows(
          data %>%
            summarise(
              pitch_type = "All Pitches",
              Count = n(),
              `Velocity` = NA,
              `iVB` = NA,
              `HB` = NA,
              `Spin` = NA,
              `vRel` = NA,
              `hRel` = NA,
              `Extension` = round(mean(release_extension, na.rm = TRUE), 1),
              `Axis` = NA,
              `Stuff+` = round(mean(stuff_plus, na.rm = TRUE), 0),
              `Zone%` = round(mean(in_zone, na.rm = TRUE) * 100, 1),
              `Chase%` = ifelse(sum(out_zone, na.rm = TRUE) > 0,
                                round(sum(chase, na.rm = TRUE) / sum(out_zone, na.rm = TRUE) * 100, 1),
                                0),
              `Whiff%` = ifelse(sum(swing, na.rm = TRUE) > 0,
                                round(sum(whiff, na.rm = TRUE) / sum(swing, na.rm = TRUE) * 100, 1),
                                0)
            )
        ) %>%
        rename(`Pitch Type` = pitch_type)
    }
    
    # ---- 9. Render table ---------------------------------------------
    output$results_table <- renderDT({
      req(current_player_data())
      
      summary_data <- summarize_player_data(get_season_data())
      
      # Format numbers for display
      summary_data <- summary_data %>%
        mutate(
          Count = format(Count, big.mark = ","),
          across(c(`Velocity`, `iVB`, `HB`, `vRel`, `hRel`, `Extension`), 
                 ~ifelse(is.na(.), "-", as.character(.))),
          across(c(`Spin`, `Axis`, `Stuff+`), 
                 ~ifelse(is.na(.), "-", format(., big.mark = ","))),
          across(c(`Zone%`, `Chase%`, `Whiff%`), 
                 ~paste0(., "%"))
        )
      
      datatable(
        summary_data,
        options = list(
          dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = "100px", targets = 0),  # Pitch Type
            list(width = "60px", targets = c(1:9)),  # Most columns
            list(width = "70px", targets = c(10:13))  # Percentage columns
          )
        ),
        rownames = FALSE,
        escape = FALSE
      ) %>%
        formatStyle(
          columns = 1:ncol(summary_data),
          fontSize = '14px'
        ) %>%
        formatStyle(
          "Pitch Type",
          fontWeight = styleEqual("All Pitches", "bold")
        )
    })
    
    
    output$results_table1 <- renderDT({
      data <- get_date_data1()
      req(!is.null(data))
      summarize_player_data(data) %>%
        {
          summary_data <- .
          summary_data <- summary_data %>%
            mutate(
              Count = format(Count, big.mark = ","),
              across(c(`Velocity`, `iVB`, `HB`, `vRel`, `hRel`, `Extension`),
                     ~ifelse(is.na(.), "-", as.character(.))),
              across(c(`Spin`, `Axis`, `Stuff+`),
                     ~ifelse(is.na(.), "-", format(., big.mark = ","))),
              across(c(`Zone%`, `Chase%`, `Whiff%`),
                     ~paste0(., "%"))
            )
          datatable(
            summary_data,
            options = list(
              dom = "t",
              ordering = FALSE,
              columnDefs = list(
                list(className = "dt-center", targets = "_all"),
                list(width = "100px", targets = 0),
                list(width = "60px", targets = c(1:9)),
                list(width = "70px", targets = c(10:13))
              )
            ),
            rownames = FALSE,
            escape = FALSE
          ) %>%
            formatStyle(columns = 1:ncol(summary_data), fontSize = '14px') %>%
            formatStyle("Pitch Type", fontWeight = styleEqual("All Pitches", "bold"))
        }
    })
    
    
    output$season_summary_ui <- renderUI({
      data <- get_season_data()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      ns <- session$ns
      years <- sort(unique(data$year))
      tagList(
        h3(paste("Season Summary for:", paste(years, collapse = ", ")), class = "mt-3"),
        div(class = "data-table-container", DTOutput(ns("results_table")))
      )
    })
    
    
    output$summary_set1_ui <- renderUI({
      data <- get_date_data1()
      if (is.null(data)) return(NULL)
      ns <- session$ns
      dates <- format(as.Date(input$date_filter1), "%b %d, %Y")
      tagList(
        h3(paste("Summary for:", paste(dates, collapse = ", "))),
        div(class = "data-table-container", DTOutput(ns("results_table1")))
      )
    })
    
  })
}