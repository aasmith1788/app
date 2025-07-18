# =====================================================================
# modules/mod_stuffplus.R - Two Player Comparison Version
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
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
        
        body {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
          color: #1a1a1a;
          background-color: #ffffff;
        }
        
        .main-container {
          max-width: 1600px;
          margin: 0 auto;
          padding: 20px;
          background-color: #ffffff;
        }
        
        .header-section {
          background: white;
          padding: 20px 24px;
          margin-bottom: 20px;
        }
        
        .page-title {
          font-size: 26px;
          font-weight: 700;
          color: #1a1a1a;
          margin: 0;
          letter-spacing: -0.5px;
        }
        
        .comparison-container {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 20px;
          background: white;
        }
        
        .player-panel {
          background: white;
          overflow: hidden;
        }
        
        .filter-bar {
          padding: 16px 20px;
          display: flex;
          align-items: center;
          gap: 16px;
          flex-wrap: wrap;
          border-bottom: 1px solid #e5e5e5;
        }
        
        .filter-item {
          display: flex;
          align-items: center;
          gap: 8px;
          flex: 1;
          min-width: 0;
        }
        
        .filter-label {
          font-size: 13px;
          font-weight: 500;
          color: #666;
          white-space: nowrap;
        }
        
        .filter-control {
          flex: 1;
          min-width: 120px;
        }
        
        .selectize-control {
          flex: 2;
          min-width: 180px;
        }
        
        .selectize-input {
          border: 1px solid #d0d0d0 !important;
          border-radius: 4px !important;
          padding: 6px 12px !important;
          min-height: 34px !important;
          font-size: 13px !important;
          background: #fff !important;
        }
        
        .selectize-input.focus {
          border-color: #999 !important;
          box-shadow: none !important;
        }
        
        .picker-input .btn {
          border: 1px solid #d0d0d0 !important;
          border-radius: 4px !important;
          padding: 6px 12px !important;
          min-height: 34px !important;
          font-size: 13px !important;
          background: #fff !important;
          color: #1a1a1a !important;
          text-align: left !important;
        }
        
        .picker-input .btn:hover {
          background: #fafafa !important;
          border-color: #999 !important;
        }
        
        .player-content {
          padding: 20px;
        }
        
        .results-header {
          display: flex;
          justify-content: space-between;
          align-items: baseline;
          margin-bottom: 16px;
          padding-bottom: 12px;
          border-bottom: 1px solid #e5e5e5;
        }
        
        .player-name {
          font-size: 20px;
          font-weight: 600;
          color: #1a1a1a;
          margin: 0;
        }
        
        .pitch-count {
          font-size: 13px;
          color: #666;
          font-weight: 500;
        }
        
        .section-title {
          font-size: 14px;
          font-weight: 600;
          color: #1a1a1a;
          margin: 0 0 12px 0;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        
        .data-table-container {
          margin-top: 8px;
        }
        
        table.dataTable {
          font-size: 11px;
          border-collapse: collapse;
          width: 100% !important;
        }
        
        table.dataTable thead th {
          background-color: transparent;
          border-bottom: 2px solid #e5e5e5;
          font-weight: 600;
          color: #666;
          padding: 6px 2px;
          text-align: center;
          font-size: 10px;
          text-transform: uppercase;
          letter-spacing: 0.3px;
        }
        
        table.dataTable tbody td {
          border-bottom: 1px solid #f0f0f0;
          padding: 4px 2px;
          text-align: center;
          color: #1a1a1a;
          font-size: 11px;
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
          font-size: 16px;
          font-weight: 600;
          margin-bottom: 8px;
          color: #1a1a1a;
        }
        
        .no-results p {
          font-size: 13px;
        }
        
        /* Remove divider between players */
        
        /* Compact styles for fitting more data */
        .compact-filters .filter-item {
          flex: initial;
        }
        
        .compact-filters .filter-control {
          min-width: 100px;
        }
        
        .compact-filters .selectize-control {
          min-width: 150px;
        }
        
        /* Hide DataTables default styling */
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          display: none;
        }
        
        @media (max-width: 1200px) {
          .comparison-container {
            grid-template-columns: 1fr;
          }
          
          .player-panel:first-child {
            margin-bottom: 20px;
          }
        }
      "))
    ),
    
    div(class = "main-container",
        # Header
        div(class = "header-section",
            h1("MLB Stuff Plus Analytics - Player Comparison", class = "page-title")
        ),
        
        # Two-player comparison layout
        div(class = "comparison-container",
            # Player 1 Panel
            div(class = "player-panel",
                div(class = "filter-bar compact-filters",
                    div(class = "filter-item",
                        span(class = "filter-label", "Player 1:"),
                        div(class = "selectize-control",
                            selectizeInput(ns("player1_search"), label = NULL,
                                           choices = NULL,
                                           options = list(
                                             placeholder = "Search pitcher...",
                                             maxOptions = 1000
                                           ))
                        )
                    ),
                    div(class = "filter-item",
                        span(class = "filter-label", "Season:"),
                        div(class = "filter-control",
                            uiOutput(ns("year_filter_ui1"))
                        )
                    ),
                    div(class = "filter-item",
                        span(class = "filter-label", "Games:"),
                        div(class = "filter-control",
                            uiOutput(ns("date_filter_ui1"))
                        )
                    )
                ),
                div(class = "player-content",
                    uiOutput(ns("player1_content"))
                )
            ),
            
            # Player 2 Panel
            div(class = "player-panel",
                div(class = "filter-bar compact-filters",
                    div(class = "filter-item",
                        span(class = "filter-label", "Player 2:"),
                        div(class = "selectize-control",
                            selectizeInput(ns("player2_search"), label = NULL,
                                           choices = NULL,
                                           options = list(
                                             placeholder = "Search pitcher...",
                                             maxOptions = 1000
                                           ))
                        )
                    ),
                    div(class = "filter-item",
                        span(class = "filter-label", "Season:"),
                        div(class = "filter-control",
                            uiOutput(ns("year_filter_ui2"))
                        )
                    ),
                    div(class = "filter-item",
                        span(class = "filter-label", "Games:"),
                        div(class = "filter-control",
                            uiOutput(ns("date_filter_ui2"))
                        )
                    )
                ),
                div(class = "player-content",
                    uiOutput(ns("player2_content"))
                )
            )
        )
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
    
    # ---- 2. Reactive values for both players -------------------------
    player1_data <- reactiveVal(NULL)
    player2_data <- reactiveVal(NULL)
    
    # ---- 3. Search functionality for both players --------------------
    # Player 1
    updateSelectizeInput(session, "player1_search",
                         choices = sort(unique(all_pitches$formatted_name)),
                         server = TRUE)
    
    observeEvent(input$player1_search, {
      req(input$player1_search)
      data <- all_pitches %>%
        filter(grepl(input$player1_search, formatted_name, ignore.case = TRUE))
      
      if (nrow(data) > 0) {
        player1_data(data)
      } else {
        player1_data(NULL)
      }
    })
    
    # Player 2
    updateSelectizeInput(session, "player2_search",
                         choices = sort(unique(all_pitches$formatted_name)),
                         server = TRUE)
    
    observeEvent(input$player2_search, {
      req(input$player2_search)
      data <- all_pitches %>%
        filter(grepl(input$player2_search, formatted_name, ignore.case = TRUE))
      
      if (nrow(data) > 0) {
        player2_data(data)
      } else {
        player2_data(NULL)
      }
    })
    
    # ---- 4. Player 1 UI outputs --------------------------------------
    output$player1_content <- renderUI({
      ns <- session$ns
      
      if (is.null(player1_data())) {
        if (!is.null(input$player1_search) && nchar(input$player1_search) > 0) {
          div(class = "no-results",
              h3("No results found"),
              p("Try searching with a different name.")
          )
        } else {
          div(class = "no-results",
              h3("Select Player 1"),
              p("Enter a pitcher's name in the search box above.")
          )
        }
      } else {
        data <- player1_data()
        player_name <- unique(data$formatted_name)[1]
        
        tagList(
          div(class = "results-header",
              h2(player_name, class = "player-name"),
              span(class = "pitch-count",
                   textOutput(ns("pitch_count1"), inline = TRUE))
          ),
          uiOutput(ns("season_summary_ui1")),
          uiOutput(ns("game_summary_ui1"))
        )
      }
    })
    
    # ---- 5. Player 2 UI outputs --------------------------------------
    output$player2_content <- renderUI({
      ns <- session$ns
      
      if (is.null(player2_data())) {
        if (!is.null(input$player2_search) && nchar(input$player2_search) > 0) {
          div(class = "no-results",
              h3("No results found"),
              p("Try searching with a different name.")
          )
        } else {
          div(class = "no-results",
              h3("Select Player 2"),
              p("Enter a pitcher's name in the search box above.")
          )
        }
      } else {
        data <- player2_data()
        player_name <- unique(data$formatted_name)[1]
        
        tagList(
          div(class = "results-header",
              h2(player_name, class = "player-name"),
              span(class = "pitch-count",
                   textOutput(ns("pitch_count2"), inline = TRUE))
          ),
          uiOutput(ns("season_summary_ui2")),
          uiOutput(ns("game_summary_ui2"))
        )
      }
    })
    
    # ---- 6. Filter UIs for Player 1 ---------------------------------
    output$year_filter_ui1 <- renderUI({
      req(player1_data())
      ns <- session$ns
      
      years <- sort(unique(player1_data()$year))
      
      pickerInput(
        inputId = ns("year_filter1"),
        label = NULL,
        choices = years,
        selected = years,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} seasons",
          size = 10
        )
      )
    })
    
    output$date_filter_ui1 <- renderUI({
      req(player1_data())
      ns <- session$ns
      
      dates <- player1_data() %>%
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
          `count-selected-text` = "{0} games",
          size = 10,
          `live-search` = TRUE,
          `none-selected-text` = "Select games"
        )
      )
    })
    
    # ---- 7. Filter UIs for Player 2 ---------------------------------
    output$year_filter_ui2 <- renderUI({
      req(player2_data())
      ns <- session$ns
      
      years <- sort(unique(player2_data()$year))
      
      pickerInput(
        inputId = ns("year_filter2"),
        label = NULL,
        choices = years,
        selected = years,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} seasons",
          size = 10
        )
      )
    })
    
    output$date_filter_ui2 <- renderUI({
      req(player2_data())
      ns <- session$ns
      
      dates <- player2_data() %>%
        select(game_date, game_date_formatted) %>%
        distinct() %>%
        arrange(desc(game_date))
      
      pickerInput(
        inputId = ns("date_filter2"),
        label = NULL,
        choices = setNames(dates$game_date, dates$game_date_formatted),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = FALSE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} games",
          size = 10,
          `live-search` = TRUE,
          `none-selected-text` = "Select games"
        )
      )
    })
    
    # ---- 8. Data helpers for both players ----------------------------
    get_season_data1 <- reactive({
      req(player1_data())
      data <- player1_data()
      
      if (!is.null(input$year_filter1) && length(input$year_filter1) > 0) {
        data <- data %>% filter(year %in% input$year_filter1)
      } else {
        data <- data[0, ]
      }
      data
    })
    
    get_season_data2 <- reactive({
      req(player2_data())
      data <- player2_data()
      
      if (!is.null(input$year_filter2) && length(input$year_filter2) > 0) {
        data <- data %>% filter(year %in% input$year_filter2)
      } else {
        data <- data[0, ]
      }
      data
    })
    
    get_game_data1 <- reactive({
      data <- player1_data()
      if (!is.null(input$date_filter1) && length(input$date_filter1) > 0) {
        data <- data %>% filter(game_date %in% input$date_filter1)
      } else {
        data <- NULL
      }
      data
    })
    
    get_game_data2 <- reactive({
      data <- player2_data()
      if (!is.null(input$date_filter2) && length(input$date_filter2) > 0) {
        data <- data %>% filter(game_date %in% input$date_filter2)
      } else {
        data <- NULL
      }
      data
    })
    
    # ---- 9. Pitch counts ---------------------------------------------
    output$pitch_count1 <- renderText({
      data <- get_season_data1()
      paste(format(nrow(data), big.mark = ","), "pitches")
    })
    
    output$pitch_count2 <- renderText({
      data <- get_season_data2()
      paste(format(nrow(data), big.mark = ","), "pitches")
    })
    
    # ---- 10. Summary function ----------------------------------------
    summarize_player_data <- function(data) {
      data %>%
        group_by(pitch_type) %>%
        summarise(
          Count = n(),
          `Velo` = round(mean(release_speed, na.rm = TRUE), 1),
          `iVB` = round(mean(pfx_z, na.rm = TRUE), 1),
          `HB` = round(mean(pfx_x, na.rm = TRUE), 1),
          `Spin` = round(mean(release_spin_rate, na.rm = TRUE), 0),
          `vRel` = round(mean(release_pos_z, na.rm = TRUE), 1),
          `hRel` = round(mean(release_pos_x, na.rm = TRUE), 1),
          `Ext` = round(mean(release_extension, na.rm = TRUE), 1),
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
              pitch_type = "All",
              Count = n(),
              `Velo` = NA,
              `iVB` = NA,
              `HB` = NA,
              `Spin` = NA,
              `vRel` = NA,
              `hRel` = NA,
              `Ext` = round(mean(release_extension, na.rm = TRUE), 1),
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
        rename(Type = pitch_type)
    }
    
    # ---- 11. Create compact table function ---------------------------
    create_compact_table <- function(data) {
      summary_data <- summarize_player_data(data)
      
      # Format for display
      summary_data <- summary_data %>%
        mutate(
          Count = format(Count, big.mark = ","),
          across(c(`Velo`, `iVB`, `HB`, `vRel`, `hRel`, `Ext`), 
                 ~ifelse(is.na(.), "-", as.character(.))),
          across(c(`Spin`, `Stuff+`), 
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
            list(width = "25px", targets = 0),  # Type
            list(width = "35px", targets = 1),  # Count
            list(width = "28px", targets = c(2:8)),  # Stats
            list(width = "35px", targets = c(9:12))  # Percentages
          )
        ),
        rownames = FALSE,
        escape = FALSE
      ) %>%
        formatStyle(
          columns = 1:ncol(summary_data),
          fontSize = '10px'
        ) %>%
        formatStyle(
          "Type",
          fontWeight = styleEqual("All", "bold")
        )
    }
    
    # ---- 12. Render all tables ---------------------------------------
    # Player 1 tables
    output$season_table1 <- renderDT({
      req(player1_data())
      create_compact_table(get_season_data1())
    })
    
    output$game_table1 <- renderDT({
      data <- get_game_data1()
      req(!is.null(data))
      create_compact_table(data)
    })
    
    # Player 2 tables
    output$season_table2 <- renderDT({
      req(player2_data())
      create_compact_table(get_season_data2())
    })
    
    output$game_table2 <- renderDT({
      data <- get_game_data2()
      req(!is.null(data))
      create_compact_table(data)
    })
    
    # ---- 13. Summary UIs ---------------------------------------------
    # Player 1
    output$season_summary_ui1 <- renderUI({
      data <- get_season_data1()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      ns <- session$ns
      years <- sort(unique(data$year))
      tagList(
        h3(paste("Season:", paste(years, collapse = ", ")), class = "section-title"),
        div(class = "data-table-container", DTOutput(ns("season_table1")))
      )
    })
    
    output$game_summary_ui1 <- renderUI({
      data <- get_game_data1()
      if (is.null(data)) return(NULL)
      ns <- session$ns
      dates <- format(as.Date(input$date_filter1), "%b %d")
      tagList(
        h3(paste("Games:", paste(dates, collapse = ", ")), class = "section-title", style = "margin-top: 16px;"),
        div(class = "data-table-container", DTOutput(ns("game_table1")))
      )
    })
    
    # Player 2
    output$season_summary_ui2 <- renderUI({
      data <- get_season_data2()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      ns <- session$ns
      years <- sort(unique(data$year))
      tagList(
        h3(paste("Season:", paste(years, collapse = ", ")), class = "section-title"),
        div(class = "data-table-container", DTOutput(ns("season_table2")))
      )
    })
    
    output$game_summary_ui2 <- renderUI({
      data <- get_game_data2()
      if (is.null(data)) return(NULL)
      ns <- session$ns
      dates <- format(as.Date(input$date_filter2), "%b %d")
      tagList(
        h3(paste("Games:", paste(dates, collapse = ", ")), class = "section-title", style = "margin-top: 16px;"),
        div(class = "data-table-container", DTOutput(ns("game_table2")))
      )
    })
    
  })
}
