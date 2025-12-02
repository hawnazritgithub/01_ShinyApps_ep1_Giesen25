library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(fmsb) # For radar chart if needed, but we'll use plotly for interactivity
# --- Global Setup ---
# Check if data exists, if not, try to generate it or warn
data_file <- "simulated_ams_data.csv"
if (!file.exists(data_file)) {
  if (file.exists("generate_data.R")) {
    message("Data file not found. Running generate_data.R...")
    source("generate_data.R")
  } else {
    warning("simulated_ams_data.csv not found and generate_data.R is missing.")
  }
}
# Load default data if available
default_data <- tryCatch({
  read.csv(data_file)
}, error = function(e) NULL)
# Define Theme
# Minimalist medical theme using bslib
my_theme <- bs_theme(
  version = 5,
  bootswatch = "minty", # Clean, fresh look
  primary = "#0d6efd",
  secondary = "#6c757d",
  success = "#198754",
  info = "#0dcaf0",
  warning = "#ffc107",
  danger = "#dc3545",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)
# --- UI ---
ui <- page_navbar(
  theme = my_theme,
  title = "ID ROLL OUT: AMS Dashboard",
  sidebar = sidebar(
    title = "Study Info",
    width = 250,
    div(
      class = "p-3 text-muted",
      style = "font-size: 0.9rem;",
      strong("Study Cohort:"), " 10 Hospitals", br(),
      strong("Bed Median:"), " 401 Beds", br(), br(),
      p("This dashboard visualizes the impact of the ID ROLL OUT intervention.")
    )
  ),
  
  # Tab 1: Data Import
  nav_panel(
    title = "Data Import",
    icon = icon("file-upload"),
    card(
      card_header("Upload Data"),
      fileInput("file_upload", "Choose CSV File", accept = ".csv"),
      helpText("If no file is uploaded, the simulated dataset is used by default."),
      hr(),
      card_header("Data Preview"),
      DTOutput("raw_table")
    )
  ),
  
  # Tab 2: Dashboard
  nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),
    
    # Top Row: Value Boxes
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Baseline (2021)",
        value = textOutput("vb_baseline"),
        showcase = icon("chart-bar"),
        theme = "danger",
        p("Median AMS Score", class = "fs-6"),
        p(textOutput("vb_baseline_range"), class = "small opacity-75")
      ),
      value_box(
        title = "Intervention (2022)",
        value = textOutput("vb_intervention"),
        showcase = icon("arrow-trend-up"),
        theme = "success",
        p("Median AMS Score", class = "fs-6"),
        p(textOutput("vb_intervention_change"), class = "small opacity-75")
      ),
      value_box(
        title = "Follow-up (2023)",
        value = textOutput("vb_followup"),
        showcase = icon("arrow-trend-down"),
        theme = "warning", # Orange-ish
        p("Median AMS Score", class = "fs-6"),
        p(textOutput("vb_followup_change"), class = "small opacity-75")
      )
    ),
    
    # Charts
    layout_columns(
      col_widths = c(12, 6, 6),
      fill = FALSE, # Allow scrolling, don't squash charts
      
      # Chart A: Trajectory
      card(
        card_header("Longitudinal Score Trajectory"),
        plotlyOutput("chart_trajectory", height = "400px"),
        card_footer("Distribution of Total AMS Scores across phases.")
      ),
      
      # Chart B: Sustainability Gap
      card(
        card_header("Impact of Staffing on Sustainability"),
        plotlyOutput("chart_staffing", height = "400px"),
        card_footer("Comparison of score retention by staffing status.")
      ),
      
      # Chart C: Radar
      card(
        card_header("AMS Subcategory Performance"),
        selectInput("radar_phase", "Select Phase:", 
                    choices = c("Compare All", "Baseline", "Intervention", "FollowUp"), 
                    selected = "Compare All"),
        plotlyOutput("chart_radar", height = "400px"),
        card_footer("Breakdown of structural, resource, and action-based scores.")
      )
    )
  ),
  
  # Tab 3: About
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card(
      card_header("About ID ROLL OUT"),
      markdown("
### Antimicrobial Stewardship in German Non-University Hospitals
Version history [01.12.2025]\n
Version [01]\n
Author [Hawnaz Najmalddin]\n
CopyRight: Copyright © [2025] [Hawnaz].\n
  This application is released under the Creative Commons Attribution–NonCommercial 4.0 International (CC BY-NC 4.0) licence. 
  \nYou are free to share and adapt the material for any non-commercial purpose.

This dashboard visualizes the impact of the **ID ROLL OUT** multifaceted intervention study (2021-2023).\n
**Reference:**\n
Giesen et al., *Infection* (2025).\n
**Purpose:**\n
This tool is designed for hospital administrators and stakeholders to visualize the impact of Antimicrobial Stewardship (AMS) interventions over time. It demonstrates how targeted interventions can significantly improve AMS scores and highlights the critical role of ongoing staffing in sustaining these improvements.
**Key Findings:**\n
*   **Baseline:** Low AMS scores (Median ~37%) indicating a need for improvement.
*   **Intervention:** Significant increase in scores (Median ~76%) during the active support phase.
*   **Follow-up:** Scores decreased after intervention support ended, but hospitals with ongoing staffing maintained significantly higher scores than those without.
      ")
    )
  )
)
# --- Server ---
server <- function(input, output, session) {
  
  # Reactive Data Source
  data_r <- reactive({
    req(input$file_upload)
    tryCatch({
      read.csv(input$file_upload$datapath)
    }, error = function(e) {
      showNotification("Error reading file", type = "error")
      return(NULL)
    })
  })
  
  # Final Data (User upload or Default)
  final_data <- reactive({
    if (is.null(input$file_upload)) {
      return(default_data)
    } else {
      return(data_r())
    }
  })
  
  # --- Tab 1: Data Table ---
  output$raw_table <- renderDT({
    req(final_data())
    datatable(final_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # --- Tab 2: Dashboard Logic ---
  
  # Helper to get stats
  get_stats <- reactive({
    req(final_data())
    df <- final_data()
    
    df %>%
      group_by(Phase) %>%
      summarise(
        Median = median(Total_AMS_Score, na.rm = TRUE),
        Min = min(Total_AMS_Score, na.rm = TRUE),
        Max = max(Total_AMS_Score, na.rm = TRUE)
      )
  })
  
  # Value Boxes
  output$vb_baseline <- renderText({
    stats <- get_stats()
    val <- stats$Median[stats$Phase == "Baseline"]
    paste0(val, "%")
  })
  
  output$vb_baseline_range <- renderText({
    stats <- get_stats()
    r_min <- stats$Min[stats$Phase == "Baseline"]
    r_max <- stats$Max[stats$Phase == "Baseline"]
    paste0("Range: ", r_min, "-", r_max, "%")
  })
  
  output$vb_intervention <- renderText({
    stats <- get_stats()
    val <- stats$Median[stats$Phase == "Intervention"]
    paste0(val, "%")
  })
  
  output$vb_intervention_change <- renderText({
    stats <- get_stats()
    base <- stats$Median[stats$Phase == "Baseline"]
    inter <- stats$Median[stats$Phase == "Intervention"]
    diff <- inter - base
    paste0("Increase: +", diff, " pp")
  })
  
  output$vb_followup <- renderText({
    stats <- get_stats()
    val <- stats$Median[stats$Phase == "FollowUp"]
    paste0(val, "%")
  })
  
  output$vb_followup_change <- renderText({
    stats <- get_stats()
    inter <- stats$Median[stats$Phase == "Intervention"]
    follow <- stats$Median[stats$Phase == "FollowUp"]
    diff <- follow - inter
    paste0("Decrease: ", diff, " pp")
  })
  
  # Chart A: Trajectory (Boxplot)
  output$chart_trajectory <- renderPlotly({
    req(final_data())
    df <- final_data()
    
    # Ensure Phase order
    df$Phase <- factor(df$Phase, levels = c("Baseline", "Intervention", "FollowUp"))
    
    p <- plot_ly(df, x = ~Phase, y = ~Total_AMS_Score, type = "box", 
                 color = ~Phase, colors = c("#dc3545", "#198754", "#ffc107")) %>%
      layout(yaxis = list(title = "Total AMS Score (%)", range = c(0, 100)),
             showlegend = FALSE)
    p
  })
  
  # Chart B: Sustainability Gap (Line Chart)
  output$chart_staffing <- renderPlotly({
    req(final_data())
    df <- final_data()
    
    # Summarize by Phase and Staffing
    df_summ <- df %>%
      group_by(Phase, Staffing_Status) %>%
      summarise(Mean_Score = mean(Total_AMS_Score, na.rm = TRUE), .groups = "drop")
    
    # Ensure Factor Order and Sort Data
    df_summ$Phase <- factor(df_summ$Phase, levels = c("Baseline", "Intervention", "FollowUp"))
    df_summ <- df_summ %>% arrange(Phase)
    
    p <- plot_ly(df_summ, x = ~Phase, y = ~Mean_Score, color = ~Staffing_Status, 
                 colors = c("#198754", "#dc3545"), # Ongoing=Green, Stopped=Red
                 type = 'scatter', mode = 'lines+markers', line = list(width = 3)) %>%
      layout(yaxis = list(title = "Mean AMS Score (%)", range = c(0, 100)),
             xaxis = list(title = "Phase"),
             legend = list(title = list(text = "Staffing Status")))
    p
  })
  
  # Chart C: Radar Chart
  output$chart_radar <- renderPlotly({
    req(final_data())
    df <- final_data()
    
    # Summarize subscores by Phase
    sub_cols <- c("Framework_Score", "Resources_Score", "Prevention_Score", "Surveillance_Score", "Evaluation_Score")
    
    df_radar <- df %>%
      group_by(Phase) %>%
      summarise(across(all_of(sub_cols), median, na.rm = TRUE))
    
    categories <- c("Framework", "Resources", "Prevention", "Surveillance", "Evaluation")
    
    # Helper to create radar trace
    add_radar_trace <- function(p, phase_name, color_code, show_legend = TRUE) {
      vals <- as.numeric(df_radar[df_radar$Phase == phase_name, sub_cols])
      # Close the loop
      vals <- c(vals, vals[1])
      cats <- c(categories, categories[1])
      
      p %>% add_trace(
        r = vals,
        theta = cats,
        name = phase_name,
        fill = 'toself',
        fillcolor = paste0(color_code, "33"), # Add transparency
        line = list(color = color_code),
        showlegend = show_legend
      )
    }
    
    p <- plot_ly(type = 'scatterpolar', mode = 'lines')
    
    if (input$radar_phase == "Compare All" || input$radar_phase == "Baseline") {
      p <- add_radar_trace(p, "Baseline", "#6c757d") # Grey
    }
    if (input$radar_phase == "Compare All" || input$radar_phase == "Intervention") {
      p <- add_radar_trace(p, "Intervention", "#0d6efd") # Blue
    }
    if (input$radar_phase == "Compare All" || input$radar_phase == "FollowUp") {
      p <- add_radar_trace(p, "FollowUp", "#ffc107") # Orange
    }
    
    p <- p %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100)
          )
        ),
        showlegend = TRUE
      )
    p
  })
}
shinyApp(ui, server)