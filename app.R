library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(nflreadr)

# Load your combined dataset (already merged)
all_data <- read.csv("all_data.csv")  # Adjust the path if necessary

df <- load_players()

all_data <- all_data %>%
  left_join(df %>% select(smart_id, birth_date), by = "smart_id")


all_data <- all_data %>%
  mutate(
    birth_year = lubridate::year(as.Date(birth_date)),  # Extract birth year
    birth_month_day = format(as.Date(birth_date), "%m-%d"),  # Extract MM-DD format
    reference_date = as.Date(paste0(Year, "-09-04")),  # Define Sept 4th of each season
    age = floor(as.numeric(difftime(reference_date, as.Date(birth_date), units = "days")) / 365.25)  # Compute age
  )


final_blocking <- read.csv("final_blocking.csv")
final_defense <- read.csv("final_defense.csv")
final_rushing <- read.csv("final_rushing.csv")
final_receiving <- read.csv("final_receiving.csv")
final_passing <- read.csv("final_passing.csv")


final_defense <- final_defense %>%
  left_join(df %>% select(smart_id, birth_date), by = "smart_id") %>%
  mutate(
    birth_year = lubridate::year(as.Date(birth_date)),  # Extract birth year
    birth_month_day = format(as.Date(birth_date), "%m-%d"),  # Extract MM-DD format
    reference_date = as.Date(paste0(Year, "-09-04")),  # Define Sept 4th of each season
    age = floor(as.numeric(difftime(reference_date, as.Date(birth_date), units = "days")) / 365.25)  # Compute age
  ) 

final_passing <- final_passing %>%
  left_join(df %>% select(smart_id, birth_date), by = "smart_id") %>%
  mutate(
    birth_year = lubridate::year(as.Date(birth_date)),  
    birth_month_day = format(as.Date(birth_date), "%m-%d"),  
    reference_date = as.Date(paste0(Year, "-09-04")),  
    age = floor(as.numeric(difftime(reference_date, as.Date(birth_date), units = "days")) / 365.25)  
  ) 

final_rushing <- final_rushing %>%
  left_join(df %>% select(smart_id, birth_date), by = "smart_id") %>%
  mutate(
    birth_year = lubridate::year(as.Date(birth_date)),  
    birth_month_day = format(as.Date(birth_date), "%m-%d"),  
    reference_date = as.Date(paste0(Year, "-09-04")),  
    age = floor(as.numeric(difftime(reference_date, as.Date(birth_date), units = "days")) / 365.25)  
  ) 

final_receiving <- final_receiving %>%
  left_join(df %>% select(smart_id, birth_date), by = "smart_id") %>%
  mutate(
    birth_year = lubridate::year(as.Date(birth_date)),  
    birth_month_day = format(as.Date(birth_date), "%m-%d"),  
    reference_date = as.Date(paste0(Year, "-09-04")),  
    age = floor(as.numeric(difftime(reference_date, as.Date(birth_date), units = "days")) / 365.25)  
  ) 

final_blocking <- final_blocking %>%
  left_join(df %>% select(smart_id, birth_date), by = "smart_id") %>%
  mutate(
    birth_year = lubridate::year(as.Date(birth_date)),  
    birth_month_day = format(as.Date(birth_date), "%m-%d"),  
    reference_date = as.Date(paste0(Year, "-09-04")),  
    age = floor(as.numeric(difftime(reference_date, as.Date(birth_date), units = "days")) / 365.25)  
  ) 



# Replace NA values in `draftround` with "UDFA" for all datasets
final_blocking <- final_blocking %>%
  mutate(draftround = ifelse(is.na(draftround), "UDFA", as.character(draftround)))

final_passing <- final_passing %>%
  mutate(draftround = ifelse(is.na(draftround), "UDFA", as.character(draftround)))

final_rushing <- final_rushing %>%
  mutate(draftround = ifelse(is.na(draftround), "UDFA", as.character(draftround)))

final_receiving <- final_receiving %>%
  mutate(draftround = ifelse(is.na(draftround), "UDFA", as.character(draftround)))

final_defense <- final_defense %>%
  mutate(draftround = ifelse(is.na(draftround), "UDFA", as.character(draftround)))

all_data <- all_data %>%
  mutate(draftround = ifelse(is.na(draftround), "UDFA", as.character(draftround)))




# Shiny Vis ---------------------------------------------------------------


# Define UI

ui <- fluidPage(
  titlePanel("NFL Draft Performance Dashboard"),
  tabsetPanel(
    # Position Comparison Tab (FULLY PRESERVED)
    tabPanel(
      "Compare Positions",
      sidebarLayout(
        sidebarPanel(
          selectInput("facet_by_round_position", "Facet By Draft Round (Optional):", 
                      choices = c("None" = "none", "Draft Round" = "draftround")),
          selectInput("position_x_axis", "X-Axis:", choices = c("Season" = "season", "Age" = "age"), selected = "season"),
          selectizeInput("highlight_position", "Highlight Position:", choices = NULL),
          selectizeInput("highlight_player_position", "Highlight Player (Optional):", choices = NULL, multiple = FALSE),
          # ✅ NEW Snap Count Sliders
          sliderInput("snap_counts_defense_range", "Filter by Defensive Snaps:", 
                      min = 0, max = 1500, value = c(0, 1500), step = 25),
          sliderInput("routes_receiving_range", "Filter by Routes Run:", 
                      min = 0, max = 800, value = c(0, 800), step = 10),
          sliderInput("attempts_rushing_range", "Filter by Rush Attempts:", 
                      min = 0, max = 500, value = c(0, 500), step = 10),
          sliderInput("attempts_passing_range", "Filter by Pass Attempts:", 
                      min = 0, max = 800, value = c(0, 800), step = 10),
          sliderInput("snap_counts_block_range", "Filter by Blocking Snaps:", 
                      min = 0, max = 1500, value = c(0, 1500), step = 25),
          actionButton("apply_filters_position", "Apply Filters")
        ),
        mainPanel(plotlyOutput("position_comparison_chart", height = "900px"))
      )
    ),
    # Team Comparison Tab (FULLY PRESERVED)
    tabPanel(
      "Team Comparison",
      sidebarLayout(
        sidebarPanel(
          selectizeInput("team_comparison", "Select Team:", choices = NULL),
          selectInput("team_x_axis", "Compare By:", choices = c("Season" = "season", "Age" = "age")),
          selectInput("facet_by_round_team", "Facet By Draft Round (Optional):", 
                      choices = c("None" = "none", "Draft Round" = "draftround")),
          selectizeInput("filter_positions_team", "Filter Positions (Optional):", choices = NULL, multiple = TRUE),
          selectizeInput("highlight_player_team", "Highlight Player (Optional):", choices = NULL, multiple = FALSE),
          # ✅ NEW Snap Count Sliders
          sliderInput("snap_counts_defense_range", "Filter by Defensive Snaps:", 
                      min = 0, max = 1500, value = c(0, 1500), step = 25),
          sliderInput("routes_receiving_range", "Filter by Routes Run:", 
                      min = 0, max = 800, value = c(0, 800), step = 10),
          sliderInput("attempts_rushing_range", "Filter by Rush Attempts:", 
                      min = 0, max = 500, value = c(0, 500), step = 10),
          sliderInput("attempts_passing_range", "Filter by Pass Attempts:", 
                      min = 0, max = 800, value = c(0, 800), step = 10),
          sliderInput("snap_counts_block_range", "Filter by Blocking Snaps:", 
                      min = 0, max = 1500, value = c(0, 1500), step = 25),
          actionButton("apply_filters_team", "Apply Filters")
        ),
        mainPanel(plotlyOutput("team_comparison_chart", height = "900px"))
      )
    ),
    # Scatterplot Tabs for Each Dataset
    tabPanel("Scatterplot - Defense", sidebarLayout(
      sidebarPanel(
        selectInput("filter_position_defense", "Select Position:", choices = NULL),
        selectInput("x_defense", "Select X-axis:", choices = NULL),
        selectInput("y_defense", "Select Y-axis:", choices = NULL),
        selectInput("highlight_defense", "Select Highlight Metric (Optional):", 
                    choices = c("None" = "none"), selected = "none"),
        selectInput("year_defense", "Select Year:", choices = NULL),
        selectizeInput("filter_team_defense", "Filter by Team (Optional):", choices = NULL, multiple = TRUE),
        selectizeInput("highlight_team_defense", "Highlight Team (Optional):", choices = NULL, multiple = FALSE),
        selectizeInput("highlight_player_defense", "Highlight Player (Optional):", choices = NULL, multiple = FALSE),
        selectInput("facet_by_round_defense", "Facet By Draft Round (Optional):", 
                    choices = c("None" = "none", "Draft Round" = "draftround")),
        
        # ✅ **Slider for Snap Counts (Defense)**
        sliderInput("snap_counts_defense_range", "Minimum Defensive Snaps:", 
                    min = 0, max = 1500, value = c(0, 1500), step = 25),
        
        actionButton("apply_filters_defense", "Apply Filters")
      ),
      mainPanel(plotlyOutput("scatter_defense", height = "900px"))
    ))
    ,
    
    tabPanel("Scatterplot - Passing", sidebarLayout(
      sidebarPanel(
        selectInput("x_passing", "Select X-axis:", choices = NULL),
        selectInput("y_passing", "Select Y-axis:", choices = NULL),
        selectInput("highlight_passing", "Select Highlight Metric (Optional):", 
                    choices = c("None" = "none"), selected = "none"),
        selectInput("year_passing", "Select Year:", choices = NULL),
        selectizeInput("filter_team_passing", "Filter by Team (Optional):", choices = NULL, multiple = TRUE),
        selectizeInput("highlight_team_passing", "Highlight Team (Optional):", choices = NULL, multiple = FALSE),
        selectizeInput("highlight_player_passing", "Highlight Player (Optional):", choices = NULL, multiple = FALSE),
        selectInput("facet_by_round_passing", "Facet By Draft Round (Optional):", 
                    choices = c("None" = "none", "Draft Round" = "draftround")),
        
        # ✅ **Slider for Pass Attempts (Passing)**
        sliderInput("attempts_passing_range", "Minimum Pass Attempts:", 
                    min = 0, max = 800, value = c(0, 800), step = 10),
        
        actionButton("apply_filters_passing", "Apply Filters")
      ),
      mainPanel(plotlyOutput("scatter_passing", height = "900px"))
    ))
    ,
    
    tabPanel("Scatterplot - Rushing", sidebarLayout(
      sidebarPanel(
        selectInput("filter_position_rushing", "Select Position:", choices = NULL),
        selectInput("x_rushing", "Select X-axis:", choices = NULL),
        selectInput("y_rushing", "Select Y-axis:", choices = NULL),
        selectInput("highlight_rushing", "Select Highlight Metric (Optional):", 
                    choices = c("None" = "none"), selected = "none"),
        selectInput("year_rushing", "Select Year:", choices = NULL),
        selectizeInput("filter_team_rushing", "Filter by Team (Optional):", choices = NULL, multiple = TRUE),
        selectizeInput("highlight_team_rushing", "Highlight Team (Optional):", choices = NULL, multiple = FALSE),
        selectizeInput("highlight_player_rushing", "Highlight Player (Optional):", choices = NULL, multiple = FALSE),
        selectInput("facet_by_round_rushing", "Facet By Draft Round (Optional):", 
                    choices = c("None" = "none", "Draft Round" = "draftround")),
        
        # ✅ **Slider for Attempts (Rushing)**
        sliderInput("attempts_rushing_range", "Minimum Rush Attempts:", 
                    min = 0, max = 500, value = c(0, 500), step = 10),
        
        actionButton("apply_filters_rushing", "Apply Filters")
      ),
      mainPanel(plotlyOutput("scatter_rushing", height = "900px"))
    ))
    ,
    
    tabPanel("Scatterplot - Receiving", sidebarLayout(
      sidebarPanel(
        selectInput("filter_position_receiving", "Select Position:", choices = NULL),
        selectInput("x_receiving", "Select X-axis:", choices = NULL),
        selectInput("y_receiving", "Select Y-axis:", choices = NULL),
        selectInput("highlight_receiving", "Select Highlight Metric (Optional):", 
                    choices = c("None" = "none"), selected = "none"),
        selectInput("year_receiving", "Select Year:", choices = NULL),
        selectizeInput("filter_team_receiving", "Filter by Team (Optional):", choices = NULL, multiple = TRUE),
        selectizeInput("highlight_team_receiving", "Highlight Team (Optional):", choices = NULL, multiple = FALSE),
        selectizeInput("highlight_player_receiving", "Highlight Player (Optional):", choices = NULL, multiple = FALSE),
        selectInput("facet_by_round_receiving", "Facet By Draft Round (Optional):", 
                    choices = c("None" = "none", "Draft Round" = "draftround")),
        
        # ✅ **Slider for Routes (Receiving)**
        sliderInput("routes_receiving_range", "Minimum Routes Run:", 
                    min = 0, max = 800, value = c(0, 800), step = 10),
        
        actionButton("apply_filters_receiving", "Apply Filters")
      ),
      mainPanel(plotlyOutput("scatter_receiving", height = "900px"))
    ))
    ,
    
    tabPanel("Scatterplot - Blocking", sidebarLayout(
      sidebarPanel(
        selectInput("filter_position_blocking", "Select Position:", choices = NULL),
        selectInput("x_blocking", "Select X-axis:", choices = NULL),
        selectInput("y_blocking", "Select Y-axis:", choices = NULL),
        selectInput("highlight_blocking", "Select Highlight Metric (Optional):", 
                    choices = c("None" = "none"), selected = "none"),
        selectInput("year_blocking", "Select Year:", choices = NULL),
        selectizeInput("filter_team_blocking", "Filter by Team (Optional):", choices = NULL, multiple = TRUE),
        selectizeInput("highlight_team_blocking", "Highlight Team (Optional):", choices = NULL, multiple = FALSE),
        selectizeInput("highlight_player_blocking", "Highlight Player (Optional):", choices = NULL, multiple = FALSE),
        selectInput("facet_by_round_blocking", "Facet By Draft Round (Optional):", 
                    choices = c("None" = "none", "Draft Round" = "draftround")),
        
        # ✅ **Slider for Snap Counts (Blocking)**
        sliderInput("snap_counts_block_range", "Minimum Blocking Snaps:", 
                    min = 0, max = 1500, value = c(0, 1500), step = 25),
        
        actionButton("apply_filters_blocking", "Apply Filters")
      ),
      mainPanel(plotlyOutput("scatter_blocking", height = "900px"))
    )),
  
    
    tabPanel("Scheme Alignment", sidebarLayout(
      sidebarPanel(
        selectInput("scheme_position", "Select Position:", choices = NULL),
        selectizeInput("scheme_player", "Select Player:", choices = NULL, multiple = FALSE),
        selectInput("scheme_metric", "Select Scheme Metric:", choices = NULL),
        selectInput("scheme_x_axis", "X-Axis (Time):", choices = c("Year", "season", "age")),
        actionButton("apply_scheme_filters", "Apply Filters")
      ),
      mainPanel(plotlyOutput("scheme_alignment_chart", height = "900px"))
    )),
    
    tabPanel("Metric Over Time", sidebarLayout(
      sidebarPanel(
        selectInput("metric_position", "Select Position:", choices = NULL),
        selectInput("metric_variable", "Select Metric:", choices = NULL),  # ✅ Fixed ID
        selectizeInput("metric_highlight_player", "Highlight Player:", choices = NULL, multiple = FALSE),
        # ✅ Snap Count Sliders
        selectInput("metric_x_axis", "Compare By:", choices = c("Season" = "season", "Age" = "age")),
        sliderInput("snap_counts_defense_range_metric", "Defense Snaps:", min = 0, max = 1500, value = c(0, 1500)),
        sliderInput("routes_receiving_range_metric", "Routes Run:", min = 0, max = 800, value = c(0, 800)),
        sliderInput("attempts_rushing_range_metric", "Rush Attempts:", min = 0, max = 500, value = c(0, 500)),
        sliderInput("attempts_passing_range_metric", "Pass Attempts:", min = 0, max = 800, value = c(0, 800)),
        sliderInput("snap_counts_block_range_metric", "Blocking Snaps:", min = 0, max = 1500, value = c(0, 1500)),
        actionButton("apply_metric_filters", "Apply Filters")  # ✅ Removed Facet Input (No "draftround" Needed)
      ),
      mainPanel(
        plotlyOutput("metric_over_time_chart", height = "900px")
      )
    ))
  )
)


# Define Server Logic
server <- function(input, output, session) {
  # Filtered Data (PRESERVED)
  filtered_data <- reactive({
    all_data <- all_data %>%
      mutate(
        grades = coalesce(grades_offense, grades_defense)
      ) 
    
    
    # ✅ Apply Snap Count Filtering (Corrected Positions)
    all_data <- all_data %>%
      filter(
        # ✅ **All defensive positions** (ED, CB, S, LB, NT, SLOT, FS, DI) use `snap_counts_defense`
        (position %in% c("ED", "CB", "S", "LB", "NT", "SLOT", "FS", "DI") & 
           between(snap_counts_defense, input$snap_counts_defense_range[1], input$snap_counts_defense_range[2])) |
          
          # ✅ **WR & TE use `routes`**
          (position %in% c("WR", "TE") & 
             between(routes, input$routes_receiving_range[1], input$routes_receiving_range[2])) |
          
          # ✅ **HB (RB) uses `attempts` for rushing**
          (position == "HB" & 
             between(attempts, input$attempts_rushing_range[1], input$attempts_rushing_range[2])) |
          
          # ✅ **QB uses `attempts` for passing**
          (position == "QB" & 
             between(attempts, input$attempts_passing_range[1], input$attempts_passing_range[2])) |
          
          # ✅ **Offensive Linemen (G, T, C) use `snap_counts_block`**
          (position %in% c("G", "T", "C") & 
             between(snap_counts_block, input$snap_counts_block_range[1], input$snap_counts_block_range[2]))
      )
    
    all_data
  })
  
  observe({
    dataset <- filtered_data()
    if (!is.null(dataset) && nrow(dataset) > 0) {
      
      # ✅ Preserve user's previously selected inputs safely
      current_highlight_position <- isolate(input$highlight_position)
      current_highlight_player_position <- isolate(input$highlight_player_position)
      current_team_comparison <- isolate(input$team_comparison)
      current_filter_positions_team <- isolate(input$filter_positions_team)
      current_highlight_player_team <- isolate(input$highlight_player_team)
      
      # ✅ Ensure that these selections exist before applying them
      updateSelectizeInput(
        session, "highlight_position",
        choices = unique(dataset$position),
        selected = if (isTruthy(current_highlight_position) && current_highlight_position %in% dataset$position) 
          current_highlight_position else "ED",
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "highlight_player_position",
        choices = unique(dataset$player),
        selected = if (isTruthy(current_highlight_player_position) && current_highlight_player_position %in% dataset$player) 
          current_highlight_player_position else NULL,
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "team_comparison",
        choices = unique(dataset$draft_club),
        selected = if (isTruthy(current_team_comparison) && current_team_comparison %in% dataset$draft_club) 
          current_team_comparison else NULL,
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "filter_positions_team",
        choices = unique(dataset$position),
        selected = if (isTruthy(current_filter_positions_team) && all(current_filter_positions_team %in% dataset$position)) 
          current_filter_positions_team else NULL,
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "highlight_player_team",
        choices = unique(dataset$player),
        selected = if (isTruthy(current_highlight_player_team) && current_highlight_player_team %in% dataset$player) 
          current_highlight_player_team else NULL,
        server = TRUE
      )
    }
  })
  
  
  # Position Comparison Chart (FULLY PRESERVED)
  output$position_comparison_chart <- renderPlotly({
    # ✅ Retrieve filtered dataset ONCE
    data <- filtered_data()
    req(data)
    
    highlight_position <- input$highlight_position
    highlight_player <- input$highlight_player_position
    x_axis_choice <- input$position_x_axis  # "season" or "age"
    
    # ✅ Ensure `draftround` exists before faceting
    has_draftround <- "draftround" %in% colnames(data)
    
    # ✅ Assign `highlight_flag` BEFORE Aggregation
    data <- data %>%
      mutate(highlight_flag = ifelse(position == highlight_position, "highlight", "dim"))
    
    # ✅ Extract Highlight Player Data BEFORE Aggregation
    highlight_player_data <- NULL
    if (!is.null(highlight_player) && highlight_player != "") {
      player_data <- data %>% filter(player == highlight_player)
      if (nrow(player_data) > 0) {
        player_draftround <- unique(player_data$draftround)
        highlight_player_data <- player_data %>%
          select(player, position, .data[[x_axis_choice]], grades, draftround, highlight_flag) %>%
          filter(draftround == player_draftround)
      }
    }
    
    # ✅ Apply Grouping Logic
    if (input$facet_by_round_position == "draftround" && has_draftround) {
      combined_data <- data %>%
        group_by(.data[[x_axis_choice]], position, draftround, highlight_flag) %>%
        summarize(
          grades = mean(grades, na.rm = TRUE),
          count = n(),
          .groups = "drop"
        ) %>%
        ungroup()
      
      # ✅ Ensure at least one non-empty draft round is available
      unique_draftrounds <- unique(combined_data$draftround)
      
      # ✅ Generate plots for each draft round
      plots_list <- lapply(unique_draftrounds, function(draft_round) {
        df <- combined_data %>% filter(draftround == draft_round)
        
        plot_obj <- plot_ly(
          data = df,
          x = ~.data[[x_axis_choice]],
          y = ~grades,
          color = ~highlight_flag,
          split = ~position,
          colors = c("dim" = "grey", "highlight" = "blue"),
          showlegend = FALSE,
          type = 'scatter',
          mode = 'lines+markers',
          hoverinfo = ifelse(df$highlight_flag == "highlight", "text", "none"),
          text = ~paste0(
            "Position: ", position, "<br>",
            "Mean Grade: ", round(grades, 2), "<br>",
            "Entries: ", count, "<br>",
            x_axis_choice, ": ", .data[[x_axis_choice]]
          )
        )
        
        # ✅ Add Highlighted Player Data to the Correct Draft Round
        if (!is.null(highlight_player_data) && nrow(highlight_player_data) > 0) {
          highlight_df <- highlight_player_data %>% filter(draftround == draft_round)
          if (nrow(highlight_df) > 0) {
            plot_obj <- plot_obj %>%
              add_trace(
                data = highlight_df,
                x = ~.data[[x_axis_choice]],
                y = ~grades,
                type = "scatter",
                mode = "markers",
                marker = list(color = "purple", size = 7, symbol = "x"),
                name = highlight_player,
                hoverinfo = "text",
                text = ~paste0("Player: ", player, "<br>Grade: ", round(grades, 2))
              )
          }
        }
        
        plot_obj %>%
          layout(
            title = "",  # ✅ Remove subplot title (handled by annotation)
            annotations = list(
              list(
                x = 0, y = 1,  # ✅ Top-left corner
                xref = "paper", yref = "paper",
                text = paste("Draft Round:", draft_round),
                showarrow = FALSE,
                font = list(size = 12, color = "black"),
                xanchor = "left", yanchor = "top"
              )
            ),
            yaxis = list(title = "Grades", range = c(20, 100)),
            xaxis = list(title = ifelse(x_axis_choice == "season", "Season", "Age"))  # ✅ Fix X-axis label
          )
      })
      
      # ✅ Dynamically calculate number of rows
      nrows_dynamic <- max(1, ceiling(0.5 * length(plots_list)))
      
      # ✅ Use `do.call(subplot, ...)` to ensure proper grid structure
      final_plot <- do.call(subplot, c(plots_list, list(nrows = nrows_dynamic, shareX = TRUE, titleY = TRUE))) %>%
        layout(
          title = "Comparison of Positions Across All Data",
          showlegend = FALSE
        )
    } else {
      combined_data <- data %>%
        group_by(.data[[x_axis_choice]], position, highlight_flag) %>%
        summarize(
          grades = mean(grades, na.rm = TRUE),
          count = n(),
          .groups = "drop"
        ) %>%
        ungroup()
      
      final_plot <- plot_ly(
        data = combined_data,
        x = ~.data[[x_axis_choice]], 
        y = ~grades, 
        color = ~highlight_flag, 
        split = ~position,
        colors = c("dim" = "grey", "highlight" = "blue"),
        showlegend = FALSE,
        type = 'scatter', 
        mode = 'lines+markers',
        hoverinfo = ifelse(combined_data$highlight_flag == "highlight", "text", "none"),
        text = ~paste0(
          "Position: ", position, "<br>",
          "Mean Grade: ", round(grades, 2), "<br>",
          "Entries: ", count, "<br>",
          x_axis_choice, ": ", .data[[x_axis_choice]]
        )
      ) 
      
      # ✅ Add Highlighted Player (Ensures It Always Displays)
      if (!is.null(highlight_player_data) && nrow(highlight_player_data) > 0) {
        final_plot <- final_plot %>%
          add_trace(
            data = highlight_player_data,
            x = ~.data[[x_axis_choice]],
            y = ~grades,
            type = "scatter",
            mode = "markers",
            marker = list(color = "purple", size = 7, symbol = "x"),
            name = highlight_player,
            hoverinfo = "text",
            text = ~paste0("Player: ", player, "<br>Grade: ", round(grades, 2))
          )
      }
      
      final_plot <- final_plot %>%
        layout(
          title = "Comparison of Positions Across All Data",
          xaxis = list(title = ifelse(x_axis_choice == "season", "Season", "Age")),  # ✅ Fix X-axis label
          yaxis = list(title = "Grades", range = c(30, 100)),
          showlegend = FALSE
        )
    }
    
    return(final_plot)
  })
  
  
  
  
  observe({
    dataset <- filtered_data()
    if (!is.null(dataset) && nrow(dataset) > 0) {
      
      # ✅ Preserve user's previously selected inputs safely
      current_team_comparison <- isolate(input$team_comparison)
      current_filter_positions_team <- isolate(input$filter_positions_team)
      current_highlight_player_team <- isolate(input$highlight_player_team)
      
      # ✅ Select the first available team as default if `NULL`
      default_team <- if (is.null(current_team_comparison) || !current_team_comparison %in% dataset$draft_club) {
        unique(dataset$draft_club)[1]  # Select the first team in dataset
      } else {
        current_team_comparison
      }
      
      # ✅ Debugging: Check what's happening before updates
      print(paste("Current Team Comparison (Before Update):", current_team_comparison))
      print(paste("Default Team Selected:", default_team))
      print(paste("Available Teams in Dataset:", paste(unique(dataset$draft_club), collapse = ", ")))
      print(paste("Current Filtered Positions:", paste(current_filter_positions_team, collapse = ", ")))
      print(paste("Available Positions:", paste(unique(dataset$position), collapse = ", ")))
      print(paste("Current Highlight Player Team:", current_highlight_player_team))
      print(paste("Available Players:", paste(unique(dataset$player), collapse = ", ")))
      
      # ✅ Ensure that these selections exist before applying them
      updateSelectizeInput(
        session, "team_comparison",
        choices = unique(dataset$draft_club),
        selected = default_team,  # ✅ Set default team
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "filter_positions_team",
        choices = unique(dataset$position),
        selected = if (isTruthy(current_filter_positions_team) && all(current_filter_positions_team %in% dataset$position)) 
          current_filter_positions_team else NULL,
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "highlight_player_team",
        choices = unique(dataset$player),
        selected = if (isTruthy(current_highlight_player_team) && current_highlight_player_team %in% dataset$player) 
          current_highlight_player_team else NULL,
        server = TRUE
      )
    }
  })
  
  
  
  # Team Comparison Chart (FULLY PRESERVED)
  output$team_comparison_chart <- renderPlotly({
    data <- filtered_data()
    req(data)
    
    req(input$team_comparison, cancelOutput = TRUE)
    
    x_axis_choice <- input$team_x_axis  # "season" or "age"
    highlight_player <- input$highlight_player_team
    
    # ✅ Apply Position Filtering
    if (!is.null(input$filter_positions_team) && length(input$filter_positions_team) > 0) {
      data <- data %>% filter(position %in% input$filter_positions_team)
    }
    
    # ✅ Group other teams into "Rest of NFL"
    data <- data %>%
      mutate(team_group = ifelse(team_name == input$team_comparison, team_name, "Rest of NFL"))
    
    # ✅ Define consistent color mapping
    team_color_map <- setNames(c("blue", "red"), c("Rest of NFL", input$team_comparison))
    
    # ✅ Check for `draftround`
    has_draftround <- "draftround" %in% colnames(data)
    
    # ✅ Extract Highlight Player Data BEFORE Aggregation
    highlight_player_data <- NULL
    if (!is.null(highlight_player) && highlight_player != "") {
      player_data <- data %>% filter(player == highlight_player)
      if (nrow(player_data) > 0) {
        player_draftround <- unique(player_data$draftround)  # ✅ Get the player's draft round
        highlight_player_data <- player_data %>%
          select(player, position, .data[[x_axis_choice]], grades, draftround, team_group) %>%
          filter(draftround == player_draftround)  # ✅ Show only in their draft round
      }
    }
    
    # ✅ Apply Grouping Logic
    if (input$facet_by_round_team == "draftround" && has_draftround) {
      combined_data <- data %>%
        group_by(.data[[x_axis_choice]], team_group, draftround) %>%
        summarize(
          grades = mean(grades, na.rm = TRUE),
          count = n(),
          .groups = "drop"
        ) %>%
        ungroup()
      
      unique_draftrounds <- unique(combined_data$draftround)
      
      # ✅ Generate plots for each draft round
      plots_list <- lapply(unique_draftrounds, function(draft_round) {
        df <- combined_data %>% filter(draftround == draft_round)
        
        plot_obj <- plot_ly(
          data = df,
          x = ~.data[[x_axis_choice]],
          y = ~grades,
          color = ~team_group,
          colors = team_color_map,  # ✅ Apply consistent color mapping
          showlegend = FALSE,
          type = 'scatter',
          mode = 'lines+markers',
          hoverinfo = 'text',
          text = ~paste0(
            "Team: ", team_group, "<br>",
            "Mean Grade: ", round(grades, 2), "<br>",
            "Entries: ", count, "<br>",
            x_axis_choice, ": ", .data[[x_axis_choice]]
          )
        )
        
        # ✅ Add Highlighted Player Data to the Correct Draft Round
        if (!is.null(highlight_player_data) && nrow(highlight_player_data) > 0) {
          highlight_df <- highlight_player_data %>% filter(draftround == draft_round)
          if (nrow(highlight_df) > 0) {
            plot_obj <- plot_obj %>%
              add_trace(
                data = highlight_df,
                x = ~.data[[x_axis_choice]],
                y = ~grades,
                type = "scatter",
                mode = "markers",
                marker = list(color = "purple", size = 7, symbol = "x"),
                name = highlight_player,
                hoverinfo = "text",
                text = ~paste0("Player: ", player, "<br>Grade: ", round(grades, 2))
              )
          }
        }
        
        plot_obj %>%
          layout(
            title = "",
            annotations = list(
              list(
                x = 0, y = 1,  # ✅ Top-left corner
                xref = "paper", yref = "paper",
                text = paste("Draft Round:", draft_round),
                showarrow = FALSE,
                font = list(size = 12, color = "black"),
                xanchor = "left", yanchor = "top"
              )
            ),
            yaxis = list(title = "Grades", range = c(20, 100)),
            xaxis = list(title = ifelse(x_axis_choice == "season", "Season", "Age"))  # ✅ Fix X-axis label
          )
      })
      
      # ✅ Dynamically calculate number of rows for subplot
      nrows_dynamic <- max(1, ceiling(0.5 * length(plots_list)))
      
      final_plot <- do.call(subplot, c(plots_list, list(nrows = nrows_dynamic, shareX = TRUE, titleY = TRUE))) %>%
        layout(
          title = paste("Comparison of", input$team_comparison, "vs. Rest of NFL"),
          showlegend = FALSE
        )
    } else {
      combined_data <- data %>%
        group_by(.data[[x_axis_choice]], team_group) %>%
        summarize(
          grades = mean(grades, na.rm = TRUE),
          count = n(),
          .groups = "drop"
        ) %>%
        ungroup()
      
      final_plot <- plot_ly(
        data = combined_data,
        x = ~.data[[x_axis_choice]], 
        y = ~grades, 
        color = ~team_group, 
        colors = team_color_map,
        showlegend = FALSE,
        type = 'scatter', 
        mode = 'lines+markers',
        hoverinfo = 'text',
        text = ~paste0(
          "Team: ", team_group, "<br>",
          "Mean Grade: ", round(grades, 2), "<br>",
          "Entries: ", count, "<br>",
          x_axis_choice, ": ", .data[[x_axis_choice]]
        )
      ) 
      
      # ✅ Add Highlighted Player Data
      if (!is.null(highlight_player_data) && nrow(highlight_player_data) > 0) {
        final_plot <- final_plot %>%
          add_trace(
            data = highlight_player_data,
            x = ~.data[[x_axis_choice]],
            y = ~grades,
            type = "scatter",
            mode = "markers",
            marker = list(color = "purple", size = 7, symbol = "x"),
            name = highlight_player,
            hoverinfo = "text",
            text = ~paste0("Player: ", player, "<br>Grade: ", round(grades, 2))
          )
      }
      
      final_plot <- final_plot %>%
        layout(
          title = paste("Comparison of", input$team_comparison, "vs. Rest of NFL"),
          xaxis = list(title = ifelse(x_axis_choice == "season", "Season", "Age")),
          yaxis = list(title = "Grades", range = c(40, 100)),
          showlegend = FALSE
        )
    }
    
    return(final_plot)
  })
  
  
  
  
  
  
  
  # Scatterplot Tabs (UPDATED)
  datasets <- list(
    "Defense" = final_defense, 
    "Passing" = final_passing, 
    "Rushing" = final_rushing, 
    "Receiving" = final_receiving, 
    "Blocking" = final_blocking
  )
  
  # Define excluded variables for each dataset
  excluded_vars <- list(
    "Defense" = c("player_id", "player_game_count", "declined_penalties", "forced_fumbles", "franchise_id", 
                  "fumble_recoveries", "fumble_recovery_touchdowns", "interception_touchdowns", "interceptions", 
                  "longest", "safeties", "Year", "draftround", "draft_number", "rookie_year", "season", "birth_year"),
    
    "Passing" = c("player_id", "player_game_count", "aimed_passes", "big_time_throws", "completions", 
                  "declined_penalties", "drops", "first_downs", "franchise_id", "grades_hands_fumble", 
                  "penalties", "sacks", "spikes", "thrown_aways", "turnover_worthy_plays", 
                  "Year", "draftround", "draft_number", "rookie_year", "season", "birth_year"),
    
    "Rushing" = c("player_id", "player_game_count", "breakaway_attempts", "declined_penalties", "drops", 
                  "designed_yards", "first_downs", "franchise_id", "grades_hands_fumble", 
                  "grades_offense_penalty", "grades_pass", "grades_run_block", "longest", 
                  "penalties", "Year", "draftround", "draft_number", "rookie_year", "season", "birth_year"),
    
    "Blocking" = c("player_id", "player_game_count", "declined_penalties", "franchise_id", 
                   "Year", "draftround", "draft_number", "rookie_year", "season", "birth_year"),
    
    "Receiving" = c("player_id", "player_game_count", "declined_penalties", "franchise_id", 
                    "grades_hands_fumble", "grades_pass_block", "interceptions", "longest", 
                    "pass_block_rate", "pass_blocks", "Year", "draftround", "draft_number", "rookie_year", "season", "birth_year")
  )
  
  # Update Inputs for Scatterplots (Exclude unwanted variables)
  lapply(names(datasets), function(name) {
    observe({
      dataset <- datasets[[name]]
      if (!is.null(dataset) && nrow(dataset) > 0) {
        # Get only numeric columns that are NOT in the exclusion list
        numeric_cols <- colnames(dataset)[sapply(dataset, is.numeric)]
        numeric_cols <- setdiff(numeric_cols, excluded_vars[[name]])
        
        updateSelectInput(session, paste0("x_", tolower(name)), choices = numeric_cols)
        updateSelectInput(session, paste0("y_", tolower(name)), choices = numeric_cols)
        updateSelectInput(session, paste0("highlight_", tolower(name)), 
                          choices = c("None" = "none", numeric_cols), 
                          selected = "none")
        updateSelectInput(session, paste0("year_", tolower(name)), choices = unique(dataset$Year))
        updateSelectizeInput(session, paste0("filter_team_", tolower(name)), choices = unique(dataset$team_name), server = TRUE)
        updateSelectizeInput(session, paste0("highlight_team_", tolower(name)), choices = unique(dataset$team_name), server = TRUE)
        updateSelectizeInput(session, paste0("highlight_player_", tolower(name)), choices = unique(dataset$player), server = TRUE)
        
        # Only update position dropdowns for non-QB datasets
        if (name != "Passing") {
          updateSelectInput(session, paste0("filter_position_", tolower(name)), choices = unique(dataset$position))
        }
      }
    })
  })
  
  
  lapply(names(datasets), function(name) {
    output[[paste0("scatter_", tolower(name))]] <- renderPlotly({
      req(input[[paste0("x_", tolower(name))]], 
          input[[paste0("y_", tolower(name))]], 
          input[[paste0("year_", tolower(name))]],
          cancelOutput = TRUE)
      
      data <- datasets[[name]]
      req(data, nrow(data) > 0)
      
      highlight_metric <- input[[paste0("highlight_", tolower(name))]]
      highlight_metric <- if (highlight_metric == "none" || highlight_metric == "") NULL else highlight_metric
      
      facet_choice <- input[[paste0("facet_by_round_", tolower(name))]]
      facet_choice <- if (facet_choice == "none" || facet_choice == "") NULL else facet_choice
      
      # Filter dataset by selected year
      data <- data %>% filter(Year == input[[paste0("year_", tolower(name))]])
      
      # Apply position filter (except for Passing)
      if (name != "Passing") {
        req(input[[paste0("filter_position_", tolower(name))]])
        data <- data %>% filter(position == input[[paste0("filter_position_", tolower(name))]])
      }
      
      # Apply team filtering if selected
      filter_teams <- input[[paste0("filter_team_", tolower(name))]]
      if (!is.null(filter_teams) && length(filter_teams) > 0) {
        data <- data %>% filter(team_name %in% filter_teams)
      }
      
      # ✅ **Apply Snap Count Filtering Here**
      if (name == "Defense") {
        data <- data %>% filter(between(snap_counts_defense, input$snap_counts_defense_range[1], input$snap_counts_defense_range[2]))
      } else if (name == "Receiving") {
        data <- data %>% filter(between(routes, input$routes_receiving_range[1], input$routes_receiving_range[2]))
      } else if (name == "Rushing") {
        data <- data %>% filter(between(attempts, input$attempts_rushing_range[1], input$attempts_rushing_range[2]))
      } else if (name == "Passing") {
        data <- data %>% filter(between(attempts, input$attempts_passing_range[1], input$attempts_passing_range[2]))
      } else if (name == "Blocking") {
        data <- data %>% filter(between(snap_counts_block, input$snap_counts_block_range[1], input$snap_counts_block_range[2]))
      }
      
      
      # Handle highlight logic
      highlight_teams <- input[[paste0("highlight_team_", tolower(name))]]
      highlight_player <- input[[paste0("highlight_player_", tolower(name))]]
      
      data <- data %>% mutate(highlight_flag = "Other")
      
      if (!is.null(highlight_teams) && length(highlight_teams) > 0) {
        data <- data %>%
          mutate(highlight_flag = ifelse(team_name %in% highlight_teams, "Team Highlight", highlight_flag))
      }
      
      if (!is.null(highlight_player) && highlight_player != "") {
        data <- data %>%
          mutate(highlight_flag = ifelse(player == highlight_player, "Player Highlight", highlight_flag))
      }
      
      # Ensure facet column exists before faceting
      if (!is.null(facet_choice) && !facet_choice %in% colnames(data)) {
        print(paste("⚠️ WARNING: Facet variable", facet_choice, "not found in dataset"))
        facet_choice <- NULL  # Reset faceting
      }
      
      # **Determine global min/max for consistent scales**
      x_min <- min(data[[input[[paste0("x_", tolower(name))]]]], na.rm = TRUE)
      x_max <- max(data[[input[[paste0("x_", tolower(name))]]]], na.rm = TRUE)
      y_min <- min(data[[input[[paste0("y_", tolower(name))]]]], na.rm = TRUE)
      y_max <- max(data[[input[[paste0("y_", tolower(name))]]]], na.rm = TRUE)
      
      # ✅ **Tooltip Cleanup**
      data <- data %>% 
        mutate(tooltip_text = paste0(
          "<b>Player:</b> ", player, "<br>",
          "<b>Team:</b> ", team_name, "<br>"))
      
      # Base scatterplot structure
      p <- ggplot(data, aes_string(
        x = input[[paste0("x_", tolower(name))]], 
        y = input[[paste0("y_", tolower(name))]]
      )) +
        geom_point(aes(
          size = ifelse(highlight_flag != "Other", 4, 2),
          text = tooltip_text  # **Tooltip now displays player, team, and stat values**
        ), alpha = 0.7) +
        scale_size_identity() +
        labs(
          title = paste(name, "Scatterplot:", input[[paste0("x_", tolower(name))]], "vs", input[[paste0("y_", tolower(name))]]),
          x = input[[paste0("x_", tolower(name))]],
          y = input[[paste0("y_", tolower(name))]]
        ) +
        coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +  # **Apply fixed axis limits**
        theme_minimal()
      
      # ✅ **Only add color if a highlight metric is selected**
      if (!is.null(highlight_metric)) {
        p <- p + aes_string(color = highlight_metric) +
          scale_color_gradient(low = "blue", high = "red", name = highlight_metric) +
          guides(color = guide_colorbar(ticks = FALSE, barwidth = 1, barheight = 10, 
                                        title.position = "top", 
                                        title.hjust = 0.5, 
                                        frame.colour = "black", 
                                        label.theme = element_text(size = 10)))
      }
      
      # ✅ **Faceting Logic Update**
      if (!is.null(facet_choice)) {
        p <- p + facet_wrap(as.formula(paste("~", facet_choice)), scales = "fixed")  # **Fixed scales for all facets**
      }
      
      # ✅ **Ensure valid ggplot object is returned when NOT faceting**
      print("✅ Returning scatterplot")  
      return(ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE))  
    })
  })
  

  # ✅ Populate Position Dropdown (Removed G, C, T, QB)
  observe({
    dataset <- filtered_data()
    
    # ✅ Exclude QB, G, C, T
    valid_positions <- setdiff(unique(dataset$position), c("QB", "G", "C", "T"))
    
    updateSelectInput(session, "scheme_position", 
                      choices = valid_positions, 
                      selected = "ED")  # Default to ED
  })
  
  # ✅ Populate Players Dropdown Based on Selected Position
  observeEvent(input$scheme_position, {
    req(input$scheme_position)
    
    dataset <- filtered_data()
    
    # ✅ Get players for selected position
    players <- dataset %>% filter(position == input$scheme_position) %>% pull(player)
    
    # ✅ Update Player Dropdown (ensure at least one valid player is selected)
    updateSelectizeInput(session, "scheme_player", 
                         choices = unique(players), 
                         selected = ifelse(length(players) > 0, players[1], NULL),
                         server = TRUE)
  })
  
  # ✅ Populate Scheme Metric Dropdown Based on Position
  observeEvent(input$scheme_position, {
    req(input$scheme_position)
    
    scheme_options <- switch(input$scheme_position,
                             "HB"  = c("gap_attempts", "zone_attempts"),
                             "WR"  = c("wide_rate", "slot_rate"),
                             "TE"  = c("inline_rate", "wide_rate", "slot_rate"),
                             "ED"  = c("snap_counts_dl_a_gap", "snap_counts_dl_b_gap", "snap_counts_dl_over_t", "snap_counts_dl_outside_t"),
                             "NT"  = c("snap_counts_dl_a_gap", "snap_counts_dl_b_gap", "snap_counts_dl_over_t", "snap_counts_dl_outside_t"),
                             "DI"  = c("snap_counts_dl_a_gap", "snap_counts_dl_b_gap", "snap_counts_dl_over_t", "snap_counts_dl_outside_t"),
                             "LB"  = c("snap_counts_fs", "snap_counts_slot", "snap_counts_box", "snap_counts_dl", "snap_counts_corner", "snap_counts_offball"),
                             "SLOT" = c("snap_counts_fs", "snap_counts_slot", "snap_counts_box", "snap_counts_dl", "snap_counts_corner", "snap_counts_offball"),
                             "CB"  = c("snap_counts_fs", "snap_counts_slot", "snap_counts_box", "snap_counts_dl", "snap_counts_corner", "snap_counts_offball"),
                             "S"   = c("snap_counts_fs", "snap_counts_slot", "snap_counts_box", "snap_counts_dl", "snap_counts_corner", "snap_counts_offball"),
                             "FS"  = c("snap_counts_fs", "snap_counts_slot", "snap_counts_box", "snap_counts_dl", "snap_counts_corner", "snap_counts_offball"),
                             character(0)  # Default empty if no match
    )
    
    # ✅ Update Scheme Metric Dropdown
    updateSelectInput(session, "scheme_metric", 
                      choices = scheme_options, 
                      selected = ifelse(length(scheme_options) > 0, scheme_options[1], NULL))
  })
  
  # ✅ Debugging Block - Check Inputs & Dataset Structure
  observeEvent(input$apply_scheme_filters, {
    dataset <- filtered_data()
    
    print("🔍 Checking dataset structure...")
    print(str(dataset))
    
    print("🔍 Checking column names...")
    print(colnames(dataset))
    
    print(paste("Selected X-axis:", input$scheme_x_axis))
    print(paste("Selected Metric:", input$scheme_metric))
    print(paste("Selected Player:", input$scheme_player))
    
    # Check if selected X-axis exists
    if (!(input$scheme_x_axis %in% colnames(dataset))) {
      stop(paste("Error: Selected X-axis", input$scheme_x_axis, "not found in dataset"))
    }
    
    # Check if selected metric exists
    if (!(input$scheme_metric %in% colnames(dataset))) {
      stop(paste("Error: Selected Scheme Metric", input$scheme_metric, "not found in dataset"))
    }
    
    # Ensure `Year` and `season` exist
    if (!"Year" %in% colnames(dataset)) {
      dataset <- dataset %>% mutate(Year = NA_real_)
    }
    if (!"season" %in% colnames(dataset)) {
      dataset <- dataset %>% mutate(season = NA_real_)
    }
    
    # Print out the dataset after modifications
    print("🔍 Dataset after ensuring Year & season:")
    print(str(dataset))
  })
  
  # ✅ Render Scheme Alignment Chart with Custom Gradient
  output$scheme_alignment_chart <- renderPlotly({
    req(input$scheme_position, input$scheme_player, input$scheme_metric, input$scheme_x_axis)
    
    dataset <- filtered_data()  # Get dataset
    
    # ✅ Ensure required columns exist in dataset
    req(input$scheme_metric %in% colnames(dataset), 
        input$scheme_x_axis %in% colnames(dataset), 
        "grades" %in% colnames(dataset))
    
    # ✅ Compute the **mean grade** for the selected position based on user-selected X-axis (Year or Season)
    avg_position_data <- dataset %>%
      filter(position == input$scheme_position) %>%
      group_by(.data[[input$scheme_x_axis]]) %>%
      summarize(mean_grade = mean(grades, na.rm = TRUE), count = n(), .groups = "drop")
    
    # ✅ Filter dataset for the selected player
    player_data <- dataset %>% filter(player == input$scheme_player)
    
    # ✅ Compute the player's **relative grade**: Player's grade minus the average grade for that year/season
    player_data <- player_data %>%
      left_join(avg_position_data, by = input$scheme_x_axis) %>%
      mutate(relative_grade = grades - mean_grade)
    
    # ✅ Determine Fixed Gradient Scale Based on Full Dataset
    min_relative_grade <- min(dataset$grades - avg_position_data$mean_grade, na.rm = TRUE)
    max_relative_grade <- max(dataset$grades - avg_position_data$mean_grade, na.rm = TRUE)
    
    # ✅ Prepare the Mean Trend Data (Dashed Line for Position Average)
    mean_trend_data <- dataset %>%
      filter(position == input$scheme_position) %>%
      group_by(.data[[input$scheme_x_axis]]) %>%
      summarize(mean_metric = mean(.data[[input$scheme_metric]], na.rm = TRUE), count = n(), .groups = "drop")
    
    # ✅ Create Interactive Plot with Plotly
    p <- plot_ly() %>%
      
      # ✅ Add Mean Trend Line (Dashed)
      add_trace(
        data = mean_trend_data,
        x = ~.data[[input$scheme_x_axis]],
        y = ~mean_metric,
        type = "scatter",
        mode = "lines",
        line = list(color = "gray", dash = "dash", width = 2),
        hoverinfo = "text",
        text = ~paste0(
          "Entries: ", count, "<br>",
          "Avg Metric: ", round(mean_metric, 2)
        ),
        name = paste(input$scheme_position, "Avg")
      ) %>%
      
      # ✅ Add Player Points with Color Gradient
      # ✅ Player Points with Improved Color Bar Placement
      add_trace(
        data = player_data,
        x = ~.data[[input$scheme_x_axis]],
        y = ~.data[[input$scheme_metric]],
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 7,
          color = ~relative_grade,
          colorscale = list(c(0, 0.25, 0.5, 0.75, 1), c("darkblue", "blue", "purple", "red", "darkred")),

          
          # ✅ Adjust Color Bar Size & Placement
          colorbar = list(
            title = "Relative Grade",
            len = 0.4,  # ✅ Makes the scale **40% the height** of the figure (smaller)
            x = 1.05,   # ✅ Moves it **closer to the plot** (right)
            y = 0.5,    # ✅ Centers it **vertically**
            thickness = 15  # ✅ Reduces the **width** of the scale
          )
        ),
        hoverinfo = "text",
        text = ~paste0(
          "Player: ", input$scheme_player, "<br>",
          input$scheme_metric, ": ", round(.data[[input$scheme_metric]], 2), "<br>",
          "Relative Grade: ", round(relative_grade, 2)
        ),
        name = input$scheme_player
      ) %>%
      
      # ✅ Layout Adjustments
      layout(
        title = paste(input$scheme_player, "vs. Position Average"),
        xaxis = list(title = input$scheme_x_axis),
        yaxis = list(title = input$scheme_metric),
        legend = list(title = list(text = "Legend"))
      )
    
    return(p)
  })
  
  
  
  # Metric Over Time Tab - Server Logic
  
  # ✅ Populate Position Dropdown (DO NOT EXCLUDE QB, C, G, or T)
  observe({
    dataset <- filtered_data()
    
    valid_positions <- unique(dataset$position)  # Keep all positions
    
    updateSelectInput(session, "metric_position", 
                      choices = valid_positions, 
                      selected = "ED")  # Default to ED
  })
  
  # ✅ Populate Players Dropdown Based on Selected Position
  observeEvent(input$metric_position, {
    req(input$metric_position)
    
    dataset <- filtered_data()
    
    # ✅ Get players for the selected position
    players <- dataset %>% filter(position == input$metric_position) %>% pull(player)
    
    # ✅ Update Player Dropdown (Ensure selection remains valid)
    updateSelectizeInput(session, "metric_highlight_player", 
                         choices = unique(players), 
                         selected = ifelse(length(players) > 0, players[1], NULL),
                         server = TRUE)
  })
  
  # ✅ Populate Metric Dropdown Dynamically Based on Selected Position
  observeEvent(input$metric_position, {
    req(input$metric_position)
    
    # ✅ Define metric options based on position
    metric_options <- switch(input$metric_position,
                             "ED"  = c("grades_defense", "grades_defense_penalty", "grades_pass_rush_defense", "grades_run_defense", "grades_tackle", 
                                       "hits", "hurries", "sacks", "missed_tackle_rate", "penalties", "tackles_for_loss", "total_pressures"),
                             "DI"  = c("grades_defense", "grades_defense_penalty", "grades_pass_rush_defense", "grades_run_defense", "grades_tackle", 
                                       "hits", "hurries", "sacks", "missed_tackle_rate", "penalties", "tackles_for_loss", "total_pressures"),
                             "NT"  = c("grades_defense", "grades_defense_penalty", "grades_pass_rush_defense", "grades_run_defense", "grades_tackle", 
                                       "hits", "hurries", "sacks", "missed_tackle_rate", "penalties", "tackles_for_loss", "total_pressures"),
                             "SLOT" = c("grades_defense", "grades_coverage_defense", "catch_rate", "interceptions", "qb_rating_against", 
                                        "targets", "yards", "yards_per_reception", "yards_after_catch"),
                             "CB"  = c("grades_defense", "grades_coverage_defense", "catch_rate", "interceptions", "qb_rating_against", 
                                       "targets", "yards", "yards_per_reception", "yards_after_catch"),
                             "FS"  = c("grades_defense", "grades_coverage_defense", "catch_rate", "interceptions", "qb_rating_against", 
                                       "targets", "yards", "yards_per_reception", "yards_after_catch"),
                             "S"   = c("grades_defense", "grades_coverage_defense", "catch_rate", "interceptions", "qb_rating_against", 
                                       "targets", "yards", "yards_per_reception", "yards_after_catch"),
                             "LB"  = c("grades_defense", "grades_coverage_defense", "catch_rate", "interceptions", "qb_rating_against", 
                                       "targets", "yards", "yards_per_reception", "yards_after_catch"),
                             "QB"  = c("accuracy_percent", "attempts", "avg_depth_of_target", "avg_time_to_throw", "btt_rate", 
                                       "completion_percent", "dropbacks", "grades_offense", "grades_pass", "grades_run", 
                                       "qb_rating", "sack_percent", "scrambles"),
                             "HB"  = c("ypa", "avoided_tackles", "breakaway_attempts", "breakaway_percent", "breakaway_yards", 
                                       "grades_offense", "grades_pass_block", "grades_run_block", "rec_yards", "routes", "run_plays", 
                                       "total_touches", "yards_after_contact", "yprr", "zone_attempts"),
                             "WR"  = c("drop_rate", "caught_percent", "contested_catch_rate", "grades_offense", "grades_hands_drop", "pass_plays", 
                                       "route_rate", "targeted_qb_rating", "yards_after_catch_per_reception", "yprr", "avoided_tackles"),
                             "TE"  = c("drop_rate", "caught_percent", "contested_catch_rate", "grades_offense", "grades_hands_drop", "pass_plays", 
                                       "route_rate", "targeted_qb_rating", "yards_after_catch_per_reception", "grades_pass_block", 
                                       "grades_run_block"),
                             "C"   = c("pbe", "hits_allowed", "hurries_allowed", "pressures_allowed", "sacks_allowed", 
                                       "snap_counts_block", "grades_offense", "grades_pass_block", "grades_run_block"),
                             "G"   = c("pbe", "hits_allowed", "hurries_allowed", "pressures_allowed", "sacks_allowed", 
                                       "snap_counts_block", "grades_offense", "grades_pass_block", "grades_run_block"),
                             "T"   = c("pbe", "hits_allowed", "hurries_allowed", "pressures_allowed", "sacks_allowed", 
                                       "snap_counts_block", "grades_offense", "grades_pass_block", "grades_run_block"),
                             character(0)  # Default empty if no match
    )
    
    # ✅ Update Metric Dropdown (Ensure valid selection)
    updateSelectInput(session, "metric_variable", 
                      choices = metric_options, 
                      selected = ifelse(length(metric_options) > 0, metric_options[1], NULL))
  })
  
  # ✅ Render Metric Over Time Plot
  output$metric_over_time_chart <- renderPlotly({
    req(input$metric_position, input$metric_variable, input$metric_x_axis)
    
    dataset <- filtered_data()
    
    # ✅ Apply Position-Specific Snap Count Filtering
    dataset <- dataset %>%
      filter(
        (position %in% c("ED", "CB", "S", "LB", "NT", "SLOT", "FS", "DI") & 
           between(snap_counts_defense, input$snap_counts_defense_range_metric[1], input$snap_counts_defense_range_metric[2])) |
          
          (position %in% c("WR", "TE") & 
             between(routes, input$routes_receiving_range_metric[1], input$routes_receiving_range_metric[2])) |
          
          (position == "HB" & 
             between(attempts, input$attempts_rushing_range_metric[1], input$attempts_rushing_range_metric[2])) |
          
          (position == "QB" & 
             between(attempts, input$attempts_passing_range_metric[1], input$attempts_passing_range_metric[2])) |
          
          (position %in% c("G", "T", "C") & 
             between(snap_counts_block, input$snap_counts_block_range_metric[1], input$snap_counts_block_range_metric[2]))
      )
    
    # ✅ Ensure valid selection
    req(input$metric_position %in% dataset$position)
    
    # ✅ Compute mean value over time (Age or Season)
    avg_position_data <- dataset %>%
      filter(position == input$metric_position) %>%
      group_by(.data[[input$metric_x_axis]]) %>%
      summarize(mean_value = mean(.data[[input$metric_variable]], na.rm = TRUE), count = n(), .groups = "drop")
    
    # ✅ Highlight Player Data
    highlight_player_data <- dataset %>%
      filter(player == input$metric_highlight_player & position == input$metric_position)
    
    # ✅ Base Plot with Correct Grouping
    p <- ggplot() +
      geom_line(data = avg_position_data, 
                aes(x = .data[[input$metric_x_axis]], y = mean_value, 
                    group = 1,  # ✅ Ensure the average line is treated as a single line
                    text = paste0("Entries: ", count, "<br>Mean Value: ", round(mean_value, 2))),
                color = "gray", linetype = "dashed", size = 1.2) +
      
      geom_point(data = highlight_player_data, 
                 aes(x = .data[[input$metric_x_axis]], y = .data[[input$metric_variable]], 
                     text = paste0("Player: ", input$metric_highlight_player, 
                                   "<br>Value: ", round(.data[[input$metric_variable]], 2))),
                 color = "blue", size = 3) +
      
      theme_minimal() +
      labs(title = paste("Comparison of", input$metric_highlight_player, "vs Position Average"),
           x = input$metric_x_axis, y = input$metric_variable)
    
    # ✅ Convert to Plotly with Tooltip
    ggplotly(p, tooltip = "text")
  })
  
}


# Run the app
shinyApp(ui, server)

