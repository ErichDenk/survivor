# Shiny App for scoring and displaying our pool
# http://shiny.rstudio.com/

##########################################
# Packages, Themes, and Functions -------
##########################################
library(shinydashboard) # For dashboard functions
library(bslib) # Easy prepackaged themes (optional)
library(fontawesome)
library(formattable) # Formatting of table
library(tidyverse)
library(tidyselect) # For helper functions
library(markdown)
library(forcats) # Easily deal with factors
library(googlesheets4)
library(ggiraph) # For interactive plots

# ---- Google Sheet Info ----
gs4_deauth()
sheet_id <- "1WFD5SAMUbJO40HbMj3TtjDCWgVbdxw0ftDgmVbhHIb4"

# ---- Read Sheets (must exist in your Google Sheet) ----
# Expect sheets named: "Tribes", "Picks", "Eliminated", "Finale", "Idols"
tribes <- read_sheet(sheet_id, sheet = "Tribes") %>% rename_with(tolower)
picks   <- read_sheet(sheet_id, sheet = "Picks")
eliminated <- read_sheet(sheet_id, sheet = "Eliminated") %>% rename_with(tolower)

# Finale sheet: optional sheet for marking finalists
# Columns: cast, finish (values: "winner", "second", "third")
finale <- tryCatch({
  read_sheet(sheet_id, sheet = "Finale") %>% rename_with(tolower)
}, error = function(e) {
  # If Finale sheet doesn't exist, return empty data frame
  data.frame(cast = character(), finish = character(), stringsAsFactors = FALSE)
})

# Idols sheet: optional sheet for tracking idol finds
# Columns: cast, week_found
idols <- tryCatch({
  read_sheet(sheet_id, sheet = "Idols") %>% rename_with(tolower)
}, error = function(e) {
  # If Idols sheet doesn't exist, return empty data frame
  data.frame(cast = character(), week_found = integer(), stringsAsFactors = FALSE)
})

# ---- Defensive checks ----
stopifnot("Tribes sheet must contain columns: cast, tribe, color" = 
            all(c("cast","color","tribe") %in% names(tribes)))

stopifnot("Picks sheet missing required columns" = 
            all(c("Contestant", "MVP", "Pick2", "Pick3", "Pick4", "Pick5") %in% names(picks)))

stopifnot("Eliminated sheet must contain columns: cast, week" = 
            all(c("cast","week") %in% names(eliminated)))

# Finale sheet is optional - only check if it exists and has data
if(nrow(finale) > 0) {
  stopifnot("Finale sheet must contain columns: cast, finish" = 
              all(c("cast","finish") %in% names(finale)))
}

# Idols sheet is optional - only check if it exists and has data
if(nrow(idols) > 0) {
  stopifnot("Idols sheet must contain columns: cast, week_found" = 
              all(c("cast","week_found") %in% names(idols)))
}

# ---- GGPlot uniform theme ----
mytheme <- function(){
  ggthemes::theme_tufte() +
    theme(
      # Lighter dark background - modern charcoal
      plot.background = element_rect(fill = "#2a2e3d", color = NA),
      panel.background = element_rect(fill = "#323745", color = NA),
      
      # Grid lines - subtle blue-gray
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted",
                                        size = 0.5, color = "#4a5566"), 
      
      # Axis styling - modern blue accent
      axis.line.x = element_line(linetype = "solid", color = "#4a90a4", size = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(face = "bold", color = "#e8eef2", size = 10),
      axis.text.x = element_text(color = "#d4dce3", size = 10),
      axis.text = element_text(size = 10, color = "#d4dce3"),
      
      # Title styling
      plot.title = element_text(face = "bold", color = "#6db8d4", size = 14,
                                family = "sans"),
      
      # Legend styling
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.background = element_rect(fill = "#2a2e3d", color = "#4a5566"),
      legend.text = element_text(color = "#e8eef2", size = 9),
      legend.key = element_rect(fill = "#323745")
    )
}

# ---- Globals ----
mergeweek <- 6
currentweek <- if(nrow(eliminated) > 0 && any(!is.na(eliminated$week))) {
  max(eliminated$week, na.rm = TRUE)
} else {
  1  # Season hasn't started yet
}

# ---- Finalist Detection ----
# Read finalists from Finale sheet
# Finale sheet columns: cast, finish (values: "winner", "second", "third")

winner <- if(nrow(finale) > 0 && any(finale$finish == "winner", na.rm = TRUE)) {
  finale %>% filter(finish == "winner") %>% pull(cast) %>% first()
} else NA

second <- if(nrow(finale) > 0 && any(finale$finish == "second", na.rm = TRUE)) {
  finale %>% filter(finish == "second") %>% pull(cast) %>% first()
} else NA

third <- if(nrow(finale) > 0 && any(finale$finish == "third", na.rm = TRUE)) {
  finale %>% filter(finish == "third") %>% pull(cast) %>% first()
} else NA

# Vector for Cast (from tribes)
castaways <- tribes %>% pull(cast)

# Tribe colors vector for badges (named vector)
tribe_colors <- setNames(tribes$color, tribes$cast)

# ---- Scoring Function (uses eliminated dataframe) ----
weekly_score <- function(x, y) {
  # Only the previously eliminated weeks considered
  nowelim <- eliminated %>%
    filter(!is.na(week) & week <= y) %>%
    pull(cast)
  
  # How many picks remain for the week?
  new <- x %>% 
    mutate(fullteam = as.vector(paste(MVP, Pick2, Pick3, Pick4, Pick5, sep = ",")),
           epi_remain = if(length(nowelim) > 0) {
             5 - str_count(fullteam, pattern = paste(nowelim, collapse = "|"))
           } else {
             5  # No eliminations yet, all picks remain
           }) 
  
  if (y < mergeweek) {
    new$epi_score <- new$epi_remain
  } else {
    new$epi_score <- new$epi_remain * 3
  }
  
  new %>%
    rename_with(~paste0(., y), epi_remain:epi_score)
}

# ---- Formatting helper for eliminated names ----
elimformatter <- formatter(
  "span",
  style = x ~ {
    # Only mark truly eliminated castaways (with non-NA weeks)
    truly_eliminated <- eliminated %>% 
      filter(!is.na(week)) %>% 
      pull(cast)
    
    style(
      color = ifelse(x %in% truly_eliminated, "gray", "black"),
      "text-decoration" = ifelse(x %in% truly_eliminated, "line-through", "none")
    )
  }
)

# ---- Helper Functions ----
place_color <- function(place) {
  case_when(
    place == 1 ~ "gold",      # custom gold CSS class
    place == 2 ~ "silver",    # custom silver CSS class
    place == 3 ~ "bronze",    # custom bronze CSS class
    TRUE ~ "aqua"
  )
}

# Tribe badge formatter for all picks
tribe_badge_formatter <- function(pick_col) {
  # Get list of truly eliminated castaways (with non-NA week values)
  truly_eliminated <- eliminated %>% 
    filter(!is.na(week)) %>% 
    pull(cast)
  
  formatter("span",
            style = x ~ {
              # Get tribe info for this castaway
              tribe_info <- tribes %>% filter(cast == x)
              bg_color <- if(nrow(tribe_info) > 0) tribe_info$color[1] else "#d9d9d9"
              
              style(
                display = "inline-block",
                padding = "2px 6px",
                "border-radius" = "8px",
                "background-color" = bg_color,
                color = ifelse(x %in% truly_eliminated, "gray", "white"),
                "text-decoration" = ifelse(x %in% truly_eliminated, "line-through", "none"),
                "font-weight" = "bold"
              )
            }
  )
}

# ---- Popular Picks & Tribe Plots ----
mvps <- picks %>% 
  group_by(MVP) %>%
  summarize(MVP_picks = n()) %>%
  ungroup() %>%
  filter(MVP_picks > 0)

popular_picks <- picks %>%
  pivot_longer(cols = starts_with("Pick"),
               values_to = "cast") %>%
  group_by(cast) %>%
  summarize(Other_picks = n()) %>%
  ungroup() %>%
  full_join(mvps, by = c("cast" = "MVP")) %>%
  left_join(tribes, by = c("cast")) %>%
  replace_na(list(MVP_picks = 0)) %>%
  mutate(cast = as_factor(cast), 
         Total = MVP_picks + Other_picks) %>%
  pivot_longer(cols = ends_with("picks"), 
               names_to = "type", 
               values_to = "Val") %>%
  arrange(desc(Total), cast) %>%
  mutate(cast = fct_reorder(cast, Val), 
         type = str_remove(type, "_picks"))

popular <- ggplot(data = popular_picks, aes(x = cast, y = Val, fill = type)) +
  geom_bar(stat="identity", position = "stack", alpha = .6) +
  scale_fill_manual(values = c("#0e3056", "#195432")) +
  coord_flip() +
  mytheme()

# Use dynamic tribe colors from tribes sheet
tribe_color_map <- setNames(tribes$color, tribes$tribe)

bytribe <- popular_picks %>%
  group_by(tribe, color) %>%
  summarize(Total = sum(Total), .groups = "drop") %>%
  ungroup() %>%
  arrange(desc(Total)) %>%
  ggplot(aes(x = reorder(tribe, Total), y = Total, fill = tribe)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = tribe_color_map) +
  theme(legend.position = "none") +
  coord_flip() +
  mytheme()

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Survivor Pool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Scoreboard", tabName = "Scoreboard", icon = icon("table")),
      menuItem("Visuals", tabName = "Visuals", icon = icon("chart-line")),
      menuItem("Pick Performance", tabName = "PickPerformance", icon = icon("star")),
      menuItem("Head-to-Head", tabName = "HeadToHead", icon = icon("users")),
      menuItem("Rules", tabName = "Rules", icon = icon("book"))
    )
  ),
  dashboardBody(
    # Custom Modern Survivor Theme CSS
    tags$head(
      tags$style(HTML("
        /* Import Google Fonts */
        @import url('https://fonts.googleapis.com/css2?family=Bebas+Neue&family=Roboto:wght@400;500;700&display=swap');
        
        /* Main body - modern charcoal with subtle texture */
        body, .content-wrapper, .right-side {
          background: linear-gradient(135deg, #2a2e3d 0%, #1e2230 50%, #252835 100%) !important;
          font-family: 'Roboto', sans-serif !important;
          color: #e8eef2 !important;
        }
        
        /* Sidebar styling - sleek dark */
        .skin-blue .main-sidebar {
          background: linear-gradient(180deg, #1a1d28 0%, #232734 100%) !important;
          border-right: 2px solid #4a90a4 !important;
        }
        
        .sidebar-menu > li > a {
          color: #d4dce3 !important;
          border-left: 3px solid transparent !important;
          transition: all 0.3s ease !important;
        }
        
        .sidebar-menu > li > a:hover,
        .sidebar-menu > li.active > a {
          background: rgba(74, 144, 164, 0.15) !important;
          border-left: 3px solid #4a90a4 !important;
          color: #6db8d4 !important;
        }
        
        /* Header styling - clean and modern */
        .skin-blue .main-header .navbar {
          background: linear-gradient(90deg, #1a1d28 0%, #232734 100%) !important;
          border-bottom: 3px solid #4a90a4 !important;
        }
        
        .skin-blue .main-header .logo {
          background: #141720 !important;
          color: #6db8d4 !important;
          font-family: 'Bebas Neue', cursive !important;
          font-size: 28px !important;
          letter-spacing: 3px !important;
          text-shadow: 2px 2px 8px rgba(74, 144, 164, 0.4) !important;
          border-right: 3px solid #4a90a4 !important;
        }
        
        /* Box styling - clean cards with subtle shadows */
        .box {
          background: linear-gradient(145deg, #323745 0%, #2a2e3d 100%) !important;
          border: 1px solid #4a5566 !important;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3) !important;
          border-radius: 10px !important;
        }
        
        .box-header {
          background: linear-gradient(90deg, #3a3f4f 0%, #2e3340 100%) !important;
          color: #6db8d4 !important;
          font-family: 'Bebas Neue', cursive !important;
          font-size: 22px !important;
          letter-spacing: 2px !important;
          border-bottom: 2px solid #4a5566 !important;
          border-radius: 10px 10px 0 0 !important;
        }
        
        .box-title {
          color: #6db8d4 !important;
        }
        
        /* H2 headers - bold modern style */
        h2, h3, h4 {
          font-family: 'Bebas Neue', cursive !important;
          color: #6db8d4 !important;
          text-shadow: 1px 1px 3px rgba(0, 0, 0, 0.5) !important;
          letter-spacing: 2px !important;
          border-bottom: 3px solid #4a90a4 !important;
          padding-bottom: 12px !important;
          margin-bottom: 20px !important;
        }
        
        /* Value boxes - modern with depth */
        .small-box {
          border-radius: 10px !important;
          border: 1px solid rgba(255, 255, 255, 0.1) !important;
          box-shadow: 0 6px 16px rgba(0, 0, 0, 0.4) !important;
        }
        
        .small-box.bg-yellow {
          background: linear-gradient(135deg, #d4af37 0%, #ffd700 100%) !important;
          border-color: #ffd700 !important;
        }
        
        .small-box.bg-light-blue {
          background: linear-gradient(135deg, #a8a9ad 0%, #c0c0c0 100%) !important;
          border-color: #c0c0c0 !important;
          color: #2a2e3d !important;
        }
        
        .small-box.bg-light-blue h3, .small-box.bg-light-blue p {
          color: #2a2e3d !important;
          text-shadow: 1px 1px 2px rgba(255, 255, 255, 0.3) !important;
        }
        
        .small-box.bg-orange {
          background: linear-gradient(135deg, #b87333 0%, #cd7f32 100%) !important;
          border-color: #cd7f32 !important;
        }
        
        .small-box.bg-aqua {
          background: linear-gradient(135deg, #48c9b0 0%, #76d7c4 100%) !important;
        }
        
        .small-box.bg-blue {
          background: linear-gradient(135deg, #3498db 0%, #5dade2 100%) !important;
        }
        
        .small-box.bg-green {
          background: linear-gradient(135deg, #52be80 0%, #7dcea0 100%) !important;
        }
        
        .small-box.bg-purple {
          background: linear-gradient(135deg, #8e44ad 0%, #a569bd 100%) !important;
        }
        
        .small-box h3, .small-box p {
          text-shadow: 1px 1px 3px rgba(0, 0, 0, 0.6) !important;
          font-weight: bold !important;
        }
        
        /* Slider styling - modern blue accent */
        .irs-bar {
          background: linear-gradient(90deg, #4a90a4 0%, #6db8d4 100%) !important;
          border: none !important;
        }
        
        .irs-slider {
          background: #6db8d4 !important;
          border: 2px solid #8ecee0 !important;
          box-shadow: 0 0 8px rgba(109, 184, 212, 0.6) !important;
        }
        
        .irs-from, .irs-to, .irs-single {
          background: #4a90a4 !important;
          color: white !important;
          font-weight: bold !important;
        }
        
        .irs-grid-text {
          color: #d4dce3 !important;
        }
        
        /* Form controls - modern inputs */
        .form-control, .selectize-input {
          background: #323745 !important;
          border: 2px solid #4a5566 !important;
          color: #e8eef2 !important;
          border-radius: 8px !important;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #6db8d4 !important;
          box-shadow: 0 0 8px rgba(109, 184, 212, 0.4) !important;
        }
        
        .selectize-dropdown {
          background: #323745 !important;
          border: 2px solid #4a5566 !important;
          color: #e8eef2 !important;
        }
        
        .selectize-dropdown-content .option:hover {
          background: rgba(74, 144, 164, 0.3) !important;
          color: #6db8d4 !important;
        }
        
        /* Checkbox styling */
        .checkbox label {
          color: #e8eef2 !important;
          font-weight: 500 !important;
        }
        
        /* Table styling - clean modern tables */
        table {
          background: #2a2e3d !important;
          border: 1px solid #4a5566 !important;
          border-collapse: separate !important;
          border-spacing: 0 !important;
          border-radius: 10px !important;
          overflow: hidden !important;
        }
        
        table thead {
          background: linear-gradient(90deg, #3a3f4f 0%, #2e3340 100%) !important;
          color: #6db8d4 !important;
          font-family: 'Bebas Neue', cursive !important;
          font-size: 18px !important;
          letter-spacing: 1.5px !important;
        }
        
        table thead th {
          border-bottom: 2px solid #4a90a4 !important;
          padding: 15px 10px !important;
          color: #6db8d4 !important;
        }
        
        table tbody tr {
          border-bottom: 1px solid rgba(74, 85, 102, 0.4) !important;
          transition: all 0.2s ease !important;
        }
        
        table tbody tr:hover {
          background: rgba(74, 144, 164, 0.12) !important;
        }
        
        table tbody td {
          padding: 12px 10px !important;
          color: #e8eef2 !important;
        }
        
        /* Subtle geometric pattern overlay */
        .content-wrapper::before {
          content: '' !important;
          position: fixed !important;
          top: 0 !important;
          left: 0 !important;
          width: 100% !important;
          height: 100% !important;
          background-image: 
            linear-gradient(30deg, transparent 48%, rgba(74, 144, 164, 0.02) 49%, rgba(74, 144, 164, 0.02) 51%, transparent 52%),
            linear-gradient(150deg, transparent 48%, rgba(74, 144, 164, 0.02) 49%, rgba(74, 144, 164, 0.02) 51%, transparent 52%) !important;
          background-size: 80px 80px !important;
          pointer-events: none !important;
          z-index: 1 !important;
        }
        
        .content {
          position: relative !important;
          z-index: 2 !important;
        }
        
        /* Plot backgrounds */
        .plotly, .svg-container {
          background: transparent !important;
        }
        
        /* Modern accent line on boxes (but not value boxes) */
        .box:not(.small-box)::before {
          content: '' !important;
          position: absolute !important;
          top: 0 !important;
          left: 0 !important;
          width: 4px !important;
          height: 100% !important;
          background: linear-gradient(180deg, #4a90a4 0%, #6db8d4 100%) !important;
          border-radius: 10px 0 0 10px !important;
        }
      "))
    ),
    tabItems(
      # Scoreboard Tab
      tabItem("Scoreboard",
              h2("Score Board by Week"),
              fluidRow(
                valueBoxOutput("leader"),
                valueBoxOutput("runnerup"),
                valueBoxOutput("thirdplace")
              ),
              sliderInput("week", "Week:", 1, currentweek, currentweek),
              formattableOutput("scoreboard")
      ),
      # Visuals Tab
      tabItem("Visuals",
              h2("Pool Visuals"),
              fluidRow(
                box(girafeOutput("ScoreTrajectory", height = "500px"), 
                    title = "Score Evolution Throughout Season", 
                    subtitle = "Hover over lines to highlight individual contestants",
                    width = 12)
              ),
              fluidRow(
                box(girafeOutput("RiskReward", height = "400px"), 
                    title = "Risk/Reward Matrix",
                    subtitle = "Value picks (top-left) vs Consensus busts (bottom-right)",
                    width = 12)
              )
      ),
      # Pick Performance Dashboard Tab
      tabItem("PickPerformance",
              h2("Pick Performance Dashboard"),
              p("Analyze which castaways are the best and worst picks across all teams."),
              fluidRow(
                box(
                  width = 12,
                  selectInput("filter_status", "Filter by Status:", 
                              choices = c("All Castaways", "Still Active", "Eliminated"),
                              selected = "All Castaways"),
                  selectInput("sort_by", "Sort by:", 
                              choices = c("Total Points Generated" = "total_points",
                                          "Average Points per Team" = "avg_points",
                                          "Times Picked" = "times_picked",
                                          "Weeks Survived" = "weeks_survived"),
                              selected = "total_points")
                )
              ),
              fluidRow(
                valueBoxOutput("best_pick"),
                valueBoxOutput("worst_pick"),
                valueBoxOutput("most_valuable_mvp")
              ),
              fluidRow(
                box(
                  title = "Pick Performance Table",
                  width = 12,
                  formattableOutput("pick_performance_table")
                )
              ),
              fluidRow(
                box(
                  title = "Points Contribution by Castaway",
                  width = 6,
                  plotOutput("pick_contribution_plot")
                ),
                box(
                  title = "MVP vs Regular Pick Performance",
                  width = 6,
                  plotOutput("mvp_comparison_plot")
                )
              )
      ),
      # Head-to-Head Tab
      tabItem("HeadToHead",
              h2("Head-to-Head Comparison"),
              fluidRow(
                box(
                  width = 12,
                  selectInput("contestant1", "Select Contestant 1:", 
                              choices = NULL, selected = NULL),
                  selectInput("contestant2", "Select Contestant 2:", 
                              choices = NULL, selected = NULL),
                  checkboxInput("add_third", "Add third contestant for comparison", FALSE),
                  conditionalPanel(
                    condition = "input.add_third == true",
                    selectInput("contestant3", "Select Contestant 3:", 
                                choices = NULL, selected = NULL)
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("h2h_score1"),
                valueBoxOutput("h2h_score2"),
                conditionalPanel(
                  condition = "input.add_third == true",
                  valueBoxOutput("h2h_score3")
                )
              ),
              fluidRow(
                box(
                  title = "Team Comparison",
                  width = 12,
                  formattableOutput("h2h_teams")
                )
              ),
              fluidRow(
                box(
                  title = "Score Progression",
                  width = 12,
                  girafeOutput("h2h_trajectory", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Pick Overlap Analysis",
                  width = 6,
                  htmlOutput("h2h_overlap")
                ),
                box(
                  title = "Performance Breakdown",
                  width = 6,
                  plotOutput("h2h_breakdown")
                )
              )
      ),
      # Rules Tab
      tabItem("Rules",
              h2("Scoring Rules"),
              box(
                width = 12,
                h3("How to Play"),
                p("Each contestant selects 5 castaways at the start of the season:"),
                tags$ul(
                  tags$li(strong("MVP:"), " Your most valuable pick - earns bonus points if they win"),
                  tags$li(strong("Picks 2-5:"), " Your supporting team")
                ),
                h3("Scoring System"),
                tags$ul(
                  tags$li(strong("Pre-Merge (Weeks 1-5):"), " 1 point per week for each castaway still in the game"),
                  tags$li(strong("Post-Merge (Week 6+):"), " 3 points per week for each castaway still in the game"),
                  tags$li(strong("Idol Bonus:"), " 2 points each time one of your picks finds a hidden immunity idol"),
                  tags$li(strong("Winner Bonus:"), " 30 points if any of your picks wins"),
                  tags$li(strong("MVP Winner Bonus:"), " Additional 30 points if your MVP wins (60 total)"),
                  tags$li(strong("Runner-Up Bonus:"), " 20 points if any of your picks finishes second"),
                  tags$li(strong("Third Place Bonus:"), " 10 points if any of your picks finishes third")
                ),
                h3("Strategy Tips"),
                p("Balance safe picks (likely to make it far) with high-upside picks (potential winners). 
                  Your MVP choice is critical - they need to not only survive but potentially win to maximize points.")
              )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  weekInput <- reactive({ 
    req(input$week)
    input$week 
  })
  
  # Central scoreboard data reactive with caching
  scoreboard_data <- reactive({
    req(exists("picks"))
    req(nrow(picks) > 0)
    
    w <- max(1, weekInput())
    
    # Calculate weekly scores
    l <- lapply(1:w, weekly_score, x = picks)
    
    # Combine weekly scores - handle single week case
    df <- if(length(l) == 1) {
      l[[1]]
    } else {
      reduce(l, left_join, by = c("Contestant", "MVP", "Pick2", "Pick3", "Pick4", "Pick5"))
    }
    
    # Recreate fullteam since it gets renamed by weekly_score
    df <- df %>%
      mutate(fullteam = as.vector(paste(MVP, Pick2, Pick3, Pick4, Pick5, sep=",")))
    
    # Calculate idol bonuses (inline to avoid rowwise conflicts)
    if(nrow(idols) == 0 || w < 1) {
      df <- df %>% mutate(idol_bonus = 0)
    } else {
      idols_found <- idols %>%
        filter(!is.na(week_found) & week_found <= w)
      
      if(nrow(idols_found) == 0) {
        df <- df %>% mutate(idol_bonus = 0)
      } else {
        df <- df %>%
          rowwise() %>%
          mutate(
            idol_bonus = {
              team_members <- c(MVP, Pick2, Pick3, Pick4, Pick5)
              sum(team_members %in% idols_found$cast) * 2
            }
          ) %>%
          ungroup()
      }
    }
    
    df <- df %>%
      rowwise() %>%
      mutate(
        # Safely sum epi_score columns (may not exist in all scenarios)
        EpiScore = {
          score_cols <- df %>% select(starts_with("epi_score")) %>% names()
          if(length(score_cols) > 0) {
            sum(c_across(starts_with("epi_score")), na.rm = TRUE)
          } else {
            0
          }
        },
        tot_remain = {
          # Only count castaways who are actually eliminated (have non-NA week values)
          truly_eliminated <- eliminated %>% 
            filter(!is.na(week)) %>% 
            pull(cast)
          
          if(length(truly_eliminated) > 0) {
            as.integer(5 - str_count(fullteam, pattern = paste(truly_eliminated, collapse = "|")))
          } else {
            5L  # No eliminations yet
          }
        }
      ) %>%
      ungroup() %>% 
      mutate(
        mvpbonus = ifelse(!is.na(winner) & MVP == winner, 30, 0),
        winneradd = if_else(!is.na(winner) & str_detect(fullteam, pattern = fixed(winner)), 30, 0),
        secondadd = if_else(!is.na(second) & str_detect(fullteam, pattern = fixed(second)), 20, 0),
        thirdadd = if_else(!is.na(third) & str_detect(fullteam, pattern = fixed(third)), 10, 0),
        top3bonus = winneradd + secondadd + thirdadd
      ) %>% 
      mutate(
        Score = as.integer(EpiScore + top3bonus + mvpbonus + idol_bonus),
        Status = case_when(
          tot_remain == 5 ~ "💪 Full Team",
          tot_remain >= 3 ~ "✅ Strong",
          tot_remain >= 2 ~ "⚠️ Vulnerable",
          tot_remain == 1 ~ "🚨 Critical",
          TRUE ~ "💀 Eliminated"
        )
      ) %>%
      rename(`Remaining Survivors` = tot_remain,
             `Running Score Total` = EpiScore,
             `MVP Bonus` = mvpbonus,
             `Top 3 Bonuses` = top3bonus,
             `Idol Bonus` = idol_bonus) %>%
      mutate(Place = dense_rank(desc(Score))) %>%
      arrange(Place, Contestant)
    
    df
  }) %>% bindCache(weekInput())
  
  # Scoreboard table
  output$scoreboard <- renderFormattable({
    df <- scoreboard_data()
    req(df)
    if (is.null(df) || nrow(df) == 0) return(formattable(data.frame()))
    
    # Pre-compute eliminated list and tribe colors as simple vectors
    truly_eliminated <- eliminated %>% 
      filter(!is.na(week)) %>% 
      pull(cast)
    
    # Create a named vector of tribe colors (castaway name -> color)
    tribe_colors_lookup <- setNames(tribes$color, tribes$cast)
    
    # Create badge formatter with pre-computed simple values only
    badge_fmt <- formatter("span",
      style = x ~ {
        # Look up color from pre-computed named vector (vectorized)
        bg_color <- ifelse(!is.na(x) & x %in% names(tribe_colors_lookup),
                          tribe_colors_lookup[x],
                          "#d9d9d9")
        
        style(
          display = "inline-block",
          padding = "2px 6px",
          "border-radius" = "8px",
          "background-color" = bg_color,
          color = ifelse(x %in% truly_eliminated, "gray", "white"),
          "text-decoration" = ifelse(x %in% truly_eliminated, "line-through", "none"),
          "font-weight" = "bold"
        )
      }
    )
    
    df %>%
      select(Place, Contestant, Score, Status, MVP, starts_with("Pick"), 
             `Running Score Total`, `MVP Bonus`, `Top 3 Bonuses`, `Idol Bonus`, `Remaining Survivors`) %>%
      formattable(list(
        MVP = badge_fmt,
        Pick2 = badge_fmt,
        Pick3 = badge_fmt,
        Pick4 = badge_fmt,
        Pick5 = badge_fmt,
        Place = formatter("span", style = x ~ style("font-weight" = "bold")),
        Status = formatter("span", style = x ~ style("font-weight" = "bold"))
      ))
  })
  
  # Leaderboard value boxes
  output$leader <- renderValueBox({
    tryCatch({
      df <- scoreboard_data()
      if (is.null(df) || nrow(df) == 0 || !any(df$Place == 1)) {
        valueBox("—", "No leader", icon = icon("crown"), color = "aqua")
      } else {
        row <- df %>% filter(Place == 1) %>% slice(1)
        valueBox(row$Contestant, paste0(row$Score, " points"),
                 icon = icon("crown"), color = "yellow")
      }
    }, error = function(e) {
      print(paste("Leader box error:", e$message))
      valueBox("Error", e$message, icon = icon("exclamation-triangle"), color = "red")
    })
  })
  
  output$runnerup <- renderValueBox({
    tryCatch({
      df <- scoreboard_data()
      if (is.null(df) || nrow(df) == 0 || !any(df$Place == 2)) {
        valueBox("—", "No runner-up", icon = icon("medal"), color = "aqua")
      } else {
        row <- df %>% filter(Place == 2) %>% slice(1)
        valueBox(row$Contestant, paste0(row$Score, " points"),
                 icon = icon("medal"), color = "light-blue")
      }
    }, error = function(e) {
      valueBox("Error", "Check data", icon = icon("exclamation-triangle"), color = "red")
    })
  })
  
  output$thirdplace <- renderValueBox({
    tryCatch({
      df <- scoreboard_data()
      if (is.null(df) || nrow(df) == 0 || !any(df$Place == 3)) {
        valueBox("—", "No 3rd", icon = icon("medal"), color = "aqua")
      } else {
        row <- df %>% filter(Place == 3) %>% slice(1)
        valueBox(row$Contestant, paste0(row$Score, " points"),
                 icon = icon("medal"), color = "orange")
      }
    }, error = function(e) {
      valueBox("Error", "Check data", icon = icon("exclamation-triangle"), color = "red")
    })
  })
  
  # Score Trajectory Plot - Interactive
  output$ScoreTrajectory <- renderGirafe({
    # Calculate scores for all weeks
    all_weeks <- lapply(1:currentweek, function(w) {
      l <- lapply(1:w, weekly_score, x = picks)
      reduce(l, left_join) %>%
        rowwise() %>%
        mutate(Score = sum(c_across(starts_with("epi_score"))),
               Week = w) %>%
        ungroup() %>%
        select(Contestant, Week, Score)
    })
    
    trajectory_data <- bind_rows(all_weeks)
    
    # Create a vibrant, distinguishable color palette
    n_contestants <- length(unique(trajectory_data$Contestant))
    
    # Use a combination of colorblind-friendly palettes
    if (n_contestants <= 8) {
      colors <- RColorBrewer::brewer.pal(max(3, n_contestants), "Dark2")
    } else if (n_contestants <= 12) {
      colors <- c(RColorBrewer::brewer.pal(8, "Dark2"),
                  RColorBrewer::brewer.pal(n_contestants - 8, "Set2"))
    } else {
      colors <- rainbow(n_contestants, s = 0.7, v = 0.8)
    }
    
    p <- ggplot(trajectory_data, 
                aes(x = Week, y = Score, color = Contestant, group = Contestant)) +
      geom_line_interactive(
        aes(tooltip = paste0(Contestant, "\nWeek ", Week, ": ", Score, " points"),
            data_id = Contestant),
        linewidth = 1.2, 
        alpha = 0.8
      ) +
      geom_point_interactive(
        aes(tooltip = paste0(Contestant, "\nWeek ", Week, ": ", Score, " points"),
            data_id = Contestant),
        size = 2.5
      ) +
      scale_x_continuous(breaks = 1:currentweek) +
      scale_color_manual(values = colors) +
      labs(y = "Cumulative Score") +
      mytheme() +
      theme(legend.position = "right",
            legend.text = element_text(size = 9))
    
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:3;stroke-opacity:1;"),
        opts_hover_inv(css = "opacity:0.2;"),
        opts_sizing(rescale = TRUE),
        opts_toolbar(saveaspng = FALSE)
      ),
      width_svg = 10,
      height_svg = 6
    )
  })
  
  # By tribe plot
  output$ByTribe <- renderPlot({ bytribe })
  
  # ---- Head-to-Head Comparison Logic ----
  output$RiskReward <- renderGirafe({
    # Calculate popularity (how many times picked)
    pick_popularity <- picks %>%
      pivot_longer(cols = c(MVP, starts_with("Pick")),
                   values_to = "cast",
                   names_to = "pick_type") %>%
      group_by(cast) %>%
      summarize(times_picked = n(), .groups = "drop")
    
    # Calculate performance (weeks survived)
    pick_performance <- if(nrow(eliminated) > 0) {
      # For eliminated castaways, use their elimination week
      elim_weeks <- eliminated %>%
        group_by(cast) %>%
        summarize(weeks_survived = max(week), .groups = "drop")
      
      # For still-active castaways, use current week
      all_castaways <- data.frame(cast = castaways)
      all_castaways %>%
        left_join(elim_weeks, by = "cast") %>%
        mutate(weeks_survived = ifelse(is.na(weeks_survived), 
                                       currentweek, 
                                       weeks_survived - 1)) # Subtract 1 since eliminated ON that week
    } else {
      data.frame(cast = castaways, weeks_survived = 0)
    }
    
    # Combine and add tribe info
    risk_reward_data <- pick_popularity %>%
      left_join(pick_performance, by = "cast") %>%
      left_join(tribes, by = "cast") %>%
      mutate(
        still_alive = !(cast %in% eliminated$cast),
        performance_category = case_when(
          weeks_survived >= currentweek * 0.8 ~ "Elite Survivor",
          weeks_survived >= currentweek * 0.5 ~ "Mid-game Exit",
          TRUE ~ "Early Boot"
        ),
        value_rating = case_when(
          times_picked <= 2 & weeks_survived >= currentweek * 0.6 ~ "Hidden Gem",
          times_picked >= 5 & weeks_survived <= currentweek * 0.3 ~ "Consensus Bust",
          times_picked >= 5 & weeks_survived >= currentweek * 0.6 ~ "Validated Pick",
          TRUE ~ "Standard"
        )
      )
    
    # Create plot
    p <- ggplot(risk_reward_data, 
                aes(x = times_picked, y = weeks_survived, color = tribe)) +
      geom_point_interactive(
        aes(tooltip = paste0(cast, "\n",
                             "Picked by ", times_picked, " team(s)\n",
                             "Survived ", weeks_survived, " week(s)\n",
                             "Tribe: ", tribe),
            data_id = cast),
        size = 4,
        alpha = 0.8
      ) +
      geom_text_interactive(
        aes(label = cast,
            tooltip = paste0(cast, "\n",
                             "Picked by ", times_picked, " team(s)\n",
                             "Survived ", weeks_survived, " week(s)\n",
                             "Tribe: ", tribe),
            data_id = cast),
        size = 3,
        hjust = -0.1,
        vjust = -0.5,
        show.legend = FALSE
      ) +
      scale_color_manual(values = tribe_color_map, name = "Tribe") +
      labs(x = "Times Picked (Popularity)", 
           y = "Weeks Survived (Performance)") +
      mytheme() +
      theme(legend.position = "bottom")
    
    # Add quadrant lines at medians
    if(nrow(risk_reward_data) > 0) {
      median_pop <- median(risk_reward_data$times_picked, na.rm = TRUE)
      median_perf <- median(risk_reward_data$weeks_survived, na.rm = TRUE)
      
      p <- p + 
        geom_vline(xintercept = median_pop, linetype = "dashed", 
                   alpha = 0.3, color = "gray50") +
        geom_hline(yintercept = median_perf, linetype = "dashed", 
                   alpha = 0.3, color = "gray50") +
        annotate("text", x = max(risk_reward_data$times_picked) * 0.05, 
                 y = max(risk_reward_data$weeks_survived) * 0.95,
                 label = "Value Picks", fontface = "bold", 
                 color = "darkgreen", size = 3.5, alpha = 0.6) +
        annotate("text", x = max(risk_reward_data$times_picked) * 0.95, 
                 y = max(risk_reward_data$weeks_survived) * 0.05,
                 label = "Consensus Busts", fontface = "bold", 
                 color = "darkred", size = 3.5, alpha = 0.6)
    }
    
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:2;stroke:black;cursor:pointer;"),
        opts_hover_inv(css = "opacity:0.3;"),
        opts_sizing(rescale = TRUE),
        opts_toolbar(saveaspng = FALSE)
      ),
      width_svg = 8,
      height_svg = 6
    )
  })
  
  # ---- Pick Performance Dashboard Logic ----
  
  # Calculate pick performance data
  pick_performance_data <- reactive({
    # Get all picks from all teams
    all_picks <- picks %>%
      pivot_longer(cols = c(MVP, starts_with("Pick")),
                   values_to = "cast",
                   names_to = "pick_type") %>%
      mutate(is_mvp = (pick_type == "MVP"))
    
    # Calculate weeks survived for each castaway
    weeks_data <- if(nrow(eliminated) > 0) {
      elim_weeks <- eliminated %>%
        group_by(cast) %>%
        summarize(weeks_survived = max(week) - 1, .groups = "drop")
      
      data.frame(cast = castaways) %>%
        left_join(elim_weeks, by = "cast") %>%
        mutate(weeks_survived = ifelse(is.na(weeks_survived), currentweek, weeks_survived),
               still_active = !(cast %in% eliminated$cast))
    } else {
      data.frame(cast = castaways, weeks_survived = 0, still_active = TRUE)
    }
    
    # Calculate points per pick
    pick_points <- all_picks %>%
      left_join(weeks_data, by = "cast") %>%
      left_join(tribes, by = "cast") %>%
      mutate(
        # Pre-merge points (weeks 1-5)
        pre_merge_points = pmin(weeks_survived, mergeweek - 1),
        # Post-merge points (week 6+)
        post_merge_weeks = pmax(0, weeks_survived - (mergeweek - 1)),
        post_merge_points = post_merge_weeks * 3,
        # Total weekly points
        weekly_points = pre_merge_points + post_merge_points,
        # Bonus points
        winner_bonus = ifelse(!is.na(winner) & cast == winner, 30, 0),
        second_bonus = ifelse(!is.na(second) & cast == second, 20, 0),
        third_bonus = ifelse(!is.na(third) & cast == third, 10, 0),
        mvp_winner_bonus = ifelse(!is.na(winner) & cast == winner & is_mvp, 30, 0),
        total_bonuses = winner_bonus + second_bonus + third_bonus + mvp_winner_bonus,
        # Total points this pick generated
        points_generated = weekly_points + total_bonuses
      )
    
    # Aggregate by castaway
    pick_summary <- pick_points %>%
      group_by(cast, tribe, color, still_active, weeks_survived) %>%
      summarize(
        times_picked = n(),
        times_mvp = sum(is_mvp),
        times_regular = sum(!is_mvp),
        total_points = sum(points_generated),
        avg_points = mean(points_generated),
        total_weekly = sum(weekly_points),
        total_bonuses = sum(total_bonuses),
        .groups = "drop"
      ) %>%
      arrange(desc(total_points))
    
    pick_summary
  })
  
  # Filter pick performance based on status
  filtered_pick_data <- reactive({
    data <- pick_performance_data()
    
    if(input$filter_status == "Still Active") {
      data <- data %>% filter(still_active == TRUE)
    } else if(input$filter_status == "Eliminated") {
      data <- data %>% filter(still_active == FALSE)
    }
    
    # Sort by selected metric
    data <- data %>% arrange(desc(!!sym(input$sort_by)))
    
    data
  })
  
  # Value boxes
  output$best_pick <- renderValueBox({
    data <- pick_performance_data()
    if(nrow(data) == 0) {
      valueBox("—", "Best Pick", icon = icon("trophy"), color = "green")
    } else {
      best <- data %>% slice(1)
      valueBox(
        best$cast,
        paste0(best$total_points, " total pts (", best$times_picked, " teams)"),
        icon = icon("trophy"),
        color = "green"
      )
    }
  })
  
  output$worst_pick <- renderValueBox({
    data <- pick_performance_data() %>% filter(times_picked > 0)
    if(nrow(data) == 0) {
      valueBox("—", "Worst Pick", icon = icon("thumbs-down"), color = "red")
    } else {
      worst <- data %>% arrange(avg_points) %>% slice(1)
      valueBox(
        worst$cast,
        paste0(round(worst$avg_points, 1), " avg pts (", worst$times_picked, " teams)"),
        icon = icon("thumbs-down"),
        color = "red"
      )
    }
  })
  
  output$most_valuable_mvp <- renderValueBox({
    data <- pick_performance_data() %>% filter(times_mvp > 0)
    if(nrow(data) == 0) {
      valueBox("—", "Best MVP Pick", icon = icon("crown"), color = "yellow")
    } else {
      best_mvp <- data %>% 
        mutate(mvp_points = total_points / times_picked * times_mvp / times_picked) %>%
        arrange(desc(mvp_points)) %>% 
        slice(1)
      valueBox(
        best_mvp$cast,
        paste0("Picked as MVP ", best_mvp$times_mvp, "x"),
        icon = icon("crown"),
        color = "yellow"
      )
    }
  })
  
  # Pick performance table
  output$pick_performance_table <- renderFormattable({
    data <- filtered_pick_data()
    if(nrow(data) == 0) return(formattable(data.frame()))
    
    data %>%
      mutate(Status = ifelse(still_active, "✅ Active", "❌ Out")) %>%
      select(cast, tribe, Status, times_picked, times_mvp, weeks_survived, 
             total_points, avg_points, total_weekly, total_bonuses) %>%
      rename(
        Castaway = cast,
        Tribe = tribe,
        `Times Picked` = times_picked,
        `As MVP` = times_mvp,
        `Weeks Survived` = weeks_survived,
        `Total Points` = total_points,
        `Avg Points` = avg_points,
        `Weekly Points` = total_weekly,
        `Bonus Points` = total_bonuses
      ) %>%
      formattable(list(
        Castaway = formatter("span", style = x ~ style("font-weight" = "bold")),
        `Total Points` = color_bar("#6db8d4"),
        `Avg Points` = color_bar("#4a90a4"),
        Status = formatter("span", style = x ~ style("font-weight" = "bold"))
      ))
  })
  
  # Points contribution plot
  output$pick_contribution_plot <- renderPlot({
    data <- filtered_pick_data() %>%
      head(15) %>%  # Top 15 only
      mutate(cast = fct_reorder(cast, total_points))
    
    if(nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = cast, y = total_points, fill = tribe)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = tribe_color_map) +
      coord_flip() +
      labs(y = "Total Points Generated", x = "") +
      mytheme() +
      theme(legend.position = "bottom")
  })
  
  # MVP vs Regular pick comparison
  output$mvp_comparison_plot <- renderPlot({
    data <- pick_performance_data() %>%
      filter(times_picked > 0) %>%
      mutate(
        mvp_avg = ifelse(times_mvp > 0, total_points / times_picked, NA),
        regular_avg = ifelse(times_regular > 0, 
                             (total_weekly / times_picked) + 
                               (ifelse(!is.na(winner) & cast == winner, 30, 0) +
                                  ifelse(!is.na(second) & cast == second, 20, 0) +
                                  ifelse(!is.na(third) & cast == third, 10, 0)), 
                             NA)
      ) %>%
      filter(!is.na(mvp_avg) | !is.na(regular_avg)) %>%
      pivot_longer(cols = c(mvp_avg, regular_avg),
                   names_to = "pick_type",
                   values_to = "avg_points") %>%
      filter(!is.na(avg_points)) %>%
      mutate(pick_type = ifelse(pick_type == "mvp_avg", "As MVP", "As Regular Pick"))
    
    if(nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = reorder(cast, avg_points), y = avg_points, fill = pick_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("As MVP" = "#ffd700", "As Regular Pick" = "#4a90a4")) +
      coord_flip() +
      labs(y = "Average Points per Team", x = "") +
      mytheme() +
      theme(legend.position = "bottom",
            legend.title = element_blank())
  })
  
  # ---- Head-to-Head Comparison Logic ----
  
  # Update contestant choices
  observe({
    contestants <- picks$Contestant
    updateSelectInput(session, "contestant1", choices = contestants, 
                      selected = contestants[1])
    updateSelectInput(session, "contestant2", choices = contestants, 
                      selected = if(length(contestants) > 1) contestants[2] else contestants[1])
    updateSelectInput(session, "contestant3", choices = contestants, 
                      selected = if(length(contestants) > 2) contestants[3] else contestants[1])
  })
  
  # Get selected contestants' data
  h2h_data <- reactive({
    req(input$contestant1, input$contestant2)
    
    contestants_to_compare <- c(input$contestant1, input$contestant2)
    if(input$add_third && !is.null(input$contestant3)) {
      contestants_to_compare <- c(contestants_to_compare, input$contestant3)
    }
    
    scoreboard_data() %>%
      filter(Contestant %in% contestants_to_compare)
  })
  
  # Value boxes for scores
  output$h2h_score1 <- renderValueBox({
    req(input$contestant1)
    data <- h2h_data()
    row <- data %>% filter(Contestant == input$contestant1)
    
    if(nrow(row) == 0) {
      valueBox("—", input$contestant1, color = "blue")
    } else {
      valueBox(
        paste0(row$Score, " pts"),
        paste0(input$contestant1, " (", row$Place, 
               switch(as.character(row$Place),
                      "1" = "st",
                      "2" = "nd", 
                      "3" = "rd",
                      "th"), ")"),
        icon = icon("user"),
        color = "blue"
      )
    }
  })
  
  output$h2h_score2 <- renderValueBox({
    req(input$contestant2)
    data <- h2h_data()
    row <- data %>% filter(Contestant == input$contestant2)
    
    if(nrow(row) == 0) {
      valueBox("—", input$contestant2, color = "green")
    } else {
      valueBox(
        paste0(row$Score, " pts"),
        paste0(input$contestant2, " (", row$Place,
               switch(as.character(row$Place),
                      "1" = "st",
                      "2" = "nd",
                      "3" = "rd",
                      "th"), ")"),
        icon = icon("user"),
        color = "green"
      )
    }
  })
  
  output$h2h_score3 <- renderValueBox({
    req(input$contestant3)
    data <- h2h_data()
    row <- data %>% filter(Contestant == input$contestant3)
    
    if(nrow(row) == 0) {
      valueBox("—", input$contestant3, color = "purple")
    } else {
      valueBox(
        paste0(row$Score, " pts"),
        paste0(input$contestant3, " (", row$Place,
               switch(as.character(row$Place),
                      "1" = "st",
                      "2" = "nd",
                      "3" = "rd",
                      "th"), ")"),
        icon = icon("user"),
        color = "purple"
      )
    }
  })
  
  # Team comparison table
  output$h2h_teams <- renderFormattable({
    data <- h2h_data()
    if(nrow(data) == 0) return(formattable(data.frame()))
    
    data %>%
      select(Contestant, MVP, Pick2, Pick3, Pick4, Pick5, 
             `Remaining Survivors`, `Idol Bonus`, Score, Status) %>%
      formattable(list(
        MVP = tribe_badge_formatter("MVP"),
        Pick2 = tribe_badge_formatter("Pick2"),
        Pick3 = tribe_badge_formatter("Pick3"),
        Pick4 = tribe_badge_formatter("Pick4"),
        Pick5 = tribe_badge_formatter("Pick5"),
        Score = formatter("span", style = x ~ style("font-weight" = "bold")),
        Status = formatter("span", style = x ~ style("font-weight" = "bold"))
      ))
  })
  
  # Head-to-head trajectory
  output$h2h_trajectory <- renderGirafe({
    req(input$contestant1, input$contestant2)
    
    contestants_to_compare <- c(input$contestant1, input$contestant2)
    if(input$add_third && !is.null(input$contestant3)) {
      contestants_to_compare <- c(contestants_to_compare, input$contestant3)
    }
    
    # Calculate weekly scores for selected contestants
    all_weeks <- lapply(1:currentweek, function(w) {
      l <- lapply(1:w, weekly_score, x = picks)
      reduce(l, left_join) %>%
        rowwise() %>%
        mutate(Score = sum(c_across(starts_with("epi_score"))),
               Week = w) %>%
        ungroup() %>%
        select(Contestant, Week, Score)
    })
    
    trajectory_data <- bind_rows(all_weeks) %>%
      filter(Contestant %in% contestants_to_compare)
    
    # Assign colors
    color_map <- c("blue", "green", "purple")
    names(color_map) <- contestants_to_compare[1:length(contestants_to_compare)]
    
    p <- ggplot(trajectory_data, 
                aes(x = Week, y = Score, color = Contestant, group = Contestant)) +
      geom_line_interactive(
        aes(tooltip = paste0(Contestant, "\nWeek ", Week, ": ", Score, " points"),
            data_id = Contestant),
        linewidth = 2
      ) +
      geom_point_interactive(
        aes(tooltip = paste0(Contestant, "\nWeek ", Week, ": ", Score, " points"),
            data_id = Contestant),
        size = 3
      ) +
      scale_x_continuous(breaks = 1:currentweek) +
      scale_color_manual(values = color_map) +
      labs(y = "Cumulative Score") +
      mytheme() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 11, face = "bold"))
    
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:4;"),
        opts_sizing(rescale = TRUE),
        opts_toolbar(saveaspng = FALSE)
      ),
      width_svg = 10,
      height_svg = 5
    )
  })
  
  # Pick overlap analysis
  output$h2h_overlap <- renderUI({
    data <- h2h_data()
    if(nrow(data) == 0) return(HTML("<p>No data available</p>"))
    
    # Get all picks for each contestant
    get_picks <- function(contestant_name) {
      row <- data %>% filter(Contestant == contestant_name)
      if(nrow(row) == 0) return(character(0))
      c(row$MVP, row$Pick2, row$Pick3, row$Pick4, row$Pick5)
    }
    
    picks1 <- get_picks(input$contestant1)
    picks2 <- get_picks(input$contestant2)
    
    overlap <- intersect(picks1, picks2)
    unique1 <- setdiff(picks1, picks2)
    unique2 <- setdiff(picks2, picks1)
    
    html_content <- paste0(
      "<h4>Shared Picks: ", length(overlap), "/5</h4>",
      if(length(overlap) > 0) {
        paste0("<p><strong>Both picked:</strong> ", paste(overlap, collapse = ", "), "</p>")
      } else {
        "<p><strong>No shared picks!</strong> Complete differentiation.</p>"
      },
      "<hr>",
      "<p><strong>", input$contestant1, " only:</strong> ", 
      if(length(unique1) > 0) paste(unique1, collapse = ", ") else "None", "</p>",
      "<p><strong>", input$contestant2, " only:</strong> ", 
      if(length(unique2) > 0) paste(unique2, collapse = ", ") else "None", "</p>"
    )
    
    if(input$add_third && !is.null(input$contestant3)) {
      picks3 <- get_picks(input$contestant3)
      overlap_all <- Reduce(intersect, list(picks1, picks2, picks3))
      
      html_content <- paste0(
        html_content,
        "<hr>",
        "<h4>All Three Share: ", length(overlap_all), "/5</h4>",
        if(length(overlap_all) > 0) {
          paste0("<p>", paste(overlap_all, collapse = ", "), "</p>")
        } else {
          "<p>No picks shared by all three contestants.</p>"
        }
      )
    }
    
    HTML(html_content)
  })
  
  # Performance breakdown
  output$h2h_breakdown <- renderPlot({
    data <- h2h_data()
    if(nrow(data) == 0) return(NULL)
    
    # Create breakdown data
    breakdown_data <- data %>%
      select(Contestant, `Running Score Total`, `Idol Bonus`, `MVP Bonus`, `Top 3 Bonuses`) %>%
      pivot_longer(cols = -Contestant, 
                   names_to = "Category", 
                   values_to = "Points") %>%
      mutate(Category = factor(Category, 
                               levels = c("Running Score Total", "Idol Bonus", "MVP Bonus", "Top 3 Bonuses")))
    
    ggplot(breakdown_data, aes(x = Contestant, y = Points, fill = Category)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Running Score Total" = "#2c7fb8",
                                   "Idol Bonus" = "#7a5195",
                                   "MVP Bonus" = "#41ab5d",
                                   "Top 3 Bonuses" = "#feb24c")) +
      labs(y = "Points", x = "") +
      mytheme() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"))
  })
}

# ---- Run App ----
shinyApp(ui, server)