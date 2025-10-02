# Shiny App for scoring and displaying our pool
# http://shiny.rstudio.com/

##########################################
# Packages, Themes, and Functions -------
##########################################
library(shiny)
library(shinydashboard) # For dashboard functions
library(bslib) # Easy prepackaged themes (optional)
library(fontawesome)
library(formattable) # Formatting of table
library(tidyverse)
library(tidyselect) # For helper functions
library(markdown)
library(forcats) # Easily deal with factors
library(googlesheets4)
library(assertr)

# ---- Google Sheet Info ----
# Replace with your sheet id (you already had one)
sheet_id <- "1WFD5SAMUbJO40HbMj3TtjDCWgVbdxw0ftDgmVbhHIb4"

# ---- Read Sheets (must exist in your Google Sheet) ----
# Expect sheets named: "Tribes", "Picks", "Eliminated"
tribes <- read_sheet(sheet_id, sheet = "Tribes") %>% rename_with(tolower)
picks   <- read_sheet(sheet_id, sheet = "Picks")
eliminated <- read_sheet(sheet_id, sheet = "Eliminated") %>% rename_with(tolower)

# ---- Defensive checks (small) ----
# Ensure tribes has cast and color columns
if (!all(c("cast","color","tribe") %in% names(tribes))) {
  stop("Tribes sheet must contain columns named: cast, tribe, color")
}

# ---- GGPlot uniform theme (kept from your original) ----
mytheme <- function(){
  ggthemes::theme_tufte() +
    theme(axis.text = element_text(size = 10),
          plot.title = element_text(face = "bold"),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = "dotted",
                                            size = 0.5, color = "gray"), 
          axis.line.x = element_line(linetype = "solid"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank()
    )
}

# ---- Globals ----
mergeweek <- 6
# currentweek derived from eliminated (safer than hardcoding)
currentweek <- ifelse(nrow(eliminated) > 0, max(eliminated$week, na.rm = TRUE), 1)

# placeholder winners (you can set these in your Eliminated sheet and derive later)
winner <- "winner"
second <- "second"
third <- "third"

# Vector for Cast (from tribes)
castaways <- tribes %>% pull(cast)

# Tribe colors vector for badges (named vector)
tribe_colors <- setNames(tribes$color, tribes$cast)

# ---- Scoring Function (uses eliminated dataframe) ----
weekly_score <- function(x, y) {
  # Only the previously eliminated weeks considered
  nowelim <- eliminated %>%
    filter(week <= y) %>%
    pull(cast)
  
  # How many picks remain for the week?
  new <- x %>% 
    mutate(fullteam = as.vector(paste(MVP, Pick2, Pick3, Pick4, Pick5, sep = ",")),
           epi_remain = 5 - str_count(fullteam, pattern = paste(nowelim, collapse = "|"))) 
  
  if (y < mergeweek) {
    new$epi_score <- new$epi_remain
  } else {
    new$epi_score <- new$epi_remain * 3
  }
  
  new %>%
    rename_with(~paste0(., y), epi_remain:epi_score)
}

# ---- Formatting helper for eliminated names (used if you still want it) ----
elimformatter <- formatter(
  "span",
  style = x ~ style(
    color = ifelse(x %in% eliminated$cast, "gray", "black"),
    "text-decoration" = ifelse(x %in% eliminated$cast, "line-through", "none")
  )
)

# ---- Helper Functions ----
place_color <- function(place) {
  case_when(
    place == 1 ~ "yellow",      # gold
    place == 2 ~ "light-blue",  # silver
    place == 3 ~ "orange",      # bronze
    TRUE ~ "aqua"
  )
}

# Robust tribe badge formatter: handles unknown names and NA
tribe_badge <- function(color_vec) {
  formatter("span",
            style = x ~ style(
              display = "inline-block",
              padding = "2px 6px",
              "border-radius" = "8px",
              "background-color" = ifelse(is.na(x) | !(x %in% names(color_vec)),
                                          "#d9d9d9", color_vec[x]),
              color = ifelse(is.na(x) | !(x %in% names(color_vec)), "black", "white"),
              "font-weight" = "bold"
            )
  )
}

# ---- (Optional) Popular / ByTribe plotting objects ----
# If you already have code to create 'popular' and 'bytribe' ggplots, paste it here.
# For now, create simple placeholders so app runs:
popular <- ggplot() + geom_blank() + ggtitle("Popular picks (placeholder)")
bytribe <- ggplot() + geom_blank() + ggtitle("Picks by tribe (placeholder)")

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Survivor Pool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Scoreboard", tabName = "Scoreboard", icon = icon("table")),
      menuItem("Plots", tabName = "Plots", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
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
      # Plots Tab
      tabItem("Plots",
              h2("Pool Visuals"),
              fluidRow(
                box(plotOutput("Popular"), width = 6),
                box(plotOutput("ByTribe"), width = 6)
              )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  weekInput <- reactive({ input$week })
  
  # Central scoreboard data reactive
  scoreboard_data <- reactive({
    # defensive: ensure picks exists and weekInput >= 1
    req(exists("picks"))
    w <- max(1, weekInput())
    
    l <- lapply(1:w, weekly_score, x = picks)
    df <- reduce(l, left_join)
    
    df <- df %>%
      rowwise() %>%
      mutate(Score = sum(c_across(starts_with("epi_score"))),
             tot_remain = as.integer(5 - str_count(as.vector(paste(MVP, Pick2, Pick3, Pick4, Pick5, sep=",")),
                                                   pattern = paste(eliminated$cast, collapse = "|")))) %>%
      ungroup() %>% 
      mutate(mvpbonus = ifelse(MVP == winner, 30, 0),
             winneradd = if_else(str_detect(fullteam, pattern = winner), 30, 0),
             secondadd = if_else(str_detect(fullteam, pattern = second), 20, 0),
             thirdadd = if_else(str_detect(fullteam, pattern = third), 10, 0),
             top3bonus = winneradd + secondadd + thirdadd) %>% 
      mutate(Score = as.integer(Score + top3bonus + mvpbonus)) %>%
      rename(`Remaining Survivors` = tot_remain,
             `MVP Bonus` = mvpbonus,
             `Top 3 Bonuses` = top3bonus) %>%
      mutate(Place = dense_rank(desc(Score))) %>%
      arrange(Place, Contestant)
    
    # join tribe info for MVP column (if you want badges for other Picks, join differently)
    df <- left_join(df, tribes, by = c("MVP" = "cast"))
    df
  })
  
  # Scoreboard table
  output$scoreboard <- renderFormattable({
    df <- scoreboard_data()
    # if df empty, show nothing
    if (nrow(df) == 0) return(formattable(data.frame()))
    
    df %>%
      select(Place, Contestant, Score, MVP, starts_with("Pick"), ends_with("bonus"), `Remaining Survivors`) %>%
      formattable(list(
        MVP   = tribe_badge(tribe_colors),
        Pick2 = tribe_badge(tribe_colors),
        Pick3 = tribe_badge(tribe_colors),
        Pick4 = tribe_badge(tribe_colors),
        Pick5 = tribe_badge(tribe_colors),
        Place = formatter("span", style = x ~ style("font-weight" = "bold"))
      ))
  })
  
  # Leaderboard value boxes (safe: handle missing rows)
  output$leader <- renderValueBox({
    df <- scoreboard_data()
    if (nrow(df) == 0 || !any(df$Place == 1)) {
      valueBox("—", "No leader", icon = icon("crown"), color = "aqua")
    } else {
      row <- df %>% filter(Place == 1) %>% slice(1)
      valueBox(row$Contestant, paste0(row$Score, " points"),
               icon = icon("crown"), color = place_color(1))
    }
  })
  
  output$runnerup <- renderValueBox({
    df <- scoreboard_data()
    if (nrow(df) == 0 || !any(df$Place == 2)) {
      valueBox("—", "No runner-up", icon = icon("medal"), color = "aqua")
    } else {
      row <- df %>% filter(Place == 2) %>% slice(1)
      valueBox(row$Contestant, paste0(row$Score, " points"),
               icon = icon("medal"), color = place_color(2))
    }
  })
  
  output$thirdplace <- renderValueBox({
    df <- scoreboard_data()
    if (nrow(df) == 0 || !any(df$Place == 3)) {
      valueBox("—", "No 3rd", icon = icon("medal"), color = "aqua")
    } else {
      row <- df %>% filter(Place == 3) %>% slice(1)
      valueBox(row$Contestant, paste0(row$Score, " points"),
               icon = icon("medal"), color = place_color(3))
    }
  })
  
  # Plots (use your ggplots here)
  output$Popular <- renderPlot({ popular + mytheme() })
  output$ByTribe <- renderPlot({ bytribe + mytheme() })
}

# ---- Run App ----
shinyApp(ui, server)