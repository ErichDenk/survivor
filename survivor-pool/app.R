# Shiny App for scoring and displaying our pool
# 
#    http://shiny.rstudio.com/
#

gc()
rm(list = ls())

##########################################
# Packages, Themes, and Functions -------
##########################################
library(shiny)
library(shinydashboard) # For dashboard functions
library(bslib) # Easy prepackaged themes
library(fontawesome)
library(formattable) # Formatting of table
library(tidyverse)
library(tidyselect) # For helper functions
library(markdown)
library(forcats) # Easily deal with factors
library(googlesheets4)
library(assertr)

# Google Sheet Info
gs4_deauth()
sheet_id <- "1MM3r3p9PxthirnMfZvWfBg3LsNm7Uf1X99BVM0fdvH0"

# GGPlot uniform theme
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

# Globals:
# Merge Cutoff and Winners (These can be radio buttons to adjust tables)
mergeweek <- 6
winner <- "none"
second <- "none"
third <- "none"

# Vector for Cast
castaways <- c("Austin", "Dee", "Brandon", "Emily", 
               "Brando", "Hannah", "Bruce", "Janani", 
               "Drew", "Julie", "Jake", "Katurah", 
               "Kaleb", "Kellie", "Sifu", "Kendra",
               "Sean", "Sabiyah")

# Scoring Function
weekly_score <- function(x,y) {
  # Only the previously eliminated weeks considered
  nowelim <- eliminated %>%
    filter(week <= y) %>%
    pull(cast)
  
  # How many picks remain for the week?
  new <- x %>% 
    mutate(epi_remain = 5 - str_count(fullteam, 
                                      pattern = paste(nowelim, collapse = "|"))) 
  
  if(y < mergeweek) {
    new$epi_score = new$epi_remain
  }
  else(new$epi_score = new$epi_remain*3)
  
  new %>%
    rename_with(~paste0(.,y), epi_remain:epi_score) 
  
}

# Formatting Function for multi-column formatting
elimformatter <-  
  formatter("span", style = x ~ style(color = ifelse(x %in% eliminated$cast, "red", "steelblue"),
                                      "text-decoration" = ifelse(x %in% eliminated$cast, "line-through",NA)))

###################
# Data Input ------
###################

# Our Selections
picks <- read_sheet(sheet_id, sheet = "Picks") %>%
  mutate(fullteam  = as.vector(paste(MVP, Pick2, Pick3, Pick4, Pick5, sep=","))) %>%
  verify(MVP %in% castaways & Pick2 %in% castaways & Pick3 %in% castaways &
           Pick4 %in% castaways & Pick5 %in% castaways)

# The Tribe Spoke: Table of eliminated choices
eliminated <- read_sheet(sheet_id, sheet = "Eliminated") %>%
  rename_with(tolower)

# Some More globals
currentweek <- max(eliminated$week)
vect <- 1:currentweek

# Create Main Scoring Table
picks <- picks %>%
  mutate(tot_remain = 5 - str_count(fullteam, 
                                    pattern = paste(eliminated$cast,collapse = "|")))
mvps <- picks %>% 
  group_by(MVP) %>%
  summarize(MVP_picks = n()) %>%
  ungroup() %>%
  filter(MVP_picks > 0)

# Popular Picks
popular_picks <- picks %>%
  pivot_longer(cols = starts_with("Pick"),
               values_to = "cast") %>%
  group_by(cast) %>%
  summarize(Other_picks = n()) %>%
  ungroup() %>%
  full_join(.,mvps, by = c("cast" = "MVP")) %>%
  replace_na(list(MVP_picks = 0)) %>%
  mutate(cast = as_factor(cast), 
         Total_picks = MVP_picks + Other_picks) %>%
  pivot_longer(cols = ends_with("picks"), 
               names_to = "type", 
               values_to = "Val") %>%
  arrange(desc(Val)) %>%
  mutate(cast = fct_reorder(cast, Val), 
         type = str_remove(type, "_picks")) %>%
  filter(type != "Total")

#####################
# PLOTS ------------
####################
popular <- ggplot(data = popular_picks, aes(x = cast, y = Val, fill = type)) +
  geom_bar(stat="identity", position = "stack", alpha = .6) +
  scale_y_continuous(breaks = 1:20) +
  scale_fill_manual(values = c("#358bbd", "#00ab50")) +
  coord_flip() +
  mytheme()

#######################
# The Shiny Apps -----
######################

# Define UI for application through built elements
header <- dashboardHeader(title = "Survivor Fantasy Pool",
                          titleWidth = 350)

sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("Welcome", tabName = "Welcome"),
    menuItem("Scoreboard", tabName = "Scoreboard"),
    menuItem("Rules", tabName = "Rules"),
    menuItem("Stats", tabName = "Stats")
    )
  )

body <- dashboardBody(
  theme = bs_theme(bootswatch = "darkly"),
  
  # Tabbed Items
  tabItems(
    tabItem("Welcome",
      p("Welcome to the new and improved site for keeping track of your Survivor pool
      team. Until I get this site fully up and running I'll still keep track in 
      the",
      span(a("Google Doc.", href = "https://docs.google.com/spreadsheets/d/1MM3r3p9PxthirnMfZvWfBg3LsNm7Uf1X99BVM0fdvH0/edit?usp=sharing"), 
           style ="color:#00ab50"), style = "font-family: 'times'; font-si20pt"),
      span("Please let me know of any suggestions you have or errors you see. I'll be adding features both in terms
      new content and design features",
      style = "font-family: 'times'; font-si20pt"),
      p("Remember:"),
      div(img(src="jeff.gif", align = "center", height='200px',width='200px'),
          style="text-align: center;")
      ),
    
    tabItem("Scoreboard",
        h2("Score Board by Week"),
        sliderInput("week", "Week:", 1, currentweek, currentweek),
        formattableOutput("scoreboard")
      ),
    
    tabItem("Rules",
      h2("A reminder of the scoring system:"),
      p("- 1 point per castaway for each week they survive prior to the merge"),
      p("- 3 points per castaway for each week they survive post-merge"),
      p("- 10 bonus points if any of your picks comes in 3rd place"),
      p("- 20 bonus points if any of your picks comes in 2nd place"),
      p("- 30 bonus points if any of your picks is Sole Survivor"),
      p("- 30, em(additional bonus) points if your MVP is Sole Survivor")
      ),
    
    tabItem("Stats",
      h2("The Most Popular Picks"),
      plotOutput("Popular")
      )
    )
  )

ui <- dashboardPage(header, sidebar, body)
  
# Define server logic
server <- function(input, output, session) {
    
    weekInput <- reactive({
        input$week
    })
    
    # Main Scoreboard
    output$scoreboard <- renderFormattable({
    lapply(1:weekInput(), weekly_score, x = picks) %>%
    reduce(left_join) %>%
    rowwise() %>%
    mutate(Score = sum(c_across(starts_with("epi_score"))),
           tot_remain = as.integer(tot_remain)) %>%
    ungroup() %>% # No longer rowwise
    mutate(mvpbonus = ifelse(MVP == winner, 30, 0),
           winneradd = if_else(str_detect(fullteam, 
                                            pattern = winner),30,0),
           secondadd = if_else(str_detect(fullteam, 
                                   pattern = second),20,0),
           thirdadd = if_else(str_detect(fullteam, 
                                           pattern = third),10,0),
           top3bonus = winneradd + secondadd + thirdadd) %>% 
    mutate(Score = as.integer(Score + top3bonus + mvpbonus)) %>%
    rename(`Remaining Survivors` = tot_remain,
           `MVP Bonus` = mvpbonus,
           `Top 3 Bonuses` = top3bonus) %>%
    mutate(Place = dense_rank(desc(Score))) %>%
    arrange(Place, Contestant) %>% 
    select(Place, Contestant, Score, MVP,
           starts_with("Pick"), ends_with("bonus"), `Remaining Survivors`) %>%
    formattable(list(MVP = elimformatter,
                     Pick2 = elimformatter,
                     Pick3 = elimformatter,
                     Pick4 = elimformatter,
                     Pick5 = elimformatter,
                     Name = formatter("span", style = x ~ style(font.style = "italic")),
                     Place = formatter("span", style = x ~ style(font.style = "bold"))))
      
    })
    
    # Popular Picks Chart
    output$Popular <- renderPlot(popular)
}

# Run the application 
shinyApp(ui = ui, server = server)
