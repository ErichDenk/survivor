# Shiny App for scoring and displaying our pool
# 
#
#    http://shiny.rstudio.com/
#

rm(list = ls())

# Packages, Themes, and Functions -------
library(shiny)
library(tidyverse)
library(tidyselect)
library(markdown)
library(forcats)
library(shinythemes)

mytheme <- function(){theme(axis.text = element_text(size = 10),
                            plot.title = element_text(face = "bold"),
                            panel.background = element_blank(),
                            panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            panel.grid.minor.y = element_blank(),
                            panel.grid.major.y = element_line(linetype = "dotted",
                                                              size = 0.5, color = "gray"),
                            axis.line.x = element_line(linetype = "solid"))}

# Merge Cutoff and current week (These can be radio buttons to adjust tables)
mergeweek <- 6
currentweek <- 8
vect <- 1:currentweek
winner <- "none"
second <- "none"
third <- "none"

# Vector for Cast
castaways <- c("Jenny","Lindsay", "Drea","Johnathan", "Chanelle",
               "Hai","Maryanne","Mike","Omar","Swati","Tori", "Daniel",
               "Rocksroy", "Romeo","Lydia","Marya S","Jackson","Zach")

# Our Selections
picks <- read_csv("picks.csv") %>%
    mutate(fullteam  = as.vector(paste(MVP, Pick2, Pick3, Pick4, Pick5, sep=",")))

# The Tribe Spoke: Table of eliminated choices
eliminated <- tribble(~cast, ~week,
                      "Marya S", 1,
                      "Jenny", 2,
                      "Swati", 3,
                      "Daniel", 4,
                      "Lydia", 5, 
                      "Chanelle", 6, # Jury Begins
                      "Rocksroy", 7,
                      "Tori", 7,
                      "Hai", 8) 

# Create Main Scoring Table ----
picks <- picks %>%
    mutate(tot_remain = 5 - str_count(fullteam, 
                                      pattern = paste(eliminated$cast,collapse = "|")))
# Popular Picks
popular_picks <- as_tibble(unname(unlist(sapply(picks$fullteam, 
                                                function(z) str_split(z, ","))))) %>%
    group_by(value) %>%
    rename(name = value) %>%
    summarise(picks = n()) %>% 
    arrange(picks) %>%
    mutate(name = as_factor(name))

# Scoring
weekly_score <- function(x,y) {
    # Only the previously eliminated weeks considered
    nowelim <- eliminated %>%
        filter(week <= y) %>%
        select(cast) %>%
        .$cast
    
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

#####################
# PLOTS ------------
####################
popular <- ggplot(data = popular_picks, aes(fct_inorder(name),picks)) +
    geom_col(fill = "#358bbd") +
    scale_y_continuous(breaks = 1:15) +
    coord_flip() +
    ggthemes::theme_tufte() +
    mytheme() +
    theme(axis.title.y = element_blank())+
    labs(title = "Most Popular Picks, Season 42",
         y = "Count")

#######################
# The Shiny Apps -----
######################

# Define UI for application interactive table
ui <- fluidPage(
    
    theme = shinytheme("darkly"),

    # Application title
    titlePanel("Survivor Fantasy Tribes"),
    br(),
    p("Welcome to the new and improved site for keeping track of your Survivor pool
      team. Until I get this site fully up and running I'll still keep track in 
      the",
      span(a("Google Doc.", href = "https://docs.google.com/spreadsheets/d/1ithAwr2YSLlWXf-hnNOYcTNgHYXdVFg1pXDV97YZsTI/edit?usp=sharing"), 
           style ="color:blue"), style = "font-family: 'times'; font-si16pt"),
    p("Please let me know of any suggestions you have or errors you see. I'll be adding features both in terms
      new content and design features",
      style = "font-family: 'times'; font-si16pt"),
    p("Remember:"),
    div(img(src="jeff.gif", align = "center", height='250px',width='250px'),
        style="text-align: center;"),


    # Sidebar with a slider input for week
    sidebarLayout(
        sidebarPanel(
            numericInput("week",
                        "Standings after week:",
                        currentweek,
                        min = 1,
                        max = currentweek)
        ),
        mainPanel(
            h2("Score Board by Week (Current Week Default)"),
            div(tableOutput("scoreboard"), style = "font-size:80%"),
            br(),
            h3("A reminder of the scoring system:"),
            p("- 1 point per castaway for each week they survive prior to the merge"),
            p("- 3 points per castaway for each week they survive post-merge"),
            p("- 10 bonus points if any of your picks comes in 3rd place"),
            p("- 20 bonus points if any of your picks comes in 2nd place"),
            p("- 30 bonus points if any of your picks is Sole Survivor"),
            p("- 30, em(additional bonus) points if your MVP is Sole Survivor"),

            br(),
            
            h2("The Most Popular Picks"),
            plotOutput("Popular")
            )
        )
    )

# Define server logic
server <- function(input, output, session) {
    
    weekInput <- reactive({
        input$week
    })
    
    # Main Scoreboard
    output$scoreboard <- renderTable({
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
    arrange(Place) %>% 
    select(Place, Name, Score, MVP,
           starts_with("Pick"), ends_with("bonus"), `Remaining Survivors`)
    })
    
    # Popular Picks Chart
    output$Popular <- renderPlot(popular)
}

# Run the application 
shinyApp(ui = ui, server = server)
