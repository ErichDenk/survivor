# Shiny App for scoring and displaying our pool
# 
#
#    http://shiny.rstudio.com/
#

# Packages -------
library(shiny)
library(tidyverse)
library(markdown)
library(googlesheets4)


# Vector for Cast
castaways <- c("Jenny","Lindsay", "Drea","Johnathan", "Chanelle",
               "Hai","Maryanne","Mike","Omar","Swati","Tori", "Daniel",
               "Rocksroy", "Romeo","Lydia","Marya S","Jackson","Zach")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Survivor Fantasy Tribes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
