#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(p8105.datasets)
library(plotly)
library(tidyverse)
library(viridis)
library(flexdashboard)

data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(rating = review_scores_location / 2) %>%
  select(boro = neighbourhood_group, neighbourhood, rating, price, room_type,
         latitude, longitude) %>%
  filter(!is.na(rating))
boros = nyc_airbnb %>% distinct(boro) %>% pull()

risk_vectors <- c("drug_level","comlications","exercise_level")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("K-M analysis"),
   
   selectInput("risk_factor", label = h3("Risk Factors"),
               choices = risk_vectors, selected = "Manhattan")
   
   
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

