#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(flexdashboard)
library(shiny)
library(p8105.datasets)
library(plotly)
library(tidyverse)
library(viridis)
library(survminer)
library(survival)
library(flexdashboard)
library(dplyr)


load('./df_combine.RData')


df_combine$survival = with(df_combine, Surv(days, tb == "Yes"))

df_combine <- df_combine %>% 
  head(10)

risk_vectors <- c("drug","gender")
district_vectors <- c("A","B","C")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("K-M analysis"),
   sidebarPanel(
     selectInput("risk_factor", label = h3("Risk Factors"),
                 choices = risk_vectors, selected = "Manhattan"),
     
     selectInput("district_vector", label = h3("Select District"),
                 choices = district_vectors, selected = "A")
   ),
   mainPanel(
     plotOutput('plot1')
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlot(
    if(input$risk_factor=="gender"){
      km =  survfit(survival ~ gender, data = df_combine, conf.type = "log-log")
      ggsurvplot(km, 
                 data = df_combine, 
                 risk.table = F, 
                 pval = T, 
                 ylab = paste0("o","1"), 
                 ylim = c(0.9, 1.0))
    }else{
      km =  survfit(survival ~ drug, data = df_combine, conf.type = "log-log")
      ggsurvplot(km, 
                 data = df_combine, 
                 risk.table = F, 
                 pval = T, 
                 ylab = paste0("o","1"), 
                 ylim = c(0.9, 1.0))
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

