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
library(survivalAnalysis)




library(survminer)
library(survival)
library(flexdashboard)
library(dplyr)


load('./df_combine.RData')

df_combine<-df_combine %>% 
  mutate(district = as.character(district),
         district = fct_collapse(district, Huangpu = '310101', 
                                 Xuhui = '310104', Changning = c('310105', '310106'), 
                                 Putuo = '310107', Zhabei = '310108', 
                                 Hongkou = '310109', Yangpu = '310110', 
                                 Minhang = '310112', Baoshan = '310113',  
                                 Pudong = c('310115', '310119'), Jiading = '310114', 
                                 Jinshan = '310116', Songjiang = '310117', 
                                 Qingpu = '310118', Fengxian = '310120', 
                                 Chongming = '310230'))

risk_vectors <- c("By Gender","By Drug Level",
                  "By Complications Level","By Exercise Level")
district_vectors <- c("Huangpu", "Xuhui", "Changning","Putuo","Zhabei","Hongkou",
                      "Yangpu","Minhang","Baoshan","Pudong","Jiading","Jinshan","Songjiang",
                      "Qingpu","Fengxian","Chongming")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("K-M analysis"),
   sidebarPanel(
     selectInput("risk_factor", label = h3("Risk Factors"),
                 choices = risk_vectors, selected = "Manhattan"),
     
     selectInput("district_factor", label = h3("District"),
                 choices = district_vectors, selected = "Huangpu"),
 
     sliderInput("age_range", label = h3("Choose age range"), min = 1, 
                 max = 100, value = c(10, 100))
   ),
   mainPanel(
     plotOutput('plot1')
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$plot1 <- renderPlot(
    
    if(input$risk_factor=="By Gender"){
      df_combine %>%
        filter(district == input$district_factor,
               dmage %in% input$age_range[1]:input$age_range[2]) %>% 
        mutate(tb = fct_recode(tb, '1'= 'Yes', '0'='No'),
               tb=as.character(tb),
               tb=as.numeric(tb)) %>% 
        analyse_survival(vars(days, tb), by=gender) %>% 
        kaplan_meier_plot(ylim=c(0.975,1),break.time.by = 300)
    }else if(input$risk_factor=="By Drug Level"){
      df_combine %>%
        filter(district == input$district_factor,
               dmage %in% input$age_range[1]:input$age_range[2]) %>% 
        mutate(tb = fct_recode(tb, '1'= 'Yes', '0'='No'),
               tb=as.character(tb),
               tb=as.numeric(tb)) %>% 
        analyse_survival(vars(days, tb), by=drug) %>% 
        kaplan_meier_plot(ylim=c(0.975,1),break.time.by = 300)
    }else if(input$risk_factor=="By Complications Level"){
      df_combine %>%
        filter(district == input$district_factor,
               dmage %in% input$age_range[1]:input$age_range[2]) %>% 
        mutate(tb = fct_recode(tb, '1'= 'Yes', '0'='No'),
               tb=as.character(tb),
               tb=as.numeric(tb)) %>% 
        analyse_survival(vars(days, tb), by=complications) %>% 
        kaplan_meier_plot(ylim=c(0.975,1),break.time.by = 300)
    }else if(input$risk_factor=="By Exercise Level"){
      df_combine %>%
        filter(district == input$district_factor,
               dmage %in% input$age_range[1]:input$age_range[2]) %>%  
        mutate(tb = fct_recode(tb, '1'= 'Yes', '0'='No'),
               tb=as.character(tb),
               tb=as.numeric(tb)) %>% 
        analyse_survival(vars(days, tb), by=exercise) %>% 
        kaplan_meier_plot(ylim=c(0.975,1),break.time.by = 300)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

