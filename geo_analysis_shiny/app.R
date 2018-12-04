
library(flexdashboard)
library(shiny)
library(plotly)
library(tidyverse)
library(rgdal)
library(maptools)
library(tidyverse)
library(rgeos)
library(ggthemes)

load('./data/df_combine.Rdata')
sh <- readOGR('./data/shanghai_shapefile/shang_dis_merged.shp',verbose = F)
##Translating and Adding two missing two districts
sh@data <- sh@data %>% 
  mutate(Name = as.factor(Name)) %>%
  mutate(Name = fct_recode(Name, Jiading = '嘉定区',
                           Fengxian = '奉贤区',
                           Baoshan = '宝山区',
                           Chongming = '崇明县',
                           Xuhui = '徐汇区',
                           Putuo ='普陀区',
                           Yangpu = '杨浦区',
                           Songjiang = '松江区',
                           Pudong='浦东新区',
                           Hongkou = '虹口区',
                           Jinshan = '金山区',
                           Changning = '长宁区',
                           Minhang = '闵行区',
                           Zhabei = '闸北区',
                           Qingpu = '青浦区', 
                           Huangpu = '黄浦区'))
sh_df <- broom::tidy(sh, region='Name')
sh_df <- sh_df %>%
  mutate(urban = as.factor(id))%>% 
  mutate(urban = fct_collapse(urban, 
                              Urban = c('Changning','Hongkou','Huangpu','Putuo','Xuhui','Yangpu','Zhabei'),
                              Suburban =  c('Baoshan','Jiading','Jinshan','Minhang','Chongming','Qingpu','Songjiang','Fengxian'),
                              Pudong__New_District = 'Pudong'))
load('./data/df_combine.Rdata')
x <- df_combine %>% 
  filter(district != "") %>% 
  mutate(tb = fct_recode(tb, '1'= 'Yes', '0'='No'),
         tb=as.character(tb),
         tb=as.numeric(tb),
         district = as.character(district),
         district = fct_collapse(district, Huangpu = '310101', Xuhui = '310104', Changning = c('310105', '310106'), Putuo = '310107', Zhabei = '310108', Hongkou = '310109', Yangpu = '310110', Minhang = '310112', Baoshan = '310113',  Pudong = c('310115', '310119'), Jiading = '310114', Jinshan = '310116', Songjiang = '310117', Qingpu = '310118', Fengxian = '310120', Chongming = '310230'),
         exercise = as.numeric(as.character(exercise)),
         complications = as.numeric(as.character(complications)))%>% 
  group_by(district) %>% 
  summarise(tb_sum = sum(tb),
            incidence = tb_sum/n(),
            exercise_rate = mean(exercise),
            complications = mean(complications),
            bmi_change = mean(bmi_change,na.rm = T),
            glucose_ave = mean(glu_average),
            glucose_freq = mean(celiangxtgl),
            drug = mean(drug)) %>% 
  rename(id=district)

sh_df <- sh_df %>% 
  inner_join(x,by='id') %>% 
  gather(tb_sum :drug, key=parameter, value=value)

choice=  sh_df %>% distinct(parameter) %>% pull()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Geo-Analysis"),
  sidebarPanel(
    selectInput("Parameter", label = h3("Parameters"),
                choices = choice, selected = "tb_sum")
  ),
  mainPanel(
    plotOutput('plot1')
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlot(
    sh_df%>%
    filter(parameter == input$Parameter) %>%
    ggplot()+
    geom_polygon(aes(x = long, y = lat, group = group,fill=value,alpha = .5), 
                 colour = "black"))
     
         
  
    }
  



# Run the application 
shinyApp(ui = ui, server = server)
