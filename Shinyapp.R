#loading Libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tm)
library(SnowballC)  # stemming
library(wordcloud2)
library(colourpicker)
library('ggplot2')
library('dplyr')
library('data.table')
library('tibble')
library('tidyr')
library('stringr')
library('lubridate')
library('forecast')


# Load data
air_visits <- as.tibble(fread('air_visit_data.csv'))
# Data Cleaning
air_visits <- air_visits %>%
  mutate(visit_date = ymd(visit_date))


air_visits %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday )) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Median visitors")




#Defining UI
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Past Visitor time series plot",
             sidebarLayout(
               sidebarPanel(selectInput("restaurant","Select a Restaurant", choices = unique(air_visits$air_store_id)),
                            dateRangeInput("dtrange","Choose the Date Range",
                                           start = ymd('2016-12-01'),
                                           end = max(air_visits$visit_date),
                                           min = min(air_visits$visit_date), 
                                           max = max(air_visits$visit_date))),
               mainPanel(h1("Time Series Visualistion of past Visitors to restaurants"),
                         plotOutput("tsplot"), #width = "100%", height = "650px"),
                         plotOutput("tsplot2"),#, width = "100%", height = "650px"),
                         plotOutput("tsplot3")#, width = "100%", height = "650px")
               ))),
    tabPanel("Future Visitor forecast",
             sidebarLayout(
               sidebarPanel(selectInput("restaurant2","Select a Restaurant", choices = unique(air_visits$air_store_id)),
                            sliderInput("no_days","Choose the number of days to forecast", min = 1, max = 100, value = 7)),
               mainPanel(h1("Future Visitors forecast"),plotOutput("futrplot", width = "100%", height = "650px"))))
  )
)

server <- function(input, output) {
  output$tsplot = renderPlot({
    data = air_visits %>% filter(air_store_id==input$restaurant) %>% 
      group_by(visit_date) %>%
      summarise(all_visitors = sum(visitors))
    data = data[(data$visit_date>=input$dtrange[1]),]
    data = data[(data$visit_date<=input$dtrange[2]),]
    ggplot(data, aes(visit_date,all_visitors)) +
      geom_line(col = 'blue') +
      labs(y = "Number of visitors", x = "Date")
  })
  x = reactive({
    air_visits %>% filter(air_store_id==input$restaurant2) %>% 
      group_by(visit_date) %>%
      summarise(all_visitors = sum(visitors))
  })
  arima.fit = reactive({
    auto.arima(tsclean(ts(x()$all_visitors, frequency = 7)),
               stepwise = FALSE, approximation = FALSE)
  })
  
  
  
  output$tsplot2 = renderPlot({
    data3 = air_visits %>% filter(air_store_id==input$restaurant) %>% 
      group_by(visit_date) %>%
      summarise(all_visitors = sum(visitors))
    data3 = data3[(data3$visit_date>=input$dtrange[1]),]
    data3 = data3[(data3$visit_date<=input$dtrange[2]),]
    data3 %>%
      mutate(wday = wday(visit_date, label = TRUE)) %>%
      group_by(wday) %>%
      summarise(visits = median(all_visitors)) %>%
      ggplot(aes(wday, visits, fill = wday )) +
      geom_col() +
      theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
      labs(x = "Day of the week", y = "Median visitors")
  })
  
  
  output$tsplot3 = renderPlot({
    data4 = air_visits %>% filter(air_store_id==input$restaurant) %>% 
      group_by(visit_date) %>%
      summarise(all_visitors = sum(visitors))
    data4 = data4[(data4$visit_date>=input$dtrange[1]),]
    data4 = data4[(data4$visit_date<=input$dtrange[2]),]
    data4 %>%
      mutate(month = month(visit_date, label = TRUE)) %>%
      group_by(month) %>%
      summarise(visits = median(all_visitors)) %>%
      ggplot(aes(month, visits, fill = month)) +
      geom_col() +
      theme(legend.position = "none") +
      labs(x = "Month", y = "Median visitors")
  })
  
  
  x = reactive({
    air_visits %>% filter(air_store_id==input$restaurant2) %>% 
      group_by(visit_date) %>%
      summarise(all_visitors = sum(visitors))
  })
  arima.fit = reactive({
    auto.arima(tsclean(ts(x()$all_visitors, frequency = 7)),
               stepwise = FALSE, approximation = FALSE)
  })
  
  
  
  
  
  output$futrplot = renderPlot({
    arima_visits <- arima.fit() %>% forecast(h = input$no_days, level = c(80,95))
    arima_visits %>% autoplot +
      labs(x = "Time [weeks]", y = "visitors")
  })  
  
  
}


shinyApp(server = server, ui = ui)
