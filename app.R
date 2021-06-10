#### Libraries ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(scales)
library(tidyquant)
library(forecast)

#### Initialize Data ----
data <- readRDS(file = "./data/subregion_agg.rds")

metric_choices <- colnames(data)[4:ncol(data)]
metric_names <- gsub("_", " ", metric_choices)
metric_names <- paste0(toupper(substr(metric_names,1,1)), substr(metric_names, 2, nchar(metric_names)))
metric_list <- as.list(metric_choices)
names(metric_list) <- metric_names

ui <- dashboardPage(
  skin = "green",
  
  #### Header ----
  dashboardHeader(
    title = "COVID-19 South Africa",
    titleWidth = 350
  ),
  
  #### Sidebar ----
  dashboardSidebar(
    
    shinyjs::useShinyjs(),
    
    width = 350,
    br(),
    h4("Select Your Inputs Here", style = "padding-left:20px"),
    uiOutput("sidebar")
  ),
  
  #### Body ----
  dashboardBody(
    tabsetPanel(
      type = "tabs",
      id = "tab_selected",
      tabPanel(
        title = "Country View",
        plotOutput("country_plot")
      )
    )
  )
)

server <- function(input, output) {
  
  make_forecast <- reactiveValues(value=0)
  
  #### _____________ ----
  #### Clean Data ----
  clean_data_province <- reactive({ clean_data <- data %>%
    filter(subregion1_name %in% input$province & date >= input$date_range[1] & date <= input$date_range[2]) %>%
    group_by(subregion1_name, date) %>%
    select(date, subregion1_name, input$metric) %>%
    set_names(c("date", "Province", "metric")) %>%
    arrange(date)
  })
  
  #### _____________ ----
  #### Forecast Data ----
  forecast_data <- reactive({
    unforecasted_data <- clean_data_province()
    unforecasted_data$forecast <- 0
    forecasted_data <- predictions_by_country()
    forecasted_data <- do.call(rbind, forecasted_data)
    rbind(unforecasted_data, forecasted_data)
  })
  
  #### _____________ ----
  #### Prediction Data ----
  predictions_by_country <- reactive({
    clean_data_province() %>%
      group_by(Province) %>%
      group_map(~ create_forecast(.x, num_forecasts=forecast_days()), .keep=T )
  })
  
  #### Plot Data ----
  plot_data_country <- function(data){
    ma_days <- ifelse(input$moving_average == T, ma_days(), 0)
    ggplot(data = clean_data_province(), aes(y = metric, x = date, color = Province)) + 
      geom_line() + 
      geom_ma(n = ma_days, size = 1) +
      ylab(metric_names[which(metric_choices == input$metric)]) +
      xlab("Date") + 
      labs(color="Province") +
      scale_y_continuous(label = comma) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      ggtitle(metric_names[which(metric_choices == input$metric)])
  }
  
  #### Forecast Data ----
  plot_data_country_forecast <- function(data){
    ma_days <- ifelse( input$moving_average == T , ma_days(), 0 )
    ggplot( data = forecast_data() %>% filter(forecast==0), aes(y = metric, x = date, color = Province) ) +
      geom_line(size = 1.5) +
      geom_ma(n=ma_days,size=1) +
      geom_line(data = forecast_data() %>% filter(forecast==1),size = 2.5,linetype=7,alpha=0.25) +
      ylab( metric_names[which(metric_choices == input$metric)] ) +
      xlab("Date") +
      scale_x_date( date_breaks = "1 month", date_labels =  "%b %Y" ) +
      theme_bw() +
      geom_vline(xintercept= forecast_data() %>% filter(forecast==0) %>% pull(date) %>% max, linetype="dotdash",size=0.5)
  }
  
  #### Render Plots ----
  output$country_plot <- renderPlot({
    req( input$province ) 
    ifelse( make_forecast$value == 0, return(plot_data_country( clean_data_province() ))
            , return(plot_data_country_forecast( clean_data_province() )) )
  })
  
  #### _____________ ----
  #### Moving Average Button ----
  ma_days <- eventReactive(input$moving_average_bttn,{
    req(input$moving_average_days)
    input$moving_average_days
  },ignoreNULL = FALSE)
  
  #### Moving Average Toggle ----
  observeEvent(input$moving_average,{
    if (input$moving_average == TRUE) {
      shinyjs::show(id = "moving_average_days", anim = TRUE, animType = "slide")
    }
    else{
      shinyjs::hide(id = "moving_average_days", anim = TRUE, animType = "fade")
    }
  })
  
  observeEvent(input$forecast_bttn, {
    make_forecast$value <- 1
  })
  
  observeEvent(input$remove_forecast_bttn, {
    make_forecast$value <- 0
  })
  
  forecast_days <- eventReactive(input$forecast_bttn,{
    input$forecast
  })
  
  #### Time Difference ----
  time_diff <- reactive({
    req(input$date_range_regional)
    ( as.Date(input$date_range_regional[2]) - as.Date(input$date_range_regional[1]) )[[1]]
  })
  
  #### _____________ ----
  #### Metric Input ----
  output$metric <- renderUI({
    selectInput(
        inputId = "metric",
        label = strong("Select Metric", style = "font-family: 'arial'; font-size: 12px"),
        choices = metric_list,
        selected = metric_list[1]
      )
    })
  
  #### Province Input ----
  output$province <- renderUI({
    selectInput(
      inputId = "province",
      multiple = FALSE,
      label = strong("Select Province to Compare", style = "font-family: 'arial'; font-size: 14px"),
      choices = unique(data$subregion1_name)
    )
  })
  
  #### Date Range ----
  output$date_range <- renderUI({
    dateRangeInput(
      inputId = "date_range",
      label = "Select Date Range",
      start = "2020-01-01",
      end = "2020-12-01"
    )
  })
  
  #### Moving Average ----
  output$moving_average <- renderUI({
    checkboxInput(
      inputId = "moving_average",
      label = div("Include Moving Average", style = "font-size: 12pt"),
      value = FALSE
    )
  })
  
  #### Moving Average Days ----
  output$moving_average_days <- renderUI({
    div(
      numericInput(
        inputId = "moving_average_days",
        label = "Number of Days for Moving Average",
        value = 5,
        min = 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_average_bttn",
        label = "Update Moving Average",
        class = "btn-success"
      )
    )
  })
  
  #### _____________ ----
  #### Forecasting ----
  
  output$forecast <- renderUI({
    numericInput(
      inputId = "forecast",
      label = "Number of Days to Forecast",
      value = 20, 
      min = 0, 
      max = 100, 
      step = 1
    )
  })
  
  #### Forecast bttn ----
  output$forecast_bttn <- renderUI({
    actionButton(inputId = "forecast_bttn",
                 icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color: white;", 
                 label = "Make a Forecast!",
                 class = "btn btn-lg btn-primary"
    )
  })
  
  output$remove_forecast_bttn <- renderUI({
    actionButton(inputId = "remove_forecast_bttn",
                 style = "color: white;", 
                 label = "Remove",
                 class = "btn btn-lg btn-danger"
    )
  })
  
  
  
  #### _____________ ----
  #### UI Side Output ----
  output$sidebar <- renderUI({
    div(
      uiOutput("metric"),
      uiOutput("province"),
      uiOutput("date_range"),
      uiOutput("moving_average"),
      uiOutput("moving_average_days") %>% hidden(),
      uiOutput("forecast"),
      uiOutput("forecast_bttn"), 
      uiOutput("remove_forecast_bttn")
    )
  })
}

shinyApp(ui, server)