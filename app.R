#### Libraries ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(scales)

#### Initialize Data ----
data <- readRDS(file = "./data/subregion_agg.rds")

metric_choices <- colnames(data)[4:ncol(data)]
metric_names <- gsub("_", " ", metric_choices)
metric_names <- paste0(toupper(substr(metric_names,1,1)), substr(metric_names, 2, nchar(metric_names)))

metric_list <- as.list(metric_choices)
names(metric_list) <- metric_names


ui <- dashboardPage(
  skin = "red",
  
  #### Header ----
  dashboardHeader(
    title = "COVID-19 South Africa",
    titleWidth = 350
  ),
  
  #### Sidebar ----
  dashboardSidebar(
    width = 350,
    br(),
    h4("Select Your Inputs Here", style = "padding-left:20px"),
    
    #### Metric Input ----
    selectInput(
      inputId = "metric",
      label = strong("Select Metric", style = "font-family: 'arial'; font-size: 12px"),
      choices = metric_list,
      selected = metric_list[1]
    ),
    
    #### Province Input ----
    selectInput(
      inputId = "province",
      multiple = TRUE,
      label = strong("Select Province to Compare", style = "font-family: 'arial'; font-size: 14px"),
      choices = sort(unique(clean_data$subregion1_name))
    ),
    
    #### Date Range ----
    dateRangeInput(
      inputId = "date_range",
      label = "Select Date Range",
      start = "2020-01-01",
      end = "2020-12-01"
    )
    
  ),
  
  #### Body ----
  dashboardBody(
    tabsetPanel(
      type = "tabs",
      id = "tab_selected",
      tabPanel(
        title = "Country View",
        plotOutput("plot_data_country")
      )
    )
  )
)

server <- function(input, output, session) {
  
  #### Clean Data ----
  clean_data_province <- reactive({ clean_data <- data %>%
    filter(subregion1_name %in% input$province & date >= input$date_range[1] & date <= input$date_range[2] ) %>%
    select(date, subregion1_name, input$metric) %>%
    set_names(c("date", "Province", "metric")) %>%
    arrange(date)
  })
  
  #### Plot Data ----
  output$plot_data_country <- renderPlot({
    ggplot(data = clean_data_province(), aes(y = metric, x = date, color = Province)) + 
      geom_line() + 
      ylab(metric_names[which(metric_choices == input$metric)]) +
      xlab("Date") + 
      labs(color="Province") +
      scale_y_continuous(label = comma) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      ggtitle(metric_names[which(metric_choices == input$metric)])
  })
}

shinyApp(ui, server)