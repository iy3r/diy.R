setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../lib/download_tri.R", chdir=T)
library(timetk)
library(PerformanceAnalytics)
library(shiny)

rollingReturns <- function(index){
  data <- download_tri(index) 
  
  df <- data %>% 
    tk_xts(select = Value, date_var = Date)
  
  returns <- Return.calculate(df[endpoints(df, on="months", k=1)])
  returns <- returns[-1, ]
  
  charts.RollingPerformance(R = returns, width = 120)
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "index", 
                  label="Index",
                  choices=c("NIFTY 50", "NIFTY NEXT 50"), 
                  selected="NIFTY 50")
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    rollingReturns(input$index)
  })
}

shinyApp(ui = ui, server = server)

