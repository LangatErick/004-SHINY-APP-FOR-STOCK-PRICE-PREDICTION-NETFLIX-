#Libraries
library(shiny)
library(miniUI)
library(htmltools)
library(shinydashboard)
library(plotly)
library(datasets)
library(skimr)
library(zoo)
library(ggfortify)
library(dplyr)
library(ggplot2)
library(skimr)
library(lubridate)
library(stats)
library(shinycssloaders)
# library(shinyWidgets)
library(rsconnect)
library(hacksaw)

#Data improting and formatting
NFLX <- read.csv("NFLX.csv")
NFLX$Date <- as.Date(NFLX$Date, format = "%m/%d/%Y")
# NFLX$Date <- format(NFLX$Date, "%Y-%m-%d")
NFLX$Date <- as.Date(NFLX$Date)

#Plot Size
plotWidth <- 800
plotHeight <- 600

#color
defaultColor = "blue"

#########################
###########UI##########'
##########################
ui <- shiny::shinyUI(
  dashboardPage(skin = defaultColor,
                dashboardHeader(title = "Stock Price Prediction for Netflix",
                                tags$li(class="dropdown",tags$a(href="https://www.netflix.com/pk/", shiny::icon("globe"), "Go to website", target="_blank")),
                                tags$li(class="dropdown",tags$a(href="https://www.google.com/finance/quote/NFLX:NASDAQ?sa=X&ved=2ahUKEwjO08jb2u3-AhVDP-wKHdhWB0gQ3ecFegQIKxAf", shiny::icon("google"), "Check stock prices live", target="_blank"))
                ),
                dashboardSidebar(
                  sidebarMenu(id="sidebar", color = defaultColor,
                              menuItem("Data",tabName = "data",icon = shiny::icon("database"),
                                       menuSubItem("Dataset",tabName = "ds", icon = shiny::icon("table")),
                                       menuSubItem("Structure",tabName = "st", icon =shiny::icon("uncharted")),
                                       menuSubItem("Summary",tabName = "sum", icon = shiny::icon("chart-pie"))
                              ),
                              menuItem("Visualization", tabName = "visualization", icon = shiny::icon("chart-line"),
                                       menuSubItem("Line Graph",tabName = "lo",icon = shiny::icon("chart-line")),
                                       menuSubItem("Line Graph(365 days)",tabName = "ly", icon = shiny::icon("chart-line")),
                                       menuSubItem("Line Graph(30 days)",tabName = "lm", icon = shiny::icon("chart-line")),
                                       menuSubItem("Scatter plot",tabName = "sp", icon=icon("circle")),
                                       menuSubItem("Boxplot of Closing Value",tabName = "bc", icon=shiny::icon("th-large", lib = "glyphicon")),
                                       menuSubItem("Histogram of Volume",tabName = "hv", icon=shiny::icon("stats", lib="glyphicon"))),
                              menuItem("Probability distribution", tabName = "pd", icon=icon("area-chart")),
                              menuItem(" Regression model",tabName = "Regression Model",icon = shiny::icon("gears"),
                                       menuSubItem("Summary Regression Model",tabName = "rms", icon = shiny::icon("chart-pie")),
                                       menuSubItem("Prediction",tabName = "rm", icon = shiny::icon("cogs"))),
                              menuItem("Confidence interval", tabName = "ci",icon=shiny::icon("bars"),
                                       menuSubItem("Descriptive measure",tabName = "dci",icon=shiny::icon("table")),
                                       menuSubItem(" ReggreSsion estimates",tabName = "rci",icon=shiny::icon("cog"))))
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "ds",DT::DTOutput("dsp")),
                    tabItem(tabName = "st",fluidRow(verbatimTextOutput("st"))),
                    tabItem(tabName = "sum",fluidRow(verbatimTextOutput("sum"))),
                    tabItem(tabName = "lo",
                            fluidRow(box(title = "Line Graph", width=plotWidth,dateInput(inputId = "lsd", label = "Select start date", value = "2021-01-01"),dateInput(inputId = "led", label = "Select end date", value = "2022-01-01"), plotlyOutput("lop",height=plotHeight)))),
                    tabItem(tabName = "ly",
                            fluidRow(box(title = "Line Graph (365 days)", width=plotWidth,plotlyOutput("lyp",height=plotHeight)))),
                    tabItem(tabName = "lm",
                            fluidRow(box(title = "Line Graph (30 days)", width=plotWidth,plotlyOutput("lmp",height=plotHeight)))),
                    tabItem(tabName = "sp",
                            fluidRow(box(title = "Scatter plot", width=plotWidth,
                                         dateInput(inputId = "ssd", label = "Select start date", value = "2021-01-01"),dateInput(inputId = "sed", label = "Select end date", value = "2022-01-01"),
                                         radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                                         withSpinner(plotlyOutput("spp")), value="relation", height=plotHeight)
                            )),
                    tabItem(tabName = "bc",
                            fluidRow(box(title = "Boxplot", width=plotWidth,plotlyOutput("bcp",height=plotHeight)))),
                    tabItem(tabName = "hv",
                            fluidRow(box(title = "Histogram of Volume", width=plotWidth,plotlyOutput("hvp",height=plotHeight)))),
                    tabItem(tabName = "pd",
                            fluidRow(box(title = "Normal distribution plot",width=plotWidth,plotlyOutput("pdp",height=plotHeight)))),
                    tabItem(tabName = "rms",fluidRow(verbatimTextOutput("rms"))),
                    tabItem(tabName = "rm",numericInput("num", h3("Enter Opening Value:"), value = "120"),fluidRow(valueBoxOutput("rm",width = 4))),
                    tabItem(tabName = "dci",fluidRow(verbatimTextOutput("dci"))),
                    tabItem(tabName = "rci",fluidRow(verbatimTextOutput("rci")))
                  )
                )
  )
)


###################################
##################SERVER###################################
####################################Server

server <- shinyServer(function(input,output,session){
  
  # Dataset Plotting
  output$dsp <- renderDataTable({
    NFLX
  })
  
  #Data structure
  output$st <- renderPrint({
    str(NFLX)
  })
  
  #Data summary
  output$sum <- renderPrint({
    summary(NFLX)
  })
  
  #Line graph overall plotting
  output$lop <- renderPlotly({
    ggplot(NFLX[NFLX$Date > input$lsd & NFLX$Date < input$led, ], aes(x = Date, y = Close ,col =defaultColor)) +
      geom_line() + 
      scale_x_date(date_labels = "%m-%Y")
  })
  
  #Line graph year plotting
  lastYear <- tail(NFLX, 365)
  output$lyp <- renderPlotly({
    ggplot(lastYear, aes(x = Date, y = Close, col = defaultColor)) +
      geom_line() + 
      scale_x_date(date_labels = "%m-%Y")
  })
  
  #Line graph month plotting
  lastMonth <- tail(NFLX, 30)
  output$lmp <- renderPlotly({
    ggplot(lastMonth, aes(x = Date, y = Close, col = defaultColor)) +
      geom_line() + 
      scale_x_date(date_labels = "%d-%m")
  })
  
  #Scatter plot plotting
  output$spp <- renderPlotly({
    p <- ggplot(NFLX[NFLX$Date > input$ssd & NFLX$Date < input$sed, ], aes(x=Date, y=Close, size=Volume, col = defaultColor)) +
      geom_point() +
      geom_smooth(method=input$fit) +
      labs(x = "Date",
           y = "Close")
    ggplotly(p)
  })
  
  #Box-plot Closing plotting
  output$bcp <- renderPlotly({
    plot_ly(data= NFLX,y = ~Close, type = "box", color = defaultColor)
  })
  
  #Histogram volume plotting
  output$hvp <- renderPlotly({
    plot_ly(data = NFLX,x=~Volume,type = "histogram",color = defaultColor)
  })
  
  #Probability distribution plotting
  m <- mean(NFLX$Volume)
  sd <- sd(NFLX$Volume)
  output$pdp <- renderPlotly({
    ggplot(NFLX, aes(x = Volume, y = pnorm(Volume,mean = m,sd = sd), col = defaultColor)) +
      geom_point() 
  })
  
  #Regression model summary
  output$rms <- renderPrint({
    summary(model)
  })
  
  #Regression model prediction
  model <- lm(Close ~ Open, data=NFLX)
  output$rm <- renderValueBox({
    valueBox(
      color = defaultColor, value = predict.lm(model, newdata = data.frame(Open = c(input$num))) ,subtitle = "Expected Closing of **NFLX** will depend on the Opening value"
    )
  })
  
  #Descriptive confidence interval
  output$dci <- renderPrint({
    cat("Confidence interval of Open:", (t.test(NFLX$Open)$conf.int)[1:2], "\n")
    cat("Confidence interval of High:", (t.test(NFLX$High)$conf.int)[1:2], "\n")
    cat("Confidence interval of Low:", (t.test(NFLX$Low)$conf.int)[1:2], "\n")
    cat("Confidence interval of Close:", (t.test(NFLX$Close)$conf.int)[1:2], "\n")
    cat("Confidence interval of Volume:", (t.test(NFLX$Volume)$conf.int)[1:2], "\n")
  })
  
  #Regression confidence interval
  output$rci <- renderPrint({
    cat("Confidence interval of slope(Open) cofficient:", confint(model)["Open",], "\n")
    cat("Confidence interval of intercept cofficient:", confint(model)["(Intercept)",], "\n")
  })
  
})

############################################
#################THE SHINNY-APP############

app <- shinyApp(ui = ui, server = server)