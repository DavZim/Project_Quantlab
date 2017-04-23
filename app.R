# Shiny Stationarity

library(shiny)
library(shinydashboard)
library(gridExtra)
library(ggplot2)
library(data.table)
library(markdown)
library(knitr)

source("global.R")

rmdfiles <- c("text_regression.rmd", "about.rmd", "text_stationarity.rmd", 
              "text_stationarity_examples.rmd", "text_stationarity_tests.rmd", 
              "text_stationarity_solutions.rmd")
sapply(rmdfiles, knit, quiet = T)

theme_set(theme_light())
if ("Cairo" %in% rownames(installed.packages())) {
  library(Cairo)
  options(shiny.usecairo = TRUE)
}

sidebar <- function() {
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("terminal")),
      menuItem("Regression", tabName = "regression", icon = icon("line-chart")),
      menuItem("Stationarity", tabName = "stationarity", icon = icon("bar-chart")),
      menuItem("About", tabName = "about", icon = icon("code"))
    )
  )
}

ui_stationarity <- function() {
  tabItem(tabName = "stationarity",
          fluidRow(
            tabsetPanel(
              tabPanel(title = "Introduction",
                       box(title = "Introduction", width = 12, 
                           inclMarkdown("text_stationarity.md")
                       ),
                       box(title = "Stationary Time Series", 
                           collapsible = F, solidHeader = T, 
                           status = "success",
                           uiOutput("plot_stat_series"),
                           plotOutput("plot_stat_hist")
                       ),
                       box(title = "Non-Stationary Time Series",
                           collapsible = F, solidHeader = T, 
                           status = "success",
                           uiOutput("plot_non_stat_series"),
                           plotOutput("plot_non_stat_hist")
                       ),
                       paste("Notice that the length of the selections",
                             "might differ; to be able to calculate the",
                             "covariance, the authors have cheated a bit",
                             "by using sampling techniques, therefore the",
                             "covariance is only approximated and does not",
                             "reflect the proper covariance.")
              ),
              tabPanel(title = "Examples",
                       inclMarkdown("text_stationarity_examples.md"),
                       box(plotOutput("plot_ts1"),
                           plotOutput("plot_ts3"),
                           plotOutput("plot_ts5")),
                       
                       box(plotOutput("plot_ts2"),
                           plotOutput("plot_ts4"),
                           plotOutput("plot_ts6"))
                       
              ),
              tabPanel(title = "Tests",
                       inclMarkdown("text_stationarity_tests.md")
              ),
              tabPanel(title = "Solutions",
                       inclMarkdown("text_stationarity_solutions.md")
              )
            )
          )
  )
}

ui_regression <- function() {
  tabItem(tabName = "regression",
          fluidRow(
            tabsetPanel(
              tabPanel(title = "Introduction",
                       #includeMarkdown("text_regression.rmd")
                       inclMarkdown("text_regression.md")
                       #includeHTML("text_regression.html")
              ),
              tabPanel(title = "Assumptions"),
              tabPanel(title = "Tests")#, width = 800
            )
          )
  )
}

body <- function() {
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(title = "Introduction", solidHeader = F, status = "success")
              )
      ),
      ui_stationarity(),
      ui_regression(),
      tabItem(tabName = "about",
              fluidRow(
                box(title = "About", solidHeader = T, status = "success",
                    inclMarkdown("about.md"))
              ))
    )
  )
}

ui <- dashboardPage(
  dashboardHeader(title = "QuantLab"),
  sidebar(),
  body()
)


server <- function(input, output) {
  source("global.R")
  
#     selmin_stat <- function() 750
#     selmax_stat <- function() 900
#     selmin_non_stat <- function() 700
#     selmax_non_stat <- function() 900
  
  # reactive expressions to get the values from the brushed area
  selmin_stat <- reactive(getSel(input$plot_brush_stat_xmin, elseval = 700))
                #reactive(round(ifna(input$plot_brush_stat$xmin, elseval = 700), 0))
  selmax_stat <- reactive(getSel(input$plot_brush_stat$xmax, elseval = 900))
                #reactive(round(ifna(input$plot_brush_stat$xmax, elseval = 900), 0))
  
  selmin_non_stat <- reactive(getSel(input$plot_brush_non_stat$xmin, elseval = 700))
    #reactive(round(ifna(input$plot_brush_non_stat$xmin, elseval = 700), 0))
  selmax_non_stat <- reactive(getSel(input$plot_brush_non_stat$xmax, elseval = 900))
    #reactive(round(ifna(input$plot_brush_non_stat$xmax, elseval = 900), 0))
  
  # Stat!
  output$plot_stat_series <- renderUI({
    plotOutput("plot_stat", height = 300, 
               brush = brushOpts(id = "plot_brush_stat", direction = "x",
                                 fill = "blue", opacity = 0.5)
    )
  })
  
  output$plot_stat <- renderPlot({
    ggplot() + geom_line(data = dat, aes(x = x, y = stat), size = 0.1) +
      geom_rect(aes(xmin = base[1], xmax = base[length(base)], 
                    ymin = -Inf, ymax = Inf), alpha = 0.5, fill = cols[1]) + 
      geom_rect(aes(xmin = 700, xmax = 900, 
                    ymin = -Inf, ymax = Inf), alpha = 0.1, fill = cols[2]) +
      ylab("Value") + xlab("t") + coord_cartesian(xlim = c(0, 1000))
  }, height = 230)
  
  output$plot_stat_hist <- renderPlot({
    
    pdat <- rbind(data.table(y = dat[x %in% base, stat],
                             type = "Base"),
                  data.table(y = dat[x %in% selmin_stat():selmax_stat(), 
                                     stat],
                             type = "Selection"))
    
    plot_hist(hdat = pdat)
    
  }, height = 230)
  
  # Non-Stat!
  output$plot_non_stat_series <- renderUI({
    plotOutput("plot_non_stat", height = 300, 
               brush = brushOpts(id = "plot_brush_non_stat", direction = "x",
                                 fill = "blue", opacity = 0.5)
    )
  })
  
  output$plot_non_stat <- renderPlot({
    ggplot() + geom_line(data = dat, aes(x = x, y = non_stat), size = 0.1) +
      geom_rect(aes(xmin = base[1], xmax = base[length(base)], 
                    ymin = -Inf, ymax = Inf), alpha = 0.5, fill = cols[1]) + 
      geom_rect(aes(xmin = 700, xmax = 900, 
                    ymin = -Inf, ymax = Inf), alpha = 0.1, fill = cols[2]) +
      ylab("Value") + xlab("t") + coord_cartesian(xlim = c(0, 1000))
  }, height = 230)
  
  output$plot_non_stat_hist <- renderPlot({
    
    # prepare the data
    pdat <- rbind(data.table(y = dat[x %in% base, non_stat],
                             type = "Base"),
                  data.table(y = dat[x %in% 
                                       selmin_non_stat():selmax_non_stat(), 
                                     non_stat],
                             type = "Selection"))
    
    plot_hist(pdat)
    
  }, height = 230)
  
  # TS Examples
  output$plot_ts1 <- renderPlot(plot_ts("ts1"), height = 250)
  output$plot_ts2 <- renderPlot(plot_ts("ts2"), height = 250)
  output$plot_ts3 <- renderPlot(plot_ts("ts3"), height = 250)
  output$plot_ts4 <- renderPlot(plot_ts("ts4"), height = 250)
  output$plot_ts5 <- renderPlot(plot_ts("ts5"), height = 250)
  output$plot_ts6 <- renderPlot(plot_ts("ts6"), height = 250)
}

shinyApp(ui, server)
