# load necessary libraries
library(rvest)
library(dplyr)
library(stringr)
library(shiny)
library(shinythemes)
library(plotly)

# read in dataset
batting_summary <- read.csv("summary_bat.csv") %>%
  select(-X)

names(batting_summary) <- c("Season", 
                            "Batting Average", 
                            "On-Base Percentage",
                            "Slugging Percentage",
                            "On-Base Plus Slugging",
                            "Total Home Runs",
                            "Home Run Rate",
                            "Walk Rate",
                            "Strikeout Rate",
                            "Walk-to-Strikeout Ratio",
                            "Sacrifice Hit Rate")

batting_summary[, 7:11] <- round(batting_summary[, 7:11], 5) # round granular ratios to 5 decimal places

##### UI SIDE #####
ui <- navbarPage(
  
  theme = shinytheme("yeti"),
  
  title = "The Evolution of Major League Baseball",
  
  sidebarLayout(
    sidebarPanel(
      
      # dropdown to select stat of interest
      selectInput(inputId = "userStat",
                  label = "Select the leaguewide statistic you'd like to see evolve over time:",
                  choices = names(batting_summary)[-1], # omit season from selections
                  selected = names(batting_summary)[2]),
      
      # dropdown for era selection
      selectInput(inputId = "era",
                  label = "Select which era of baseball you'd like to highlight:",
                  choices = c("",
                              "Dead Ball Era (1901-1919)",
                              "Live Ball Era (1920-1941)",
                              "Integration Era (1947-1960)",
                              "Expansion Era (1961-1976)",
                              "Steroid Era (1990-2005)"))
      
    ),
    
    mainPanel(
      plotly::plotlyOutput(outputId = "historyPlot")
    )
  )
)

##### SERVER SIDE #####
server <- function(input, output){
  
  output$historyPlot <- renderPlotly({
    
    user_choice <- input$userStat
    
    custom_summary <- batting_summary %>%
      select(Season, user_choice)
    
    custom_summary <- filter(custom_summary, !is.na(custom_summary[,2])) # prevent rectangle geom from acting strangely with jump discontinuities
    
    # define width of rectangle contingent on user era input
    if (input$era == "Dead Ball Era (1901-1919)") {
      interval <- c(1901, 1919)
    } else if (input$era == "Live Ball Era (1920-1941)") {
      interval <- c(1920, 1941)
    } else if (input$era == "Integration Era (1947-1960)") {
      interval <- c(1947, 1960)
    } else if (input$era == "Expansion Era (1961-1976)") {
      interval <- c(1961, 1976)
    } else if (input$era == "Steroid Era (1990-2005)") {
      interval <- c(1990, 2005)
    } else
      interval <- c(min(custom_summary$Season), max(custom_summary$Season))
    
    user_dependent <- custom_summary[, 2]
    
    
    # interactive scatterplot
    plot_ly(data = custom_summary,
            x = ~Season,
            y = ~user_dependent,
            type = 'scatter',
            mode = 'lines',
            line = list(color = "navy", size = 15),
            hoverinfo = 'text',
            text = ~paste("Season: ", Season,
                          "<br>Value: ", user_dependent)) %>%
      layout(xaxis = list(title = "Season"),
             yaxis = list(title = paste(user_choice),
                          tickformat = ifelse(user_choice == "Total Home Runs", ".0f", ".3f")),
             shapes = list(type = "rect",
                           fillcolor = "orange",
                           line = list(color = "orange"),
                           opacity = ifelse(input$era == "", 0, 0.25),
                           x0 = interval[1], x1 = interval[2], xref = "x",
                           y0 = min(user_dependent), y1 = max(user_dependent), yref = "y"
                           ) # set bounds on how high the rectangle extends
             )
  
  })
}

shinyApp(ui = ui, server = server)