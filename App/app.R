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
                            "Sacrifice Hit Rate",
                            "Three True Outcomes Rate")

batting_summary[, 7:12] <- round(batting_summary[, 7:12], 5) # round granular ratios to 5 decimal places

##### UI SIDE #####
ui <- navbarPage(
  
  theme = shinytheme("yeti"),
  
  title = "The Evolution of Major League Baseball Offense",
  
  tabPanel("About",
           mainPanel(
             br(),
             img(src = "bryce.jpg"),
             h2("Welcome!"), br(),
             p("Though the lethargic pace of Major League Baseball games (I wrote the code for this app and took a nap within the span of one inning, give or take) has led me to shy from watching a full nine innings this season, I'm still very curious about the sport and the league. After all, baseball was the first sport that I enjoyed as a kid. It was also (as is the case for many) the backdrop of my introduction to quantitative approaches to measuring performance."),
             br(), p(htmlOutput("narrative_two")),
             br(), p(htmlOutput("narrative_three")),
             br(), p(htmlOutput("narrative_four")), br(), br()
           )),
  
  tabPanel("Chart", 
           sidebarLayout(
             sidebarPanel(
               # dropdown to select stat of interest
               selectInput(inputId = "userStat",
                           label = "Select the leaguewide statistic you'd like to see evolve over time:",
                           choices = names(batting_summary)[-1], # omit season from selections
                           selected = names(batting_summary)[2]
                           ),
      
               # dropdown for era selection
               selectInput(inputId = "era",
                           label = "Select which era of baseball you'd like to highlight:",
                           choices = c("",
                                       "Dead Ball Era (1901-1919)",
                                       "Live Ball Era (1920-1941)",
                                       "Integration Era (1947-1960)",
                                        "Expansion Era (1961-1976)",
                                        "Steroid Era (1990-2005)")
                           )
               ),
             mainPanel(
               plotly::plotlyOutput(outputId = "historyPlot")
               )
             )
           ),
  
  tabPanel("Data Glossary",
           mainPanel(
             h2("Variable Definitions"),
             br(), h4(names(batting_summary)[2]), p("Proportion of at-bats that end with a base hit"),
             br(), h4(names(batting_summary)[3]), p("Proportion of plate appearances that end with the batter reaching base via base hit, base on balls, or hit by pitch"),
             br(), h4(names(batting_summary)[4]), p("Total bases (attained via base hit) per at-bat"),
             br(), h4(names(batting_summary)[5]), p("Sum of on-base and slugging percentages"),
             br(), h4(names(batting_summary)[6]), p("Number of home runs hit league-wide"),
             br(), h4(names(batting_summary)[7]), p("Proportion of plate appearances that end with a home run"),
             br(), h4(names(batting_summary)[8]), p("Proportion of plate appearances that end with a base on balls"),
             br(), h4(names(batting_summary)[9]), p("Proportion of plate appearances that end with a strikeout"),
             br(), h4(names(batting_summary)[10]), p("Ratio of bases on balls to strikeouts"),
             br(), h4(names(batting_summary)[11]), p("Proportion of plate appearances that end with a sacrifice hit (the advancing of runners by bunting the ball for an out)"),
             br(), h4(names(batting_summary)[12]), p("Proportion of plate appearances that end via bases on balls, home runs, or strikeouts (i.e. without a ball put into play)"),
             br(), br())
           ),
  
  tabPanel("Links",
           mainPanel(
             h3("Code:"), htmlOutput("git"), br(),
             h3("Data:"), htmlOutput("fangraphs"), br(), htmlOutput("era"), br(),
             h3("Photography:"), htmlOutput("bryce")
           ))
  )

##### SERVER SIDE #####
server <- function(input, output){
  output$bryce <- renderText({
    usa_link <- "http://www.usatsimg.com/"
    HTML(paste0("<a href='", usa_link, "' target='_blank'>USA Today Sports Images</a>"))
  })
  output$narrative_two <- renderText({
    jose_link <- "https://www.youtube.com/watch?v=-UdsVO7HaJg"
    pujols_link <- "https://www.youtube.com/watch?v=lsEuTYbDRwE"
    HTML(paste0("Back when I watched baseball more frequently, I found the home run to be the most magnetizing aspect of the game. A violent swing of a bat sends rubber and cork flying hundreds of feet, all while the stadium fills with <a href='", jose_link, "' target='_blank'>deafening applause</a> or  <a href='", pujols_link, "' target='_blank'>deafening silence.</a>"))
  })
  
  fangraphs_link <- "https://www.fangraphs.com/"
  output$narrative_three <- renderText({
    HTML(paste0("In the few games that I have watched in the last year or two, I noticed that these 'special' moments seemed to be occuring more frequently. As a statistics student, however, I was aware that this might have been a mirage, a function of the small number of games that I had watched. Hoping to find a clear refutation or confirmation of my suspicion, I decided to scrape league-wide batting statistics from  <a href='", fangraphs_link, "' target='_blank'>Fangraphs</a> and create this app to visualize how major league offenses have transformed over time."))
  })
  
  output$narrative_four <- renderText({
    juice_link <- "https://www.usatoday.com/story/sports/mlb/2018/05/24/mlb-home-run-study-juiced-ball-aerodynamics-carry-humidor/641654002/"
    HTML(paste0("Using this app, we can clearly see that home runs are now occurring more frequently than at any other point in league history.  <a href='", juice_link, "' target='_blank'>A new study commissioned by Major League Baseball</a> indicates that this surge is at least partially explained by newly altered aerodynamic properties of the baseballs being used in-game. We can also observe that, in addition to homering more often, hitters are striking out more often too. Indeed, the frequency of the 'three true outcomes' (walks, homers, and strikeouts) has been rising to unprecedented levels leaguewide; one might surmise that a shift in batters' approaches also contributes to the proliferation of home runs."))
  })
  
  output$fangraphs <- renderText({
    HTML(paste0("Batting statistics were scraped from <a href='", fangraphs_link, "' target='_blank'>Fangraphs.</a>"))
  })
  
  netshrine_link <- "http://www.netshrine.com/era.html"
  output$era <- renderText({
    HTML(paste0("Information about Major League Baseball's historical eras was adapted from <a href='", netshrine_link, "' target='_blank'>NetShrine.</a> See the link to learn more about these eras."))
  })
  
  github_link <- "https://github.com/kvu1/baseball-evolution"
  output$git <- renderText({
    HTML(paste0("Check out the code for the data scraping and app-making on <a href='", github_link, "' target='_blank'>GitHub.</a>"))
  })
  
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