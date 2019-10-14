library(shinydashboard)

dashboardPage(
    dashboardHeader(
        title = "Roulette Simulator"
    ),
  
    dashboardSidebar(
        sliderInput("n_games", 
        label="Number of games played:", 
        min=0, max=10000, value=100, step=100)
    ),

    dashboardBody(
        # css file
        tags$head(
            tags$link(rel="stylesheet", type="text/css", href="snare_industries.css")
            ),
        plotOutput("plot1"),
        fluidRow(
           box(plotOutput("plot3")),
           box(plotOutput("plot2")))
    )
)
