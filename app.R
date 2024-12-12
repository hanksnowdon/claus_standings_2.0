# https://nrennie.rbind.io/blog/automatically-deploying-a-shiny-app-for-browsing-rstats-tweets-with-github-actions/#building-the-shiny-app

library(shiny)
library(DT)

# Load the required datasets
webpage_picks <- readRDS('results.rds')
standings <- readRDS('standings.rds')

# Define the UI
ui <- navbarPage(
  "Bowl Games App",
  tabPanel(
    "Picks",
    fluidPage(
      titlePanel("Game Picks"),
      dataTableOutput("picks_table")
    )
  ),
  tabPanel(
    "Standings",
    fluidPage(
      titlePanel("Standings"),
      dataTableOutput("standings_table")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Render the picks table
  output$picks_table <- renderDataTable({
    datatable(
      webpage_picks,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # Render the standings table
  output$standings_table <- renderDataTable({
    datatable(
      standings,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)