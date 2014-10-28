library(shiny)

shinyUI(fluidPage(
  headerPanel('Optimum pub crawl order finder'),
  sidebarPanel(
    selectizeInput(
      'v_pubs', 'Select pubs', choices = pubs_all_list, multiple = TRUE
    ),
    submitButton("Find route")
  ),
  mainPanel(
    plotOutput('plot1'),
    plotOutput("plot2")
  )
))