#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
students_state = read_csv(file = "data/students_state.csv")

ratio_order = c(
  "undergrad_ratio",
  "professional_ratio",
  "grad_ratio"
)

ui = navbarPage(
  title = "Students and States",
  tabPanel(
    title = "Input/Visulization",
    titlePanel(title = "Students Number Ratio: 1975 -2017"),
    sidebarPanel(
      selectInput(
        inputId = "year", 
        label = "Year:", 
        choices = sort(unique(students_state$Year))),
      selectInput(
        inputId = "state",
        label = "State",
        choices = sort(unique(students_state$State))),
      selectInput(
        inputId = "level",
        label = "Level",
        choices = unique(students_state$Level)
      ),
      checkboxInput(inputId = "filterstate", label = "Filter Table to State",value = FALSE)
      ),
    mainPanel(plotOutput("plot"))
  ),
  tabPanel(title = "Table",dataTableOutput("table")),
  tabPanel(title = "About",includeMarkdown("about.Rmd"))
)

# Define server logic required to draw a histogram
server = function(input, output) {
  
  year_state = reactive({
    students_state |>
      filter(Year == input$year)
  })
  
  observeEvent(
    eventExpr = input$year,
    handlerExpr = {
      updateSelectInput(
        inputId = "state", 
        choices = unique(year_state()$State))
    }
  )
  year_state_r = reactive({
    year_state() |>
      filter(State == input$state)
  })
  
  observeEvent(
    eventExpr = input$state,
    handlerExpr = {
      updateSelectInput(
        inputId = "level_2",
        choices = sort(unique(year_state()$Level))
      )
    }
  )
  output$plot = renderPlot({
    students_state |>
      filter(State == input$state) |>
      filter(Year == input$year) |>
      pivot_longer(
        cols = undergrad_ratio : grad_ratio,
        names_to = "Ratio",
        values_to = "Percent"
      ) |>
      mutate(Ratio = factor(Ratio, levels = ratio_order)) |>
      filter(Level == input$level) |>
      ggplot() +
      aes(x = Ratio, y = Percent,fill = Ratio) |>
      geom_col()
  })
  
  output$table = renderDataTable({
    tab = year_state() |>
      calc_ratio()
    if(input$filterstate) {
      tab = tab |>
        filter(State == input$state)
    }
    tab
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
