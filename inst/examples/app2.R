library(shiny)

ui <- shinyUI(fluidPage(
  actionButton("click", "Add one"),
  DT::dataTableOutput("cars_data")
))

connection <- shiny.collections::connect()

server <- shinyServer(function(input, output) {
  cars <- shiny.collections::collector("cars", connection)

  observeEvent(input$click, {
    cars$insert(list(name = "Sample name", value = sample(1:100, 1)))
  })
  output$cars_data <- DT::renderDataTable(DT::datatable(cars$all()))
})

shinyApp(ui = ui, server = server)
