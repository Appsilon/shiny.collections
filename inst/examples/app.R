library(shiny)

ui <- shinyUI(fluidPage(
  actionButton("click", "Add one"),
  DT::dataTableOutput("cars_data")
))

connection <- shiny.collections::connect()

server <- shinyServer(function(input, output) {
  cars <- shiny.collections::collection("cars", connection)

  observeEvent(input$click, {
    shiny.collections::insert(cars, list(name = "Sample name", value = sample(1:100, 1)))
  })
  output$cars_data <- DT::renderDataTable(DT::datatable(cars$collection))
})

shinyApp(ui = ui, server = server)