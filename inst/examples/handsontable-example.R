library(shiny)
library(shiny.collections)
library(rhandsontable)
library(purrr)

ui = shinyUI(fluidPage(
  titlePanel("Handsontable"),
  mainPanel(
    rHandsontableOutput("hot")
  )
))

connection <- shiny.collections::connect()

server = function(input, output) {
  # We create collection object, where mydata$collection is reactive value.
  mydata <- shiny.collections::collection("mydata", connection)
  column_names <- c("a", "b", "c")

  isolate({
    # If we run the app for the first time, we should fill our DB in
    # with some content.
    if(is_empty(mydata$collection)) {
      shiny.collections::insert(mydata,
                                list(a = 1, b="a", c = TRUE))
      shiny.collections::insert(mydata,
                                list(a = 3.14, b="xx", c = TRUE))
      shiny.collections::insert(mydata,
                                list(a = 100, b="some text", c = FALSE))

    }
  })

  # Reactive which gives list with changes.
  change_list <- reactive({
    changes <- NULL
    if(!is.null(input$hot$changes$changes)) {
      changes <- input$hot$changes$changes %>%
        map(function (change) {
          # +1 is needed as JS and R indexing differs.
          row <- change[[1]] + 1
          col <- change[[2]] + 1
          new_value <- change[[4]]
          list(row = row, col = col, val = new_value)
        })
    }
    changes[[1]]
  })

  # Here we observe for a change and update mydata using shiny.collections
  # insert function.
  observe({
    if (!is.null(change_list()$val)) {
      change_row <- as.list(mydata$collection[change_list()$row, ])
      change_col <- column_names[[change_list()$col]]
      change_row[[change_col]] <- change_list()$val
      shiny.collections::insert(mydata,
                                change_row,
                                conflict = "update")
    }
  })

  output$hot <- renderRHandsontable({
    rhandsontable(mydata$collection[column_names], useTypes = TRUE) %>%
      hot_table(readOnly = FALSE)
  })
}

shinyApp(ui = ui, server = server)
