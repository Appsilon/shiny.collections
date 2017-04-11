library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("Chat app (shiny.collections demo)"),
  textAreaInput("caption", "Message Box", width = "600px", height = "300px"),
  div(style="display:inline-block", textInput("msgText", "Your message", width='400px')),
  div(style="display:inline-block", actionButton("send", "Send"))
))

connection <- shiny.collections::connect()

chat_printing <- function(tibble_df){
  stroutput <- ""
  for (i in 1:dim(tibble_df)[[1]]) {
    newrow <- paste(tibble_df$user[i], " : ", tibble_df$text[i])
    stroutput <- paste(stroutput, newrow, '\n')
  }
  return(stroutput)
}

server <- shinyServer(function(input, output, session) {
  chat <- shiny.collections::collection("chat", connection)
  observeEvent(input$send, {
    shiny.collections::insert(chat, list(user = "User1", text = input$msgText))
    print(chat_printing(chat$collection))
    updateTextInput(session, "msgText", value = "")
  })
  observeEvent(chat$collection, {
    #output$caption <- chat_printing(chat$collection)
    updateTextAreaInput(session, "caption", value = chat_printing(chat$collection))
  })


  #print(DT::datatable(chat$collection))
})

shinyApp(ui = ui, server = server)