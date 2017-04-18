library(shiny)
library(purrr)
library(dplyr)
library(magrittr)

ui <- shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
                    #chatbox {
                     padding: .5em;
                     border: 1px solid #777;
                     height: 300px;
                     overflow-y: scroll;
                    }
                    "))
  ),
  #' maybe possible easier ?
  tags$script("
              var oldContent = null;
              window.setInterval(function() {
              var elem = document.getElementById('chatbox');
              if (oldContent != elem.innerHTML){
              scrollToBottom();
              }
              oldContent = elem.innerHTML;
              }, 300);
              function scrollToBottom(){
              var elem = document.getElementById('chatbox');
              elem.scrollTop = elem.scrollHeight;
              }
              "),
  titlePanel("Chat app (shiny.collections demo)"),
  div(style = "display:inline-block",
      textInput("unText", "Username", width = "200px")),
  uiOutput("chatbox"),
  div(style = "display:inline-block",
      textInput("msgText", "Your message", width = "500px")),
  div(style = "display:inline-block",
      actionButton("send", "Send"))
))

connection <- shiny.collections::connect()

#' TODO - collect only N latest
render_msg_divs <- function(collection){
    div(class = "ui very relaxed list",
        collection %>% arrange(time) %>%
          by_row(~ div(class = "item",
                      a(class = "header", .$user),
                      div(class = "description", .$text)
                      )
                ) %>% {.$.out}
       )
}

server <- shinyServer(function(input, output, session) {
  chat <- shiny.collections::collection("chat", connection)
  #' default username is a big int number
  updateTextInput(session, "unText",
                  value = paste0("User", round(runif(1, 10000, 99999))))
  observeEvent(input$send, {
    shiny.collections::insert(chat,
                              list(user = input$unText,
                                   text = input$msgText,
                                   time = Sys.time()))
    updateTextInput(session, "msgText", value = "")
  })
  observeEvent(chat$collection, {
    output$chatbox <- renderUI({
      tryCatch({
        render_msg_divs(chat$collection)
      }, error = function(err) {
        tags$span("Empty chat")
      })
    })
  })
})

shinyApp(ui = ui, server = server)