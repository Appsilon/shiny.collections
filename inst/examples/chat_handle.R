#' Shiny Chat example using shiny.collections
#' This app does exactly the same what `chat.R`, but uses object oriented
#' approach with `collection_handle` object.
#' Previous examples are going to expire soon.

library(shiny)
library(purrr)
library(dplyr)
library(purrrlyr)

#' This global variable describes how many recent variables are visible
#' in the chat window.
VISIBLE_MESSAGES <- 40

# Helper Functions
##############################

#' Render divs with messages
#'
#' @param collection Collection of entries from rethinkDB
#'
#' @return div object with formetted entries
render_msg_divs <- function(collection) {
  div(class = "ui very relaxed list",
      collection %>%
        arrange(time) %>%
        tail(VISIBLE_MESSAGES) %>%
        by_row(~ div(class = "item",
                     a(class = "header", .$user),
                     div(class = "description", .$text)
        )) %>% {.$.out}
  )
}

#' Get random username
#'
#' @return Character "User"+random number 10000<x99999
get_random_username <- function() {
  paste0("User", round(runif(1, 10000, 99999)))
}

# User Interface of the Chat
##############################

ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js")
  ),
  titlePanel("Chat app (shiny.collections demo)"),
  div(textInput("username_field", "Username", width = "200px")),
  uiOutput("chatbox"),
  div(style = "display:inline-block",
      textInput("message_field", "Your message", width = "500px")),
  div(style = "display:inline-block",
      actionButton("send", "Send"))
))

# Server with app logic
##############################

#' Before running the server we set up a connection with rethinkDB
connection <- shiny.collections::connect()

server <- shinyServer(function(input, output, session) {
  chat <- shiny.collections::collection_handle("chat", connection)
  #' By default the username is a big int number.
  updateTextInput(session, "username_field",
                  value = get_random_username()
  )
  observeEvent(input$send, {
    new_message <- list(user = input$username_field,
                        text = input$message_field,
                        time = Sys.time())
    chat$insert(new_message)
    updateTextInput(session, "message_field", value = "")
  })

  output$chatbox <- renderUI({
    #' We render message boxes (divs) only if collections
    #' contains some entries.
    if (!is_empty(chat$all())) {
      render_msg_divs(chat$all())
    } else {
      tags$span("Empty chat")
    }
  })

})

shinyApp(ui = ui, server = server)
