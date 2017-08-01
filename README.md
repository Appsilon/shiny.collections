
<link href="http://fonts.googleapis.com/css?family=Lato:300,700,300italic|Inconsolata" rel="stylesheet" type="text/css"> <link href='docs/style.css' rel='stylesheet' type='text/css'>

shiny.collections
=================

Google Docs-like live collaboration in Shiny

**shiny.collections** adds persistent reactive collections that can be effortlessly integrated with components like Shiny inputs, DT::dataTable or rhandsontable. The package makes it easy to build collaborative Shiny applications with persistent data.

<!-- #Basic tutorial article is available on [Appsilon Data Science blog](your_future_art_link). -->
<!-- Live demo link below 
<p style="text-align: center; font-size: x-large;">
<a href="http://appsilondatascience.com/demo">Live demo</a>
</p>-->

Source code
-----------

This library source code can be found on [Appsilon Data Science's](http://appsilondatascience.com) Github: <br> <https://github.com/Appsilon/shiny.collections>

How to install?
---------------

**Note! This library is still in its infancy. Api might change in the future.**

At the moment it's possible to install this library through [devtools](https://github.com/hadley/devtools).

    devtools::install_github("Appsilon/shiny.collections")

To install [previous version]() you can run:

    devtools::install_github("Appsilon/shiny.collections", ref = "0.1.0")

Example
-------

Before running an example make sure that your RethinkDB is set-up and running. For installation and running guidelines you can visit [RethinkDB docs](https://www.rethinkdb.com/).

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

Talk
----

We are proud that **shiny.collections** gained a lot of interests at **useR!2017** in Brussels. Here you can see a talk by [Marek Rogala](https://github.com/marekrogala) presenting two interesting use cases: <https://goo.gl/mD1KfV> .

How to contribute?
------------------

If you want to contribute to this project please submit a regular PR, once you're done with new feature or bug fix.<br>

**Changes in documentation**

Both repository **README.md** file and an official documentation page are generated with Rmarkdown, so if there is a need to update them, please modify accordingly a **README.Rmd** file and run a **build\_readme.R** script to compile it.

Troubleshooting
---------------

We used the latest versions of dependencies for this library, so please update your R environment before installation.

However, if you encounter any problems, try the following:

1.  Up-to-date R language environment
2.  Installing specific dependent libraries versions
    -   shiny

            install.packages("shiny", version='0.14.2.9001')

Future enhacements
------------------

-   CRAN release
-   More methods (allowing for batch insert, update, delete, etc)
-   Publications and subscriptions allowing to sync only part of the data in each Shiny session

Appsilon Data Science
=====================

We Provide End-to-End Data Science Solutions

<a href="http://appsilondatascience.com"><img alt="Appsilon Data Science" src="https://cdn.rawgit.com/Appsilon/website-cdn/gh-pages/logo-white.png" /></a>

Get in touch [dev@appsilondatascience.com](dev@appsilondatascience.com)
