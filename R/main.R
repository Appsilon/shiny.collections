#' Collect table entries
#'
#' This collects entries from specific collection and assigns it to
#' a reactive value.
#'
#' @param collection_name A name of collection.
#' @param connection Connection object.
#' @param column_names Names of specific columns.
#' @param post_process Post process function called before query is run.
#'
#' @return Reactive value with connection, collection_name and collection
#' @export
#'
#' @examples
#' \dontrun{
#' # Set up a connection to DEFAULT_DB
#' cn <- connect()
#' tvs <- collection("tv_shows", cn)
#' # Take a look at gathered entries
#' isolate(tvs$collection)
#'
#' # Using post_process you might use specific rethinkDB operations
#' # in rethinker query format before query is run (like filter, orderBy).
#' # E.g., to gather only tv shows with less than 100 episodes use:
#' tvs <- collection("tv_shows", cn,
#'             post_process = function(q) q$filter(function(x) x$bracket("episodes")$lt(100)))
#' }
collection <- function(collection_name, connection, column_names = character(),
                       post_process = I) {
  make_sure_table_exists(connection, collection_name)

  db_name <- connection$db_name
  query <- post_process(rethinker::r()$db(db_name)$table(collection_name))
  cursor <- query$run(connection$raw_connection)
  reactive_value <- shiny::reactiveValues(
    name = collection_name,
    connection = connection,
    collection = cursor_to_tibble(cursor, column_names)
  )

  rethinker::r()$db(db_name)$table(collection_name)$changes()$runAsync(connection$raw_connection, function(x) {
    other_connection <- clone_connection(connection)
    query <- post_process(rethinker::r()$db(db_name)$table(collection_name))
    cursor <- query$run(other_connection$raw_connection)
    data <- cursor_to_tibble(cursor, column_names)
    close(other_connection$raw_connection)
    reactive_value$collection <- data
    TRUE
  })

  reactive_value
}

#' Insert element to collection
#'
#' @param collection reactive value with collection data
#' @param element list with content of element
#' @param ... other params to insert
#'
#' @return result status with numer of elements: deleted, errored,
#' inserted, replaced, skipped, unchanged
#' @export
insert <- function(collection, element, ...) {
  db_name <- collection$connection$db_name
  rethinker::r()$db(db_name)$table(collection$name)$insert(
    element, ...
  )$run(collection$connection$raw_connection)
}

#' Delete element from collection
#'
#' @param collection reactive value with collection data
#' @param element_id character with element id
#'
#' @return result status with numer of elements: deleted, errored,
#' inserted, replaced, skipped, unchanged
#' @export
delete <- function(collection, element_id) {
  db_name <- collection$connection$db_name
  rethinker::r()$db(db_name)$table(collection$name)$get(element_id)$delete()$run(collection$connection$raw_connection)
}
