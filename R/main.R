#' cc <- collection("cars", connection)
#' isolate(cc$insert(list(name="skoda", value=10)))
#' cc$query(c("name","value"))
collection <- function(collection_name, connection) {
  make_sure_table_exists(connection, collection_name)
  collect_elem <- collect(collection_name, connection)
  structure(
    list(
      all = function()
        collect_elem,
      query = function(column_names = character(), post_process = I)
        collect(collection_name, connection, column_names, post_process),
      name = collection_name,
      connection = connection,
      insert = function(element, ...) {
        insert(collect_elem, element, ...)
        collect_elem <<- collect(collection_name, connection)
      },
      delete = function(element_id) {
        delete(collect_elem, element_id)
        collect_elem <<- collect(collection_name, connection)
      }
    ),
    class = "collection"
  )
}

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
#' tvs <- collect("tv_shows", cn)
#' # Take a look at gathered entries
#' isolate(tvs)
#'
#' # Using post_process you might use specific rethinkDB operations
#' # in rethinker query format before query is run (like filter, orderBy).
#' # E.g., to gather only tv shows with less than 100 episodes use:
#' tvs <- collect("tv_shows", cn,
#'             post_process = function(q) q$filter(function(x) x$bracket("episodes")$lt(100)))
#' }
collect <- function(collection_name, connection, column_names = character(),
                       post_process = I) {
  make_sure_table_exists(connection, collection_name)

  table_handle <- get_table_handle(connection$db_name, collection_name)
  query <- post_process(table_handle)
  cursor <- query$run(connection$raw_connection)
  reactive_value <- shiny::reactiveValues(
    collection = cursor_to_tibble(cursor, column_names)
  )

  table_handle$changes()$runAsync(connection$raw_connection, function(x) {
    other_connection <- clone_connection(connection)
    query <- post_process(get_table_handle(connection$db_name, collection_name))
    cursor <- query$run(other_connection$raw_connection)
    data <- cursor_to_tibble(cursor, column_names)
    close(other_connection$raw_connection)
    reactive_value$collection <- data
    TRUE
  })
  reactive_value$collection
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
  table_handle <- get_table_handle(collection$connection$db_name, collection$name)
  table_handle$insert(
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
  table_handle <- get_table_handle(collection$connection$db_name, collection$name)
  table_handle$get(element_id)$delete()$run(collection$connection$raw_connection)
}
