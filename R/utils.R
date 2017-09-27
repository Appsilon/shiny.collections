#' Create empty tibble
#'
#' This functions creates empty tibble with columns names specified
#' as parameter.
#'
#' @param column_names character vector with column names
#'
#' @import tibble
#' @import purrr
#'
#' @return empty tibble object
#' @export
#'
#' @examples
#' empty_tibble(c("a", "b"))
empty_tibble <- function(column_names) {
  args <- purrr::map(column_names, ~ character())
  names(args) <- column_names
  do.call(tibble::tibble, args)
}

#' Make tibble from cursor
#'
#' Receives data in tibble format from rethinkDB cursor.
#'
#' @param cursor rethinkDB cursor
#' @param column_names character vector with column names
#'
#' @import dplyr
#' @import rethinker
#' @import purrr
#'
#' @return either empty tibble or tibble with data
#' @export
cursor_to_tibble <- function(cursor, column_names) {
  data <- dplyr::bind_rows(rethinker::cursorToList(cursor))
  if (nrow(data) == 0) {
    empty_tibble(column_names)
  } else {
    if (purrr::is_empty(column_names)) {
      data
    } else {
      data[, column_names]
    }
  }
}

#' Get table handle
#'
#' @param db_name character with database name
#' @param collection_name character with collection name
#'
#' @import rethinker
#'
#' @return reql object
#'
#' @examples
#' \dontrun{
#'  get_table_handle("shiny", "cars")
#' }
get_table_handle <- function(db_name, collection_name) {
  rethinker::r()$db(db_name)$table(collection_name)
}
