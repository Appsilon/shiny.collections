setOldClass("reactivevalues")

#' Collector
#'
#' @field all reactive value with collection values.
#' @field name character with a name of collection.
#' @field connection list with connection details
#'
#' @export
#'
#' @rdname collector
#'
#' @examples
#' #TODO
Collector <- setRefClass("Collector",
                         fields = list(all = "reactivevalues",
                                       name = "character",
                                       connection = "list")
)

#' @rdname collector
Collector$methods(
  initialize = function(collection_name, connection) {
    make_sure_table_exists(connection, collection_name)
    all <<- collection(collection_name, connection)
    connection <<- connection
    name <<- collection_name
  },
  query = function(column_names = character(), post_process = I){
    collection(collection_name, connection, column_names, post_process)
  },
  insert = function(element, ...) {
    insert(all, element, ...)
    all <<- collection(collection_name, connection)
  },
  delete = function(element_id) {
    delete(all, element_id)
    all <<- collection(collection_name, connection)
  }
)
