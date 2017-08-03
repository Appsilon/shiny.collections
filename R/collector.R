Collector <- setRefClass("Collector",
                         fields = list(all = "reactivevalues",
                                       collection = "tbl_df",
                                       name = "character",
                                       connection = "list")
)

setOldClass("reactivevalues")

Collector$methods(
  initialize = function(collection_name, connection) {
    make_sure_table_exists(connection, collection_name)
    all <<- collect(collection_name, connection)
    collection <<- all$collection
    connection <<- connection
    name <<- collection_name
  },
  query = function(column_names = character(), post_process = I){
    collect(collection_name, connection, column_names, post_process)
  },
  insert = function(element, ...) {
    insert(all, element, ...)
    all <<- collect(collection_name, connection)
    collection <<- all$collection
  },
  delete = function(element_id) {
    delete(all, element_id)
    all <<- collect(collection_name, connection)
    collection <<- all$collection
  }
)