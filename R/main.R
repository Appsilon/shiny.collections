.onAttach <- function(...) {
  # assign("cached_connection", NULL, .GlobalEnv)
}

#' Make sure that rethink database exists
#'
#' This function checks whether database `db_name` exists and if not
#' it creates one.
#'
#' @param connection structure with rethinkdb connection details
#' @param db_name character with database name
#' @export
#'
#' @examples
#' cn <- connect()
#' make_sure_db_exists(cn, "temp_db")
make_sure_db_exists <- function(connection, db_name) {
  if (!(db_name %in% rethinker::r()$dbList()$run(connection$raw_connection))) {
    rethinker::r()$dbCreate(db_name)$run(connection$raw_connection)
  }
}

#' Make sure that rethinkdb table exists
#'
#' This function checks whether table `table_name` exists in databse `db_name`
#' and if not it creates one.
#'
#' @param connection structure with rethinkdb connection details
#' @param table_name character with table name
#' @param db_name character with databse name (default `DEFAULT_DB`)
#'
#' @return
#' @export
#'
#' @examples
#' cn <- connect()
#' make_sure_db_exists(cn, "temp_db")
#' make_sure_table_exists(cn, "table one", temp_db")
make_sure_table_exists <- function(connection, table_name, db_name = DEFAULT_DB) {
  if (!(table_name %in% rethinker::r()$db(db_name)$tableList()$run(connection$raw_connection))) {
    rethinker::r()$db(db_name)$tableCreate(table_name)$run(connection$raw_connection)
  }
}

#' Connect with RethinkDB
#'
#' This function reads config file (if exists) or from `host` and `port`
#' from parameters. Next, it opens a connection and creates database `db_name`
#' (only if  already does not exists).
#'
#' @param host character with host address (default `localhost`)
#' @param port character with port number (default `28015`)
#' @param db_name character with databse name (default `DEFAULT_DB`)
#' @param config_file character with rethink configuration file name
#' (default `DEFAULT_CONFIG_NAME`)
#'
#' @return connection structure
#' @export
#'
#' @examples
#' cn <- connect()
connect <- function(host = "localhost", port = "28015", db_name = DEFAULT_DB,
                    config_file = DEFAULT_CONFIG_NAME) {
  if (file.exists(config_file)) {
    config <- yaml::yaml.load_file(config_file)
  } else {
    config <- list(host = host, port = port)
  }

  connection <- list(
    id = sample(10^6, 1),
    config = config,
    raw_connection = rethinker::openConnection(config$host, config$port)
  )
  make_sure_db_exists(connection, db_name)
  connection
}

#' Clone connection
#'
#' @param connection structure with rethinkdb connection details
#'
#' @return copy of connection structure
#' @export
#'
#' @examples
#' cn <- connect()
#' cn_copy <- clone_connection(cn)
clone_connection <- function(connection) {
  host <- connection$config$host
  port <- connection$config$port
  connect(host = host, port = port)
}

#' Drain RethinkDB connection
#'
#' @param connection structure with rethinkdb connection details
#'
#' @export
clear <- function(connection) {
  rethinker::drainConnection(connection$raw_connection)
}

empty_tibble <- function(column_names) {
  args <- purrr::map(column_names, ~ character())
  names(args) <- column_names
  do.call(tibble, args)
}

cursor_to_tibble <- function(cursor, column_names) {
  data <- dplyr::bind_rows(rethinker::cursorToList(cursor))
  if (nrow(data) == 0) empty_tibble(column_names) else data
}

#' Collect table entries
#'
#' This collects entries from specific collection and assigns it to reactive value.
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
collection <- function(collection_name, connection, column_names = character(),
                       post_process = I) {
  make_sure_table_exists(connection, collection_name)

  query <- post_process(rethinker::r()$db(DEFAULT_DB)$table(collection_name))
  cursor <- query$run(connection$raw_connection)
  reactive_value <- shiny::reactiveValues(
    name = collection_name,
    connection = connection,
    collection = cursor_to_tibble(cursor, column_names)
  )

  rethinker::r()$db(DEFAULT_DB)$table(collection_name)$changes()$runAsync(connection$raw_connection, function(x) {
    other_connection <- clone_connection(connection)
    query <- post_process(rethinker::r()$db(DEFAULT_DB)$table(collection_name))
    cursor <- query$run(other_connection$raw_connection)
    data <- cursor_to_tibble(cursor, column_names)
    close(other_connection$raw_connection)
    reactive_value$collection <- data
    TRUE
  })

  reactive_value
}

insert <- function(collection, element, ...) {
  rethinker::r()$db(DEFAULT_DB)$table(collection$name)$insert(
    element, ...
  )$run(collection$connection$raw_connection)
}

delete <- function(collection, element_id) {
  rethinker::r()$db(DEFAULT_DB)$table(collection$name)$get(element_id)$delete()$run(collection$connection$raw_connection)
}
