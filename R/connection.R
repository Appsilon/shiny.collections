#' Make sure that rethink database exists
#'
#' This function checks whether database `db_name` exists and if not
#' it creates one.
#'
#' @param connection structure with rethinkdb connection details
#' @param db_name character with database name
#' @export
#'
#' @import rethinker
#'
#' @examples
#' \dontrun{
#' cn <- connect()
#' make_sure_db_exists(cn, "temp_db")
#' }
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
#'
#' @import rethinker
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- connect()
#' make_sure_db_exists(cn, "temp_db")
#' make_sure_table_exists(cn, "table one", temp_db")
make_sure_table_exists <- function(connection, table_name) {
  db_name <- connection$db_name
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
#' @import rethinker
#' @importFrom yaml yaml.load_file
#'
#' @return connection structure
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- connect()
#' }
connect <- function(host = "localhost", port = "28015", db_name = DEFAULT_DB,
                    config_file = DEFAULT_CONFIG_NAME) {
  if (file.exists(config_file)) {
    config <- yaml.load_file(config_file)
  } else {
    config <- list(host = host, port = port)
  }
  connection <- list(
    id = sample(10^6, 1),
    config = config,
    raw_connection = rethinker::openConnection(config$host, config$port),
    db_name = db_name
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
#' \dontrun{
#' cn <- connect()
#' cn_copy <- clone_connection(cn)
#' }
clone_connection <- function(connection) {
  host <- connection$config$host
  port <- connection$config$port
  connect(host = host, port = port)
}

#' Drain RethinkDB connection
#'
#' @param connection structure with rethinkdb connection details
#'
#' @import rethinker
#'
#' @export
clear <- function(connection) {
  rethinker::drainConnection(connection$raw_connection)
}
