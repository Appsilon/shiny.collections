DEFAULT_CONFIG_NAME <- "rethinkdb_config.yml"
DEFAULT_DB <- "shiny"

.onAttach <- function(...) {
  # assign("cached_connection", NULL, .GlobalEnv)
}

make_sure_db_exists <- function(connection, db_name) {
  if (!(db_name %in% rethinker::r()$dbList()$run(connection$raw_connection))) {
    rethinker::r()$dbCreate(db_name)$run(connection$raw_connection)
  }
}

make_sure_table_exists <- function(connection, table_name) {
  if (!(table_name %in% rethinker::r()$db(DEFAULT_DB)$tableList()$run(connection$raw_connection))) {
    rethinker::r()$db(DEFAULT_DB)$tableCreate(table_name)$run(connection$raw_connection)
  }
}

connect <- function(host = "localhost", port = "28015", config_file = DEFAULT_CONFIG_NAME) {
  if (file.exists(config_file)) {
    config <- yaml::yaml.load_file(config_file)
  } else {
    config <- list(host = host, port = port)
  }

  connection <- list(
    id = sample(10^6, 1),
    raw_connection = rethinker::openConnection(config$host, config$port)
  )
  make_sure_db_exists(connection, DEFAULT_DB)
  connection
}

clear <- function(connection) {
  rethinker::drainConnection(connection$raw_connection)
}

collection_as_table_sync <- function(collection_name, connection) {
  cursor <- rethinker::r()$db(DEFAULT_DB)$table(collection_name)$run(connection$raw_connection)
  rethinker::cursorToList(cursor)
}

collection <- function(collection_name, connection) {
  make_sure_table_exists(connection, collection_name)

  cursor <- rethinker::r()$db(DEFAULT_DB)$table(collection_name)$run(connection$raw_connection)
  data <- rethinker::cursorToList(cursor)
  reactive_value <- shiny::reactiveValues(
    name = collection_name,
    collection = do.call(rbind, data)
  )

  rethinker::r()$db(DEFAULT_DB)$table(collection_name)$changes()$runAsync(connection$raw_connection, function(x) {
    other_connection <- connect()$raw_connection
    cursor <- rethinker::r()$db(DEFAULT_DB)$table(collection_name)$run(other_connection)
    data <- rethinker::cursorToList(cursor)
    close(other_connection)
    reactive_value$collection <- do.call(rbind, data)
    TRUE
  })

  reactive_value
}

insert <- function(collection_name, element, connection) {
  rethinker::r()$db(DEFAULT_DB)$table(collection_name)$insert(
    element
  )$run(connection$raw_connection)
}

