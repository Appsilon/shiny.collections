#' Collection handle
#'
#' It is a wrapper for shiny.collections functions, which allows object oriented
#' behaviour.
#'
#' @param collection_name character with name of collection.
#' @param connection list with connection details.
#'
#' @return collection_handle object, which can be intefaced using the methods
#' described in the 'Methods' section.
#'
#' @export
#'
#' @examples
#' #TODO
#'
#' @section Methods
#' \describe{
#'   \item{\code{query(column_names = character(), post_process = I)}}
#'   {Query database for elements with specific \code{column_names} or
#'   \code{post_process} function.}
#'   \item{\code{all()}}
#'   {Returns reactive value with all elements stored in database.}
#'   \item{\code{query(column_names = character(), post_process = I)}}
#'   {Query database for elements with specific \code{column_names} or
#'   \code{post_process} function and resturns reactive value with query result.}
#'   \item{\code{insert(element, ...)}}{Insert element into the collection.
#'   Argument 'element' must be a data-frame, named list (for single record) or
#'   character vector with json strings (one string for each row).}
#'   \item{\code{delete(element_id)}}
#'   {Returns element with character \code{element_id} from database.}
#' }
#' @section Properties
#' \describe{
#'   \item{\code{name}}
#'   {Character with collection name.}
#'   \item{\code{connection}}
#'   {\code{connection} object with connection details.}
#' }
collection_handle <- function(collection_name, connection) {
    make_sure_table_exists(connection, collection_name)
    col <- collection(collection_name, connection, direct = FALSE)
    structure(
      list(
        name = collection_name,
        connection = connection,
        all = function()
            col$collection,
        query = function(column_names = character(), post_process = I)
            collection(collection_name,
                       connection,
                       column_names,
                       post_process,
                       direct = FALSE),
        insert = function(element, ...) {
            delta <- insert(col, element, direct = FALSE, ...)
            col <<- collection(collection_name, connection, direct = FALSE)
            delta
          },
        delete = function(element_id) {
            delta <- delete(collection, element_id, direct = FALSE)
            col <<- collection(collection_name, connection)
            delta
          }
      ),
      class = "collection_handle"
    )
}
