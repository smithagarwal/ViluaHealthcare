#' Runs all the sql queries for a connection instance
#'
#' Returns a list of results generated after running the sql queries
#' @param con connection handle as returned by odbcConnect
#' @param sql.vector vector of sql queries
#' @return a list of results generated after running the sql queries
#'
#' @export
runSQL <- function(con, sql.vector) {
  # Runs all the sql queries for a connection instance
  #
  # Args:
  #  con: connection handle as returned by odbcConnect
  #  sql.vector : vector of sql queries
  #
  # Returns:
  #  A list of results generated after running the sql queries
  result.list <- lapply(sql.vector,sqlQuery,channel = con)

  return(result.list)
}
