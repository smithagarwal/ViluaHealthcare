#' Establishes a connection handle to the SQL Server
#'
#' Returns a connection string to the SQL Server to be used to query tables and views
#' @param username Username/RACF to connect to the server
#'
#' @import RODBC rstudioapi
#' @return connection string to the SQL Server to be used to query tables and views
#'
#' @export

OdbcConnect.SQLServer <- function(username) {
  con <- odbcConnect(dsn = "SQL Server", uid = username, pwd = askForPassword("Database Password"))

  return(con)
}
