#' Reads sql queries from a .sql file
#'
#' Returns A vector of SQL queries present in the .sql file separated by semi colon
#' @param filepath Full path of the sql file
#' @return A vector of SQL queries present in the .sql file separated by semi colon
#'
#' @export
readSQL <- function(filepath) {
  # Reads sql queries from a .sql file
  #
  # Args:
  #  filepath: Full path of the sql file
  #
  # Returns:
  #  A vector of SQL queries present in the .sql file separated by semi colon
  con <- file(filepath, "r")

  sql.vector <- c()
  sql.string <- ""

  while (TRUE) {
    line <- readLines(con, n = 1)

    if (length(line) == 0) {
      break
    }

    line <- gsub("\\t", " ", line)

    if (grepl("--", line) == TRUE) {
      line <- paste(sub("--", "/*", line), "*/")
    }

    if (grepl(";", line) == TRUE) {
      split.sql <- strsplit(line, ";")[[1]]

      for (i in 1:length(split.sql)) {
        sql.string <- paste(sql.string, split.sql[i], ";")
        sql.vector <- c(sql.vector, sql.string)
        sql.string <- ""
      }
    }
    else {
      sql.string <- paste(sql.string, line)
    }
  }

  if (sql.string != "") {
    sql.vector <- c(sql.vector, sql.string)
  }

  close(con)

  return(trimws(sql.vector))
}
