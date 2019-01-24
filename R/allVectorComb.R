#' All possible combinations of a vector
#'
#' Returns all possible combinations of a vector
#' @param data A vector for which you want to find all the possible combinations
#' @return Vector with all the possible combinations of the input vector
#'
#' @examples
#' allVectorComb(data = c("a","b","c"))
#'
#' @export

allVectorComb <- function(data){
  c(data, lapply(seq_along(data)[-1L],
              function(y) combn(data, y, paste0, collapse = ",")),
    recursive = TRUE)
}
