#' All possible combinations of a vector
#'
#' Returns all possible combinations of a vector
#' @param data A vector for which you want to find all the possible combinations
#' @return Vector with all the possible combinations of the input vector
#'
#' @examples
#' all_vector_comb(data = c("a","b","c"))
#'
#' @export

all_vector_comb <- function(data){
  c(data, lapply(seq_along(data)[-1L],
              function(y) combn(data, y, paste0, collapse = ",")),
    recursive = TRUE)
}
