#' Replace Inf/-Inf/NaN values
#'
#' Replace \code{Inf}, \code{-Inf}, and \code{NaN} in a matrix with \code{NA}
#'
#' @param x input matrix
#'
#' @import ggplot2
#'
#' @export
#'
#' @details This function replaces \code{Inf}, \code{-Inf}, and \code{NaN} in a matrix with \code{NA}. It is used internally by several functions.
#'
#' @author Julie Padilla
#'
#' @concept miscellaneous
#'
#' @return Returns a \code{matrix} object
#"
#' Replace Inf/-Inf/NaN values
#' @param x a matrix
remove_inf_and_nan <- function(x) {
  x[is.na(x)] <- NA
  x <- apply(x, 2, function(x) {
    x[is.infinite(x)] <- NA
    return(x)
  })
}
