#' arc-sine transformation
#'
#' @param p numeric vector
#'
#' @return arc-sine transformation of p
#' @export
#'
#' @examples asinTransform(reproducerTaskGra::data_experiment$COR)
asinTransform <- function(p) { 2 * asin(sqrt(p/100)) }

#' Inverse arc-sine transformation
#'
#' @param p numeric vector
#'
#' @return numeric vector representing inverse of arc-sine transformation of p
#' @export
#'
#' @examples asinInverseTransform(fitted(reproduce_lmm_CORRECTNESS()))
asinInverseTransform <- function(p) { 100*((sin(p/2))^2) }
