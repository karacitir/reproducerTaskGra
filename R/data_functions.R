#' Get experiment dataset
#'
#' @param columns a character vector containing the column names to be returned
#'
#' @return a dataframe contating the selected columns of the experiment dataset
#' @export
#'
#' @examples get_data()
#' get_data(c("TASK_GRA", "COMP"))
get_data <- function(columns =c())
{
  data <- reproducerTaskGra::data_experiment
  if (length(columns)>0) {
    data <- data %>%
      dplyr::select(columns)
  }
  data
}
