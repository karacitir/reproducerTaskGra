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


# Descriptive statistics
#
# @param data a dataframe
# @param variable a character representing the column name in the data dataframe
# for which the descriptive statistics are returned
# @param overall Should the overall statistics be returned
#
# @return data frame which contains the descriptive statistics
#
describe_by_factor <- function(data, variable, overall = TRUE) {
  CONTROL_LEVEL <- "coarser-grained"
  TREATMENT_LEVEL <- "finer-grained"

  descriptiveStatLabels <- c("Mean (Std. Err.)", "95 % CI for mean",
                             "5% Trimmed Mean", "Median", "Std. Deviation",
                             "Min.- Max.", "Range", "Interquartile Range",
                             "Skewness", "Kurtosis")

  descriptivesByTreatment <- list()

# compute the descriptive statistics with coarser-grained Task Descriptions
  descriptivesByTreatment[[CONTROL_LEVEL]] <- data %>%
    dplyr::filter(.data$TASK_GRA==CONTROL_LEVEL) %>%
    dplyr::select(variable) %>%
    psych::describe(IQR = TRUE, trim = 0.05) %>%
    as.data.frame() %>%
    dplyr::select(-.data$vars, -.data$mad) %>%
    round(digits=2)

# compute the descriptive statistics with finer-grained Task Descriptions
  descriptivesByTreatment[[TREATMENT_LEVEL]] <- data %>%
    dplyr::filter(.data$TASK_GRA==TREATMENT_LEVEL) %>%
    dplyr::select(variable) %>%
    psych::describe(IQR = TRUE, trim = 0.05) %>%
    as.data.frame() %>%
    dplyr::select(-.data$vars, -.data$mad) %>%
    round(digits=2)

  descriptiveStats <- data.frame(row.names = c(CONTROL_LEVEL, TREATMENT_LEVEL))

  descriptiveStats <- rbind(descriptiveStats, descriptivesByTreatment[[CONTROL_LEVEL]])
  descriptiveStats <- rbind(descriptiveStats, descriptivesByTreatment[[TREATMENT_LEVEL]])

  row.names(descriptiveStats) <- c(CONTROL_LEVEL, TREATMENT_LEVEL)

  colNames <- c(CONTROL_LEVEL, TREATMENT_LEVEL)

# Compute the descriptive statistics for overall data (for both coarser- and
#  finer-grained task descriptions)
  if (overall) {
    descriptivesByTreatment[["All"]] <- data %>%
      dplyr::select(variable) %>%
      psych::describe(IQR = TRUE, trim = 0.05) %>%
      as.data.frame() %>%
      dplyr::select(-.data$vars, -.data$mad) %>%
      round(digits=2)

    descriptiveStats <- rbind(descriptiveStats, descriptivesByTreatment[["All"]])

    row.names(descriptiveStats) <- c(CONTROL_LEVEL, TREATMENT_LEVEL, "Overall")

  }

# Format the columns and rename labels
  descriptiveStats <- descriptiveStats %>%
    dplyr::mutate("Mean (Std. Err.)"=paste(.data$mean, " (", .data$se, ")", sep="")) %>%
    dplyr::mutate("Min.- Max." = paste(.data$min, .data$max, sep=" - ")) %>%
    dplyr::mutate("95 % CI for mean" =
                    paste("(",
                          round(.data$mean - qt(0.975,df=.data$n-1)*.data$se, digits=2),
                          ", ",
                          round(.data$mean + qt(0.975,df=.data$n-1)*.data$se,digits=2),
                          ")",
                          sep="")) %>%
    dplyr::rename("Std. Deviation"= .data$sd) %>%
    dplyr::rename("Median"= .data$median) %>%
    dplyr::rename("5% Trimmed Mean"= .data$trimmed) %>%
    dplyr::rename("Range"= .data$range) %>%
    dplyr::rename("Skewness"= .data$skew) %>%
    dplyr::rename("Kurtosis"= .data$kurtosis) %>%
    dplyr::rename("Interquartile Range"= .data$IQR) %>%
    dplyr::select(descriptiveStatLabels) %>%
    t()

  colnames(descriptiveStats) <- c(CONTROL_LEVEL, TREATMENT_LEVEL, "Overall")

  descriptiveStats
}

#' Reproduce Descriptive Statistics for Correctness
#'
#' @return a dataframe containing the descriptive statistics for Correctness
#' @export
#'
#' @examples reproduce_descriptive_stats_CORRECTNESS()
reproduce_descriptive_stats_CORRECTNESS <- function() {
  describe_by_factor(get_data(), "COR")
}

#' Reproduce Descriptive Statistics for Completeness
#'
#' @return a dataframe containing the descriptive statistics for Completeness
#' @export
#'
#' @examples reproduce_descriptive_stats_COMPLETENESS()
reproduce_descriptive_stats_COMPLETENESS <- function() {
  describe_by_factor(get_data(), "COMP")
}
