#' Create violin plot
#'
#' @param data_plot a dataframe containing the data for plot
#' @param x_variable name of the variable for x-axis
#' @param y_variable name of the variable for y-axis
#' @param ifStat Should the mean and mean values added to the plot
#'
#' @return a ggplot object
#' @export
#' @examples plotViolin(get_data(), "TASK_GRA", "COR")
#' plotViolin(reproducerTaskGra::data_experiment, "TASK", "TDD_CONF_SELF")
plotViolin <- function(data_plot, x_variable, y_variable, ifStat=FALSE) {
  plotLabels <- list(COR = "CORRECTNESS", COMP="COMPLETENESS", TASK="Task",
                     TASK_GRA="Task Description Granularity", CONF = "TDD-conformance",
                     TESTS="Number of Tests", EPISODES = "Number of Episodes",
                     QLTY = "Quality", EXP_JAVA = "Experience in Java")

  #theme_set(theme_custom_grey)
  h <- ggplot(data_plot, aes_string(x = x_variable, y = y_variable, color=x_variable, shape = x_variable)) +
    scale_y_continuous(breaks = seq(0, 100, 10), expand = c(0.01,0.05)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    theme(aspect.ratio = 5/7) +
    geom_dotplot(binaxis='y', binwidth=5, stackdir='center', dotsize = .2, aes_string(fill=x_variable)) +
    scale_x_discrete(expand = c(0.1,0)) +
    xlab(plotLabels[[x_variable]]) + ylab(plotLabels[[y_variable]])

  if (ifStat) {
#    h <- h + stat_summary(fun.y = "median", geom = "text", size=2.5, vjust=2.5, color="black", aes(label = paste("median = ", round(..y.., digits = 2))))
#    h <- h + stat_summary(fun.y = "mean", geom = "text", size=2.5, color="black", aes(label = paste("mean = ", round(..y.., digits = 2))))
  }
  h

}

#' Reproduce violin plot for Correctness
#'
#' @return an object of class ggplot
#' @export
#'
#' @examples reproduce_violin_plot_CORRECTNESS()
reproduce_violin_plot_CORRECTNESS <- function(){
  plotViolin(reproducerTaskGra::data_experiment, "TASK_GRA", "COR")
}

#' Reproduce violin plot for Completeness
#'
#' @return an object of class ggplot
#' @export
#'
#' @examples reproduce_violin_plot_COMPLETENESS()
reproduce_violin_plot_COMPLETENESS <- function(){
  plotViolin(reproducerTaskGra::data_experiment, "TASK_GRA", "COR")
}
