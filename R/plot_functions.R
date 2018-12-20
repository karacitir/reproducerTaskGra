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
  h <- ggplot2::ggplot(data_plot, aes_string(x = x_variable, y = y_variable, color=x_variable, shape = x_variable)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 10), expand = c(0.01,0.05)) +
    ggplot2::geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    ggplot2::theme(aspect.ratio = 5/7) +
    ggplot2::geom_dotplot(binaxis='y', binwidth=5, stackdir='center', dotsize = .2, aes_string(fill=x_variable)) +
    ggplot2::scale_x_discrete(expand = c(0.1,0)) +
    ggplot2::xlab(plotLabels[[x_variable]]) + ggplot2::ylab(plotLabels[[y_variable]])

  if (ifStat) {
#    h <- h + stat_summary(fun.y = "median", geom = "text", size=2.5, vjust=2.5, color="black", aes(label = paste("median = ", round(..y.., digits = 2))))
#    h <- h + stat_summary(fun.y = "mean", geom = "text", size=2.5, color="black", aes(label = paste("mean = ", round(..y.., digits = 2))))
  }
  h

}

#' Reproduce violin plot for Correctness
#'
#' @return an object of class ggplot containing the violin plot of Correctness
#' @export
#'
#' @examples reproduce_violin_plot_CORRECTNESS()
reproduce_violin_plot_CORRECTNESS <- function(){
  plotViolin(reproducerTaskGra::data_experiment, "TASK_GRA", "COR")
}

#' Reproduce violin plot for Completeness
#'
#' @return an object of class ggplot containing the violin plot of Completeness
#' @export
#'
#' @examples reproduce_violin_plot_COMPLETENESS()
reproduce_violin_plot_COMPLETENESS <- function(){
  plotViolin(reproducerTaskGra::data_experiment, "TASK_GRA", "COR")
}

#' Reproduce bar plot for participants' demographics data
#'
#' @return an object of class ggplot containing the bar plot for participants'
#' demographics data
#'
#' @export
#'
#' @examples reproduce_barplot_participants()
reproduce_barplot_participants <- function(){
  surveyQuestions <- c("Prog.", "Java", "Unit Test.", "TDD", "JUnit", "Eclipse")
  responseScale <- c("None", "Novice", "Intermediate", "Expert")

  dataExperienceSurvey <- reproducerTaskGra::data_experiment %>%
    dplyr::select("ParticipantID", contains("EXP")) %>%
    dplyr::distinct() %>%
    dplyr::mutate_at(vars(contains("EXP")), funs(factor(., levels=responseScale, ordered = TRUE))) %>%
    dplyr::rename("Prog." = .data$EXP_LEVEL_PROGRAMMING) %>%
    dplyr::rename(Java = .data$EXP_LEVEL_JAVA) %>%
    dplyr::rename("Unit Test." = .data$EXP_LEVEL_UNIT_TESTING) %>%
    dplyr::rename(TDD = .data$EXP_LEVEL_TDD) %>%
    dplyr::rename(JUnit = .data$EXP_LEVEL_JUNIT) %>%
    dplyr::rename("Eclipse" = .data$EXP_LEVEL_ECLIPSE)

  freqTableExperienceSurvey <- dataExperienceSurvey %>%
    dplyr::select(.data$ParticipantID, surveyQuestions) %>%
    tidyr::gather("Question", "Response", -.data$ParticipantID) %>%
    dplyr::count(.data$Question, .data$Response) %>%
    dplyr::mutate(Percentage = 100*.data$n/nrow(dataExperienceSurvey))  %>%
    dplyr::mutate(Response = factor(.data$Response, levels=responseScale, ordered = TRUE)) %>%
    dplyr::mutate(Question = factor(.data$Question, levels=surveyQuestions, ordered = TRUE))

  plot_experience_survey <- ggplot2::ggplot(data = freqTableExperienceSurvey,
                                            aes_string(x = "Question", y = "Percentage",
                                                fill = "Response")) +
    ggplot2::theme_set(theme_minimal()) +
    ggplot2::geom_bar(stat = "identity",
                      position = position_dodge2(width = 0.7, preserve = "single")) +
    ggplot2::scale_y_continuous(limits=c(0,90), breaks = seq(0, 90,10),
                                labels = c(seq(0, 90,10)), expand = c(0,1)) +
    ggplot2::geom_text(position = position_dodge2(width = 0.9, preserve = "single"),
                       aes(y=Percentage+2, label=Response, hjust=0),
                       angle=90, size=3) +
    ggplot2::scale_fill_brewer(palette="OrRd") +
    ggplot2::theme(axis.text.x = element_text(size=8),
                   axis.text.y = element_text(size=8),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   legend.title=element_blank(),
                   legend.position = "none",
                   aspect.ratio = 1/1)


  #ylab("Percent of Responses")
  plot_experience_survey
}
