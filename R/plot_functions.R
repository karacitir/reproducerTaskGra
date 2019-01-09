#' Reproduce violin plot for Correctness
#'
#' @return an object of class ggplot containing the violin plot of Correctness
#' @export
#'
#' @examples reproduce_violin_plot_CORRECTNESS()
reproduce_violin_plot_CORRECTNESS <- function(){
  create_violin_plot(reproducerTaskGra::data_experiment, "TASK_GRA", "COR")
}

#' Reproduce violin plot for Completeness
#'
#' @return an object of class ggplot containing the violin plot of Completeness
#' @export
#'
#' @examples reproduce_violin_plot_COMPLETENESS()
reproduce_violin_plot_COMPLETENESS <- function(){
  create_violin_plot(reproducerTaskGra::data_experiment, "TASK_GRA", "COMP")
}

#' Reproduce box plot for Correctness
#'
#' @return an object of class ggplot containing the box plot of Correctness
#' @export
#'
#' @examples reproduce_boxplot_CORRECTNESS()
reproduce_boxplot_CORRECTNESS <- function() {
  p1 <- create_boxplot(reproducerTaskGra::data_experiment, "TASK_GRA", "COR", "TASK")
  p2 <- create_boxplot(reproducerTaskGra::data_experiment, "TASK_GRA", "COR", "SEQUENCE")
  grid.arrange(p1, p2, ncol = 2, nrow = 1)
}

#' Reproduce box plot for Completeness
#'
#' @return an object of class ggplot containing the box plot of Completeness
#' @export
#'
#' @examples reproduce_boxplot_COMPLETENESS()
reproduce_boxplot_COMPLETENESS <- function() {
  create_boxplot(reproducerTaskGra::data_experiment, "TASK_GRA", "COMP", "TASK")
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
    dplyr::rename("Prog." = .data$EXP_PROGRAMMING_LEVEL) %>%
    dplyr::rename(Java = .data$EXP_JAVA_LEVEL) %>%
    dplyr::rename("Unit Test." = .data$EXP_UNIT_TESTING_LEVEL) %>%
    dplyr::rename(TDD = .data$EXP_TDD_LEVEL) %>%
    dplyr::rename(JUnit = .data$EXP_JUNIT_LEVEL) %>%
    dplyr::rename("Eclipse" = .data$EXP_ECLIPSE_LEVEL)

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
                       aes(y=freqTableExperienceSurvey$Percentage+2, label=freqTableExperienceSurvey$Response, hjust=0),
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

#### Generic functions
#' Create violin plot
#'
#' @param data_plot a dataframe containing the data for plot
#' @param x_variable name of the variable for x-axis
#' @param y_variable name of the variable for y-axis
#' @param ifStat Should the mean and mean values added to the plot
#'
#' @return a ggplot object containing the violin plot
#' @export
#' @examples create_violin_plot(get_data(), "TASK_GRA", "COR")
#' create_violin_plot(reproducerTaskGra::data_experiment, "TASK", "TDD_CONF_SELF")
create_violin_plot <- function(data_plot, x_variable, y_variable, ifStat=FALSE) {
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

#' Create box plot with facet
#'
#' @param data_plot a dataframe containing the data for plot
#' @param x_variable name of the factor for x-axis
#' @param y_variable name of the variable for y-axis
#' @param facet_variable name of the factor for creating facets
#' @param ifStat Should the mean and mean values added to the plot
#'
#' @return a ggplot object containing the violin plot
#' @export
#'
#' @examples create_boxplot(reproducerTaskGra::data_experiment, "TASK_GRA", "COR", "TASK")
create_boxplot <- function(data_plot, x_variable, y_variable, facet_variable, ifStat=FALSE) {
  plotLabels <- list(COR = "CORRECTNESS", COMP="COMPLETENESS", TASK="Task",
                     TASK_GRA="Task Description Granularity", CONF = "TDD-conformance",
                     TESTS="Number of Tests", EPISODES = "Number of Episodes",
                     QLTY = "Quality", EXP_JAVA = "Experience in Java")
  #theme_set(theme_custom_grey)
  h <- ggplot2::ggplot(data=data_plot, aes_string(x = x_variable, y = y_variable, color=x_variable)) +
    ggplot2::geom_boxplot(outlier.fill = NULL, outlier.shape = NA, varwidth = TRUE) +
    ggplot2::geom_dotplot(binaxis='y', binwidth=5, stackdir='center', dotsize = .2,
                          aes_string(fill=x_variable)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(size=8),
                   axis.text.y = element_text(size=8),
                   axis.title.x = element_text(size=8),
                   axis.title.y = element_text(size=8),
                   legend.position = "none") +
    ggplot2::xlab(plotLabels[[x_variable]]) + ggplot2::ylab(plotLabels[[y_variable]]) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 10), expand = c(0.01,0.05))

  if (ifStat) {
    h <- h + stat_summary(fun.y = "mean", geom = "text", size=2.5, color="black", aes(label = "mean"))
  }

  h <- h + facet_grid(paste(". ~ ", facet_variable), scales = "free_x")
  h
}

