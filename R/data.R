#' Oulu Task Description Granularity Experiment Dataset
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 96 rows and 19 variables:
#' \describe{
#'   \item{ParticipantID}{Identifier assigned to the participant}
#'   \item{TASK}{The name of the implemented task: BSK or MR}
#'   \item{TASK_GRA}{The Task Description Granularity level at which the task was presented to the participant: coarser-graiend or finer-grained}
#'   \item{SEQUENCE}{The sequence that the participant was randomly assgined to: Sequence 1 or Sequence 2}
#'   \item{COR}{CORRECTNESS metric value of the software delivered by the participant}
#'   \item{COMP}{COMPLETENESS metric value of the software delivered by the participant}
#'   \item{TDD_CONF_BESOURO}{TDD Conformance score computed according  to the Besouro tool}
#'   \item{TDD_CONF_SELF}{Participant's self assessment of their TDD Conformance on a scale of 0 to 100}
#'   \item{EXP_PROGRAMMING}{Participant's experience on programming}
#'   \item{EXP_JAVA}{Participant's experience on Java}
#'   \item{EXP_PROGRAMMING_LEVEL}{Participant's self assessment of their experience level in programming (None, Novice, Intermediate, Expert)}
#'   \item{EXP_JAVA_LEVEL}{Participant's self assessment of their experience level in Java (None, Novice, Intermediate, Expert)}
#'   \item{EXP_ECLIPSE_LEVEL}{Participant's self assessment of their experience level in Eclipse IDE (None, Novice, Intermediate, Expert)}
#'   \item{EXP_UNIT_TESTING_LEVEL}{Participant's self assessment of their experience level in Unit Testing (None, Novice, Intermediate, Expert)}
#'   \item{EXP_JUNIT_LEVEL}{Participant's self assessment of their experience level in jUnit (None, Novice, Intermediate, Expert)}
#'   \item{EXP_TDD_LEVEL}{Participant's self assessment of their experience level in TDD (None, Novice, Intermediate, Expert)}
#'   \item{Difficulty}{Participant's assessment of the difficulty of the task (Trivial, Easy, Of average difficulty, Somewhat difficult, Difficult)}
#'   \item{Specification}{Participant's assessment of the adequacy of the task specification (Adequate, Somewhat adequate, Neither adequate, nor inadequate, Somewhat inadequate, Inadequate)}
#'   \item{Comprehensibility}{Participant's assessment of the comprehensibility of the task specification (Comprehensible, Somewhat Comprehensible, Neither Comprehensible, nor incomprehensible, Somewhat incomprehensible, Incomprehensible)}
#' }
#' @source \url{http://www.eatdd.oulu.fi}
"data_experiment"
