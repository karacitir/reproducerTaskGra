#' Reproduce the LMM model for Correctness
#'
#' @return an object of class nlme::lme which represents the linear mixed-effects model fit
#' for CORRECTNESS ~ TASK_GRA + TASK + (1|ParticipantID)
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS()
reproduce_lmm_CORRECTNESS <- function() {
  lmm_CORRECTNESS <- nlme::lme(sqrt(COR) ~ TASK_GRA + TASK, data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID, method = "ML")
  lmm_CORRECTNESS
}

#' Reproduce the estimates of fixed effects for CORRECTNESS
#'
#' @return a data frame which contains the estimates of fixed effects Task and Task Description Granularity
#' obtained with the the linear mixed-effects model fit for Correctness
#' @export
#'
#' @examples reproduce_estimates_CORRECTNESS()
reproduce_estimates_CORRECTNESS <- function() {
  # reproduce the lmm for Correctness
  lmm_CORRECTNESS <- reproduce_lmm_CORRECTNESS()
  # Extract the estimates of fixed effects from the summary of the lmm for Correctness
  as.data.frame(summary(lmm_CORRECTNESS)[["tTable"]])
}

#' Reproduce the hypothesis tests of fixed effects for Correctness
#'
#' @return a data frame with numerator degrees of freedom, denominator degrees of freedom,
#' F-values, and P-values for Wald tests for the fixed effects in the model
#' @export
#'
#' @examples reproduce_hypothesis_testing_CORRECTNESS()
reproduce_hypothesis_testing_CORRECTNESS <- function() {
  # reproduce the lmm for Correctness
  lmm_CORRECTNESS <- reproduce_lmm_CORRECTNESS()
  # Obtain the Wald test statistcs using anova.lme
  nlme::anova.lme(lmm_CORRECTNESS)
}

#' Reproduce the model fit statistics for the linear-mixed model which represents
#' the linear mixed-effects model fit for CORRECTNESS
#'
#' @return a dataframe containing marginal R-squared (R2m), conditional R-squared (R2c),
#' Akaike's Information Criterion (AIC), Schwarz's Bayesian criterion (BIC), and
#' log-likelihood values
#' @export
#'
#' @examples reproduce_model_fit_statistics_CORRECTNESS()
reproduce_model_fit_statistics_CORRECTNESS <- function() {
  # reproduce the lmm for Correctness
  lmm_CORRECTNESS <- reproduce_lmm_CORRECTNESS()
  # compute model fit statistics
  model_fit_R2m <- MuMIn::r.squaredGLMM(lmm_CORRECTNESS)[1]
  model_fit_R2c <- MuMIn::r.squaredGLMM(lmm_CORRECTNESS)[2]
  model_fit_AIC <- stats::AIC(lmm_CORRECTNESS)
  model_fit_BIC <- stats::BIC(lmm_CORRECTNESS)
  model_fit_LL <- stats::logLik(lmm_CORRECTNESS)
  # return the statistics as a list
  model_fit_statistics <- list("R2m" = model_fit_R2m,
                               "R2c" = model_fit_R2c,
                               "AIC" = model_fit_AIC,
                               "BIC" = model_fit_BIC,
                               "LL"= model_fit_LL)
  model_fit_statistics
}

#' Reproduce the LMM model for Completeness
#'
#' @return an object of class nlme::lme which represents the linear mixed-effects model fit
#' for COMPLETENESS ~ TASK_GRA + TASK + (1|ParticipantID)
#' @export
#'
#' @examples reproduce_lmm_COMPLETENESS()
reproduce_lmm_COMPLETENESS <- function() {
  lmm_COMPLETENESS <- nlme::lme(asinTransform(COMP) ~ TASK_GRA + TASK, data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID, method = "ML")
  lmm_COMPLETENESS
}

#' Reproduce the estimates for fixed effects for Completeness
#'
#' @return a data frame which contains the estimates for fixed effects Task and Task Description Granularity
#' obtained with the the linear mixed-effects model fit for Completeness
#' @export
#'
#' @examples reproduce_estimates_COMPLETENESS()
reproduce_estimates_COMPLETENESS <- function() {
  lmm_COMPLETENESS <- reproduce_lmm_COMPLETENESS()
  as.data.frame(summary(lmm_COMPLETENESS)[["tTable"]])
}

#' Reproduce the hypothesis tests of fixed effects for Completeness
#'
#' @return a data frame with numerator degrees of freedom, denominator degrees of freedom,
#' F-values, and P-values for Wald tests for the fixed effects in the model for Completeness
#' @export
#'
#' @examples reproduce_hypothesis_testing_COMPLETENESS()
reproduce_hypothesis_testing_COMPLETENESS <- function() {
  lmm_COMPLETENESS <- reproduce_lmm_COMPLETENESS()
  nlme::anova.lme(lmm_COMPLETENESS)
}
