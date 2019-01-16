#' Reproduce the LMM model for Correctness
#'
#' @return an object of class nlme::lme which represents the linear mixed-effects model fit
#' for CORRECTNESS ~ TASK_GRA + TASK + (1|ParticipantID)
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS()
reproduce_lmm_CORRECTNESS <- function() {
  lmm_CORRECTNESS <- nlme::lme(sqrt(COR) ~ TASK_GRA + TASK,
                               data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                               method = "ML")
  lmm_CORRECTNESS
}

#' Reproduce alternative lmm model 1 for Correctness
#'
#' @return an object of class lme which represents the Model 1 for Correctness
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS_1()
reproduce_lmm_CORRECTNESS_1 <- function() {
  lmm_CORRECTNESS_X <- nlme::lme(sqrt(COR) ~ 1,
                                 data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                 method = "ML")
  lmm_CORRECTNESS_X
}

#' Reproduce alternative lmm model 2 for Correctness
#'
#' @return an object of class lme which represents the Model 2 for Correctness
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS_2()
reproduce_lmm_CORRECTNESS_2 <- function() {
  lmm_CORRECTNESS_X <- nlme::lme(sqrt(COR) ~ TASK_GRA,
                                 data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                 method = "ML")
  lmm_CORRECTNESS_X
}

#' Reproduce alternative lmm model 3 for Correctness
#'
#' @return an object of class lme which represents the Model 3 for Correctness
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS_3()
reproduce_lmm_CORRECTNESS_3 <- function() {
  lmm_CORRECTNESS_X <- nlme::lme(sqrt(COR) ~ TASK_GRA + TASK, data = reproducerTaskGra::data_experiment,
                                 random = ~ 1 | ParticipantID,
                                 method = "ML")
  lmm_CORRECTNESS_X
}

#' Reproduce alternative lmm model 4 for Correctness
#'
#' @return an object of class lme which represents the Model 4 for Correctness
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS_4()
reproduce_lmm_CORRECTNESS_4 <- function() {
  lmm_CORRECTNESS_X <- nlme::lme(sqrt(COR) ~ TASK_GRA + TASK + SEQUENCE,
                                 data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                 method = "ML")
  lmm_CORRECTNESS_X
}

#' Reproduce alternative lmm model 5 for Correctness
#'
#' @return an object of class lme which represents the Model 5 for Correctness
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS_5()
reproduce_lmm_CORRECTNESS_5 <- function() {
  lmm_CORRECTNESS_X <- nlme::lme(sqrt(COR) ~ TASK_GRA + TASK + EXP_PROGRAMMING,
                                 data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                 method = "ML")
  lmm_CORRECTNESS_X
}

#' Reproduce alternative lmm model 6 for Correctness
#'
#' @return an object of class lme which represents the Model 6 for Correctness
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS_6()
reproduce_lmm_CORRECTNESS_6 <- function() {
  lmm_CORRECTNESS_X <- nlme::lme(sqrt(COR) ~ TASK_GRA + TASK + EXP_JAVA, data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID, method = "ML")
  lmm_CORRECTNESS_X
}

#' Reproduce alternative lmm model 7 for Correctness
#'
#' @return an object of class lme which represents the Model 7 for Correctness
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS_7()
reproduce_lmm_CORRECTNESS_7 <- function() {
  lmm_CORRECTNESS_X <- nlme::lme(sqrt(COR) ~ TASK_GRA + TASK + EXP_JAVA + EXP_UNIT_TESTING_LEVEL,
                                 data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                 method = "ML")
  lmm_CORRECTNESS_X
}

#' Reproduce alternative lmm model 8 for Correctness
#'
#' @return an object of class lme which represents the Model 8 for Correctness
#' @export
#'
#' @examples reproduce_lmm_CORRECTNESS_8()
reproduce_lmm_CORRECTNESS_8 <- function() {
  lmm_CORRECTNESS_X <- nlme::lme(sqrt(COR) ~ TASK_GRA + TASK + EXP_JAVA + EXP_TDD_LEVEL,
                                 data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                 method = "ML")
  lmm_CORRECTNESS_X
}

#' Reproduce the estimates of fixed effects, standard errors and confidence intervals for CORRECTNESS
#'
#' @return a data frame which contains the estimates of fixed effects Task and Task Description Granularity
#' obtained with the the linear mixed-effects model fit for Correctness
#' @export
#'
#' @examples reproduce_estimates_CORRECTNESS()
reproduce_estimates_CORRECTNESS <- function() {
  # reproduce the lmm for Correctness
  lmm_CORRECTNESS <- reproduce_lmm_CORRECTNESS()
  # Get parameter estimates from tTable in summary.lme
  # append confidence intervals
  as.data.frame(cbind(summary(lmm_CORRECTNESS)[["tTable"]], intervals(lmm_CORRECTNESS)[["fixed"]])) %>%
    rownames_to_column(var="Parameter") %>%
    rename("Estimate" = .data$Value) %>%
    rename("SE" = .data$Std.Error) %>%
    mutate_if(is.numeric, round, digits=2) %>%
    mutate("95% CI" = paste("(", .data$lower, ", ", .data$upper, ")", sep="")) %>%
    select("Parameter", "Estimate", "SE", "95% CI")
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
  nlme::anova.lme(lmm_CORRECTNESS, type="marginal", adjustSigma = F)
}

#' Reproduce the confidence intervals for estimates of fixed effects for CORRECTNESS
#'
#' @return a data frame with parameter name, estimate of fixed effect, standard error and
#' 95% confidence interval limits
#' @export
#'
#' @examples reproduce_confidence_intervals_CORRECTNESS()
reproduce_confidence_intervals_CORRECTNESS <- function() {
  # reproduce the lmm for Correctness
  lmm_CORRECTNESS <- reproduce_lmm_CORRECTNESS()
  #obtain confidence intervals
  as.data.frame(cbind(summary(lmm_CORRECTNESS)[["tTable"]], intervals(lmm_CORRECTNESS)[["fixed"]])) %>%
    rownames_to_column(var = "Parameter") %>%
    rename(Estimate = .data$Value) %>%
    rename(SE = .data$Std.Error) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(`95% CI` = paste("(", .data$lower, ", ", .data$upper, ")", sep = "")) %>%
    select("Parameter", "Estimate", "SE", "95% CI")
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
  # compute model fit statistics and return the statistics as data.frame
  model_fit_statistics <- data.frame("R2m" = MuMIn::r.squaredGLMM(lmm_CORRECTNESS)[1],
                                     "R2c" = MuMIn::r.squaredGLMM(lmm_CORRECTNESS)[2],
                                     "AIC" = stats::AIC(lmm_CORRECTNESS),
                                     "BIC" = stats::BIC(lmm_CORRECTNESS),
                                     "LL"= stats::logLik(lmm_CORRECTNESS)) %>%
    mutate_at(vars(.data$R2m, .data$R2c), funs(round(. * 100, 2))) %>%
    mutate_all(funs(round(., 2)))

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

#' Reproduce alternative lmm model 1 for Completeness
#'
#' @return an object of class lme which represents the Model 1 for Completeness
#' @export
#'
#' @examples reproduce_lmm_COMPLETENESS_1()
reproduce_lmm_COMPLETENESS_1 <- function() {
  lmm_COMPLETENESS_X <- nlme::lme(asinTransform(COMP) ~ 1,
                                  data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                  method = "ML")
  lmm_COMPLETENESS_X
}

#' Reproduce alternative lmm model 2 for Completeness
#'
#' @return an object of class lme which represents the Model 2 for Completeness
#' @export
#'
#' @examples reproduce_lmm_COMPLETENESS_2()
reproduce_lmm_COMPLETENESS_2 <- function() {
  lmm_COMPLETENESS_X <- nlme::lme(asinTransform(COMP) ~ TASK_GRA,
                                  data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                  method = "ML")
  lmm_COMPLETENESS_X
}

#' Reproduce alternative lmm model 3 for Completeness
#'
#' @return an object of class lme which represents the Model 3 for Completeness
#' @export
#'
#' @examples reproduce_lmm_COMPLETENESS_3()
reproduce_lmm_COMPLETENESS_3 <- function() {
  lmm_COMPLETENESS_X <- nlme::lme(asinTransform(COMP) ~ TASK_GRA + TASK,
                                  data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                  method = "ML")
  lmm_COMPLETENESS_X
}

#' Reproduce alternative lmm model 4 for Completeness
#'
#' @return an object of class lme which represents the Model 4 for Completeness
#' @export
#'
#' @examples reproduce_lmm_COMPLETENESS_4()
reproduce_lmm_COMPLETENESS_4 <- function() {
  lmm_COMPLETENESS_X <- nlme::lme(asinTransform(COMP) ~ TASK_GRA + TASK + SEQUENCE,
                                  data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                  method = "ML")
  lmm_COMPLETENESS_X
}

#' Reproduce alternative lmm model 5 for Completeness
#'
#' @return an object of class lme which represents the Model 5 for Completeness
#' @export
#'
#' @examples reproduce_lmm_COMPLETENESS_5()
reproduce_lmm_COMPLETENESS_5 <- function() {
  lmm_COMPLETENESS_X <- nlme::lme(asinTransform(COMP) ~ TASK_GRA + TASK + EXP_PROGRAMMING,
                                  data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                  method = "ML")
  lmm_COMPLETENESS_X
}

#' Reproduce alternative lmm model 6 for Completeness
#'
#' @return an object of class lme which represents the Model 6 for Completeness
#' @export
#'
#' @examples reproduce_lmm_COMPLETENESS_6()
reproduce_lmm_COMPLETENESS_6 <- function() {
  lmm_COMPLETENESS_X <- nlme::lme(asinTransform(COMP) ~ TASK_GRA + TASK + EXP_JAVA,
                                  data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                  method = "ML")
  lmm_COMPLETENESS_X
}

#' Reproduce alternative lmm model 7 for Completeness
#'
#' @return an object of class lme which represents the Model 7 for Completeness
#' @export
#'
#' @examples reproduce_lmm_COMPLETENESS_7()
reproduce_lmm_COMPLETENESS_7 <- function() {
  lmm_COMPLETENESS_X <- nlme::lme(asinTransform(COMP) ~ TASK_GRA + TASK + EXP_JAVA + EXP_UNIT_TESTING_LEVEL,
                                  data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                  method = "ML")
  lmm_COMPLETENESS_X
}

#' Reproduce alternative lmm model 8 for Completeness
#'
#' @return an object of class lme which represents the Model 8 for Completeness
#' @export
#'
#' @examples reproduce_lmm_COMPLETENESS_8()
reproduce_lmm_COMPLETENESS_8 <- function() {
  lmm_COMPLETENESS_X <- nlme::lme(asinTransform(COMP) ~ TASK_GRA + TASK + EXP_JAVA + EXP_TDD_LEVEL,
                                  data = reproducerTaskGra::data_experiment, random = ~ 1 | ParticipantID,
                                  method = "ML")
  lmm_COMPLETENESS_X
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
  nlme::anova.lme(lmm_COMPLETENESS, type="marginal", adjustSigma = F)
}

#' Reproduce the confidence intervals for estimates of fixed effects for COMPLETENESS
#'
#' @return a data frame with parameter name, estimate of fixed effect, standard error and
#' 95% confidence interval limits
#' @export
#'
#' @examples reproduce_confidence_intervals_COMPLETENESS()
reproduce_confidence_intervals_COMPLETENESS <- function() {
  # reproduce the lmm for Correctness
  lmm_COMPLETENESS <- reproduce_lmm_COMPLETENESS()
  #obtain confidence intervals
  as.data.frame(cbind(summary(lmm_COMPLETENESS)[["tTable"]], intervals(lmm_COMPLETENESS)[["fixed"]])) %>%
    rownames_to_column(var = "Parameter") %>%
    rename(Estimate = .data$Value) %>%
    rename(SE = .data$Std.Error) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(`95% CI` = paste("(", .data$lower, ", ", .data$upper, ")", sep = "")) %>%
    select("Parameter", "Estimate", "SE", "95% CI")
}

#' Reproduce the model fit statistics for the linear-mixed model which represents
#' the linear mixed-effects model fit for COMPLETENESS
#'
#' @return a dataframe containing marginal R-squared (R2m), conditional R-squared (R2c),
#' Akaike's Information Criterion (AIC), Schwarz's Bayesian criterion (BIC), and
#' log-likelihood values
#' @export
#'
#' @examples reproduce_model_fit_statistics_COMPLETENESS()
reproduce_model_fit_statistics_COMPLETENESS <- function() {
  # reproduce the lmm for Correctness
  lmm_COMPLETENESS <- reproduce_lmm_COMPLETENESS()
  # compute model fit statistics and return the statistics as a data.frame
  model_fit_statistics <- data.frame("R2m" = MuMIn::r.squaredGLMM(lmm_COMPLETENESS)[1],
                                     "R2c" = MuMIn::r.squaredGLMM(lmm_COMPLETENESS)[2],
                                     "AIC" = stats::AIC(lmm_COMPLETENESS),
                                     "BIC" = stats::BIC(lmm_COMPLETENESS),
                                     "LL"= stats::logLik(lmm_COMPLETENESS)) %>%
    mutate_at(vars(.data$R2m, .data$R2c), funs(round(. * 100, 2))) %>%
    mutate_all(funs(round(., 2)))

  model_fit_statistics
}
