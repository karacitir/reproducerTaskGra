## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
fig.align = "center"
)

## ------------------------------------------------------------------------
library(reproducerTaskGra)

## ----participant_demographics, fig.cap = "Fig. 1: Summary of demographic survey for participants"----
reproduce_barplot_participants()

## ----descriptive_stats_COR-----------------------------------------------
knitr::kable(reproduce_descriptive_stats_CORRECTNESS())

## ---- fig.cap = "Fig. 2. Violin plots for CORRECTNESS at both levels of Task Description Granularity. Data points are indicated as dots grouped into bins of width 5. Horizontal lines within the plots indicate 0.25, 0.50 and 0.75 quantiles."----
reproduce_violin_plot_CORRECTNESS()

## ---- fig.width = 8, fig.cap = "Fig. 3. Boxplots for CORRECTNESS shown separately on different tasks and sequences. Data points are indicated as dots grouped into bins of width 5. Horizontal lines within the plots indicate 0.25, 0.50 and 0.75 quantiles"----
reproduce_boxplot_CORRECTNESS()

## ------------------------------------------------------------------------
reproduce_lmm_CORRECTNESS()
knitr::kable(reproduce_estimates_CORRECTNESS(), caption = "Estimates of fixed effects for CORRECTNESS")

## ------------------------------------------------------------------------
knitr::kable(reproduce_hypothesis_testing_CORRECTNESS(), caption = "Tests of fixed effects for CORRECTNESS")

## ------------------------------------------------------------------------
knitr::kable(reproduce_confidence_intervals_CORRECTNESS(), caption = "Confidence intervals for estimates of fixed effects for CORRECTNESS")

## ------------------------------------------------------------------------
knitr::kable(reproduce_model_fit_statistics_CORRECTNESS(), caption = "Model fit statistics for CORRECTNESS")

## ----descriptive_stats_COMP----------------------------------------------
knitr::kable(reproduce_descriptive_stats_COMPLETENESS())

## ---- fig.cap = "Fig. 5. Violin plots for COMPLETENESS at both levels of Task Description Granularity. Data points are indicated as dots grouped into bins of width 5. Horizontal lines within the plots indicate 0.25, 0.50 and 0.75 quantiles."----
reproduce_violin_plot_COMPLETENESS()

## ---- fig.cap = "Fig. 6. Boxplots for COMPLETENESS shown separately on different tasks and sequences. Data points are indicated as dots grouped into bins of width 5. Horizontal lines within the plots indicate 0.25, 0.50 and 0.75 quantiles"----
reproduce_boxplot_COMPLETENESS()

## ------------------------------------------------------------------------
reproduce_lmm_COMPLETENESS()
knitr::kable(reproduce_estimates_COMPLETENESS(), caption = "Estimates of fixed effects for COMPLETENESS")

## ------------------------------------------------------------------------
knitr::kable(reproduce_hypothesis_testing_COMPLETENESS(), caption = "Tests of fixed effects for COMPLETENESS")

## ------------------------------------------------------------------------
knitr::kable(reproduce_confidence_intervals_COMPLETENESS(), caption = "Confidence intervals for estimates of fixed effects for COMPLETENESS")

## ------------------------------------------------------------------------
knitr::kable(reproduce_model_fit_statistics_COMPLETENESS(), caption = "Model fit statistics for COMPLETENESS")

