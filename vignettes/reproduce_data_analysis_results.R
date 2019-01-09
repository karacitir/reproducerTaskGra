## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## ------------------------------------------------------------------------
library(reproducerTaskGra)

## ----participant_demographics, fig.cap = "Figure 1"----------------------
  reproduce_barplot_participants()

## ----descriptive_stats_COR-----------------------------------------------
knitr::kable(reproduce_descriptive_stats_CORRECTNESS())

## ---- fig.cap = "Figure X"-----------------------------------------------
  reproduce_violin_plot_CORRECTNESS()

## ----descriptive_stats_COMP----------------------------------------------
knitr::kable(reproduce_descriptive_stats_COMPLETENESS())

## ---- fig.cap = "Figure X"-----------------------------------------------
  reproduce_violin_plot_COMPLETENESS()

