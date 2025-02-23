
#' Create post-stratification specific report section
create_post_strat_section <- function() {
  return('
## Post-Stratification Details

### Strata Analysis
```{r strata_analysis, echo=FALSE}
plots$strata_dist
```

### Coverage Assessment
```{r coverage, echo=FALSE}
plots$coverage_plot
```

### Strata Statistics
```{r strata_stats, echo=FALSE}
knitr::kable(summary_stats$method_statistics$strata_counts)
```
')
}

#' Create IPW specific report section
create_ipw_section <- function() {
  return('
## Inverse Probability Weighting Details

### Propensity Score Distribution
```{r propensity_dist, echo=FALSE}
plots$propensity_plot
```

### Balance Assessment
```{r balance, echo=FALSE}
plots$balance_plot
```

### Balance Statistics
```{r balance_stats, echo=FALSE}
knitr::kable(summary_stats$method_statistics$balance_measures)
```
')
}

#' Create calibration specific report section
create_calibration_section <- function() {
  return('
## Calibration Details

### Convergence Analysis
```{r convergence, echo=FALSE}
plots$convergence_plot
```

### Margin Adjustments
```{r margins, echo=FALSE}
plots$margin_plot
```

### Convergence Statistics
```{r conv_stats, echo=FALSE}
knitr::kable(summary_stats$method_statistics$convergence_stats)
```
')
}

#' Create RAKE specific report section
create_rake_section <- function() {
  return('
## RAKE Details

### Iteration Progress
```{r iterations, echo=FALSE}
plots$iteration_plot
```

### Margin Adjustments
```{r margin_adj, echo=FALSE}
plots$margin_adjustment
```

### Final Margin Statistics
```{r margin_stats, echo=FALSE}
knitr::kable(summary_stats$method_statistics$final_margins)
```
')
}

#' Create base report template
#' @param method Weighting method
#' @param method_section Method-specific markdown section
#' @param plots List of plots
#' @param summary_stats Summary statistics
#' @param params Method parameters
create_report_template <- function(method, method_section, plots, 
                                 summary_stats, params) {
  sprintf('---
title: "Survey Weighting Report (%s Method)"
date: "`r format(Sys.time(), \"%%Y-%%m-%%d %%H:%%M:%%S\")`"
output: 
  html_document:
    theme: cosmo
    toc: true
    toc_float: true
---

## Overview

### Method Parameters
```{r params, echo=FALSE}
knitr::kable(data.frame(
  Parameter = names(params),
  Value = unlist(params)
))
```

### Weight Distribution
```{r weight_dist, echo=FALSE}
plots$weight_dist
```

### Basic Statistics
```{r basic_stats, echo=FALSE}
knitr::kable(summary_stats$base_statistics)
```

%s

## Diagnostic Information
```{r diagnostics, echo=FALSE}
plots$diagnostic_plots
```
', method, method_section)
}
