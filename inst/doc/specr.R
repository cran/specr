## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  #fig.path = "man/figures/README-",
  out.width = "100%",
  fig.retina = 2
)

## ---- message=F, warning = F--------------------------------------------------
# Load libraries
library(specr)
library(tidyverse)

## ---- message=F, warning = F--------------------------------------------------
# We have a look at the simulated data set that is included in the package
head(example_data)

# Summary of the data set
summary(example_data)

## ---- message=F, warning = F--------------------------------------------------
setup_specs(y = c("y1"),               # We choose only one dependent variale
            x = c("x1", "x2"),         # We are not sure which independent variable is better
            model = c("lm"),           # We only estimate one type of model (linear model)
            controls = c("c1", "c2"))  # We include two control variable 

## ---- message=F, warning = F--------------------------------------------------
# specific model fitting function
lm_gauss <- function(formula, data) {
  glm(formula = formula, 
      data = data, 
      family = gaussian(link = "identity"))
}

# Run specification curve analysis
results <- run_specs(df = example_data, 
                     y = c("y1", "y2"), 
                     x = c("x1", "x2"), 
                     model = c("lm", "lm_gauss"), 
                     controls = c("c1", "c2"), 
                     subsets = list(group1 = unique(example_data$group1),
                                    group2 = unique(example_data$group2)))

# Check
results

## ---- fig.height=6, fig.width=10, message=F, warning = F----------------------
plot_decisiontree(results, 
                  legend = TRUE)

## ---- message=F, warning = F--------------------------------------------------
# basic summary of the entire specification curve
summarise_specs(results)

# summary by specific groups and  statistics
summarise_specs(results,                         # result data frame
                x, y,                            # grouping variables
                stats = lst(median, min, max))   # specific functions

# summary of another statistic
summarise_specs(results, 
                subsets,
                var = p.value)

## ---- fig.height=10, fig.width=10, message=F, warning = F---------------------
# Plot specification curve analysis
plot_specs(results)

## ---- fig.height=7, fig.width=7, message=F, warning = F-----------------------
plot_summary(results)

## ---- fig.height=3.5, fig.width=8, message=F, warning = F---------------------
# Estimate multilevel model 
library(lme4)
model <- lmer(estimate ~ 1 + (1|x)  + (1|y) + (1|controls) + (1|subsets), data = results)

# Get intra-class correlation
icc_specs(model) %>%
  mutate_if(is.numeric, round, 2)

# Plot decomposition
plot_variance(model)

