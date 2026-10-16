## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  #fig.path = "man/figures/README-",
  out.width = "100%",
  fig.retina = 2,
  # Evaluate the code only if the suggested packages are installed
  eval = requireNamespace("tidyverse", quietly = TRUE)
)

## ----message=F, warning = F---------------------------------------------------
# Load packages
library(specr)
library(tidyverse)

## -----------------------------------------------------------------------------
glimpse(example_data)

## -----------------------------------------------------------------------------
specs <- setup(data = example_data,
   y = "y1",
   x = c("x1", "x2"),
   model = "lm",
   controls = c("c1", "c2"),
   subsets = list(group2 = c("female", "male")))

results <- specr(specs)
summary(results)

## -----------------------------------------------------------------------------
set.seed(42)
inference <- infer_curve(results, specs, n_samples = 1000)
inference

## ----eval = FALSE-------------------------------------------------------------
# library(furrr)
# plan(multisession, workers = 4)
# inference <- infer_curve(results, specs, n_samples = 1000)
# plan(sequential)

## -----------------------------------------------------------------------------
summary(inference)

## ----include = FALSE----------------------------------------------------------
s <- summary(inference)

## -----------------------------------------------------------------------------
summary(inference, direction = "greater")

## -----------------------------------------------------------------------------
summary(inference, group = "x")

## ----fig.width=9, fig.height = 5, warning = F---------------------------------
plot(inference)

## -----------------------------------------------------------------------------
set.seed(1)
shuffled <- example_data
shuffled$y1 <- sample(shuffled$y1)

specs_null <- setup(data = shuffled,
   y = "y1",
   x = c("x1", "x2"),
   model = "lm",
   controls = c("c1", "c2"),
   subsets = list(group2 = c("female", "male")))

results_null <- specr(specs_null)

set.seed(42)
inference_null <- infer_curve(results_null, specs_null, n_samples = 1000)
summary(inference_null)

## ----fig.width=9, fig.height = 5, warning = F---------------------------------
plot(inference_null)

