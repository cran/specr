## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(specr)

# Set up specific lm function
linear <- function(formula, data) {
  pb$tick()
  glm(formula = formula, data = data, family = gaussian(link = "identity"))
}

## ---- message = F, warnings = F-----------------------------------------------
library(progress)

# Provide number of ticks
pb <- progress_bar$new(format = "[:bar] | :percent (:current/:total) | Finished in:eta",
                       width = 100,
                       total = 192)

## -----------------------------------------------------------------------------
# run spec analysis
results <- run_specs(example_data,
                     y = c("y1", "y2"),
                     x = c("x1", "x2"),
                     model = "linear",      # use customized function
                     controls = c("c1", "c2"),
                     subset = list(group1 = unique(example_data$group1),
                                   group2 = unique(example_data$group2)))


## ---- eval=F------------------------------------------------------------------
#  [==========================================================-------------]  81% (156/192) Finished in 1s

