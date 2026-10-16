## ----include = FALSE----------------------------------------------------------
# Evaluate the code only if the suggested packages are installed
knitr::opts_chunk$set(eval = requireNamespace("tidyverse", quietly = TRUE))

## ----message = F, warning = F-------------------------------------------------
library(tidyverse)
library(specr)

## -----------------------------------------------------------------------------
# Setup specs
specs <- setup(data = example_data,
               x = c("x1", "x2", "x3", "x4"),
               y = c("y1", "y2", "y3"),
               model = "lm")

# Summary of specifications
summary(specs, rows = 12)

# Run analysis and plot results
results <- specr(specs)
head(results$data)

## -----------------------------------------------------------------------------
# Dichotomous dependent variable
data <- example_data %>%
  mutate(y_dich = ifelse(y1 > mean(y1), 1, 0))

# Specific function
log_glm <- function(formula, data) {
  glm(formula, data, family = binomial())
}

# Setup specs
specs <- setup(data = data,
               x = c("x1", "x2"),
               y = c("y1", "y2", "y3", "y_dich"),
               model = c("lm", "log_glm"))
# Check
specs %>%
  as_tibble

## -----------------------------------------------------------------------------
# Filter out models that are not meaningful (here only keep log_glm, when y == "y_dich")
specs$specs <- specs$specs %>%
  filter(!(model == "log_glm" & y != "y_dich")) %>%
  filter(!(model == "lm" & y == "y_dich"))

# Check results (only meaningful specifications remain)
summary(specs, rows = 8)

# Run analysis and plot results
results <- specr(specs)
head(results$data)

## ----warning = F, message = F, eval = requireNamespace("tidyverse", quietly = TRUE) && requireNamespace("lavaan", quietly = TRUE)----
# Add mean (one choice)
data <- data %>%
  mutate(x_mean = rowMeans(across(c(x1, x2, x3, x4))))

# Add custom function with latent measurement models to pass to "models" (another choice)
custom_sem <- function(formula, data) {
  
  # Add latent measurement as list
  latent <- list(latent_x12 = "latent_x12 =~ x1 + x2")
  
  # Remove +1 from formula as lavaan doesn't know how to process it 
  semformula <- str_remove_all(formula, "\\+ 1")
  
  # remove non-used latent measurement models from list by checking the formula
  valid <- purrr::keep(names(latent), ~ stringr::str_detect(formula, .x))
  
  # Create new formula that includes latent measurement models
  formula <- paste(semformula, "\n", paste(latent[valid], collapse = " \n "))
  
  # Pass formula to `sem()`
  lavaan::sem(formula, data)
}

# Create custom tidy function that extracts the same parameters from different models!
tidy_new <- function(x) {
  if(inherits(x, "lavaan")) {
    broom::tidy(x, conf.int = TRUE) %>% 
    select(term, estimate, conf.low, conf.high) %>%   # select parameters you want to keep
    filter(grepl(" ~ ", term)) %>%                    # term needs to be adjusted
    separate(term, c("dv", "term"), sep = " ~ ") %>%  # extract independent variable
    select(-dv)                                       # remove dependent variable
  } else {
    broom::tidy(x, conf.int = TRUE) %>% 
    select(term, estimate, conf.low, conf.high)      # same parameters as above
  }
}

# Setup specs with new custom function
specs <- setup(data = data,
               x = c("x1", "x2", "x3", "x4", "x_mean", "latent_x12"),
               y = c("y1", "y2"),
               model = c("lm", "custom_sem"),
               fun1 = tidy_new, # We pass the new extract function
               fun2 = NULL)     # switch off "glance" as it produces different fit indices and wouldn't work

# Quick check (still includes non-meaningful specifications)
summary(specs, rows = 12)

# Filter out non-meaningful specifications
specs$specs <- specs$specs %>%
  filter(!(model == "custom_sem" & !grepl("latent", x))) %>%
  filter(!(model == "lm" & grepl("latent", x)))

# Check again
summary(specs, rows = 12)

# Run analysis and plot results
results <- specr(specs)
plot(results, choices = c("x", "y"))

## -----------------------------------------------------------------------------
# Setup specification that include all combinations of covariates
specs1 <- setup(data = example_data,
               x = c("x1", "x2"),
               y = c("y1", "y2"),
               model = "lm",
               controls = c("c1", "c2", "c3", "c4"))  # simply providing a vector of control variables

# Setup specifications that include only no covariates, each individually, and all together
specs2 <- setup(data = example_data,
               x = c("x1", "x2"),
               y = c("y1", "y2"),
               model = "lm",
               controls = c("c1", "c2", "c3", "c4"),
               simplify = TRUE)   # Difference to specs1!

# Check
distinct(specs1$specs, controls)
distinct(specs2$specs, controls)

## -----------------------------------------------------------------------------
# Add groups of covariates
specs3 <- setup(data = example_data,
               x = c("x1", "x2"),
               y = c("y1", "y2"),
               model = "lm",
               controls = c("c1 + c2", "c3 + c4"))
# Check
distinct(specs3$specs, controls)

## -----------------------------------------------------------------------------
# Add some control variables to all models
specs4 <- setup(data = example_data,
               x = c("x1", "x2"),
               y = c("y1", "y2"),
               model = "lm",
               controls = c("c1", "c2"),
               add_to_formula = "c3")

# Check (see how `c3` is added to each formula, but is not part of controls)
specs4$specs[1:6,]

## -----------------------------------------------------------------------------
# Adding a covariate that is also an independent or dependent variable
specs5 <- setup(data = example_data,
               x = c("x1", "x2"),
               y = c("y1", "y2"),
               model = "lm",
               controls = c("x1", "y1"))

# Check (see how only 9 specifications are kept)
specs5$specs

## -----------------------------------------------------------------------------
# Setup specifications
specs <- setup(data = example_data,
               x = c("x1", "x2"),
               y = c("y1", "y2"),
               model = c("lm"),
               controls = "c1",
               subsets = list(group1 = unique(example_data$group1),
                              group2 = unique(example_data$group2)))

# Summary of specifications
summary(specs)

# Check subsets (in this case, 12 different types of subset analysis,
# including using "all" subjects)
distinct(specs$specs, subsets)

# Run analysis and plot results
results <- specr(specs)
plot(results, choices = c("x", "y", "subsets"))

## -----------------------------------------------------------------------------
# Create variables that denote outliers (here with a range of arbitrary thresholds)
data <- data %>%
  mutate(outlier1 = ifelse(y1 < mean(y1) - 2*sd(y1) | y1 > mean(y1) + 2*sd(y1), "outlier", "2.0*SD"),
         outlier2 = ifelse(y1 < mean(y1) - 2.1*sd(y1) | y1 > mean(y1) + 2.1*sd(y1), "outlier", "2.1*SD"),
         outlier3 = ifelse(y1 < mean(y1) - 2.2*sd(y1) | y1 > mean(y1) + 2.2*sd(y1), "outlier", "2.2*SD"),
         outlier4 = ifelse(y1 < mean(y1) - 2.3*sd(y1) | y1 > mean(y1) + 2.3*sd(y1), "outlier", "2.3*SD"),
         outlier5 = ifelse(y1 < mean(y1) - 2.4*sd(y1) | y1 > mean(y1) + 2.4*sd(y1), "outlier", "2.4*SD"),
         outlier6 = ifelse(y1 < mean(y1) - 2.5*sd(y1) | y1 > mean(y1) + 2.5*sd(y1), "outlier", "2.5*SD"))

# Setup specs
specs <- setup(data = data,
               x = c("x1", "x2"),
               y = c("y1", "y2"),
               model = "lm",
               controls = c("c1", "c2"),
               subsets = list(outlier1 = c("2.0*SD"),
                              outlier2 = c("2.1*SD"),
                              outlier3 = c("2.2*SD"),
                              outlier4 = c("2.3*SD"),
                              outlier5 = c("2.4*SD"),
                              outlier6 = c("2.5*SD")))

# Remove unnecessary combinations
specs$specs <- specs$specs %>%
  filter(subsets == "2.0*SD" | subsets == "2.1*SD" | 
         subsets == "2.2*SD" | subsets == "2.3*SD" |
         subsets == "2.4*SD" | subsets == "2.5*SD" |  
         subsets == "all")

# Check specifications (see how it contains only meaningful subsets?)
summary(specs, rows = 7)

# Run analysis and plot results
results <- specr(specs)
plot(results, choices = c("x", "y", "subsets"))

## -----------------------------------------------------------------------------
# Simulate weights
set.seed(42)
data_weights <- example_data %>%
  mutate(weight = runif(n(), min = .5, max = 1.5))

# Custom model function
lm_weighted <- function(formula, data) {
  lm(formula, data = data, weights = weight)
}

# Setup specs
specs <- setup(data = data_weights,
               x = c("x1", "x2"),
               y = "y1",
               model = list(unweighted = lm, weighted = lm_weighted),
               controls = c("c1", "c2"))

# Run analysis and plot results
results <- specr(specs)
plot(results, choices = c("x", "model", "controls"))

## ----eval = requireNamespace("tidyverse", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)----
library(survival)

# Deaths in the data set `colon` from the package survival
colon_death <- colon %>%
  filter(etype == 2)

# Custom model function
cox_model <- function(formula, data) {
  coxph(as.formula(formula), data = data)
}

# Setup specs
specs <- setup(data = colon_death,
               x = c("age", "nodes"),
               y = "Surv(time, status)",
               model = list(cox = cox_model),
               controls = c("sex", "obstruct", "perfor"),
               fun1 = function(x) broom::tidy(x, exponentiate = TRUE, conf.int = TRUE))

# Run analysis and plot results (hazard ratios)
results <- specr(specs)
plot(results, null = 1, choices = c("x", "controls"))

## ----eval = requireNamespace("fixest", quietly = TRUE)------------------------
# # Custom model function (fixed effects for group3 and heteroskedasticity-robust
# # standard errors; with real panel data, they can be clustered e.g. via `cluster = ~id`)
# feols_fe <- function(formula, data) {
#   fixest::feols(as.formula(paste(formula, "| group3")),
#                 vcov = "hetero",
#                 data = data)
# }
# 
# # Setup specs
# specs <- setup(data = example_data,
#                x = c("x1", "x2"),
#                y = c("y1", "y2"),
#                model = list(ols = lm, fixed_effects = feols_fe),
#                controls = c("c1", "c2"),
#                fun2 = NULL)  # different fit indices for lm and feols
# 
# # Run analysis and plot results
# results <- specr(specs)
# plot(results, choices = c("x", "y", "model", "controls"))

## ----eval = FALSE-------------------------------------------------------------
# specs <- setup(data = example_data,
#                x = c("x1", "x2"),
#                y = c("y1", "y2"),
#                model = list(lmer = lmerTest::lmer),
#                controls = "c1",
#                add_to_formula = "(1|group3)",
#                fun1 = function(x) broom.mixed::tidy(x, conf.int = TRUE),
#                fun2 = NULL)
# 
# results <- specr(specs)
# plot(results)

