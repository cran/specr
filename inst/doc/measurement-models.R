## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  #fig.path = "man/figures/README-",
  out.width = "100%",
  fig.retina = 2
)

## ---- message=F, warning = F--------------------------------------------------
# Load packages
library(tidyverse)
library(specr)
library(lavaan)

## ---- message=F, warning = F--------------------------------------------------
# Load data and recode
d <- HolzingerSwineford1939 %>% 
  mutate(sex = as.character(sex),
         school = as.character(school)) %>%
  as_tibble

# Check data
head(d)

## -----------------------------------------------------------------------------
# Model syntax
model <- "
  # measures
  visual  =~ x1 + x2 + x3

  # regressions
  visual ~ ageyr + grade
"

fit <- sem(model, d)
broom::tidy(fit)

## -----------------------------------------------------------------------------
sem_custom <- function(formula, data) {
  
  require(lavaan)
  
  # 1) Define latent variables as a named list
  latent <- list(visual =  "visual  =~ x1 + x2 + x3",
                 textual = "textual =~ x4 + x5 + x6",
                 speed =   "speed   =~ x7 + x8 + x9")
  
  # 2) Remove placeholder for no covariates (lavaan does not like "+ 1")
  formula <- str_remove_all(formula, "\\+ 1")
  
  # 3) Check which of the additional measurement models are actually used in the formula
  valid <- purrr::keep(names(latent), 
                       ~ stringr::str_detect(formula, .x))
  
  # 4) Include measurement models in the formula using lavaan syntax
  formula <- paste(formula, "\n",
                   paste(latent[valid], 
                         collapse = " \n "))
  
  # 5) Run SEM with sem function
  sem(formula, data)
}

# In short:
sem_custom <- function(formula, data) {
  require(lavaan)
  latent <- list(visual =  "visual  =~ x1 + x2 + x3",
                 textual = "textual =~ x4 + x5 + x6",
                 speed =   "speed   =~ x7 + x8 + x9")
  formula <- stringr::str_remove_all(formula, "\\+ 1")
  valid <- purrr::keep(names(latent), ~ stringr::str_detect(formula, .x))
  formula <- paste(formula, "\n", paste(latent[valid], collapse = " \n "))
  sem(formula, data)
}


## -----------------------------------------------------------------------------
# Setup specs
specs <- setup(data = d,
               y = c("textual", "visual", "speed"),
               x = c("ageyr"),
               model = c("sem_custom"),
               controls = c("grade"),
               subsets = list(sex = unique(d$sex),
                              school = unique(d$school)))

# Summarize specifications
summary(specs, row = 10)

## ---- fig.height=8, fig.width=8, message=F, warning = F-----------------------
results <- specr(specs)
plot(results, choices = c("y", "controls", "model", "subsets"))

## ---- fig.height=9, fig.width=9, message=F, warning = F-----------------------
# Create curve with standardized coefficients
plot_a <- plot(results, "curve") + 
    geom_point(aes(y = std.all, alpha = .1, size = 1.25)) +
    geom_hline(yintercept = 0, linetype = "dashed")

# Choice panel
plot_b <- plot(results, "choices",
               choices = c("y", "controls", "subsets"))

# Combine plots
plot_grid(plot_a, plot_b,
          ncol = 1,           
          align = "v",            
          axis = "rbl",            
          rel_heights = c(1.5, 2))  

## ---- fig.height=8, fig.width=8, message=F, warning = F-----------------------
# Looking at included fit indices
results %>%
  as_tibble %>%
  select(x, y, model, controls, subsets, 
         fit_cfi, fit_tli, fit_rmsea) %>%
  head

# Create curve plot
p1 <- plot(results, "curve", var = fit_cfi, ci = FALSE) +
  geom_line(aes(x = specifications, y = fit_cfi), color = "grey") +
  geom_point(size = 2, shape = 18) + # increasing size of points
  geom_line(aes(x = specifications, y = fit_rmsea), color = "grey") +
  geom_point(aes(x = specifications, y = fit_rmsea), shape = 20, size = 2) +
  ylim(0, 1) +
  labs(y = "cfi & rmsea")

# Create choice panel with chisq arrangement
p2 <- plot(results, "choices", var = fit_cfi, 
           choices = c("y", "controls", "subsets"))

# Bind together
plot_grid(p1, p2,
          ncol = 1,           
          align = "v",            
          axis = "rbl",            
          rel_heights = c(1.5, 2))

