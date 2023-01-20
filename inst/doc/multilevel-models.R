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
library(gapminder)
library(lme4)

# Recode some variables
gapminder <- gapminder %>%
  mutate(gdpPercap_log = log(gdpPercap),
         pop = pop/1000)

# Check data
head(gapminder)

## -----------------------------------------------------------------------------
specs <- setup(data = gapminder,
                y = c("lifeExp"),
                x = c("gdpPercap_log"), 
                model = c("lmer"),
                controls = "pop",
                fun1 = function(x) broom.mixed::tidy(x, conf.int = TRUE),
                add_to_formula = "(1|country) + (1|year)")

# Check formula
summary(specs)

# Run analysis and inspect results
results <- specr(specs)
as_tibble(results)

## -----------------------------------------------------------------------------
# Random intercept model (only country as grouping variable)
lmer_ri_1 <- function(formula, data,...) {
  require(lme4)
  require(broom.mixed)
  formula <- paste(formula, "+ (1|country)")
  lmer(formula, data)
}

# Including random slopes (only country as grouping variable)
lmer_rs_1 <- function(formula, data,...) {
  require(lme4)
  require(broom.mixed)
  slopevars <- unlist(strsplit(formula, " ~ "))[2]
  formula <- paste0(formula, "+ (1 + ", slopevars, "|country)" )
  lmer(formula, data)
}

# Random intercept model (lifeExp is nested in both countries and years)
lmer_ri_2 <- function(formula, data,...) {
  require(lme4)
  require(broom.mixed)
  formula <- paste0(formula, "+ (1|country) + (1|year)")
  lmer(formula, data)
}

# Including random slopes (intercept and slopes are nested in both countries and years)
lmer_rs_2 <- function(formula, data,...) {
  require(lme4)
  require(broom.mixed)
  slopevars <- unlist(strsplit(formula, " ~ "))[2]
  formula <- paste0(formula, "+ (1 + ", slopevars, "|country) + (", slopevars, "|year)" )
  lmer(formula, data)
}

## ---- message=F, warning = F--------------------------------------------------
# Setup specifications with customized functions
specs <- setup(data = gapminder,
               y = c("lifeExp"),
               x = c("gdpPercap_log"), 
               model = c("lmer_ri_1", "lmer_ri_2", 
                         "lmer_rs_1", "lmer_rs_2"),
               controls = "pop")

# Check specifications
summary(specs)

## ---- warning = F, message = F, fig.height=8, fig.width=8---------------------
# Run analysis and plot results
results <- specr(specs) 
plot(results)

