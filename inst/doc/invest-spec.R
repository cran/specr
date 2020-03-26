## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  #fig.path = "man/figures/README-",
  out.width = "100%",
  fig.retina = 2
)

## ---- message=F, warning = F--------------------------------------------------
library(specr)
library(dplyr)
library(purrr)
library(ggplot2)
library(performance)

# Run specification curve analysis
results <- run_specs(df = example_data, 
                     y = c("y1", "y2"), 
                     x = c("x1", "x2"), 
                     model = c("lm"), 
                     controls = c("c1", "c2"), 
                     subsets = list(group1 = unique(example_data$group1),
                                    group2 = unique(example_data$group2)),
                     keep.results = TRUE)

## -----------------------------------------------------------------------------
(y_models <- results %>%
  filter(x == "x1", 
         controls == "c1 + c2",
         subsets == "all"))

## -----------------------------------------------------------------------------
y_models %>%
  pull(res) %>%
  map(summary) %>%
  map(coef)

## -----------------------------------------------------------------------------
y_models %>%
  pull(res) %>% 
  map(r2)       # r2 is include in the package "performance"

## ---- fig.height=8, fig.width=10, message=F, warning = F----------------------
r2_results <- results %>%
  filter(subsets == "all") %>%
  mutate(r2 = map(res, r2), 
         r2 = map_dbl(r2, 1)) %>%
  arrange(r2)

r2_results %>%
  select(x:controls, r2)

## -----------------------------------------------------------------------------
r2_results %>%
  arrange(r2) %>%
  mutate(rank = 1:n()) %>%
  ggplot(aes(x = rank, 
             y = r2)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(strip.text = element_blank(),
        axis.line = element_line("black", size = .5),
        axis.text = element_text(colour = "black"))

