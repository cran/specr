## ---- message = F, warnings = F-----------------------------------------------
library(specr)
library(ggplot2)
library(dplyr)

# run spec analysis
results <- run_specs(example_data,
                     y = c("y1", "y2"),
                     x = c("x1", "x2"),
                     model = "lm",
                     controls = c("c1", "c2"),
                     subset = list(group1 = unique(example_data$group1),
                                   group2 = unique(example_data$group2)))

## ---- message = F, warnings = F-----------------------------------------------
# Package to estimate multilevel models
library(lme4)

# Estimate model
m1 <- lmer(estimate ~ 1 + (1|x) + (1|y) + (1|controls) + (1|subsets), data = results)

# Check model summary
summary(m1)

## ---- message = F, warnings = F-----------------------------------------------
icc_specs(m1) %>%
  mutate_if(is.numeric, round, 2)

## ---- message = F, warnings = F-----------------------------------------------
plot_variance(m1) +
  ylim(0, 100)

## ----message = F, warnings = F------------------------------------------------
m2 <- lmer(estimate ~ 1 + (1|x) + (1|y) + (1|controls) + (1|subsets) + (1|x:y) + (1|y:controls) + (1|y:subsets), data = results)

# Get table
icc_specs(m2) %>%
  mutate_if(is.numeric, round, 2)

# Plot results
plot_variance(m2)

## -----------------------------------------------------------------------------
m3 <- lmer(p.value ~ 1 + (1|x) + (1|y) + (1|controls) + (1|subsets), data = results)

icc_specs(m3) %>%
  mutate_if(is.numeric, round, 2)

plot_variance(m3)

