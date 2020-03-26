## ---- message = F, warnings = F-----------------------------------------------
library(specr)
library(dplyr)
library(ggplot2)
library(cowplot)

# run spec analysis
results <- run_specs(example_data,
                     y = c("y1", "y2"),
                     x = c("x1", "x2"),
                     model = "lm",
                     controls = c("c1", "c2"),
                     subset = list(group1 = unique(example_data$group1),
                                   group2 = unique(example_data$group2)))

## ---- message = F, warnings = F-----------------------------------------------
summarise_specs(results)

summarise_specs(results, x)

## ---- fig.height=10, fig.width=10, message=F, warning = F---------------------
plot_specs(results)

## ---- fig.height=10, fig.width=10, message=F, warning = F---------------------
plot_specs(results, 
           choices = c("x", "y", "controls", "subsets"),  # "model is not plotted
           rel_heights = c(1, 2))                         # changing relative heights

# Investigating specific contrasts
results %>%
  mutate(group1 = ifelse(grepl("group1 = 0", subsets), "0", "1"),
         group2 = ifelse(grepl("group2 = A", subsets), "A", "B & C")) %>%
  plot_specs(choices = c("x", "y", "controls", "group1", "group2"))

## ---- fig.height=10, fig.width=10, message=F, warning = F---------------------
# Plot specification curve
p1 <- plot_curve(results, 
                 ci = FALSE, 
                 ribbon = TRUE) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black") +
  ylim(-8, 12) +
  labs(x = "", y = "unstandarized regression coefficient") +
  theme_half_open()

# Plot choices
p2 <- plot_choices(results, 
                   choices = c("x", "y", "controls", "subsets")) +
  labs(x = "specifications (ranked)") +
  theme_half_open() +
  theme(strip.text.x = element_blank())

# Combine plots
plot_specs(plot_a = p1,
           plot_b = p2,
           labels = c("", ""),      # remove plot labels
           rel_height = c(2, 2.5))  # adjust relative heights

## ---- fig.height=10, fig.width=10, message=F, warning = F---------------------
p3 <- plot_samplesizes(results) +
  theme_half_open()

# Combine via cowplot
plot_grid(p1, p2, p3,
          ncol = 1,
          align = "v",
          rel_heights = c(1.5, 2, 0.8),
          axis = "rbl")

