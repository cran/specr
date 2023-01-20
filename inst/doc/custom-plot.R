## ---- message = F, warning = F------------------------------------------------
# Load packages
library(tidyverse)
library(specr)

# Setup specifications
specs <- setup(data = example_data,
               y = c("y1", "y2"),
               x = c("x1", "x2"),
               model = "lm",
               controls = c("c1", "c2"),
               subsets = list(group1 = c("young", "middle", "old"),
                              group2 = c("female", "male")))

# Summary of the specification setup
summary(specs)

## -----------------------------------------------------------------------------
# Running specification curve analysis 
results <- specr(specs)

## ---- message = F, warnings = F-----------------------------------------------
# Overall summary
summary(results)

## ---- message = F, warnings = F-----------------------------------------------
# Specific descriptive analysis of the curve, grouped by x and y
summary(results, 
        type = "curve", 
        group = c("x", "y"))

## ---- fig.height=8, fig.width=8, message=F, warning = F-----------------------
plot(results)

## ---- fig.height=8, fig.width=8, message=F, warning = F-----------------------
# Customizing plot
plot(results, 
     choices = c("x", "y", "controls", # model is not plotted
                 "group1", "group2"),  # subset split into original groups
     rel_heights = c(.75, 2))          # changing relative heights

# Investigating specific contrasts
results$data <- results$data %>%
  mutate(gender = ifelse(grepl("female", subsets), "female", 
                         ifelse(grepl("male", subsets), "male", "all")))

# New variable as "choice"
plot(results, 
     choices = c("x", "y", "gender"))

## ---- fig.height=8, fig.width=8, message=F, warning = F-----------------------
# Plot specification curve
p1 <- plot(results, 
           type = "curve",
           ci = FALSE, 
           ribbon = TRUE) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black") +
  labs(x = "", y = "unstd. coefficients")

# Plot choices
p2 <- plot(results, 
           type = "choices",
           choices = c("x", "y", "controls")) +
  labs(x = "specifications (ranked)")

# Combine plots (see ?plot_grid for possible adjustments)
plot_grid(p1, p2,
          ncol = 1,           
          align = "v",             # to align vertically
          axis = "rbl",            # align axes
          rel_heights = c(2, 2))   # adjust relative heights

## ---- fig.height=8.5, fig.width=8, message=F, warning = F---------------------
p3 <- plot(results, type = "samplesizes") 

# Add to overall plot
plot_grid(p1, p2, p3,
          ncol = 1,
          align = "v",
          axis = "rbl",
          rel_heights = c(1.5, 2, 0.8))

## ---- message = F, warning = F------------------------------------------------
# ALl choices
plot(results, 
     type = "boxplot") 

# Specific choices and further adjustments
plot(results, 
     type = "boxplot", 
     choices = c("x", "y", "controls")) +
  scale_fill_brewer(palette = 4)

plot(results, 
     type = "boxplot",
     choices = c("group1", "group2")) +
  scale_fill_manual(values = c("steelblue", "darkred"))

## -----------------------------------------------------------------------------
results %>%
  as_tibble %>%
  ggplot(aes(x = group1, y = estimate, fill = group2)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "age groups", fill = "gender")

