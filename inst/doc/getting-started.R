## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  #fig.path = "man/figures/README-",
  out.width = "100%",
  fig.retina = 2
)

## ---- message=F, warning = F--------------------------------------------------
# Load library
library(specr)
library(ggplot2)

## ---- message=F, warning = F--------------------------------------------------
# We have a look at the simulated data set that is included in the package
head(example_data)

## ---- message=F, warning = F--------------------------------------------------
# specific model fitting function
lm_gaussian <- function(formula, data) {
  glm(formula = formula, 
      data = data, 
      family = gaussian(link = "identity"))
}

# Specifying the analytic decisions
specs <- setup(data = example_data,            # First, we provide the data set
               y = c("y1", "y2"),              # We choose only one dependent variable
               x = c("x1", "x2"),              # Two independent variables
               model = c("lm", "lm_gaussian"), # We estimate two types of models
               controls = c("c1", "c2"),       # We include two control variable 
               subsets = list(group1 = unique(example_data$group1),  # Subset 1
                              group2 = unique(example_data$group2))) # Subset 2
                      

# Overview of the specifications/multiverse
summary(specs, rows = 20)

## -----------------------------------------------------------------------------
plot(specs, circular = TRUE)

## ---- message=F, warning = F--------------------------------------------------
# Run specification curve analysis
results <- specr(specs)  

# Summarize results
summary(results)

## ---- message=F, warning = F--------------------------------------------------
# basic descriptive summary of the entire specification curve
summary(results, 
        type = "curve")

# basic descriptive summary per specific choices
summary(results, 
        type = "curve", 
        group = c("x", "controls"))  # group analysis by choices

# basic descriptive summary with other statistics
summary(results, 
        type = "curve", 
        group = "subsets", 
        stats = list(mean = mean, 
                     median = median))

## ---- fig.height=10, fig.width=10, message=F, warning = F---------------------
# Plot entire visualization
plot(results)

## ---- fig.height=5, fig.width=10, message=F, warning = F----------------------
# Plot 
plot(results, type = "curve", group = controls, desc = TRUE) +
  scale_color_brewer(palette = "Dark2") +
  theme_grey() +
  theme(legend.position = "none")

## ---- fig.height=7, fig.width=7, message=F, warning = F-----------------------
plot(results, type = "boxplot")

## ---- fig.height=3.5, fig.width=8, message=F, warning = F---------------------
plot(results, type = "variance")

