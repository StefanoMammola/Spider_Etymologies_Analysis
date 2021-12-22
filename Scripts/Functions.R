## ------------------------------------------------------------------------
# 'Useful function and parameters'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)

# Functions ---------------------------------------------------------------

## Function for calculating the standard error
std <- function(x) sd(x)/sqrt(length(x))

# Custom function to predict logistic regressions
logisticline <- function(z,model) {
  eta <- model$coefficients[1] + model$coefficients[2]*z ;
  1 / (1 + exp(-eta))
}

logisticline_min <- function(z,model) {
  eta <- model$coefficients[1] + model$coefficients[2]*z - 1.96*summary(model)$coefficients[2] ;
  1 / (1 + exp(-eta))
}

logisticline_max <- function(z,model) {
  eta <- model$coefficients[1] + model$coefficients[2]*z + 1.96*summary(model)$coefficients[2] ;
  1 / (1 + exp(-eta))
}


# Plot parameters ---------------------------------------------------------

## ggplot2 custom themes
theme_year <- theme(
  legend.position = c(0.3, 0.7),
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size=10),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  panel.grid = element_blank(),
  plot.caption = element_text(size = 10, color = "gray50"),
  plot.title = element_text(face="bold", size=12)
)