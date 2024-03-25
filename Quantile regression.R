

source(file = "~/work/Stat_app/DATA/exporting_data_from_link.R")

#Packages :
install.packages("dplyr")
install.packages("ggplot2")
install.packages("labelled")
install.packages("quantreg")
library("dplyr")
library("ggplot2")
library("labelled")
library(quantreg)

# Add this package for %>%
library("magrittr")


# Install and load the quantreg package if not already installed

library(quantreg)


# Fit conditional quantile regression
fit_conditional <- rq(hwactual ~ sex, data = eulfs_small, tau = 0.1) #add robustness ?
#I took the variable sex as an exemple, just to see
# Values seem all meaningless for tau = 0.9 ...

# Summary of the model with additional information
summary_conditional <- summary.rq(fit_conditional)

# Print summary including confidence intervals and p-values
print(summary_conditional, digits = 4) 
# no IC, F stat, R-squared ... don't know yet how to make them appear



"
Trying to make the regression for different values for tau in a single regression

data_filtered <- eulfs_small[!is.na(eulfs_small$hwactual), ]


# Define a sequence of tau values from 0.1 to 0.9
tau_values <- seq(0.1, 0.9, by = 0.1)

# List to store the models
models <- list()

# Loop to fit the models for different quantiles
for (tau in tau_values) {
  model <- rq(hwactual ~ sex, data = data_filtered, tau = tau, method = "fn")
  models[[as.character(tau)]] <- model
}

# Print the results for each quantile
for (tau in tau_values) {
  cat("Tau =", tau, "\n")
  summary(models[[as.character(tau)]])
  cat("\n")
}

"


#For a single value for tau, selectioning the years 1998 and 2013



# Loop to fit the models for each year
for (year in c(1998, 2013)) {
  # Filter data for the specific year
  data_year <- subset(data_filtered, year == year)
  
  # Fit the models
 regyear <- rq(hwactual ~ sex, data = data_year, tau = 0.3) #add robustness ?
}

# Print the results for each year
for (year in c(1998, 2013)) {
    cat("Year =", year, "\n")
    summary(regyear[[paste0(year)]])
    cat("\n")
}

