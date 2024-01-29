##### First analyse with Oaxaca -----

# Source:
# Hlavac, Marek (2022). oaxaca: Blinder-Oaxaca Decomposition in R. R package version 0.1.5. https://CRAN.R-project.org/package=oaxaca

install.packages("oaxaca")

# The explanatory variables are not necessarily sufficient here
# This is only a first attempt to use the package oaxaca
results <- eulfs_small %>% oaxaca(formula = hwactual ~ age + hatlev1d + YSTARTWK |sex)
