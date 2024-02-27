# Source:
# Hlavac, Marek (2022). oaxaca: Blinder-Oaxaca Decomposition in R. R package version 0.1.5. https://CRAN.R-project.org/package=oaxaca

install.packages("oaxaca")

eulfs_small$hwactual <- na_if(eulfs_small$hwactual, 99)
eulfs_small$hwwish <- na_if(eulfs_small$hwwish, 99)


##### First analysis with Oaxaca -----

# The explanatory variables are not necessarily sufficient here
# This is only a first attempt to use the package oaxaca
eulfs_small_bis <- eulfs_small
eulfs_small_bis$log_sex <- as.logical(eulfs_small_bis$sex - 1)
# To have an indicator variable to distinguish two groups
# FALSE is for male, TRUE for female
# We treat the categorical variable degurba thanks to "as.factor"
results <- eulfs_small_bis %>% oaxaca::oaxaca(formula = hwactual ~ age + hatlev1d + as.factor(degurba) | log_sex)
# Relatively slow
# Other variables that may be relevant:
# numjob (hwactual only takes into account the main job), migstat (to distinguish immigrants from natives)...

results$n
# Among 86 482 observations, we have 46 603 men and 39 879 women

results$y
# The mean of the number of hours actually worked is 37.50 hours for men and 30.66 for women
# leaving a difference of approximately 6.84 hours to be explained by the Blinder-Oaxaca decomposition

results$threefold$overall
# This suggests that, of the 6.84 hours difference, 
# approximately -0.07 can be attributed to group differences in endowments (i.e., age, education, living in a city or not),
# 6.86 to differences in coefficients,
# and the remaining 0.05 is accounted for by the interaction of the two.

# We can visualize it thanks to a bar plot:
plot(results, components = c("endowments","coefficients"))

# We should now take time to select the most relevant variables before further interpretation


##### Second analysis with Oaxaca -----

eulfs_women <- filter(eulfs_small, eulfs_small$sex == 2, eulfs_small$marstat == 1 | eulfs_small$marstat == 2 )
eulfs_women$log_mar <- as.logical(eulfs_women$marstat - 1)
# Comparison between married and single women
results <- eulfs_women %>% oaxaca::oaxaca(formula = hwactual ~ age + hatlev1d + as.factor(degurba) | log_mar)

results$y
# Single women work slightly more than married women
# 30.72 VS 30.45

results$threefold$overall
# This suggests that, of the 0.270 hours difference, 
# approximately -0.215 can be attributed to group differences in endowments (i.e., age, education, living in a city or not),
# 1.425 to differences in coefficients,
# and the remaining -0.940 is accounted for by the interaction of the two.

# We can visualize it thanks to a bar plot:
plot(results, components = c("endowments","coefficients"))
# Education is clearly the most decisive factor


