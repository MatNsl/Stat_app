##### First attempt to use recentered influence functions -----

install.packages("dplyr")
install.packages("ggplot2")
install.packages("labelled")
library("dplyr")
library("ggplot2")
library("labelled")

source(file = "~/work/Stat_app/DATA/exporting_data_from_link.R")

# Source:
# https://search.r-project.org/CRAN/refmans/dineq/html/00Index.html

install.packages("dineq")

eulfs_small$hwactual <- na_if(eulfs_small$hwactual, 99)

#Recentered influence funtion of 25th quantile
rif_q25 <- dineq::rif(x=eulfs_small$hwactual, weights=NULL, method="quantile", quantile=0.25)
# rif_q25 is a vector with as many components as observations in eulfs_small
# Each component corresponds to a particular individual’s influence on the distributional statistic

# Remark: 
# To have the influence function itself, we can subtract the quantile to RIF
if_q25 <- rif_q25 - quantile(eulfs_small$hwactual, probs = 0.25, na.rm = TRUE)

# RIF Regression with the same distributional statistic, namely the 25th quantile
rifr_q <- dineq::rifr(hwactual ~ age + hatlev1d + as.factor(degurba), eulfs_small, weights = NULL, method = "quantile", quantile = 0.25, kernel = "gaussian")
# We obtain a list of 5 elements that sum up the regression
rifr_q[["Coef"]]
# For instance, we obtain a negative coefficient (-0.0192) before the age variable
rifr_q[["adjusted_r2"]]
# The R2 is extremely small (0.00061)

