########## Descriptive statistics ----

# The mean of the number of hours actually worked in main job
mean(eulfs_small$hwactual, na.rm = TRUE)
# N.B.: I don't really know how to treat the value "99", i.e. not applicable

# The mean of the number of hours that the person would like to work in total in a week
mean(eulfs_small$hwwish, na.rm = TRUE)

# An histogramm about the number of hours actually worked in main job
hist(eulfs_small$hwactual, col = "skyblue",
     main = "Distribution of the numbers of hours",
     xlab = "Number of hours",
     ylab = "Number of people")

# An histogramm about the number of hours that the person would like to work in total in a week
hist(eulfs_small$hwwish, col = "skyblue",
     main = "Distribution of the numbers of hours people wish",
     xlab = "Number of hours",
     ylab = "Number of people")

# Comparison between men and women concerning the number of hours actually worked in main job
boxplot(eulfs_small$hwactual ~ eulfs_small$sex,
        xlab = "Sex",
        ylab = "Number of hours",
        names = c("Men", "Women")
)

