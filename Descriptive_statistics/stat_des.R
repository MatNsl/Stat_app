library("dplyr")
library("ggplot2")

head(eulfs_small)

##Descriptive statistics ----
#hwusual: Number of hours per week usually worked in main job


# The mean of the number of hours per week actually worked in main job
eulfs_small$hwusual <- na_if(eulfs_small$hwusual, 99) 
mean_hwusual<- mean(eulfs_small$hwactual, na.rm = TRUE)

#The mean of the number of hours per week actually worked in main job is around 34.6
########## Graphics ----

# An histogramm about the number of hours actually worked in main job per week 
hist(eulfs_small$hwusual, col = "skyblue",
     main = "Distribution of the numbers of hours",
     xlab = "Number of hours",
     ylab = "Number of people",
)


# Comparison between men and women concerning the number of hours actually worked in main job per week 
boxplot(eulfs_small$hwusual ~ eulfs_small$sex,
        xlab = "Sex",
        ylab = "Number of hours",
        names = c("Men", "Women"),
        col = c("blue", "red")  # Set colors for the boxes
)


