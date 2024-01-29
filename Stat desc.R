########## Descriptive statistics ----

# AVAILBLE : Availability to start working immediately or to work more
eulfs_small$availble <- as.numeric(as.character(eulfs_small$availble), na.rm = TRUE)

# An histogramm about the availability to start working immediately or to work more
percentages <- table(eulfs_small$availble) / length(eulfs_small$availble) * 100

# Barplot with pourcentages
barplot(percentages, 
        col = "skyblue",
        main = "Availability to start working immediately \nor to work more (within two weeks)",
        ylab = "Percentage of people",
        names.arg = c("Yes", "No"),
        ylim = c(0, max(percentages) + 2))
# Add percentages
text(c(1, 2), percentages, sprintf("%.1f%%", percentages), pos = 3, offset = 0.5)

# We note we have a lot of non-respondants.
# They must be people who don't suffer their working hours (both ways)


#WSTATOR : WKSTAT et STAPRO recoded

# An histogramm about the availability to start working immediately or to work more
percentages <- table(eulfs_small$wstator) / length(eulfs_small$wstator) * 100

# Barplot with pourcentages
barplot(percentages, 
        col = "skyblue",
        main = "need to understand wstator",
        ylab = "Percentage of people",
        ylim = c(0, max(percentages) + 5))
# Add percentages
text(1:5, percentages, sprintf("%.1f%%", percentages), pos = 3, offset = 0.5)

#wstator has been recoded and seems tricky
# 1 : worked in the reference week
# 2 :Absent from work or business during the reference week (self-declared)
# 3 and 4 : ???
# 5 : Neither worked nor had a job or business during the reference week



#hwusual : Number of hours per week usually worked in main job
#CONTRHRS not found
unique(eulfs_small$hwusual)
class(eulfs_small$hwusual)

hist(eulfs_small$hwusual, col = "skyblue",
     main = "Number of hours per week usually worked in main job",
     xlab = "Number of hours",
     ylab = "Number of people",
     breaks = 30)

# Add supplementary graduations on the abscisses' axe
axis(1, at = seq(0, 80, by = 5))

# Most people work around 39hours a week in main job

# Comparison between men and women concerning the number of hours per week worked in main job
boxplot(eulfs_small$hwusual ~ eulfs_small$sex,
        xlab = "Sex",
        ylab = "Number of hours",
        names = c("Men", "Women")
)

#We norice a small genre difference between men and women.
#These lasts tend to work a little less hours per week but the working hour rate
# is more variable within their social group.

