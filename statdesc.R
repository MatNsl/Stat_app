library("dplyr")
library("ggplot2")

head(eulfs_small)

########## Descriptive statistics ----

# REFYEAR: year of survey
# COUNTRYW: Country of place of work for main job
# Not to be mixed up with COUNTRY: concerning residence and not citizenship)
# According to the latest userguide, "SCL GEO alpha-2 code" applies for countries
# Nonetheless, it appears that it is not the case with this base 
# since neither "FR", nor "250" are present.
# On the contrary with "3" which does not correspond to any country in the SCL GEO alpha-2 code.

# The mean of the number of hours actually worked in main job
eulfs_small$hwactual <- na_if(eulfs_small$hwactual, 99) # 99 means "not applicable"
mean(eulfs_small$hwactual, na.rm = TRUE)

# The mean of the number of hours that the person would like to work in total in a week
eulfs_small$hwwish <- na_if(eulfs_small$hwwish, 99)
mean(eulfs_small$hwwish, na.rm = TRUE)

# Sociodemographic variables that may be useful:
# SEX, AGE, HATLEVEL (for the level of education)...

########## Graphics ----

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

# The code below does not work since we do not know yet how to treat variables concerning countries

eulfs_France <- eulfs_small %>% filter(countryw == 250) %>% select(hwactual, refyear)
ggplot(eulfs_France) + geom_line(aes(x = refyear, y = hwactual))

eulfs_small %>% group_by(countryw) %>% plot(eulfs_small$hwactual, .by_group = TRUE)
