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
# The peak of the distribution is clearly between 35 and 40 hours with more than 40000 people
# But the second most important interval is between 0 and 5 hours

# An histogramm about the number of hours that the person would like to work in total in a week
hist(eulfs_small$hwwish, col = "skyblue",
     main = "Distribution of the numbers of hours people wish",
     xlab = "Number of hours",
     ylab = "Number of people")
# A vast majority of people (more than 40000) wish to work between 35 and 40 hours

# Comparison between men and women concerning the number of hours actually worked in main job
boxplot(eulfs_small$hwactual ~ eulfs_small$sex,
        xlab = "Sex",
        ylab = "Number of hours",
        names = c("Men", "Women")
)
# Both medians are not so far away from each other
# But the variability is much higher for women with a first quartile that is close from the minimum of men

# We do not know yet how to identify countries
# However, we may analyse data for a specific country, for instance "number 3"
eulfs_pays3 <- eulfs_small %>% filter(countryw == 3) %>% select(hwactual, refyear)
frame <- eulfs_pays3 %>% group_by(refyear, .drop = NA) %>% summarise(Min = min(hwactual, na.rm = TRUE), Max = max(hwactual, na.rm = TRUE), Median = median(hwactual, na.rm = TRUE), quant1 = quantile(hwactual, probs = 0.25, na.rm = TRUE), quant2 = quantile(hwactual, probs = 0.75, na.rm = TRUE))
ggplot(frame, aes(x = refyear, na.rm = TRUE)) + geom_line(aes(y=Min)) + geom_line(aes(y=Max)) + geom_line(aes(y=Median), color = "darkred") + geom_line(aes(y=quant1), color = "green") + geom_line(aes(y=quant2), color = "green") 
# We notice that all quartiles remain somewhat stable, except the first quartile
# Indeed, the quarter of the population who work the least has less and less working hours
# The number of hours dropped below 20 hours after 2010

