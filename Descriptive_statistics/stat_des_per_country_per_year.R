install.packages("dplyr")
install.packages("ggplot2")
install.packages("labelled")
library("dplyr")
library("ggplot2")
library("labelled")
library("haven")


##An histogram of the mean of the number of hours actually worked in main job by year 
# Convert the "year" variable to a factor for correct visualization in the histogram
eulfs_small$year <- as.factor(eulfs_small$year)

# Remove values 99 (not applicable) from the hwactual variable
eulfs_small$hwactual <- na_if(eulfs_small$hwactual, 99)

# Filter data to exclude NA values in the hwactual variable
eulfs_filtered <- eulfs_small %>% filter(!is.na(hwactual))

# Calculate the mean of the number of hours worked for each year
mean_by_year <- eulfs_filtered %>%
  group_by(year) %>%
  summarise(mean_hwactual = mean(hwactual, na.rm = TRUE))

# Create a histogram
ggplot(mean_by_year, aes(x = year, y = mean_hwactual, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean of the Number of Hours Worked per Year",
       x = "Year",
       y = "Mean of Hours Worked")

# Using base R plot function
print(plot(mean_by_year$year, mean_by_year$mean_hwactual, type = "bar",
     main = "Mean of the Number of Hours Worked per Year",
     xlab = "Year", ylab = "Mean of Hours Worked"))


##An histogram of the mean of the number of hours actually worked in main job by country  

# Convert the "countryw" variable to a factor for correct visualization in the bar plot
eulfs_small$country <- as.factor(eulfs_small$country)

# Remove values 99 (not applicable) from the hwactual variable
eulfs_small$hwactual <- na_if(eulfs_small$hwactual, 99)

# Filter data to exclude NA values in the hwactual variable
eulfs_filtered_country <- eulfs_small %>% filter(!is.na(hwactual))



# Calculate the mean of the number of hours worked for each country
mean_by_country <- eulfs_filtered_country %>%
  group_by(country) %>%
  summarise(mean_hwactual = mean(hwactual, na.rm = TRUE))
# Merge mean_by_country with countries_code
mean_by_country <- merge(mean_by_country, countries_code, by = "country")
#this code will work when we'll have the right correspondance between the numeric value and the alpha.2.code (that is not the case with the actual data base)

# Create a bar plot
ggplot(mean_by_country, aes(x = label, y = mean_hwactual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean of the Number of Hours Worked per Country",
       x = "Country",
       y = "Mean of Hours Worked") +
  theme_minimal()



