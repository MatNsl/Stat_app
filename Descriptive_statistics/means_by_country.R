install.packages("dplyr")
install.packages("ggplot2")
install.packages("labelled")
library("dplyr")
library("ggplot2")
library("labelled")
library("haven")

##An histogram of the mean of the number of hours actually worked in main job by country  

# Convert the "country" variable to a factor for correct visualization in the bar plot
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


mean_value_EU <- mean(eulfs_filtered_country$hwactual)

# Create a point and line plot
ggplot(mean_by_country, aes(x = label, y = mean_hwactual, group = 1)) +
  
  # Add points to the plot
  geom_point() +
  
  # Add lines to connect the points
  geom_line() +
  
  # Add labels and title to the plot
  labs(
    title = "Mean of the Number of Hours Worked per Country",
    x = "Country",
    y = "Mean of Hours Worked"
  ) +
  
  # Set the theme to minimal
  theme_minimal() +
  
  # Adjust x-axis labels spacing
  scale_x_discrete(expand = c(0.002, 0.002)) +
  
  ylim(27,42)




# Create a horizontal bar plot
ggplot(mean_by_country, aes(x = mean_hwactual, y = reorder(country_name, -mean_hwactual))) +
  
  # Add horizontal bars to the plot
  geom_col(width = 0.7) +
  # Add labels and title to the plot
  labs(
    title = "Mean of the Number of Hours Worked per Week, per Country",
    x = "Mean of Hours Worked per week",
    y = "Country"
  ) +
  geom_vline(xintercept = mean_value_EU, linetype = "solid", color = "red") +
  # Set the theme to minimal
  theme_minimal()


