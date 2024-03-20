install.packages("dplyr")
install.packages("ggplot2")
install.packages("labelled")
install.packages("tidyr")
library("dplyr")
library("ggplot2")
library("labelled")
library("haven")
library("tidyr")

# Convert the "year" and "country" variable to a factor for correct visualization in the histogram
eulfs_small$year <- as.factor(eulfs_small$year)
eulfs_small$country <- as.factor(eulfs_small$country)

# Remove values 99 (not applicable) from the hwactual variable
eulfs_small$hwactual <- na_if(eulfs_small$hwactual, 99)
# Filter data to exclude NA values in the hwactual variable
eulfs_filtered <- eulfs_small %>% filter(!is.na(hwactual))
eulfs_filtered <- merge(eulfs_filtered, countries_code, by="country")

mean_by_country_year <- eulfs_filtered %>%
  group_by(country_name, year) %>%
  summarise(mean_hwactual = mean(hwactual, na.rm = TRUE))
mean_by_country_year<- mean_by_country_year %>%
  filter(country_name != "Malta")
mean_by_country_year$year <- as.numeric(as.character(mean_by_country_year$year))

# Utiliser le nom de colonne nettoyé dans ggplot
ggplot(mean_by_country_year, aes_string(x = "year", y = "mean_hwactual",group=1)) +
  geom_line() +
  labs(
    title = "Mean of the Number of Hours Worked per Week by Year",
    x = "",
    y = "Actual weekly hours in first job"
  ) +
  theme_minimal()+
  ylim(25,45)+
  facet_wrap(~country_name)+
  scale_x_continuous(breaks = c(2000,2005,2010,2013))+
  theme(panel.grid = element_blank())


# Create a point and line plot

for (colonne in colnames(mean_by_country_year,-1)) {
  ggplot(mean_by_country_year[, c(1, i)], aes(x = year, y = mean_by_country_year[, c(1, i)][, 2], group = 1)) +
    geom_point() +
    geom_line() +
    labs(
      title = "Mean of the Number of Hours Worked per Country",
      x = "Year",
      y = "Mean of Hours Worked per week"
    ) +
    theme_minimal() 
}
 

column_names <- colnames(mean_by_country_year)
column_names <- tail(column_names, -1)

ggplot(mean_by_country_year, aes(x = year, y = column_names[2]) +
  geom_point() +
  geom_line() +
  labs(
    title = "Mean of the Number of Hours Worked per Country",
    x = "Year",
    y = "Mean of Hours Worked per week"
  ) +
  theme_minimal()) 

for (colonne in column_names) {
  ggplot(mean_by_country_year, aes_string(x = "year", y = colonne)) +
           geom_point() +
           geom_line() +
           labs(
             title = "Mean of the Number of Hours Worked per Country",
             x = "Year",
             y = "Mean of Hours Worked per week") +
           theme_minimal() }
  

for (colonne in column_names) {
  # Nettoyer le nom de la colonne
  clean_colonne <- make.names(colonne)
  
  # Utiliser le nom de colonne nettoyé dans ggplot
  ggplot(mean_by_country_year, aes_string(x = "year", y = clean_colonne)) +
    geom_point() +
    geom_line() +
    labs(
      title = "Mean of the Number of Hours Worked per Country",
      x = "Year",
      y = "Mean of Hours Worked per week"
    ) +
    theme_minimal()
}


