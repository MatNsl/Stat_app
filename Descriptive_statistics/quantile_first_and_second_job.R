install.packages("dplyr")
install.packages("ggplot2")
install.packages("labelled")
install.packages("tidyr")
library("dplyr")
library("ggplot2")
library("labelled")
library("haven")
library("tidyr")

eulfs_jobs <- subset(eulfs_small, select=c("country","year","hwactual","hwactua2","exist2j"))
eulfs_jobs <- merge(eulfs_jobs,countries_code, by="country")

eulfs_jobs$hwactual <- na_if(eulfs_jobs$hwactual, 99)
eulfs_jobs$hwactua2 <- na_if(eulfs_jobs$hwactua2, 99)
# Filter data to exclude NA values in the hwactual variable
eulfs_jobs <- eulfs_jobs %>% filter(!is.na(hwactual))

eulfs_jobs <- eulfs_jobs %>% filter(!(exist2j == 2 & is.na(hwactua2)))

eulfs_jobs <- eulfs_jobs %>% mutate(hwactua2 = ifelse(exist2j == 1 & is.na(hwactua2), 0, hwactua2))

eulfs_jobs["sum_jobs"]<-eulfs_jobs["hwactual"]+eulfs_jobs["hwactua2"]
eulfs_jobs<- eulfs_jobs %>% filter(country_name!="Malta")

quantiles_jobs <- eulfs_jobs %>%
  group_by(country_name, year) %>% 
  summarise(
  max_hwactua1 = max(hwactual, na.rm = TRUE),
  min_hwactua1 = min(hwactual, na.rm = TRUE),
  q1_hwactua1 = quantile(hwactual, probs = 0.25, na.rm = TRUE),
  median_hwactua1 = median(hwactual, na.rm = TRUE),
  q3_hwactua1 = quantile(hwactual, probs = 0.75, na.rm = TRUE),
  max_sum = max(sum_jobs, na.rm = TRUE),
  min_sum = min(sum_jobs, na.rm = TRUE),
  q1_sum = quantile(sum_jobs, probs = 0.25, na.rm = TRUE),
  median_sum = median(sum_jobs, na.rm = TRUE),
  q3_sum = quantile(sum_jobs, probs = 0.75, na.rm = TRUE),
  )



g<-
  ggplot(quantiles_jobs, aes_string(x = "year", group=1)) +
  geom_line(aes(y=max_hwactua1), color="red") +
  geom_line(aes(y=min_hwactua1), color= "black") +
  geom_line(aes(y=q1_hwactua1), color= "green") +
  geom_line(aes(y=median_hwactua1), color= "yellow") +
  geom_line(aes(y=q3_hwactua1), color= "blue") +
  geom_line(aes(y=max_sum), color="darkorange") +
  geom_line(aes(y=min_sum), color= "bisque4") +
  geom_line(aes(y=q1_sum), color= "darkgreen") +
  geom_line(aes(y=median_sum), color= "gold") +
  geom_line(aes(y=q3_sum), color= "blueviolet") +
  labs(
    title = "",
    x = "",
    y = "Actual weekly hours worked",
    color = NULL
  ) +
  theme_minimal()+
  facet_wrap(~country_name)+
  scale_x_continuous(breaks = c(2000,2005,2010))+
  theme(panel.grid = element_blank())

print(g)
