install.packages("openxlsx")
library(openxlsx)


eulfs_small |>
  dplyr::mutate(country = haven::as_factor(country)) |>
  head()


path <- "~/work/Stat_app/Countries/countries_code_coordinates.xlsx"

# Lire le fichier Excel
countries_code <- read.xlsx(path)
