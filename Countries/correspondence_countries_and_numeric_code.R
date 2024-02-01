#Create a tab to have the correspondance between the numerate code and the country according to the alpha_2 code 

countries_codes <- read.csv("~/work/Stat_app/Countries/countries_codes_and_coordinates.csv")
countries_codes <- countries_codes_and_coordinates[, c("Country","Alpha.2.code", "Numeric.code")]
colnames(countries_codes)[colnames(countries_codes) == "Numeric.code"] <- "countryw"




write.csv(countries_codes, file = "~/work/Stat_app/Coordinates for countries/countries_codes.csv", row.names = FALSE)


