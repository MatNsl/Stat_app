install.packages("haven")
# Load the haven package
library(haven)


data <- read_dta("~/work/Stat_app/DATA/eulfs_small.dta")
output_path <- ("~/work/Stat_app/DATA/eulfs_small.csv")

# Write the data in CSV in the path wanted
write.csv(data, file = output_path, row.names = FALSE)

# Read the CSV previously created
eulfs_small <- read.csv(output_path)




