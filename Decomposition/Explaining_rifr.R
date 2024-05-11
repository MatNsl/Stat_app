source(file = "~/work/Stat_app/DATA/exporting_data_from_link.R")

### Preparation by Anais to have the correct dec1



df_med <- dec1[dec1$hwusual <= 40,]

round(((nrow(df_med[df_med[ ,"low_educ"] == 1,]))/(nrow(df_med)))*100, digits = 2)

round(((nrow(dec1[dec1[ ,"low_educ"] == 1,]))/(nrow(dec1)))*100, digits = 2)