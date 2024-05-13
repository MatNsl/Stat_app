source(file = "~/work/Stat_app/DATA/exporting_data_from_link.R")

### Preparation by Anais to have the correct dec1



df_med <- dec1[dec1$hwusual <= 40,]

round(((nrow(df_med[df_med[ ,"low_educ"] == 1,]))/(nrow(df_med)))*100, digits = 2)

round(((nrow(dec1[dec1[ ,"low_educ"] == 1,]))/(nrow(dec1)))*100, digits = 2)


##### Densities of age (overall vs 20% lowest) --------

# quantile(dec1$hwusual, 0.20, na.rm = TRUE) # 32 hours
df_0.2 <- dec1[dec1$hwusual <= 32,]
dens_age <- density(df_0.2$age, na.rm = TRUE)

# dens_age2 <- density(dec1$age, na.rm = TRUE) # total

# quantile(dec1$hwusual, 0.10, na.rm = TRUE) # 20 hours
df_0.1 <- dec1[dec1$hwusual <= 20,]
dens_age3 <- density(df_0.1$age, na.rm = TRUE)

# quantile(dec1$hwusual, 0.30, na.rm = TRUE) # 38 hours
df_0.3 <- dec1[dec1$hwusual <= 38,]
dens_age5 <- density(df_0.3$age, na.rm = TRUE)

# df_0.5 <- dec1[dec1$hwusual <= 40,]
# dens_age4 <- density(df_0.5$age, na.rm = TRUE)

plot <- plot(dens_age5$x, dens_age5$y,type="l",xlab="Age",ylab="", 
             main = "Distribution of the age", 
             las = 1) # 30%
# lines(dens_age$x, dens_age$y, col = "red") # 20%
lines(dens_age3$x, dens_age3$y, col = "blue") # 10%
# lines(dens_age4$x, dens_age4$y, col = "green") # median
lines(dens_age$x, dens_age$y, col = "purple") # 20%

# Joli et pratiquement illisible, rajouter les déciles au fur et à mesure pour voir quelque chose

