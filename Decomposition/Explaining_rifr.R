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

##### Second attempt -----

# quantile(dec1$hwusual, 0.20, na.rm = TRUE) # 32 hours
df_0.2 <- dec1[dec1$hwusual <= 32,]
dens_age <- density(df_0.2$age, na.rm = TRUE)

# quantile(dec1$hwusual, 0.10, na.rm = TRUE) # 20 hours
# df_0.1 <- dec1[dec1$hwusual <= 20,]
dens_age3 <- density(df_0.1$age, na.rm = TRUE)

plot <- plot(dens_age$x, dens_age$y,type="l",xlab="Age",ylab="", 
             main = "Distribution of the age", 
             las = 1) # 20%
lines(dens_age3$x, dens_age3$y, col = "blue") # 10%
# lines(dens_age$x, dens_age$y, col = "purple") # 20%


dens_agetot <- density(dec1$age, na.rm = TRUE) # total

plot <- plot(dens_agetot$x, dens_agetot$y,type="l",xlab="Age",ylab="", 
             main = "Distribution of the age", 
             las = 1)

dens_agetot <- density(eulfs_small$age, na.rm = TRUE) # total

plot <- plot(dens_agetot$x, dens_agetot$y,type="l",xlab="Age",ylab="", 
             main = "Distribution of the age", 
             las = 1)


round(((nrow(df_0.2[df_0.2[ ,"age"] <= 22,]))/(nrow(df_0.2)))*100, digits = 2) 
# 8.08% of the population that work less than 32 hours has between 20 and 24 years old
# 14.67% of the population that work less than 32 hours has less than 24 years old
round(((nrow(df_0.2[df_0.2[ ,"age"] == 42,]))/(nrow(df_0.2)))*100, digits = 2) 
# 12.3% of the population that work less than 32 hours has between 40 and 44 years old

round(((nrow(dec1[dec1[ ,"age"] <= 22,]))/(nrow(dec1)))*100, digits = 2) 
# 10.32% of the population has less than 24 years old
round(((nrow(dec1[dec1[ ,"age"] == 42,]))/(nrow(dec1)))*100, digits = 2) 
# 13.82% of the population has between 40 and 44


