
reg<-eulfs_small

#Nettoyer la base en enlevant les valeurs NA

reg$hatlev1d<-na_if(reg$hatlev1d,99)
reg<-reg %>% filter(!is.na(hatlev1d))

reg$hwactual<-na_if(reg$hwactual,99)
reg<-reg %>% filter(!is.na(hwactual))

#On fait de la variable sex une variable binaire :
reg$sex<-eulfs_small$sex
reg$sex <- ifelse(reg$sex==2,1,0)

#On fait de la variable hatlev1d 3 variables binaires :

reg$high_educ <- ifelse(reg$hatlev1d==3,1,0)
reg$medium_educ <- ifelse(reg$hatlev1d==2,1,0)
reg$low_educ <- ifelse(reg$hatlev1d==1,1,0)

#On regarde la répartition du niveau d'éducation 

prop_high <- sum(reg$high_educ==1)/107576*100
prop_medium <- sum(reg$medium_educ==1)/107576*100
prop_low <- sum(reg$low_educ==1)/107576*100

#Variables occupation

occupation<-subset(eulfs_small, select=c("year","isco1d","is881d"))
occupation$isco1d<-na_if(occupation$isco1d,99)
occupation$is881d<-na_if(occupation$is881d,99)
occupation$isco1d<-ifelse(occupation$year > 2010 & is.na(occupation$isco1d),18,occupation$isco1d)
occupation$is881d<-ifelse(occupation$year < 2011 & is.na(occupation$is881d),18,occupation$is881d)

occupation$occupation_continuous <- ifelse(is.na(occupation$isco1d),occupation$is881d,occupation$isco1d)
df<-eulfs_small
df$occupation_continous<-occupation$occupation_continuous

df$hatlev1d<-na_if(df$hatlev1d,99)
df<-df %>% filter(!is.na(hatlev1d))
df$hwactual<-na_if(df$hwactual,99)
df<-df %>% filter(!is.na(hwactual))

reg$occupation <- df$occupation_continous
reg<- reg %>% filter(occupation!= 18)

#On a crée une seule variable qui donne l'occupation du job et qui a une valeur pour chaque année

#Régression linéaire de panel :
panel_data <- pdata.frame(reg, index=c("qhhnum","year"))
plm <-  plm(hwactual ~ year + sex + age + high_educ + low_educ + occupation,
            data = panel_data, index = c("qhhnum","year"), model="fd",effect="individual", vcov="cluster")
summary(plm)
plm2 <-  plm(hwactual ~ sex + age + high_educ + low_educ + occupation,
            data = panel_data, index = c("qhhnum","year"), model="fd",effect="individual", vcov="cluster")
summary(plm2)

occupation_status <- reg %>% group_by(occupation,year)%>%summarise(mean_hwactual=mean(hwactual, na.rm=TRUE))


path <- "~/Stat_app/Occupation/occupation_status.xlsx"

# Lire le fichier Excel
occu_code <- read.xlsx(path)

occupation_status <- merge(occupation_status,occu_code, by="occupation")

occupation_status_1 <- occupation_status %>% filter(occupation<=600 & occupation>=0)
occupation_status_2 <- occupation_status %>% filter(occupation>600)
occupation_status_3 <- occupation_status %>% filter(occupation>=700)

ploc1 <- ggplot(occupation_status_1, aes_string(x = "year", y = "mean_hwactual", group=1)) +
  geom_line()+
  # Add labels and title to the plot
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~occu_label)+
  ylim(25,45)+
  scale_x_continuous(breaks = c(2000,2005,2010))+
  theme(panel.grid = element_blank())
print(ploc1)

ploc2 <- ggplot(occupation_status_2, aes_string(x = "year", y = "mean_hwactual", group=1)) +
  geom_line()+
  # Add labels and title to the plot
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~occu_label)+
  ylim(25,45)+
  scale_x_continuous(breaks = c(2000,2005,2010))+
  theme(panel.grid = element_blank())
print(ploc2)

ploc3 <- ggplot(occupation_status_3, aes_string(x = "year", y = "mean_hwactual", group=1)) +
  geom_line()+
  # Add labels and title to the plot
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~occu_label)+
  ylim(25,45)+
  scale_x_continuous(breaks = c(2000,2005,2010))+
  theme(panel.grid = element_blank())
    
  
print(ploc3)

g<-ggplot(occupation_status, aes(x =mean_hwactual, y =reorder(occu_label,mean_hwactual), group=1))+
  geom_col()+
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~year)+
  theme(panel.grid = element_blank())
  
print(g)

#Calculer la proportion de chaque type de métier par année :


prop_by_year <- reg %>%
  group_by(year, occupation) %>%
  summarise(nb_occ = n())

nb_1998 <- sum(prop_by_year$nb_occ[prop_by_year$year==1998])
nb_1999 <- sum(prop_by_year$nb_occ[prop_by_year$year==1999])
nb_2000 <- sum(prop_by_year$nb_occ[prop_by_year$year==2000])
nb_2001 <- sum(prop_by_year$nb_occ[prop_by_year$year==2001])
nb_2002 <- sum(prop_by_year$nb_occ[prop_by_year$year==2002])
nb_2003 <- sum(prop_by_year$nb_occ[prop_by_year$year==2003])
nb_2004 <- sum(prop_by_year$nb_occ[prop_by_year$year==2004])
nb_2005 <- sum(prop_by_year$nb_occ[prop_by_year$year==2005])
nb_2006 <- sum(prop_by_year$nb_occ[prop_by_year$year==2006])
nb_2007 <- sum(prop_by_year$nb_occ[prop_by_year$year==2007])
nb_2008 <- sum(prop_by_year$nb_occ[prop_by_year$year==2008])
nb_2009 <- sum(prop_by_year$nb_occ[prop_by_year$year==2009])
nb_2010 <- sum(prop_by_year$nb_occ[prop_by_year$year==2010])
nb_2011 <- sum(prop_by_year$nb_occ[prop_by_year$year==2011])
nb_2012 <- sum(prop_by_year$nb_occ[prop_by_year$year==2012])
nb_2013 <- sum(prop_by_year$nb_occ[prop_by_year$year==2013])

prop_0_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==1998])/nb_1998*100
prop_100_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==1998])/nb_1998*100
prop_200_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==1998])/nb_1998*100
prop_300_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==1998])/nb_1998*100
prop_400_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==1998])/nb_1998*100
prop_500_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==1998])/nb_1998*100
prop_600_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==1998])/nb_1998*100
prop_700_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==1998])/nb_1998*100
prop_800_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==1998])/nb_1998*100
prop_900_98 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==1998])/nb_1998*100

prop_0_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==1999])/nb_1999*100
prop_100_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==1999])/nb_1999*100
prop_200_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==1999])/nb_1999*100
prop_300_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==1999])/nb_1999*100
prop_400_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==1999])/nb_1999*100
prop_500_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==1999])/nb_1999*100
prop_600_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==1999])/nb_1999*100
prop_700_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==1999])/nb_1999*100
prop_800_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==1999])/nb_1999*100
prop_900_99 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==1999])/nb_1999*100

prop_0_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2000])/nb_2000*100
prop_100_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2000])/nb_2000*100
prop_200_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2000])/nb_2000*100
prop_300_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2000])/nb_2000*100
prop_400_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2000])/nb_2000*100
prop_500_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2000])/nb_2000*100
prop_600_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2000])/nb_2000*100
prop_700_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2000])/nb_2000*100
prop_800_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2000])/nb_2000*100
prop_900_00 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2000])/nb_2000*100

prop_0_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2001])/nb_2001*100
prop_100_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2001])/nb_2001*100
prop_200_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2001])/nb_2001*100
prop_300_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2001])/nb_2001*100
prop_400_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2001])/nb_2001*100
prop_500_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2001])/nb_2001*100
prop_600_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2001])/nb_2001*100
prop_700_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2001])/nb_2001*100
prop_800_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2001])/nb_2001*100
prop_900_01 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2001])/nb_2001*100

prop_0_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2002])/nb_2002*100
prop_100_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2002])/nb_2002*100
prop_200_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2002])/nb_2002*100
prop_300_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2002])/nb_2002*100
prop_400_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2002])/nb_2002*100
prop_500_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2002])/nb_2002*100
prop_600_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2002])/nb_2002*100
prop_700_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2002])/nb_2002*100
prop_800_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2002])/nb_2002*100
prop_900_02 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2002])/nb_2002*100

prop_0_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2003])/nb_2003*100
prop_100_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2003])/nb_2003*100
prop_200_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2003])/nb_2003*100
prop_300_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2003])/nb_2003*100
prop_400_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2003])/nb_2003*100
prop_500_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2003])/nb_2003*100
prop_600_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2003])/nb_2003*100
prop_700_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2003])/nb_2003*100
prop_800_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2003])/nb_2003*100
prop_900_03 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2003])/nb_2003*100

prop_0_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2004])/nb_2004*100
prop_100_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2004])/nb_2004*100
prop_200_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2004])/nb_2004*100
prop_300_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2004])/nb_2004*100
prop_400_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2004])/nb_2004*100
prop_500_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2004])/nb_2004*100
prop_600_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2004])/nb_2004*100
prop_700_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2004])/nb_2004*100
prop_800_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2004])/nb_2004*100
prop_900_04 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2004])/nb_2004*100

prop_0_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2005])/nb_2005*100
prop_100_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2005])/nb_2005*100
prop_200_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2005])/nb_2005*100
prop_300_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2005])/nb_2005*100
prop_400_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2005])/nb_2005*100
prop_500_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2005])/nb_2005*100
prop_600_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2005])/nb_2005*100
prop_700_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2005])/nb_2005*100
prop_800_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2005])/nb_2005*100
prop_900_05 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2005])/nb_2005*100

prop_0_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2006])/nb_2006*100
prop_100_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2006])/nb_2006*100
prop_200_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2006])/nb_2006*100
prop_300_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2006])/nb_2006*100
prop_400_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2006])/nb_2006*100
prop_500_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2006])/nb_2006*100
prop_600_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2006])/nb_2006*100
prop_700_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2006])/nb_2006*100
prop_800_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2006])/nb_2006*100
prop_900_06 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2006])/nb_2006*100

prop_0_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2007])/nb_2007*100
prop_100_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2007])/nb_2007*100
prop_200_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2007])/nb_2007*100
prop_300_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2007])/nb_2007*100
prop_400_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2007])/nb_2007*100
prop_500_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2007])/nb_2007*100
prop_600_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2007])/nb_2007*100
prop_700_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2007])/nb_2007*100
prop_800_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2007])/nb_2007*100
prop_900_07 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2007])/nb_2007*100

prop_0_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2008])/nb_2008*100
prop_100_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2008])/nb_2008*100
prop_200_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2008])/nb_2008*100
prop_300_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2008])/nb_2008*100
prop_400_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2008])/nb_2008*100
prop_500_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2008])/nb_2008*100
prop_600_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2008])/nb_2008*100
prop_700_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2008])/nb_2008*100
prop_800_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2008])/nb_2008*100
prop_900_08 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2008])/nb_2008*100

prop_0_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2009])/nb_2009*100
prop_100_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2009])/nb_2009*100
prop_200_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2009])/nb_2009*100
prop_300_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2009])/nb_2009*100
prop_400_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2009])/nb_2009*100
prop_500_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2009])/nb_2009*100
prop_600_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2009])/nb_2009*100
prop_700_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2009])/nb_2009*100
prop_800_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2009])/nb_2009*100
prop_900_09 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2009])/nb_2009*100

prop_0_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2010])/nb_2010*100
prop_100_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2010])/nb_2010*100
prop_200_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2010])/nb_2010*100
prop_300_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2010])/nb_2010*100
prop_400_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2010])/nb_2010*100
prop_500_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2010])/nb_2010*100
prop_600_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2010])/nb_2010*100
prop_700_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2010])/nb_2010*100
prop_800_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2010])/nb_2010*100
prop_900_10 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2010])/nb_2010*100

prop_0_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2011])/nb_2011*100
prop_100_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2011])/nb_2011*100
prop_200_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2011])/nb_2011*100
prop_300_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2011])/nb_2011*100
prop_400_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2011])/nb_2011*100
prop_500_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2011])/nb_2011*100
prop_600_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2011])/nb_2011*100
prop_700_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2011])/nb_2011*100
prop_800_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2011])/nb_2011*100
prop_900_11 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2011])/nb_2011*100

prop_0_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2012])/nb_2012*100
prop_100_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2012])/nb_2012*100
prop_200_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2012])/nb_2012*100
prop_300_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2012])/nb_2012*100
prop_400_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2012])/nb_2012*100
prop_500_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2012])/nb_2012*100
prop_600_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2012])/nb_2012*100
prop_700_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2012])/nb_2012*100
prop_800_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2012])/nb_2012*100
prop_900_12 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2012])/nb_2012*100

prop_0_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==0 & prop_by_year$year==2013])/nb_2013*100
prop_100_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==100 & prop_by_year$year==2013])/nb_2013*100
prop_200_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==200 & prop_by_year$year==2013])/nb_2013*100
prop_300_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==300 & prop_by_year$year==2013])/nb_2013*100
prop_400_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==400 & prop_by_year$year==2013])/nb_2013*100
prop_500_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==500 & prop_by_year$year==2013])/nb_2013*100
prop_600_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==600 & prop_by_year$year==2013])/nb_2013*100
prop_700_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==700 & prop_by_year$year==2013])/nb_2013*100
prop_800_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==800 & prop_by_year$year==2013])/nb_2013*100
prop_900_13 <- sum(prop_by_year$nb_occ[prop_by_year$occupation==900 & prop_by_year$year==2013])/nb_2013*100

prop_occ <- numeric()
for (a in c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) {
  for (b in c(98,99,00,01,02,03,04,05,06,07,08,09,10,11,12,13)) {
    # Calcul du nom de la variable
    var_name <- paste0("prop_", a, "_", ifelse(b < 10, paste0("0", b), b))
    #Ajout de la valeur de la variable à prop_occ
    prop_occ <- c(prop_occ, get(var_name))
  }
}

occupation_status$prop <- prop_occ

occupation_status_1 <- occupation_status %>% filter(occupation<=300 & occupation>=0)
occupation_status_2 <- occupation_status %>% filter(occupation>=400 & occupation<=600)
occupation_status_3 <- occupation_status %>% filter(occupation>=700)


h<-ggplot(occupation_status, aes(x =prop, y =reorder(occu_label,prop), group=1))+
  geom_col()+
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~year)+
  theme(panel.grid = element_blank())

print(h)

wo<-ggplot(occupation_status, aes(x =mean_hwactual, y =reorder(occu_label,mean_hwactual), group=1))+
  geom_col()+
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~year)+
  theme(panel.grid = element_blank())

print(wo)

k1<-ggplot(occupation_status_1, aes(x =year, group=1))+
  geom_line(aes(y=mean_hwactual), color="red")+
  geom_line(aes(y=prop),color="blue")+
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~occu_label)+
  theme(panel.grid = element_blank())

print(k1)

k2<-ggplot(occupation_status_2, aes(x =year, group=1))+
  geom_line(aes(y=mean_hwactual), color="red")+
  geom_line(aes(y=prop),color="blue")+
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~occu_label)+
  theme(panel.grid = element_blank())

print(k2)

k3<-ggplot(occupation_status_3, aes(x =year, group=1))+
  geom_line(aes(y=mean_hwactual), color="red")+
  geom_line(aes(y=prop),color="blue")+
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~occu_label)+
  theme(panel.grid = element_blank())

print(k3)

reg$level_1<-ifelse(reg$occupation==100 | reg$occupation==200 | reg$occupation==300, 1, 0)
reg$level_2<-ifelse(reg$occupation==400 | reg$occupation==500 | reg$occupation==700 | reg$occupation==600 | reg$occupation==800, 1, 0)
reg$level_3<-ifelse(reg$occupation==900, 1, 0)


prop_1 <- sum(reg$level_1==1)/106853*100
prop_2 <- sum(reg$level_2==1)/106853*100
prop_3 <- sum(reg$level_3==1)/106853*100
prop_0 <- sum(reg$occupation==0)/106853*100


panel_databis <- pdata.frame(reg, index=c("qhhnum","year"))
plmbis <-  plm(hwactual ~ age + high_educ + low_educ,
            data = panel_databis, index = c("qhhnum","year"), model="fd",effect="individual", vcov="cluster")

summary(plmbis)

results <- oaxaca(formula = hwactual~age+level_1+high_educ+level_3+low_educ|sex|level_1, data=reg, R=1000)
oaxaca1 <- plot.oaxaca(results,decomposition="threefold", group.weight = -1)
print(oaxaca1)

plot(results, decomposition = "twofold", group.weight = -1,unexplained.split = TRUE, components = c("unexplained A","unexplained B"), component.labels = c("unexplained A" ="In Favor of Men", "unexplained B" = "Against Women"),component.left = TRUE, variables = c("age","level_1","low_educ"), variable.labels = c("age" = "Years of Age", "level_1" = "High-qualified job","low_educ" = "Low level of education"))

plot(results, components = c("endowments","coefficients"))

k4<-ggplot(occupation_status, aes(x =year, group=1))+
  geom_line(aes(y=mean_hwactual), color="red")+
  geom_line(aes(y=prop),color="blue")+
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~occu_label)+
  theme(panel.grid = element_blank())
print(k4)

reg$children <- ifelse(reg$hhnb0014!=0,1,0)


gender_occ<- reg %>%
  group_by(occupation, sex) %>%
  summarise(total=n())%>%
  group_by(occupation)

gender_freq <- gender_occ %>% 
  group_by(occupation)%>%
  summarise(freqw=total[sex==1]/sum(total)*100,freqm=total[sex==0]/sum(total)*100)
gender_freq<- merge(gender_freq,occu_code,by="occupation")

yo <- ggplot(gender_freq)+
  barplot(aes(y=freqm),color="blue")+
  barplot(aes(y=freqw),color="red")+
  theme_minimal()+
  facet_wrap(occu_label)
print(yo)

total_women <- sum(gender_occ$total[gender_occ$sex==1])
total_men <- sum(gender_occ$total[gender_occ$sex==0])



gender_freq_long <- gender_freq %>%
  gather(key = "gender", value = "percentage", -occu_label) %>%
  complete(gender = c("freqm", "freqw"), fill = list(percentage = 0))
gender_freq_long<-filter(gender_freq_long,gender!="occupation")

yo <- ggplot(gender_freq_long) +
  geom_bar(aes(x = "", y = percentage, fill = gender), stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ occu_label) +
  theme_void() +
  scale_fill_manual(values = c("slateblue1" , "tomato"), guide = FALSE) +  # Couleurs pour freqm (bleu) et freqw (rouge)
  labs(title = "Répartition des pourcentages par type de métier")

# Affichage du graphique
print(yo)



reg$armed <- ifelse(reg$occupation==0,1,0)
reg$manager <- ifelse(reg$occupation==100,1,0)
reg$pro <- ifelse(reg$occupation==200,1,0)
reg$tech <- ifelse(reg$occupation==300,1,0)
reg$clerical <- ifelse(reg$occupation==400,1,0)
reg$service <- ifelse(reg$occupation==500,1,0)
reg$agri <- ifelse(reg$occupation==600,1,0)
reg$trade <- ifelse(reg$occupation==700,1,0)
reg$operator <- ifelse(reg$occupation==800,1,0)
reg$elementary <- ifelse(reg$occupation==900,1,0)

oaxaca_gender<-oaxaca(formula=hwactual~age+armed+
                        manager+clerical+pro+
                        tech+service+
                        agri+trade+
                        operator+elementary+high_educ+low_educ+
                        children|sex|
                        children, 
                        data=reg,
                        R=1000)
ya <- plot(oaxaca_gender, method="threefold" )
print(ya)

yi <- plot(oaxaca_gender, decomposition="twofold",group.weight = -1,
           unexplained.split = TRUE, 
           components = c("unexplained A","unexplained B"),
           component.labels = c("unexplained A" ="In Favor of Men", "unexplained B" = "Against Women"),
           component.left = TRUE, variables = c("age","children","trade","manager","operator","service","low_educ"), variable.labels = c("age" = "Years of Age", "children"="Having children","trade"="Working in trade sector","manager"="Working in managerial sector","operator"="Working in operator sector","service"="Working in service sector","low_educ"="Low level of education"))
print(yi)

reg$children <- ifelse(reg$hhnb0014!=0,1,0)