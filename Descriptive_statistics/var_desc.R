##### Preliminaries -----

install.packages("dplyr")
install.packages("ggplot2")
install.packages("labelled")
install.packages("dineq") # specific package for RIF
install.packages("stargazer") # for nice tables
install.packages("sandwich")
install.packages("lmtest")

library("dplyr")
library("ggplot2")
library("labelled")

source(file = "~/work/Stat_app/DATA/exporting_data_from_link.R")

##### Copy of Anais -----

dec1 <- eulfs_small
#1. Enlever les valeurs NA de la variable expliquée : 

dec1$hwusual<-na_if(dec1$hwusual,99)
dec1 <-dec1%>% filter(!is.na(hwusual))

#2. Variable education : 

dec1$hatlev1d<-na_if(dec1$hatlev1d,99)
dec1 <-dec1%>% filter(!is.na(hatlev1d))

#On fait de la variable hatlev1d 3 variables binaires :

dec1$high_educ <- ifelse(dec1$hatlev1d==3,1,0)
dec1$medium_educ <- ifelse(dec1$hatlev1d==2,1,0)
dec1$low_educ <- ifelse(dec1$hatlev1d==1,1,0)

#3. Occupation status : 
#Création d'un nouveau df pour récupérer les variables occupation 

occupation<-subset(eulfs_small, select=c("year","isco1d","is881d"))
occupation$isco1d<-na_if(occupation$isco1d,99)
occupation$is881d<-na_if(occupation$is881d,99)
occupation$isco1d<-ifelse(occupation$year > 2010 & is.na(occupation$isco1d),18,
                          occupation$isco1d)
occupation$is881d<-ifelse(occupation$year < 2011 & is.na(occupation$is881d),18,
                          occupation$is881d)

occupation$occupation_continuous <- ifelse(is.na(occupation$isco1d),occupation$is881d,occupation$isco1d)
df<-eulfs_small
df$occupation_continous<-occupation$occupation_continuous

df$hatlev1d<-na_if(df$hatlev1d,99)
df<-df %>% filter(!is.na(hatlev1d))
df$hwusual<-na_if(df$hwusual,99)
df<-df %>% filter(!is.na(hwusual))

dec1$occupation <- df$occupation_continous
dec1<- dec1 %>% filter(occupation!= 18)

#On crée une variable binaire pour chaque statut

dec1$armed <- ifelse(dec1$occupation==0,1,0)
dec1$manager <- ifelse(dec1$occupation==100,1,0)
dec1$pro <- ifelse(dec1$occupation==200,1,0)
dec1$tech <- ifelse(dec1$occupation==300,1,0)
dec1$cler <- ifelse(dec1$occupation==400,1,0)
dec1$service <- ifelse(dec1$occupation==500,1,0)
dec1$agri <- ifelse(dec1$occupation==600,1,0)
dec1$trade <- ifelse(dec1$occupation==700,1,0)
dec1$operator <- ifelse(dec1$occupation==800,1,0)
dec1$elementary <- ifelse(dec1$occupation==900,1,0)

#4. Gender

dec1$women <-if_else(dec1$sex==2,1,0)


##### My part -----

# Dataframe with percentages

vec_occup <- c("armed", "manager","pro","tech","cler","service","agri","operator"
,"elementary", "trade")

df_percent <- data.frame(Variable = character(), Percentage = numeric())
i <- 0
for (v in vec_occup){
  col <- as.name(paste("dec1$", v, sep = ""))
  percent <- ((nrow(dec1[col == 1,]))/(nrow(dec1)))*100
  i <- i+1
  df_percent[i,] <- c(v, percent)
}

vec_occup <- c(dec1$armed, dec1$manager,dec1$pro,dec1$tech,dec1$cler,dec1$service,dec1$agri,dec1$operator
               ,dec1$elementary, dec1$trade)

df_percent <- data.frame(Variable = character(), Percentage = numeric())
i <- 0
for (col in vec_occup){
  percent <- ((nrow(dec1[col == 1,]))/(nrow(dec1)))*100
  i <- i+1
  df_percent[i,] <- c(as.character(names(col)), percent)
}












dec1 <- eulfs_small

#1. Enlever les valeurs NA de la variable expliquée : 

dec1$hwusual<-na_if(dec1$hwusual,99)
dec1 <-dec1%>% filter(!is.na(hwusual))

##### Which variables? -----

# Occupation

occupation <- subset(eulfs_small, select=c("year","isco1d","is881d"))
occupation$isco1d <- na_if(occupation$isco1d,99)
occupation$is881d <- na_if(occupation$is881d,99)
occupation$isco1d <- ifelse(occupation$year > 2010 & is.na(occupation$isco1d),18,
                          occupation$isco1d)
occupation$is881d <- ifelse(occupation$year < 2011 & is.na(occupation$is881d),18,
                          occupation$is881d)

occupation$occupation_continuous <- ifelse(is.na(occupation$isco1d),occupation$is881d,occupation$isco1d)
eulfs <- eulfs_small
eulfs$occupation_continous<-occupation$occupation_continuous

eulfs$hatlev1d <- na_if(eulfs$hatlev1d,99)
eulfs <- eulfs %>% filter(!is.na(hatlev1d))
eulfs$hwusual <- na_if(eulfs$hwusual,99)
eulfs <- eulfs %>% filter(!is.na(hwusual))

dec1$occupation <- eulfs$occupation_continous
dec1 <- dec1 %>% filter(occupation!= 18)

#On crée une variable binaire pour chaque statut

dec1$armed <- ifelse(dec1$occupation==0,1,0)
dec1$manager <- ifelse(dec1$occupation==100,1,0)
dec1$pro <- ifelse(dec1$occupation==200,1,0)
dec1$tech <- ifelse(dec1$occupation==300,1,0)
dec1$cler <- ifelse(dec1$occupation==400,1,0)
dec1$service <- ifelse(dec1$occupation==500,1,0)
dec1$agri <- ifelse(dec1$occupation==600,1,0)
dec1$trade <- ifelse(dec1$occupation==700,1,0)
dec1$operator <- ifelse(dec1$occupation==800,1,0)
dec1$elementary <- ifelse(dec1$occupation==900,1,0)


