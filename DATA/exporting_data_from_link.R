install.packages("haven")  
install.packages("foreign")
library(foreign)
library(haven)

#Dropbox link to access the data 
dropbox_link <- "https://dl.dropboxusercontent.com/scl/fi/8unrcectb4nofv3461rip/eulfs_small.dta?rlkey=sj2ipe6ipgbazjhnh1t70b7j2&dl=0"


# Download the file 
download.file(dropbox_link, destfile = "eulfs_small.dta", mode = "wb")


#Read the .dta file

eulfs_small <- haven::read_dta('eulfs_small.dta')


