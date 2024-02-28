# Inserting data 
vacc <- data.frame(catgry=rep(c("Covishield", "Covaxin"), each=2), 
                   dose=rep(c("D1", "D2"),2), 
                   slots=c(33, 45, 66, 50)) 

library(ggplot2) 
# Plotting basic line with multiple groups 
plt <- ggplot(data=vacc, aes(x=dose, y=slots, group=catgry))+ 
  geom_line()+ 
  geom_point(color="red", size=3)+ 
  labs(x="Doses",y="Free Slots")+ 
  ggtitle("Vaccine Details") 

# Adding legends 
plt+geom_line(aes(color=catgry)) 

plt 
#######------

p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(colour = factor(cyl))) +
  scale_color_manual(values = c("red", "blue", "green"))
plot(p)
library(grid)
grid.newpage()

# It's recommended to use a named vector
cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
p + scale_colour_manual(values = cols)
