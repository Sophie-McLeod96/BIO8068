

# Bird Nest analysis 
# name: ??

# Nov 2019 

# use this code to load the data 


wants <- c("ggfortify", "here")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load library 

library(here)

# load data and check the top 6 rows. 

nest<-read.csv(here("Data", "BirdNest.csv"))
head(nest)



###### copy and past your code below here.......

# Data Exploration ####

summary(nest)

table(nest$Nesttype)

table(nest$Location)

table (nest$Location, nest$Nesttype)

table(nest$Nesttype, nest$Closed.)

#graph - histogram
hist(nest$No.eggs)


library(ggplot2)
m<- ggplot(nest,aes(x=No.eggs))
m <- m+ geom_histogram(binwidth = 1)
m+facet_grid(~ Closed.)

boxplot(nest$Length)

boxplot(nest$Nestling)

boxplot(nest$Incubate)

boxplot(nest$Totcare)

plot(nest$Length, nest$Nestling)

plot(Nestling ~ Length, nest)


ggplot(nest, aes(Length,Nestling))+geom_point()


ggplot(nest, aes(Length, Nestling, color=as.factor(Closed.))) +geom_point()


# Model Building ####

#GLM

M1.nest <- glm(Nestling ~ Length + Nesttype + Location + No.eggs, data = nest)

summary(M1.nest)



plot(M1.nest)

