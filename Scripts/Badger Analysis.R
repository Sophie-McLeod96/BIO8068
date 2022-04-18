

# Badger  analysis 
# name: Sophie Carolan

# Nov 2021

# use this code to load the data 


wants <- c("ggfortify", "here", "nlme", "lattice","ggplot2")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load library 

library(here)

# load data and check the top 6 rows. 

badger<-read.csv(here("Data", "bad4.csv"))
head(badger)



###### copy and past your code below here.......

library(nlme)
library(ggplot2)
library(lattice)

summary(badger)

# create a hypothesis --> define perceived relationship 
#Badger weight is dependent on badger length
#plot the response variable against the predictor
plot(badger$Length,badger$Weight )

#write the model into R
test.glm <- glm(Weight~Length, gaussian, badger)
summary(test.glm)

#assess error for model suitability
hist(residuals(test.glm)) #bell-shaped curved centered on zero formally
qqnorm(residuals(test.glm)) # should be a straight line

# new hypothesis
# sex will determine size
#plot
plot(badger$Sex, badger$Length)

sexvslengthplot <-ggplot(data= badger,aes(x= Sex, y=Length)) +geom_boxplot()
sexvslengthplot

new.glm<- glm(Weight ~Length + Age + Tooth, gaussian, badger)
summary(new.glm)

hist(residuals(new.glm))
qqnorm(residuals(new.glm))

# making a LME - Linear Mixed Effects Model ####

install.packages("nlme")

library(nlme)
library(ggplot2)

#creating the plot
ggplot(badger,aes(Weight, Length, group=Area)) +
  geom_point(aes(color=Area))+
  geom_smooth(method="lm")

#create the plot with facets
ggplot(badger,aes(Weight, Length, group=Area)) +
  geom_point()+
  facet_wrap(.~Area)

#can group the data 
#the model requires grouping to be specified - so set group structure
badgerG<-groupedData(Weight ~ Length | Area, badger)
#badgerList <- lmList(Weight ~Length|Area, badger)

#grouping structure can be visualised using the package "lattice' 
#does the same as ggplot but was an older version
library(lattice)
xyplot(Weight ~Length|Area, data=badger, type=c("p","smooth"))


#mixed model is compiled using the lme function rather than the glm function as there are some NA values in the dataset na.action is set to na.exclude
model_lme<-lme(Weight ~Length + Sex, random = ~1|Area, data =badgerG, na.action=na.exclude)
summary(model_lme)

#residuals can be plotted in a number of different ways to visualise the model assumptions

res_lme=residuals(model_lme)
plot(res_lme)

qqnorm(res_lme)
qqline(res_lme)

plot(model_lme)
#same as
plot(model_lme, resid(.) ~Length, albine =0)

qqnorm(model_lme, ~resid(.), id=0.10, cex=0.7)

qqnorm(model_lme, ~ranef(.), id=0.10, cex=0.7)

plot(ranef(model_lme))

