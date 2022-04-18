

# Analysis of Hunting Data from Sweden 1819-1919#
# name: Sophie Carolan

# Nov 2021

# use this code to load the data 

wants <- c("ggfortify", "here", "nlme", "lattice","ggplot2", "MASS")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])


library(nlme)
library(lattice)
library(MASS)
library(here)
# load data and check the top 6 rows. 


rawd<-read.csv(here("Data", "sweden_simple.csv"))
head(rawd)



###### copy and past your code below here.......

#Data exploration ####
#plot the number of red fox shot by country 
xyplot(red~time|name, rawd, typ=c("a","b"))

# conventional to model these as a log

#Analysing the Fox Bag
#are there any trends with time and county? To do this we use the gls function from MASS
fox_trends.gls<- gls(log(red)~time+name,rawd)
#to get the output
summary(fox_trends.gls)

#are any predictors significant? 
#YES no * to determine so need to look at numbers manually anything smaller than 0.05 is significant. 

#look at the fitted model by county

xyplot(exp(fitted(fox_trends.gls))+red~time|name, rawd)

#what about the residuals
truehist(residuals(fox_trends.gls))

#not allowed for autocorrelation in the model. = dependence of the response on previous years
#autocorrelation = acf function ####

plot(ACF(fox_trends.gls))

# can see that there is considerable autocorrelation. estimates of significance will be biased upwards
#adjust using the correlation statement
#have to define what sort of correlation exists
#here will use a simple autoregressive which assumes that part of the observed pattern is driven by the last measurement (here and the previous bag)
#we will also assume that this varies by county

fox_trends_auto.gls<-gls(log(red)~time+name, correlation=corAR1(form=~1|name),rawd)
summary(fox_trends_auto.gls)

#phi gives estimate of the serial dependence between measures
#assuming this means how dependant this year is on last year


# Comparing Models ####
#use AIC to compare models
#Akaike Information Criterion = metric used to compare the fit of several regression models
anova(fox_trends.gls,fox_trends_auto.gls)
#lower AIC number is better as means havent' got as much information unaccounted for

#mixed effect modelling ####

#difference in number of fox bags accross counties - don't know why so can make hypothesis on why? 
#unmeasured phenomena

#use lme function to estimate the error due to country --> random effect
red_fox.lme<-lme(log(red)~time, random=~1|name,rawd)
summary(red_fox.lme)
#look at standard deviation for the ransom effect and compare it to that or remaining residuals
#STdev = 0.911 and residual is 0.578
#can account for 0.911/(0.911+0+578)= 0.612 = 61% of the error/residuals because we have accounted for random effects in the county measurements of bag

#look at predicted vs observed ####
xyplot(exp(fitted(red_fox.lme))+red~time|name,rawd)

#investigate the residuals
#done this before in simple regression analyses
#qqnorm function used to abstract the residuals by county and plot a straight line representing that on which the points should fit
qqnorm(red_fox.lme, ~resid(.,type="p")|name, abline=c(0,1))
# p = pearsons correlation

#the autocorrelation that had been made with GLS can be identidied and added to the model by specifying the autocorrelation function
#corAR1 is the simplest

red_fox_auto.lme <-lme(log(red)~time, random=~1|name, correlation=corAR1(form=~1|name), rawd)
red_fox_auto.lme
xyplot(exp(fitted(red_fox_auto.lme))+red~time|name,rawd)

#now test to see if the model is better for inclusion of autocorrelation

anova(red_fox.lme,red_fox_auto.lme)


#repeat for the wolf and the lynx
#how do these results compare with those of the red fox
#note that you will have to add +1 to logged data for wolves/lynx as there are many zeroes
#you may choose to merge lynx and wolf into one variable top predators



# Wolf Data exploration  ####
#plot the number of red fox shot by country 
xyplot(wolf~time|name, rawd, typ=c("a","b"))

# conventional to model these as a log

#Analyzing the wolf Bag
#are there any trends with time and county? To do this we use the gls function from MASS
wolf_trends.gls<- gls(log(wolf+1)~time+name,rawd)
#to get the output
summary(wolf_trends.gls)

#are any predictors significant? 
#YES there are no * to determine so need to look at numbers manually anything smaller than 0.05 is significant. 

#look at the fitted model by county

xyplot(exp(fitted(wolf_trends.gls))+wolf~time|name, rawd)

#what about the residuals
truehist(residuals(wolf_trends.gls))

#not allowed for autocorrelation in the model. = dependence of the response on previous years
#autocorrelation = acf function ####

plot(ACF(wolf_trends.gls))

# can see that there is considerable autocorrelation. estimates of significance will be biased upwards
#adjust using the correlation statement
#have to define what sort of correlation exists
#here will use a simple autoregressive which assumes that part of the observed pattern is driven by the last measurement (here and the previous bag)
#we will also assume that this varies by county

wolf_trends_auto.gls<-gls(log(wolf+1)~time+name, correlation=corAR1(form=~1|name),rawd)
summary(wolf_trends_auto.gls)

#phi gives estimate of the serial dependence between measures



# Comparing Models Wolf ####
#use AIC to compare models
#Akaike Information Criterion = metric used to compare the fit of several regression models
anova(wolf_trends.gls,wolf_trends_auto.gls)


#mixed effect modelling ####

#difference in number of fox bags accross counties - don't know why so can make hypothesis on why? 
#unmeasured phenomena

#use lme function to estimate the error due to country --> random effect
wolf.lme<-lme(log(wolf+1)~time, random=~1|name,rawd)
summary(wolf.lme)
#look at standard deviation for the ransom effect and compare it to that or remaining residuals
#STdev = 0.911 and residual is 0.578
#can account for 0.098/(0.098+0,301 = 0.612 = 24% of the error/residuals because we have accounted for random effects in the county measurements of bag

#look at predicted vs observed ####
xyplot(exp(fitted(wolf.lme))+wolf~time|name,rawd)

#investigate the residuals
#done this before in simple regression analyses
#qqnorm function used to abstract the residuals by county and plot a straight line representing that on which the points should fit
qqnorm(wolf.lme, ~resid(.,type="p")|name, abline=c(0,1))
# p = pearsons correlation

#the autocorrelation that had been made with GLS can be identidied and added to the model by specifying the autocorrelation function
#corAR1 is the simplest

wolf_auto.lme <-lme(log(wolf+1)~time, random=~1|name, correlation=corAR1(form=~1|name), rawd)
summary(wolf_auto.lme)
#now test to see if the model is better for inclusion of autocorrelation

anova(wolf.lme,wolf_auto.lme)

# Repeat for Lynx ####

#Data exploration Lynx ####
#plot the number of Lynx shot by country 
xyplot(lynx~time|name, rawd, typ=c("a","b"))

# conventional to model these as a log

#Analysing the Fox Bag
#are there any trends with time and county? To do this we use the gls function from MASS
lynx_trends.gls<- gls(log(lynx+1)~time+name,rawd)
#to get the output
summary(lynx_trends.gls)

#are any predictors significant? 
#YES no * to determine so need to look at numbers manually anything smaller than 0.05 is significant. 

#look at the fitted model by county

xyplot(exp(fitted(lynx_trends.gls))+lynx~time|name, rawd)

#what about the residuals
truehist(residuals(lynx_trends.gls))

#not allowed for autocorrelation in the model. = dependence of the response on previous years
#autocorrelation = acf function ####

plot(ACF(lynx_trends.gls))

# can see that there is considerable autocorrelation. estimates of significance will be biased upwards
#adjust using the correlation statement
#have to define what sort of correlation exists
#here will use a simple autoregressive which assumes that part of the observed pattern is driven by the last measurement (here and the previous bag)
#we will also assume that this varies by county

lynx_trends_auto.gls<-gls(log(lynx+1)~time+name, correlation=corAR1(form=~1|name),rawd)
summary(lynx_trends_auto.gls)

#phi gives estimate of the serial dependence between measures



# Comparing Models Lynx ####
#use AIC to compare models
#Akaike Information Criterion = metric used to compare the fit of several regression models
anova(lynx_trends.gls,lynx_trends_auto.gls)


#mixed effect modelling ####

#difference in number of lynx bags across counties - don't know why so can make hypothesis on why? 
#unmeasured phenomena

#use lme function to estimate the error due to country --> random effect
lynx.lme<-lme(log(lynx+1)~time, random=~1|name,rawd)
summary(lynx.lme)
#look at stadard deviation for the ransom effect and compare it to that or remaining residuals
#STdev = 0.133 and residual is 0.248
#can account for 0.133/(0.133+0,248)= 0.0.349 = 35% of the error/residuals because we have accounted for random effects in the county measurements of bag

#look at predicted vs observed ####
xyplot(exp(fitted(lynx.lme))+lynx~time|name,rawd)





#investigate the residuals
#done this before in simple regression analyses
#qqnorm function used to abstract the residuals by county and plot a straight line representing that on which the points should fit
qqnorm(lynx.lme, ~resid(.,type="p")|name, abline=c(0,1))
# p = pearsons correlation

#the autocorrelation that had been made with GLS can be identified and added to the model by specifying the autocorrelation function
#corAR1 is the simplest

lynx_auto.lme <-lme(log(lynx+1)~time, random=~1|name, correlation=corAR1(form=~1|name), rawd)
summary(lynx_auto.lme)
#now test to see if the model is better for inclusion of autocorrelation

anova(lynx.lme,lynx_auto.lme)



