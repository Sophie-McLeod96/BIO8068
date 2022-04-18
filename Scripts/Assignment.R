# Analysis of Hunting Data from Sweden 1819-1919#
# name: Sophie Carolan

# Nov 2021

# use this code to load the data 
#AIM: investigate trends in bags of top predators and mesopredators through time

wants <- c("ggfortify", "here", "nlme", "lattice","ggplot2", "MASS")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])


library(nlme)
library(lattice)
library(MASS)
library(here)
library(ggplot2)
library(ggfortify)
library(patchwork)


rawd<-read.csv(here("Data", "sweden_simple.csv"))
head(rawd) # shows first 6 rows to check you have right data set


#Data exploration ####
#plot the number of red fox shot 

fox_trends.gls<- gls(log(red)~time+name,rawd)
summary(fox_trends.gls)

xyplot(exp(fitted(fox_trends.gls))+red~time|name, rawd)

truehist(residuals(fox_trends.gls))
plot(ACF(fox_trends.gls))

fox_trends_auto.gls<-gls(log(red)~time+name, correlation=corAR1(form=~1|name),rawd)
summary(fox_trends_auto.gls)

#use lme function to estimate the error due to country --> random effect
red_fox.lme<-lme(log(red)~time, random=~1|name,rawd)
summary(red_fox.lme)
#look at standard deviation for the ransom effect and compare it to that or remaining residuals
#STdev = 0.911 and residual is 0.578
#can account for 0.911/(0.911+0+578)= 0.612 = 61% of the error/residuals because we have accounted for random effects in the county measurements of bag

#look at predicted vs observed ####
xyplot(exp(fitted(red_fox.lme))+red~time|name,rawd)




#red fox lme ####

red_fox_auto.lme<-lme(log(red)~time, random=~1|name,correlation=corAR1(form=~1|name), rawd)  
summary(red_fox_auto.lme)
red_fox_auto.lme

devred<-lme(red~1, random =~1| name, rawd)
summary(devred)


residualred<-qqnorm(red_fox_auto.lme, ~resid(.,type="p")|name, abline=c(0,1))
residualred
summary(residualred)
qqline(red_fox_auto.lme)
??nlme
fitRed<-fitted(red_fox_auto.lme)

#wolf lme ####
wolf_auto.lme <-lme(log(wolf+1)~time, random=~1|name, correlation=corAR1(form=~1|name), rawd)
summary(wolf_auto.lme)
fitWolf<-fitted(wolf_auto.lme)

#lynx lme ####
lynx_auto.lme <-lme(log(lynx+1)~time, random=~1|name, correlation=corAR1(form=~1|name), rawd)
summary(lynx_auto.lme)
fitLynx<-fitted(lynx_auto.lme)

#seed lme ####
seed_auto.lme <-lme(log(seed+1)~time, random=~1|name, correlation=corAR1(form=~1|name), rawd)
summary(seed_auto.lme)
fitSeed<-fitted(seed_auto.lme)


#pred lme? 
rawd$pred<-rawd$lynx+rawd$wolf
pred_auto.lme <-lme(log(pred+1)~time, random=~1|name, correlation=corAR1(form=~1|name), rawd)
summary(pred_auto.lme)
fitPred<-fitted(pred_auto.lme)

xyplot(exp(fitted(pred_auto.lme))+pred~time|name,rawd)

#poplme 
pop_auto.lme <-lme(log(pop+1)~time, random=~1|name, correlation=corAR1(form=~1|name), rawd)
summary(pop_auto.lme)
fitPop<-fitted(pop_auto.lme)

# making the three in one for everywhere graph

library(ggplot2)


mean(red~time,rawd)

head(rawd,6)  

str(rawd)

library(dplyr)
g_year <- group_by(rawd, time)
mean_fox<- summarise(g_year, mean (red))       
mean_fox


redp2 <- ggplot(mean_fox, aes(x=time, y='mean(red)'))+ ylim(300)+
  geom_point(size=1) +
  theme_classic()
redp2 


names(mean_fox)[names(mean_fox)=="mean(red)"] <- "fox"
names(mean_fox)
 foxplot<- plot(mean_fox, fox~time)
 
 
 library(dplyr)
 g_year <- group_by(rawd, time)
 mean_all<- summarise(g_year, mean (red), mean(lynx), mean(wolf), mean(seed), mean(pop))       
 mean_all
 
 

 
 
 names(mean_all) <-c("time","red", "lynx", "wolf","seed", "pop")
 names(mean_all)
 
 foxplot<-plot(red~time, mean_all) 
 lynxplot<- plot(lynx~time,mean_all) 
 wolfplot<- plot(wolf~time,mean_all)
 
 seedplot<- plot(seed~time,mean_all)
 popplot<- plot(pop~time,mean_all)
lynxplot
library(patchwork)

foxplot+lynxplot+wolfplot

#predator plot ####
redlynx<-ggplot(mean_all,aes(x = time)) + 
  geom_point(aes(y=red, color="#8c3928"), size=2)+
  geom_point(aes(y=lynx*30, color="#dcc727"), size= 2)+
  geom_point(aes(y=wolf*30, color="#568125"), size =2)+
  labs( x=  "Time in Years", y= "Red Fox",
             title= "Population Density of Red Fox, Lynx and Wolves /1000km^2 in Sweden between 1829 and 1917",
        plot.title = element_text(vjust = - 10))+
  scale_color_identity(name = "Species",
                        breaks = c("#8c3928", "#dcc727", "#568125"),
                        labels = c("Red Fox", "Lynx", "Wolf"),
                        guide = "legend")+
  theme_classic()+
  coord_cartesian(ylim = c(0, 120), expand = c(0, 0)) + #needed to determine the red one
  scale_y_continuous(sec.axis = sec_axis(~./30, name = "Predator"))#sorts the lynx scale
redlynx

#by putting the colour inside the aes it makes it something you can sort into a legend

#seed and population plot ####
seedpopplot<-ggplot(mean_all,aes(x = time)) + 
  geom_point(aes(y=seed/1000, color="#c90d7b"), size=2)+
  geom_point(aes(y=pop*0.7, color="#0cc99f"), size= 2)+
  labs( x=  "Time in Years", y= "Seed (1000 barrels)",
        title= "Population and Seed density/1000km2 in Sweden between 1829 and 1917")+
  scale_color_identity(name = "",
                       breaks = c("#c90d7b", "#0cc99f"),
                       labels = c("Seed Use", "Population"),
                       guide = "legend")+
  theme_classic()+
  coord_cartesian(ylim = c(0, 22), expand = c(0, 0)) + #needed to determine the red one
  scale_y_continuous(sec.axis = sec_axis(~(./7) *10, name = "Population (1000 people)"))
seedpopplot



#kinda does what I want - but unsure its using the correct lme####
#Red fox 
red_lmeplt<- ggplot(rawd, aes(time,red)) + 
  geom_point(size =0.75, color= "#8c3928") + 
  facet_wrap(~name) +
  labs(title = "Trend of Red Fox killings between 1829-1917 by County in Sweden ") + 
  scale_y_continuous() + 
  ylab("Red Fox Killings per 1000km^2")+
  scale_x_continuous() + 
    xlab("Time in Years")+
  theme_classic() + 
  geom_line(fitRed, color = "#0f0371")
red_lmeplt  


  
#lynx
lynx_lmeplt<- ggplot(rawd, aes(time,lynx)) + 
  geom_point(size =0.75, color= "#dcc727") + 
  facet_wrap(~name) +
  labs(title = "Trend of Lynx Killings between 1829-1917 by County in Sweden ") + 
  scale_y_continuous() + 
  ylab("Lynx Killings per 1000km^2")+
  scale_x_continuous() + 
  xlab("Time in Years")+
  theme_classic() + 
  geom_line(fitLynx, color = "#dcc727")
lynx_lmeplt  

#wolf
wolf_lmeplt<- ggplot(rawd, aes(time,wolf)) + 
  geom_point(size =0.75, color= "#568125") + 
  facet_wrap(~name) +
  labs(title = "Trend of Wolf Killings between 1829-1917 by County in Sweden ") + 
  scale_y_continuous() + 
  ylab("Wolf Killings per 1000km^2")+
  scale_x_continuous() + 
  xlab("Time in Years")+
  theme_classic() + 
  geom_line(fitWolf, color = "#568125")
wolf_lmeplt  


#seed
seed_lmeplt<- ggplot(rawd, aes(time,seed)) + 
  geom_point(size =0.75, color= "#c90d7b") + 
  facet_wrap(~name) +
  labs(title = "Trend of Seed planting density between 1829-1917 by County in Sweden ") + 
  scale_y_continuous() + 
  ylab("Seed Planting per 1000km^2")+
  scale_x_continuous() + 
  xlab("Time in Years")+
  theme_classic() + 
  geom_line(fitSeed, color = "#c90d7b")
seed_lmeplt  

#merging the wolf and lynx
rawd$pred <- rawd$lynx+rawd$wolf
sweden_simple$pred <-sweden_simple$lynx+sweden_simple$wolf
head(rawd)
pred<-data(rawd,pred)

#pred

predator_lme<-ggplot(rawd,aes(time,pred)) + 
  geom_point(size =0.75, color= "#568125") + 
  facet_wrap(~name) +
  labs(title = "Trend of Top Predator Killings between 1829-1917 by County in Sweden ") + 
  scale_y_continuous() + 
  ylab("Top Predator Killings per 1000km^2")+
  scale_x_continuous() + 
  xlab("Time in Years") +
  theme_classic() + 
  geom_line(fitPred, color = "#568125")
predator_lme


#pop lme####
pop_lmeplt<- ggplot(rawd, aes(time,pop)) + 
  geom_point(size =0.75, color= "#0cc99f") + 
  facet_wrap(~name) +
  labs(title = "Trend of population density between 1829-1917 by County in Sweden  ") + 
  scale_y_continuous() + 
  ylab("Population per 1000km^2")+
  scale_x_continuous() + 
  xlab("Time in Years")+
  theme_classic()+
  geom_line(fitPop, color = "#0f0371",size=0.5)
pop_lmeplt




# HALF WAY THERE 3 species but no trends####
pred_county<-ggplot(rawd,aes(x = time)) + 
  geom_point(aes(y=red, color="#8c3928"), size=0.5)+
  geom_point(aes(y=lynx*30, color="#dcc727"), size= 0.5)+
  geom_point(aes(y=wolf*30, color="#568125"), size =0.5)+
  labs( x=  "Time in Years", y= "Red Fox density/1000km^2",
        title= "Population density of Red Fox, Lynx and Wolves /1000km2 in Sweden")+
  scale_color_identity(name = "Species",
                       breaks = c("#8c3928", "#dcc727", "#568125"),
                       labels = c("Red Fox", "Lynx", "Wolf"),
                       guide = "legend")+
  facet_wrap(~name) +
  labs(title = "Trend of Red Fox, Lynx and Wolf from 1829-1917 by County in Sweden") + 
  theme_classic()+
  coord_cartesian(ylim = c(0, 120), expand = c(0, 0)) + #needed to determine the red one
  scale_y_continuous(sec.axis = sec_axis(~./30, name = "Top Predator density /1000km^2"))+#sorts the lynx scale
scale_x_continuous()
pred_county

#halfway seed?
seedpopplot<-ggplot(rawd,aes(x = time)) + 
  geom_point(aes(y=seed/1000, color="#c90d7b"), size=0.75)+
  geom_point(aes(y=pop*0.7, color="#0cc99f"), size= 0.75)+
  labs( x=  "Time in Years", y= "Seed Use per 1000km^2",
        title= "Population and Seed Trend from 1829-1917 by County in Sweden")+
  scale_color_identity(name = "",
                       breaks = c("#8c3928", "#dcc727"),
                       labels = c("Seed use", "population"),
                       guide = "legend")+
  facet_wrap(~name) +
  theme_classic()+
  coord_cartesian(ylim = c(0, 22), expand = c(0, 0)) + #needed to determine the red one
  scale_y_continuous(sec.axis = sec_axis(~(./7) *10, name = "Population/1000km^2"))
seedpopplot





#Trying a model like the bird model
data("sweden_simple")
M1.red<- glm(red~wolf+lynx+seed+time, data=rawd)
summary(M1.red)
plot(M1.red)



#residuals?####
#red

redres_lme=residuals(red_fox_auto.lme) 
plot(redres_lme)
qqnorm(redres_lme)
qqline(redres_lme) 

qqnorm(red_fox_auto.lme, ~resid(.), id=0.10, cex=0.7)

redres_lme
summary(redres_lme)


#wolf
wolfres_lme=residuals(wolf_auto.lme) 
plot(wolfres_lme)
qqnorm(wolfres_lme)
qqline(wolfres_lme) 

qqnorm(wolf_auto.lme, ~resid(.), id=0.10, cex=0.7)



#lynx
lynxres_lme=residuals(lynx_auto.lme) 
plot(lynxres_lme)
qqnorm(lynxres_lme)
qqline(lynxres_lme) 

qqnorm(lynx_auto.lme, ~resid(.), id=0.10, cex=0.7)


#seed
seedres_lme=residuals(seed_auto.lme) 
plot(seedres_lme)
qqnorm(seedres_lme)
qqline(seedres_lme) 


qqnorm(seed_auto.lme, ~resid(.), id=0.10, cex=0.7)

install.packages("ggfortify")
library(ggplot2)
library(ggfortify)
autoplot(red_fox_auto.lme, smooth.colour =NA)



#plots??####
plot(red_fox_auto.lme)
plot(wolf_auto.lme)
plot(lynx_auto.lme)
plot(seed_auto.lme)


install.packages("ggpubr")
library(ggpubr)
ggqqplot(rawd$year)
ggqqplot(rawd$red)
