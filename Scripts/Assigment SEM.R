




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
library(piecewiseSEM)
library(multicompView)
library(nloptr)

rawd<-read.csv(here("Data", "sweden_simple.csv"))
head(rawd) # shows first 6 rows to check you have right data set



??piecewiseSEM

#doesnt work
sem.m1<-  psem(
  lme(red~seed+pop+lynx+wolf+county, random = ~1|name, na.action = na.omit, data = rawd),
  lme(seed~lynx+wolf+pop+county, random = ~1|name, na.action = na.omit,data = rawd),
  lme(lynx~ wolf+pop+county, random = ~1|name, na.action = na.omit, data = rawd),    
  lme(wolf~ lynx+pop+county, random = ~1|name, na.action = na.omit, data = rawd),         
  lme(pop~county,random = ~1|name,data = rawd)
)
sem.m1
summary(sem.m1)

plot(sem.m1)


#Sem 2 has sheep no wolf

sem.m2 = psem(
  
  lme(red~seed+pop+lynx+sheep ,random = ~1|name, na.action = na.omit, data = rawd), 
  lme(seed~pop+lynx, random = ~1|name, na.action = na.omit,data = rawd),                       
  lme(lynx~ pop+north, random = ~1|name, na.action = na.omit, data = rawd),         
  lme(pop~north,random = ~1|name,data = rawd),
  lme(sheep~lynx+pop+north+seed, random=~1|name, data=rawd)
)
sem.m2
summary(sem.m2)

plot(sem.m2)

#sem 3 doesn't work

sem.m3 = psem(
  
  lme(red~seed+pop+lynx+wolf ,random = ~1|name, na.action = na.omit, data = rawd), 
  lme(seed~pop+lynx, random = ~1|name, na.action = na.omit,data = rawd),                       
  lme(lynx~ pop+north+seed, random = ~1|name, na.action = na.omit, data = rawd),         
  lme(pop~north,random = ~1|name,data = rawd),
  lme(wolf~pop+north+seed, random=~1|name, data=rawd)
)
sem.m3
summary(sem.m3)
plot(sem.m3)

#WORKS!####
sem.m4 = psem(
  
  lme(red~seed+pop+wolf+lynx ,random = ~1|name, na.action = na.omit, data = rawd), 
  lme(seed~pop+north + east, random = ~1|name, na.action = na.omit,data = rawd),                       
  lme(wolf~ pop+north+east+ red, random = ~1|name, na.action = na.omit, data = rawd),         
  lme(pop~north+east,random = ~1|name,data = rawd),
  lme(lynx~pop+north+east+ red,random = ~1|name, na.action = na.omit, data = rawd)
)
sem.m4
summary(sem.m4)

plot(sem.m4)

#removing the non significants
sem.m5 = psem(
  
  lme(red~seed+pop+wolf+lynx ,random = ~1|name, na.action = na.omit, data = rawd), 
  lme(seed~pop, random = ~1|name, na.action = na.omit,data = rawd),                       
  lme(wolf~ pop+north+red, random = ~1|name, na.action = na.omit, data = rawd),         
  lme(pop~north,random = ~1|name,data = rawd),
  lme(lynx~pop+north+ red,random = ~1|name, na.action = na.omit, data = rawd)
)
sem.m5
summary(sem.m5)

plot(sem.m5)

anova(sem.m2, sem.m4, sem.m5)


#merging the wolf and lynx
rawd$pred <- rawd$lynx+rawd$wolf

#sem6 merging the predators
sem.m6 = psem(
  
  lme(red~seed+pop+pred ,random = ~1|name, na.action = na.omit, data = rawd), 
  lme(seed~pop+north, random = ~1|name, na.action = na.omit,data = rawd),                       
  lme(pred~ pop+north+red+seed, random = ~1|name, na.action = na.omit, data = rawd),         
  lme(pop~north,random = ~1|name,data = rawd)
)
sem.m6
summary(sem.m6)

plot(sem.m61)
sem.m61 = psem(
  
  lme(red~seed+pop+pred ,random = ~1|name, na.action = na.omit, data = rawd), 
  lme(seed~pop+north, random = ~1|name, na.action = na.omit,data = rawd),                       
  lme(pred~ pop+north+seed, random = ~1|name, na.action = na.omit, data = rawd),         
  lme(pop~north,random = ~1|name,data = rawd)
)
sem.m61
summary(sem.m61)

plot(sem.m61)

anova(sem.m5,sem.m6)

plot(sem.m6,
return = FALSE,
node_attrs = data.frame(shape = "circle", color = "grey", fillcolor = "grey", fontcolor ="white"),
edge_attrs = data.frame(style = "solid", color = "grey", arrowsize=0.5, penwidth=0.5),
ns_dashed = T,
alpha = 0.05,
show = "std",
digits = 3,
add_edge_label_spaces = TRUE
)


#sem 7 without pop
sem.m7 = psem(
  
  lme(red~seed+pred ,random = ~1|name, na.action = na.omit, data = rawd), 
  lme(seed~north, random = ~1|name, na.action = na.omit,data = rawd),                       
  lme(pred~ north+red+seed, random = ~1|name, na.action = na.omit, data = rawd)
)
sem.m7
summary(sem.m7)

plot(sem.m7,
     return = FALSE,
     node_attrs = data.frame(shape = "circle", color = "grey", fillcolor = "grey", fontcolor ="white"),
     edge_attrs = data.frame(style = "solid", color = "grey", arrowsize=0.5, penwidth=0.5),
     ns_dashed = T,
     alpha = 0.05,
     show = "std",
     digits = 3,
     add_edge_label_spaces = TRUE
)
plot(sem.m7)
anova(sem.m5,sem.m6, sem.m7)


sem.m8 = psem(
  lme(red~seed+pred ,random = ~1|name, na.action = na.omit, data = rawd), 
  lme(seed~north, random = ~1|name, na.action = na.omit,data = rawd),                       
  lme(pred~ north+seed, random = ~1|name, na.action = na.omit, data = rawd)
)
sem.m8
summary(sem.m8)
anova(sem.m6, sem.m7, sem.m8)
plot(sem.m8)

plot(sem.m8,
     return = FALSE,
     node_attrs = data.frame(shape = "circle", color = "grey", fillcolor = "grey", fontcolor ="white"),
     edge_attrs = data.frame(style = "solid", color = "grey", arrowsize=0.5, penwidth=0.5),
     ns_dashed = T,
     alpha = 0.05,
     show = "std",
     digits = 3,
     add_edge_label_spaces = TRUE
)
#GGgalley to make a correlation matrix
install.packages("GGally")
library(GGally)
ggcorr(rawd,
       label=TRUE,
       label_alpha= 0.5)

#new data frame

rawd2<- (rawd$county, rawd$pop, rawd$red, rawd$lynx, rawd$wolf,rawd$pred, rawd$seed, rawd$north)

#pearsons correlation
#seed vs north
seednorth<-cor.test(rawd$north, rawd$seed, method="pearson")
seednorth

rednorth <-cor.test(rawd$north, rawd$red, method="pearson")
rednorth

rawd$pred<-rawd$lynx+rawd$wolf
prednorth <-cor.test(rawd$north, rawd$pred, method="pearson")
prednorth

popnorth <- cor.test(rawd$north, rawd$pop, method="pearson")
popnorth

redseed<- cor.test(rawd$red, rawd$seed, method="pearson")
redseed

redpred <-cor.test(rawd$red, rawd$pred, method="pearson")
redpred

popseed<-cor.test(rawd$pop, rawd$seed, method="pearson")
popseed

predseed <-cor.test (rawd$seed, rawd$pred, method="pearson")
predseed
