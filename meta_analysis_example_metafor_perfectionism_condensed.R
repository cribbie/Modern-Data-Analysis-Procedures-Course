#### Introduction ####

#Meta Analysis Example

#Difference between Males and Females in Maladaptive Perfectionism


#Open and view the dataset
dat<-read.csv(file.choose())
names(dat)
head(dat)

library(metafor)



#### Fixed Effects Model ####

#Fixed Effects Model (method="FE")
#Raw mean difference (measure="MD")

ma1<-rma(data=dat,
         n1i=N_male,
         n2i=N_female,
         sd1i=SD_male,
         sd2i=SD_female,
         m1i=M_male,
         m2i=M_female,
         method="FE",
         slab=Study,
         measure="MD")  #MD = mean difference
summary(ma1)

#Fixed Effects Model (method="FE") 
#Standardized Mean Difference (measure="SMD")

ma2<-rma(data=dat,
         n1i=N_male,
         n2i=N_female,
         sd1i=SD_male,
         sd2i=SD_female,
         m1i=M_male,
         m2i=M_female,
         method="FE",
         slab=Study,
         measure="SMD") #SMD = standardized mean difference
summary(ma2)




#### Random Effects Model ####

#Random Effects Model
#Restricted Maximum Likelihood Estimation (method="REML")
#Raw Mean Difference (measure="MD")

ma3<-rma(data=dat,
         n1i=N_male,
         n2i=N_female,
         sd1i=SD_male,
         sd2i=SD_female,
         m1i=M_male,
         m2i=M_female,
         method="REML",
         slab=Study,
         measure="MD")
summary(ma3)

#Random Effects Model
#Restricted Maximum Likelihood Estimation (method="REML")
#Standardized Mean Difference (measure="SMD")

ma4<-rma(data=dat,
         n1i=N_male,
         n2i=N_female,
         sd1i=SD_male,
         sd2i=SD_female,
         m1i=M_male,
         m2i=M_female,
         method="REML",
         slab=Study,
         measure="SMD")
summary(ma4)




#### Diagnostics on the Random Effect Model ####

#Forest Plot for the Standardized Mean Difference
forest(ma4,level=95)

#Identify Influential Cases
ic<-influence(ma4)
plot(ic)
(cd_cut<-4/(length(dat$Study)))
any(ic$inf$cook.d > cd_cut)

#Influential Cases
#One at a time leave out a study
#Plot the effect sizes from each analsysis
plot(leave1out(ma4)$estimate)

#Publication Bias - Funnel Plot
funnel(ma4)
regtest(ma4) #Test for Funnel Plot Asymmetry

#Explore the effect of Filling in Studies 
#Missing Due to Publication Bias
tf<-trimfill(ma4)
tf
funnel(tf)

# Original and Trim & Fill Confidence Intervals on
# Cohen's D
c(ma4$ci.lb,ma4$ci.ub)
c(tf$ci.lb,tf$ci.ub)

#Four helpful plots with one command
plot(ma4)



#### Moderator Analyses ####

library(ggplot2)

#Perfectionism Measure as a Moderator of Effect Size
boxplot(dat_es$yi ~ dat_es$Perf_Measure)

#Quality as a Moderator of Effect Size?
ggplot(data=dat_es, aes(x=Quality,y=yi)) + geom_point(size=5)


#Random Effects Model
#Restricted Maximum Likelihood Estimation 
#(method="REML")
#Standardized Mean Difference (measure="SMD")
#With moderator "Perfectionism Measure"

ma6<-rma(data=dat,
         n1i=N_male,n2i=N_female,
         sd1i=SD_male,sd2i=SD_female,
         m1i=M_male,m2i=M_female,
         method="REML",measure="SMD",
         mods=~Perf_Measure)
summary(ma6)

#Forest Plot with Fitted Values
forest(ma6)
forest(ma6,order="obs")

plot(ma6)



#Restricted Maximum Likelihood Estimation (method="REML")
#Standardized Mean Difference (measure="SMD")
#With moderator centered "Study Quality"
#(Centering makes the intercept interpretable; i.e.,
#the diff b/w Ms and Fs when Quality = Mean = 0)
dat$Quality_c<-dat$Quality-mean(dat$Quality)

ma8<-rma(data=dat,
         n1i=N_male,n2i=N_female,
         sd1i=SD_male,sd2i=SD_female,
         m1i=M_male,m2i=M_female,
         method="REML",measure="SMD",
         mods=~Quality_c)
summary(ma8)



#Random Effects Model
#Restricted Maximum Likelihood Estimation (method="REML")
#Raw Mean Difference (measure="SMD")
#With moderators "Study Quality" and "Perf_Measure"

ma9<-rma(data=dat,
         n1i=N_male,n2i=N_female,
         sd1i=SD_male,sd2i=SD_female,
         m1i=M_male,m2i=M_female,
         method="REML",measure="SMD",
         mods=~Quality+Perf_Measure)
summary(ma9)

