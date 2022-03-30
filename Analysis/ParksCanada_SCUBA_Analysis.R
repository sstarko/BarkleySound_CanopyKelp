
#This code generates plot of trophic cascade between 2013 and 2016 at Parks Canada sites

#Load packages
library(readxl)
library(tidyverse)
library(cowplot)
library(glmmTMB)
library(nlme)
library(lsmeans)
library(rstatix)
library(PMCMR)

## Create function to add half of the minimum non-zero value to zeroes for gamma distribution
gammadd <- function(x) {
  ifelse(x > 0, x, x + 0.5*min(x[x > 0]))
}


##########################
#####SEASTARS#############
##########################

#Load seastar data
d<-read_excel("./data/ParksCanada_sites/PRNPR Seastar 2013-2016.xlsx", sheet = "Site-description")

#Averages
d.avg <- d %>% group_by(Site, Year) %>%
  summarise(Pycno = mean(SUST))

#Transect-level replicates
d.avg_tr <- d %>% group_by(Site, Year, `Transect #`) %>%
  summarise(Pycno = mean(SUST))

#Transform Pycno data (not actually used in models)
d.avg2 <- d.avg %>% group_by(Year) %>%
  summarise(mean = mean(Pycno), se = sqrt(var(Pycno))/sqrt(5))

#Plot Pycnopodia data
ss_plot<-ggplot(d.avg2, aes(x = Year, y = mean))+
  geom_point(size = 3)+
  theme_cowplot()+
  ylim(c=0.2,6)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2)+
  geom_line(size = 0.2, type = 2)+
  ylab("Pycnopodia density")

d.avg$Year <- as.factor(d.avg$Year)
d.avg$Pycno2 <- log10(d.avg$Pycno + 1)

#Adjust LSmeans method to work with glmmTMB
source(system.file("other_methods","lsmeans_methods.R",package="glmmTMB"))

####GLMM models######

#Negative binomial
glmmTMB(Pycno ~ as.factor(Year) + (1|Site), data = d.avg_tr, family = nbinom2) %>% Anova()
glmmTMB(Pycno ~ as.factor(Year) + (1|Site), data = d.avg_tr, family = nbinom2) %>% emmeans(~as.factor(Year)) %>% plot()


##POISSON MODEL (note that negative binomial was used in the end)
#Pycnopodia abundance differs by year
glmmTMB(Pycno ~ as.factor(Year) + (1|Site), data = d.avg_tr, family = poisson) %>% Anova()

#Glmer method
Pycno_mod<- lme4::glmer(Pycno ~ as.factor(Year) + (1|Site), data = d.avg_tr, family = poisson)
rstatix::Anova(Pycno_mod)

#Compare means
emmeans(Pycno_mod, ~as.factor(Year)) %>% plot()

plot(resid(Pycno_mod))
#Residuals look acceptable with a single outlier

#Zero inflation makes no difference
glmmTMB(Pycno ~ as.factor(Year) + (1|Site), data = d.avg_tr, family = poisson, zi = ~.) %>% Anova()

#Comparing means on zero inflated model
glmmTMB(Pycno ~ as.factor(Year) + (1|Site), data = d.avg_tr, family = poisson) %>% emmeans(~as.factor(Year)) %>% plot()

#Run non-parametric comparisons on mean counts
d.avg$Site <- d.avg$Site %>% as.factor()
friedman.test(d.avg$Pycno, d.avg$Year, d.avg$Site)
posthoc.friedman.conover.test(d.avg$Pycno, d.avg$Year, d.avg$Site)


##############################
#########KELP#################
##############################

#Load in kelp data
parks<-read.csv("./data/ParksCanada_sites/Kelp-density-2013-2016.csv")

#Filter and summarize data
#Averages
parks.summary<-parks %>% 
  group_by(Species,Site,Year) %>%
  summarize(mean.counts=mean(Count_persqm),
            sd.counts=sd(Count_persqm))

#Transect-level replicates
parks.summary2<-parks %>% 
  group_by(Species,Site,Year, Transect) %>%
  summarize(mean.counts=mean(Count_persqm),
            sd.counts=sd(Count_persqm))

#Macrocystis only with transect-level replication replication
MACRO_trans <- filter(parks.summary2, Species == "MACRO")

hist(MACRO_trans$mean.counts)

#Housekeeping...
parks.summary$ln.counts<-log(parks.summary$mean.counts+1,2)

Macro.summary$Site <- Macro.summary$Site %>% as.factor()

# change "DK[space]" to "DK" 
parks.summary$Site <- gsub( "DK " , "DK", parks.summary$Site )
Macro.summary <- parks.summary[parks.summary$Species=="MACRO",]
Macro.summary2 <- Macro.summary %>% group_by(Year) %>%
  summarise(mean = mean(mean.counts), se = sqrt(var(mean.counts))/sqrt(5))

par(mfrow=c(1,1))

#Plot data
MP_plot<-ggplot(Macro.summary2, aes(x = Year, y = mean))+
  geom_point(size = 3)+
  theme_cowplot()+
  ylim(c=0,3)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2)+
  geom_line(size = 0.2)+
  ylab("Macrocystis stipe density")


##GLMM models comparing kelp abundance versus year
#Use gammadd function(defined above) to add small number to each zero, so as to fit the gamma model
kelp_mod<- lme4::glmer(gammadd(mean.counts) ~ as.factor(Year) + (1|Site), data = MACRO_trans, family = Gamma) 

kelp_mod %>% rstatix::Anova(type = 3, test.statistic = "F")
kelp_mod %>% emmeans(~as.factor(Year)) %>% plot()
kelp_mod %>% resid() %>% plot()

MACRO_trans$Site <- as.factor(MACRO_trans$Site )

#GLMM with glmmADMB yields same results
kelp_mod<- glmmADMB::glmmadmb(gammadd(mean.counts) ~ as.factor(Year) + (1|Site), data = MACRO_trans, family = "Gamma") 

#Alternative with glmmTMB yields same results
glmmTMB(mean.counts+0.01 ~ as.factor(Year) + (1|Site), data = MACRO_trans, family = Gamma) %>% rstatix::Anova()


#Test for normality in site-level means
shapiro.test(Macro.summary$mean.counts[d.avg$Year=="2016"])

#Friedman's test on site-level means
friedman.test(Macro.summary$mean.counts, Macro.summary$Year, Macro.summary$Site)
posthoc.friedman.conover.test(Macro.summary$mean.counts, Macro.summary$Year, Macro.summary$Site)



#############################
###########URCHINS###########
#############################

#Load in urchin data (averages)
urch<-read_excel("./data/ParksCanada_sites/PRNPR Red Sea Urchin 2013-2016.xlsx", sheet = "Density")

#Load in urchin data (raw data)
urch_quad <- read_excel("./data/ParksCanada_sites/PRNPR_urchin_subtidal_2013-2016.xlsx", sheet = "Macroinvert")

urch.sum <- urch %>% group_by(Year) %>%
  summarise(mean = mean(`Ave density 60m2`), se =  sqrt(var(`Ave density 60m2`))/sqrt(5))

urchin_plot<-ggplot(urch.sum, aes(x = Year, y = mean))+
  geom_point(size = 3)+
  theme_cowplot()+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2)+
  geom_line(size = 0.2)+
  ylab("Red urchin density")

plot_grid(ss_plot, urchin_plot, MP_plot, ncol =1)


urch$Site <- as.factor(urch$Site)
urch$Year <- as.factor(urch$Year)
names(urch)[3] <- "density"

#By transect
urch_quad$Year <- as.factor(urch_quad$Year)

urch_trans <- urch_quad %>% 
  group_by(Site, Year, Transect) %>%
  summarize(count_total = sum(Abundance))

plot(count_total ~ Year, data = urch_trans)
hist(urch_trans$count_total, breaks = seq(0,200,1))

##Negative binomial model

kelp_mod<- glmmTMB(count_total ~ as.factor(Year) + (1|Site), data = urch_trans, family = nbinom1) 
kelp_mod %>% rstatix::Anova(type = 3)
kelp_mod %>% emmeans(~as.factor(Year)) %>% plot()
kelp_mod %>% resid() %>% plot()

##Alternative models (e.g., Poisson)
kelp_mod<- lme4::glmer(count_total ~ as.factor(Year) + (1|Site), data = urch_trans, family = poisson) 

kelp_mod %>% rstatix::Anova(type = 3, test.statistic = "F")
kelp_mod %>% emmeans(~as.factor(Year)) %>% plot()
kelp_mod %>% resid() %>% plot()


kelp_mod<- glmmTMB(count_total ~ as.factor(Year) + (1|Site), data = urch_trans, family = poisson, zi = ~.) 
kelp_mod %>% rstatix::Anova(type = 3)
kelp_mod %>% emmeans(~as.factor(Year)) %>% plot()

kelp_mod %>% resid() %>% plot()
urch_trans$count_total %>% var()

