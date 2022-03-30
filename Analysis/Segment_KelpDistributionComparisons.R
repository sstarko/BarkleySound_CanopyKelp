#Changes in kelp distribution inferred from shoreline segment data


#In this file, we run two types of analyses. 
#We test for an effect of distance from open coast on kelp persistance
#and we also fit models to kelp presence-absence along this gradient 
#at multiple time points. We note that only the logistic regressions 
#testing the initial hypothesis are included in the published paper but 
#the other models are interesting.

#Load libraries
library(readxl)
library(visTree)
library(tidyverse)
library(bbmle)
library(cowplot)
library(lme4)
library(arm)  


#First we compare 2018 and 2019 to 2007
#Import and format data

##Import 2018 data
shore.data<-read_excel("./data/kelp_comparisons/ShorezonePaired_2019.xlsx", sheet="Sheet1")
shore.data$HubDist2<-shore.data$HubDist/1000
shore.data$Change<-shore.data$Presence_2018-shore.data$Presence_2007
shore.data$Segment_number<-as.character(shore.data$Segment_number)

##Import 2019 data
shore.2019<-read_excel("./data/kelp_comparisons/ShoreZoneCCE2019.xlsx", sheet="RawShoreline")

shore.2019$Segment_number<-shore.2019$`2018 Transect #`
shore.2019.red<-tibble(shore.2019$Segment_number, shore.2019$Presence_2019,shore.2019$M_2019, shore.2019$N_2019 )
shore.2019.red$Segment_number<-as.character(shore.2019.red$`shore.2019$Segment_number`)
shore.2019.red$Presence_2019<-shore.2019.red$`shore.2019$Presence_2019`
shore.2019.red$M_2019<-shore.2019.red$`shore.2019$M_2019`
shore.2019.red$N_2019<-shore.2019.red$`shore.2019$N_2019`

#Join 2018 and 2019 data
shorezone_combined<-left_join(shore.2019.red,shore.data, by="Segment_number")
shorezone_combined$HubDist2<-shorezone_combined$HubDist/1000

#Import 1981 data
shorezone_1981 <- read_csv("./data/kelp_comparisons/1981_Shorezone_unit_data.csv")
#shore.data2 <- left_join(shore.data, shorezone_1981, by = "Segment_number")
#shore.data2$Change_1981v2018<-shore.data2$Presence_2018-shore.data2$kelp_1981_bin
#shore.data2$Segment_number<-as.character(shore.data2$Segment_number)

####BOTH SPECIES OF KELP combined #####
####BOTH SPECIES OF KELP#####
#Logistic regression
Both_2018 <- shore.data
Both_2018$Presence <- shore.data$Presence_2018
Both_2018$Year <- "2018"
Both_2018 <- Both_2018[,c("Segment_number", "HubDist2", "Presence", "Year") ]

Both_2007 <- shore.data
Both_2007$Presence <- shore.data$Presence_2007
Both_2007$Year <- "2007"
Both_2007 <- Both_2007[,c("Segment_number", "HubDist2", "Presence", "Year") ]


Both2 <- rbind(Both_2007, Both_2018)
Both_plot<- ggplot(Both2, aes(y = Presence, x = HubDist2, group = Year, color = Year))+
  geom_point(cex = 3)+
  theme_cowplot()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial))  +
  scale_color_manual(values=c(makeTransparent("#2165AC", 0.8), makeTransparent("#B63238", 0.8)))+ 
  #theme(legend.position = "none")+
  ylab(expression(paste("Probability of", " ",italic("Macrocystis"))))+
  xlab("Distance from edge mouth (km)")


Both_plot

Both2$Segment_number<-Both2$Segment_number %>% as.factor() 
Both2$Year<-Both2$Year %>% as.factor() 

bayesglm(Presence ~ HubDist2*Year, data = Both2) %>% summary()

n0 <- glmer(Presence ~ 1 + (1|Segment_number), data = Both2, family = "binomial") 
n1 <- glmer(Presence ~ HubDist2 + (1|Segment_number), data = Both2, family = "binomial")
n2 <- glmer(Presence ~ Year + (1|Segment_number), data = Both2, family = "binomial") 
n3 <- glmer(Presence ~ HubDist2*Year + (1|Segment_number), data = Both2, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
n4 <- glmer(Presence ~ HubDist2+Year + (1|Segment_number), data = Both2, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
AICctab(n0, n1, n2, n3,n4, weights = TRUE)  

summary(n3)                                                                        

Both_2007 <- filter(Both2, Year == "2007")
Both_2018 <- filter(Both2, Year == "2018")
colnames(Both_2018) <- c("Seg.2","HubDist2.2","Presence.2", "Year.2")
BothCom<-cbind(Both_2007, Both_2018)
BothCom$Change<-(as.numeric(BothCom$Presence) - as.numeric(BothCom$Presence.2))
BothCom2 <- filter(BothCom, Change > -1 ) %>% filter(Presence == 1)

##Test for probability of extirpation
glm(Change ~ HubDist2, data = BothCom2, family = "binomial") %>% summary()


ggplot(BothCom2, aes(y = Change, x = HubDist2))+
  geom_point(cex = 3)+
  theme_cowplot()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  #theme(legend.position = "none")+
  ylab(expression(paste("Probability of extirpation")))+
  xlab("Distance from edge mouth (km)")

##Nereocystis only

####LOGISTIC REGRESSIONS, one by one
####NEREOCYSTIS ###

##Create Nereocystis only dataframe
Nereo<-shore.data[shore.data$Nereo_not_na==1,]
Nereo<-drop_na(Nereo,1)
Nereo$Nereo_pres_2018<-as.numeric(ifelse(Nereo$N_2018>0,1,0))
Nereo$Nereo_pres_2007<-as.numeric(Nereo$Nereo_pres_2007)
Nereo2 <- Nereo

shorezone_combined<-drop_na(shorezone_combined, c(17:18))
shorezone_combined$Macro_pres_2019<-ifelse(shorezone_combined$M_2019>0,1,0)
shorezone_combined$Macro_pres_2019<-as.numeric(shorezone_combined$Macro_pres_2019)
shorezone_combined$Nereo_pres_2019<-ifelse(shorezone_combined$N_2019>0,1,0)
shorezone_combined$Nereo_pres_2019<-as.numeric(shorezone_combined$Nereo_pres_2019)

#Logistic regression
Nereo2_2018 <- Nereo
Nereo2_2018$Presence <- Nereo$Nereo_pres_2018
Nereo2_2018$Year <- "2018"
Nereo2_2018 <- Nereo2_2018[,c("Segment_number", "HubDist2", "Presence", "Year") ]

Nereo2_2007 <- Nereo
Nereo2_2007$Presence <- Nereo$Nereo_pres_2007
Nereo2_2007$Year <- "2007"
Nereo2_2007 <- Nereo2_2007[,c("Segment_number", "HubDist2", "Presence", "Year") ]


Nereo2 <- rbind(Nereo2_2007, Nereo2_2018)
Nereo_plot<-ggplot(Nereo2, aes(y = Presence, x = HubDist2, group = Year, color = Year))+
  geom_point(cex = 3)+
  theme_cowplot()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial))  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  #theme(legend.position = "none")+
  ylab(expression(paste("Probability of", " ",italic("Nereocystis"))))+
  xlab("Distance from edge mouth (km)")

Nereo_plot

Nereo2$Segment_number<-Nereo2$Segment_number %>% as.factor() 
Nereo2$Year<-Nereo2$Year %>% as.factor() 

bayesglm(Presence ~ HubDist2*Year, data = Nereo2) %>% AICc()

n0 <- glmer(Presence ~ 1 + (1|Segment_number), data = Nereo2, family = "binomial") 
n1 <- glmer(Presence ~ HubDist2 + (1|Segment_number), data = Nereo2, family = "binomial")
n2 <- glmer(Presence ~ Year + (1|Segment_number), data = Nereo2, family = "binomial") 
n3 <- glmer(Presence ~ HubDist2*Year + (1|Segment_number), data = Nereo2, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
n4 <- glmer(Presence ~ HubDist2+Year + (1|Segment_number), data = Nereo2, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

AICctab(n0, n1, n2, n3,n4, weights = TRUE)                               

summary(n3)                                                                        

Nereo2_2007.2 <- Nereo2_2007

Nereo2_2018.2 <- Nereo2_2018
colnames(Nereo2_2018.2) <- c("Segment_number.2", "HubDist2.2", "Presence.2", "Year.2")

Nereo3 <- cbind(Nereo2_2007.2, Nereo2_2018.2)
Nereo4 <- Nereo3 %>% filter(Presence==1)

Nereo4$Change <- Nereo4$Presence - Nereo4$Presence.2

##Test for effect of distance from open coast on probability of extirpation
glm(Change ~ HubDist2, data = Nereo4, family = "binomial") %>% summary()

NereoChange <- ggplot(Nereo4, aes(y = Change, x = HubDist2))+
  geom_point(cex = 3)+
  theme_cowplot()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  #theme(legend.position = "none")+
  ylab(expression(paste("Probability of extirpation")))+
  xlab("Distance from edge mouth (km)")

NereoChange


##Macrocystis only
####Macrocystis ###

##Create Macro only dataframe
Macro<-shore.data[shore.data$Macro_not_NA==1,]
Macro<-drop_na(Macro,1)
Macro$Macro_pres_2018<-as.numeric(ifelse(Macro$M_2018>0,1,0))
Macro$Macro_pres_2007<-as.numeric(Macro$Macro_pres_2007)


#Logistic regression
Macro2_2018 <- Macro
Macro2_2018$Presence <- Macro$Macro_pres_2018
Macro2_2018$Year <- "2018"
Macro2_2018 <- Macro2_2018[,c("Segment_number", "HubDist2", "Presence", "Year") ]

Macro2_2007 <- Macro
Macro2_2007$Presence <- Macro$Macro_pres_2007
Macro2_2007$Year <- "2007"
Macro2_2007 <- Macro2_2007[,c("Segment_number", "HubDist2", "Presence", "Year") ]


Macro2 <- rbind(Macro2_2007, Macro2_2018)
ggplot(Macro2, aes(y = Presence, x = HubDist2, group = Year, color = Year))+
  geom_point(cex = 3)+
  theme_cowplot()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  #theme(legend.position = "none")+
  ylab(expression(paste("Probability of", " ",italic("Macrocystis"))))+
  xlab("Distance from edge mouth (km)")

Macro2$Segment_number<-Macro2$Segment_number %>% as.factor() 
Macro2$Year<-Macro2$Year %>% as.factor() 

bayesglm(Presence ~ HubDist2*Year, data = Macro2) %>% AICc()

m0<-glmer(Presence ~ 1 + (1|Segment_number), data = Macro2, family = "binomial") 
m1<-glmer(Presence ~ Year + (1|Segment_number), data = Macro2, family = "binomial") 
m2<-glmer(Presence ~ HubDist2 + (1|Segment_number), data = Macro2, family = "binomial") 
m3<-glmer(Presence ~ HubDist2*Year + (1|Segment_number), data = Macro2, family = "binomial")
m3a<-glmer(Presence ~ poly(HubDist2, 2)*Year + (1|Segment_number), data = Macro2, family = "binomial")
m4<-glmer(Presence ~ HubDist2+Year + (1|Segment_number), data = Macro2, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

AICctab(m0,m1,m2,m3,m4,m3a, weights = TRUE)
summary(m3)
summary(m3a)

Macro2_2007.2 <- Macro2_2007
Macro2_2018.2 <- Macro2_2018

colnames(Macro2_2018.2) <- c("Segment_number.2", "HubDist2.2", "Presence.2", "Year.2")

Macro3 <- cbind(Macro2_2007.2, Macro2_2018.2)
Macro4 <- Macro3 %>% filter(Presence==1)

Macro4$Change <- Macro4$Presence - Macro4$Presence.2

##Test for effect of distance from open coast on kelp persistance

glm(Change ~ HubDist2, data = Nereo4, family = "binomial") %>% summary()


MacroChange <- ggplot(Macro4, aes(y = Change, x = HubDist2))+
  geom_point(cex = 3)+
  theme_cowplot()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  #theme(legend.position = "none")+
  ylab(expression(paste("Probability of extirpation")))+
  xlab("Distance from edge mouth (km)")


##Load in 2013-2016 remote sensing kelp data

####
dis <- read_csv("./data/kelp_comparisons/BarkleySoundAllData_Distance.csv")
d <- read_excel("./data/kelp_comparisons/BarkleyAllDatawithChange.xls")
d14 <- read_excel("./data/kelp_comparisons/BarkleySound2014.xlsx")
dd <- left_join(d, dis, by = "Segment")


dd_2016 <- filter(dd, Exclude2013_2016=="FALSE")
dd_2018 <- filter(dd, Exclude2013_2018=="FALSE")
dd_2016$Kelp_2013Sat <- as.numeric(dd_2016$Kelp_2013Sat)
dd_2016$Kelp_2016Sat <- as.numeric(dd_2016$Kelp_2016Sat)

#2013 (Satellite) vs 2016 (Satellite)

##2013 vs 2016
d2013<-dd_2016[,c("Segment", "NEAR_DIST_KM", "Kelp_2013Sat")]
d2016<-dd_2016[,c("Segment", "NEAR_DIST_KM", "Kelp_2016Sat")]
colnames(d2013) <- c("Segment", "Distance", "Kelp")
colnames(d2016) <- c("Segment", "Distance", "Kelp")
d2013$Year <- "2013"
d2016$Year <- "2016"

dd2013_2016 <- rbind(d2013, d2016)

#Best model is one with interaction between timepoint and distance from mouth
i0<- glmer(Kelp ~ 1 + (1|Segment) , data = dd2013_2016, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i3<-glmer(Kelp ~ Year*Distance + (1|Segment) , data = dd2013_2016, family = "binomial")
i2<-glmer(Kelp ~ Distance + (1|Segment) , data = dd2013_2016, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i1<-glmer(Kelp ~ Year + (1|Segment) , data = dd2013_2016, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i4<-glmer(Kelp ~ Year+Distance + (1|Segment) , data = dd2013_2016, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
AICtab(i2,i3,i4,i0,i1, weights = TRUE)



#1981 (aerial) vs 2018 (in situ)
dd_1981 <- filter(dd, Exclude_1981_2018=="FALSE")
dd_1981$Change1981to2018_extinction <- as.numeric(dd_1981$Change1981to2018_extinction)

dd_1981_2<-filter(dd_1981, kelp_1981_bin ==1)

#Test for effect of distance from open coast on kelp persistance
glm(Change1981to2018_extinction ~ NEAR_DIST_KM, data = dd_1981_2, family = "binomial") %>% summary()


#2013 (Satellite) vs 2018 (in situ)
##2013 vs 2018
d2013<-dd_2018[,c("Segment", "NEAR_DIST_KM", "Kelp_2013Sat")]
d2018<-dd_2018[,c("Segment", "NEAR_DIST_KM", "Kelp_2018")]
colnames(d2013) <- c("Segment", "Distance", "Kelp")
colnames(d2018) <- c("Segment", "Distance", "Kelp")
d2013$Year <- "2013"
d2018$Year <- "2018"
dd2013_2018 <- rbind(d2013, d2018)
dd2013_2018$Kelp <- as.numeric(dd2013_2018$Kelp)
dd2013_2018$Kelp_bin <- ifelse(dd2013_2018$Kelp>0,1,0) 

#Best model is one with interaction between timepoint and distance from mouth
i0<- glmer(Kelp_bin ~ 1 + (1|Segment) , data = dd2013_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i3<-glmer(Kelp_bin ~ Year*Distance + (1|Segment) , data = dd2013_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i2<-glmer(Kelp_bin ~ Distance + (1|Segment) , data = dd2013_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i1<-glmer(Kelp_bin ~ Year + (1|Segment) , data = dd2013_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i4<-glmer(Kelp_bin ~ Year+Distance + (1|Segment) , data = dd2013_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
AICtab(i2,i3,i4,i0,i1, weights = TRUE)


d2013$Kelp <- ifelse(as.numeric(d2013$Kelp) > 0, 1, 0)
d2018$Kelp <- ifelse(as.numeric(d2018$Kelp) > 0, 1, 0)
names(d2018) <- c("Segment.2", "Distance.2", "Kelp.2", "Year.2")

dd2013_2018_2<-cbind(d2013, d2018)
dd2013_2018_2<-filter(dd2013_2018_2, Kelp==1)
dd2013_2018_2$Change <- dd2013_2018_2$Kelp - dd2013_2018_2$Kelp.2

#Test for effect of distance from open coast on kelp persistance
glm(Change ~ Distance, data = dd2013_2018_2, family = "binomial") %>% summary()

#2014 (aerial) vs 2018 (In situ)
###2014 vs 2018
dd2014<-left_join(d14, dd, by = "Segment")[,c("Segment","NEAR_DIST_KM","KelpAerial2014","Kelp_2018" )]
dd2014$KelpAerial2014 <- as.numeric(dd2014$KelpAerial2014)
dd2014$Kelp_2018 <- as.numeric(dd2014$Kelp_2018)
dd2014<-dd2014 %>% drop_na()
d2014<-dd2014[,c("Segment", "NEAR_DIST_KM", "KelpAerial2014")]
d2018<-dd2014[,c("Segment", "NEAR_DIST_KM", "Kelp_2018")]
colnames(d2014) <- c("Segment", "Distance", "Kelp")
colnames(d2018) <- c("Segment", "Distance", "Kelp")
d2014$Year <- "2014"
d2018$Year <- "2018"
d2018$Kelp <- as.numeric(d2018$Kelp)
d2014$Kelp <- as.numeric(d2014$Kelp)
d2014_2018<-rbind(d2014, d2018)
d2014_2018$Kelp_bin <- ifelse(d2014_2018$Kelp>0,1,0) 


#Best model is one with interaction between timepoint and distance from mouth
i0<- glmer(Kelp_bin ~ 1 + (1|Segment) , data = d2014_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i3<-glmer(Kelp_bin ~ Year*Distance + (1|Segment) , data = d2014_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i2<-glmer(Kelp_bin ~ Distance + (1|Segment) , data = d2014_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i1<-glmer(Kelp_bin ~ Year + (1|Segment) , data = d2014_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i4<-glmer(Kelp_bin~ Year+Distance + (1|Segment) , data = d2014_2018, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
AICtab(i2,i3,i4,i0,i1, weights = TRUE)

AICcmodavg::AICc(i1)



d2014$Kelp <- ifelse(as.numeric(d2014$Kelp) > 0, 1, 0)
d2018$Kelp <- ifelse(as.numeric(d2018$Kelp) > 0, 1, 0)
names(d2018) <- c("Segment.2", "Distance.2", "Kelp.2", "Year.2")

dd2014_2018_2<-cbind(d2014, d2018)
dd2014_2018_2<-filter(dd2014_2018_2, Kelp==1)
dd2014_2018_2$Change <- dd2014_2018_2$Kelp - dd2014_2018_2$Kelp.2

#Test for effect of distance from open coast on kelp persistance
glm(Change ~ Distance, data = dd2014_2018_2, family = "binomial") %>% summary()






#Import data
long <- read_csv("./data/kelp_comparisons/Segment_All_LongCombined.csv")

distance <- read_csv("./data/kelp_comparisons/BarkleySoundAllData_Distance.csv")

distance2 <- tibble(Segment = distance$Segment, Dist = distance$NEAR_DIST_KM)

long2 <- left_join(long, distance2, by = "Segment")

#Remove NAs
long2 <- long2 %>% drop_na()
long2$Segment <- as.factor(long2$Segment)

long3<- long2 %>% filter(Dist > 8)
long3 

long3$Dummy <- 1
long3$Year <- as.factor(long3$Year)

long.sum <- long3 %>% group_by(Year) %>%
  summarise(avg = mean(Kelp_bin), SampleSize = sum(Dummy))

#Confidence intervals with Wilson method for sample occurrence
long.sum$upper <- c(1, 0.98, 0.91, 0.27, 0.41, 0.07, 0.16)
long.sum$lower <- c(1, 0.89, 0.81, 0.12, 0.32, 0.002,0.08)

trend2<-ggplot(long.sum, aes(x = as.numeric(as.character(Year)), y = avg))+
  geom_point(size = 3)+
  geom_line(size = 0.5)+
  theme_cowplot()+
  ylim(c(0,1))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5)+
  ylab("Proportion of Segments")+
  xlab("Year")+
  scale_x_continuous(breaks=seq(2006,2021,2))

trend2

mod.bin<-bayesglm(Kelp_bin ~ Year, data = long3, family = "binomial")

summary(mod.bin)


pdf(file="../figures/Trend2.pdf", width = 8, height = 4)
trend2
dev.off()

