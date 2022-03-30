#Comparison of kelp distributions to historical baselines"

#Load libraries
library(tidyverse)
library(readxl)
library(cowplot)
library(arm)
library(bbmle)
library(lme4)
  
#This is just a useful function (makeTransparent)

makeTransparent<-function(someColor, alpha=100){
  "Given a colour name (e.g. 'red'), make it transparent. 
  someColor:
        Vector of colour names to make transparent e.g. c('red', 'blue')
  alpha:
        Alpha transparency. 100 fully opaque, 0 fully transparent.
  Credit: http://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
  "
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

####COMPARISONS TO DRUEHL DATA (1993-1995)
#JUST intertidal Macrocystis from Druehl & Elliot sites (originally sampled in 1993-1995)

d<-read_excel("./data/kelp_comparisons/Druehl_MacroIntertidalData.xlsx")

set.seed(9)
d2 <- d %>%
  group_by(KeyVariable) %>%
  sample_n(1) 

inter_plot<- ggplot(d2, aes(y = Macrocystis, x = DistSound_km, group = TimePoint, color = TimePoint))+
  geom_point(cex = 3)+
  theme_cowplot()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial))  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  #theme(legend.position = "none")+
  ylab(expression(paste("Probability of", " ",italic("Macrocystis"))))+
  xlab("Distance from edge mouth (km)")

bayesglm(Macrocystis ~ TimePoint*DistSound_km , data = d2, family = "binomial") %>% summary()

inter_plot


#Model comparison (Druehl - intertidal)
```{r}

#Best model is one with interaction between timepoint and distance from mouth
i0<- glmer(Macrocystis ~ 1 + (1|Site) , data = d2, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i3<-glmer(Macrocystis ~ TimePoint*DistSound_km + (1|Site) , data = d2, family = "binomial")
i2<-glmer(Macrocystis ~ DistSound_km + (1|Site) , data = d2, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
i1<-glmer(Macrocystis ~ TimePoint + (1|Site) , data = d2, family = "binomial") 
i4<-glmer(Macrocystis ~ TimePoint+DistSound_km + (1|Site) , data = d2, family = "binomial")
AICctab(i0,i1,i2,i3,i4, weights = TRUE)
```


#This is the same but excluding the sites that lack Macrocystis at both time points
set.seed(10)
d3 <- d2 %>%
  filter(Always_zero==0) 


ggplot(d3, aes(y = Macrocystis, x = DistSound_km, group = TimePoint, color = TimePoint))+
  geom_point(cex = 3)+
  theme_cowplot()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  #theme(legend.position = "none")+
  ylab(expression(paste("Probability of", " ",italic("Macrocystis"))))+
  xlab("Distance from edge mouth (km)")


#Test whether kelp beds closer to the outer edge of the sound were more likely to persist
set.seed(10)

d_before <- d3 %>% filter(TimePoint == "Historic")
d_after <- d3 %>% filter(TimePoint == "Modern")

d_before2 <- tibble(Site = d_before$Site, DistSound_km = d_before$DistSound_km, Macrocystis_before = d_before$Macrocystis)

d_after2 <- tibble(Site = d_after$Site, DistSound_km = d_after$DistSound_km, Macrocystis_after = d_after$Macrocystis)

d_combined <- left_join(d_before2, d_after2, by = "Site")

d_combined <- d_combined %>% filter(Macrocystis_before == 1)

d_combined$Change <- (d_combined$Macrocystis_before - d_combined$Macrocystis_after)

KelpBedChange<- ggplot(d_combined, aes(y = Change, x = DistSound_km.x))+
  geom_point(cex = 3)+
  theme_cowplot()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  #theme(legend.position = "none")+
  ylab(expression(paste("Probability of extirpation")))+
  xlab("Distance from edge mouth (km)")

KelpBedChange

glm(Change ~ DistSound_km.x , data = d_combined, family = "binomial") %>% summary()


#2008 kelp bed data
#These original data were collected by BMSC students in 2008 (Conservation Biology)

##Import 2008 kelp bed dataset
kelpbed.data<-read_excel("./data/kelp_comparisons//ShoreZoneCCE2019.xlsx", sheet="KelpBedPresenceData_Cleaned")
kelpbed.dist<-read_csv("./data/kelp_comparisons/KelpBed2008_distance.csv")

n0 <- glmer(Presence_2019 ~ 1 + (1|`2008 Transect #`), data = kelpbed.data, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
n1 <- glmer(Presence_2019 ~ Longitude + (1|`2008 Transect #`), data = kelpbed.data, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

n0 <- glmer(Presence_2 ~ 1 + (1|F2008_Trans), data = kelpbed.dist, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
n1 <- glmer(Presence_2 ~ NEAR_DIST_KM + (1|F2008_Trans), data = kelpbed.dist, family = "binomial", control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
AICctab(n0, n1, weights = TRUE)                                                                                                       
summary(n1)

kelpbed.dist$Presence_4 <- -1*kelpbed.dist$Presence_2 + 1

glm(Presence_4 ~ NEAR_DIST_KM, data = kelpbed.dist, family = "binomial") %>% summary()
