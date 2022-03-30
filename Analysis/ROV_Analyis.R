##ROV analyses

#Load in libraries
library(tidyverse)
library(readxl)
library(cowplot)
library(MASS)
library(arm)

#Import ROV data set
ROV<-read_excel("./data/ROV/ROV_data_2020.xlsx", sheet = "ROV_data_2020")
ROV.dist <- read_csv("./data/ROV/ROV_data_2020_distance.csv")
ROV.urch <- read_excel("./data/ROV/ROVUrchinScores.xlsx")
ROV.urch$rov_site_id <- as.character(ROV.urch$rov_site_id)


#Remove observation 41 because it was redone later (with better footage)
ROV2<-left_join(ROV.dist, ROV, by = "rov_site_id")
ROV.ran <- filter(ROV2, type == "random")
ggplot(ROV.ran, aes(x = NEAR_DIST_KM, y = seaweed_lower_m_adjusted*-1, color = substrate, fill = substrate))+
  geom_bar(stat= "identity",width =0.1)+
  scale_color_manual(values = c("#595959", "#C5AC92"))+
  scale_fill_manual(values = c("#595959", "#C5AC92"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  xlim(c(4,15))

ROV.ran2<-full_join(ROV.ran, ROV.urch)
p1<-ggplot(ROV.ran2, aes(x = NEAR_DIST_KM, y = seaweed_lower_m_adjusted*-1, color = group, fill = group))+
  geom_bar(stat= "identity",width =0.05)+
  scale_color_manual(values = c("#595959", "#C5AC92"))+
  scale_fill_manual(values = c("#595959", "#C5AC92"))+
  #scale_color_manual(values = c("black", "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  xlim(c(4,15))+
  ylim(c(-16,0))+
  theme_cowplot()

p1<-ggplot(ROV.ran2, aes(x = NEAR_DIST_KM, y = seaweed_lower_m_adjusted*-1, color = group, fill = group))+
  geom_bar(stat= "identity",width =0.07)+
  geom_point(size = 2)+
  scale_color_manual(values = c("#595959", "#C5AC92"))+
  scale_fill_manual(values = c("#595959", "#C5AC92"))+
  #scale_color_manual(values = c("black", "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  xlim(c(4,15))+
  theme_cowplot()+
  ylim(c(-16,0))

lm(seaweed_lower_m_adjusted*-1 ~ NEAR_DIST_KM*group, data = ROV.ran2) %>% summary()

p2<-ggplot(ROV.ran2, aes(x = NEAR_DIST_KM, y = as.numeric(Urchin_score), color = group, fill = group))+
  geom_point(stat= "identity", size = 2)+
  geom_smooth(se = FALSE)+
  scale_color_manual(values = c("#595959", "#C5AC92"))+
  scale_fill_manual(values = c("#595959", "#C5AC92"))+
  #theme(panel.border = element_rect(colour = "black", fill=NA, size=2))
  xlim(c(4,15))+
  ylim(c(-0.5,3.5))+
  theme_cowplot()



ROV.ran2$Urchin_score <- as.ordered(ROV.ran2$Urchin_score)
kruskal.test(Urchin_score ~ group2, data = ROV.ran2)

bayesglm(as.numeric(canopy_kelp) ~ NEAR_DIST_KM*group, data = ROV.ran2, family = "binomial") %>% summary()

p3<-ggplot(ROV.ran2, aes(x = NEAR_DIST_KM, y = as.numeric(canopy_kelp), color = group, fill = group))+
  geom_point(stat= "identity",width =0.1, size = 2)+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), se = FALSE)+
  scale_color_manual(values = c("#595959", "#C5AC92"))+
  scale_fill_manual(values = c("#595959", "#C5AC92"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  xlim(c(4,15))+
  ylim(c(0,1))+
  theme_cowplot()

plot_grid(p1,p2,p3, ncol = 1)


##Supplemental plot comparing urchin and seaweed zonation across sites that do and do not have kelp

ROV.rock <- filter(ROV.ran2, substrate == "rock")

p4<- ggplot(ROV.rock, aes(x = NEAR_DIST_KM, y = as.numeric(urchin_upper_m_adjusted)*-1, color = canopy_kelp, fill = canopy_kelp))+
  geom_point(stat= "identity",width =0.1, size = 2)+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), se = FALSE)+
  scale_color_manual(values = c("black", "grey"))+
  scale_fill_manual(values = c("black", "grey"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  xlim(c(4,15))+
  ylab("Upper limit of urchins (m)")+
  theme_cowplot()


#Welch's t-test comparing depth of urchins at rocky sites with and without canopy kelp
t.test(as.numeric(ROV.rock$urchin_upper_m_adjusted[ROV.rock$canopy_kelp==0]), as.numeric((ROV.rock$urchin_upper_m_adjusted[ROV.rock$canopy_kelp==1])))

ROV.rock$urchin_upper_m_adjusted <- as.numeric(ROV.rock$urchin_upper_m_adjusted)

ROV.rock$seaweed_lower_m_adjusted <- as.numeric(ROV.rock$seaweed_lower_m_adjusted)


#Sites WITHOUT canopy kelp have slightly deeper urchins (P = 0.041) but the range is largely overlapping

#Plot urchin depth versus 
p5 <- ggplot(ROV.rock, aes(x = as.numeric(urchin_upper_m_adjusted), y = as.numeric(ROV.rock$seaweed_lower_m_adjusted)))+
  geom_point(cex = 2)+
  geom_abline()+
  theme_cowplot()+
  ylab("Lower limit of seaweeds (m)")+
  xlab("Upper limit of urchins (m)")+
  ylim(c(0,6))+
  xlim(c(0,6))

#Plot 
plot_grid(p4,p5)
