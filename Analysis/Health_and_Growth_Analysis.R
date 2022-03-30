## Macro and Nereo Health and Growth

#Load necessary libraries
library(tidyverse)
library(ggpubr)
library(cowplot)
library(lme4)
library(nlme)

load(file = "./data/health_growth/macro_growth.Rdata")
load(file = "./data/health_growth/macro_health.Rdata")
load(file = "./data/health_growth/nereo_health.Rdata")


#Plot Macro bleaching data
p1<- ggplot(macro_health, aes(x = dist_sound_km, y = mean_gray_value)) +
  ylim(100,200) + xlim (3,15) +
  labs(x = "", y = expression(paste("Blade bleaching index"))) +
  geom_point() +
  theme_classic() +
  geom_smooth(method="lm", alpha = 0.15, color = "black") +
  theme(legend.position = "none") +
  theme (plot.margin=unit(c(0.5,0,0,1), "cm"))

p1


#Create summary file
macro_growth_summary <- Rmisc::summarySE(macro_growth2, measurevar = "net_blade_growth", groupvars = "site", na.rm = TRUE)

#Convert "site" to factor
macro_growth2$site <- factor(macro_growth2$site,ordered = TRUE, levels = c("Execution Bay", "Second Beach", "Grappler", "Nanat Sheltered"))

#Plot Macrocystis growth rate for 4 sites along the gradient
p2 <- ggplot(macro_growth2, aes(x=site,y = net_blade_growth, color = site, fill = site))+
  geom_dotplot(binaxis="y", colors = c("grey"), size = 2) + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                                                                         geom="errorbar", color="black", width=0.3) +
  stat_summary(fun=mean, geom="point", color="black")+
  theme_cowplot()+
  scale_color_manual(values = c("#8176FE", "#8176FE", "#FF6B72", "#FF6B72"))+
  scale_fill_manual(values = c("#8176FE", "#8176FE", "#FF6B72", "#FF6B72"))+
  ylab("Net blade growth per day (cm)")

p2

p3<- ggplot(macro_growth2, aes(x=site,y = frond_growth, color = site, fill = site))+
  geom_dotplot(binaxis="y", colors = c("grey"), size = 2) + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                                                                         geom="errorbar", color="black", width=0.3) +
  stat_summary(fun=mean, geom="point", color="black")+
  theme_cowplot()+
  scale_color_manual(values = c("#8176FE", "#8176FE", "#FF6B72", "#FF6B72"))+
  scale_fill_manual(values = c("#8176FE", "#8176FE", "#FF6B72", "#FF6B72"))+
  ylim(c(0,6))+
  ylab("Frond growth per day (cm)")

p3

#Tukey test of growth vs site
aov(net_blade_growth ~ site, data = macro_growth2) %>% TukeyHSD()
aov(frond_growth ~ site, data = macro_growth2) %>% summary()
aov(blade_length_start ~ site, data = macro_growth2) %>% summary()

#model output for health data
mixed.lmer1 <- lme(mean_gray_value ~ dist_sound_km, random= ~1|site, data = macro_health)
macro_model <- lm(as.numeric(macro_growth$net_blade_growth) ~ macro_growth$dist_sound_km+ macro_growth$site)

macro_health_sum <- macro_health %>% group_by(site) %>%
  summarize(gray = mean(mean_gray_value), dist_sound_km = unique(dist_sound_km))

nereo_health_sum <- nereo_health %>% group_by(site) %>% 
  summarize(length = mean(mean_blade_len), longest = mean(blade_longest), dist_sound_km = mean(dist_sound_km))

#Bleaching
mixed.health<-lme(mean_gray_value ~ dist_sound_km, random = ~1|site/ind_no, data = macro_health)

macro.health<-lm(mean_gray_value ~ dist_sound_km, data = macro_health)

#Mean blade length - Nereo
nereo.health<-lm(mean_blade_len ~ dist_sound_km, data = nereo_health)

p4 <- ggplot(nereo_health, aes(x = dist_sound_km, y=blade_no)) +
  labs(x = "", y = "Number of blades") +
  xlim (3,15) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none") +
  theme (plot.margin=unit(c(0.5,0,0,1), "cm"))

p4

p5 <- ggplot(nereo_health, aes(x = dist_sound_km, y=mean_blade_len)) +
  labs(x = "Distance from open coast (km)", y ="Mean blade length (cm)") +
  geom_point() +
  xlim (3,15) +
  theme_classic() +
  geom_smooth(method="lm", alpha = 0.15, color = "black") +
  theme(legend.position = "none") +
  theme (plot.margin=unit(c(0.5,0,0,1), "cm"))

p5

