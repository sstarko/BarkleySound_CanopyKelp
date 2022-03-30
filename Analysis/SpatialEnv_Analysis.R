##Spatial patterns of environmental drivers

#Load in libraries
library(ggrepel)
library(ggthemes)
library(tidyverse)
library(cowplot)
library(readxl)

######TEMPERATURE############

## 2019 iButton Data
#This script creates a plot of average temperature versus distance from open coast (data from summer 2019)

load(file="./data/spatial_env_data/2019_iButtons_by_day_1m.RData")
my.ylab = expression(atop(paste("Mean Sea Surface Temperature, ", degree,"C")))

load(file="./data/spatial_env_data/2019_iButtons_by_day_1m.RData")
my.ylab = expression(atop(paste("Mean Daily Water Temperature, ", degree,"C"), "(+/- Min., Max.)"))

p <- ggplot(subset(sum_by_site_1m, water %in% c("water")), mapping = aes(x = dist_in_sound, y = mean_mean_daily_temp_c)) +
  geom_smooth(method=lm, color = "black") +
  geom_point() +
  geom_pointrange(aes(ymin=mean_min_daily_temp_c, ymax=mean_max_daily_temp_c)) +
  labs(y= my.ylab, x = "Distance into sound (m)")

p + theme_classic()



##Compare one inshore to one outershore site in 2019
ibut <- read_csv("./data/spatial_env_data/2019_iButtons_air_water.csv")

ibut.roq <- filter(ibut, site == "RoquefoilBay"&water=="water")

roq_plot <- ggplot(ibut.roq, aes(x = value))+
  geom_histogram(color = "black",fill="red", binwidth=0.5)+
  xlim(c(7.5,20))+
  ylim(c(0,140))+
  theme_cowplot()+
  ylab("Number of measurements")


ibut.folger <- filter(ibut, site == "Folger"&water=="water")

folger_plot <- ggplot(ibut.folger, aes(x = value))+
  geom_histogram(color = "black", fill="blue", binwidth=0.5)+
  xlim(c(7.5,20))+
  ylim(c(0,140))+
  theme_cowplot()+
  ylab("Number of measurements")


plot_grid(folger_plot, roq_plot)

ibut.both <- rbind(ibut.folger, ibut.roq)
both_plot <- ggplot(ibut.both, aes(x = value, group = site, color = site, fill = site,))+
  geom_histogram(binwidth=0.5, position="identity")+
  xlim(c(7.5,20))+
  ylim(c(0,150))+
  theme_cowplot()+
  ylab("Number of measurements")+
  scale_fill_manual(values = c(makeTransparent("blue"),makeTransparent("red")))+
  scale_color_manual(values = c(makeTransparent("blue"),makeTransparent("red")))+
  xlab(expression(atop(paste("Temperature, ", degree,"C"))))

both_plot

######NITRATE############

## 1980s Druehl NO3 Data
#This creates a plot of nitrate vs distance from open coast

load(file="./data/spatial_env_data/Druehl_no3_1980.RData")

druehl_nutrients_1980_means <- druehl_nutrients_1980 %>% group_by(site, depth, month) %>% summarize(mean_no3 = mean(no3, na.rm = T))

druehl_nutrients_1980_means <- druehl_nutrients_1980_means  %>% 
  left_join(sites, by = "site")
druehl_nutrients_1980_means$dist_sound_km <- druehl_nutrients_1980_means$dist_sound/1000

nutrients.means.0m <- filter(druehl_nutrients_1980_means, depth == 0|depth == 4)
nutrients.means.4m <- filter(druehl_nutrients_1980_means, depth == 4)

druehl_nutrients_site_meta <- druehl_nutrients_1980_means %>% 
  group_by(site) %>% 
  summarize(dist_sound = mean(dist_sound))

load(file = "./data/spatial_env_data/druehl_nutrients.Rdata")

ggplot(druehl_nutrients2, aes(x = as.numeric(dist_sound/1000), y = no2_no3, color = as.character(depth)))+
  geom_point()+
  theme_cowplot()+
  scale_color_manual(values = c("blue", "red"))


druehl_nutrients3 <- tibble(no2_no3 = druehl_nutrients2$no2_no3, dist_sound_km = druehl_nutrients2$dist_sound/1000, site = druehl_nutrients2$site, depth = druehl_nutrients2$depth)

#Use 1.1 as a "dummy" value to keep years separate despite being same depth
druehl_nutrients3$depth <- gsub("1", "1.1",druehl_nutrients3$depth )


#Load in 2021 data
nit_2021 <- read_excel("./data/spatial_env_data/2021_nutrient_data.xlsx", sheet = "nitrate_all")

p2<-ggplot(nit_2021, aes(x = Distance_opencoast_km,y=Nitrate_plus_Nitrite_uM, color = as.factor(Depth_m))) + geom_point()+
  scale_color_manual(values = c("black","grey")) +
  geom_smooth(method = "nls", formula=y ~ (a*exp(-b * x)),method.args = list(start = c(b = 0.02, a = 10), control=nls.control(maxiter=200)), size=1, se = FALSE)+
  xlim(c(2,16.5)) +
  ylim(c(0,10.5))

p2 <- p2 + theme_cowplot(8)
p2



nit_2021_2 <- tibble(no2_no3 = nit_2021$Nitrate_plus_Nitrite_uM, dist_sound_km = nit_2021$Distance_opencoast_km, site = nit_2021$Station,depth = nit_2021$Depth_m)

rbind(druehl_nutrients3, nit_2021_2)

ggplot(rbind(druehl_nutrients3, nit_2021_2), aes(x = dist_sound_km, y = no2_no3, color = as.character(depth)))+
  geom_point()+
  theme_cowplot()+
  scale_color_manual(values = c("blue", "lightblue", "grey", "black"))+
  geom_smooth(method = "nls", formula=y ~ (a*exp(-b * x)),method.args = list(start = c(b = 0.02, a = 10), control=nls.control(maxiter=200)), size=1, se = FALSE)+
  xlim(c(0,17))

dd <- rbind(druehl_nutrients3, nit_2021_2)
ddd <- filter(rbind(druehl_nutrients3, nit_2021_2), depth == "1")

nls(no2_no3 ~ a*exp(-b * dist_sound_km), start = list(b = 0.02, a = 10), data = dd, control=nls.control(maxiter=200)) %>% summary()

nls(no2_no3 ~ a*exp(-b * dist_sound_km), start = list(b = 0.02, a = 10), data = filter(dd, depth == 1), control=nls.control(maxiter=200)) %>% summary()

nls(no2_no3 ~ a*exp(-b * dist_sound_km), start = list(b = 0.02, a = 10), data = filter(dd, depth == 1.1), control=nls.control(maxiter=200)) %>% summary()

nls(no2_no3 ~ a*exp(-b * dist_sound_km), start = list(b = 0.02, a = 10), data = filter(dd, depth == 4), control=nls.control(maxiter=200)) %>% summary()

nls(no2_no3 ~ a*exp(-b * dist_sound_km), start = list(b = 0.02, a = 10), data = filter(dd, depth == 5), control=nls.control(maxiter=200)) %>% summary()










