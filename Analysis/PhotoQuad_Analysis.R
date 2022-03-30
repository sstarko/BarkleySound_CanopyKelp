##Analysis of subtidal photo quadrat data

library(readxl)
library(tidyverse)
d <- read_excel("./data/photo_quad/DropQuad_Foliose.xlsx")

ggplot(d, aes(x = Depth_below_MLLWLT, y = Foliose_cover))+
  facet_wrap(~`Site Number`, ncol = 2)+
  geom_point()+
  geom_smooth(color = "grey", se = FALSE)+
  ylim(-1,102)+
  theme_cowplot()

drop<-read_csv("./data/photo_quad/UrchinData_2019DropQuad.csv")

drop.sum <- drop %>% group_by(Site) %>%
  summarise(sed = mean(Cobble_percent, na.rm = TRUE)+mean(Sediment_percent, na.rm = TRUE), urch = mean(Urchin_density, na.rm = TRUE),dist = mean(Distance_from_open, na.rm = TRUE) )

pl<-ggplot(drop.sum, aes(x = sed , y = urch))+
  geom_point()+
  geom_smooth(method = "nls", formula=y ~ (a*exp(-b * x)),method.args = list(start = c(b = 0.01, a = 10)), size=1, se = FALSE, color = "black")+
  ylab("Urchin density (ind/m^2)")+
  xlab("Percent unconsolidated substrate")

pl + theme_classic()

form <- urch ~ a*exp(-b * sed)
mod <- nls(form, data = drop.sum, start = c(a = 5,b = 0.1))

pl2<-ggplot(drop.sum, aes(x = dist , y = sed))+
  geom_point()+
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  ylab("Percent unconsolidated substrate")+
  xlab("Distance from open coast (km)")

pl2 + theme_classic()

