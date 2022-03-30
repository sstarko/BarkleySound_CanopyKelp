#iButton 2015-2016 Analysis (data from Iwabuchi and Gosselin)
#This script is an analysis of the 2015-2016 in situ data collected by B. Iwabuchi and L. Gosselin
  
#Load required packages
library(tidyverse) 
library(cowplot)
library(Tides)
library(lubridate)

#The first step is to figure out how to find the start and end times for each tide window where the tide is above (or below) the level of the iButton. It looks like the package called 'Tides' should be able to do this. First we need to read in the Bamfield tide observation data.
Jul_2015<-read_csv("~/Dropbox/R_projects/SubtidalKelps3/data/tides_2015_2016/8545-01-JUL-2015_slev.csv", skip = 7)
Aug_2015<-read_csv("~/Dropbox/R_projects/SubtidalKelps3/data/tides_2015_2016/8545-01-AUG-2015_slev.csv", skip = 7)
Sep_2015<-read_csv("~/Dropbox/R_projects/SubtidalKelps3/data/tides_2015_2016/8545-01-SEP-2015_slev.csv", skip = 7)
Bamfield_tides_2015 <- rbind(Jul_2015, Aug_2015, Sep_2015)
Bamfield_tides_2015$Obs_date<-ymd_hm(Bamfield_tides_2015$Obs_date)
names(Bamfield_tides_2015)<-c("time","h")

Jul_2016<-read_csv("~/Dropbox/R_projects/SubtidalKelps3/data/tides_2015_2016/8545-01-JUL-2016_slev.csv", skip = 7)
Aug_2016<-read_csv("~/Dropbox/R_projects/SubtidalKelps3/data/tides_2015_2016/8545-01-AUG-2016_slev.csv", skip = 7)
Sep_2016<-read_csv("~/Dropbox/R_projects/SubtidalKelps3/data/tides_2015_2016/8545-01-SEP-2016_slev.csv", skip = 7)
Bamfield_tides_2016 <- rbind(Jul_2016, Aug_2016, Sep_2016)
Bamfield_tides_2016$Obs_date<-ymd_hm(Bamfield_tides_2016$Obs_date)
names(Bamfield_tides_2016)<-c("time","h")

#Create interval lists
#2015
#Times when tide is above 0.5m #not usable, just a test
Interval_list_0.5m<-IT(Bamfield_tides_2015,0.5)
list2env(Interval_list_0.5m,globalenv())
IT_0.5m<-IT

#Times when tide is above 2.5m (1.0m above iButton)
Interval_list_2.5m<-IT(Bamfield_tides_2015,2.5)
list2env(Interval_list_2.5m,globalenv())
IT_2.5m<-IT

#Times when tide is above 3.0m (1.5m above iButton)
Interval_list_3m<-IT(Bamfield_tides_2015,3)
list2env(Interval_list_3m,globalenv())
IT_3m<-IT

rm(DT,IT,Interval_list_0.5m,Interval_list_2.5m, Interval_list_3m)

#2016
Interval_list_0.5m_2016<-IT(Bamfield_tides_2016,0.5)
list2env(Interval_list_0.5m_2016,globalenv())
IT_0.5m_2016<-IT

#Times when tide is above 2.2m (0.7m above iButton)
Interval_list_2.5m_2016<-IT(Bamfield_tides_2016,2.5)
list2env(Interval_list_2.5m_2016,globalenv())
IT_2.5m_2016<-IT

#Times when tide is above 2.7m (1.2m above iButton)
Interval_list_3m_2016<-IT(Bamfield_tides_2016,3)
list2env(Interval_list_3m_2016,globalenv())
IT_3m_2016<-IT

rm(DT,IT,Interval_list_0.5m,Interval_list_2.5m, Interval_list_3m)



#Next we need to be able to select and identify times in the iButton data when the iButton is above vs. below the water. We do this by joining the relevant start and end times directly to the full 2015 and 2016 iButton data tables.
#the data.table library seems to do a lot quickly with minimal code.
library(data.table)
library(readxl)
#Read in the ibutton data
full_tbl_2015<-read_excel("~/Dropbox/R_projects/SubtidalKelps3/data/temporal_env_data/2015-2016-iButtonData.xlsx", sheet = "2015_temp_data") %>% as.data.frame()
colnames(full_tbl_2015)[1]  <- "date_adj"

full_tbl_2016<-read_excel("~/Dropbox/R_projects/SubtidalKelps3/data/temporal_env_data/2015-2016-iButtonData.xlsx", sheet = "2016_temp_data") %>% as.data.frame()
colnames(full_tbl_2016)[1]  <- "date_adj"

#first we add an ID column to facilitate the joins.
full_tbl_2015$ID = "ID"
full_tbl_2016$ID = "ID"

IT_0.5m$ID = "ID"
IT_2.5m$ID = "ID"
IT_3m$ID = "ID"

IT_0.5m_2016$ID = "ID"
IT_2.5m_2016$ID = "ID"
IT_3m_2016$ID = "ID"

#Make sure that there is a key that connects all of the dataframes


# perform update join. Used to join the columns t1,t2, dt from IT data tables to the full 1m and 2m data tables. These are used for generating times when iBUttons are predicted to be at least 0.5m below still water level, and should reflect **sea surface temperatures in the area.**
setDT(full_tbl_2015)[
  setDT(IT_2.5m), on = .(ID, date_adj >= t1, date_adj <= t2), 
  `:=`(duration_water_2.5 = dt , t1water_2.5 = t1, t2water_2.5 = t2)]

setDT(full_tbl_2015)[
  setDT(IT_3m), on = .(ID, date_adj >= t1, date_adj <= t2), 
  `:=`(duration_water_3 = dt , t1water_3 = t1, t2water_3 = t2)]

setDT(full_tbl_2016)[
  setDT(IT_2.5m_2016), on = .(ID, date_adj >= t1, date_adj <= t2), 
  `:=`(duration_water_2.5 = dt , t1water_2.5 = t1, t2water_2.5 = t2)]

setDT(full_tbl_2016)[
  setDT(IT_3m_2016), on = .(ID, date_adj >= t1, date_adj <= t2), 
  `:=`(duration_water_3 = dt , t1water_3 = t1, t2water_3 = t2)]

#create water variable for plotting
#2015 data
full_tbl_2015$water<-ifelse(is.na(full_tbl_2015$t1water_2.5)==F, "water", "air")
full_tbl_2015$toodeep<-ifelse(is.na(full_tbl_2015$t1water_3)==F, "yes", "no")

tbl_2015<-full_tbl_2015[full_tbl_2015$water=="water",]
tbl_2015<-tbl_2015[tbl_2015$toodeep=="no",]

hist(as.numeric(tbl_2015$`Fleming-2015 Avg temp-1.5m`))
hist(as.numeric(tbl_2015$`Ross Islets-2015 Avg temp-1.5m`))

#2016
full_tbl_2016$water<-ifelse(is.na(full_tbl_2016$t1water_2.5)==F, "water", "air")
full_tbl_2016$toodeep<-ifelse(is.na(full_tbl_2016$t1water_3)==F, "yes", "no")

tbl_2016<-full_tbl_2016[full_tbl_2016$water=="water",]
tbl_2016<-tbl_2016[tbl_2016$toodeep=="no",]

hist(as.numeric(tbl_2016$`Fleming-2016 Avg temp-1.5m`))
hist(as.numeric(tbl_2016$`Ross Islets-2016 Avg temp-1.5m`))


#Import Amphitrite Point Light temperature data
amph<-read_csv("~/Dropbox/R_projects/SubtidalKelps3/data/temporal_env_data/2015-2016-Summer-Amphitrite.csv")

hist(amph$SST)

amph_2015 <- filter(amph, year == 2015)
amph_2016 <- filter(amph, year == 2016)

#Combine with iButton data to make single dataframe for visualization

tbl_2016_clean_Ross <- data.frame(date = tbl_2016$"date_adj", SST = tbl_2016$`Ross Islets-2016 Avg temp-1.5m`, site = "Ross")

tbl_2016_clean_Fleming <- data.frame(date = tbl_2016$"date_adj", SST = tbl_2016$`Fleming-2016 Avg temp-1.5m`, site = "Fleming")

tbl_2016_clean_Amph <- data.frame(date = amph_2016$Date, SST = amph_2016$SST, site = "Amphitrite")

tbl_2016_clean<-rbind(tbl_2016_clean_Ross,tbl_2016_clean_Fleming, tbl_2016_clean_Amph )

ggplot(tbl_2016_clean, aes(x = SST, group = site, color = site))+
  geom_density()



tbl_2015_clean_Ross <- data.frame(date = tbl_2015$"date_adj", SST = tbl_2015$`Ross Islets-2015 Avg temp-1.5m`, site = "Ross")

tbl_2015_clean_Fleming <- data.frame(date = tbl_2015$"date_adj", SST = tbl_2015$`Fleming-2015 Avg temp-1.5m`, site = "Fleming")

tbl_2015_clean_Amph <- data.frame(date = amph_2015$Date, SST = amph_2015$SST, site = "Amphitrite")

tbl_2015_clean<-rbind(tbl_2015_clean_Ross,tbl_2015_clean_Fleming, tbl_2015_clean_Amph )

tbl_2015_clean$SST <- as.numeric(tbl_2015_clean$SST )

plot2015<- ggplot(tbl_2015_clean, aes(x = SST, fill = site, color = site))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.6)+
  theme_cowplot()+
  scale_color_manual(values = c("black", "black", "black"))+
  scale_fill_manual(values = c("blue","red", "darkgreen"))+
  ylab("Density")+
  xlab(expression('Water Temperature ('*~degree*C*')'))

plot2016<- ggplot(tbl_2016_clean, aes(x = SST, fill = site, color = site))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  theme_cowplot()+
  scale_color_manual(values = c("black", "black", "black"))+
  scale_fill_manual(values = c("blue","red", "darkgreen"))+
  ylab("Density")+
  xlab(expression('Water Temperature ('*~degree*C*')'))

cowplot::plot_grid(plot2015,plot2016)
