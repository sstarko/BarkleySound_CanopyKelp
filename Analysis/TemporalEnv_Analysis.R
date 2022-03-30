##Environmental variables through time

#Load libraries
library(tidyverse)
library(zoo)

#Amphitrite Point Lighthouse Data
#Lighthouse data analysis for regional trends in temperature

SST.data <-read_csv("./data/temporal_env_data/Amphitrite_Point_-_Daily_Sea_Surface_Temperature_and_Salinity_1934-2020.csv", skip = 1)
head(SST.data)
SST.data$Date<-as.Date(SST.data$Date)
SST.data$Year<-substring(SST.data$Date,1,4)
SST.data$Month<-substring(SST.data$Date,6,7)
SST.data$Day<-substring(SST.data$Date,9,10)
SST.data.trim<-SST.data[SST.data$Year>2005&SST.data$Year<2021,]
SST.data.trim<-drop_na(SST.data.trim)
SST.data.trim$greater.12<-ifelse(SST.data.trim$SST>11.999,1,0)
SST.data.trim$greater.13<-ifelse(SST.data.trim$SST>12.999,1,0)
SST.data.trim$greater.14<-ifelse(SST.data.trim$SST>13.999,1,0)
SST.data.trim$greater.15<-ifelse(SST.data.trim$SST>14.999,1,0)
SST.data.trim$greater.16<-ifelse(SST.data.trim$SST>15.99,1,0)

SST.proc.12<-SST.data.trim %>%
  group_by(Year) %>%
  summarize(deg_12=sum(greater.12))

SST.proc.13<-SST.data.trim %>%
  group_by(Year) %>%
  summarize(deg_13=sum(greater.13))

SST.proc.14<-SST.data.trim %>%
  group_by(Year) %>%
  summarize(deg_14=sum(greater.14))

SST.proc.15<-SST.data.trim %>%
  group_by(Year) %>%
  summarize(deg_15=sum(greater.15))

SST.proc.16<-SST.data.trim %>%
  group_by(Year) %>%
  summarize(deg_16=sum(greater.16))

par(mfrow=c(1,1))
barplot(deg_12~Year, data=SST.proc.12, ylim=c(0,180), col="blue")
barplot(deg_13~Year, data=SST.proc.13, add=TRUE, col="yellow")
barplot(deg_14~Year, data=SST.proc.14, add=TRUE, col="orange")
barplot(deg_16~Year, data=SST.proc.16, add=TRUE, col="red")

####

#Replace 999.9 values which indicate an error with "NA"
SST.data$SST<-gsub("99.9","NA", SST.data$SST)

#Optional: remove data from before a given year, say 1985
SST.data2<-SST.data[SST.data$Year>"1984",]

#Make temperature numeric and rename the columns
SST.data2$SST<-as.numeric(SST.data2$SST)
#colnames(SST.data2)<-c("Year","Month","Day","Temperature","Salinity")

###Calculate monthly average temperature
SST.data.sum<-SST.data2[SST.data2$SST!="NA",] %>%
  group_by(Year, Month) %>%
  summarize(Temperature = mean(SST))

SST.data.byMonth <- SST.data.sum %>%
  group_by(Month) %>%
  summarize(avgTemperature = mean(Temperature))

SST.data.byMonth<-SST.data.byMonth[1:12,]
SST.data.byMonth

##Calculate Anomalies
SST.data.comb<-left_join(SST.data.sum, SST.data.byMonth, by="Month")
SST.data.comb$Anomaly <- SST.data.comb$Temperature - SST.data.comb$avgTemperature
SST.data.comb<-SST.data.comb[complete.cases(SST.data.comb$Anomaly),]
SST.data.RockyTime<-SST.data.comb[SST.data.comb$Year>1999,]

#Add in months that have no data 
#SST.data.RockyTime<-InsertRow(SST.data.RockyTime, NewRow=c(2013,4,NA,8.50, NA), RowNum=28)
#SST.data.RockyTime<-InsertRow(SST.data.RockyTime, NewRow=c(2015,1,NA,7.87, NA), RowNum=49)

#Interpolate those months
#temp.pine.RockyTime$Anomaly<-na.ma(temp.pine.RockyTime$Anomaly,k=12, weighting = "exponential")

#Assign colour to temperature anomaly direction
for (i in 1:length(SST.data.RockyTime$Anomaly)) {
  if (SST.data.RockyTime$Anomaly[i] > 0) {
    SST.data.RockyTime$An.col[i]<-"#B51D2C"
  } else {
    SST.data.RockyTime$An.col[i]<-"#296BA8"
  }
}

SST.data.RockyTime$Date<-as.yearmon(paste(SST.data.RockyTime$Year, SST.data.RockyTime$Month), "%Y %m")
SST.data.RockyTime$Date<-as.Date(SST.data.RockyTime$Date)

##Plot SST through time
ggplot(SST.data.RockyTime, aes(y = Anomaly, x = Date, col = An.col))+
  geom_col()+
  scale_color_manual(values = c("#296BA8", "#B51D2C"))+
  theme_cowplot()+
  ylim(c(-2.2,2.2))+
  scale_x_date(breaks=as.Date(c("2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01")))

#Calculate standard deviation to add to plot
SST.data.RockyTime$Anomaly %>% sd()

##Watson sites in the Deer Group
##Code for analysis of Jane Watson's temperature data
temp<-read_excel("./data/temporal_env_data/WatsonTempSummaries.xlsx")
temp$Great.mean <- ifelse(temp$Mean > 12, "pink", "black")
temp$Great.max <- ifelse(temp$High > 15, "red", ifelse(temp$High > 12, "pink", "black"))
temp$Mean <- temp$Mean %>% as.numeric()
temp$High <- temp$High %>% as.numeric()
temp_wiz <- temp %>% filter(Site == "Wizard")
temp_tay <- temp %>% filter(Site == "Taylor")

wiz_plot<-ggplot(temp_wiz, aes(x = Date, y = Mean))+
  geom_point(aes(color = Great.mean))+
  geom_line()+
  theme_cowplot()+
  ylim(c(7,15))+
  scale_color_manual(values = c("black","red"))

tay_plot<-ggplot(temp_tay, aes(x = Date, y = Mean))+
  geom_point(aes(color = Great.mean))+
  geom_line()+
  theme_cowplot()+
  ylim(c(7,15))+
  scale_color_manual(values = c("black","red"))

plot_grid(wiz_plot, tay_plot, ncol=1)


ggplot(temp_wiz, aes(x = Date, y = High))+
  geom_point(aes(color = Great.max))+
  geom_line()+
  theme_cowplot()+
  ylim(c(7,16))+
  scale_color_manual(values = c("black", "pink", "red"))

ggplot(temp_tay, aes(x = Date, y = High))+
  geom_point(aes(color = Great.max))+
  geom_line()+
  theme_cowplot()+
  ylim(c(7,16))+
  scale_color_manual(values = c("black", "pink", "red"))

temp_wiz %>% group_by(Year) %>%
  summarise(max = max(Mean, na.rm = TRUE)) %>% 
  ggplot(aes(y = max, x = Year))+
  geom_point()

temp_tay %>% group_by(Year) %>%
  summarise(max = max(Mean, na.rm = TRUE)) %>% 
  ggplot(aes(y = max, x = Year))+
  geom_point()

temp %>% group_by(Year, Site) %>%
  summarise(max = max(Mean, na.rm = TRUE)) %>% 
  ggplot(aes(y = max, x = Year, col = Site))+
  geom_point()+
  stat_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values = c( "grey", "black"))+
  theme_cowplot()+
  ylab("Average temperature of warmest month")

temp_max<- temp %>% group_by(Year, Site) %>%
  summarise(max = max(Mean, na.rm = TRUE))

SST.data.sum %>% filter(Year > 1989) %>% group_by(Year) %>%
  summarise(max = max(Temperature, na.rm = TRUE)) %>% 
  ggplot(aes(y = max, x = Year))+
  geom_point()+
  xlim(c("2000-01-01", "2020-01-01"))

lm(max~Year, data = temp_max[temp_max$Site=="Wizard",]) %>% anova()

lm(max~Year, data = temp_max[temp_max$Site=="Taylor",]) %>% anova()


SST.data.sum %>% filter(Year > 1989) %>% group_by(Year) %>%
  summarise(max = max(Temperature, na.rm = TRUE))

