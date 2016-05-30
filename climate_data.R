setwd("C:\\Users\\geo_masc\\Desktop\\Climate data\\Daily")
source("G:\\temp\\temp_Marcel\\Scripts\\R\\multiplot.R")

library(ggplot2)
library(gridExtra)

clim_data<-as.data.frame(read.csv("Climate_data_daily_Brasilia_1981_2014_.csv", sep=";"))
ls_doy<-as.data.frame(read.csv("G:\\temp\\temp_Marcel\\Time Series\\PNB\\LS_doys_2000_2006_2013.csv", sep=";"))

clim_ls_2000<-clim_data[which(clim_data$Doy %in% ls_doy$X2000_01),]
clim_ls_2006<-clim_data[which(clim_data$Doy %in% ls_doy$X2006_07),]
clim_ls_2013<-clim_data[which(clim_data$Doy %in% ls_doy$X2013_14),]

clim_ls_2000$Date<-strptime(clim_ls_2000$Date, format="%d. %m. %Y")
clim_ls_2006$Date<-strptime(clim_ls_2006$Date, format="%d. %m. %Y")
clim_ls_2013$Date<-strptime(clim_ls_2013$Date, format="%d. %m. %Y")


p1 <- ggplot(data = clim_ls_2000, aes(x=Date))+
  geom_line(aes(y=Precip, colour= "Precip"))+ 
  ylab("Precipitation")+
  ggtitle("Precipitaion at 2000 - 2001 Landsat DOY")+
  theme(plot.title=element_text(size=15, vjust=3)) 

p2 <- ggplot(data = clim_ls_2006, aes(x=Date))+
  geom_line(aes(y=Precip, colour= "Precip"))+ 
  ylab("Precipitation")+
  ggtitle("Precipitaion at 2006 - 2007 Landsat DOY")+
  theme(plot.title=element_text(size=15, vjust=3)) 

p3 <- ggplot(data = clim_ls_2013, aes(x=Date))+
  geom_line(aes(y=Precip, colour= "Precip"))+ 
  ylab("Precipitation")+
  ggtitle("Precipitaion at 2013 - 2014 Landsat DOY")+
  theme(plot.title=element_text(size=15, vjust=3)) 



multiplot(p1,p2,p3, cols=2)



# Calculate 30 year mean
clim.30.yr<-clim_data[clim_data$Year < 2011,]
clim.30.yr.precip<-aggregate(clim_data$Precip ~ clim_data$Year + clim_data$Month, FUN=sum)
clim.30.yr.temp<-aggregate(clim_data$T_Mean ~ clim_data$Year + clim_data$Month, FUN=mean)

names<-c("Year", "Month", "Precip")
names(clim.30.yr.precip)<-names

clim.30.yr.precip.mean<-aggregate(clim.30.yr.precip$Precip ~ clim.30.yr.precip$Month, FUN=mean)
names_2<-c("Month", "Precip")
names(clim.30.yr.precip.mean)<-names_2


names_3<-c("Year", "Month", "T_Mean")
names(clim.30.yr.temp)<-names_3

clim.30.yr.temp.mean<-aggregate(clim.30.yr.temp$T_Mean ~ clim.30.yr.temp$Month, FUN=mean)
names_4<-c("Month", "T_Mean")
names(clim.30.yr.temp.mean)<-names_4




p1 <- ggplot(data = clim.30.yr.precip.mean, aes(x=Month))+
  geom_line(aes(y=Precip, colour= "Precip"))+ 
  ylab("Precipitation in mm")+
  scale_colour_manual("", breaks = clim.30.yr.precip.mean$Precip, values = "blue")+  
  #coord_cartesian(xlim = c(1,12))+
  scale_x_discrete(breaks=clim.30.yr.precip.mean$Month, labels=c("Januar","February","March","April", "May", "June", "July", "August", "September", "October", "November", "December"))+
  ggtitle("30 Year Normalperiod precipitaiton 1981-2010")+
  theme(plot.title=element_text(size=15, vjust=2)) 


plot(p1)



clim.30.yr.precip$Date<-as.Date(paste(clim.30.yr.precip$Year,clim.30.yr.precip$Month,1, sep="_"),format="%Y_%m_%d")

p2 <- ggplot(data = clim.30.yr.precip, aes(x=Date))+
  geom_line(aes(y=Precip, colour= "Precip"))+ 
  ylab("Precipitation in mm")+
  xlab("Year")+
  scale_colour_manual("", breaks = clim.30.yr.precip$Precip, values = "blue")+  
  coord_cartesian(xlim = as.Date(c("2013-06-01", "2014-07-01")))+
  #scale_x_discrete(breaks=clim.30.yr.precip$Date, labels=c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014"))+
  ggtitle("Monthly precipitation Brasilia 2000-2014")+
  theme(plot.title=element_text(size=15, vjust=2)) 


plot(p2)

# Multiplot of the precipitation in the seleceted seasons

s1 <- ggplot(data = clim.30.yr.precip, aes(x=Date))+
  geom_line(aes(y=Precip, colour= "Precip"))+ 
  ylab("Precipitation in mm")+
  scale_colour_manual("", breaks = clim.30.yr.precip$Precip, values = "blue")+  
  coord_cartesian(xlim = as.Date(c("2000-06-01", "2001-07-01")))+
  ggtitle("2000-2001")+
  theme(plot.title=element_text(size=12, vjust=1),axis.title.x=element_blank()) 

s2 <- ggplot(data = clim.30.yr.precip, aes(x=Date))+
  geom_line(aes(y=Precip, colour= "Precip"))+ 
  ylab("Precipitation in mm")+
  scale_colour_manual("", breaks = clim.30.yr.precip$Precip, values = "blue")+  
  coord_cartesian(xlim = as.Date(c("2006-06-01", "2007-07-01")))+
  ggtitle("2006-2007")+
  theme(plot.title=element_text(size=12, vjust=1),axis.title.x=element_blank())

s3 <- ggplot(data = clim.30.yr.precip, aes(x=Date))+
  geom_line(aes(y=Precip, colour= "Precip"))+ 
  ylab("Precipitation in mm")+
  scale_colour_manual("", breaks = clim.30.yr.precip$Precip, values = "blue")+  
  coord_cartesian(xlim = as.Date(c("2013-06-01", "2014-07-01")))+
  ggtitle("2013-2014")+
  theme(plot.title=element_text(size=12, vjust=1),axis.title.x=element_blank())

grid.arrange(s1,s2,s3, ncol=1, main=textGrob("Seasonal precipitation patterns in Brasilia", gp=gpar(font=3, fontsize=18), just="top"))


