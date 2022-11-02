# Radon flux plots from ANSTO routine Merge_CR_AG.py
#Read the output from previous python script 
# path of the data in my pc
path <- paste("C:/Users/gross/Desktop/traceRadon/WP2_scientific_material/A2.1.2/ANSTO_Flux/Pruebas_Autoflux_INTE/20210712SNT",sep="")
setwd(path)        
     
# Plot data   

data <- read.table('AutoFlux_summary.csv', header= T, sep=',')
data <- subset(data, data$Flux > 0)
press <- read.table('AutoFlux_summary2.csv', header= T, sep=',') 
# Prepare data coloumn  

data$Date <- as.POSIXct(data$Datetime, format="%d/%m/%Y %H:%M",tz="GMT")
press$Date <- as.POSIXct(press$Datetime, format="%d/%m/%Y %H:%M",tz="GMT")
#data$Flux <- data$RnFlux*3.6
# Starting and ending date of the detectors exposure in POSIXct format
date.ini <- ISOdatetime(2021, 07, 07, 12, 00, 00, tz = "GMT")
date.end <- ISOdatetime(2021, 07, 12, 03, 00, 00, tz = "GMT")
ini.end <- as.POSIXct(strptime(c(date.ini, date.end), format= Ymd.format), tz = "GMT")
tiff("Rn_Flux.tiff", width = 10, height = 7, units = "in",res=200)
par(mar=c(2,3.8,1,3.9)+.1)
plot(data$Date,data$Flux,type='b', xaxt='n',col='black',ylim= c(0,7000), ylab=expression(paste('222-Rn Flux'," ",
                                                           '(Bq','m'^-2,h^-1,')',sep=" ")))
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
#par(new = TRUE)
#plot(my$Date+10800,my$Flux,type='b', xaxt='n',col='green',ylim= c(0,10000), ylab=expression(paste('222-Rn Flux'," ",
#                                                                                      '(Bq','m'^-2,h^-1,')',sep=" ")))
# arrows(x0=data$Date,
#        y0=data$Flux-data$Std_err, 
#        x1=data$Date,
#        y1=data$Flux+data$Std_err,
#        angle=90, 
#        code=3,
#        length=0.04,
#        lwd=0.4)
#text(data$Date,data$Flux, labels=data$Flux, cex=0.9, font=2) 
abline(a=mean(data$Flux),b=0,col='blue')
abline(a=mean(data$Flux)+sd(data$Flux),b=0,col='blue',lty=2)
abline(a=mean(data$Flux)-sd(data$Flux),b=0,col='blue',lty=2)
dev.off()

tiff("Rn_Flux.unc.percent.tiff", width = 10, height = 7, units = "in",res=200)
plot(data$Date,data$Std_err*100/data$Flux, type='b',xlab='Date',ylab='Uncertanty (k=1) (%)',ylim=c(0,70000))
abline(a=mean(data$Std_err*100/data$Flux),b=0,col='blue')
abline(a=10,b=0,col='green')
# abline(a=mean(data$Std_err*100/data$Flux)+sd(data$Std_err*100/data$Flux),b=0,col='blue',lty=2)
# abline(a=mean(data$Std_err*100/data$Flux)-sd(data$Std_err*100/data$Flux),b=0,col='blue',lty=2)
dev.off()



# join dataframes
ds <- merge(data,press,by="Date",all.x=TRUE)
write.csv(ds,file = 'data.csv')

dt <- ds[,c(3,4,5,6,7,8,9,10,11,12,13,14,17,18)] 
#eliminate NaN
dp <- subset(dt, dt$VWC_Avg > 0)
library(PerformanceAnalytics)
chart.Correlation(dp, histogram=TRUE, pch=19)  


tiff("Rn_Flux.rain.tiff", width = 10, height = 7, units = "in",res=200)
par(mar=c(4,12,4,4)+.1)
plot(ds$Date, ds$Flux, type="b",col="black",pch=16, yaxt="n",xaxt="n",lwd=0.7,
     main='Flux ENEA',ylab="",xlab='Date')
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
axis(2, pretty(c(min(ds$Flux), max(ds$Flux))), col='black')
mtext(expression(paste('Rn'^222," ",'(Bq/','m'^2,'h',')',sep=" ")),side=2,line=3)
# second plot
par(new = TRUE)
plot(ds$Date, ds$Rain_Tot, xaxt="n",type="b", pch=16,col='red',yaxt="n",ylim=c(0,10), lwd=0.7,xlab="",ylab="")
axis(4, pretty(c(0, 10)),col.axis = "red",col='red')
mtext(expression(paste('Rain'," ",'(mm)',sep=" ")),side=4,line=3,col='red')
dev.off()

tiff("Rn_Flux_Temp.tiff", width = 10, height = 7, units = "in",res=200)
par(mar=c(4,12,4,4)+.1)
plot(ds$Date, ds$Flux/3.6, type="b",col="black",pch=16, yaxt="n",xaxt="n",lwd=0.7,
     main='Flux ENEA',ylab="",xlab='Date')
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
axis(2, pretty(c(min(ds$Flux/3.6), max(ds$Flux/3.6))), col='black')
mtext(expression(paste('Rn'^222," ",'(mBq/','m'^2,'s',')',sep=" ")),side=2,line=3)
# second plot
par(new = TRUE) 
plot(ds$Date, ds$DrumTemp_Avg, xaxt="n",type="b", pch=16,col='red',yaxt="n", ylim=c(-10,50), lwd=0.7,xlab="",ylab="")
axis(4, pretty(c(-10, 50)),col.axis = "red",col='red')
mtext(expression(paste('T drum (red) and T soil (blue)'," ",'(ºC)',sep=" ")),side=4,line=3,col='red')
par(new = TRUE) 
plot(ds$Date, ds$T_Avg, xaxt="n",type="b", pch=16,col='blue',yaxt="n", ylim=c(-10,50), lwd=0.7,xlab="",ylab="")
dev.off()

tiff("Rn_Flux_Temp.tiff", width = 10, height = 7, units = "in",res=200)
par(mar=c(4,12,4,4)+.1)
plot(ds$Date, ds$Flux, type="b",col="black",pch=16, yaxt="n",xaxt="n",lwd=0.7,
     main='Flux ENEA',ylab="",xlab='Date')
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
axis(2, pretty(c(min(ds$Flux), max(ds$Flux))), col='black')
mtext(expression(paste('Rn'^222," ",'(Bq/','m'^2,'h',')',sep=" ")),side=2,line=3)
# second plot
par(new = TRUE) 
plot(ds$Date, ds$DrumTemp_Avg, xaxt="n",type="b", pch=16,col='red',yaxt="n", ylim=c(-10,50), lwd=0.7,xlab="",ylab="")
axis(4, pretty(c(-10, 50)),col.axis = "red",col='red')
mtext(expression(paste('Drum Temp'," ",'(ºC)',sep=" ")),side=4,line=3,col='red')
dev.off()

tiff("Rn_Flux_against_Diff_Pressure.tiff", width = 10, height = 7, units = "in",res=200)
plot(ds$DiffPress_Slope,ds$Flux,xlab='Pressure Diff',xlim=c(-2,2),ylab='Radon Flux (Bq/m2h)',ylim=c(0,10040))
# second plot
par(new = TRUE)
plot(ds$DiffPress_Diff,ds$Flux,col='blue',xlab='',xlim=c(-2,2),ylab='Radon Flux (Bq/m2h)',ylim=c(0,10040))
legend("topleft",col=c("black","blue"),y.intersp=1.0,bty="n",
       pch=19,legend=c(expression(paste('Slope (Pa/h)',sep=" ")),expression(paste('Corrected_Diff_Pressure_increment (Pa)',sep=" "))),cex=1.0)
dev.off()

dat <- read.table('AutoFlux1.csv', header= T, sep=',')
dat$Datetime <- as.POSIXct(dat$Datetime, format="%d/%m/%Y %H:%M",tz="GMT")
dat$date <- dat$Datetime
# read doseman data
# Read Doraymon data

dd <- dat[c(6:nrow(dat)),]
# Starting and ending date of the detectors exposure in POSIXct format
date.ini <- ISOdatetime(2021, 07, 07, 10, 20, 00, tz = "GMT")
date.end <- ISOdatetime(2021, 07, 12, 07, 50, 00, tz = "GMT")
tiff("Rn_time_series_inte.tiff", res = 10000) 
par(mfrow=c(3,1),mar=c(4,5,2,3))
plot(dd$date,dd$radon,type='b', xaxt='n',lwd=3,ylim=c(0,30000), cex.lab=1.5,cex.axis=1.5,xlab="Date-Time", ylab=expression(paste('222Rn Concentration',"  ",
                                                                   '(Bq',' ','m'^-3,')',sep=" ")))
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",cex.lab=1.5,cex.axis=1.5)
legend("topright",col=c("black"),y.intersp=1.0,bty="n",
       legend=c('a)'),cex=1.3)

plot(dd$date, dd$DrumTemp_Avg,type='b', xaxt='n',ylim=c(19,23), ylab='Drum_Temp_Avg (ºC)',col='red',xlab="Date-Time",cex.lab=1.5,cex.axis=1.5)
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",cex.lab=1.5,cex.axis=1.5)
legend("topright",col=c("black"),y.intersp=1.0,bty="n",
       legend=c('b)'),cex=1.3)

plot(dd$date, dd$VWC_Avg,xaxt='n', type='b',xlab="Date-Time", ylim=c(0.015,0.040),col='blue',cex.lab=1.5,cex.axis=1.5, ylab=expression(paste('VWC'," ",
                                                                     '(','m'^3,'m'^-3,')',sep=" ")))
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date",cex.lab=1.5,cex.axis=1.5)
legend("topright",col=c("black"),y.intersp=1.0,bty="n",
       legend=c('c)'),cex=1.3)
dev.off() 


# date.ini2 <- ISOdatetime(2022, 05, 05, 12, 00, 00, tz = "GMT") 
# sub <- subset(dat, dat$Date >= date.ini2)

tiff("Rn_conc_AG_quality.tiff", width = 13, height = 7, units = "in",res=200)
plot(dat$Date,dat$radon,type='b', xaxt="n",xlab='Date',ylab=expression(paste('222Rn concentration within the Autoflux'," ","    ",
                                                                             '(Bq'," ",'m'^-3,')',sep=" ")))
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
# par(new = TRUE)
# # #plot(data$Date, data$DrumTemp_Avg, xaxt="n",type="b", col='blue',yaxt="n", lwd=0.7,xlab="",ylab="")
# plot(data$Date, data$Flux, xaxt="n",type="b", col='red',yaxt="n", lwd=0.7,xlab="",ylab="") 
#text(dat$Datetime,dat$radon, labels=dat$radon, cex=0.9, font=2)
dev.off() 

tiff("Rn_time_series.tiff", res = 10000) 
par(mfrow=c(8,1),mar=c(2,5,2,3))
plot(data$Date,data$Flux,type='b', xaxt='n', ylab=expression(paste('222-Rn Flux'," ",
                                                                       '(Bq','m'^-2,h^-1,')',sep=" ")))
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
#text(data$Date,data$Flux, labels=data$Flux, cex=0.9, font=2)
abline(a=mean(data$Flux),b=0,col='blue')
abline(a=mean(data$Flux)+sd(data$Flux),b=0,col='blue',lty=2)
abline(a=mean(data$Flux)-sd(data$Flux),b=0,col='blue',lty=2)
plot(dat$Date, dat$Flow_Avg,xaxt='n', type='b', ylab='Flow (L/min))',xlab='date') 
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
plot(dat$Date,dat$T_Avg,type='b', xaxt='n',ylab='T_Avg (ºC)',xlab='date')
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
plot(dat$Date, dat$VaporPress_Avg,xaxt='n', type='b',ylab='VaporPress_Avg (kPa)',xlab='date')
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
plot(dat$Date, dat$AirTemp_Avg,xaxt='n',type='b', ylab='AirTemp_Avg (ºC)',xlab='date')
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
plot(dat$Date, dat$DrumTemp_Avg,xaxt='n',type='b', ylab='DrumTemp_Avg (ºC)',xlab='date')
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
plot(dat$Date, dat$Rain_Tot,xaxt='n', type='b', ylab='Rain (mm)',xlab='date') 
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
plot(dat$Date, dat$VWC_Avg,xaxt='n', type='b', ylab='VWC (mm3/mm3)',xlab='date') 
axis.POSIXct(1, at=seq(date.ini, date.end, by = "hour"), format="%Y-%m-%d %H:%M",xlab="Date")
dev.off()  

