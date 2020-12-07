## 
## Werp WQ
## L28I Canal OFW
##
## Code was compiled by Paul Julian
## contact info: paul.julian@floridadep.gov

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
# Data Wrangling
library(AnalystHelper);
library(plyr)
library(reshape)
library(openxlsx)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)

# Paths
wd="C:/Julian_LaCie/_Github/WERP_WQ"
paths=paste0(wd,c("/Exports","/Plots","/Data"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
export.path=paths[1]
plot.path=paths[2]
data.path=paths[3]

GIS.path="C:/Julian_LaCie/_GISData"

# Helper variables
nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


# -------------------------------------------------------------------------
dates=date.fun(c("1977-05-01","2016-05-01"))

# WQ Data
parameters=data.frame(Test.Number=c(25,23,18,21,80,8,7),Param=c("TP","OP","NOx","TKN","TN","DO.mgL","Temp"))
wq.sites=c("WWEIR","PC17A","S190","L28I","USSO","S140")
wq.dat=DBHYDRO_WQ(dates[1],dates[2],wq.sites,parameters$Test.Number)
# write.csv(wq.dat,paste(data.path,"/",format(Sys.Date(),"%Y%m%d"),"_canalwq.csv",sep=""),row.names = F)

wq.dat=merge(wq.dat,parameters,"Test.Number")
unique(wq.dat$Collection.Method)
wq.dat=subset(wq.dat,Collection.Method%in%c("G","ACF","ACT","ADT"))
wq.dat$Samp.Date=with(wq.dat,ifelse(Collection.Method%in%c("ACF","ACT","ADT")&is.na(First.Trigger.Date)==F,as.character(First.Trigger.Date),as.character(Collection_Date)))
wq.dat$Samp.Date=as.POSIXct(strptime(wq.dat$Samp.Date,"%F"),tz=tz.val)
head(wq.dat[,c("Collection.Method","Collection_Date","Samp.Date","Date.EST")],20L)

wq.dat.xtab=cast(subset(wq.dat,Collection.Method%in%c("G","GP")),DateTime.EST+Date.EST+Date+Collection.Method+Station.ID~Param,value="HalfMDL",mean)
wq.dat.xtab$WY=with(wq.dat.xtab,WY(DateTime.EST))
wq.dat.xtab$HydroSeason=with(wq.dat.xtab,FL.Hydroseason(Date.EST))
wq.dat.xtab$TP.ugL=with(wq.dat.xtab,TP*1000)
wq.dat.xtab$OPFlag=with(wq.dat.xtab,ifelse(is.na(OP)|OP==0,1,0));
wq.dat.xtab$TPFlag=with(wq.dat.xtab,ifelse(is.na(TP),1,0));
wq.dat.xtab$Reversal=with(wq.dat.xtab,ifelse(OPFlag==1,0,ifelse(OP>(TP*1.3),1,0)));
subset(wq.dat.xtab,TP<OP)
sum(wq.dat.xtab$Reversal,na.rm=T)
wq.dat.xtab$TN.Final=with(wq.dat.xtab,TN_Combine(NOx,TKN,TN))

# Data Screening 
wq.dat.screenTP=cast(wq.dat.xtab,Station.ID+WY~HydroSeason,value="TP.ugL",fun.aggregate=function(x)N.obs(x))
head(wq.dat.screenTP)
wq.dat.screenTP$NTotal=with(wq.dat.screenTP,A_Wet+B_Dry);
wq.dat.screenTP$SeasonScreen=with(wq.dat.screenTP,ifelse(B_Dry>=1&A_Wet>=1,1,0));
wq.dat.screenTP$NScreen=with(wq.dat.screenTP,ifelse(NTotal>=4,1,0));
wq.dat.screenTP$TP.UseData=with(wq.dat.screenTP,ifelse((SeasonScreen+NScreen)==2,"Yes","No"));
head(wq.dat.screenTP)

wq.dat.screenTN=cast(wq.dat.xtab,Station.ID+WY~HydroSeason,value="TN.Final",fun.aggregate=function(x)N.obs(x))
head(wq.dat.screenTN)
wq.dat.screenTN$NTotal=with(wq.dat.screenTN,A_Wet+B_Dry);
wq.dat.screenTN$SeasonScreen=with(wq.dat.screenTN,ifelse(B_Dry>=1&A_Wet>=1,1,0));
wq.dat.screenTN$NScreen=with(wq.dat.screenTN,ifelse(NTotal>=4,1,0));
wq.dat.screenTN$TN.UseData=with(wq.dat.screenTN,ifelse((SeasonScreen+NScreen)==2,"Yes","No"));
head(wq.dat.screenTN)

screened.dat=merge(wq.dat.xtab,wq.dat.screenTP[,c("Station.ID","WY","TP.UseData")],by=c("Station.ID","WY"),all.x=T)
screened.dat=merge(screened.dat,wq.dat.screenTN[,c("Station.ID","WY","TN.UseData")],by=c("Station.ID","WY"),all.x=T)
TP.AGM=ddply(screened.dat,c("Station.ID","WY","TP.UseData"),summarise,N=length(TP.ugL),Geomean=exp(mean(log(TP.ugL),na.rm=T)),N=N.obs(TP.ugL),sd=sd(TP.ugL,na.rm=T),GM.SE=Geomean*(sd(log(TP.ugL),na.rm=T)/sqrt(N-1)));
TP.AGM=merge(TP.AGM,data.frame(Station.ID=sort(rep(wq.sites,length(seq(WY(dates[1]),WY(dates[2])-1,1)))),WY=rep(seq(WY(dates[1]),WY(dates[2])-1,1),length(wq.sites))),by=c("Station.ID","WY"),all.y=T)

#write.csv(subset(TP.AGM,Station.ID=="L28I"),paste(data.path,"/",format(Sys.Date(),"%Y%m%d"),"_L28I_TPAGM.csv",sep=""),row.names = F)

ddply(subset(TP.AGM,WY%in%seq(2000,2016,1)),c("Station.ID"),summarise,mean=mean(Geomean,na.rm=T))
TN.AGM=ddply(screened.dat,c("Station.ID","WY","TN.UseData"),summarise,N=length(TN.Final),Geomean=exp(mean(log(TN.Final),na.rm=T)),N=N.obs(TN.Final),sd=sd(TN.Final,na.rm=T),GM.SE=Geomean*(sd(log(TN.Final),na.rm=T)/sqrt(N-1)));
TN.AGM=merge(TN.AGM,data.frame(Station.ID=sort(rep(wq.sites,length(seq(WY(dates[1]),WY(dates[2])-1,1)))),WY=rep(seq(WY(dates[1]),WY(dates[2])-1,1),length(wq.sites))),by=c("Station.ID","WY"),all.y=T)

#write.csv(subset(TN.AGM,Station.ID=="L28I"),paste(data.path,"/",format(Sys.Date(),"%Y%m%d"),"_L28I_TNAGM.csv",sep=""),row.names = F)

head(subset(screened.dat,Station.ID=="PC17A"))
cast(screened.dat,TP.UseData~TN.UseData,value="Station.ID",fun.aggregate = function(x)N.obs(x))


# Canal OFW Derivation ----------------------------------------------------
OFW.period=as.POSIXct(strptime(c("1977-05-01","1979-03-01"),"%F"),tz="America/New_York")
OFW.period=as.POSIXct(strptime(seq(OFW.period[1],OFW.period[2],"1 days"),"%F"),tz="America/New_York")
wq.dat.xtab$OFW=with(wq.dat.xtab,ifelse(Date%in%OFW.period,1,0))

#Baseline Period
baseline.test=merge(wq.dat.xtab,data.frame(WY=seq(1978,1985,1),Period=c(rep("baseline",4),rep("post-base",4))),"WY")
baseline.test.AGM=ddply(subset(baseline.test,Period%in%c("baseline","post-base")),c("WY","Period"),summarise,GM.TP.ugL=exp(mean(log(TP.ugL),na.rm=T)),GM.TN.mgL=exp(mean(log(TN.Final),na.rm=T)))

kruskal.test(TP.ugL~Period,subset(baseline.test,Period%in%c("baseline","post-base")))
kruskal.test(TN.Final~Period,subset(baseline.test,Period%in%c("baseline","post-base")))

kruskal.test(GM.TP.ugL~Period,baseline.test.AGM)
kruskal.test(GM.TN.mgL~Period,baseline.test.AGM)


ylim.val=c(20,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.val2=c(1,2.5);by.y2=0.5;ymaj2=seq(ylim.val2[1],ylim.val2[2],by.y2);ymin2=seq(ylim.val2[1],ylim.val2[2],by.y2/2)

# tiff(filename=paste0(plot.path,"/L28I_BaselineCompare_landscape.tiff"),width=5,height=3,units="in",res=220,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1,2.5,1,1),oma=c(1.5,1,0.25,0.25),mgp=c(3,1,0));
layout(matrix(seq(1,2,1),1,2,byrow=T));

boxplot(GM.TP.ugL~Period,baseline.test.AGM,yaxt="n",xaxt="n",col=adjustcolor("dodgerblue1",0.75),ylim=ylim.val)
axis_fun(2,ymaj,ymin,ymaj,cex.axis=1)
axis_fun(1,line=-0.5,c(1,2),c(1,2),c("Baseline","Post-Baseline"),cex.axis=0.75)
axis(side=1,line=0.2,c(1,2),c("(WY1978-1981)","(WY1982-1985)"),lty=0,cex.axis=0.5)
mtext(side=2,line=2,expression(paste("Total Phosphorus (",mu," g L"^"-1",")",sep="")))

boxplot(GM.TN.mgL~Period,baseline.test.AGM,yaxt="n",xaxt="n",col=adjustcolor("indianred1",0.75),ylim=ylim.val2)
axis_fun(2,ymaj2,ymin2,format(ymaj2),cex.axis=1)
axis_fun(1,line=-0.5,c(1,2),c(1,2),c("Baseline","Post-Baseline"),cex.axis=0.75)
axis(side=1,line=0.2,c(1,2),c("(WY1978-1981)","(WY1982-1985)"),lty=0,cex.axis=0.5)
mtext(side=2,line=2,expression(paste("Total Nitrogen (mg L"^"-1",")",sep="")))
dev.off()

#TP
ddply(subset(wq.dat.xtab,Station.ID=="L28I"&WY%in%seq(1978,1981,1)),c("WY"),summarise,GM.TP.ugL=round(exp(mean(log(TP.ugL),na.rm=T)),1),N.TP=N.obs(TP.ugL),TP.AGM.SE=GM.TP.ugL*(sd(log(TP.ugL),na.rm=T)/sqrt(N.TP-1)),GM.TN.mgL=round(exp(mean(log(TN.Final),na.rm=T)),2),N.TN=N.obs(TN.Final),TN.AGM.SE=GM.TN.mgL*(sd(log(TN.Final),na.rm=T)/sqrt(N.TN-1)))

annual.geomeans=ddply(subset(wq.dat.xtab,Station.ID=="L28I"&WY%in%seq(1978,1981,1)),c("WY"),summarise,GM=exp(mean(log(TP.ugL),na.rm=T)))

# Long Term (Confidence Interval)
Mean.GM=mean(annual.geomeans$GM);Mean.GM
SD.GM=sd(annual.geomeans$GM);SD.GM
N.val=N.obs(annual.geomeans$GM);N.val
DOF=N.val-1;DOF
Tp=abs(qt(0.05,DOF));Tp
LT.Mean=Mean.GM+(SD.GM*Tp)/sqrt(N.val)
round(LT.Mean,0)

Tp.target=abs(qt(0.5,DOF));Tp
LT.Mean.target=Mean.GM+(SD.GM*Tp.target)/sqrt(N.val)
round(LT.Mean.target,0)

# Annual (Prediction Interval)
ybar=mean(log(annual.geomeans$GM),na.rm=T);ybar
var=var(log(annual.geomeans$GM),na.rm=T);var
N.val=N.obs(annual.geomeans$GM)
DOF=N.val-1;DOF
Tp=abs(qt(0.05,DOF));Tp
PI=exp(ybar+Tp*sqrt(var+(var/N.val)));PI


xlim.val=c(1977,2017);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
# tiff(filename=paste0(plot.path,"/L28I_AGMTPTN.tiff"),width=5.5,height=6.5,units="in",res=220,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1,1.5,1,1),oma=c(6,3,0.25,0.25),mgp=c(3,1,0));
layout(matrix(seq(1,2,1),2,1,byrow=T));
pt.cex=1.5

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Geomean~WY,TP.AGM,type="n",ylim=ylim.val,xlim=xlim.val,xaxt="n",yaxt="n",xaxs="i",ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,col="grey",lty=3)
polygon(x=c(1978,1981,1981,1978),y=c(-10,-10,200,200),col=adjustcolor("grey",0.50))
#with(subset(TP.AGM,Station.ID=="L28I"),errorbars(WY,Geomean,GM.SE,"dodgerblue1"))
with(subset(TP.AGM,Station.ID=="L28I"),pt_line(WY,Geomean,2,"dodgerblue1",1,21,"dodgerblue1",cex=pt.cex))
#with(subset(TP.AGM,Station.ID=="S190"),errorbars(WY,Geomean,GM.SE,"indianred1"))
with(subset(TP.AGM,Station.ID=="S190"),pt_line(WY,Geomean,2,"indianred1",1,21,"indianred1",cex=pt.cex))
axis_fun(1,xmaj,xmin,xmaj,cex.axis = 1)
axis_fun(2,ymaj,ymin,ymaj,cex.axis = 1)
box(lwd=1)
mtext(side=2,line=3,"Total Phosphorus")
mtext(side=2,line=2,expression(paste("Geometric Mean (  ",mu,"g L"^"-1",")",sep="")),cex=1)

ylim.val=c(0.5,2);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Geomean~WY,TN.AGM,type="n",ylim=ylim.val,xlim=xlim.val,xaxt="n",yaxt="n",xaxs="i",ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,col="grey",lty=3)
polygon(x=c(1978,1981,1981,1978),y=c(-10,-10,200,200),col=adjustcolor("grey",0.50))
with(subset(TN.AGM,Station.ID=="L28I"),pt_line(WY,Geomean,2,"dodgerblue1",1,21,"dodgerblue1",cex=pt.cex))
with(subset(TN.AGM,Station.ID=="S190"),pt_line(WY,Geomean,2,"indianred1",1,21,"indianred1",cex=pt.cex))
axis_fun(1,xmaj,xmin,xmaj,cex.axis = 1)
axis_fun(2,ymaj,ymin,format(ymaj),cex.axis = 1)
box(lwd=1)
mtext(side=1,line=2,"Florida Water Year",cex=1.25)
mtext(side=2,line=3,"Total Nitrogen")
mtext(side=2,line=2,expression(paste("Geometric Mean (mg L"^"-1",")",sep="")),cex=1)
leg.x=xlim.val[1]+(diff(xlim.val)/2)
leg.y=ylim.val[1]-0.45
legend(leg.x,leg.y,legend=c("L-28I","S-190","Baseline Period"),pch=c(21,21,22),pt.bg=c("dodgerblue1","indianred1","grey"),col=c("dodgerblue1","indianred1","black"),lty=c(2,2,0),lwd=0.5,pt.cex=1.25,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,text.col="white")
legend(leg.x,leg.y,legend=c("L-28I","S-190","Baseline Period"),pch=c(21,21,22),pt.bg=c("dodgerblue1","indianred1","grey"),col=c("black"),lty=0,lwd=0.5,pt.cex=1.25,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()