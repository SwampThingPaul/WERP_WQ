## 
## Werp WQ
## BCNP Numeric interpretation of narrative WQS
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
## GIS Data 
# personal geodatabase 
ogrListLayers(paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb"))
BCNP=spTransform(readOGR(paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb"),"BCNP"),utm17)
canals=spTransform(readOGR(paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),utm17)

# https://geo-sfwmd.hub.arcgis.com/datasets/environmental-monitoring-stations
wmd.mon=spTransform(readOGR(paste0(GIS.path,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),utm17)
wmd.mon=subset(wmd.mon,ACTIVITY_S=="Surface Water Grab")

# library(tmap)
# tmap_mode("view")
# tm_shape(BCNP)+tm_polygons()+
# tm_shape(wmd.mon[BCNP,])+tm_dots()

BCNP.mon.wq.all=wmd.mon[BCNP,]
# WQ Data -----------------------------------------------------------------
dates=date.fun(c("1977-05-01","2016-05-01"))
parameters=data.frame(Test.Number=c(25,23,18,21,80,8,7,9),Param=c("TP","OP","NOx","TKN","TN","DO.mgL","Temp","SPC"))
wq.sites2=data.frame(Station.ID=c(c('BCWQA1','BCWQA2','BCWQA17'),c('BCWQA18','BCWQA12'),c('BCWQA3','BCWQA4','BCWQA13','BCWQA14','BCWQA16'),c('BCWQA21','BCWQA15','BCWQA13A','BCWQA5','BCWQA6'),c('BCWQA8','BCWQA7','BCWQA11','BCWQA19'),c('BCWQA9A','BCWQA9','BCWQA10','BCWQA20')),
                     Region=c(rep('NW',3),rep('NE',2),rep('CW',5),rep('CE',5),rep('SW',4),rep('SE',4)),Region2=c(rep("N_C",15),rep("S",8)))
wq.dat.BCNP=data.frame()
for(i in 1:nrow(wq.sites2)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],wq.sites2$Station.ID[i],parameters$Test.Number)
  tmp$Sigfig.Value=as.numeric(tmp$Sigfig.Value)
  wq.dat.BCNP=rbind(tmp,wq.dat.BCNP)
}
#write.csv(wq.dat.BCNP,paste(export.path,"/",format(Sys.Date(),"%Y%m%d"),"_rawBCNPwq.csv",sep=""),row.names = F)

wq.dat.BCNP=merge(wq.dat.BCNP,parameters,"Test.Number")
wq.dat.BCNP=merge(wq.dat.BCNP,wq.sites2,"Station.ID")

#boxplot(SPC~Station.ID,wq.dat.xtab.BCNP)
#range(subset(wq.dat.xtab.BCNP,Station.ID=="BCWQA19")$SPC,na.rm=T)

wq.dat.xtab.BCNP=cast(subset(wq.dat.BCNP,Collection.Method%in%c("G","GP")),DateTime.EST+Date.EST+Collection.Method+Station.ID+Region2+Region~Param,value="HalfMDL",mean)
wq.dat.xtab.BCNP$WY=with(wq.dat.xtab.BCNP,WY(DateTime.EST))
wq.dat.xtab.BCNP$HydroSeason=with(wq.dat.xtab.BCNP,FL.Hydroseason(Date.EST))
wq.dat.xtab.BCNP$TP.ugL=with(wq.dat.xtab.BCNP,TP*1000)
wq.dat.xtab.BCNP$OPFlag=with(wq.dat.xtab.BCNP,ifelse(is.na(OP)|OP==0,1,0));
wq.dat.xtab.BCNP$TPFlag=with(wq.dat.xtab.BCNP,ifelse(is.na(TP),1,0));
wq.dat.xtab.BCNP$Reversal=with(wq.dat.xtab.BCNP,ifelse(OPFlag==1,0,ifelse(OP>(TP*1.3),1,0)));
subset(wq.dat.xtab.BCNP,TP<OP)
sum(wq.dat.xtab.BCNP$Reversal,na.rm=T)

wq.dat.xtab.BCNP$TN=NA
wq.dat.xtab.BCNP$TN.Final=with(wq.dat.xtab.BCNP,TN_Combine(NOx,TKN,TN))

wq.dat.screenTP=cast(subset(wq.dat.xtab.BCNP,Reversal==0),Station.ID+WY~HydroSeason,value="TP.ugL",fun.aggregate=function(x)N.obs(x))
head(wq.dat.screenTP)
wq.dat.screenTP$NTotal=with(wq.dat.screenTP,A_Wet+B_Dry);
wq.dat.screenTP$SeasonScreen=with(wq.dat.screenTP,ifelse(B_Dry>=1&A_Wet>=1,1,0));
wq.dat.screenTP$NScreen=with(wq.dat.screenTP,ifelse(NTotal>=4,1,0));
wq.dat.screenTP$TP.UseData=with(wq.dat.screenTP,ifelse((SeasonScreen+NScreen)==2,"Yes","No"));
head(wq.dat.screenTP)

wq.dat.screenTN=cast(wq.dat.xtab.BCNP,Station.ID+WY~HydroSeason,value="TN.Final",fun.aggregate=function(x)N.obs(x))
head(wq.dat.screenTN)
wq.dat.screenTN$NTotal=with(wq.dat.screenTN,A_Wet+B_Dry);
wq.dat.screenTN$SeasonScreen=with(wq.dat.screenTN,ifelse(B_Dry>=1&A_Wet>=1,1,0));
wq.dat.screenTN$NScreen=with(wq.dat.screenTN,ifelse(NTotal>=4,1,0));
wq.dat.screenTN$TN.UseData=with(wq.dat.screenTN,ifelse((SeasonScreen+NScreen)==2,"Yes","No"));
head(wq.dat.screenTN)

screened.dat=merge(wq.dat.xtab.BCNP,wq.dat.screenTP[,c("Station.ID","WY","TP.UseData")],by=c("Station.ID","WY"),all.x=T)
screened.dat=merge(screened.dat,wq.dat.screenTN[,c("Station.ID","WY","TN.UseData")],by=c("Station.ID","WY"),all.x=T)
TP.AGM=ddply(screened.dat,c("Station.ID","WY","Region2","TP.UseData"),summarise,N=N.obs(TP.ugL),Geomean=exp(mean(log(TP.ugL),na.rm=T)),sd=sd(TP.ugL,na.rm=T),SE.GM=Geomean*(sd(log(TP.ugL),na.rm=T)/sqrt(N.obs(TP.ugL)-1)));
TP.AGM=merge(TP.AGM,data.frame(Station.ID=sort(rep(wq.sites2$Station.ID,length(seq(WY(dates[1]),WY(dates[2])-1,1)))),WY=rep(seq(WY(dates[1]),WY(dates[2])-1,1),length(wq.sites2$Station.ID))),by=c("Station.ID","WY"),all.y=T)

TP.AGM2=ddply(screened.dat,c("WY","Region2","TP.UseData"),summarise,N=N.obs(TP.ugL),Geomean=exp(mean(log(TP.ugL),na.rm=T)));

TN.AGM=ddply(screened.dat,c("Station.ID","WY","Region2","TN.UseData"),summarise,N=N.obs(TN.Final),Geomean=exp(mean(log(TN.Final),na.rm=T)),sd=sd(TP.ugL,na.rm=T),SE.GM=Geomean*(sd(log(TN.Final),na.rm=T)/sqrt(N.obs(TN.Final)-1)));
TN.AGM=merge(TN.AGM,data.frame(Station.ID=sort(rep(wq.sites2$Station.ID,length(seq(WY(dates[1]),WY(dates[2])-1,1)))),WY=rep(seq(WY(dates[1]),WY(dates[2])-1,1),length(wq.sites2$Station.ID))),by=c("Station.ID","WY"),all.y=T)

# DO Data
wq.dat.xtab.BCNP$TimeFlag=with(wq.dat.xtab.BCNP,ifelse(as.numeric(format(DateTime.EST,"%H"))==0,1,ifelse(as.numeric(format(DateTime.EST,"%H"))<6&as.numeric(format(DateTime.EST,"%H"))>18,1,0)))
wq.dat.xtab.BCNP$DOFlag=with(wq.dat.xtab.BCNP,ifelse(DO.mgL>20|is.na(DO.mgL),1,0))
wq.dat.xtab.BCNP$TempFlag=with(wq.dat.xtab.BCNP,ifelse(Temp<5|Temp>42|is.na(Temp),1,0));
wq.dat.xtab.BCNP$UseData=ifelse(rowSums(wq.dat.xtab.BCNP[,c("TimeFlag","DOFlag","TempFlag")])>0,"No","Yes")
wq.dat.xtab.BCNP$DO.PerSat=with(wq.dat.xtab.BCNP,ifelse(UseData=="Yes",DO_PerSat(Temp,DO.mgL,0),NA))

DO.dat=subset(wq.dat.xtab.BCNP,UseData=="Yes")[,c("DateTime.EST","WY","Station.ID","Region2","UseData","DO.mgL","Temp","DO.PerSat")]
DO.dat$DO.WQS=with(DO.dat,DO.TOD.WQS.stream(DateTime.EST))
DO.dat$DOSatWQS.Assess=with(DO.dat,ifelse(DO.PerSat<DO.WQS,1,0));

DOWQS.Summary=ddply(DO.dat,c("Station.ID","Region2","WY"),summarise,
                    Mean.DOConc=round(mean(DO.mgL,na.rm=T),1),
                    Mean.DOSat=round(mean(DO.PerSat,na.rm=T),1),
                    StDev.DOSat=round(sd(DO.PerSat,na.rm=T),1),
                    SE.DOSat=round(SE(DO.PerSat),3),
                    Min.DOSat=round(min(DO.PerSat,na.rm=T),1),
                    Max.DOSat=round(max(DO.PerSat,na.rm=T),1),
                    N.DOSat=N.obs(DO.PerSat),
                    N.Exceed=sum(DOSatWQS.Assess),
                    Per.Exceed=N.Exceed/N.DOSat*100);
DOWQS.Summary$CompStatus=with(DOWQS.Summary,ifelse(Per.Exceed>10,"Fail","Pass"));
DOWQS.Summary=merge(DOWQS.Summary,data.frame(Station.ID=sort(rep(wq.sites2$Station.ID,length(seq(WY(dates[1]),WY(dates[2]),1)))),WY=rep(seq(WY(dates[1]),WY(dates[2]),1),length(wq.sites2$Station.ID))),by=c("Station.ID","WY"),all.y=T)

vars=c("Station.ID","WY","Mean.DOSat","Per.Exceed","CompStatus")
TP.DO=merge(TP.AGM,DOWQS.Summary[,vars],c("Station.ID","WY"))
TP.DO$Use.Final=with(TP.DO,ifelse(TP.UseData=="Yes"&CompStatus=="Pass",1,0))
TP.DO$Ln.Geomean=with(TP.DO,log(Geomean))
#write.csv(subset(TP.DO,Use.Final==1),paste(export.path,"BCNP_TPAGM_DO_forderivation.csv",sep=""),row.names = F)
#write.csv(TP.DO,paste(ExportPath,"BCNP_TPAGM_DO.csv",sep=""),row.names = F)

TN.DO=merge(TN.AGM,DOWQS.Summary[,vars],c("Station.ID","WY"))
TN.DO$Use.Final=with(TN.DO,ifelse(TN.UseData=="Yes"&CompStatus=="Pass",1,0))
TN.DO$Ln.Geomean=with(TN.DO,log(Geomean))
#write.csv(TN.DO,paste(ExportPath,"BCNP_TNAGM_DO.csv",sep=""),row.names = F)

####

graph.sites=c("BCWQA1","BCWQA2","BCWQA17","BCWQA18","BCWQA12","BCWQA3","BCWQA13","BCWQA21","BCWQA14","BCWQA16","BCWQA15","BCWQA13A","BCWQA4","BCWQA5")#,"BCWQA9A","BCWQA9","BCWQA10","BCWQA20","BCWQA19")

# tiff(filename=paste0(plot.path,"/DO_WQS_BCNP_Plot.tiff"),width=7,height=6.5,units="in",res=220,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1,1.5,1,1),oma=c(6,2.5,0.25,0.25),mgp=c(3,1,0));
layout(matrix(c(seq(1,14,1),0),3,5,byrow=T));
xlim.val=c(1993,2012);by.x=8;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],1)
ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 1:length(graph.sites)){
  plot(Mean.DOSat~WY,DOWQS.Summary,type="n",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA)
  abline(h=ymaj,v=xmaj,col="grey",lty=3)
  with(subset(DOWQS.Summary,Station.ID==graph.sites[i]),errorbars(WY,Mean.DOSat,SE.DOSat,col="black",length=0.025))
  with(subset(DOWQS.Summary,Station.ID==graph.sites[i]),lines(WY,Mean.DOSat,lty=2,lwd=1))
  with(subset(DOWQS.Summary,Station.ID==graph.sites[i]),points(WY,Mean.DOSat,pch=ifelse(CompStatus=="Fail",23,21),bg=ifelse(CompStatus=="Fail","indianred1","darkolivegreen2"),cex=1.25,lwd=0.5))
  axis_fun(1,line=-0.5,xmaj,xmin,xmaj,cex.axis=1)
  axis_fun(2,ymaj,ymin,ymaj,cex.axis=1)
  mtext(side=3,line=-1,paste(graph.sites[i]),cex=0.75,outer=F)
}
mtext(side=1,line=2,"Water Year",outer=T,cex=1.25)
mtext(side=2,line=1,"Dissolved Oxygen (% Saturation)",cex=1.25,outer=T)
leg.x=xlim.val[1]-19
leg.y=ylim.val[1]-80
legend(leg.x,leg.y,legend=c("Achieve WQS","Exceed WQS"),pch=c(21,23),pt.bg=c("darkolivegreen2","indianred1"),col=c("black"),lty=0,lwd=0.5,pt.cex=1.75,ncol=2,cex=1.5,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()

# tiff(filename=paste0(plot.path,"/TP_WQS_BCNP_Plot.tiff"),width=7,height=6.5,units="in",res=220,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1,1.5,1,1),oma=c(6,2.5,0.25,0.25),mgp=c(3,1,0));
layout(matrix(c(seq(1,14,1),0),3,5,byrow=T));
xlim.val=c(1993,2012);by.x=8;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],1)
ylim.val=c(1,300);by.y=50;
ymaj=c(1,10,100,1000);
ymin=c(seq(1,10,1),seq(10,100,10),seq(100,1000,100))

for(i in 1:length(graph.sites)){
  plot(Geomean~WY,TP.AGM,type="n",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,log="y")
  abline(h=ymaj,v=xmaj,col="grey",lty=3)
  with(subset(TP.AGM,Station.ID==graph.sites[i]),lines(WY,Geomean,lty=2,lwd=1))
  with(subset(TP.AGM,Station.ID==graph.sites[i]),points(WY,Geomean,pch=21,bg=adjustcolor(ifelse(TP.UseData=="Yes","dodgerblue1","grey"),0.8),col=ifelse(TP.UseData=="Yes","dodgerblue1","grey"),cex=1.25))
  axis_fun(1,line=-0.5,xmaj,xmin,xmaj,cex.axis=1)
  axis_fun(2,ymaj,ymin,ymaj,cex.axis=1)
  mtext(side=3,line=-1,paste(graph.sites[i]),cex=0.75)
}
mtext(side=2,line=1,expression(paste("Geometric Mean TP Concentration ( ",mu,"g L"^"-1",")",sep="")),cex=1,outer=T)
mtext(side=1,line=1,"Water Year",outer=T,cex=1.25)
leg.x=xlim.val[1]-19;leg.y=0.15
legend(leg.x,leg.y,legend=c("Seasonal Screening Passed","Seasonal Screening Failed"),pch=c(21,21),pt.bg=c("dodgerblue1","grey"),col=c("black"),lty=0,lwd=0.5,pt.cex=1.75,ncol=2,cex=1.5,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()

# tiff(filename=paste0(plot.path,"/TN_WQS_BCNP_Plot.tiff"),width=7,height=6.5,units="in",res=220,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1,1.5,1,1),oma=c(6,2.5,0.25,0.25),mgp=c(3,1,0));
layout(matrix(c(seq(1,14,1),0),3,5,byrow=T));
xlim.val=c(1993,2012);by.x=8;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],1)
ylim.val=c(0.5,7);by.y=1;
ymaj=c(0,1,2,4,6,8)
ymin=seq(0,7,0.5)

for(i in 1:length(graph.sites)){
  plot(Geomean~WY,TN.AGM,type="n",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,log="y")
  abline(h=ymaj,v=xmaj,col="grey",lty=3)
  with(subset(TN.AGM,Station.ID==graph.sites[i]),lines(WY,Geomean,lty=2,lwd=1))
  with(subset(TN.AGM,Station.ID==graph.sites[i]),points(WY,Geomean,pch=21,bg=adjustcolor(ifelse(TN.UseData=="Yes","indianred1","grey"),0.8),col=ifelse(TN.UseData=="Yes","indianred1","grey"),cex=1.25))
  axis_fun(1,line=-0.5,xmaj,xmin,xmaj,cex.axis=1)
  axis_fun(2,ymaj,ymin,ymaj,cex.axis=1)
  mtext(side=3,line=-1,paste(graph.sites[i]),cex=0.75)
}
mtext(side=2,line=1,expression(paste("Geometric Mean TN Concentration (mg L"^"-1",")",sep="")),cex=1,outer=T)
mtext(side=1,line=1,"Water Year",outer=T,cex=1.25)
leg.x=xlim.val[1]-19;leg.y=0.21
legend(leg.x,leg.y,legend=c("Seasonal Screening Passed","Seasonal Screening Failed"),pch=c(21,21),pt.bg=c("indianred1","grey"),col=c("black"),lty=0,lwd=0.5,pt.cex=1.75,ncol=2,cex=1.5,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()

#Derivation
#Sites A1 and A2 were removed from derivation due to suspected anthropogenic influence (see presentation).

der.sites=c("BCWQA17","BCWQA18","BCWQA12","BCWQA3","BCWQA13","BCWQA21","BCWQA14","BCWQA16","BCWQA15","BCWQA13A","BCWQA4","BCWQA5")#,"BCWQA1","BCWQA2","BCWQA9A","BCWQA9","BCWQA10","BCWQA20","BCWQA19")

##TP
AnnualAvg.TP=ddply(subset(TP.DO,Station.ID%in%der.sites&Use.Final==1),c("Station.ID","WY"),summarise,Mean=mean(Geomean,na.rm=T),SD=sd(Geomean,na.rm=T))
#write.csv(subset(TP.DO,Station.ID%in%der.sites&Use.Final==1),paste(export.path,"/BCNP_AGM_TP_screened_forderivation2.csv",sep=""),row.names=F)

diff(range(AnnualAvg.TP$WY))

#BCNP All
m=mean(AnnualAvg.TP$Mean,na.rm=T);m
s=sd(AnnualAvg.TP$Mean,na.rm=T);s
N.val=N.obs(unique(AnnualAvg.TP$WY));N.val
Df=N.val-1
Tp.95=abs(qt(0.05,Df));Tp.95
Tp.90=abs(qt(0.10,Df));Tp.90
LTL.95=m+s*(Tp.95/sqrt(N.val));LTL.95
LTL.90=m+s*(Tp.90/sqrt(N.val));LTL.90

gm=exp(mean(subset(TP.DO,Station.ID%in%der.sites&Use.Final==1)$Ln.Geomean,na.rm=T));gm
ybar=mean(subset(TP.DO,Station.ID%in%der.sites&Use.Final==1)$Ln.Geomean,na.rm=T);ybar
var=var(subset(TP.DO,Station.ID%in%der.sites&Use.Final==1)$Ln.Geomean,na.rm=T);var
NYR=N.obs(subset(TP.DO,Station.ID%in%der.sites&Use.Final==1)$Ln.Geomean);NYR
NS=N.obs(ddply(subset(TP.DO,Station.ID%in%der.sites&Use.Final==1),"Station.ID",summarise,N=N.obs(WY))$Station.ID);NS
Df=NYR-NS;Df
Tp.95=abs(qt(0.05,Df));Tp.95
AL.95=exp(ybar+Tp.95*sqrt(var+(var/NYR)));AL.95

##TN 
AnnualAvg.TN=ddply(subset(TN.DO,Station.ID%in%der.sites&Use.Final==1),c("Station.ID","WY"),summarise,Mean=mean(Geomean,na.rm=T),SD=sd(Geomean,na.rm=T))
#write.csv(subset(TN.DO,Station.ID%in%der.sites&Use.Final==1),paste(export.path,"/BCNP_AGM_TN_screened_forderivation2.csv",sep=""),row.names=F)
range(AnnualAvg.TN$Mean,na.rm=T)
range(AnnualAvg.TP$Mean,na.rm=T)

#BCNP All
m=mean(AnnualAvg.TN$Mean,na.rm=T);m
s=sd(AnnualAvg.TN$Mean,na.rm=T);s
N.val=N.obs(unique(AnnualAvg.TN$WY));N.val
Df=N.val-1
Tp.95=abs(qt(0.05,Df));Tp.95
LTL.95=m+s*(Tp.95/sqrt(N.val));LTL.95

gm=exp(mean(subset(TN.DO,Station.ID%in%der.sites&Use.Final==1)$Ln.Geomean,na.rm=T));gm
ybar=mean(subset(TN.DO,Station.ID%in%der.sites&Use.Final==1)$Ln.Geomean,na.rm=T);ybar
var=var(subset(TN.DO,Station.ID%in%der.sites&Use.Final==1)$Ln.Geomean,na.rm=T);var
NYR=N.obs(subset(TN.DO,Station.ID%in%der.sites&Use.Final==1)$Ln.Geomean);NYR
NS=N.obs(ddply(subset(TN.DO,Station.ID%in%der.sites&Use.Final==1),"Station.ID",summarise,N=N.obs(WY))$Station.ID);NS
Df=NYR-NS;Df
Tp.95=abs(qt(0.05,Df));Tp.95
AL.95=exp(ybar+Tp.95*sqrt(var+(var/NYR)));AL.95

derivation.TNTP=merge(ddply(subset(TN.DO,Station.ID%in%der.sites&Use.Final==1),c("Station.ID","WY"),summarise,AGM.TN=mean(Geomean,na.rm=T)),
                      ddply(subset(TP.DO,Station.ID%in%der.sites&Use.Final==1),c("Station.ID","WY"),summarise,AGM.TP=mean(Geomean,na.rm=T)),
                      by=c("Station.ID","WY"),all.y=T)
# write.csv(derivation.TNTP,paste(export.path,"/BCNP_AGM.csv",sep=""),row.names=F)