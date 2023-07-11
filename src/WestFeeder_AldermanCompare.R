## 
# Title:      WERP Model Evaluation
# Objective:  Evalute WQ data in west feeder
# Created by: Paul Julian; pjulian@sccf.org
# Created on: 08/08/2022
## 
## 

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(openxlsx)
library(plyr)
library(reshape2)
library(dssrip)
library(zoo)
library(classInt)
#
library(magrittr)
library(flextable)
library(ggplot2)

# GIS libraries 
# library(sp)
library(sp)
library(rgdal)
library(rgeos)
library(raster)



#Paths
wd="C:/Julian_LaCie/_GitHub/WERP_WQ"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/","/_documents/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen="C:/Julian_LaCie/_GISData"

# Helper variables
# epsg.io
nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")
wgs84=CRS("+proj=longlat +datum=WGS84")

## Functions
consec.startend=function(var){
  runs=rle(var)
  myruns = which(runs$values == TRUE)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  rslt=list(starts=starts,ends=ends)
  return(rslt)
}

# -------------------------------------------------------------------------

# WQ Data -----------------------------------------------------------------
dates=date.fun(c("1999-05-01","2022-05-01"))

# WQ Data
parameters=data.frame(Test.Number=c(25,23,18,21,80,8,7),Param=c("TP","OP","NOx","TKN","TN","DO.mgL","Temp"))
wq.sites=c("WWEIR","PC17A","S190","L28I","USSO","S140")
sites
wq.dat=DBHYDRO_WQ(dates[1],dates[2],sites,parameters$Test.Number)

wq.dat=merge(wq.dat,parameters,"Test.Number")
unique(wq.dat$Collection.Method)
wq.dat=subset(wq.dat,Collection.Method%in%c("G","GP"))

wq.dat.xtab=reshape2::dcast(wq.dat,DateTime.EST+Date.EST+Date+Collection.Method+Station.ID~Param,value.var="HalfMDL",mean)

wq.dat.xtab$WY=with(wq.dat.xtab,WY(DateTime.EST))
wq.dat.xtab$HydroSeason=with(wq.dat.xtab,FL.Hydroseason(Date.EST))
wq.dat.xtab$TP.ugL=with(wq.dat.xtab,TP*1000)
wq.dat.xtab$OPFlag=with(wq.dat.xtab,ifelse(is.na(OP)|OP==0,1,0));
wq.dat.xtab$TPFlag=with(wq.dat.xtab,ifelse(is.na(TP),1,0));
wq.dat.xtab$Reversal=with(wq.dat.xtab,ifelse(OPFlag==1,0,ifelse(OP>(TP*1.3),1,0)));

subset(wq.dat.xtab,TP<OP)
sum(wq.dat.xtab$Reversal,na.rm=T)
wq.dat.xtab$TN=NA
wq.dat.xtab$TN.Final=with(wq.dat.xtab,TN_Combine(NOx,TKN,TN))

# Data Screening 
wq.dat.screenTP=reshape2::dcast(wq.dat.xtab,Station.ID+WY~HydroSeason,value.var="TP.ugL",fun.aggregate=function(x)N.obs(x))
head(wq.dat.screenTP)
wq.dat.screenTP$NTotal=with(wq.dat.screenTP,A_Wet+B_Dry);
wq.dat.screenTP$SeasonScreen=with(wq.dat.screenTP,ifelse(B_Dry>=1&A_Wet>=1,1,0));
wq.dat.screenTP$NScreen=with(wq.dat.screenTP,ifelse(NTotal>=4,1,0));
wq.dat.screenTP$TP.UseData=with(wq.dat.screenTP,ifelse((SeasonScreen+NScreen)==2,"Yes","No"));
head(wq.dat.screenTP)

screened.dat=merge(wq.dat.xtab,wq.dat.screenTP[,c("Station.ID","WY","TP.UseData")],by=c("Station.ID","WY"),all.x=T)
TP.AGM=ddply(screened.dat,c("Station.ID","WY","TP.UseData"),summarise,N=length(TP.ugL),Geomean=exp(mean(log(TP.ugL),na.rm=T)),N=N.obs(TP.ugL),sd=sd(TP.ugL,na.rm=T),GM.SE=Geomean*(sd(log(TP.ugL),na.rm=T)/sqrt(N-1)));
TP.AGM$GM.plot=with(TP.AGM,ifelse(TP.UseData=="Yes",Geomean,NA))

with(subset(TP.AGM,Station.ID=="WWEIR"&WY>=2008),cor.test(Geomean,WY,method="kendall"))
with(subset(TP.AGM,Station.ID=="WWEIR"&WY>=2010),cor.test(Geomean,WY,method="kendall"))

sites.val=c("WWEIR","LC01.7TN","LC03.0TN","LC03.0TN01","WC01.11TN")
wq.dat.xtab$Station.ID=factor(wq.dat.xtab$Station.ID,levels=sites.val)
ddply(wq.dat.xtab,c("Station.ID"),summarise,
      min.val=min(TP.ugL,na.rm=T),
      max.val=max(TP.ugL,na.rm=T))

alderman.dat=data.frame(TP=c(34,51,86,43,39,29,35,30))
# png(filename=paste0(plot.path,"WERP_TP_boxplot.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3,1,1),oma=c(4,1.5,0.25,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

ylim.val=c(0,350);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TP.ugL~Station.ID,wq.dat.xtab,outline=F,ylim=ylim.val,axes=F,ann=F)
axis_fun(1,1:5,1:5,sites.val,cex=0.75,las=3)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,"Monitoring Station",outer=T,line=2)
mtext(side=2,line=3,"TP (\u03BCg L\u207B\u00B9)")
mtext(side=3,adj=0,"May 1999 - May 2022 (Grab Samples)", font=3)

par(mar=c(2,0,1,0.5))
boxplot(alderman.dat,outline=F,ylim=ylim.val,axes=F,ann=F,col=adjustcolor("indianred1",0.5))
axis_fun(1,1,1,"Alderman Ranch",line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=3,adj=0,"Collected 2023 Jan 09", font=3)
dev.off()