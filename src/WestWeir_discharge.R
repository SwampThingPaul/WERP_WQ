## 
# Title:      WERP Model Evaluation
# Objective:  Evalute changes in discharges at 
#             WWEIR and Winggate Mill STA
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
# ogrListLayers(paste0(GIS.path.gen,"/AHED_release/AHED_20171102.gdb"))
# ogrListLayers(paste0(GIS.path.gen,"/SFER_GIS_Geodatabase.gdb"))

struct=spTransform(readOGR(paste0(GIS.path.gen,"/AHED_release/AHED_20171102.gdb"),"STRUCTURE"),utm17)
other.canals=spTransform(readOGR(paste0(GIS.path,"/WERP Canals"),"WINGLARD_WERP"),utm17)
canals=spTransform(readOGR(paste0(GIS.path.gen,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),utm17)
tribe=spTransform(readOGR(paste0(GIS.path.gen,"/SFER_GIS_Geodatabase.gdb"),"TribalAreas_all"),utm17)

ogrListLayers(paste0(GIS.path,"/20220418_WERP_FEATURES.kml"))
WMSTA=spTransform(readOGR(paste0(GIS.path,"/20220418_WERP_FEATURES.kml"),"WMSTA_footprint"),utm17)
FW=spTransform(readOGR(paste0(GIS.path,"/20220418_WERP_FEATURES.kml"),"Flow Way Embankments"),utm17)
Ranch_Culv=spTransform(readOGR(paste0(GIS.path,"/20220418_WERP_FEATURES.kml"),"Ranch Rd Culverts"),utm17)
WMSTA_flow=spTransform(readOGR(paste0(GIS.path,"/20220418_WERP_FEATURES.kml"),"WMSTA_flow"),utm17)
boundry_culv=spTransform(readOGR(paste0(GIS.path,"/20220418_WERP_FEATURES.kml"),"W Boundary Rd Culverts"),utm17)
wingate_plug=spTransform(readOGR(paste0(GIS.path,"/20220418_WERP_FEATURES.kml"),"WingateMill_Plug"),utm17)


bbox.lims=bbox(gBuffer(WMSTA,width=2000))
plot(WMSTA,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=3,col=adjustcolor("dodgerblue1",0.5),border="dodgerblue1")
plot(tribe,add=T,col="cornsilk")
plot(canals,add=T,col="dodgerblue1",lwd=2)
plot(other.canals,add=T,col="dodgerblue1",lwd=2)
plot(FW,add=T,col="purple",lwd=2)
plot(Ranch_Culv,pch=21,bg="indianred1",lwd=0.1,add=T)
plot(boundry_culv,pch=21,bg="indianred1",lwd=0.1,add=T)
plot(struct,pch=21,bg="indianred1",lwd=0.1,add=T)
plot(WMSTA_flow,pch=21,bg="dodgerblue1",lwd=0.1,add=T,cex=2)
plot(wingate_plug,pch=22,bg="grey",lwd=0.1,cex=1.25,add=T)
plot(wingate_plug,pch=21,bg="black",lwd=0.1,cex=0.75,add=T)


# -------------------------------------------------------------------------
list.files(paste0(data.path,"RSMBGL"))
alts=c('WECB',"WFWO","ALTHR")

cols.alts=c(grey.colors(2),wesanderson::wes_palette("Zissou1",1,"continuous"))

# Discharge ---------------------------------------------------------------
RSM.sites=c("WESTWEIR","WINGMILLCAN_WMSTA_IN","WMSTA_OUT_KBSN")
q.dat=data.frame()

## FWO
j=2
RSM.sites=c("WESTWEIR","WFEED_STA_LARD_IN","WFEED_STA_WIN_IN","WFEED_STA_OUT")
dss_out=opendss(paste0(data.path,"RSMGL/",alts[j],"/RSMGL_CEPP_output.dss"))  

for(i in 1:length(RSM.sites)){
  paths=paste0("/RSMGL_CEPP/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2005/1DAY/SIMULATED/")  
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$SITE=RSM.sites[i]
  tmp$Alt=alts[j]
  q.dat=rbind(tmp,q.dat)
  print(i)
}
## ECB
j=1
dss_out=opendss(paste0(data.path,"RSMGL/",alts[j],"/RSMGL_SD_output.dss"))  
paths=paste0("/RSMGL_SD/",RSM.sites[1],"/FLOW/01JAN1965 - 01JAN2005/1DAY/SIMULATED/")  
tmp=data.frame(getFullTSC(dss_out,paths))
tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
rownames(tmp)<-NULL
tmp$SITE=RSM.sites[1]
tmp$Alt=alts[j]
q.dat=rbind(tmp,q.dat)

## ALTHr
j=3
RSM.sites=c("WESTWEIR","WINGMILLCAN_WMSTA_IN","WMSTA_OUT_KBSN")
dss_out=opendss(paste0(data.path,"RSMGL/",alts[j],"/RSMGL_CEPP_output.dss"))  

for(i in 1:length(RSM.sites)){
  paths=paste0("/RSMGL_CEPP/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2005/1DAY/SIMULATED/")  
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$SITE=RSM.sites[i]
  tmp$Alt=alts[j]
  q.dat=rbind(tmp,q.dat)
  print(i)
}


# -------------------------------------------------------------------------

q.dat$CY=as.numeric(format(q.dat$Date,'%Y'))
q.dat$month=as.numeric(format(q.dat$Date,'%m'))
q.dat$Hydrosea_2=with(q.dat,ifelse(month%in%c(6:10),"wet",
                                   ifelse(month%in%c(11,12,1,2),"early_dry",
                                          ifelse(month%in%c(3:5),"late_dry",NA))))
ddply(q.dat,c("month","Hydrosea_2"),summarise,N.val=N.obs(FLOW))

q.dat$Hydrosea_yr=with(q.dat,ifelse(month>5,CY+1,CY))


# WWEIR change
reshape2::dcast(q.dat,Date+SITE~Alt,value.var = "FLOW",mean)
reshape2::dcast(q.dat,Date+Alt~SITE,value.var = "FLOW",mean)

CY.posQ=ddply(q.dat,c("CY","Alt","SITE"),summarise,TFlow.AcFt=sum(cfs.to.acftd(ifelse(FLOW<0,NA,FLOW)),na.rm=T))

CY.avg=ddply(subset(CY.posQ,SITE=="WESTWEIR"),"Alt",summarise,TFlow=mean(TFlow.AcFt))
CY.avg
CY.avg=CY.avg[match(alts,CY.avg$Alt),]

n.alts=length(alts)
ylim.val=c(0,50e3);by.y=10e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"AvgFloodControl.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(1,2,0.75,0.25),lwd=0.5);
# layout(matrix(c(1:2),1,2,byrow=T),heights=c(1,0.4))

x=barplot(CY.avg$TFlow,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(CY.avg$TFlow,beside=F,col=adjustcolor(cols.alts,0.75),ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts),add=T)
axis_fun(2,ymaj,ymin,ymaj/1e3)
axis_fun(1,x,x,alts);box(lwd=1)
text(x,CY.avg$TFlow,round(CY.avg$TFlow/1e3,2),pos=3,offset=0.05)
mtext(side=1,line=1.75,"Alternative")             
mtext(side=2,line=2,"Discharge (x1000 Ac-Ft Yr\u207B\u00B9)")
mtext(side=3,adj=0,"West Weir")
mtext(side=3,adj=1,"CY1965 - 2005")
dev.off()


CY.avg2=ddply(subset(CY.posQ,Alt==alts[3]),c("Alt","SITE"),summarise,TFlow=mean(TFlow.AcFt))
CY.avg2
