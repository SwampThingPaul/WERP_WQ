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

wmd.mon=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),wkt(utm17))
wq.mon=subset(wmd.mon,ACTIVITY_S=="Surface Water Grab")

sites=c("WWEIR","LC01.7TN","LC03.0TN","LC03.0TN01","WC01.11TN")
wq.mon_winggate=subset(wq.mon,STATION%in%sites)

AOI.wgs=raster::extent(spTransform(gBuffer(wq.mon_winggate,width=2000),wgs84))
AOI.wgs1=raster::extent(spTransform(gBuffer(other.canals,width=5000),wgs84))
AOI.wgs2=raster::extent(spTransform(gBuffer(wq.mon_winggate,width=1000),wgs84))

AOI.poly1=as(AOI.wgs1,"SpatialPolygons")
proj4string(AOI.poly1)=wgs84
AOI.poly2=as(AOI.wgs2,"SpatialPolygons")
proj4string(AOI.poly2)=wgs84
AOI.wgs=raster::extent(bind(AOI.poly1,AOI.poly2))

AOI.utm=as(extent(bind(AOI.poly1,AOI.poly2)),"SpatialPolygons")
proj4string(AOI.utm)=wgs84
AOI.utm=spTransform(AOI.utm,utm17)

library(ceramic)
# ceramic public token for API
public.token="pk.eyJ1IjoicGp1bGlhbiIsImEiOiJjanllbmJ0eXkxMzV0M2dzNXh5NGRlYXdqIn0.g4weKGOt1WdNZLg2hxBz1w"
Sys.setenv(MAPBOX_API_KEY=public.token)

roi=AOI.wgs
im <- cc_location(roi,zoom=13)
plotRGB(im)

im=projectRaster(im,crs=utm17)
im=setValues(im,scales::rescale(values(im), c(0,255)))

bbox.lims=bbox(gBuffer(WMSTA,width=2000))
plot(WMSTA,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=3,col=adjustcolor("dodgerblue1",0.5),border="dodgerblue1")
plotRGB(im,add=T)


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
plot(wq.mon_winggate,add=T,pch=21,bg="black")


# WQ Data -----------------------------------------------------------------
dates=date.fun(c("1999-05-01","2022-05-01"))

# WQ Data
parameters=data.frame(Test.Number=c(25,23,18,21,80,8,7),Param=c("TP","OP","NOx","TKN","TN","DO.mgL","Temp"))
wq.sites=c("WWEIR","PC17A","S190","L28I","USSO","S140")
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
cols=RColorBrewer::brewer.pal(length(sites.val),"Paired")
# png(filename=paste0(plot.path,"WERP_TP_Annual.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1.5,1,1.5),oma=c(1,1.5,0.25,0.5));

layout(matrix(c(1:3,3),2,2),widths=c(0.4,1))
par(mar=c(0.1,0.1,0.1,0.1))
bbox.lims=bbox(AOI.utm)
plot(other.canals,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=2)
plotRGB(im,add=T,xpd=NA)
plot(tribe,add=T,col=NA,border="white",lty=2)
plot(crop(canals,AOI.utm),add=T,col="dodgerblue1",lwd=2)
plot(other.canals,add=T,col="dodgerblue1",lwd=2)
plot(wq.mon_winggate,add=T,pch=21,bg="red",cex=1.25,lwd=0.01)
text(subset(wq.mon_winggate,STATION%in%sites.val[2:4]),"STATION",halo=T,pos=2,cex=0.7,font=2)
text(subset(wq.mon_winggate,STATION%in%sites.val[5]),"STATION",halo=T,pos=3,cex=0.7,font=2)
text(subset(wq.mon_winggate,STATION%in%sites.val[1]),"STATION",halo=T,pos=1,cex=0.7,font=2)
plot(WMSTA,add=T,col=adjustcolor("grey",0.5),density=40,border="grey")
plot(FW,add=T,col="grey",lty=3)
plot(AOI.utm,add=T,col=NA)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");


plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=sites.val,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "Monitoring Locations")

par(mar=c(2,2.5,1,0.5),xpd=F)
ylim.val=c(0,120);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2000,2022);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(Geomean~WY,TP.AGM,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
for(i in 1:length(sites.val)){
  with(subset(TP.AGM,Station.ID==sites.val[i]),pt_line(WY,GM.plot,2,cols[i],1,21,cols[i],cex=1.25))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Water Year")
mtext(side=2,line=2,"TP GM (\u03BCg L\u207B\u00B9)")
dev.off()

sites.val=c("WWEIR","LC01.7TN","LC03.0TN","LC03.0TN01","WC01.11TN")
cols=RColorBrewer::brewer.pal(length(sites.val),"Paired")
# viridis::cividis(length(sites.val))# wesanderson::wes_palette("Zissou1",length(sites.val),"continuous")

ylim.val=c(0,120);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2000,2022);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
# png(filename=paste0(plot.path,"WERP_TP_Annual.png"),width=6.5,height=3.75,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1.5,1,1.5),oma=c(1,1.5,0.25,0.5));
layout(matrix(c(1:2),1,2),widths=c(1,0.25))

plot(Geomean~WY,TP.AGM,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
for(i in 1:length(sites.val)){
  with(subset(TP.AGM,Station.ID==sites.val[i]),pt_line(WY,GM.plot,2,cols[i],1,21,cols[i],cex=1.25))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Water Year")
mtext(side=2,line=2,"TP GM (\u03BCg L\u207B\u00B9)")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=sites.val,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "Monitoring Locations")
dev.off()