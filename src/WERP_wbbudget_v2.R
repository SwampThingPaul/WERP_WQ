## WERP Natural Flow alternative evaluation
##
## Code was compiled by Paul Julian
## contact info: pjulian@evergladesfoundation.org

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Preps DSSRip
options(dss_override_location="C:\\projects\\dssrip\\monolith")
options(dss_config_filename="C:\\projects\\dssrip\\dssrip2.config")
options(dss_default_config="monolith-win-x86_64")
options(dss_allowed_states="untested") 
options(dssrip_debug=T)

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(openxlsx)
library(plyr)
library(reshape2)
library(dssrip)

# GIS Libraries
library(rgdal)
library(rgeos)
library(raster)
library(gstat)
library(tmap)

library(sf)
library(sfheaders)

# netcdf 
library(chron) # package for creating chronological objects
library(ncdf4)  # package to handle NetCDF

## Paths
wd="C:/Julian_LaCie/_GitHub/WERP_WQ"

paths=paste0(wd,c("/Plots/","/Exports/","/Data/","/src/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
gis.path=paths[5]
GIS.path.gen="C:/Julian_LaCie/_GISData"
# Helper variables
nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")
wgs84=CRS("+init=epsg:4326")
NAD83HARN=CRS("+init=epsg:2881")

tmap_mode("view")


# GIS ---------------------------------------------------------------------
gen.GIS="C:/Julian_LaCie/_GISData"
db.path=paste(gen.GIS,"/SFER_GIS_Geodatabase.gdb",sep=""); 

ogrListLayers(paste0(gen.GIS,"/AHED_release/AHED_20171102.gdb"))
ogrListLayers(paste0(gen.GIS,"/AHED_release/20230405/AHED.gdb"))
# 
canals=readOGR(paste0(gen.GIS,"/AHED_release/20230405/shp"),"canals")
canals=spTransform(canals,NAD83HARN)
unique(canals$FLOWLINETY)
canals2=subset(canals,FLOWLINETY%in%c("CANALDITCH","STREAMRIVER"))

canals=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),NAD83HARN)
# plot(subset(canals,FLOWLINETY%in%c("CANALDITCH","STREAMRIVER")))
# wmd.struct=spTransform(readOGR(paste0(gen.GIS,"/AHED_release/20230405/AHED.gdb"),"STRUCTURE"),utm17)
# canals3=readOGR(paste0(gen.GIS,"/AHED_release/canals"),"Canals")

wg.canal.shp=spTransform(readOGR(paste0(gis.path,"/WERP Canals"),"WinggateCanal"),NAD83HARN)

ogrListLayers(db.path)
trib.all=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"TribalAreas_all"),NAD83HARN)
plot(trib.all)

## AltHNF
ogrListLayers(paste0(gis.path,"/AltHNF_backfills.kml"))
LCWG_backfill=spTransform(readOGR(paste0(gis.path,"/AltHNF_dig"),"ALTHNF_backfill"),NAD83HARN)
LCWG_backfill_ln=spTransform(readOGR(paste0(gis.path,"/AltHNF_backfills.kml")),NAD83HARN)



# -------------------------------------------------------------------------
alts=c("WFWOR","WECBRR","ALTHR","ALTHNF")
n.alts=length(alts)

cols.alts=c("grey50","grey10",wesanderson::wes_palette("Zissou1",n.alts-2,"continuous"))

# RSMGL WERP Mesh -------------------------------------------------------
mesd2dm=read.table(paste0(data.path,"20230331/mesh_werp_V5.2dm"),header = F,skip=1,fill=T)
colnames(mesd2dm)=c("MESH2D","CellId",paste0("Node",1:3),"val")

werp_mesh=readOGR(paste0(export.path,"GIS"),"WERPMesh")

werp_mesh_node=readOGR(paste0(export.path,"GIS"),"WERPMeshNodes")
werp_mesh_node$nodeid=as.numeric(werp_mesh_node$nodeid)

werp_transects=readOGR(paste0(export.path,"GIS"),"WERP_TransLines")

tm_shape(werp_transects)+tm_lines(col="transect",lwd=2)+
  tm_shape(werp_mesh_node)+tm_dots()

w1.west=subset(werp_mesh_node,nodeid%in%c(4159,4229,4242))
w1.west=w1.west@data
w1.west=merge(w1.west,data.frame(nodeid=c(4159,4229,4242),transect="W1 west",plot.ord=1:3),"nodeid")

w1.west=w1.west[order(w1.west$transect,w1.west$plot.ord),]
w1.west=subset(w1.west,is.na(X1)==F)
w1.west$dX1=with(w1.west,ave(X1,transect,FUN=function(x)c(0,diff(x))))
w1.west$dX2=with(w1.west,ave(X2,transect,FUN=function(x)c(0,diff(x))))
w1.west$dist.ft=with(w1.west,sqrt((dX2^2)+(dX1^2)));#calculates distance between points
w1.west=SpatialPointsDataFrame(coords=w1.west[,c("X1","X2")],data=w1.west,proj4string = NAD83HARN)
path=sp::split(w1.west,w1.west$transect)
trans.id=ddply(w1.west@data,c("transect"),summarise,N.val=N.obs(transect),max.dist=max(dist.ft,na.rm=T),tot.dist=sum(dist.ft,na.rm=T))
sp_lines=SpatialLinesDataFrame(SpatialLines(list(Lines(list(Line(path[[1]])),unique(path[[1]]@data$transect))),NAD83HARN),data.frame(row.names=trans.id$transect,transect=trans.id$transect))
w1.west=sp_lines
plot(w1.west)


w1.east=subset(werp_mesh_node,nodeid%in%c(4242,4241,4240))
w1.east=w1.east@data
w1.east=merge(w1.east,data.frame(nodeid=c(4242,4241,4240),transect="W1 east",plot.ord=1:3),"nodeid")

w1.east=w1.east[order(w1.east$transect,w1.east$plot.ord),]
w1.east=subset(w1.east,is.na(X1)==F)
w1.east$dX1=with(w1.east,ave(X1,transect,FUN=function(x)c(0,diff(x))))
w1.east$dX2=with(w1.east,ave(X2,transect,FUN=function(x)c(0,diff(x))))
w1.east$dist.ft=with(w1.east,sqrt((dX2^2)+(dX1^2)));#calculates distance between points
w1.east=SpatialPointsDataFrame(coords=w1.east[,c("X1","X2")],data=w1.east,proj4string = NAD83HARN)
path=sp::split(w1.east,w1.east$transect)
trans.id=ddply(w1.east@data,c("transect"),summarise,N.val=N.obs(transect),max.dist=max(dist.ft,na.rm=T),tot.dist=sum(dist.ft,na.rm=T))
sp_lines=SpatialLinesDataFrame(SpatialLines(list(Lines(list(Line(path[[1]])),unique(path[[1]]@data$transect))),NAD83HARN),data.frame(row.names=trans.id$transect,transect=trans.id$transect))
w1.east=sp_lines
plot(w1.east)


KB_slough=subset(werp_mesh_node,nodeid%in%c(4231,4361,4223,4222))
KB_slough=KB_slough@data
KB_slough=merge(KB_slough,data.frame(nodeid=c(4231,4361,4223,4222),transect="KissimmeeBilly",plot.ord=1:4),"nodeid")
KB_slough=KB_slough[order(KB_slough$transect,KB_slough$plot.ord),]
KB_slough=subset(KB_slough,is.na(X1)==F)
KB_slough$dX1=with(KB_slough,ave(X1,transect,FUN=function(x)c(0,diff(x))))
KB_slough$dX2=with(KB_slough,ave(X2,transect,FUN=function(x)c(0,diff(x))))
KB_slough$dist.ft=with(KB_slough,sqrt((dX2^2)+(dX1^2)));#calculates distance between points
KB_slough=SpatialPointsDataFrame(coords=KB_slough[,c("X1","X2")],data=KB_slough,proj4string = NAD83HARN)
path=sp::split(KB_slough,KB_slough$transect)
trans.id=ddply(KB_slough@data,c("transect"),summarise,N.val=N.obs(transect),max.dist=max(dist.ft,na.rm=T),tot.dist=sum(dist.ft,na.rm=T))
sp_lines=SpatialLinesDataFrame(SpatialLines(list(Lines(list(Line(path[[1]])),unique(path[[1]]@data$transect))),NAD83HARN),data.frame(row.names=trans.id$transect,transect=trans.id$transect))
KB_slough=sp_lines
plot(KB_slough)

WGSouth=subset(werp_mesh_node,nodeid%in%c(4244,4228,4227))
WGSouth=WGSouth@data
WGSouth=merge(WGSouth,data.frame(nodeid=c(4244,4228,4227),transect="WinggateSouth",plot.ord=1:3),"nodeid")
WGSouth=WGSouth[order(WGSouth$transect,WGSouth$plot.ord),]
WGSouth=subset(WGSouth,is.na(X1)==F)
WGSouth$dX1=with(WGSouth,ave(X1,transect,FUN=function(x)c(0,diff(x))))
WGSouth$dX2=with(WGSouth,ave(X2,transect,FUN=function(x)c(0,diff(x))))
WGSouth$dist.ft=with(WGSouth,sqrt((dX2^2)+(dX1^2)));#calculates distance between points
WGSouth=SpatialPointsDataFrame(coords=WGSouth[,c("X1","X2")],data=WGSouth,proj4string = NAD83HARN)
path=sp::split(WGSouth,WGSouth$transect)
trans.id=ddply(WGSouth@data,c("transect"),summarise,N.val=N.obs(transect),max.dist=max(dist.ft,na.rm=T),tot.dist=sum(dist.ft,na.rm=T))
sp_lines=SpatialLinesDataFrame(SpatialLines(list(Lines(list(Line(path[[1]])),unique(path[[1]]@data$transect))),NAD83HARN),data.frame(row.names=trans.id$transect,transect=trans.id$transect))
WGSouth=sp_lines
plot(WGSouth)
# WBBudget ----------------------------------------------------------
WFWOR.dat.nc<-nc_open(paste0(data.path,"20230331/",alts[1],"/wbbudget.nc"))
WECBRR.dat.nc<-nc_open(paste0(data.path,"20230331/",alts[2],"/wbbudget.nc"))
ALTHR.dat.nc<-nc_open(paste0(data.path,"20230331/",alts[3],"/wbbudget.nc"))
ALTHNF.dat.nc<-nc_open(paste0(data.path,"20230331/",alts[4],"/wbbudget.nc"))

print(WFWOR.dat.nc)
attributes(WFWOR.dat.nc$var)$names

ts_stamp=ncvar_get(WFWOR.dat.nc,"timestamps")
ts_stamp=ts_stamp+1
date.vals=as.Date("1964-12-31")+lubridate::duration(ts_stamp,"days")
range(date.vals)
dates.df=data.frame(ts_stamp=ts_stamp,date=date.vals)
dates.df$CY=as.numeric(format(dates.df$date,'%Y'))
dates.df$month=as.numeric(format(dates.df$date,'%m'))


# transect flows ----------------------------------------------------------

WM.df=function(x){
  waterMoverID=ncvar_get(x,"waterMoverID");#contains fmwatermover
  waterMoverMap=ncvar_get(x,"waterMoverMap")
  waterMoverMap=data.frame(t(waterMoverMap))
  waterMoverType=ncvar_get(x,"waterMoverType")
  waterMoverMap2=cbind(waterMoverMap,data.frame(waterMoverType=trimws(waterMoverType)))
  waterMoverMap2$rownum.val=as.numeric(rownames(waterMoverMap2))
  return(waterMoverMap2)
}

waterMoverMap.WFWOR=WM.df(WFWOR.dat.nc)
waterMoverMap.WECBRR=WM.df(WECBRR.dat.nc)
waterMoverMap.AltHR=WM.df(ALTHR.dat.nc)
waterMoverMap.AltHNF=WM.df(ALTHNF.dat.nc)

## W1 Tranect --------------------------------------------------------------
x1vals=c(8162,8163,8160,8062)
x2vals=c(8167,8207,8161,8064)
xvals=data.frame(X1=x1vals,X2=x2vals)

bbox.lims=bbox(gBuffer(subset(werp_transects,transect=="W1_ol"),width=5000))
plot(werp_mesh,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(subset(werp_mesh,as.numeric(CellId)%in%x1vals),add=T,col="grey")
plot(subset(werp_mesh,as.numeric(CellId)%in%x2vals),add=T,col="yellow")
text(werp_mesh,"CellId")
plot(subset(werp_transects,transect=="W1_ol"),add=T,col="red",lwd=2)


idval=data.frame()
tmp=merge(subset(waterMoverMap.WFWOR,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[1]
idval=rbind(idval,tmp)

tmp=merge(subset(waterMoverMap.WECBRR,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[2]
idval=rbind(idval,tmp)

tmp=merge(subset(waterMoverMap.AltHR,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[3]
idval=rbind(idval,tmp)

tmp=merge(subset(waterMoverMap.AltHNF,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[4]
idval=rbind(idval,tmp)


W1_parsed.Q=data.frame()
# i=1
for(i in 1:length(alts)){
  dat.nc<-nc_open(paste0(data.path,"20230331/",alts[i],"/wbbudget.nc"))
  FMWatermovers=ncvar_get(dat.nc,"FMWatermovers")
  
  tmp=t(FMWatermovers[subset(idval,Alt==alts[i])$rownum.val,])
  tmp=data.frame(tmp)
  colnames(tmp)=paste0("C_",paste(subset(idval,Alt==alts[i])$X1,subset(idval,Alt==alts[i])$X2,sep="_"))
  
  tmp.dat=cbind(data.frame(Date=dates.df$date,Alt=alts[i]),tmp)
  
  W1_parsed.Q=rbind(W1_parsed.Q,tmp.dat)
  print(i)
}
head(W1_parsed.Q)

W1_parsed.Q$C_8162_8167=W1_parsed.Q$C_8162_8167*-1
W1_parsed.Q$C_8062_8064=W1_parsed.Q$C_8062_8064*-1

names(W1_parsed.Q)[grepl("C_",names(W1_parsed.Q))]

W1_parsed.Q$Alt=factor(W1_parsed.Q$Alt,levels=alts)
W1_parsed.Q$CY=as.numeric(format(W1_parsed.Q$Date,'%Y'))
W1_parsed.Q$OLFLOW=rowSums(W1_parsed.Q[,names(W1_parsed.Q)[grepl("C_",names(W1_parsed.Q))]],na.rm=T)
W1_parsed.Q$OLFLOW.kacft=(W1_parsed.Q$OLFLOW*2.29569e-5)/1000 ;# no time conversion kacft/d  

W1_parsed.Q$west.OLFLOW=(rowSums(W1_parsed.Q[,c('C_8162_8167',"C_8163_8207")],na.rm=T)*2.29569e-5)/1000
W1_parsed.Q$east.OLFLOW=(rowSums(W1_parsed.Q[,c('C_8160_8161',"C_8062_8064")],na.rm=T)*2.29569e-5)/1000

head(W1_parsed.Q)
W1_parsed.Q$month=as.numeric(format(W1_parsed.Q$Date,"%m"))
W1_parsed.Q$hydro.season=with(W1_parsed.Q,ifelse(month%in%seq(6,10,1),"A_Wet","B_Dry"));# FL.Hydroseason(trans_flow.ol$Date)

W1_parsed.flow.seasonal=ddply(W1_parsed.Q,c("Alt","CY","hydro.season"),summarise,west.TFlow.kacft=sum(west.OLFLOW,na.rm=T),east.TFlow.kacft=sum(east.OLFLOW,na.rm=T))
W1_parsed.flow.seasonal.west=dcast(W1_parsed.flow.seasonal,Alt~hydro.season,value.var="west.TFlow.kacft",mean)
W1_parsed.flow.seasonal.east=dcast(W1_parsed.flow.seasonal,Alt~hydro.season,value.var="east.TFlow.kacft",mean)

W1_parsed.flow.month=ddply(W1_parsed.Q,c("Alt","CY","month"),summarise,west.TFlow.kacft=sum(west.OLFLOW,na.rm=T),east.TFlow.kacft=sum(east.OLFLOW,na.rm=T))
W1_parsed.flow.month=ddply(W1_parsed.flow.month,c("Alt","month"),summarise,west.mean.val=mean(west.TFlow.kacft,na.rm=T),east.mean.val=mean(east.TFlow.kacft,na.rm=T))

cols=c("lightblue","khaki3")
# png(filename=paste0(plot.path,"2023_AltNHF/W1_parsed_Seasonal_monthly.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3.75,0.5,1),oma=c(2,1,1,0.5),lwd=0.5);
layout(matrix(1:6,3,2,byrow=F))

ylim.val=c(0,50);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=t(W1_parsed.flow.seasonal.west[,2:3])
x=barplot(tmp,col=cols,border="grey",
          space=0.05,
          ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
mtext(side=3,adj=0,"West W1 Overland Flow")
abline(h=0,lwd=1)
text(x,colSums(tmp),format(round(colSums(tmp),1)),offset=0.1,pos=ifelse(colSums(tmp)<0,1,3))
text(x,tmp[1,]+tmp[2,]/2,format(round(tmp[2,],1)),font=3,cex=0.75)
text(x,tmp[1,]/2,format(round(tmp[1,],1)),font=3,cex=0.75)
axis_fun(1,x,x,alts,line=-0.5,cex=0.7)
axis_fun(2,ymaj,ymin,format(ymaj));
box(lwd=1)
legend("topleft",legend=c("Wet (June - Oct)","Dry (Nov - May)"),
       lty=c(0),lwd=c(0.1),col=c(cols),pch=22,pt.bg=cols,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=2,line=2.5,"Average Net\nDischarge (x10\u2074 AcFt Y\u207B\u00B9)",cex=0.75)
mtext(side=1,line=1.5,"Alternatives")

xlim.val=c(1,12);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,10);by.y=2;ymaj=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y);ymin=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y/2)
plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
with(subset(W1_parsed.flow.month,Alt==alts[1]),lines(month,west.mean.val,col=cols.alts[1],lty=1,lwd=1.5))
with(subset(W1_parsed.flow.month,Alt==alts[2]),lines(month,west.mean.val,col=cols.alts[2],lty=2,lwd=1.5))
for(i in 3:n.alts){
  with(subset(W1_parsed.flow.month,Alt==alts[i]),lines(month,west.mean.val,col=cols.alts[i],lty=1,lwd=1.5))
}
axis_fun(1,xmaj,xmin,month.abb[xmaj],line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("topleft",legend=c(alts),
       lty=c(1,2,1,1),lwd=c(1.5),col=c(cols.alts),
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=1,line=1.5,"Month")
mtext(side=2,line=2.5,"Average Net\nDischarge (x10\u2074 AcFt M\u207B\u00B9)",cex=0.75)

par(mar=c(0,0.5,0.5,1))
bbox.lims=bbox(gBuffer(subset(werp_transects,transect=="W1_ol"),width=10000))
plot(trib.all,col="gray80",border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(werp_mesh,border=T,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(LCWG_backfill_ln,add=T,col="blue",lwd=2)
plot(subset(werp_transects,transect=="W1_ol"),add=T,col="yellow",lwd=2)
plot(w1.west,add=T,col="red",lty=2)
box(lwd=1)
mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);

par(mar=c(2,3.75,0.5,1))
ylim.val=c(0,50);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=t(W1_parsed.flow.seasonal.east[,2:3])
x=barplot(tmp,col=cols,border="grey",
          space=0.05,
          ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
mtext(side=3,adj=0,"East W1 Overland Flow")
abline(h=0,lwd=1)
text(x,colSums(tmp),format(round(colSums(tmp),1)),offset=0.1,pos=ifelse(colSums(tmp)<0,1,3))
text(x,tmp[1,]+tmp[2,]/2,format(round(tmp[2,],1)),font=3,cex=0.75)
text(x,tmp[1,]/2,format(round(tmp[1,],1)),font=3,cex=0.75)
axis_fun(1,x,x,alts,line=-0.5,cex=0.7)
axis_fun(2,ymaj,ymin,format(ymaj));
box(lwd=1)
mtext(side=1,line=1.5,"Alternatives")

xlim.val=c(1,12);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,10);by.y=2;ymaj=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y);ymin=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y/2)
plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
with(subset(W1_parsed.flow.month,Alt==alts[1]),lines(month,east.mean.val,col=cols.alts[1],lty=1,lwd=1.5))
with(subset(W1_parsed.flow.month,Alt==alts[2]),lines(month,east.mean.val,col=cols.alts[2],lty=2,lwd=1.5))
for(i in 3:n.alts){
  with(subset(W1_parsed.flow.month,Alt==alts[i]),lines(month,east.mean.val,col=cols.alts[i],lty=1,lwd=1.5))
}
axis_fun(1,xmaj,xmin,month.abb[xmaj],line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Month")

par(mar=c(0,0.5,0.5,1))
bbox.lims=bbox(gBuffer(subset(werp_transects,transect=="W1_ol"),width=10000))
plot(trib.all,col="gray80",border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(werp_mesh,border=T,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(LCWG_backfill_ln,add=T,col="blue",lwd=2)
plot(subset(werp_transects,transect=="W1_ol"),add=T,col="yellow",lwd=2)
plot(w1.east,add=T,col="red",lty=2)
box(lwd=1)
mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);
dev.off()

## RSM Transects -----------------------------------------------------------
dss.cat.fun=function(x,open.dss.fun=T){
  dss_out=if(open.dss.fun==T){opendss(x)}else{x}
  rslt=data.frame(path=getCatalogedPathnames(dss_out))
  str.val=strsplit(rslt$path,"/")
  rslt=data.frame(SITE=sapply(str.val,"[",3),TYPE=sapply(str.val,"[",4),
                  DateVal=sapply(str.val,"[",5))
  rslt=ddply(rslt,c("SITE","TYPE"),summarise,N.val=N.obs(SITE))
  return(rslt)
}

# dss_out=opendss(paste0(data.path,"20230331/",alts[4],"/transect_flows.dss"))
# dss_cat=dss.cat.fun(dss_out,F)
# 
# dss_cat[grepl("W1_",dss_cat$SITE),]
# dss_cat[grepl("W2_",dss_cat$SITE),]

RSM.sites=c("W1_TRANSECT","W2_TRANSECT","T10_TRANSECT")

trans_flow.ol=data.frame()
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"20230331/",alts[j],"/transect_flows.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMGL_SD/",RSM.sites[i],"/OLFLOW//1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(rownames(tmp))
    rownames(tmp)<-NULL
    tmp$transect=RSM.sites[i]
    tmp$Alt=alts[j]
    trans_flow.ol=rbind(tmp,trans_flow.ol)
    print(i)
    
  }
}
trans_flow.ol$Alt=factor(trans_flow.ol$Alt,levels=alts)
trans_flow.ol$month=as.numeric(format(trans_flow.ol$Date,"%m"))
trans_flow.ol$hydro.season=with(trans_flow.ol,ifelse(month%in%seq(6,10,1),"A_Wet","B_Dry"));# FL.Hydroseason(trans_flow.ol$Date)
trans_flow.ol$CY=as.numeric(format(trans_flow.ol$Date,"%Y"))
trans_flow.ol$OLFLOW.kacft=(trans_flow.ol$OLFLOW*2.29569e-5)/1000 ;# no time conversion

trans.flow.ann=ddply(trans_flow.ol,c("Alt","CY","transect"),summarise,TFlow.kacft=sum(OLFLOW.kacft,na.rm=T))
trans.flow.ann=dcast(trans.flow.ann,Alt~transect,value.var="TFlow.kacft",mean)

trans.flow.seasonal=ddply(trans_flow.ol,c("Alt","CY","hydro.season","transect"),summarise,TFlow.kacft=sum(OLFLOW.kacft,na.rm=T))
trans.flow.seasonal=dcast(trans.flow.seasonal,Alt+transect~hydro.season,value.var="TFlow.kacft",mean)

trans.flow.month=ddply(trans_flow.ol,c("Alt","CY","month","transect"),summarise,TFlow.kAcft=sum(OLFLOW.kacft,na.rm=T))
trans.flow.month=ddply(trans.flow.month,c("Alt","month","transect"),summarise,mean.val=mean(TFlow.kAcft,na.rm=T))


head(subset(trans_flow.ol,Alt==alts[1]&transect=="W1_TRANSECT"))
head(subset(W1_parsed.Q,Alt==alts[1]))


head(subset(trans_flow.ol,Alt==alts[2]&transect=="W1_TRANSECT"))
head(subset(W1_parsed.Q,Alt==alts[2]))

# LC and WG degrade Discharge ---------------------------------------------
## South Winggate Transect -------------------------------------------------

x2vals=c(8175,206)
x1vals=c(8151,205)
xvals=data.frame(X1=x1vals,X2=x2vals)

bbox.lims=bbox(gBuffer(KB_slough,width=20000))
plot(werp_mesh,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(subset(werp_mesh,as.numeric(CellId)%in%x1vals),add=T,col="grey")
plot(subset(werp_mesh,as.numeric(CellId)%in%x2vals),add=T,col="yellow")
plot(canals2,add=T,col="blue",lwd=2);plot(LCWG_backfill_ln,add=T,col="blue",lwd=2)
plot(WGSouth,add=T,col="red",lwd=2)
plot(KB_slough,add=T,col="red",lwd=2)
text(werp_mesh,"CellId",cex=0.75)

idval=data.frame()
tmp=merge(subset(waterMoverMap.WFWOR,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[1]
idval=rbind(idval,tmp)

tmp=merge(subset(waterMoverMap.WECBRR,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[2]
idval=rbind(idval,tmp)

tmp=merge(subset(waterMoverMap.AltHR,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[3]
idval=rbind(idval,tmp)

tmp=merge(subset(waterMoverMap.AltHNF,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[4]
idval=rbind(idval,tmp)

WGSouth.Q=data.frame()
for(i in 1:length(alts)){
  dat.nc<-nc_open(paste0(data.path,"20230331/",alts[i],"/wbbudget.nc"))
  FMWatermovers=ncvar_get(dat.nc,"FMWatermovers")
  
  tmp=t(FMWatermovers[subset(idval,Alt==alts[i])$rownum.val,])
  tmp=data.frame(tmp)
  colnames(tmp)=paste0("C_",paste(subset(idval,Alt==alts[i])$X1,subset(idval,Alt==alts[i])$X2,sep="_"))
  
  tmp.dat=cbind(data.frame(Date=dates.df$date,Alt=alts[i]),tmp)
  
  WGSouth.Q=rbind(WGSouth.Q,tmp.dat)
  print(i)
}
head(WGSouth.Q)

WGSouth.Q$C_205_206=WGSouth.Q$C_205_206*-1

names(WGSouth.Q)[grepl("C_",names(WGSouth.Q))]

WGSouth.Q$Alt=factor(WGSouth.Q$Alt,levels=alts)
WGSouth.Q$CY=as.numeric(format(WGSouth.Q$Date,'%Y'))
WGSouth.Q$OLFLOW=rowSums(WGSouth.Q[,names(WGSouth.Q)[grepl("C_",names(WGSouth.Q))]],na.rm=T)
WGSouth.Q$OLFLOW.kacft=(WGSouth.Q$OLFLOW*2.29569e-5)/1000 ;# no time conversion kacft/d  

head(WGSouth.Q)
WGSouth.Q$month=as.numeric(format(WGSouth.Q$Date,"%m"))
WGSouth.Q$hydro.season=with(WGSouth.Q,ifelse(month%in%seq(6,10,1),"A_Wet","B_Dry"));# FL.Hydroseason(trans_flow.ol$Date)

WGSouth.flow.seasonal=ddply(WGSouth.Q,c("Alt","CY","hydro.season"),summarise,TFlow.kacft=sum(OLFLOW.kacft,na.rm=T))
WGSouth.flow.seasonal=dcast(WGSouth.flow.seasonal,Alt~hydro.season,value.var="TFlow.kacft",mean)

WGSouth.flow.month=ddply(WGSouth.Q,c("Alt","CY","month"),summarise,TFlow.kacft=sum(OLFLOW.kacft,na.rm=T))
WGSouth.flow.month=ddply(WGSouth.flow.month,c("Alt","month"),summarise,mean.val=mean(TFlow.kacft,na.rm=T))


cols=c("lightblue","khaki3")
# png(filename=paste0(plot.path,"2023_AltNHF/WinggateSouth_transect.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3.75,0.5,1),oma=c(2,1,1,0.5),lwd=0.5);
layout(matrix(1:4,2,2,byrow=T))

ylim.val=c(0,60);by.y=20;ymaj=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y);ymin=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y/2)
tmp=t(WGSouth.flow.seasonal[,2:3])
x=barplot(tmp,col=cols,border="grey",
          space=0.05,
          ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
mtext(side=3,adj=0,"Winggate Canal South Overland Flow (North to South)")
abline(h=0,lwd=1)
text(x,colSums(tmp),format(round(colSums(tmp),1)),offset=0.1,pos=ifelse(colSums(tmp)<0,1,3))
text(x,tmp[1,]+tmp[2,]/2,format(round(tmp[2,],1)),font=3,cex=0.7)
text(x,tmp[1,]/2,format(round(tmp[1,],1)),font=3,cex=0.7)
axis_fun(1,x,x,alts,line=-0.5,cex=0.7)
axis_fun(2,ymaj,ymin,format(ymaj));
box(lwd=1)
legend("topleft",legend=c("Wet (June - Oct)","Dry (Nov - May)"),
       lty=c(0),lwd=c(0.1),col=c(cols),pch=22,pt.bg=cols,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=2,line=2.5,"Average Net\nDischarge (x10\u2074 AcFt Y\u207B\u00B9)",cex=0.75)
mtext(side=1,line=1.5,"Alternatives")

xlim.val=c(1,12);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,12);by.y=2;ymaj=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y);ymin=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y/2)
plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
with(subset(WGSouth.flow.month,Alt==alts[1]),lines(month,mean.val,col=cols.alts[1],lty=1,lwd=1.5))
with(subset(WGSouth.flow.month,Alt==alts[2]),lines(month,mean.val,col=cols.alts[2],lty=2,lwd=1.5))
for(i in 3:n.alts){
  with(subset(WGSouth.flow.month,Alt==alts[i]),lines(month,mean.val,col=cols.alts[i],lty=1,lwd=1.5))
}
axis_fun(1,xmaj,xmin,month.abb[xmaj],line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("topleft",legend=c(alts),
       lty=c(1,2,1,1),lwd=c(1.5),col=c(cols.alts),
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=1,line=1.5,"Month")
mtext(side=2,line=2.5,"Average Net\nDischarge (x10\u2074 AcFt M\u207B\u00B9)",cex=0.75)

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")  
with(ecdf_fun(subset(WGSouth.Q,Alt==alts[1])$OLFLOW.kacft),lines(1-proportion,value,col=cols.alts[1],lty=1,lwd=1.5))
with(ecdf_fun(subset(WGSouth.Q,Alt==alts[2])$OLFLOW.kacft),lines(1-proportion,value,col=cols.alts[2],lty=2,lwd=1.5))
for(i in 3:n.alts){
  with(ecdf_fun(subset(WGSouth.Q,Alt==alts[i])$OLFLOW.kacft),lines(1-proportion,value,col=adjustcolor(cols.alts[i],0.5),lwd=2))
}
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=1.5,outer=F,"Proportion of Time \u2265 Discharge")  
mtext(side=2,line=2.5,"Discharge Volume (x10\u2074 AcFt D\u207B\u00B9)",cex=0.75)

par(mar=c(0,0.5,0.5,1))
bbox.lims=bbox(gBuffer(KBSlough,width=20000))
plot(trib.all,col="gray80",border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(werp_mesh,border=T,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(LCWG_backfill_ln,add=T,col="blue",lwd=2)
plot(WGSouth,add=T,col="red",lwd=2)
box(lwd=1)
mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);
dev.off()

## Kissimmee Billy Trans ---------------------------------------------------

x1vals=c(952,8065,8060)
x2vals=c(8450,8449,8097)
xvals=data.frame(X1=x1vals,X2=x2vals)

bbox.lims=bbox(gBuffer(KB_slough,width=20000))
plot(werp_mesh,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(subset(werp_mesh,as.numeric(CellId)%in%x1vals),add=T,col="grey")
plot(subset(werp_mesh,as.numeric(CellId)%in%x2vals),add=T,col="yellow")
plot(canals2,add=T,col="blue",lwd=2);plot(LCWG_backfill_ln,add=T,col="blue",lwd=2)
plot(WGSouth,add=T,col="red",lwd=2)
plot(KB_slough,add=T,col="red",lwd=2)
text(werp_mesh,"CellId",cex=0.75)

idval=data.frame()
tmp=merge(subset(waterMoverMap.WFWOR,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[1]
idval=rbind(idval,tmp)

tmp=merge(subset(waterMoverMap.WECBRR,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[2]
idval=rbind(idval,tmp)

tmp=merge(subset(waterMoverMap.AltHR,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[3]
idval=rbind(idval,tmp)

tmp=merge(subset(waterMoverMap.AltHNF,waterMoverType=="ManningCircle"),xvals,c('X1',"X2"))
tmp$Alt=alts[4]
idval=rbind(idval,tmp)


KB_slough.Q=data.frame()
for(i in 1:length(alts)){
  dat.nc<-nc_open(paste0(data.path,"20230331/",alts[i],"/wbbudget.nc"))
  FMWatermovers=ncvar_get(dat.nc,"FMWatermovers")
  
  tmp=t(FMWatermovers[subset(idval,Alt==alts[i])$rownum.val,])*-1  #rev flow north to south
  tmp=data.frame(tmp)
  colnames(tmp)=paste0("C_",paste(subset(idval,Alt==alts[i])$X1,subset(idval,Alt==alts[i])$X2,sep="_"))
  
  tmp.dat=cbind(data.frame(Date=dates.df$date,Alt=alts[i]),tmp)
  
  KB_slough.Q=rbind(KB_slough.Q,tmp.dat)
  print(i)
}
head(KB_slough.Q)

KB_slough.Q$Alt=factor(KB_slough.Q$Alt,levels=alts)
KB_slough.Q$CY=as.numeric(format(KB_slough.Q$Date,'%Y'))
KB_slough.Q$OLFLOW=rowSums(KB_slough.Q[,names(KB_slough.Q)[grepl("C_",names(KB_slough.Q))]],na.rm=T)
KB_slough.Q$OLFLOW.kacft=(KB_slough.Q$OLFLOW*2.29569e-5)/1000 ;# no time conversion kacft/d  

head(KB_slough.Q)
KB_slough.Q$month=as.numeric(format(KB_slough.Q$Date,"%m"))
KB_slough.Q$hydro.season=with(KB_slough.Q,ifelse(month%in%seq(6,10,1),"A_Wet","B_Dry"));# FL.Hydroseason(trans_flow.ol$Date)

KB_slough.flow.seasonal=ddply(KB_slough.Q,c("Alt","CY","hydro.season"),summarise,TFlow.kacft=sum(OLFLOW.kacft,na.rm=T))
KB_slough.flow.seasonal=dcast(KB_slough.flow.seasonal,Alt~hydro.season,value.var="TFlow.kacft",mean)

KB_slough.flow.month=ddply(KB_slough.Q,c("Alt","CY","month"),summarise,TFlow.kacft=sum(OLFLOW.kacft,na.rm=T))
KB_slough.flow.month=ddply(KB_slough.flow.month,c("Alt","month"),summarise,mean.val=mean(TFlow.kacft,na.rm=T))


cols=c("lightblue","khaki3")
# png(filename=paste0(plot.path,"2023_AltNHF/KBSlough_transect.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3.75,0.5,1),oma=c(2,1,1,0.5),lwd=0.5);
layout(matrix(1:4,2,2,byrow=T))

ylim.val=c(0,40);by.y=10;ymaj=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y);ymin=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y/2)
tmp=t(KB_slough.flow.seasonal[,2:3])
x=barplot(tmp,col=cols,border="grey",
          space=0.05,
          ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
mtext(side=3,adj=0,"Kissimmee Billy Slough Overland Flow (North to South)")
abline(h=0,lwd=1)
text(x,colSums(tmp),format(round(colSums(tmp),1)),offset=0.1,pos=ifelse(colSums(tmp)<0,1,3))
text(x,tmp[1,]+tmp[2,]/2,format(round(tmp[2,],1)),font=3,cex=0.7)
text(x,tmp[1,]/2,format(round(tmp[1,],1)),font=3,cex=0.7)
axis_fun(1,x,x,alts,line=-0.5,cex=0.7)
axis_fun(2,ymaj,ymin,format(ymaj));
box(lwd=1)
legend("topleft",legend=c("Wet (June - Oct)","Dry (Nov - May)"),
       lty=c(0),lwd=c(0.1),col=c(cols),pch=22,pt.bg=cols,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=2,line=2.5,"Average Net\nDischarge (x10\u2074 AcFt Y\u207B\u00B9)",cex=0.75)
mtext(side=1,line=1.5,"Alternatives")

xlim.val=c(1,12);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,6.5);by.y=2;ymaj=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y);ymin=seq(max(c(0,ylim.val[1])),ylim.val[2],by.y/2)
plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
with(subset(KB_slough.flow.month,Alt==alts[1]),lines(month,mean.val,col=cols.alts[1],lty=1,lwd=1.5))
with(subset(KB_slough.flow.month,Alt==alts[2]),lines(month,mean.val,col=cols.alts[2],lty=2,lwd=1.5))
for(i in 3:n.alts){
  with(subset(KB_slough.flow.month,Alt==alts[i]),lines(month,mean.val,col=cols.alts[i],lty=1,lwd=1.5))
}
axis_fun(1,xmaj,xmin,month.abb[xmaj],line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("topleft",legend=c(alts),
       lty=c(1,2,1,1),lwd=c(1.5),col=c(cols.alts),
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=1,line=1.5,"Month")
mtext(side=2,line=2.5,"Average Net\nDischarge (x10\u2074 AcFt M\u207B\u00B9)",cex=0.75)

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")  
with(ecdf_fun(subset(KB_slough.Q,Alt==alts[1])$OLFLOW.kacft),lines(1-proportion,value,col=cols.alts[1],lty=1,lwd=1.5))
with(ecdf_fun(subset(KB_slough.Q,Alt==alts[2])$OLFLOW.kacft),lines(1-proportion,value,col=cols.alts[2],lty=2,lwd=1.5))
for(i in 3:n.alts){
  with(ecdf_fun(subset(KB_slough.Q,Alt==alts[i])$OLFLOW.kacft),lines(1-proportion,value,col=adjustcolor(cols.alts[i],0.5),lwd=2))
}
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=1.5,outer=F,"Proportion of Time \u2265 Discharge")  
mtext(side=2,line=2.5,"Discharge Volume (x10\u2074 AcFt D\u207B\u00B9)",cex=0.75)

par(mar=c(0,0.5,0.5,1))
bbox.lims=bbox(gBuffer(KB_slough,width=20000))
plot(trib.all,col="gray80",border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(werp_mesh,border=T,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(LCWG_backfill_ln,add=T,col="blue",lwd=2)
plot(KB_slough,add=T,col="red",lwd=2)
box(lwd=1)
mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);
dev.off()





# END ---------------------------------------------------------------------

















