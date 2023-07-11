## WERP
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
ogrListLayers(db.path)

ogrListLayers(paste0(gen.GIS,"/AHED_release/AHED_20171102.gdb"))
ogrListLayers(paste0(gen.GIS,"/AHED_release/20230405/AHED.gdb"))


struct=readOGR(paste0(gen.GIS,"/AHED_release/AHED_20171102.gdb"),"STRUCTURE")
canals=readOGR(paste0(gen.GIS,"/AHED_release/20230405/shp"),"canals")
canals=spTransform(canals,NAD83HARN)
unique(canals$FLOWLINETY)
canals2=subset(canals,FLOWLINETY%in%c("CANALDITCH","STREAMRIVER"))

canals=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),NAD83HARN)
BCNP=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"BCNP"),NAD83HARN)
trib.all=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"TribalAreas_all"),NAD83HARN)
# plot(subset(canals,FLOWLINETY%in%c("CANALDITCH","STREAMRIVER")))
# wmd.struct=spTransform(readOGR(paste0(gen.GIS,"/AHED_release/20230405/AHED.gdb"),"STRUCTURE"),utm17)
# canals3=readOGR(paste0(gen.GIS,"/AHED_release/canals"),"Canals")

wg.canal.shp=spTransform(readOGR(paste0(gis.path,"/WERP Canals"),"WinggateCanal"),NAD83HARN)


## AltH
ogrListLayers(paste0(gis.path,"/20220418_WERP_FEATURES.kml"))
WMSTA=spTransform(readOGR(paste0(gis.path,"/20220418_WERP_FEATURES.kml"),"WMSTA_footprint"),NAD83HARN)
FW=spTransform(readOGR(paste0(gis.path,"/20220418_WERP_FEATURES.kml"),"Flow Way Embankments"),NAD83HARN)
Ranch_Culv=spTransform(readOGR(paste0(gis.path,"/20220418_WERP_FEATURES.kml"),"Ranch Rd Culverts"),NAD83HARN)
WMSTA_flow=spTransform(readOGR(paste0(gis.path,"/20220418_WERP_FEATURES.kml"),"WMSTA_flow"),NAD83HARN)
boundry_culv=spTransform(readOGR(paste0(gis.path,"/20220418_WERP_FEATURES.kml"),"W Boundary Rd Culverts"),NAD83HARN)
wingate_plug=spTransform(readOGR(paste0(gis.path,"/20220418_WERP_FEATURES.kml"),"WingateMill_Plug"),NAD83HARN)

## AltHNF
ogrListLayers(paste0(gis.path,"/AltHNF_backfills.kml"))
LCWG_backfill=spTransform(readOGR(paste0(gis.path,"/AltHNF_dig"),"ALTHNF_backfill"),NAD83HARN)
LCWG_backfill_ln=spTransform(readOGR(paste0(gis.path,"/AltHNF_backfills.kml")),NAD83HARN)


# -------------------------------------------------------------------------
alts=c("WFWOR","WECBRR","ALTHR","ALTHNF")
n.alts=length(alts)

cols.alts=c("grey50","grey10",wesanderson::wes_palette("Zissou1",n.alts-2,"continuous"))


# Structure Q (DSS) -------------------------------------------------------





# RSMGL WERP Mesh -------------------------------------------------------
mesd2dm=read.table(paste0(data.path,"20230331/mesh_werp_V5.2dm"),header = F,skip=1,fill=T)
colnames(mesd2dm)=c("MESH2D","CellId",paste0("Node",1:3),"val")


# globalmonitors ----------------------------------------------------------
WFWOR.dat.nc<-nc_open(paste0(data.path,"20230331/",alts[1],"/globalmonitors.nc"))
WECBRR.dat.nc<-nc_open(paste0(data.path,"20230331/",alts[2],"/globalmonitors.nc"))
ALTHR.dat.nc<-nc_open(paste0(data.path,"20230331/",alts[3],"/globalmonitors.nc"))
ALTHNF.dat.nc<-nc_open(paste0(data.path,"20230331/",alts[4],"/globalmonitors.nc"))

print(WFWOR.dat.nc)
attributes(WFWOR.dat.nc$var)$names

# Mesh/base info ----------------------------------------------------------

meshNodeMap=ncvar_get(WECBRR.dat.nc,"meshNodeMap")
meshNodeMap=data.frame(t(meshNodeMap))
colnames(meshNodeMap)=c("nodeid","node_index")
meshNodeMap$node_index=meshNodeMap$node_index+1

locations=ncvar_get(WECBRR.dat.nc,"locations")
locations=data.frame(t(locations))

locations=cbind(meshNodeMap,locations)
## Cells 
tricons=ncvar_get(WECBRR.dat.nc,"tricons")
tricons=data.frame(t(tricons))
colnames(tricons)=paste0("Node",1:3)

cellmap=ncvar_get(WECBRR.dat.nc,"cellmap")
cellmap=data.frame(t(cellmap))
colnames(cellmap)=c('CellId',"cell_index")
cellmap$cell_index=cellmap$cell_index+1

cellarea=ncvar_get(WECBRR.dat.nc,"cellarea")
cellarea=data.frame(cellarea=cellarea)

CellMap_nodes=cbind(cellmap,tricons)

head(CellMap_nodes,2L)

## meshnode shapefile
meshnode=SpatialPointsDataFrame(locations[,c("X1","X2")],
                                locations,
                                proj4string = NAD83HARN)
# writeOGR(meshnode,paste0(export.path,"GIS"),"WERPMeshNodes",driver="ESRI Shapefile",overwrite_layer = T)
plot(meshnode)

## RSMGL grid
# mesd2dm=subset(mesd2dm,MESH2D=="E3T")
# cellids.ls=mesd2dm$CellId
# mesh1=st_sf(st_sfc())
# for(i in 1:length(cellids.ls)){
#   cell.tmp=subset(mesd2dm,CellId==cellids.ls[i])
#   
#   tmp1=subset(locations,nodeid%in%cell.tmp[,3:5])
#   
#   dat=tmp1
#   dat$CellId=cellids.ls[i]
#   sf <- sfheaders::sf_polygon(
#     obj = dat
#     , x = "X1"
#     , y = "X2"
#     , polygon_id = "CellId"
#   )
#   mesh1=rbind(mesh1,sf)
#   print(i)
# }
# plot(mesh1)
# attributes(mesh1)
# 
# mesh1=as(mesh1,"Spatial")
# proj4string(mesh1)=NAD83HARN
# 
# plot(mesh1)
# 
# plot(gCentroid(mesh1,byid=T),add=T,pch=21)
# writeOGR(mesh1,paste0(export.path,"GIS"),"WERPMesh",driver="ESRI Shapefile",overwrite_layer = T)
mesh1=readOGR(paste0(export.path,"GIS"),"WERPMesh")

plot(mesh1)

mesh1.centroid=data.frame(CellId=mesh1$CellId,x=coordinates(mesh1)[,1],y=coordinates(mesh1)[,2])
mesh1.centroid=SpatialPointsDataFrame(mesh1.centroid[,c("x","y")],
                                      mesh1.centroid,
                                      proj4string = NAD83HARN)
plot(mesh1.centroid,add=T,pch=21,bg="green")

# vector maps -------------------------------------------------------------
ts_stamp=ncvar_get(WFWOR.dat.nc,"timestamps")
ts_stamp=ts_stamp+1
date.vals=as.Date("1964-12-31")+lubridate::duration(ts_stamp,"days")
range(date.vals)
dates.df=data.frame(ts_stamp=ts_stamp,date=date.vals)
dates.df$CY=as.numeric(format(dates.df$date,'%Y'))

magmin = 9999.0
magmax = -9999.0

gwvec=ncvar_get(WFWOR.dat.nc,"gwvector")
yr.vals=1965

anixvect=rowMeans(gwvec[1,,subset(dates.df,CY==yr.vals[1])$ts_stamp],na.rm=T)
aniyvect=rowMeans(gwvec[2,,subset(dates.df,CY==yr.vals[1])$ts_stamp],na.rm=T)
mag=NA
for(i in 1:length(aniyvect)){
  mag[i]=norm(c(anixvect[i],aniyvect[i]),"2")
}
range(mag)

gw.vect.mag=cbind(cellmap,data.frame(magnitude=mag,icomp=anixvect,jcomp=aniyvect,yr=yr.vals[1]))


tmp=merge(mesh1.centroid,gw.vect.mag,"CellId")

bbox.lims=bbox(LCWG_backfill_ln)
plot(mesh1,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(LCWG_backfill_ln,add=T,col="green")
fields::arrow.plot(tmp$x,tmp$y,u=tmp$icomp,v=tmp$jcomp,arrow.ex=5)

bks=seq(min(tmp$magnitude),max(tmp$magnitude),length.out=6)
cols=c(rgb(255/255,187/255,51/253),
       rgb(255/255,253/255,114/253),
       rgb(207/255,255/255,130/255),
       rgb(51/255,206/255,85/255),
       rgb(147/255,213/255,255/255),
       rgb(67/255,133/255,255/255))
int.vals=findInterval(tmp$magnitude,bks)

bbox.lims=bbox(gBuffer(LCWG_backfill_ln,width=5000))
plot(mesh1,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(LCWG_backfill_ln,add=T,col="green")
fields::arrow.plot(tmp$x,tmp$y,u=tmp$icomp,v=tmp$jcomp,arrow.ex=5,length=0.1,
                   col=cols[int.vals])


# Hydroperiod -------------------------------------------------------------

WG.downstream.pts=data.frame(lat=c(26.319770,26.2827,26.28516,26.321042),
                             long=c(-81.135627,-81.1397,-81.08847,-81.094462),
                             id="WG_DS")
WG.ds.poly <- sfheaders::sf_polygon(obj = WG.downstream.pts, x = "long", y = "lat", polygon_id = "id")
plot(WG.ds.poly)

WG.ds.poly=as(WG.ds.poly,"Spatial")
proj4string(WG.ds.poly)=wgs84
WG.ds.poly=spTransform(WG.ds.poly,NAD83HARN)

tm_shape(WG.ds.poly)+tm_polygons()

LC.downstream.pts=data.frame(lat=c(26.3588,26.360586,26.322685,26.320740),
                             long=c(-81.1376,-81.09657,-81.095062,-81.137776),
                             id="LC_DS")
LC.ds.poly <- sfheaders::sf_polygon(obj = LC.downstream.pts, x = "long", y = "lat", polygon_id = "id")
plot(LC.ds.poly)
LC.ds.poly=as(LC.ds.poly,"Spatial")
proj4string(LC.ds.poly)=wgs84
LC.ds.poly=spTransform(LC.ds.poly,NAD83HARN)


L28I.pts=data.frame(lat=c( 26.250435,26.25477, 26.16269, 26.156589),
                    long=c(-80.987338,-80.886296,-80.877239,-80.983759),
                    id="L28I")
L28I.ds.poly <- sfheaders::sf_polygon(obj = L28I.pts, x = "long", y = "lat", polygon_id = "id")
plot(L28I.ds.poly)
L28I.ds.poly=as(L28I.ds.poly,"Spatial")
proj4string(L28I.ds.poly)=wgs84
L28I.ds.poly=spTransform(L28I.ds.poly,NAD83HARN)
# plot(mesh1)
# plot(WG.ds.poly,add=T,col="red")
# plot(LC.ds.poly,add=T,col="red")
# 
# tm_shape(mesh1)+tm_polygons(col="white",alpha=0.5)+
#   tm_shape(WG.ds.poly)+tm_polygons(alpha=0.5)+
#   tm_shape(LC.ds.poly)+tm_polygons(alpha=0.5)+
#   tm_shape(mesh1[LC.ds.poly,])+tm_polygons(col="blue")

# plot(mesh1[WG.ds.poly,])
# plot(mesh1[LC.ds.poly,])

# plot(mesh1)
# plot(mesh1[WG.ds.poly,],add=T,col="blue")
# plot(WG.ds.poly,add=T,border="red")

# Ponding/HP --------------------------------------------------------------
ts_stamp=ncvar_get(WFWOR.dat.nc,"timestamps")
ts_stamp=ts_stamp+1
date.vals=as.Date("1964-12-31")+lubridate::duration(ts_stamp,"days")
range(date.vals)
dates.df=data.frame(ts_stamp=ts_stamp,date=date.vals)
dates.df$CY=as.numeric(format(dates.df$date,'%Y'))


yr.vals=seq(1965,2005,1)
FWO.ponding=ncvar_get(WFWOR.dat.nc,"PondDepth")

FWO.pond.mean=matrix(NA,nrow=nrow(FWO.ponding),ncol=length(yr.vals))
FWO.HP.mean=matrix(NA,nrow=nrow(FWO.ponding),ncol=length(yr.vals))
for(i in 1:length(yr.vals)){
  tmp=rowMeans(FWO.ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp])
  FWO.pond.mean[,i]=tmp
  
  tmp=rowSums(FWO.ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp]>0,na.rm=T)
  FWO.HP.mean[,i]=tmp
}

WECBRR.ponding=ncvar_get(WECBRR.dat.nc,"PondDepth")
WECBRR.pond.mean=matrix(NA,nrow=nrow(WECBRR.ponding),ncol=length(yr.vals))
WECBRR.HP.mean=matrix(NA,nrow=nrow(WECBRR.ponding),ncol=length(yr.vals))
for(i in 1:length(yr.vals)){
  tmp=rowMeans(WECBRR.ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp])
  WECBRR.pond.mean[,i]=tmp
  
  tmp=rowSums(WECBRR.ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp]>0,na.rm=T)
  WECBRR.HP.mean[,i]=tmp
}

ALTHR.ponding=ncvar_get(ALTHR.dat.nc,"PondDepth")
ALTHR.pond.mean=matrix(NA,nrow=nrow(ALTHR.ponding),ncol=length(yr.vals))
ALTHR.HP.mean=matrix(NA,nrow=nrow(ALTHR.ponding),ncol=length(yr.vals))
for(i in 1:length(yr.vals)){
  tmp=rowMeans(ALTHR.ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp])
  ALTHR.pond.mean[,i]=tmp
  
  tmp=rowSums(ALTHR.ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp]>0,na.rm=T)
  ALTHR.HP.mean[,i]=tmp
}

ALTHNF.ponding=ncvar_get(ALTHNF.dat.nc,"PondDepth")
ALTHNF.pond.mean=matrix(NA,nrow=nrow(ALTHNF.ponding),ncol=length(yr.vals))
ALTHNF.HP.mean=matrix(NA,nrow=nrow(ALTHNF.ponding),ncol=length(yr.vals))
for(i in 1:length(yr.vals)){
  tmp=rowMeans(ALTHNF.ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp])
  ALTHNF.pond.mean[,i]=tmp
  
  tmp=rowSums(ALTHNF.ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp]>0,na.rm=T)
  ALTHNF.HP.mean[,i]=tmp
}

## Temporally average spatial TS
HPmean=cbind(mesh1,data.frame(FWO=rowMeans(FWO.HP.mean,na.rm=T),
                              WECBRR=rowMeans(WECBRR.HP.mean,na.rm=T),
                              ALTHR=rowMeans(ALTHR.HP.mean,na.rm=T),
                              ALTHNF=rowMeans(ALTHNF.HP.mean)))
PondDepth.mean=cbind(mesh1,data.frame(FWO=rowMeans(FWO.pond.mean,na.rm=T),
                                     WECBRR=rowMeans(WECBRR.pond.mean,na.rm=T),
                                     ALTHR=rowMeans(ALTHR.pond.mean,na.rm=T),
                                     ALTHNF=rowMeans(ALTHNF.pond.mean)))

pond.pal=c("grey95",
                  rgb(255/255,253/255,114/253),
                  rgb(207/255,255/255,130/255),
                  rgb(51/255,206/255,85/255),
                  rgb(147/255,213/255,255/255),
                  rgb(67/255,133/255,255/255))
HP.pal=c("grey95",
                rgb(255/255,187/255,51/253),
                rgb(255/255,253/255,114/253),
                rgb(207/255,255/255,130/255),
                rgb(51/255,206/255,85/255),
                rgb(147/255,213/255,255/255),
                rgb(67/255,133/255,255/255))
# int.vals=ifelse(HPmean$WECBRR==0,1,findInterval(HPmean$WECBRR,bks)+1)
pond.bks=c(0,0.5,1,2,3,5)

# png(filename=paste0(plot.path,"2023_AltNHF/MeanHP.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,1.5,0.5),lwd=0.5);
layout(matrix(c(1,2,5,3,4,6),2,3,byrow=T),widths=c(1,1,0.5))
# bbox.lims=bbox(bind(WG.ds.poly,LC.ds.poly))
bbox.lims=bbox(spTransform(gBuffer(spTransform(bind(WG.ds.poly,LC.ds.poly),utm17),width=2000),NAD83HARN))

HP.bks=c(0,60,120,180,240,300,330,366)
int.vals=findInterval(HPmean$WECBRR,HP.bks)
plot(HPmean,col=HP.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);
mtext(side=3,line=-1.25,adj=1,font=2,"WECBRR ")

int.vals=findInterval(HPmean$FWO,HP.bks)
plot(HPmean,col=HP.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,line=-1.25,adj=1,font=2,"WFWOR ")

int.vals=findInterval(HPmean$ALTHR,HP.bks)
plot(HPmean,col=HP.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,line=-1.25,adj=1,font=2,"ALTHR ")
plot(FW,add=T,col="purple",lwd=2)
plot(WMSTA,add=T,border="grey50",col=adjustcolor("grey",0.5),lwd=2,lty=2)
plot(Ranch_Culv,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(boundry_culv,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(WMSTA_flow,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(wingate_plug,add=T,pch=4,cex=2)

int.vals=findInterval(HPmean$ALTHNF,HP.bks)
plot(HPmean,col=HP.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,line=-1.25,adj=1,font=2 ,"ALTHNF ")
plot(Ranch_Culv,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(boundry_culv,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(LCWG_backfill,add=T,pch=8,cex=2,col="grey")
plot(wingate_plug,add=T,pch=4,cex=2)

plot(mesh1,border="grey",lwd=0.01)
AOI=raster::extent(spTransform(gBuffer(spTransform(bind(WG.ds.poly,LC.ds.poly),utm17),width=2000),NAD83HARN))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm17
plot(AOI.poly,add=T,border="red",lwd=2,lty=1)

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(HP.bks,HP.pal,leg.type = "categorical",
        leg.title="Mean Annual\nHydroperoid\n(Days)",
        xmax=1,xmin=0.6)
dev.off()

# png(filename=paste0(plot.path,"2023_AltNHF/MeanHP_L28I.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,1.5,0.5),lwd=0.5);
layout(matrix(c(1,2,5,3,4,6),2,3,byrow=T),widths=c(1,1,0.5))
# bbox.lims=bbox(bind(WG.ds.poly,LC.ds.poly))
bbox.lims=bbox(spTransform(gBuffer(spTransform(L28I.ds.poly,utm17),width=2000),NAD83HARN))

AOI=raster::extent(spTransform(gBuffer(spTransform(L28I.ds.poly,utm17),width=2000),NAD83HARN))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm17

HP.bks=c(0,60,120,180,240,300,330,366)
int.vals=findInterval(HPmean$WECBRR,HP.bks)
plot(HPmean,col=HP.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
plot(BCNP,add=T,col=NA,lty=2)
box(lwd=1)
mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);
mtext(side=3,line=-1.25,adj=1,font=2,"WECBRR ")

int.vals=findInterval(HPmean$FWO,HP.bks)
plot(HPmean,col=HP.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
plot(BCNP,add=T,col=NA,lty=2)
box(lwd=1)
mtext(side=3,line=-1.25,adj=1,font=2,"WFWOR ")

int.vals=findInterval(HPmean$ALTHR,HP.bks)
plot(HPmean,col=HP.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
plot(BCNP,add=T,col=NA,lty=2)
box(lwd=1)
mtext(side=3,line=-1.25,adj=1,font=2,"ALTHR ")

int.vals=findInterval(HPmean$ALTHNF,HP.bks)
plot(HPmean,col=HP.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
plot(BCNP,add=T,col=NA,lty=2)
box(lwd=1)
mtext(side=3,line=-1.25,adj=1,font=2 ,"ALTHNF ")

plot(mesh1,border="grey",lwd=0.01)
plot(AOI.poly,add=T,border="red",lwd=2,lty=1)

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(HP.bks,HP.pal,leg.type = "categorical",
        leg.title="Mean Annual\nHydroperoid\n(Days)",
        x.max=1,x.min=0.6)
dev.off()


## Diff in HP between FWO and Alts
HPmean$FWO_AltHR=with(HPmean@data,ALTHR-FWO)
HPmean$FWO_AltHNF=with(HPmean@data,ALTHNF-FWO)

hist(HPmean$FWO_AltHR)
hist(HPmean$FWO_AltHNF)

HP.bks=seq(-400,400,50)
HP.diff.pal=colorRampPalette(c("red","white","blue"))(length(HP.bks)-1)
# png(filename=paste0(plot.path,"2023_AltNHF/MeanHP_diff.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,1.5,0.5),lwd=0.5);
layout(matrix(c(1:3),1,3,byrow=T),widths=c(1,1,0.5))
# bbox.lims=bbox(bind(WG.ds.poly,LC.ds.poly))

int.vals=findInterval(HPmean$FWO_AltHR,HP.bks)
plot(HPmean,col=HP.diff.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,"AltHR - WFWOR")

int.vals=findInterval(HPmean$FWO_AltHNF,HP.bks)
plot(HPmean,col=HP.diff.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(HPmean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,"AltHNF - WFWOR")

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(HP.bks,HP.diff.pal,leg.type = "categorical",
        leg.title="Difference\nin\nHydroperoid\n(Days)",
        xmax=1,xmin=0.6)
dev.off()

# png(filename=paste0(plot.path,"2023_AltNHF/MeanPondZ.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,1.5,0.5),lwd=0.5);
layout(matrix(c(1,2,5,3,4,6),2,3,byrow=T),widths=c(1,1,0.5))
# bbox.lims=bbox(bind(WG.ds.poly,LC.ds.poly))
bbox.lims=bbox(spTransform(gBuffer(spTransform(bind(WG.ds.poly,LC.ds.poly),utm17),width=2000),NAD83HARN))
 
pond.bks=c(0,0.5,1,2,3,5)
int.vals=ifelse(PondDepth.mean$WECBRR==0,1,findInterval(PondDepth.mean$WECBRR,pond.bks)+1)
plot(PondDepth.mean,col=pond.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(PondDepth.mean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);
mtext(side=3,line=-1.25,adj=1,font=2,"WECBRR ")

int.vals=ifelse(PondDepth.mean$FWO==0,1,findInterval(PondDepth.mean$FWO,pond.bks)+1)
plot(PondDepth.mean,col=pond.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(PondDepth.mean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,line=-1.25,adj=1,font=2,"WFWOR ")

int.vals=ifelse(PondDepth.mean$ALTHR==0,1,findInterval(PondDepth.mean$ALTHR,pond.bks)+1)
plot(PondDepth.mean,col=pond.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(PondDepth.mean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,line=-1.25,adj=1,font=2,"ALTHR ")
plot(FW,add=T,col="purple",lwd=2)
plot(WMSTA,add=T,border="grey50",col=adjustcolor("grey",0.5),lwd=2,lty=2)
plot(Ranch_Culv,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(boundry_culv,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(WMSTA_flow,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(wingate_plug,add=T,pch=4,cex=2)

int.vals=ifelse(PondDepth.mean$ALTHNF==0,1,findInterval(PondDepth.mean$ALTHNF,pond.bks)+1)
plot(PondDepth.mean,col=pond.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(PondDepth.mean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,line=-1.25,adj=1,font=2,"ALTHNF ")
plot(Ranch_Culv,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(boundry_culv,add=T,pch=21,bg="indianred1",lwd=0.01,cex=1.25)
plot(LCWG_backfill,add=T,pch=8,cex=2,col="grey")
plot(wingate_plug,add=T,pch=4,cex=2)

plot(mesh1,border="grey",lwd=0.01)
AOI=raster::extent(spTransform(gBuffer(spTransform(bind(WG.ds.poly,LC.ds.poly),utm17),width=2000),NAD83HARN))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm17
plot(AOI.poly,add=T,border="red",lwd=2,lty=1)

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(format(pond.bks),pond.pal,leg.type = "categorical",
        leg.title="Annual Mean\nPonding Depth\n(Ft)",
        xmax=1,xmin=0.6)
dev.off()


PondDepth.mean$FWO_AltHR=with(PondDepth.mean@data,ALTHR-FWO)
PondDepth.mean$FWO_AltHNF=with(PondDepth.mean@data,ALTHNF-FWO)

hist(PondDepth.mean$FWO_AltHR)
hist(PondDepth.mean$FWO_AltHNF)

HP.bks=seq(-3,3,0.5)
HP.diff.pal=colorRampPalette(c("red","white","blue"))(length(HP.bks)-1)
# png(filename=paste0(plot.path,"2023_AltNHF/MeanPond_diff.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,1.5,0.5),lwd=0.5);
layout(matrix(c(1:3),1,3,byrow=T),widths=c(1,1,0.5))
# bbox.lims=bbox(bind(WG.ds.poly,LC.ds.poly))

int.vals=findInterval(PondDepth.mean$FWO_AltHR,HP.bks)
plot(PondDepth.mean,col=HP.diff.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(PondDepth.mean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,"AltHR - WFWOR")

int.vals=findInterval(PondDepth.mean$FWO_AltHNF,HP.bks)
plot(PondDepth.mean,col=HP.diff.pal[int.vals],border=F,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(PondDepth.mean,border="grey",lwd=0.1,add=T)
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mtext(side=3,"AltHNF - WFWOR")

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(HP.bks,HP.diff.pal,leg.type = "categorical",
        leg.title="Difference\nin\nPonding Depth\n(Ft)",
        xmax=1,xmin=0.6)
dev.off()








## Spatially average TS 
vals=mesh1[WG.ds.poly,]@data
subset(cellmap,CellId%in%vals$CellId)$cell_index
plot(colMeans(ALTHNF.HP.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]))

WG.ds.mean.vals=rbind(
  data.frame(Alt="ALTHNF",
             region="Winggate Mill DS",
             Yr=yr.vals,
             HP=colMeans(ALTHNF.HP.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]),
             PondDepth=colMeans(ALTHNF.pond.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,])),
  data.frame(Alt="ALTHR",
             region="Winggate Mill DS",
             Yr=yr.vals,
             HP=colMeans(ALTHR.HP.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]),
             PondDepth=colMeans(ALTHR.pond.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,])),
  data.frame(Alt="WECBRR",
             region="Winggate Mill DS",
             Yr=yr.vals,
             HP=colMeans(WECBRR.HP.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]),
             PondDepth=colMeans(WECBRR.pond.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,])),
  data.frame(Alt="WFWOR",
             region="Winggate Mill DS",
             Yr=yr.vals,
             HP=colMeans(FWO.HP.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]),
             PondDepth=colMeans(FWO.pond.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]))
)
WG.ds.mean.vals$Alt=factor(WG.ds.mean.vals$Alt,levels=alts)


vals=mesh1[LC.ds.poly,]@data
LC.ds.mean.vals=rbind(
  data.frame(Alt="ALTHNF",
             region="Lardcan DS",
             Yr=yr.vals,
             HP=colMeans(ALTHNF.HP.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]),
             PondDepth=colMeans(ALTHNF.pond.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,])),
  data.frame(Alt="ALTHR",
             region="Lardcan DS",
             Yr=yr.vals,
             HP=colMeans(ALTHR.HP.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]),
             PondDepth=colMeans(ALTHR.pond.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,])),
  data.frame(Alt="WECBRR",
             region="Lardcan DS",
             Yr=yr.vals,
             HP=colMeans(WECBRR.HP.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]),
             PondDepth=colMeans(WECBRR.pond.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,])),
  data.frame(Alt="WFWOR",
             region="Lardcan DS",
             Yr=yr.vals,
             HP=colMeans(FWO.HP.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]),
             PondDepth=colMeans(FWO.pond.mean[subset(cellmap,CellId%in%vals$CellId)$cell_index,]))
)
LC.ds.mean.vals$Alt=factor(LC.ds.mean.vals$Alt,levels=alts)

# png(filename=paste0(plot.path,"2023_AltNHF/LC_temporal.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,0.5),oma=c(2,2.5,1,0.5),lwd=0.5);
layout(matrix(c(1,2,5,3,4,6),2,3,byrow=T),widths=c(1,1,0.5))

xlim.val=c(1965,2005);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/3)
plot(HP~Yr,LC.ds.mean.vals,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lty.val=c(1,2,1,1)
for(i in 1:n.alts){
lines(HP~Yr,subset(LC.ds.mean.vals,Alt==alts[i]),lty=lty.val[i],col=cols.alts[i])
}
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("topleft",legend=alts,
       lty=lty.val,lwd=2,col=cols.alts,
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=1,yjust=1,title.adj = 0.5)
mtext(side=2,line=2.75,"Annual Hydroperiod (Days)")
mtext(side=3,adj=0,"Lardcan Canal region")

x=boxplot(HP~Alt,LC.ds.mean.vals,ylim=ylim.val,col=adjustcolor(cols.alts,0.5),axes=F,ann=F,outline=F)
axis_fun(1,1:4,1:4,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
DT.val=with(LC.ds.mean.vals,dunn.test::dunn.test(HP,Alt))
DT.lts=rcompanion::cldList(P.adjusted ~ comparison,data=DT.val,threshold = 0.05)
DT.lts=DT.lts[match(DT.lts$Group,alts),]
dunn.letters(4,1:4,x$stats[5,],toupper(DT.lts$Letter),"red",1)


ylim.val=c(0,0.3);by.y=0.05;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/3)
plot(PondDepth~Yr,LC.ds.mean.vals,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lty.val=c(1,2,1,1)
for(i in 1:n.alts){
  lines(PondDepth~Yr,subset(LC.ds.mean.vals,Alt==alts[i]),lty=lty.val[i],col=cols.alts[i])
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.75,"Annual Mean\nPonding Depth (Ft)")
mtext(side=1,line=2,"Calendar Year")

x=boxplot(PondDepth~Alt,LC.ds.mean.vals,ylim=ylim.val,col=adjustcolor(cols.alts,0.5),axes=F,ann=F,outline=F)
axis_fun(1,1:4,1:4,alts,line=-0.5,cex=0.8)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
DT.val=with(LC.ds.mean.vals,dunn.test::dunn.test(PondDepth,Alt))
DT.lts=rcompanion::cldList(P.adjusted ~ comparison,data=DT.val,threshold = 0.05)
DT.lts=DT.lts[match(DT.lts$Group,alts),]
dunn.letters(4,1:4,x$stats[5,],toupper(DT.lts$Letter),"red",1)
mtext(side=1,line=2,"Alternative")

par(mar=c(1,0.1,0.5,0.1))
bbox.lims=bbox(spTransform(gBuffer(spTransform(bind(WG.ds.poly,LC.ds.poly),utm17),width=2000),NAD83HARN))
plot(PondDepth.mean,border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(mesh1[LC.ds.poly,],add=T,border="red",col="pink")
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);

plot(0:1,0:1,ann=F,axes=F,type="n")
dev.off()


# png(filename=paste0(plot.path,"2023_AltNHF/WG_temporal.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,0.5),oma=c(2,2.5,1,0.5),lwd=0.5);
layout(matrix(c(1,2,5,3,4,6),2,3,byrow=T),widths=c(1,1,0.5))

xlim.val=c(1965,2005);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,366);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/3)
plot(HP~Yr,WG.ds.mean.vals,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lty.val=c(1,2,1,1)
for(i in 1:n.alts){
  lines(HP~Yr,subset(WG.ds.mean.vals,Alt==alts[i]),lty=lty.val[i],col=cols.alts[i])
}
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("topleft",legend=alts,
       lty=lty.val,lwd=2,col=cols.alts,
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=1,yjust=1,title.adj = 0.5)
mtext(side=2,line=2.75,"Annual Hydroperiod (Days)")
mtext(side=3,adj=0,"Winggate mill Canal region")

x=boxplot(HP~Alt,WG.ds.mean.vals,ylim=ylim.val,col=adjustcolor(cols.alts,0.5),axes=F,ann=F,outline=F)
axis_fun(1,1:4,1:4,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
DT.val=with(WG.ds.mean.vals,dunn.test::dunn.test(HP,Alt))
DT.lts=rcompanion::cldList(P.adjusted ~ comparison,data=DT.val,threshold = 0.05)
DT.lts=DT.lts[match(DT.lts$Group,alts),]
dunn.letters(4,1:4,x$stats[5,],toupper(DT.lts$Letter),"red",1)


ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/3)
plot(PondDepth~Yr,WG.ds.mean.vals,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lty.val=c(1,2,1,1)
for(i in 1:n.alts){
  lines(PondDepth~Yr,subset(WG.ds.mean.vals,Alt==alts[i]),lty=lty.val[i],col=cols.alts[i])
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2.75,"Annual Mean\nPonding Depth (Ft)")
mtext(side=1,line=2,"Calendar Year")

x=boxplot(PondDepth~Alt,WG.ds.mean.vals,ylim=ylim.val,col=adjustcolor(cols.alts,0.5),axes=F,ann=F,outline=F)
axis_fun(1,1:4,1:4,alts,line=-0.5,cex=0.8)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
DT.val=with(WG.ds.mean.vals,dunn.test::dunn.test(PondDepth,Alt))
DT.lts=rcompanion::cldList(P.adjusted ~ comparison,data=DT.val,threshold = 0.05)
DT.lts=DT.lts[match(DT.lts$Group,alts),]
dunn.letters(4,1:4,x$stats[5,],toupper(DT.lts$Letter),"red",1)
mtext(side=1,line=2,"Alternative")

par(mar=c(1,0.1,0.5,0.1))
bbox.lims=bbox(spTransform(gBuffer(spTransform(bind(WG.ds.poly,LC.ds.poly),utm17),width=2000),NAD83HARN))
plot(PondDepth.mean,border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(mesh1[WG.ds.poly,],add=T,border="red",col="pink")
plot(canals2,add=T,col="blue",lwd=2);plot(wg.canal.shp,add=T,col="blue",lwd=2)
box(lwd=1)
mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);

plot(0:1,0:1,ann=F,axes=F,type="n")
dev.off()

