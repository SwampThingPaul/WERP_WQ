## attempt at opening RSM output netcdfs

# Libraries
library(chron) # package for creating chronological objects
library(ncdf4)  # package to handle NetCDF

library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(tmap) # package for plotting map data
library(RColorBrewer) # package for color palettes

# GIS libraries 
library(rgeos)
library(tmap)

## 
RSMmeshnodes.shp=readOGR("C:/Julian_LaCie/_GitHub/BBSEER_WQ/GIS/Round2","meshnodes")
RSMmesh.shp=readOGR("C:/Julian_LaCie/_GitHub/BBSEER_WQ/GIS/Round2","RSMGL_mesh")
WERP.RSMmesh=readOGR("C:/Julian_LaCie/_GitHub/WERP_WQ/GIS/Mesh_Export","Mesh_Export")


nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")
wgs84=CRS("+init=epsg:4326")
NAD83HARN=CRS("+init=epsg:2881")


## Functions
leg.fun=function(b,pal,leg.title,
                 top.val=0.8,bot.val=0.2,mid.v.val=NULL,
                 x.max=0.3,x.min=0.1,mid.val=NULL,
                 txt.offset.val=-0.01,txt.y=NULL,leg.txt=NULL,
                 txt.cex=0.75,txt.adj=0,txt.pos=4,txt.offset=0.5,
                 title.cex=0.8,title.pos=3,title.adj=0,
                 title.x=NULL,title.y=NULL,
                 leg.type=c("continuous","categorical"), ...){
  l.b=length(b)
  labs=c(paste0("< ",b[2]),paste(b[2:(l.b-2)],b[3:(l.b-1)],sep=" - "),paste(paste0(">",b[(l.b-1)])))
  n.bks=length(b)-1
  mid.v.val=if(is.null(mid.v.val)==T){bot.val+(top.val-bot.val)/2}else{mid.v.val}
  
  mid.val=if(is.null(mid.val)==T){x.min+(x.max-x.min)/2}else{mid.val}
  if(leg.type=="continuous"){
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    rasterImage(legend_image,x.min,bot.val,x.max,top.val)
    txt.y=if(is.null(txt.y)==T){c(bot.val,top.val)}else(txt.y)
    leg.txt=if(is.null(leg.txt)==T){format(c(min(b),max(b)))}else(leg.txt)
    text(x=x.max, y = txt.y, labels =leg.txt,cex=txt.cex,adj=txt.adj,pos=txt.pos,offset=txt.offset, ...)
  }
  if(leg.type=="categorical"){
    bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
    rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal),lty=0)
    text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, 
         labels = rev(labs),cex=txt.cex,xpd=NA,pos=txt.pos,adj=txt.adj)
  }
  
  title.x=if(is.null(title.x)==T){mid.val}else{title.x}
  title.y=if(is.null(title.y)==T){top.val}else{title.y}
  text(x=title.x,y=title.y,leg.title,adj=title.adj,cex=title.cex,pos=title.pos,xpd=NA)
}

## 

mesd2dm=read.table("C:/Julian_LaCie/_GitHub/WERP_WQ/Data/20230331/mesh_werp_V5.2dm",header = F,skip=1,fill=T)
colnames(mesd2dm)=c("MESH2D","CellId",paste0("Node",1:3),"val")


dat.nc<-nc_open("C:/Julian_LaCie/_GitHub/WERP_WQ/Data/20230331/ALTHR/globalmonitors.nc")
print(dat.nc)

attributes(dat.nc$var)$names

ncatt_get(dat.nc, attributes(dat.nc$var)$names[26])
ncatt_get(dat.nc, attributes(dat.nc$var)$names[6])
ncatt_get(dat.nc, attributes(dat.nc$var)$names[8])

meshNodeMap=ncvar_get(dat.nc,"meshNodeMap")
meshNodeMap=data.frame(t(meshNodeMap))
colnames(meshNodeMap)=c("nodeid","node_index")
meshNodeMap$node_index=meshNodeMap$node_index+1

locations=ncvar_get(dat.nc,"locations")
locations=data.frame(t(locations))

locations=cbind(meshNodeMap,locations)
## Cells 
tricons=ncvar_get(dat.nc,"tricons")
tricons=data.frame(t(tricons))
colnames(tricons)=paste0("Node",1:3)

cellmap=ncvar_get(dat.nc,"cellmap")
cellmap=data.frame(t(cellmap))
colnames(cellmap)=c('CellId',"cell_index")
cellmap$cell_index=cellmap$cell_index+1

cellarea=ncvar_get(dat.nc,"cellarea")
cellarea=data.frame(cellarea=cellarea)

CellMap_nodes=cbind(cellmap,tricons)

head(RSMmesh.shp@data,2L)
head(CellMap_nodes,2L)

## Junction Nodes are canals/flow-ways
junctionNodeId=ncvar_get(dat.nc,"junctionNodeId")
junctionNodeId=data.frame(junctionNodeId=junctionNodeId)

junctionNodeLoc=ncvar_get(dat.nc,"junctionNodeLoc")
junctionNodeLoc=data.frame(t(junctionNodeLoc))
junctionNodeLoc.shp=SpatialPointsDataFrame(junctionNodeLoc[,c("X1","X2")],
                                           cbind(junctionNodeId,junctionNodeLoc),
                                           proj4string = NAD83HARN)

junctionMap=ncvar_get(dat.nc,"junctionMap")
junctionMap=data.frame(t(junctionMap))

## Water mover
waterBodyCategory=ncvar_get(dat.nc,"waterBodyCategory")
waterBodyCategory=data.frame(waterBodyCategory=waterBodyCategory)
ncatt_get(dat.nc, "waterBodyCategory")
# 1-cellWBCat, 2-segmentWBCat, 3-lakeWBCat, 4-basinWBCat, 5-impoundmentWBCat, 6-wcdWBCat

waterMoverMap=ncvar_get(dat.nc,"waterMoverMap")
waterMoverMap=data.frame(t(waterMoverMap))
dim(waterMoverMap)

waterMoverName=ncvar_get(dat.nc,"waterMoverName")
waterMoverName=data.frame(waterMoverName=waterMoverName)

waterMoverType=ncvar_get(dat.nc,"waterMoverType")
unique(waterMoverType);dim(waterMoverType)

waterMoverTag=ncvar_get(dat.nc,"waterMoverTag")
unique(waterMoverTag)

waterMoverID=ncvar_get(dat.nc,"waterMoverID")
waterMoverID=data.frame(waterMoverID=waterMoverID)

waterMover=cbind(waterMoverID,waterMoverName,waterMoverMap)


# Mesh Node ---------------------------------------------------------------
locs1=SpatialPointsDataFrame(locations[,c("X1","X2")],
                             locations,
                             proj4string = NAD83HARN)

plot(RSMmeshnodes.shp,pch=21)
plot(locs1,add=T,pch=21,bg="red")
# writeOGR(locs1,"C:/Julian_LaCie/_GitHub/WERP_WQ/Exports/GIS","meshnodes_tmp",driver="ESRI Shapefile",overwrite_layer = T)


# Mesh --------------------------------------------------------------------
head(locations)
head(CellMap_nodes)

range(locations$nodeid)
range(CellMap_nodes$CellId)
range(CellMap_nodes$Node1)
range(CellMap_nodes$Node2)
range(CellMap_nodes$Node3)

subset(CellMap_nodes,Node3==50)
subset(WERP.RSMmesh,Node3==50)
subset(WERP.RSMmesh,CellId==97)@data

mesd2dm=subset(mesd2dm,MESH2D=="E3T")
range(mesd2dm$CellId)
range(mesd2dm$Node1)
range(mesd2dm$Node1)
range(mesd2dm$Node1)

library(sf)
library(sfheaders)

cellids.ls=mesd2dm$CellId
mesh1=st_sf(st_sfc())
for(i in 1:length(cellids.ls)){
  cell.tmp=subset(mesd2dm,CellId==cellids.ls[i])
  
  tmp1=subset(locations,nodeid%in%cell.tmp[,3:5])
  
  dat=tmp1
  dat$CellId=cellids.ls[i]
  sf <- sfheaders::sf_polygon(
    obj = dat
    , x = "X1"
    , y = "X2"
    , polygon_id = "CellId"
  )
  mesh1=rbind(mesh1,sf)
  print(i)
}
plot(mesh1)
attributes(mesh1)

mesh1=as(mesh1,"Spatial")
proj4string(mesh1)=NAD83HARN

par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5),lwd=0.5);
layout(matrix(1:2,1,2))

plot(WERP.RSMmesh)
mtext(side=3,line=-1.2,"WERP RSM Mesh (from FTP)")

plot(mesh1)
mtext(side=3,line=-1.2,"WERP RSM Mesh (from 2dm file)")

# Topo --------------------------------------------------------------------
topo=ncvar_get(dat.nc,"Topography")
topo=cbind(cellmap,data.frame(topo.ft=topo))


mesh1.topo=merge(mesh1,topo,"CellId")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5),lwd=0.5);
layout(matrix(1:2,1,2),widths = c(1,0.5))
bks=seq(-3,32,1)
int.vals=findInterval(mesh1.topo$topo.ft,bks)
cols=hcl.colors(length(bks)-1,"Terrain")
plot(mesh1.topo,col=cols[int.vals],border=F)
# plot(mesh1.topo,add=T,border=adjustcolor("white",0.5))

mapmisc::scaleBar(NAD83HARN,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F);

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(bks,cols,leg.type = "continuous",
        leg.title="Elevation\n(Ft, NGVD29)")



# Pond --------------------------------------------------------------------
ts_stamp=ncvar_get(dat.nc,"timestamps")
ts_stamp=ts_stamp+1
date.vals=as.Date("1964-12-31")+lubridate::duration(ts_stamp,"days")
range(date.vals)

dates.df=data.frame(ts_stamp=ts_stamp,date=date.vals)
dates.df$CY=as.numeric(format(dates.df$date,'%Y'))

ponding=ncvar_get(dat.nc,"PondDepth")

ponding.65=ponding[,subset(dates.df,CY==1965)$ts_stamp]

mesh1.ponding_1965=cbind(mesh1,data.frame(pond65=rowMeans(ponding[,subset(dates.df,CY==1965)$ts_stamp])))

bks=c(0,0.5,1,2,3,5)
int.vals=ifelse(mesh1.ponding_1965$pond65==0,1,findInterval(mesh1.ponding_1965$pond65,bks)+1)
pond.pal=c("white",
            rgb(255/255,253/255,114/253),
            rgb(207/255,255/255,130/255),
            rgb(51/255,206/255,85/255),
            rgb(147/255,213/255,255/255),
            rgb(67/255,133/255,255/255))

par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5),lwd=0.5);
plot(mesh1.ponding_1965,col=pond.pal[int.vals],border=F)



## Hydroperiod
hydro.period65=rowSums(ponding[,subset(dates.df,CY==1965)$ts_stamp]>0)

mesh1.HP_1965=cbind(mesh1,data.frame(HP65=hydro.period65))

bks=c(0,60,120,180,240,300,330,366)
int.vals=findInterval(mesh1.HP_1965$HP65,bks)
HP.pal=c("white",
         rgb(255/255,187/255,51/253),
         rgb(255/255,253/255,114/253),
         rgb(207/255,255/255,130/255),
         rgb(51/255,206/255,85/255),
         rgb(147/255,213/255,255/255),
         rgb(67/255,133/255,255/255))

par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5),lwd=0.5);
plot(mesh1.HP_1965,col=HP.pal[int.vals],border=F)


pond.mean=matrix(NA,nrow=nrow(ponding),ncol=length(yr.vals))
yr.vals=seq(1965,2005,1)
for(i in 1:length(yr.vals)){
tmp=rowMeans(ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp])
pond.mean[,i]=tmp
}

hydro.period=matrix(NA,nrow=nrow(ponding),ncol=length(yr.vals))
yr.vals=seq(1965,2005,1)
for(i in 1:length(yr.vals)){
  tmp=rowSums(ponding[,subset(dates.df,CY==yr.vals[i])$ts_stamp]>0,na.rm=T)
  hydro.period[,i]=tmp
}

mesh1.pond=cbind(mesh1,data.frame(Pondmean=rowMeans(pond.mean,na.rm=T)))

mesh1.HPmean=cbind(mesh1,data.frame(HPmean=rowMeans(hydro.period,na.rm=T)))
int.vals=findInterval(mesh1.HPmean$HPmean,bks)
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5),lwd=0.5);
plot(mesh1.HPmean,col=HP.pal[int.vals],border=F)

tmap_mode("view")
tm_shape(mesh1.HPmean)+tm_polygons("HPmean",breaks=bks,pal=HP.pal)

bks=c(0,0.5,1,2,3,5)
tm_shape(mesh1.pond)+tm_polygons("Pondmean",breaks=bks,pal=pond.pal,alpha=0.5)
