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

## 


dat.nc<-nc_open("C:/Julian_LaCie/_GitHub/WERP_WQ/Data/20230331/ALTHR/globalmonitors.nc")
print(dat.nc)

attributes(dat.nc$var)$names

ncatt_get(dat.nc, attributes(dat.nc$var)$names[26])
ncatt_get(dat.nc, attributes(dat.nc$var)$names[6])
ncatt_get(dat.nc, attributes(dat.nc$var)$names[8])

meshNodeMap=ncvar_get(dat.nc,"meshNodeMap")
meshNodeMap=data.frame(t(meshNodeMap))

locations=ncvar_get(dat.nc,"locations")
locations=data.frame(t(locations))

## Cells 
tricons=ncvar_get(dat.nc,"tricons")
tricons=data.frame(t(tricons))

cellmap=ncvar_get(dat.nc,"cellmap")
cellmap=data.frame(t(cellmap))

cellarea=ncvar_get(dat.nc,"cellarea")
cellarea=data.frame(cellarea=cellarea)

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
node.values=ncvar_get(dat.nc,"meshNodeMap")
dim(node.values)

dname="locations"
tmp_array <- ncvar_get(dat.nc,dname)
tmp_array=data.frame(t(tmp_array),nodeid=t(node.values)[,1])
locs1=SpatialPointsDataFrame(tmp_array[,c("X1","X2")],
                             tmp_array,
                             proj4string = NAD83HARN)
plot(RSMmeshnodes.shp,pch=21)
plot(locs1,add=T,pch=21,bg="red")
# writeOGR(locs1,"C:/Julian_LaCie/_GitHub/WERP_WQ/Exports/GIS","meshnodes_tmp",driver="ESRI Shapefile",overwrite_layer = T)



# Mesh --------------------------------------------------------------------
tricons=ncvar_get(dat.nc,"tricons")
dim(tricons)

dname="tricons"
tmp_array <- ncvar_get(dat.nc,dname)
dim(tmp_array)
head(t(tmp_array))

tmp_array=data.frame(t(tmp_array))
colnames(tmp_array)=paste0("Node",1:3)


dname="cellmap"
tmp_array2 <- ncvar_get(dat.nc,dname)
dim(tmp_array2)

head(t(tmp_array2))

node.vals=cbind(tmp_array,data.frame(CellId=t(tmp_array2)[,1]))
subset(node.vals,CellId==8281)

cellids.ls=t(tmp_array2)[,1]


library(sf)
library(sfheaders)


mesh1=st_sf(st_sfc())
for(i in 1:length(cellids.ls)){
cell.tmp=subset(node.vals,CellId==cellids.ls[i])

tmp1=subset(locs1,nodeid%in%cell.tmp[,1:3])

dat=tmp1@data
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


library(misc3d)

cell.tmp=subset(node.vals,CellId==cellids.ls[1])

tmp1=subset(locs1,nodeid%in%cell.tmp[,1:3])

tmp1=makeTriangles(subset(locs1,nodeid%in%cell.tmp[,1]),
              subset(locs1,nodeid%in%cell.tmp[,2]),
              subset(locs1,nodeid%in%cell.tmp[,3]))
tmp1
# -------------------------------------------------------------------------
var1=ncvar_get(dat.nc,"Topography")
dim(var1)

dname="cellmap"
tmp_array2 <- ncvar_get(dat.nc,dname)
dim(tmp_array2)

topo.vals=cbind(data.frame(topo=var1),data.frame(CellId=t(tmp_array2)[,1]))

WERP.RSMmesh=merge(WERP.RSMmesh,topo.vals,"CellId")

range(var1)
layout(matrix(1:2,1,2))
bks=seq(-3,60,1)
int.vals=findInterval(WERP.RSMmesh$topo,bks)
cols=hcl.colors(length(bks)-1,"Terrain")
plot(WERP.RSMmesh,col=cols[int.vals],border=F)

#bks=seq(-3,60,0.5)
int.vals=findInterval(WERP.RSMmesh$SFTopo50_T,bks)
cols=hcl.colors(length(bks)-1,"Terrain")
plot(WERP.RSMmesh,col=cols[int.vals],border=F)
