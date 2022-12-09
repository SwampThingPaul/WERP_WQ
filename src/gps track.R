## 
# Title:      Field visit map
# Objective:  Convert gpx to kml
# Created by: Paul Julian; pjulian@sccf.org
# Created on: 08/12/2022
## 
## 

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
library(sf)

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


# -------------------------------------------------------------------------

gpx_dat=st_read(paste0(GIS.path,"/FieldTrip/onx-markups-2022-08-12.gpx"),'track_points')
plot(gpx_dat)

kml_file <- tempfile(fileext = ".kml")
gdal_utils(
  util = "vectortranslate", 
  source = gpx_dat, 
  destination = kml_file, 
  options = c("-f", "KML", "-fieldTypeToString", "DateTime", "track_points"), 
  quiet = FALSE
)

library(rgdal)
gpx_dat2=as(gpx_dat,"Spatial")

d=gpx_dat2
coordinates(d)
d$id=1:nrow(gpx_dat2)
## list of Lines per id, each with one Line in a list
x <- lapply(split(d, d$id), function(x) Lines(list(Line(coordinates(x))), x$id[1L]))


## or one Lines in a list, with all Line objects
## x <- list(Lines(lapply(split(d, d$id), function(x) Line(coordinates(x))), paste(unique(d$id), collapse = "_")))

## etc.
x2=SpatialLines(x,CRS(as.character(NA)))
proj4string(x2)=proj4string(gpx_dat2)

## need to be careful here, assuming one Lines per original row
## and we trash the original rownames  . . .
x3=SpatialLinesDataFrame(x2,data.frame(id=d$id, match.ID = FALSE))
plot(x3)


writeOGR(gpx_dat2,paste0(GIS.path,"/FieldTrip/GreenGladesWest_2022-08-12.kml"),layer="track",driver='KML')

