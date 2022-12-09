## Title:      Analysis of Lee County 10 mile canal filter marsh data
## Created by: Paul Julian (pjulian@sccf.org)
## Created on: 12/08/2022 

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

# Libraries
# devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(tmap)



## Paths
wd="C:/Julian_LaCie/_GitHub/WERP_WQ"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

GIS.path.gen=paste0(dirname(dirname(wd)),"/_GISData")

nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")
wgs84=CRS("+init=epsg:4326")

tmap_mode("view")

# -------------------------------------------------------------------------
dat=read.csv(paste0(data.path,"LeeCounty_filtermarsh/GIS_Surface_Water.txt"))
unique(dat$SAMPLE_LOCCODE)

# QA/QC qualifiers 
unique(dat$RESULT_QUALIFY)
dat.qual=data.frame(QUALIFIER=c(NA,"!","A","D","E","F","I","R","T","U","*","?","B","H","J","K","L","M","N","O","Q","V","Y","Z"),
                    FATALYN=c("N",rep("N",9),rep("Y",14)))

quals=as.character(unique(dat$RESULT_QUALIFY))
spl=strsplit(quals,split="")
quals=data.frame(RESULT_QUALIFY=quals,
                 q1=sapply(spl,"[",1),
                 q2=sapply(spl,"[",2),
                 q3=sapply(spl,"[",3),
                 q4=sapply(spl,"[",4),
                 q5=sapply(spl,"[",5))
quals$Fatal=with(quals,ifelse(q1%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q2%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q3%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q4%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER,"Y","N"))
quals$Fatal=with(quals,ifelse(RESULT_QUALIFY%in%c("STL","ELAB"),"N",Fatal))

dat=merge(dat,quals[,c("RESULT_QUALIFY","Fatal")],"RESULT_QUALIFY")

lee.analyte=ddply(dat,c("RESULT_ANALYTE","AUNIT"),summarise,N.val=N.obs(ID))
param.xwalk=data.frame(
  RESULT_ANALYTE=c("Ammonia", "Biochemical Oxygen Demand 5 day", "Chlorophyll a - corrected for Pheophytin", 
                   "Color", "Enterococci", "Enterococci", "Field Temperature", "LCS Turbidity (Nephelometric)", 
                   "Nitrate", "NITRATE", "Nitrate + Nitrite", "NITRATE + NITRITE", 
                   "Nitrite", "NITRITE", "Nitrogen, Kjeldahl, Total", "Nitrogen, Organic", 
                   "Nitrogen, Total", "Oxygen, Dissolved, Electrode", "OXYGEN, DISSOLVED, ELECTRODE", 
                   "Oxygen, Dissolved, Percent Saturation", "pH      (electrometric)", 
                   "pH, Field (electrometric)", "Pheophytin", "Phosphorus, Ortho", 
                   "Phosphorus, Total", "Photosynthetically Active Radiation", "PHOTOSYNTHETICALLY ACTIVE RADIATION", 
                   "Salinity by meter", "Silica, disolved as SiO2", "Silica, dissolved as SiO2", 
                   "Specific Conductance, 25 C, Field", "Specific Conductance, 25°C", 
                   "Specific Conductance, 25°C, Field", "Total Organic Carbon", 
                   "TOTAL ORGANIC CARBON", "Total Suspended Solids", "Turbidity (Nephelometric)", 
                   "TURBIDITY (NEPHELOMETRIC)", "Turbidity (Nephelometric), field measure", 
                   "TURBIDITY (NEPHELOMETRIC), FIELD MEASURE", "Water Clarity by Secchi Disk", 
                   "WATER CLARITY BY SECCHI DISK"),
  param=c("NH4","BOD","Chla",
          "color","entero","entero","temp","Turb",
          "NO3","NO3","NOx","NOx",
          "NO2","NO2","TKN","TON",
          "TN","DO.mgL","DO.mgL",
          "DO.persat","pH",
          "pH","Pheo","SRP",
          "TP","K.par","K.par",
          "Sal","SiO2","SiO2",
          "SPC","SPC","SPC","TOC",
          "TOC","TSS","Turb","Turb","Turb","Turb","secchi","secchi"),
  Units=c("mg/L","mg/L","ug/L",
          "PCU","MPN/100mL","MPN/100mL","Deg C","NTU",
          rep("mg/L",11),"%","SU","SU",
          "ug/L","mg/L","mg/L","1/m","1/m","PSU","mg/L","mg/L",
          rep("uS/cm",3),"mg/L","mg/L","mg/L",
          rep("NTU",4),"m","m"))


dat=merge(dat,param.xwalk,"RESULT_ANALYTE")
dat$DateTime.EST=date.fun(dat$SAMPLE_COLDATE,form="%m/%d/%Y %H:%M")
dat$Date.EST=date.fun(dat$DateTime.EST)
# dat$Station.ID=dat$SAMPLE_LOCCODE
dat$HalfMDL=dat$CALC_RESULT

dat$Station.ID=dat$SAMPLE_LOCCODE


subset(dat.clean,N.val>1)
unique(dat.clean$Station.ID)


site.locs=ddply(dat,c("Station.ID"),summarise,Lat=mean(SUSERFLDS_LATITUDE,na.rm=T),Long=mean(SUSERFLDS_LONGITUDE,na.rm=T),N.val=N.obs(Station.ID))
site.locs2=data.frame(Station.ID=c("10MIFM01", "10MIFM02", "10MIFM02A", "10MIFM03", "10MIFM03A", 
                                   "10MIFM04", "10MIFM04A", "10MIFM05", "10MIFM06", "10MIFM06A", 
                                   "10MIFM07", "10MIFM08A", "10MIGR20", "10MIGR50"),
                      alias=c("10MIFM01", "10MIFM02", "10MIFM02", "10MIFM03", "10MIFM03", 
                              "10MIFM04", "10MIFM04", "10MIFM05", "10MIFM06", "10MIFM06", 
                              "10MIFM07", "10MIFM08", "10MIGR20", "10MIGR50"))

site.locs.shp=SpatialPointsDataFrame(site.locs[,c("Long","Lat")],
                                     site.locs,
                                     proj4string = wgs84)
tm_shape(site.locs.shp)+tm_dots()

###
dat=merge(dat,site.locs2,"Station.ID",all.x=T)
dat.clean=ddply(subset(dat,Fatal=="N"&SUSERFLDS_SAMP_TYPE!="FD"),
                c("alias","Date.EST","param", "Units"),summarise, 
                HalfMDL=mean(HalfMDL,na.rm=T),N.val=N.obs(param),
                min.val=min(HalfMDL,na.rm=T),max.val=max(HalfMDL,na.rm=T))
dat.clean$WY=WY(dat.clean$Date.EST)

plot(HalfMDL~Date.EST,subset(dat.clean,param=="TP"&alias=="10MIFM01"),type="l")
lines(HalfMDL~Date.EST,subset(dat.clean,param=="TP"&alias=="10MIFM02"),col="red")
# lines(HalfMDL~Date.EST,subset(dat.clean,param=="TP"&alias=="10MIFM07"),col="blue")
lines(HalfMDL~Date.EST,subset(dat.clean,param=="TP"&alias=="10MIFM06"),col="blue")

plot(HalfMDL~Date.EST,subset(dat.clean,param=="TN"&alias=="10MIFM01"),type="l")
lines(HalfMDL~Date.EST,subset(dat.clean,param=="TN"&alias=="10MIFM02"),col="red")
# lines(HalfMDL~Date.EST,subset(dat.clean,param=="TP"&alias=="10MIFM07"),col="blue")
lines(HalfMDL~Date.EST,subset(dat.clean,param=="TN"&alias=="10MIFM06"),col="blue")


WY.sum=ddply(subset(dat.clean,param%in%c("TP","TN")),
             c("alias",'WY',"param"),summarise,
             GM.val=exp(mean(log(HalfMDL),na.rm=T)),
             N.val=N.obs(HalfMDL))
WY.sum$N.screen=with(WY.sum,ifelse(N.val>=4,1,0))
subset(WY.sum,N.screen==0)

xtab.GM.dat=dcast(subset(WY.sum,N.screen==1&alias%in%c("10MIGR50","10MIFM02","10MIFM06")&WY%in%seq(2014,2021,1)),
                  param+WY~alias,value.var = "GM.val",mean)
colnames(xtab.GM.dat)=c("param","WY","inflow","outflow","upstream")
xtab.GM.dat
xtab.GM.dat$per.change=with(xtab.GM.dat,(outflow-inflow)/inflow)*100
sum.stats=ddply(xtab.GM.dat,c("param"),summarise,mean.in=mean(inflow),mean.out=mean(outflow))

GM.dat=subset(WY.sum,N.screen==1&alias%in%c("10MIFM02","10MIFM06")&WY%in%seq(2014,2021,1))
GM.dat=merge(GM.dat,
             data.frame(
               alias=c("10MIFM02","10MIFM06"),
               loc=c("inflow","outflow")),
             "alias")

plot(GM.val~WY,subset(GM.dat,param=="TN"&loc=="inflow"),type="l",ylim=c(0,2))
lines(GM.val~WY,subset(GM.dat,param=="TN"&loc=="outflow"),col="red")

plot(GM.val~WY,subset(GM.dat,param=="TP"&loc=="inflow"),type="l",ylim=c(0,0.05))
lines(GM.val~WY,subset(GM.dat,param=="TP"&loc=="outflow"),col="red")


GM.dat.sumstats=ddply(GM.dat,c("param","loc"),summarise,mean.val=mean(GM.val),sd.val=sd(GM.val))

tmp=dcast(GM.dat, param~loc,value.var = "GM.val",mean)
tmp$per.change=with(tmp,(outflow-inflow)/inflow)*100

# png(filename=paste0(plot.path,"LeeFilter/GMCompare.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,1,1),oma=c(2.5,1,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

ylim.val=c(0,0.05);by.y=0.025;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(subset(GM.dat.sumstats,param=="TP")$mean.val,ylim=ylim.val,
          space=0,axes=F,ann=F,col=adjustcolor(c("indianred1","dodgerblue1"),0.5))
with(subset(GM.dat.sumstats,param=="TP"),errorbars(x,mean.val,sd.val,"black"))
axis_fun(2,ymaj,ymin,ymaj*1000)
axis_fun(1,x,x,c("Inflow","Outflow"));box(lwd=1)
mtext(side=2,line=2.5,"Avg Anunal GM TP (\u03BCg L\u207B\u00b9)")
# mtext(side=3,adj=0,line=-1," mean \u00B1 sd",font=3)
mtext(side=3,adj=0,"10 mile Canal Filter Marsh\n(Lee County)",font=3)
mtext(side=3,adj=0,line=-1," FLWY 2014 - 2021",font=3,cex=0.75)

ylim.val=c(0,1.5);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(subset(GM.dat.sumstats,param=="TN")$mean.val,ylim=ylim.val,
          space=0,axes=F,ann=F,col=adjustcolor(c("indianred1","dodgerblue1"),0.5))
with(subset(GM.dat.sumstats,param=="TN"),errorbars(x,mean.val,sd.val,"black"))
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,x,x,c("Inflow","Outflow"));box(lwd=1)
mtext(side=2,line=2.5,"Avg Anunal GM TN (mg L\u207B\u00b9)")
mtext(side=1,line=1.0,outer=T,"Monitoring Location")
dev.off()