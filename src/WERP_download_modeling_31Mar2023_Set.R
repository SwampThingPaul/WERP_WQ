## Download WERP Modeling
##
## Code was compiled by Paul Julian
## contact info: pjulian@sccf.org

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(RCurl)

## Paths
wd="C:/Julian_LaCie/_GitHub/WERP_WQ"
data.path=paste0(wd,"/Data/20230331")
# Folder.Maker(paste0(data.path,c("/DMSTA","/RSMBN","/RSMGL")))


# -------------------------------------------------------------------------
link="ftp://ftppub.sfwmd.gov/outgoing/WesternEverglades/31Mar2023_SetA/rsmgl_model_output/"

result=getURL(link)

result2=strsplit(result, "\r*\n")[[1]]
result2=strsplit(result2,"\\s+")

alts1=sapply(strsplit(strsplit(result, "\r*\n")[[1]],"\\s+"),"[",9)
alts1=alts1[!(alts1%in%c(".",".."))]

# Folder.Maker(paste(data.path,alts1,sep="/"))
## RSMGL_CEPP_output
dss.mod="RSMGL_CEPP_output.dss"
for(i in 1:length(alts1)){
  url=paste0(link,alts1[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts1[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

dss.mod="RSMGL_CEPP_TT.dss"
for(i in 1:length(alts1)){
  url=paste0(link,alts1[i],"/",dss.mod)
  dest=paste(data.path,"/",alts1[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

dss.mod="transect_flows.dss"
for(i in 1:length(alts1)){
  url=paste0(link,alts1[i],"/",dss.mod)
  dest=paste(data.path,"/",alts1[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

# dss.mod="globalmonitors.nc"
for(i in 2:length(alts1)){
  rslt=getURL(paste0(link,alts1[i],"/"))
  result2=strsplit(strsplit(rslt, "\r*\n")[[1]],"\\s+")
  result2=sapply(result2,"[",9)
  dss.mod=result2[grepl("global",result2)]
  
  url=paste0(link,alts1[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts1[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

# dss.mod="globalmonitors.nc"
for(i in 2:length(alts1)){
  rslt=getURL(paste0(link,alts1[i],"/"))
  result2=strsplit(strsplit(rslt, "\r*\n")[[1]],"\\s+")
  result2=sapply(result2,"[",9)
  dss.mod=result2[grepl("wbbudget",result2)]
  
  url=paste0(link,alts1[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts1[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}


link="ftp://ftppub.sfwmd.gov/outgoing/WesternEverglades/31Mar2023_SetA/PM_WECBRR_WFWOR_WALT1R_WALT3R_W3RNL_ALTHR/WERP_Phosphorus_Dynamics/"
result=getURL(link)
result2=strsplit(result, "\r*\n")[[1]]
result2=strsplit(result2,"\\s+")
# Folder.Maker(paste(data.path,"WERP_Phosphorus_Dynamics",sep="/"))

file=sapply(strsplit(strsplit(result, "\r*\n")[[1]],"\\s+"),"[",9)
file=file[!(file%in%c(".",".."))]
for(i in 1:length(file)){
  url=paste0(link,file[i])
  dest=paste0(data.path,"/WERP_Phosphorus_Dynamics/",file[i])
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}


# -------------------------------------------------------------------------
link="ftp://ftppub.sfwmd.gov/outgoing/WesternEverglades/31Mar2023_SetB/rsmgl_model_output/"

result=getURL(link)

result2=strsplit(result, "\r*\n")[[1]]
result2=strsplit(result2,"\\s+")

alts2=sapply(strsplit(strsplit(result, "\r*\n")[[1]],"\\s+"),"[",9)
alts2=alts2[!(alts2%in%c(".",".."))]

alts2=alts2[!alts2%in%alts1]

# Folder.Maker(paste(data.path,alts2,sep="/"))
## RSMGL_CEPP_output
dss.mod="RSMGL_CEPP_output.dss"
for(i in 1:length(alts2)){
  url=paste0(link,alts2[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts2[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

dss.mod="RSMGL_CEPP_TT.dss"
for(i in 1:length(alts2)){
  url=paste0(link,alts2[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts2[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

dss.mod="transect_flows.dss"
for(i in 1:length(alts2)){
  url=paste0(link,alts2[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts2[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

# dss.mod="globalmonitors.nc"
for(i in 1:length(alts2)){
  rslt=getURL(paste0(link,alts2[i],"/"))
  result2=strsplit(strsplit(rslt, "\r*\n")[[1]],"\\s+")
  result2=sapply(result2,"[",9)
  dss.mod=result2[grepl("global",result2)]
  
  url=paste0(link,alts2[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts2[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

# dss.mod="globalmonitors.nc"
for(i in 1:length(alts2)){
  rslt=getURL(paste0(link,alts2[i],"/"))
  result2=strsplit(strsplit(rslt, "\r*\n")[[1]],"\\s+")
  result2=sapply(result2,"[",9)
  dss.mod=result2[grepl("wbbudget",result2)]
  
  url=paste0(link,alts2[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts2[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}



link="ftp://ftppub.sfwmd.gov/outgoing/WesternEverglades/31Mar2023_SetB/PM_WECBRR_WFWOR_ALTHR_ALTHNF/WERP_Phosphorus_Dynamics/"
result=getURL(link)
result2=strsplit(result, "\r*\n")[[1]]
result2=strsplit(result2,"\\s+")
# Folder.Maker(paste(data.path,"WERP_Phosphorus_Dynamics",sep="/"))

file2=sapply(strsplit(strsplit(result, "\r*\n")[[1]],"\\s+"),"[",9)
file2=file2[!(file2%in%c(".",".."))]

file2=file2[!(file2%in%file)]

for(i in 1:length(file2)){
  url=paste0(link,file2[i])
  dest=paste0(data.path,"/WERP_Phosphorus_Dynamics/",file2[i])
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}



# -------------------------------------------------------------------------
link="ftp://ftppub.sfwmd.gov/outgoing/WesternEverglades/31Mar2023_SetC/rsmgl_model_output/"

result=getURL(link)

result2=strsplit(result, "\r*\n")[[1]]
result2=strsplit(result2,"\\s+")

alts3=sapply(strsplit(strsplit(result, "\r*\n")[[1]],"\\s+"),"[",9)
alts3=alts3[!(alts3%in%c(".",".."))]

alts3=alts3[!alts3%in%c(alts2,alts1)]


# Folder.Maker(paste(data.path,alts3,sep="/"))
## RSMGL_CEPP_output
dss.mod="RSMGL_CEPP_output.dss"
for(i in 1:length(alts3)){
  url=paste0(link,alts3[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts3[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

dss.mod="RSMGL_CEPP_TT.dss"
for(i in 1:length(alts3)){
  url=paste0(link,alts3[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts3[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

dss.mod="transect_flows.dss"
for(i in 1:length(alts3)){
  url=paste0(link,alts3[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts3[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

# dss.mod="globalmonitors.nc"
for(i in 1:length(alts3)){
  rslt=getURL(paste0(link,alts3[i],"/"))
  result2=strsplit(strsplit(rslt, "\r*\n")[[1]],"\\s+")
  result2=sapply(result2,"[",9)
  dss.mod=result2[grepl("global",result2)]
  
  url=paste0(link,alts3[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts3[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

for(i in 1:length(alts3)){
  rslt=getURL(paste0(link,alts3[i],"/"))
  result2=strsplit(strsplit(rslt, "\r*\n")[[1]],"\\s+")
  result2=sapply(result2,"[",9)
  dss.mod=result2[grepl("wbbudget",result2)]
  
  url=paste0(link,alts3[i],"/",dss.mod)
  dest=paste0(data.path,"/",alts3[i],"/",dss.mod)
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}


link="ftp://ftppub.sfwmd.gov/outgoing/WesternEverglades/31Mar2023_SetC/PM_WECBRR_IORBLR_ALTHNF/WERP_Phosphorus_Dynamics/"
result=getURL(link)
result2=strsplit(result, "\r*\n")[[1]]
result2=strsplit(result2,"\\s+")
# Folder.Maker(paste(data.path,"WERP_Phosphorus_Dynamics",sep="/"))

file3=sapply(strsplit(strsplit(result, "\r*\n")[[1]],"\\s+"),"[",9)
file3=file3[!(file3%in%c(".",".."))]

file3=file3[!(file3%in%c(file,file2))]

for(i in 1:length(file3)){
  url=paste0(link,file3[i])
  dest=paste0(data.path,"/WERP_Phosphorus_Dynamics/",file3[i])
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}
