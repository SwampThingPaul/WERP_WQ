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
data.path=paste0(wd,"/Data")
# Folder.Maker(paste0(data.path,c("/DMSTA","/RSMBN","/RSMGL")))

# -------------------------------------------------------------------------
link="ftp://ftppub.sfwmd.gov/outgoing/WesternEverglades/31Jul2020_Set1/"

result=getURL(link)

result2=strsplit(result, "\r*\n")[[1]]
result2=strsplit(result2,"\\s+")

## RSMBN 
result=getURL(paste0(link,"rsmbn_model_output/"))
strsplit(strsplit(result, "\r*\n")[[1]],"\\s+")

alts=c("WALT3R","WECB","WFWO")
Folder.Maker(paste0(data.path,"/RSMBN/",alts))
for(i in 1:length(alts)){
  
  url=paste0(link,"rsmbn_model_output/",alts[i],"/RSMBN_output.dss")
  dest=paste0(data.path,"/RSMBN/",alts[i],"/RSMBN_output.dss")
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}

# RSMGL
result=getURL(paste0(link,"rsmgl_model_output/"))
strsplit(strsplit(result, "\r*\n")[[1]],"\\s+")
alts=c("ALTHR","WALT1R","WALT3R","WALT3RNL","WECB","WFWO")
Folder.Maker(paste0(data.path,"/RSMGL/",alts))
for(i in 1:length(alts)){
  
  url=paste0(link,"rsmgl_model_output/",alts[i],"/RSMGL_CEPP_output.dss")
  dest=paste0(data.path,"/RSMGL/",alts[i],"/RSMGL_CEPP_output.dss")
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}


# DMSTA
result=getURL(paste0(link,"DMSTA/"))
tmp=strsplit(strsplit(result, "\r*\n")[[1]],"\\ ")

alts=sapply(tmp,"[",26)[!(sapply(tmp,"[",26)%in%c(".","..",NA))]
## Transfered by hand


# set2 --------------------------------------------------------------------



link="ftp://ftppub.sfwmd.gov/outgoing/WesternEverglades/31Jul2020_Set2/"

result=getURL(link)

result2=strsplit(result, "\r*\n")[[1]]
result2=strsplit(result2,"\\s+")
result2
sapply(result2,"[",9)

## RSMBN 
result=getURL(paste0(link,"rsmbn_model_output/"))
strsplit(strsplit(result, "\r*\n")[[1]],"\\s+")

alts=c("IORBL","WECBR")
Folder.Maker(paste0(data.path,"/RSMBN/",alts))
## folders empty, transfered README 

# RSMGL
result=getURL(paste0(link,"rsmgl_model_output/"))
strsplit(strsplit(result, "\r*\n")[[1]],"\\s+")

Folder.Maker(paste0(data.path,"/RSMGL/",alts))
for(i in 1:length(alts)){
  
  url=paste0(link,"rsmgl_model_output/",alts[i],"/RSMGL_CEPP_output.dss")
  dest=paste0(data.path,"/RSMGL/",alts[i],"/RSMGL_CEPP_output.dss")
  download.file(url,dest,mode="wb",cacheOK = F)
  print(i)
}
