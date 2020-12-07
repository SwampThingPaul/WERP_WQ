## 
## Werp WQ
## Abiaki Discharge simulation exercise
##
## Code was compiled by Paul Julian
## contact info: paul.julian@floridadep.gov

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
# Data Wrangling
library(AnalystHelper);
library(zoo)


# -------------------------------------------------------------------------
set.seed(123)

val=19
uncert=val*0.3

iter=100
ann.conc=NA
for(i in 1:iter){
  ann.conc[i]=rnorm(1,val,uncert)
}

plot(ann.conc)
ann.sim=data.frame(sim=round(ann.conc,0))
ann.sim$ann.exceed=with(ann.sim,ifelse(sim<19,1,0))
ann.sim$LT.exceed=with(ann.sim,ifelse(sim<13,1,0))
ann.sim$LT.exceed5=with(ann.sim,c(rep(NA,4),rollsum(LT.exceed,5)))
ann.sim
with(ann.sim,ifelse(ann.exceed>0&LT.exceed5>=3,1,0))

xlim.val=c(0,101);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,35);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

par(family="serif",mar=c(2,2,1,1),oma=c(2,2,0.5,0.5))
plot(ann.conc,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F,xaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(ann.sim,pt_line(1:100,sim,2,"dodgerblue1",1,21,"dodgerblue1"))
abline(h=c(13,19),lty=1,col="indianred1")
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,paste0("Simulated values based on ",val," \u03BCg L\u207B\u00B9 and 30% varability"))
mtext(side=1,line=2,"Simulated Time (Years)")         
mtext( side=2,line=2,"TP FWM (\u03BCg L\u207B\u00B9)")


# set.seed(123)
val=15
uncert=val*0.3

iter=100
ann.conc=NA
for(i in 1:iter){
  ann.conc[i]=rnorm(1,val,uncert)
}

plot(ann.conc)
ann.sim=data.frame(sim=round(ann.conc,0))
ann.sim$ann.exceed=with(ann.sim,ifelse(sim<19,1,0))
ann.sim$LT.exceed=with(ann.sim,ifelse(sim<13,1,0))
ann.sim$LT.exceed5=with(ann.sim,c(rep(NA,4),rollsum(LT.exceed,5)))
ann.sim
ann.sim$QBEL.pass=with(ann.sim,ifelse(ann.exceed>0&LT.exceed5>=3,1,0))

xlim.val=c(0,101);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,35);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

par(family="serif",mar=c(2,2,1,1),oma=c(2,2,0.5,0.5))
plot(ann.conc,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F,xaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(ann.sim,pt_line(1:100,sim,2,"dodgerblue1",1,21,"dodgerblue1"))
abline(h=c(13,19),lty=1,col="indianred1")
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,paste0("Simulated values based on ",val," \u03BCg L\u207B\u00B9 and 30% varability"))
mtext(side=1,line=2,"Simulated Time (Years)")         
mtext( side=2,line=2,"TP FWM (\u03BCg L\u207B\u00B9)")
