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
library(plyr)

# -------------------------------------------------------------------------
set.seed(123)

val=19
uncert=val*0.3

# Normal Distribution
iter=100
ann.conc=rnorm(iter,val,uncert)

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


#uniform
ann.conc.uni=runif(iter,val-uncert,val+uncert)

plot(ann.conc.uni)
ann.sim.uni=data.frame(sim=round(ann.conc.uni,0))
ann.sim.uni$ann.exceed=with(ann.sim.uni,ifelse(sim<19,1,0))
ann.sim.uni$LT.exceed=with(ann.sim.uni,ifelse(sim<13,1,0))
ann.sim.uni$LT.exceed5=with(ann.sim.uni,c(rep(NA,4),rollsum(LT.exceed,5)))
ann.sim.uni
with(ann.sim.uni,ifelse(ann.exceed>0&LT.exceed5>=3,1,0))

xlim.val=c(0,101);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,35);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

par(family="serif",mar=c(2,2,1,1),oma=c(2,2,0.5,0.5))
plot(ann.sim.uni$sim,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F,xaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(ann.sim.uni,pt_line(1:100,sim,2,"dodgerblue1",1,21,"dodgerblue1"))
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
ann.conc==rnorm(1,val,uncert)

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

## Monte Carlo type analysis

val=19
uncert=val*0.3

iter=100
ann.conc.MC=data.frame()

for(j in 1:50){
  ann.conc=rnorm(iter,val,uncert)
  ann.conc.MC=rbind(ann.conc.MC,data.frame(sim.time=1:100,sim=ann.conc,MC.iter=rep(j,iter)))
}


plot(sim~sim.time,ann.conc.MC)

ann.conc.MC$ann.exceed=with(ann.conc.MC,ifelse(sim<19,1,0))
ann.conc.MC$LT.exceed=with(ann.conc.MC,ifelse(sim<13,1,0))
ann.conc.MC$LT.exceed5=with(ann.conc.MC,ave(LT.exceed,MC.iter,FUN=function(x)c(rep(NA,4),rollsum(x,5))))
ann.conc.MC$QBEL.pass=with(ann.conc.MC,ifelse(ann.exceed>0&LT.exceed5>=3,1,0))

# QBEL.pass=ddply(ann.conc.MC,"MC.iter",summarise,N.val=sum(QBEL.pass,na.rm=T))
QBEL.pass=ddply(ann.conc.MC,"MC.iter",summarise,N.val=sum(LT.exceed,na.rm=T))
mean(QBEL.pass$N.val)

plot(N.val~MC.iter,QBEL.pass,ylim=c(0,100),type="b")

test=ddply(ann.conc.MC,"sim.time",summarise,min.val=min(sim,na.rm=T),max.val=max(sim,na.rm=T),mean.val=mean(sim,na.rm=T))

plot(mean.val~sim.time,test,ylim=c(0,30))
with(test,shaded.range(sim.time,min.val,max.val,"grey",lty=1))
