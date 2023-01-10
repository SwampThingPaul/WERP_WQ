## Libraries
# Data Wrangling
library(AnalystHelper);
library(plyr)
library(reshape)
library(openxlsx)


dates=date.fun(c("1977-05-01","2016-05-01"))
parameters=data.frame(Test.Number=c(25,23,18,21,80,8,7,9,33),Param=c("TP","OP","NOx","TKN","TN","DO.mgL","Temp","SPC","SO4"))
wq.sites2=data.frame(Station.ID=c(c('BCWQA1','BCWQA2','BCWQA17'),c('BCWQA18','BCWQA12'),c('BCWQA3','BCWQA4','BCWQA13','BCWQA14','BCWQA16'),c('BCWQA21','BCWQA15','BCWQA13A','BCWQA5','BCWQA6'),c('BCWQA8','BCWQA7','BCWQA11','BCWQA19'),c('BCWQA9A','BCWQA9','BCWQA10','BCWQA20')),
                     Region=c(rep('NW',3),rep('NE',2),rep('CW',5),rep('CE',5),rep('SW',4),rep('SE',4)),Region2=c(rep("N_C",15),rep("S",8)))
wq.dat.BCNP=data.frame()
for(i in 1:nrow(wq.sites2)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],wq.sites2$Station.ID[i],33)
  tmp$Sigfig.Value=as.numeric(tmp$Sigfig.Value)
  wq.dat.BCNP=rbind(tmp,wq.dat.BCNP)
}

range(wq.dat.BCNP$HalfMDL)
x=boxplot(wq.dat.BCNP$HalfMDL,outline=F)
subset(wq.dat.BCNP,HalfMDL>2.5)

wq.dat.BCNP=merge(wq.dat.BCNP,parameters,"Test.Number")
wq.dat.xtab.BCNP=cast(subset(wq.dat.BCNP,Collection.Method%in%c("G","GP")),DateTime.EST+Date.EST+Collection.Method+Station.ID~Param,value="HalfMDL",mean)
wq.dat.xtab.BCNP$WY=with(wq.dat.xtab.BCNP,WY(DateTime.EST))

SO4.AGM=ddply(wq.dat.xtab.BCNP,c("Station.ID","WY"),summarise,N=N.obs(SO4),Geomean=exp(mean(log(SO4),na.rm=T)),sd=sd(SO4,na.rm=T),SE.GM=Geomean*(sd(log(SO4),na.rm=T)/sqrt(N.obs(SO4)-1)));
subset(SO4.AGM,WY!=2014)
ddply(SO4.AGM,c("Station.ID","WY"),summarise,mean.val=mean(Geomean));

range(subset(SO4.AGM,WY!=2012)$Geomean)
