##[QUES]=group
##Wdir=folder
##location=string
##init.date=string
##final.date=string
##end.rch=number 10
##passfilenames
##showplots

library(plyr)
library(lubridate)
library(foreign)
library(ggplot2)
library(reshape)
library(pastecs)
library(plyr)
library(lubridate)
library(foreign)
library(rtf)

workd<-Wdir
setwd(workd)
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#Step 5: Read observed particular sub-basin water balance
sub.out<-read.table(paste(workd,"/output.sub", sep=''), skip=9)
rch.out<-read.table(paste(workd,"/output.rch", sep=''), skip=9)
mod.date<-as.character(seq(as.Date(init.date, "%d/%m/%Y"), as.Date(final.date, "%d/%m/%Y"), "days")); #create date sequence
YEAR<-year(mod.date)
DAY<-yday(mod.date)
NSUB<-max(rch.out$V2, na.rm=T);#number of sub-basin
tot.area<-rch.out[rch.out$V2==end.rch,5]


#create sub-basin level data table function
get.sub.params<-function(subas){
  precip.sub.out<-sub.out[sub.out$V2==subas,5]
  pet.sub.out<-sub.out[sub.out$V2==subas,7]
  et.sub.out<-sub.out[sub.out$V2==subas,8]
  perc.sub.out<-sub.out[sub.out$V2==subas,10]
  surq.sub.out<-sub.out[sub.out$V2==subas,11]
  gwq.sub.out<-sub.out[sub.out$V2==subas,12]
  latq.sub.out<-sub.out[sub.out$V2==subas,20]
  wyld.sub.out<-sub.out[sub.out$V2==subas,13]
  syld.sub.out<-sub.out[sub.out$V2==subas,14]
  eval(parse(text=(paste("sub.out.params.",subas,"<-as.data.frame(cbind(YEAR,DAY,precip.sub.out,pet.sub.out,et.sub.out,perc.sub.out,surq.sub.out,gwq.sub.out,latq.sub.out,wyld.sub.out,syld.sub.out))", sep=""))))
}

#Create all sub-basin level data table (loop function)
x<-1
while(x<=NSUB)
{
  eval(parse(text=(paste("sub.out.params.",x,"<-get.sub.params(",x,")", sep=""))))
  x<-x+1
}

#calculate total surface quick flow, soil quick flow, and base flow for basin level indicator
y<-1
basin.precip<-sub.out.params.1[3]
basin.surface<-sub.out.params.1[7]
basin.base<-sub.out.params.1[8]
basin.soil<-sub.out.params.1[9]

#.rch param, default in cms unit
outflow.rch.out<-(((rch.out[rch.out$V2==end.rch,7])*24*3600)/(tot.area[1]*(10^6)))*1000; #conversion from cms to mm
sed.rch.out<-rch.out[rch.out$V2==end.rch,10]

while(y<NSUB)
{
  y<-y+1
  basin.precip<-(basin.precip)+(eval(parse(text=(paste("sub.out.params.",y,"[3]", sep="")))))
  basin.surface<-(basin.surface)+(eval(parse(text=(paste("sub.out.params.",y,"[7]", sep="")))))
  basin.base<-(basin.base)+(eval(parse(text=(paste("sub.out.params.",y,"[8]", sep="")))))
  basin.soil<-(basin.soil)+(eval(parse(text=(paste("sub.out.params.",y,"[9]", sep="")))))
}


#data compilation for "basin level" criteria & indicators input
basin.a<-as.data.frame(cbind(YEAR,DAY,tot.area,(basin.precip/NSUB),(basin.surface/NSUB), (basin.base/NSUB), (basin.soil/NSUB),outflow.rch.out, sed.rch.out))
colnames(basin.a)<-c("YEAR", "DAY","area","precip", "surfq", "gwq", "latq", "outflow","sed_out")


period1<- min(basin.a[1])
period2<-max(basin.a[1])


get.bas.indicators<-function(basin.data){
  #PIVOT TABLE: MELT & CAST
  #1create actual rainfall sheet
  rain.m<- melt(basin.data, id=c(1:2), measure=c(4))
  rain.m$value<- as.numeric(as.character(rain.m$value))
  rain.m$day<- as.numeric(as.character(rain.m$DAY))
  rain.c<-cast(rain.m, DAY ~ YEAR, sum)
  stat.rain<-stat.desc(rain.c)
  
  #2create outflow sheet
  mdebit.m<- melt(basin.data, id=c(1:2), measure=c(8))
  mdebit.m$value<- as.numeric(as.character(mdebit.m$value))
  mdebit.m$day<- as.numeric(as.character(mdebit.m$DAY))
  mdebit.c<-cast(mdebit.m, DAY ~ YEAR, sum)
  stat.mdebit<-stat.desc(mdebit.c)
  
  #3create base flow sheet
  bflow.m<- melt(basin.data, id=c(1:2), measure=c(6))
  bflow.m$value<- as.numeric(as.character(bflow.m$value))
  bflow.m$day<- as.numeric(as.character(bflow.m$DAY))
  bflow.c<-cast(bflow.m, DAY ~ YEAR)
  stat.bflow<-stat.desc(bflow.c)
  
  #4create SoilQFlow sheet
  sqflow.m<- melt(basin.data, id=c(1:2), measure=c(7))
  sqflow.m$value<- as.numeric(as.character(sqflow.m$value))
  sqflow.m$day<- as.numeric(as.character(sqflow.m$DAY))
  sqflow.c<-cast(sqflow.m, DAY ~ YEAR)
  stat.sqflow<-stat.desc(sqflow.c)
  
  #5create Runoff sheet
  runoff.m<- melt(basin.data, id=c(1:2), measure=c(5))
  runoff.m$value<- as.numeric(as.character(runoff.m$value))
  runoff.m$day<- as.numeric(as.character(runoff.m$DAY))
  runoff.c<-cast(runoff.m, DAY ~ YEAR)
  stat.runoff<-stat.desc(runoff.c)
  
  
  #8create Sediment Yield sheet
  sed.out.m<- melt(basin.data, id=c(1:2), measure=c(9))
  sed.out.m$value<- as.numeric(as.character(sed.out.m$value))
  sed.out.m$day<- as.numeric(as.character(sed.out.m$DAY))
  sed.out.c<-cast(sed.out.m, DAY ~ YEAR)
  stat.sed.out<-stat.desc(sed.out.c)
  
  
  #9calculate Precipitation above average
  rain.above<-as.data.frame(1:366, header=T)
  colnames(rain.above)<-"DAY"
  
  period.count<-(ncol(rain.c))
  var.rain<-2
  
  while(var.rain <= period.count)
  {
    rain.bol<-(rain.c[var.rain]-(stat.rain[9,var.rain]))
    rain.multiply<-ifelse(rain.bol>0,1,0)
    rain.bol<-rain.bol*rain.multiply
    rain.above<-cbind(rain.above, rain.bol); # row binding
    var.rain<-(var.rain+1); # Increment counter
    
  }
  stat.rain.above<-stat.desc(rain.above)
  
  #10calculate measured debit above average
  mdebit.above<-as.data.frame(1:366, header=T)
  colnames(mdebit.above)<-"DAY"
  
  period.count<-(ncol(mdebit.c))
  var.mdebit<-2
  
  while(var.mdebit <= period.count)
  {
    mdebit.bol<-(mdebit.c[var.mdebit]-(stat.mdebit[9,var.mdebit])) #difference of measured debit and annual mean debit
    mdebit.multiply<-ifelse(mdebit.bol>0,1,0) #draw boolean value, filter for value below 0
    mdebit.bol<-mdebit.bol*mdebit.multiply #filtering data, positive above average remains
    mdebit.above<-cbind(mdebit.above, mdebit.bol); # row binding
    var.mdebit<-(var.mdebit+1); # Increment counter
    
  }
  stat.mdebit.above<-stat.desc(mdebit.above)
  
  #CREATE MONTHLY PRECIPITATION SUMMARY DATA TABLE
  init.year<-as.numeric(colnames(rain.c[2]))
  period.count<-(ncol(rain.c)-1)
  final.year<-init.year+period.count-1
  
  rain.month<-as.data.frame(1:12, header=T)
  colnames(rain.month)<-"month.number"
  
  rain.year<-2
  while(rain.year <= (period.count+1))
  {
    annual.sum.rain<-as.data.frame(c(sum(rain.c[1:31,rain.year]),sum(rain.c[32:59,rain.year]),sum(rain.c[60:90,rain.year]),sum(rain.c[91:120,rain.year]),sum(rain.c[121:151,rain.year]),sum(rain.c[152:181,rain.year]),sum(rain.c[182:212,rain.year]),sum(rain.c[213:243,rain.year]),sum(rain.c[244:273,rain.year]),sum(rain.c[274:304,rain.year]),sum(rain.c[305:334,rain.year]),sum(rain.c[335:365,rain.year])), header=T)
    colnames(annual.sum.rain)<-as.character(init.year+rain.year-2)
    rain.month<-cbind(rain.month, annual.sum.rain)
    rain.year<-rain.year+1
  }
  
  #CREATE MONTHLY PREDICTED DEBIT SUMMARY DATA TABLE
  init.year<-as.numeric(colnames(mdebit.c[2]))
  period.count<-(ncol(mdebit.c)-1)
  final.year<-init.year+period.count-1
  
  mdebit.month<-as.data.frame(1:12, header=T)
  colnames(mdebit.month)<-"month.number"
  mdebit.year<-2
  
  while(mdebit.year <= (period.count+1))
  {
    annual.sum.mdebit<-as.data.frame(c(sum(mdebit.c[1:31,mdebit.year]),sum(mdebit.c[32:59,mdebit.year]),sum(mdebit.c[60:90,mdebit.year]),sum(mdebit.c[91:120,mdebit.year]),sum(mdebit.c[121:151,mdebit.year]),sum(mdebit.c[152:181,mdebit.year]),sum(mdebit.c[182:212,mdebit.year]),sum(mdebit.c[213:243,mdebit.year]),sum(mdebit.c[244:273,mdebit.year]),sum(mdebit.c[274:304,mdebit.year]),sum(mdebit.c[305:334,mdebit.year]),sum(mdebit.c[335:365,mdebit.year])), header=T)
    colnames(annual.sum.mdebit)<-as.character(init.year+mdebit.year-2)
    mdebit.month<-cbind(mdebit.month, annual.sum.mdebit)
    mdebit.year<-mdebit.year+1
  }
  
  stat.mdebit.month<-stat.desc(mdebit.month)
  
  #CREATE AUXILLARY VARIABLE TABLE
  #PREDICTED
  aux.val.pr<-as.data.frame( stat.rain[7,], header=T);#add ptot
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit[7,]); #add qtot predicted
  aux.val.pr<-rbind(aux.val.pr, stat.rain.above[7,]); #add precipitation above average
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit.above[7,]); #add qtot predicted above average
  aux.val.pr<-rbind(aux.val.pr, stat.rain[5,]); #add max daily precipitation
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit[5,]); #add max qtot predicted
  colnames(aux.val.pr)[1]<-"month.number"; # match column names
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit.month[5,]); #add monthly max qtot predicted
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit.month[4,]); #add monthly min qtot predicted
  colnames(aux.val.pr)[1]<-"DAY"; # match column names
  
  aux.val.pr<-rbind(aux.val.pr, stat.runoff[7,]); #add total runoff
  aux.val.pr<-rbind(aux.val.pr, stat.sqflow[7,]); #add total
  aux.val.pr<-rbind(aux.val.pr, stat.sed.out[7,]);
  rownames(aux.val.pr) <- c("Ptot","Qtot (simulated)", "PabAvg", "Qabavg", "PdailyMax", "QdailyMax", "QmonthlyMax", "QmonthlyMin", "SumRO", "SumSoilQFlow", "tot.sed.out")
  
  #PREDICTED DATA INDICATORS
  pr.TotDischargeFrac.pred<-aux.val.pr[2,]/aux.val.pr[1,]
  pr.BufferingIndicator<-(1-(aux.val.pr[4,]/aux.val.pr[3,]))
  pr.RelBufferingIndicator<-(1-((aux.val.pr[4,]/aux.val.pr[2,])/(aux.val.pr[3,]/aux.val.pr[1,])))
  pr.BufferingPeak<-(1-(((aux.val.pr[6,]-aux.val.pr[2,])/365)/((aux.val.pr[5,]-aux.val.pr[1,])/365)))
  pr.HighestMonthFrac<-(12*(aux.val.pr[7,]/aux.val.pr[2,]))
  pr.OverlandFlowFrac<-aux.val.pr[9,]/aux.val.pr[1,]
  pr.SoilQflowFrac<-aux.val.pr[10,]/aux.val.pr[1,]
  pr.SlowFlowFrac_Meth1<-(pr.TotDischargeFrac.pred-pr.OverlandFlowFrac-pr.SoilQflowFrac)
  pr.LowestMonthFrac<-(12*(aux.val.pr[8,]/aux.val.pr[2,]))
  pr.sed.out<-sum(aux.val.pr[11,])
  
  #CREATE TABLE
  indicator.pre<-as.data.frame(pr.TotDischargeFrac.pred, header=T)
  indicator.pre<-rbind(indicator.pre, pr.RelBufferingIndicator)
  indicator.pre<-rbind(indicator.pre, pr.BufferingIndicator)
  indicator.pre<-rbind(indicator.pre, pr.BufferingPeak)
  indicator.pre<-rbind(indicator.pre, pr.HighestMonthFrac)
  indicator.pre<-rbind(indicator.pre, pr.OverlandFlowFrac)
  indicator.pre<-rbind(indicator.pre, pr.SoilQflowFrac)
  indicator.pre<-rbind(indicator.pre, pr.SlowFlowFrac_Meth1)
  indicator.pre<-rbind(indicator.pre, pr.LowestMonthFrac)
  indicator.pre<-rbind(indicator.pre, pr.sed.out)
  rownames(indicator.pre) <- c("TotDischargeFrac.pre","RelBufferingIndicator", "BufferingIndicator", "BufferingPeak",
                               "HighestMonthFrac", "OverlandFlowFrac","SoilQflowFrac","SlowFlowFrac_Meth1","LowestMonthFrac","tot.sed.out")
  indicator.pre[1]<-NULL
  indicator.pre<-t(indicator.pre)
  return(indicator.pre)
}

basin.data.final<-as.data.frame(get.bas.indicators(basin.a))
basin.years<-rownames(basin.data.final)
basin.data.final<-cbind(basin.years,basin.data.final)
basin.data.final[,2:10]<-round(basin.data.final[,2:10], digits=2)
basin.data.final[,11]<-round(basin.data.final[,11])
file_name<-paste("basin_data_final.dbf",sep="")
write.dbf(basin.data.final, file=file_name, factor2char = TRUE)


get.sub.indicators<-function(new.sub.out){
  #identify sub_basin
  
  #PIVOT TABLE: MELT & CAST
  #1create actual rainfall sheet
  rain.m<- melt(new.sub.out, id=c(1:2), measure=c(3))
  rain.m$value<- as.numeric(as.character(rain.m$value))
  rain.m$day<- as.numeric(as.character(rain.m$DAY))
  rain.c<-cast(rain.m, DAY ~ YEAR, sum)
  stat.rain<-stat.desc(rain.c)
  
  #2create predicted water yield sheet
  mdebit.m<- melt(new.sub.out, id=c(1:2), measure=c(10))
  mdebit.m$value<- as.numeric(as.character(mdebit.m$value))
  mdebit.m$day<- as.numeric(as.character(mdebit.m$DAY))
  mdebit.c<-cast(mdebit.m, DAY ~ YEAR, sum)
  stat.mdebit<-stat.desc(mdebit.c)
  
  #3create base flow sheet
  bflow.m<- melt(new.sub.out, id=c(1:2), measure=c(8))
  bflow.m$value<- as.numeric(as.character(bflow.m$value))
  bflow.m$day<- as.numeric(as.character(bflow.m$DAY))
  bflow.c<-cast(bflow.m, DAY ~ YEAR)
  stat.bflow<-stat.desc(bflow.c)
  
  #4create SoilQFlow sheet
  sqflow.m<- melt(new.sub.out, id=c(1:2), measure=c(9))
  sqflow.m$value<- as.numeric(as.character(sqflow.m$value))
  sqflow.m$day<- as.numeric(as.character(sqflow.m$DAY))
  sqflow.c<-cast(sqflow.m, DAY ~ YEAR)
  stat.sqflow<-stat.desc(sqflow.c)
  
  #5create Runoff sheet
  runoff.m<- melt(new.sub.out, id=c(1:2), measure=c(7))
  runoff.m$value<- as.numeric(as.character(runoff.m$value))
  runoff.m$day<- as.numeric(as.character(runoff.m$DAY))
  runoff.c<-cast(runoff.m, DAY ~ YEAR)
  stat.runoff<-stat.desc(runoff.c)
  
  #6create percolation sheet
  perc.m<- melt(new.sub.out, id=c(1:2), measure=c(6))
  perc.m$value<- as.numeric(as.character(perc.m$value))
  perc.m$day<- as.numeric(as.character(perc.m$DAY))
  perc.c<-cast(perc.m, DAY ~ YEAR)
  stat.perc<-stat.desc(perc.c)
  
  #7create Evapotranspiration sheet
  et.m<- melt(new.sub.out, id=c(1:2), measure=c(5))
  et.m$value<- as.numeric(as.character(et.m$value))
  et.m$day<- as.numeric(as.character(et.m$DAY))
  et.c<-cast(et.m, DAY ~ YEAR)
  stat.et<-stat.desc(et.c)
  
  #8create Sediment Yield sheet
  syld.m<- melt(new.sub.out, id=c(1:2), measure=c(11))
  syld.m$value<- as.numeric(as.character(syld.m$value))
  syld.m$day<- as.numeric(as.character(syld.m$DAY))
  syld.c<-cast(syld.m, DAY ~ YEAR)
  stat.syld<-stat.desc(syld.c)
  
  
  #9calculate Precipitation above average
  rain.above<-as.data.frame(1:366, header=T)
  colnames(rain.above)<-"DAY"
  
  period.count<-(ncol(rain.c))
  var.rain<-2
  
  while(var.rain <= period.count)
  {
    rain.bol<-(rain.c[var.rain]-(stat.rain[9,var.rain]))
    rain.multiply<-ifelse(rain.bol>0,1,0)
    rain.bol<-rain.bol*rain.multiply
    rain.above<-cbind(rain.above, rain.bol); # row binding
    var.rain<-(var.rain+1); # Increment counter
    
  }
  stat.rain.above<-stat.desc(rain.above)
  
  #10calculate measured debit above average
  mdebit.above<-as.data.frame(1:366, header=T)
  colnames(mdebit.above)<-"DAY"
  
  period.count<-(ncol(mdebit.c))
  var.mdebit<-2
  
  while(var.mdebit <= period.count)
  {
    mdebit.bol<-(mdebit.c[var.mdebit]-(stat.mdebit[9,var.mdebit])) #difference of measured debit and annual mean debit
    mdebit.multiply<-ifelse(mdebit.bol>0,1,0) #draw boolean value, filter for value below 0
    mdebit.bol<-mdebit.bol*mdebit.multiply #filtering data, positive above average remains
    mdebit.above<-cbind(mdebit.above, mdebit.bol); # row binding
    var.mdebit<-(var.mdebit+1); # Increment counter
    
  }
  stat.mdebit.above<-stat.desc(mdebit.above)
  
  
  #CREATE MONTHLY PRECIPITATION SUMMARY DATA TABLE
  init.year<-as.numeric(colnames(rain.c[2]))
  period.count<-(ncol(rain.c)-1)
  final.year<-init.year+period.count-1
  
  rain.month<-as.data.frame(1:12, header=T)
  colnames(rain.month)<-"month.number"
  
  rain.year<-2
  while(rain.year <= (period.count+1))
  {
    annual.sum.rain<-as.data.frame(c(sum(rain.c[1:31,rain.year]),sum(rain.c[32:59,rain.year]),sum(rain.c[60:90,rain.year]),sum(rain.c[91:120,rain.year]),sum(rain.c[121:151,rain.year]),sum(rain.c[152:181,rain.year]),sum(rain.c[182:212,rain.year]),sum(rain.c[213:243,rain.year]),sum(rain.c[244:273,rain.year]),sum(rain.c[274:304,rain.year]),sum(rain.c[305:334,rain.year]),sum(rain.c[335:365,rain.year])), header=T)
    colnames(annual.sum.rain)<-as.character(init.year+rain.year-2)
    rain.month<-cbind(rain.month, annual.sum.rain)
    rain.year<-rain.year+1
  }
  
  #CREATE MONTHLY PREDICTED DEBIT SUMMARY DATA TABLE
  init.year<-as.numeric(colnames(mdebit.c[2]))
  period.count<-(ncol(mdebit.c)-1)
  final.year<-init.year+period.count-1
  
  mdebit.month<-as.data.frame(1:12, header=T)
  colnames(mdebit.month)<-"month.number"
  mdebit.year<-2
  
  while(mdebit.year <= (period.count+1))
  {
    annual.sum.mdebit<-as.data.frame(c(sum(mdebit.c[1:31,mdebit.year]),sum(mdebit.c[32:59,mdebit.year]),sum(mdebit.c[60:90,mdebit.year]),sum(mdebit.c[91:120,mdebit.year]),sum(mdebit.c[121:151,mdebit.year]),sum(mdebit.c[152:181,mdebit.year]),sum(mdebit.c[182:212,mdebit.year]),sum(mdebit.c[213:243,mdebit.year]),sum(mdebit.c[244:273,mdebit.year]),sum(mdebit.c[274:304,mdebit.year]),sum(mdebit.c[305:334,mdebit.year]),sum(mdebit.c[335:365,mdebit.year])), header=T)
    colnames(annual.sum.mdebit)<-as.character(init.year+mdebit.year-2)
    mdebit.month<-cbind(mdebit.month, annual.sum.mdebit)
    mdebit.year<-mdebit.year+1
  }
  
  stat.mdebit.month<-stat.desc(mdebit.month)
  
  #CREATE AUXILLARY VARIABLE TABLE
  #PREDICTED
  aux.val.pr<-as.data.frame( stat.rain[7,], header=T);#add ptot
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit[7,]); #add qtot predicted
  aux.val.pr<-rbind(aux.val.pr, stat.rain.above[7,]); #add precipitation above average
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit.above[7,]); #add qtot predicted above average
  aux.val.pr<-rbind(aux.val.pr, stat.rain[5,]); #add max daily precipitation
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit[5,]); #add max qtot predicted
  colnames(aux.val.pr)[1]<-"month.number"; # match column names
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit.month[5,]); #add monthly max qtot predicted
  aux.val.pr<-rbind(aux.val.pr, stat.mdebit.month[4,]); #add monthly min qtot predicted
  colnames(aux.val.pr)[1]<-"DAY"; # match column names
  
  aux.val.pr<-rbind(aux.val.pr, stat.perc[7,]); #add total interception
  aux.val.pr<-rbind(aux.val.pr, stat.et[7,]); #add total ET
  aux.val.pr<-rbind(aux.val.pr, stat.runoff[7,]); #add total runoff
  aux.val.pr<-rbind(aux.val.pr, stat.sqflow[7,]); #add total ET
  aux.val.pr<-rbind(aux.val.pr, stat.syld[7,]);
  rownames(aux.val.pr) <- c("Ptot","Qtot (simulated)", "PabAvg", "Qabavg", "PdailyMax", "QdailyMax", "QmonthlyMax", "QmonthlyMin", "SumPerc", "SumET_SoilplusVeg", "SumRO", "SumSoilQFlow", "tot.syld")
  
  #PREDICTED DATA INDICATORS
  pr.TotDischargeFrac.pred<-aux.val.pr[2,]/aux.val.pr[1,]
  pr.BufferingIndicator<-(1-(aux.val.pr[4,]/aux.val.pr[3,]))
  pr.RelBufferingIndicator<-(1-((aux.val.pr[4,]/aux.val.pr[2,])/(aux.val.pr[3,]/aux.val.pr[1,])))
  pr.BufferingPeak<-(1-(((aux.val.pr[6,]-aux.val.pr[2,])/365)/((aux.val.pr[5,]-aux.val.pr[1,])/365)))
  pr.HighestMonthFrac<-(12*(aux.val.pr[7,]/aux.val.pr[2,]))
  pr.OverlandFlowFrac<-aux.val.pr[11,]/aux.val.pr[1,]
  pr.SoilQflowFrac<-aux.val.pr[12,]/aux.val.pr[1,]
  pr.SlowFlowFrac_Meth1<-(pr.TotDischargeFrac.pred-pr.OverlandFlowFrac-pr.SoilQflowFrac)
  #pr.SlowFlowFrac_Meth2<-(aux.val.pr[1,]-aux.val.pr[9,]-aux.val.pr[10,]-aux.val.pr[11,])/aux.val.pr[1,]
  pr.LowestMonthFrac<-(12*(aux.val.pr[8,]/aux.val.pr[2,]))
  pr.syld<-sum(aux.val.pr[13,])
  
  #CREATE TABLE
  indicator.pre<-as.data.frame(pr.TotDischargeFrac.pred, header=T)
  indicator.pre<-rbind(indicator.pre, pr.RelBufferingIndicator)
  indicator.pre<-rbind(indicator.pre, pr.BufferingIndicator)
  indicator.pre<-rbind(indicator.pre, pr.BufferingPeak)
  indicator.pre<-rbind(indicator.pre, pr.HighestMonthFrac)
  indicator.pre<-rbind(indicator.pre, pr.OverlandFlowFrac)
  indicator.pre<-rbind(indicator.pre, pr.SoilQflowFrac)
  indicator.pre<-rbind(indicator.pre, pr.SlowFlowFrac_Meth1)
  #indicator.pre<-rbind(indicator.pre, pr.SlowFlowFrac_Meth2)
  indicator.pre<-rbind(indicator.pre, pr.LowestMonthFrac)
  indicator.pre<-rbind(indicator.pre, pr.syld)
  rownames(indicator.pre) <- c("TotDischargeFrac.pre","RelBufferingIndicator", "BufferingIndicator", "BufferingPeak",
                               "HighestMonthFrac", "OverlandFlowFrac","SoilQflowFrac","SlowFlowFrac_Meth1","LowestMonthFrac","tot.syld")
  indicator.pre[1]<-NULL
  return(indicator.pre)
}


#CREATE INDICATOR TABLES AT SUB-BASIN LEVEL: FINAL PART

x<-1
while(x<=NSUB)
{
  indicator.pre<-get.sub.indicators(get(paste("sub.out.params.",x, sep="")))
  indicator.pre<-t(indicator.pre)
  YEARS<-as.data.frame(rownames(indicator.pre))
  colnames(YEARS)<-"YEAR"
  ID<-x
  n<-as.data.frame(ID)
  indicator.pre<-cbind(YEARS,n,indicator.pre)
  
  
  eval(parse(text=(paste("indicator.pre.",x,"<-indicator.pre", sep=""))))
  
  x<-x+1
}


indicator.pre.final<-indicator.pre.1
x<-2
while(x<=NSUB)
{
  indicator.pre.final<-rbind(indicator.pre.final,get(paste("indicator.pre.",x, sep="")))
  x<-x+1
}

y<-period1
while(y<=period2)
{
  indicator.sub<-indicator.pre.final[indicator.pre.final$YEAR==y,]
  eval(parse(text=(paste("indicator.pre.",y,"<-indicator.sub", sep=""))))
  file_name<-paste("indicator.pre.",y,".dbf",sep="")
  write.dbf(get(paste("indicator.pre.",y, sep="")), file=file_name, factor2char = FALSE)
  y<-y+1
}
indicator.pre.final[,3:11]<-round(indicator.pre.final[,3:11],digits=2)
indicator.pre.final[,12]<-round(indicator.pre.final[,12])

basin.data.chart<-t(basin.data.final)

# CHART 1: Discharge Fraction
plot.TotDischargeFrac<-rbind(basin.data.chart[2,], basin.data.chart[1,]); #create TotDischargeFrac dataframe
plot.TotDischargeFrac.T <- as.data.frame(t(plot.TotDischargeFrac[,1:ncol(plot.TotDischargeFrac)]))
colnames(plot.TotDischargeFrac.T)<-c("Total_Discharge_Fraction", "Year")

plot.discharge.overland <- ggplot() +
  geom_point(data = plot.TotDischargeFrac.T, aes(x = Year, y = Total_Discharge_Fraction, color = "TotDischargeFrac.pre")) +
  #geom_point(data = plot.OverlandFlowFrac.T, aes(x = Year, y = Total_Overland_Flow_Fraction, color = "OverlandFlowFrac"))  +
  xlab('Year') +  ylab('Indicators') + geom_smooth(data= plot.TotDischargeFrac.T, aes(x = Year, y = Total_Discharge_Fraction, group=1, color = "TotDischargeFrac.pre"),method="lm", se=FALSE) +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))
#geom_smooth(data= plot.OverlandFlowFrac.T, aes(x = Year, y = Total_Overland_Flow_Fraction, group=1),method="lm", se=FALSE)

#CHART 2: Gradual Water release
plot.HighestMonthFrac<-rbind(basin.data.chart[6,], basin.data.chart[1,]); #create HighestMonthFrac dataframe
plot.HighestMonthFrac.T <- as.data.frame(t(plot.HighestMonthFrac[,1:ncol(plot.HighestMonthFrac)]))
plot.HighestMonthFrac.T$V1<-as.numeric(levels(plot.HighestMonthFrac.T$V1))[plot.HighestMonthFrac.T$V1]
y1<-plot.HighestMonthFrac.T$V1
colnames(plot.HighestMonthFrac.T)<-c("Highest_Month_Fraction", "Year")

plot.OverlandFlowFrac<-rbind(basin.data.chart[7,], basin.data.chart[1,]); #create OverlandFlowFrac dataframe
plot.OverlandFlowFrac.T <- as.data.frame(t(plot.OverlandFlowFrac[,1:ncol(plot.OverlandFlowFrac)]))
plot.OverlandFlowFrac.T$V1<-as.numeric(levels(plot.OverlandFlowFrac.T$V1))[plot.OverlandFlowFrac.T$V1]
y2<-plot.OverlandFlowFrac.T$V1
colnames(plot.OverlandFlowFrac.T)<-c("Total_Overland_Flow_Fraction", "Year")

y_scale<-c(y1,y2)

plot.grad.water.res <- ggplot() +
  geom_point(data = plot.OverlandFlowFrac.T, aes(x = Year, y = Total_Overland_Flow_Fraction, color = "Total_Overland_Flow_Fraction")) +
  geom_point(data = plot.HighestMonthFrac.T, aes(x = Year, y = Highest_Month_Fraction, color = "Highest_Month_Fraction")) +
  geom_smooth(data= plot.OverlandFlowFrac.T, aes(x = Year, y = Total_Overland_Flow_Fraction, group=1, color="Total_Overland_Flow_Fraction"),method="lm", se=FALSE)+
  geom_smooth(data= plot.HighestMonthFrac.T, aes(x = Year, y = Highest_Month_Fraction, group=1, color = "Highest_Month_Fraction"),method="lm", se=FALSE)+
  xlab('Year') + ylab('Indicators') +  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8)) +
  scale_y_continuous(breaks=seq(round(min(y_scale)), round(max(y_scale)),(round(max(y_scale))/5)))

#CHART 3: Water Transmission Function
plot.SoilQflow<-rbind(basin.data.chart[8,], basin.data.chart[1,]); #create SoilQflow dataframe
plot.SoilQflow.T <- as.data.frame(t(plot.SoilQflow[,1:ncol(plot.SoilQflow)]))
plot.SoilQflow.T$V1<-as.numeric(levels(plot.SoilQflow.T$V1))[plot.SoilQflow.T$V1]
y1<-plot.SoilQflow.T$V1
colnames(plot.SoilQflow.T)<-c("SoilQFlow", "Year")

plot.SlowFlowFrac<-rbind(basin.data.chart[9,], basin.data.chart[1,]); #create SlowFlowFrac_Meth2 dataframe
plot.SlowFlowFrac.T <- as.data.frame(t(plot.SlowFlowFrac[,1:ncol(plot.SlowFlowFrac)]))
plot.SlowFlowFrac.T$V1<-as.numeric(levels(plot.SlowFlowFrac.T$V1))[plot.SlowFlowFrac.T$V1]
y2<-plot.SlowFlowFrac.T$V1
colnames(plot.SlowFlowFrac.T)<-c("SlowFlowFrac", "Year")

plot.LowestMonthFrac<-rbind(basin.data.chart[10,], basin.data.chart[1,]); #create LowestMonthFrac dataframe
plot.LowestMonthFrac.T <- as.data.frame(t(plot.LowestMonthFrac[,1:ncol(plot.LowestMonthFrac)]))
plot.LowestMonthFrac.T$V1<-as.numeric(levels(plot.LowestMonthFrac.T$V1))[plot.LowestMonthFrac.T$V1]
y3<-plot.LowestMonthFrac.T$V1
colnames(plot.LowestMonthFrac.T)<-c("LowestMonthFrac", "Year")

y_scale<-c(y1,y2,y3)

plot.water.trans.func <- ggplot() +
  geom_point(data = plot.SoilQflow.T, aes(x = Year, y = SoilQFlow, color = "SoilQFlow")) +
  geom_point(data = plot.SlowFlowFrac.T, aes(x = Year, y = SlowFlowFrac, color = "SlowFlowFrac")) +
  geom_point(data = plot.LowestMonthFrac.T, aes(x = Year, y = LowestMonthFrac, color = "LowestMonthFrac")) +
  geom_smooth(data= plot.SoilQflow.T, aes(x = Year, y = SoilQFlow, group=1,color = "SoilQFlow"),method="lm", se=FALSE)+
  geom_smooth(data= plot.SlowFlowFrac.T, aes(x = Year, y = SlowFlowFrac, group=1,color = "SlowFlowFrac"),method="lm", se=FALSE)+
  geom_smooth(data= plot.LowestMonthFrac.T, aes(x = Year, y = LowestMonthFrac, group=1,color = "LowestMonthFrac"),method="lm", se=FALSE)+
  xlab('Year') + ylab('Indicators') + theme( legend.title = element_text(size=8),legend.text = element_text(size = 8)) +
  scale_y_continuous(breaks=seq(round(min(y_scale)), round(max(y_scale)),(round(max(y_scale))/5)))


#CHART 4: Buffering capacity function
plot.RelBufferingIndicator<-rbind(basin.data.chart[3,], basin.data.chart[1,]); #create RelBufferingIndicator dataframe
plot.RelBufferingIndicator.T <- as.data.frame(t(plot.RelBufferingIndicator[,1:ncol(plot.RelBufferingIndicator)]))
plot.RelBufferingIndicator.T$V1<-as.numeric(levels(plot.RelBufferingIndicator.T$V1))[plot.RelBufferingIndicator.T$V1]
y1<-plot.RelBufferingIndicator.T$V1
colnames(plot.RelBufferingIndicator.T)<-c("RelBufferingIndicator", "Year")

plot.BufferingIndicator<-rbind(basin.data.chart[4,], basin.data.chart[1,]); #create BufferingIndicator dataframe
plot.BufferingIndicator.T <- as.data.frame(t(plot.BufferingIndicator[,1:ncol(plot.BufferingIndicator)]))
plot.BufferingIndicator.T$V1<-as.numeric(levels(plot.BufferingIndicator.T$V1))[plot.BufferingIndicator.T$V1]
y2<-plot.BufferingIndicator.T$V1
colnames(plot.BufferingIndicator.T)<-c("BufferingIndicator", "Year")

plot.BufferingPeak<-rbind(basin.data.chart[5,], basin.data.chart[1,]); #create BufferingPeak dataframe
plot.BufferingPeak.T <- as.data.frame(t(plot.BufferingPeak[,1:ncol(plot.BufferingPeak)]))
plot.BufferingPeak.T$V1<-as.numeric(levels(plot.BufferingPeak.T$V1))[plot.BufferingPeak.T$V1]
y3<-plot.BufferingPeak.T$V1
colnames(plot.BufferingPeak.T)<-c("BufferingPeak", "Year")

y_scale<-c(y1,y2,y3)

plot.buffer <- ggplot() +
  geom_point(data = plot.BufferingIndicator.T, aes(x = Year, y = BufferingIndicator, color = "BufferingIndicator")) +
  geom_point(data = plot.RelBufferingIndicator.T, aes(x = Year, y = RelBufferingIndicator, color = "RelBufferingIndicator"))  +
  geom_point(data = plot.BufferingPeak.T, aes(x = Year, y = BufferingPeak, color = "BufferingPeak"))  +
  geom_smooth(data= plot.BufferingIndicator.T, aes(x = Year, y = BufferingIndicator, group=1,color = "BufferingIndicator"),method="lm", se=FALSE)+
  geom_smooth(data= plot.RelBufferingIndicator.T, aes(x = Year, y = RelBufferingIndicator, group=1, color = "RelBufferingIndicator"),method="lm", se=FALSE)+
  geom_smooth(data= plot.BufferingPeak.T, aes(x = Year, y = BufferingPeak, group=1 ,color = "BufferingPeak"),method="lm", se=FALSE)+
  xlab('Year') + ylab('Indicators') + theme( legend.title = element_text(size=8),legend.text = element_text(size = 8)) +
  scale_y_continuous(breaks=seq(round(min(y_scale)), round(max(y_scale)),(round(max(y_scale))/5)))


#WRITE REPORT
title<-"\\b\\fs32 LUMENS Ques-H Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules : Watershed Indicators\\b0\\fs20"
date<-Sys.Date()
date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
rtffile <- RTF(paste("Watershed Indicators -",location, period1,"-",period2, ".lpr",sep=" "))
if (file.exists("C:/Program Files (x86)/LUMENS")){
  addPng (rtffile, "C:/Program Files (x86)/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
} else{
  addPng (rtffile, "C:/Program Files/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
}
subtitle.head<-paste(location, period1,"to",period2,sep=" ")
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, subtitle.head)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
text <- paste("area[ha]:", tot.area[1]*100 ,"       Number of Subbasins:", NSUB,sep=" ")
addParagraph(rtffile, text)
text<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
addParagraph(rtffile, text)
text <- paste("Basin level watershed indicators")
addHeader(rtffile, title=text)
addTable(rtffile, basin.data.final, font.size=7)
addNewLine(rtffile, n=1)
text<-paste("Discharge Fraction")
addHeader(rtffile, title=text)
addPlot(rtffile, plot.fun=print,width=6.7, height=3, res=300, plot.discharge.overland  )
addNewLine(rtffile,n=1)
text<-paste("Gradual Water Release Indicators")
addHeader(rtffile, title=text)
addPlot(rtffile, plot.fun=print,width=6.7, height=3, res=300, plot.grad.water.res)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
text<-paste("Water Transmission Function Indicators")
addHeader(rtffile, title=text)
addPlot(rtffile, plot.fun=print, width=6.7, height=3, res=300, plot.water.trans.func )
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
text<-paste("Buffering Capacity Function Indicators")
addHeader(rtffile, title=text)
addPlot(rtffile, plot.fun=print, width=6.7, height=3, res=300, plot.buffer )
addNewLine(rtffile, n=1)
addNewLine(rtffile,n=1)
text<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
addParagraph(rtffile, text)
text<-paste("Sub-Basin level watershed indicators")
addHeader(rtffile, title=text)
y<-period1
while(y<=period2)
{
  indicator.sub<-indicator.pre.final[indicator.pre.final$YEAR==y,]
  eval(parse(text=(paste("indicator.pre.",y,"<-indicator.sub", sep=""))))
  text<-paste("Watershed Indicators for Subbasin",y,sep=" ")
  addParagraph(rtffile, text)
  plot.graph=paste("indicator.pre.",y, sep="")
  addTable(rtffile, get(plot.graph)[,2:12], font.size=6)
  addNewLine(rtffile,n=1)
  y<-y+1
}
done(rtffile)