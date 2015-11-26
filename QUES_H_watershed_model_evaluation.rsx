##[QUES]=group
##Wdir=folder
##location=string
##init.date=string
##final.date=string
##swatrun=number 0
##end.rch=number 10
##obsv=file
##passfilenames
##showplots

library(plyr)
library(lubridate)
library(foreign)
library(hydroGOF)
library(rtf)
library(reshape)
library(pastecs)

workd<-Wdir
setwd(workd)
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

# Step 1: Excecute SWAT2009
#check swat availability
if (file.exists("SWAT2009.exe")==TRUE){
if (swatrun==1){
system("SWAT2009.exe")
} else{
print("SWAT model skipped")
}
} else{
print("SWAT model not available in TxtInOut Directory")
}

# Step 2: Read observed and simulated files
Qo<-read.table(obsv)[,3]
Qs1<-read.table(paste(workd,"/output.rch", sep=''), skip=9)
Qs<-Qs1[Qs1$V2==end.rch,7]

#Step 3: Calculate model performance statistics
SSR<-sum((Qs-Qo)^2)
NS<-1-(SSR/(sum((Qo-mean(Qo))^2)))
PBIAS<-100*(sum(Qs)-sum(Qo))/sum(Qo)
SSR;NS;PBIAS
Observed.daily<-Qo
Simulated.Daily<-Qs
#ggof(obs=Observed.daily, sim=Simulated.Daily);# Annual Performance

mod.date<-as.character(seq(as.Date(init.date, "%d/%m/%Y"), as.Date(final.date, "%d/%m/%Y"), "days")); #create date sequence
YEAR<-year(mod.date)
DAY<-yday(mod.date)
NSUB<-max(Qs1$V2, na.rm=T);#number of sub-basin
tot.area<-Qs1[Qs1$V2==end.rch,5]

basin.a<-as.data.frame(cbind(YEAR,DAY,Qo,Qs))
colnames(basin.a)<-c("YEAR", "DAY","Qo","Qs")

#PIVOT TABLE: MELT & CAST
#1create observed water yield sheet
odebit.m<- melt(basin.a, id=c(1:2), measure=c(3))
odebit.m$value<- as.numeric(as.character(odebit.m$value))
odebit.m$day<- as.numeric(as.character(odebit.m$DAY))
odebit.c<-cast(odebit.m, DAY ~ YEAR, sum)

#2create predicted water yield sheet
mdebit.m<- melt(basin.a, id=c(1:2), measure=c(4))
mdebit.m$value<- as.numeric(as.character(mdebit.m$value))
mdebit.m$day<- as.numeric(as.character(mdebit.m$DAY))
mdebit.c<-cast(mdebit.m, DAY ~ YEAR, sum)
stat.mdebit<-stat.desc(mdebit.c)

#CREATE MONTHLY OBSERVED DEBIT SUMMARY DATA TABLE
init.year<-as.numeric(colnames(odebit.c[2]))
period.count<-(ncol(odebit.c)-1)
final.year<-init.year+period.count-1

odebit.year<-2
annual.sum.odebit<-as.data.frame(c(sum(odebit.c[1:31,odebit.year]),sum(odebit.c[32:59,odebit.year]),sum(odebit.c[60:90,odebit.year]),sum(odebit.c[91:120,odebit.year]),sum(odebit.c[121:151,odebit.year]),sum(odebit.c[152:181,odebit.year]),sum(odebit.c[182:212,odebit.year]),sum(odebit.c[213:243,odebit.year]),sum(odebit.c[244:273,odebit.year]),sum(odebit.c[274:304,odebit.year]),sum(odebit.c[305:334,odebit.year]),sum(odebit.c[335:365,odebit.year])), header=T)
while(odebit.year <= (period.count))
{
odebit.year<-odebit.year+1
annual.sum.odebit2<-as.data.frame(c(sum(odebit.c[1:31,odebit.year]),sum(odebit.c[32:59,odebit.year]),sum(odebit.c[60:90,odebit.year]),sum(odebit.c[91:120,odebit.year]),sum(odebit.c[121:151,odebit.year]),sum(odebit.c[152:181,odebit.year]),sum(odebit.c[182:212,odebit.year]),sum(odebit.c[213:243,odebit.year]),sum(odebit.c[244:273,odebit.year]),sum(odebit.c[274:304,odebit.year]),sum(odebit.c[305:334,odebit.year]),sum(odebit.c[335:365,odebit.year])), header=T)
annual.sum.odebit<-rbind(annual.sum.odebit, annual.sum.odebit2)
}
colnames(annual.sum.odebit)<-as.character("obs.Debit")

#CREATE MONTHLY PREDICTED DEBIT SUMMARY DATA TABLE
init.year<-as.numeric(colnames(mdebit.c[2]))
period.count<-(ncol(mdebit.c)-1)
final.year<-init.year+period.count-1


mdebit.year<-2
annual.sum.mdebit<-as.data.frame(c(sum(mdebit.c[1:31,mdebit.year]),sum(mdebit.c[32:59,mdebit.year]),sum(mdebit.c[60:90,mdebit.year]),sum(mdebit.c[91:120,mdebit.year]),sum(mdebit.c[121:151,mdebit.year]),sum(mdebit.c[152:181,mdebit.year]),sum(mdebit.c[182:212,mdebit.year]),sum(mdebit.c[213:243,mdebit.year]),sum(mdebit.c[244:273,mdebit.year]),sum(mdebit.c[274:304,mdebit.year]),sum(mdebit.c[305:334,mdebit.year]),sum(mdebit.c[335:365,mdebit.year])), header=T)
while(mdebit.year <= (period.count))
{
mdebit.year<-mdebit.year+1
annual.sum.mdebit2<-as.data.frame(c(sum(mdebit.c[1:31,mdebit.year]),sum(mdebit.c[32:59,mdebit.year]),sum(mdebit.c[60:90,mdebit.year]),sum(mdebit.c[91:120,mdebit.year]),sum(mdebit.c[121:151,mdebit.year]),sum(mdebit.c[152:181,mdebit.year]),sum(mdebit.c[182:212,mdebit.year]),sum(mdebit.c[213:243,mdebit.year]),sum(mdebit.c[244:273,mdebit.year]),sum(mdebit.c[274:304,mdebit.year]),sum(mdebit.c[305:334,mdebit.year]),sum(mdebit.c[335:365,mdebit.year])), header=T)
annual.sum.mdebit<-rbind(annual.sum.mdebit, annual.sum.mdebit2)
}
colnames(annual.sum.mdebit)<-as.character("pred.Debit")
mod.month<-as.character(seq(as.Date(init.date, "%d/%m/%Y"), as.Date(final.date, "%d/%m/%Y"), "months")); #create date sequence

monthly.obspred<-as.data.frame(cbind(annual.sum.odebit, annual.sum.mdebit), rownames=T); #MONTHLY SUMMARY TABLE
monthly.obspred<-round(monthly.obspred, digits=2)
rownames(monthly.obspred)<-mod.month

stat.odebit.month<-stat.desc(annual.sum.odebit)
stat.mdebit.month<-stat.desc(annual.sum.mdebit)
stat.summary<-cbind(stat.odebit.month, stat.mdebit.month)
stat.summary<-rbind(stat.summary[4,], stat.summary[5,], stat.summary[9,], stat.summary[7,], stat.summary[13,])
stat.summary<-round(stat.summary, digits=2)

simulated.monthly<-as.numeric(as.matrix(annual.sum.mdebit))
observed.monthly<-as.numeric(as.matrix(annual.sum.odebit))

#ggof(sim=simulated.monthly, obs=observed.monthly)

#WRITE REPORT
title<-"\\b\\fs32 LUMENS Ques-H Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules : Watershed Modeling Performance Evaluation\\b0\\fs20"
date<-Sys.Date()
date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
rtffile <- RTF(paste("Watershed Modelling Performance Evaluation -",location, init.year,"-",final.year, ".lpr",sep=" "))
if (file.exists("C:/Program Files (x86)/LUMENS")){
  addPng (rtffile, "C:/Program Files (x86)/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
} else{
  addPng (rtffile, "C:/Program Files/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
}
subtitle.head<-paste(location, init.date,"to",final.date,sep=" ")
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
text <- paste("Monthly Model Performance Summary")
addHeader(rtffile, title=text)
addTable(rtffile, monthly.obspred, row.names=T)
addNewLine(rtffile, n=1)
addTable(rtffile, stat.summary, row.names=T)
addNewLine(rtffile, n=1)
text <- paste("Monthly Plot of Simulation Against Observation for", init.date, "-", final.date, sep=" ")
addHeader(rtffile, title=text)
addPlot(rtffile, plot.fun=print, width=6.7, height=4, res=300, ggof(sim=simulated.monthly, obs=observed.monthly) )
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
text <- paste("Annual Plot of Simulation Against Observation for", init.date, "-", final.date, sep=" ")
addHeader(rtffile, title=text)
addPlot(rtffile, plot.fun=print, width=6.7, height=4, res=300, ggof(obs=Observed.daily, sim=Simulated.Daily))
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
done(rtffile)
