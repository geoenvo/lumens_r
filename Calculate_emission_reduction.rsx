##[SCIENDO]=group
##Set_Working_Directory=folder
##period1= number 2010
##period2=number 2014
##iteration=number 5
##Scenario_name=file
##Abacus_Project_File = file

library(foreign)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(rtf)


time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
setwd(Set_Working_Directory)
file.name<-Scenario_name



# THIS PART IS ORIGINALLY WROTE TO ACCESS ABACUS .CAR FILE

#Set_Working_Directory<-workingDirectory
#setwd(Set_Working_Directory)
#New_Abacus_Project_db<-as.data.frame(readLines(paste(Set_Working_Directory,  "/Paling_Baru.car",sep="")))
New_Abacus_Project_file<-readLines(file.name)
zone_number<-as.character(New_Abacus_Project_file[5])
zone_number<-as.data.frame(strsplit(zone_number, "="))
zone_number<-as.numeric(as.character(zone_number[2,]))
iteration_number<-as.character(New_Abacus_Project_file[12])
iteration_number<-as.data.frame(strsplit(iteration_number, "="))
iteration_number<-as.numeric(as.character(iteration_number[2,]))
baris<-as.numeric(pmatch('Summary', New_Abacus_Project_file))
baris<-baris+4

#Net Emission Per-Ha (ton CO2-eq/ha.year)
#NE.ha<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(NE.ha, paste(Set_Working_Directory,  "/NE.ha.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#NE.ha<-read.table(paste(Set_Working_Directory,  "/NE.ha.txt",sep=""),sep="\t")
#NE.ha$V8<-NULL
#NE.ha[,2:7]<-as.numeric(as.character(as.factor(unlist(NE.ha[,2:7]))))
#colnames(NE.ha)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/NE.ha.txt",sep=""))
baris<-baris+zone_number+3

#Net Emission (ton CO2-eq/year)
#NE<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(NE, paste(Set_Working_Directory,  "/NE.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#NE<-read.table(paste(Set_Working_Directory,  "/NE.txt",sep=""),sep="\t")
#NE$V8<-NULL
#NE[,2:7]<-as.numeric(as.character(as.factor(unlist(NE[,2:7]))))
#colnames(NE)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/NE.txt",sep=""))
baris<-baris+zone_number+3

#Emission Per-Ha Area (ton CO2-eq/ha.year)
#NE.Ha.A<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(NE.Ha.A, paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#NE.Ha.A<-read.table(paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""),sep="\t")
#NE.Ha.A$V8<-NULL
#NE.Ha.A[,2:7]<-as.numeric(as.character(as.factor(unlist(NE.Ha.A[,2:7]))))
#colnames(NE.Ha.A)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""))
baris<-baris+zone_number+3

#Emission Total  (ton CO2-eq/year)
TE<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(TE, paste(Set_Working_Directory,  "/TE.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
TE<-read.table(paste(Set_Working_Directory,  "/TE.txt",sep=""),sep="\t")
eval(parse(text=(paste( "TE$V" ,iteration+3, "<-NULL", sep="" ))))
TE[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(TE[,2:(iteration+2)]))))
#colnames(TE)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/TE.txt",sep=""))
baris<-baris+zone_number+3

#Sequestration Per-Ha Area (ton CO2-eq/ha.year)
#Se.Ha.A<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(Se.Ha.A, paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#Se.Ha.A<-read.table(paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""),sep="\t")
#Se.Ha.A$V8<-NULL
#Se.Ha.A[,2:7]<-as.numeric(as.character(as.factor(unlist(Se.Ha.A[,2:7]))))
#colnames(Se.Ha.A)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""))
baris<-baris+zone_number+3

#Sequestration Total (ton CO2-eq/year)
ST<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(ST, paste(Set_Working_Directory,  "/ST.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
ST<-read.table(paste(Set_Working_Directory,  "/ST.txt",sep=""),sep="\t")
eval(parse(text=(paste( "ST$V" ,iteration+3, "<-NULL", sep="" ))))
ST[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(ST[,2:(iteration+2)]))))
#colnames(ST)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/ST.txt",sep=""))
baris<-baris+zone_number+3

#Land Use System  Emission Per-Ha Area (ton CO2-eq/ha.year)
baris2<-as.numeric(pmatch('Land Use System\tEmission Total  (ton CO2-eq/year)', New_Abacus_Project_file))
#LUS.EM<-as.data.frame(New_Abacus_Project_file[baris:(baris2-2)])
#write.table(LUS.EM, paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
#LUS.EM<-read.table(paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""),sep="\t")
#LUS.EM$V10<-NULL
#LUS.EM[,4:9]<-as.numeric(as.character(as.factor(unlist(LUS.EM[,4:9]))))
#colnames(LUS.EM)<-c("Zone","Original.LUS","New.LUS","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""))
baris<-baris2+2

#Land Use System  Emission Total  (ton CO2-eq/year)
baris2<-as.numeric(pmatch('Emissions Associated with Benefit Less Than Threshold ', New_Abacus_Project_file))
#LUS.EM.T<-as.data.frame(New_Abacus_Project_file[baris:(baris2-2)])
#write.table(LUS.EM.T, paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
#LUS.EM.T<-read.table(paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""),sep="\t")
#LUS.EM.T$V10<-NULL
#LUS.EM.T[,4:9]<-as.numeric(as.character(as.factor(unlist(LUS.EM.T[,4:9]))))
#colnames(LUS.EM.T)<-c("Zone","Original.LUS","New.LUS","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""))
baris<-baris2+4

#Cost Threshold ($/ton CO2-eq)  5.0
#CT<-as.data.frame(New_Abacus_Project_file[baris:(baris+1+iteration_number)])
#write.table(CT, paste(Set_Working_Directory,  "/CT.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
#CT<-read.table(paste(Set_Working_Directory,  "/CT.txt",sep=""),sep="\t")
#CT[,2]<-as.numeric(as.character(as.factor(unlist(CT[,2]))))
#colnames(CT)<-c("Iteration","Private")
#file.remove(paste(Set_Working_Directory,  "/CT.txt",sep=""))
baris<-baris+iteration_number+4

#Total
baris2<-as.numeric(nrow(as.data.frame(New_Abacus_Project_file)))
Summary<-as.data.frame(New_Abacus_Project_file[baris:baris2])
write.table(Summary, paste(Set_Working_Directory,  "/Summary.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
Summary<-read.table(paste(Set_Working_Directory,  "/Summary.txt",sep=""),sep="\t")
eval(parse(text=(paste( "Summary$V" ,iteration+3, "<-NULL", sep="" ))))
Summary[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(Summary[,2:(iteration+2)]))))
#colnames(Summary)<-c("Summary","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/Summary.txt",sep=""))

# SCENARIO COMPARISON
# THIS PART IS ORIGINALLY WROTE TO ACCESS ABACUS .CAR FILE

#Set_Working_Directory<-workingDirectory
#setwd(Set_Working_Directory)
#New_Abacus_Project_db<-as.data.frame(readLines(paste(Set_Working_Directory,  "/Paling_Baru.car",sep="")))
New_Abacus_Project_file<-readLines(Abacus_Project_File)
zone_number<-as.character(New_Abacus_Project_file[5])
zone_number<-as.data.frame(strsplit(zone_number, "="))
zone_number<-as.numeric(as.character(zone_number[2,]))
iteration_number<-as.character(New_Abacus_Project_file[12])
iteration_number<-as.data.frame(strsplit(iteration_number, "="))
iteration_number<-as.numeric(as.character(iteration_number[2,]))
baris<-as.numeric(pmatch('Summary', New_Abacus_Project_file))
baris<-baris+4

#Net Emission Per-Ha (ton CO2-eq/ha.year)
#NE.ha<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(NE.ha, paste(Set_Working_Directory,  "/NE.ha.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#NE.ha<-read.table(paste(Set_Working_Directory,  "/NE.ha.txt",sep=""),sep="\t")
#NE.ha$V8<-NULL
#NE.ha[,2:7]<-as.numeric(as.character(as.factor(unlist(NE.ha[,2:7]))))
#colnames(NE.ha)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/NE.ha.txt",sep=""))
baris<-baris+zone_number+3

#Net Emission (ton CO2-eq/year)
#NE<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(NE, paste(Set_Working_Directory,  "/NE.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#NE<-read.table(paste(Set_Working_Directory,  "/NE.txt",sep=""),sep="\t")
#NE$V8<-NULL
#NE[,2:7]<-as.numeric(as.character(as.factor(unlist(NE[,2:7]))))
#colnames(NE)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/NE.txt",sep=""))
baris<-baris+zone_number+3

#Emission Per-Ha Area (ton CO2-eq/ha.year)
#NE.Ha.A<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(NE.Ha.A, paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#NE.Ha.A<-read.table(paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""),sep="\t")
#NE.Ha.A$V8<-NULL
#NE.Ha.A[,2:7]<-as.numeric(as.character(as.factor(unlist(NE.Ha.A[,2:7]))))
#colnames(NE.Ha.A)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""))
baris<-baris+zone_number+3

#Emission Total  (ton CO2-eq/year)
TE1<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(TE1, paste(Set_Working_Directory,  "/TE1.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
TE1<-read.table(paste(Set_Working_Directory,  "/TE1.txt",sep=""),sep="\t")
eval(parse(text=(paste( "TE1$V" ,iteration+3, "<-NULL", sep="" ))))
TE1[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(TE1[,2:(iteration+2)]))))
#colnames(TE)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/TE1.txt",sep=""))
baris<-baris+zone_number+3

#Sequestration Per-Ha Area (ton CO2-eq/ha.year)
#Se.Ha.A<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(Se.Ha.A, paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#Se.Ha.A<-read.table(paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""),sep="\t")
#Se.Ha.A$V8<-NULL
#Se.Ha.A[,2:7]<-as.numeric(as.character(as.factor(unlist(Se.Ha.A[,2:7]))))
#colnames(Se.Ha.A)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""))
baris<-baris+zone_number+3

#Sequestration Total (ton CO2-eq/year)
ST1<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(ST1, paste(Set_Working_Directory,  "/ST1.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
ST1<-read.table(paste(Set_Working_Directory,  "/ST1.txt",sep=""),sep="\t")
eval(parse(text=(paste( "ST1$V" ,iteration+3, "<-NULL", sep="" ))))
ST1[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(ST1[,2:(iteration+2)]))))
#colnames(ST)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/ST1.txt",sep=""))
baris<-baris+zone_number+3

#Land Use System  Emission Per-Ha Area (ton CO2-eq/ha.year)
baris2<-as.numeric(pmatch('Land Use System\tEmission Total  (ton CO2-eq/year)', New_Abacus_Project_file))
#LUS.EM<-as.data.frame(New_Abacus_Project_file[baris:(baris2-2)])
#write.table(LUS.EM, paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
#LUS.EM<-read.table(paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""),sep="\t")
#LUS.EM$V10<-NULL
#LUS.EM[,4:9]<-as.numeric(as.character(as.factor(unlist(LUS.EM[,4:9]))))
#colnames(LUS.EM)<-c("Zone","Original.LUS","New.LUS","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""))
baris<-baris2+2

#Land Use System  Emission Total  (ton CO2-eq/year)
baris2<-as.numeric(pmatch('Emissions Associated with Benefit Less Than Threshold ', New_Abacus_Project_file))
#LUS.EM.T<-as.data.frame(New_Abacus_Project_file[baris:(baris2-2)])
#write.table(LUS.EM.T, paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
#LUS.EM.T<-read.table(paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""),sep="\t")
#LUS.EM.T$V10<-NULL
#LUS.EM.T[,4:9]<-as.numeric(as.character(as.factor(unlist(LUS.EM.T[,4:9]))))
#colnames(LUS.EM.T)<-c("Zone","Original.LUS","New.LUS","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""))
baris<-baris2+4

#Cost Threshold ($/ton CO2-eq)  5.0
#CT<-as.data.frame(New_Abacus_Project_file[baris:(baris+1+iteration_number)])
#write.table(CT, paste(Set_Working_Directory,  "/CT.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
#CT<-read.table(paste(Set_Working_Directory,  "/CT.txt",sep=""),sep="\t")
#CT[,2]<-as.numeric(as.character(as.factor(unlist(CT[,2]))))
#colnames(CT)<-c("Iteration","Private")
#file.remove(paste(Set_Working_Directory,  "/CT.txt",sep=""))
baris<-baris+iteration_number+4

#Total
baris2<-as.numeric(nrow(as.data.frame(New_Abacus_Project_file)))
Summary1<-as.data.frame(New_Abacus_Project_file[baris:baris2])
write.table(Summary1, paste(Set_Working_Directory,  "/Summary1.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
Summary1<-read.table(paste(Set_Working_Directory,  "/Summary1.txt",sep=""),sep="\t")
eval(parse(text=(paste( "Summary1$V" ,iteration+3, "<-NULL", sep="" ))))
Summary1[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(Summary1[,2:(iteration+2)]))))
#colnames(Summary)<-c("Summary","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/Summary1.txt",sep=""))


#BASELINE RESULT
TableSum1<-round((Summary1[(1:12),(2:(iteration+2))]),digits=2)
TableSum1<-cbind((Summary1[1]),TableSum1)
TableSum1 = TableSum1[-2,]
TableSum1 = TableSum1[-3,]
TableSum1 = TableSum1[-4,]
TableSum1 = TableSum1[-5,]
TableSum1 = TableSum1[-6,]
TableSum1 = TableSum1[-7,]
TableSum.Baseline<-TableSum1
TE2<-round((TE1[,(2:(iteration+2))]),digits=2)
ST2<-round((ST1[,(2:(iteration+2))]),digits=2)
TE1<-cbind((TE1[1]),TE2)
ST1<-cbind((ST1[1]),ST2)
TE1$Total<-rowSums(TE1[,2:(iteration+2)])
ST1$Total<-rowSums(ST1[,2:(iteration+2)])
TE1.total<-sum(TE1$Total)
ST1.total<-sum(ST1$Total)
TE1$Percentage<-round(((TE1$Total/TE1.total)*100),digits=2)
ST1$Percentage<-round(((ST1$Total/ST1.total)*100),digits=2)
TE1 <- TE1[order(-TE1$Percentage),]
ST1 <- ST1[order(-ST1$Percentage),]
TE.Baseline<-TE1
ST.Baseline<-ST1

#SCENARIO RESULT
TableSum<-round((Summary[(1:12),(2:(iteration+2))]),digits=2)
TableSum<-cbind((Summary[1]),TableSum)
TableSum = TableSum[-2,]
TableSum = TableSum[-3,]
TableSum = TableSum[-4,]
TableSum = TableSum[-5,]
TableSum = TableSum[-6,]
TableSum = TableSum[-7,]
TableSum.Scenario<-TableSum
TE2<-round((TE[,(2:(iteration+2))]),digits=2)
ST2<-round((ST[,(2:(iteration+2))]),digits=2)
TE<-cbind((TE[1]),TE2)
ST<-cbind((ST[1]),ST2)
TE$Total<-rowSums(TE[,2:(iteration+2)])
ST$Total<-rowSums(ST[,2:(iteration+2)])
TE.total<-sum(TE$Total)
ST.total<-sum(ST$Total)
TE$Percentage<-round(((TE$Total/TE.total)*100),digits=2)
ST$Percentage<-round(((ST$Total/ST.total)*100),digits=2)
TE <- TE[order(-TE$Percentage),]
ST <- ST[order(-ST$Percentage),]
TE.Scenario<-TE
ST.Scenario<-ST


#CREATE CUMULATIVE SUMMARY
Cum.summary<-as.data.frame(cumsum(t(TableSum.Baseline[1,2:(iteration+2)])))
Cum.summary.sc<-as.data.frame(cumsum(t(TableSum.Scenario[1,2:(iteration+2)])))


# CREATE PERIODE OF SIMULATION
interval<-period2-period1
t_1<-period1
t_2<-t_1+interval
Period.db<-as.data.frame(NULL)
Periode<-as.data.frame(NULL)
for ( i in 1:(ncol(Cum.summary)-1)){
period.int<-paste(t_1,"-",t_2, sep="")
Period.db<-c(Period.db,period.int)
t_1<-t_1+interval
t_2<-t_1+interval
}
Period.db<-as.character(Period.db)
t_1<-period1
t_2<-t_1+interval
for ( i in 1:(nrow(Cum.summary))){
period.int<-paste(t_1,"-",t_2, sep="")
Periode<-c(Periode,period.int)
t_1<-t_1+interval
t_2<-t_1+interval
}
Periode<-as.character(Periode)

#FIX COLUMN NAME
Colname.ST.TE<-append("Planning unit", Periode)
Colname.ST.TE<-append(Colname.ST.TE, "Total")
Colname.ST.TE<-append(Colname.ST.TE, "Percentage")
colnames(TE.Baseline) <- c(Colname.ST.TE)
colnames(TE.Scenario) <- c(Colname.ST.TE)
colnames(ST.Baseline) <- c(Colname.ST.TE)
colnames(ST.Scenario) <- c(Colname.ST.TE)
Colname.Summary<-append("Parameters", Periode)
colnames(TableSum.Baseline) <- c(Colname.Summary)
colnames(TableSum.Scenario) <- c(Colname.Summary)

#CUMULATIVE TABLE
Cumulative.table<-cbind(as.data.frame(Periode), Cum.summary)
Cumulative.table<-cbind(Cumulative.table, Cum.summary.sc)
colnames(Cumulative.table)[2]<-"BASELINE Cum.em (tonCo2/ha.yr)"
colnames(Cumulative.table)[3]<-"SCENARIO Cum.em (tonCo2/ha.yr)"


#PREPARE TABLE FOR GRAPH AND CREATE GRAPH
Cum.summary.temp<-Cum.summary
colnames(Cum.summary.temp)[1]<-"Cum.em"
class1<-"BASELINE"
Cum.summary.temp<-cbind(class1,Cum.summary.temp)
Cum.summary.temp<-cbind(as.data.frame(Periode), Cum.summary.temp)
Cum.summary.temp.sc<-Cum.summary.sc
colnames(Cum.summary.temp.sc)[1]<-"Cum.em"
class1<-"SCENARIO"
Cum.summary.temp.sc<-cbind(class1,Cum.summary.temp.sc)
Cum.summary.temp.sc<-cbind(as.data.frame(Periode), Cum.summary.temp.sc)
Cum.summary.tab<-rbind(Cum.summary.temp,Cum.summary.temp.sc)
PLOT1<-ggplot(data=Cum.summary.tab, aes(x=Periode, y=Cum.em, group=class1, colour=class1)) + geom_line() + geom_point()+ geom_point(size=3, fill="white")

#CALCULATE EMISSION REDUCTION
em.reduction1<-Cumulative.table[nrow(Cumulative.table),2]
em.reduction2<-Cumulative.table[nrow(Cumulative.table),3]
em.reduction<-((em.reduction1-em.reduction2)/em.reduction1)*100
em.reduction<-round(em.reduction, digits=2)
kalimat<-paste("Persentase penurunan emisi terhadap baseline adalah " , em.reduction, "%", sep="")

# PREPARE COMPARISON OF PLANNING UNIT
compare.pu<-merge(TE.Baseline, TE.Scenario, by="Planning unit")
compare.pu.fin<-compare.pu[1]
compare.pu.fin$em.reduction<-compare.pu$Total.x-compare.pu$Total.y
sum1<-sum(compare.pu.fin$em.reduction)
if(sum1==0){
compare.pu.fin$percentage<-0
} else {
compare.pu.fin$percentage<-(compare.pu.fin$em.reduction/sum1)*100
compare.pu.fin$percentage<-round(compare.pu.fin$percentage, digits=2)
}
colnames(compare.pu.fin)[1]<-"Planning_unit"
PLOT2<-ggplot(data=compare.pu.fin, aes(x=Planning_unit, y=percentage)) + geom_bar(stat="identity")+coord_flip()
compare.pu.total <- c("Total", sum1, 100)
compare.pu.fin$Planning_unit <- as.character(compare.pu.fin$Planning_unit)
compare.pu.fin <- rbind(compare.pu.fin, compare.pu.total)
colnames(compare.pu.fin)[1] <- 'Planning unit'
colnames(compare.pu.fin)[2] <- 'Penurunan emisi'
colnames(compare.pu.fin)[3] <- 'Persentase'

# WRITE REPORT
title<-"\\b\\fs40 LUMENS-SCIENDO Project Report\\b0\\fs20"
sub_title<-"\\b\\fs32 PERBANDINGAN PROYEKSI EMISI TERHADAP BASELINE \\b0\\fs20"
date<-paste("Date : ", as.character(Sys.Date()), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
#filename<-paste("LUMENS_Scenario", )
rtffile <- RTF("LUMENS_SCIENDO-PHB_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 1. INTISARI PERHITUNGAN EMISI DARI SCENARIO\\b0 \\fs20"))
addNewLine(rtffile)
addParagraph(rtffile, "Bagian ini menjelaskan hasil perhitungan emisi dari skenario perencanaan penggunaan lahan yang disusun dalam periode tertentu. Informasi yang dihasilkan meliputi jumlah emisi, jumlah sequestrasi dan jumlah emisi mulatif dalam periode analisa")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 Tabel risalah emisi\\b0 \\fs20"))
addNewLine(rtffile)
addTable(rtffile,TableSum.Scenario, font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 Tabel risalah emisi untuk setiap unit perencanaan\\b0 \\fs20"))
addNewLine(rtffile)
addTable(rtffile,TE.Scenario, font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 Tabel risalah sequestrasi untuk setiap unit perencanaan\\b0 \\fs20"))
addNewLine(rtffile)
addTable(rtffile,ST.Scenario, font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 2. PERBANDINGAN TERHADAP BASELINE\\b0 \\fs20"))
addNewLine(rtffile)
addParagraph(rtffile, "Bagian ini menjelaskan hasil perbandingan emisi dari skenario perencanaan penggunaan lahan yang disusun dalam periode tertentu terhadap baseline atau kondisi yang diperkirakan terjadi jika tidak ada intervensi terhadap kondisi saat ini. Informasi yang dihasilkan meliputi jumlah emisi, jumlah sequestrasi dan jumlah emisi mulatif dalam periode analisa")
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT1)
addParagraph(rtffile,"Grafik perbandingan emisi scenario terhadap baseline")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 Tabel perbandingan emisi kumulatif\\b0 \\fs20"))
addNewLine(rtffile)
addTable(rtffile,Cumulative.table, font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs24", kalimat,"\\b0 \\fs20", sep=""))
addNewLine(rtffile)
addTable(rtffile,compare.pu.fin, font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT2)
done(rtffile)
