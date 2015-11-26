##[SCIENDO]=group
##Set_Working_Directory=folder
##period1=number 2000
##period2=number 2005
##iteration=number 3
##Scenario_Directory=folder
##Abacus_Project_File=file

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

time_start <- paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
setwd(Set_Working_Directory)
cars <- list.files(Scenario_Directory, full.names=TRUE, pattern="\\.car$")

numOfCars <- length(cars)
for(i in 1:numOfCars) {
  eval(parse(text=(paste("carsLocation", i, " <- basename(as.character(cars[", i, "]))", sep=""))))  #carsLocation1 <- basename(as.character(cars[1]))
  eval(parse(text=(paste("carsName", i, " <- substr(basename(carsLocation", i, "), 1, nchar(basename(carsLocation", i, ")) - 4)", sep="")))) #carsName1 <- substr(basename(carsLocation1), 1, nchar(basename(carsLocation1)) - 4)
  
  eval(parse(text=(paste("New_Abacus_Project_file <- readLines(cars[", i, "])", sep="")))) #New_Abacus_Project_file<-readLines(file.name)
  zone_number <- as.character(New_Abacus_Project_file[5])
  zone_number <- as.data.frame(strsplit(zone_number, "="))
  zone_number <- as.numeric(as.character(zone_number[2,]))
  iteration_number <- as.character(New_Abacus_Project_file[12])
  iteration_number <- as.data.frame(strsplit(iteration_number, "="))
  iteration_number <- as.numeric(as.character(iteration_number[2,]))
  
  baris <- as.numeric(pmatch('Summary', New_Abacus_Project_file))
  baris <- baris+4
  baris <- baris+zone_number+3
  baris <- baris+zone_number+3
  baris <- baris+zone_number+3
  
  #Emission Total  (ton CO2-eq/year)
  TE<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
  write.table(TE, paste(Set_Working_Directory,  "/TE.txt",sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
  eval(parse(text=(paste("TE", i, " <- read.table(paste(Set_Working_Directory, '/TE.txt',sep=''), sep='\t')", sep="")))) #TE1<-read.table(paste(Set_Working_Directory,  "/TE.txt",sep=""),sep="\t")
  eval(parse(text=(paste("TE", i, "$V", iteration+3, " <- NULL", sep="" ))))
  eval(parse(text=(paste("TE", i, "[,2:(iteration+2)] <- as.numeric(sapply(TE", i, "[,2:(iteration+2)], as.character))", sep=""))))
  #eval(parse(text=(paste("TE", i, "[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(TE", i, "[,2:(iteration+2)]))))", sep="" )))) #TE1[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(TE1[,2:(iteration+2)]))))
  file.remove(paste(Set_Working_Directory,  "/TE.txt", sep=""))
  
  baris<-baris+zone_number+3
  baris<-baris+zone_number+3
  
  #Sequestration Total (ton CO2-eq/year)
  ST<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
  write.table(ST, paste(Set_Working_Directory, "/ST.txt",sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
  eval(parse(text=(paste("ST", i, " <- read.table(paste(Set_Working_Directory, '/ST.txt', sep=''), sep='\t')", sep=""))))
  eval(parse(text=(paste("ST", i, "$V", iteration+3, " <- NULL", sep=""))))
  eval(parse(text=(paste("ST", i, "[,2:(iteration+2)] <- as.numeric(sapply(ST", i, "[,2:(iteration+2)], as.character))", sep=""))))
  #eval(parse(text=(paste("ST", i, "[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(ST", i, "[,2:(iteration+2)]))))", sep=""))))
  file.remove(paste(Set_Working_Directory,  "/ST.txt", sep=""))
  
  baris<-baris+zone_number+3
  baris2<-as.numeric(pmatch('Land Use System\tEmission Total  (ton CO2-eq/year)', New_Abacus_Project_file))
  baris<-baris2+2
  baris2<-as.numeric(pmatch('Emissions Associated with Benefit Less Than Threshold ', New_Abacus_Project_file))
  baris<-baris2+4
  baris<-baris+iteration_number+4
  
  #Total
  baris2<-as.numeric(nrow(as.data.frame(New_Abacus_Project_file)))
  Summary<-as.data.frame(New_Abacus_Project_file[baris:baris2])
  write.table(Summary, paste(Set_Working_Directory,  "/Summary.txt", sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
  eval(parse(text=(paste("Summary", i, " <- read.table(paste(Set_Working_Directory, '/Summary.txt', sep=''), sep='\t')", sep="")))) #Summary<-read.table(paste(Set_Working_Directory,  "/Summary.txt",sep=""),sep="\t")
  eval(parse(text=(paste("Summary", i, "$V", iteration+3, " <- NULL", sep="" ))))
  eval(parse(text=(paste("Summary", i, "[,2:(iteration+2)] <- as.numeric(sapply(Summary", i, "[,2:(iteration+2)], as.character))", sep=""))))
  #eval(parse(text=(paste("Summary", i, "[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(Summary",  i, "[,2:(iteration+2)]))))", sep="")))) 
  file.remove(paste(Set_Working_Directory,  "/Summary.txt", sep=""))
}

# SCENARIO COMPARISON
# THIS PART IS ORIGINALLY WROTE TO ACCESS ABACUS .CAR FILE
#====Historical Baseline====

New_Abacus_Project_file <- readLines(Abacus_Project_File)
zone_number <- as.character(New_Abacus_Project_file[5])
zone_number <- as.data.frame(strsplit(zone_number, "="))
zone_number <- as.numeric(as.character(zone_number[2,]))
iteration_number <- as.character(New_Abacus_Project_file[12])
iteration_number <- as.data.frame(strsplit(iteration_number, "="))
iteration_number <- as.numeric(as.character(iteration_number[2,]))

baris <- as.numeric(pmatch('Summary', New_Abacus_Project_file))
baris <- baris+4
baris <- baris+zone_number+3
baris <- baris+zone_number+3
baris <- baris+zone_number+3

#Emission Total  (ton CO2-eq/year)
TE <- as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(TE, paste(Set_Working_Directory, "/TEHist.txt", sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
TEHist <- read.table(paste(Set_Working_Directory, "/TEHist.txt", sep=""), sep="\t")
eval(parse(text=(paste("TEHist$V", iteration+3, " <- NULL", sep="" ))))
TEHist[,2:(iteration+2)]<-as.numeric(sapply(TEHist[,2:(iteration+2)], as.character))
#TEHist[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(TEHist[,2:(iteration+2)]))))
file.remove(paste(Set_Working_Directory, "/TEHist.txt", sep=""))

baris <- baris+zone_number+3
baris <- baris+zone_number+3

#Sequestration Total (ton CO2-eq/year)
ST <- as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(ST, paste(Set_Working_Directory, "/STHist.txt",sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
STHist <- read.table(paste(Set_Working_Directory, "/STHist.txt",sep=""), sep="\t")
eval(parse(text=(paste( "STHist$V" ,iteration+3, " <- NULL", sep="" ))))
STHist[,2:(iteration+2)]<-as.numeric(sapply(STHist[,2:(iteration+2)], as.character))
#STHist[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(STHist[,2:(iteration+2)]))))
file.remove(paste(Set_Working_Directory, "/STHist.txt", sep=""))

baris <- baris+zone_number+3
baris2 <- as.numeric(pmatch('Land Use System\tEmission Total  (ton CO2-eq/year)', New_Abacus_Project_file))
baris <- baris2+2
baris2 <- as.numeric(pmatch('Emissions Associated with Benefit Less Than Threshold ', New_Abacus_Project_file))
baris <- baris2+4
baris <- baris+iteration_number+4

#Total
baris2 <- as.numeric(nrow(as.data.frame(New_Abacus_Project_file)))
Summary <- as.data.frame(New_Abacus_Project_file[baris:baris2])
write.table(Summary, paste(Set_Working_Directory, "/SummaryHist.txt", sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
SummaryHist <- read.table(paste(Set_Working_Directory, "/SummaryHist.txt", sep=""), sep="\t")
eval(parse(text=(paste( "SummaryHist$V", iteration+3, " <- NULL", sep="" ))))
SummaryHist[,2:(iteration+2)]<-as.numeric(sapply(SummaryHist[,2:(iteration+2)], as.character))
#SummaryHist[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(SummaryHist[,2:(iteration+2)]))))
file.remove(paste(Set_Working_Directory, "/SummaryHist.txt",sep=""))

#BASELINE RESULT
TableSumHist <- round((SummaryHist[(1:12),(2:(iteration+2))]),digits=2)
TableSumHist <- cbind((SummaryHist[1]),TableSumHist)
TableSumHist = TableSumHist[-2,]
TableSumHist = TableSumHist[-3,]
TableSumHist = TableSumHist[-4,]
TableSumHist = TableSumHist[-5,]
TableSumHist = TableSumHist[-6,]
TableSumHist = TableSumHist[-7,]
TableSum.Baseline<-TableSumHist
TEtemp <- round((TEHist[,(2:(iteration+2))]),digits=2)
STtemp <- round((STHist[,(2:(iteration+2))]),digits=2)
TEHist <- cbind((TEHist[1]), TEtemp)
STHist <- cbind((STHist[1]), STtemp)
TEHist$Total <- rowSums(TEHist[,2:(iteration+2)])
STHist$Total <- rowSums(STHist[,2:(iteration+2)])
TEHist.total <- sum(TEHist$Total)
STHist.total <- sum(STHist$Total)
TEHist$Percentage <- round(((TEHist$Total/TEHist.total)*100),digits=2)
STHist$Percentage <- round(((STHist$Total/STHist.total)*100),digits=2)
TEHist <- TEHist[order(-TEHist$Percentage),]
STHist <- STHist[order(-STHist$Percentage),]
TE.Baseline <- TEHist
ST.Baseline <- STHist

#SCENARIO RESULT
for(i in 1:numOfCars){
  eval(parse(text=(paste("TableSum <- round((Summary", i, "[(1:12), (2:(iteration+2))]), digits=2)", sep="")))) #TableSum <- round((Summary[(1:12), (2:(iteration+2))]), digits=2)
  eval(parse(text=(paste("TableSum <- cbind((Summary", i, "[1]), TableSum)", sep="")))) #TableSum <- cbind((Summary[1]), TableSum)
  TableSum = TableSum[-2,]
  TableSum = TableSum[-3,]
  TableSum = TableSum[-4,]
  TableSum = TableSum[-5,]
  TableSum = TableSum[-6,]
  TableSum = TableSum[-7,]
  eval(parse(text=(paste("TableSum.Scenario", i, " <- TableSum", sep="")))) #TableSum.Scenario <- TableSum
  
  eval(parse(text=(paste("TEtemp <- round((TE", i, "[,(2:(iteration+2))]),digits=2)", sep="")))) #TEtemp <- round((TE[,(2:(iteration+2))]),digits=2)
  eval(parse(text=(paste("TE", i, " <- cbind((TE", i, "[1]),TEtemp)", sep="")))) #TE <- cbind((TE[1]),TEtemp)
  eval(parse(text=(paste("TE", i, "$Total <- rowSums(TE", i, "[,2:(iteration+2)])", sep="")))) #TE$Total <- rowSums(TE[,2:(iteration+2)])
  eval(parse(text=(paste("TE.total", i, " <- sum(TE", i, "$Total)", sep="")))) #TE.total <- sum(TE$Total)
  eval(parse(text=(paste("TE", i, "$Percentage <- round(((TE", i, "$Total/TE.total", i, ")*100),digits=2)", sep="")))) #TE$Percentage <- round(((TE$Total/TE.total)*100),digits=2)
  eval(parse(text=(paste("TE", i, " <- TE", i, "[order(-TE", i, "$Percentage),]", sep="")))) #TE <- TE[order(-TE$Percentage),]
  eval(parse(text=(paste("TE.Scenario", i, " <- TE", i, sep="")))) #TE.Scenario <- TE
  
  eval(parse(text=(paste("STtemp <- round((ST", i, "[,(2:(iteration+2))]),digits=2)", sep="")))) #STtemp <- round((ST[,(2:(iteration+2))]),digits=2)
  eval(parse(text=(paste("ST", i, " <- cbind((ST", i, "[1]),STtemp)", sep="")))) #ST <- cbind((ST[1]),STtemp)
  eval(parse(text=(paste("ST", i, "$Total <- rowSums(ST", i, "[,2:(iteration+2)])", sep="")))) #ST$Total <- rowSums(ST[,2:(iteration+2)])
  eval(parse(text=(paste("ST.total", i, " <- sum(ST", i, "$Total)", sep="")))) #ST.total <- sum(ST$Total)
  eval(parse(text=(paste("ST", i, "$Percentage <- round(((ST", i, "$Total/ST.total", i, ")*100),digits=2)", sep="")))) #ST$Percentage <- round(((ST$Total/ST.total)*100),digits=2)
  eval(parse(text=(paste("ST", i, " <- ST", i, "[order(-ST", i, "$Percentage),]", sep="")))) #ST <- ST[order(-ST$Percentage),]
  eval(parse(text=(paste("ST.Scenario", i, " <- ST", i, sep="")))) #TE.Scenario <- TE
}

#====CREATE CUMULATIVE SUMMARY====
Cum.summary<-as.data.frame(cumsum(t(TableSum.Baseline[1,2:(iteration+2)])))
CumSummaryNetEm<-as.data.frame(cumsum(t(TableSum.Baseline[5,2:(iteration+2)])))
CumSummarySeq<-as.data.frame(cumsum(t(TableSum.Baseline[2,2:(iteration+2)])))
for(i in 1:numOfCars){
  eval(parse(text=(paste("Cum.summary.sc", i, "<-as.data.frame(cumsum(t(TableSum.Scenario", i, "[1,2:(iteration+2)])))", sep="")))) #Cum.summary.sc<-as.data.frame(cumsum(t(TableSum.Scenario[1,2:(iteration+2)])))
  eval(parse(text=(paste("CumSummaryNetEmSc", i, "<-as.data.frame(cumsum(t(TableSum.Scenario", i, "[5,2:(iteration+2)])))", sep=""))))
  eval(parse(text=(paste("CumSummarySeqSc", i, "<-as.data.frame(cumsum(t(TableSum.Scenario", i, "[2,2:(iteration+2)])))", sep=""))))
}

#====CREATE PERIODE OF SIMULATION====
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
for (i in 1:(nrow(Cum.summary))){
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
colnames(ST.Baseline) <- c(Colname.ST.TE)

for(i in 1:numOfCars){
  eval(parse(text=(paste("colnames(TE.Scenario", i, ") <- c(Colname.ST.TE)", sep="")))) #colnames(TE.Scenario) <- c(Colname.ST.TE)
  eval(parse(text=(paste("colnames(ST.Scenario", i, ") <- c(Colname.ST.TE)", sep="")))) #colnames(ST.Scenario) <- c(Colname.ST.TE)
}

Colname.Summary<-append("Keterangan", Periode)
colnames(TableSum.Baseline) <- c(Colname.Summary)
TableSum.Baseline$Keterangan<-as.character(TableSum.Baseline$Keterangan)
TableSum.Baseline[1,1]<-"Emisi Per-Ha Area (ton CO2-eq/ha.tahun)"
TableSum.Baseline[2,1]<-"Sequestrasi Per-Ha Area (ton CO2-eq/ha.tahun)"
TableSum.Baseline[3,1]<-"Emisi Total (ton CO2-eq/tahun)"
TableSum.Baseline[4,1]<-"Sequestrasi Total (ton CO2-eq/tahun)"
TableSum.Baseline[5,1]<-"Emisi Bersih Per-Ha Area (ton CO2-eq/ha.tahun)"
TableSum.Baseline[6,1]<-"Emisi Bersih (ton CO2-eq/tahun)"

for(i in 1:numOfCars){
  eval(parse(text=(paste("colnames(TableSum.Scenario", i, ") <- c(Colname.Summary)", sep="")))) #colnames(TableSum.Scenario) <- c(Colname.Summary)
  eval(parse(text=(paste("TableSum.Scenario", i, "$Keterangan<-as.character(TableSum.Scenario", i, "$Keterangan)", sep=""))))
  eval(parse(text=(paste("TableSum.Scenario", i, "[1,1]<-'Emisi Per-Ha Area (ton CO2-eq/ha.tahun)'", sep=""))))
  eval(parse(text=(paste("TableSum.Scenario", i, "[2,1]<-'Sequestrasi Per-Ha Area (ton CO2-eq/ha.tahun)'", sep=""))))
  eval(parse(text=(paste("TableSum.Scenario", i, "[3,1]<-'Emisi Total (ton CO2-eq/tahun)'", sep=""))))
  eval(parse(text=(paste("TableSum.Scenario", i, "[4,1]<-'Sequestrasi Total (ton CO2-eq/tahun)'", sep=""))))
  eval(parse(text=(paste("TableSum.Scenario", i, "[5,1]<-'Emisi Bersih Per-Ha Area (ton CO2-eq/ha.tahun)'", sep=""))))
  eval(parse(text=(paste("TableSum.Scenario", i, "[6,1]<-'Emisi Bersih (ton CO2-eq/tahun)'", sep=""))))
}

#====CUMULATIVE TABLE====
Cumulative.table <- cbind(as.data.frame(Periode), Cum.summary)
colnames(Cumulative.table)[2] <- "BASELINE Cum.em (tonCO2/ha.yr)"
CumulativeTableNetEm <- cbind(as.data.frame(Periode), CumSummaryNetEm)
colnames(CumulativeTableNetEm)[2] <- "BASELINE Cum.NetEm (tonCO2/ha.yr)"
CumulativeTableSeq <- cbind(as.data.frame(Periode), CumSummarySeq)
colnames(CumulativeTableSeq)[2] <- "BASELINE Cum.Seq (tonCO2/ha.yr)"

for(i in 1:numOfCars){
  eval(parse(text=(paste("Cumulative.table <- cbind(Cumulative.table, Cum.summary.sc", i, ")", sep="")))) #Cumulative.table<-cbind(Cumulative.table, Cum.summary.sc)
  eval(parse(text=(paste("a <- carsName", i, sep=""))))
  b <- paste(a, " Cum.em (tonCO2/ha.yr)", sep="")
  eval(parse(text=(paste("colnames(Cumulative.table)[", 3+i-1, "] <- b", sep="")))) #colnames(Cumulative.table)[3]<-'SCENARIO Cum.em (tonCo2/ha.yr)'
  
  eval(parse(text=(paste("CumulativeTableNetEm <- cbind(CumulativeTableNetEm, CumSummaryNetEmSc", i, ")", sep="")))) #Cumulative.table<-cbind(Cumulative.table, Cum.summary.sc)
  eval(parse(text=(paste("a <- carsName", i, sep=""))))
  b <- paste(a, " Cum.NetEm (tonCO2/ha.yr)", sep="")
  eval(parse(text=(paste("colnames(CumulativeTableNetEm)[", 3+i-1, "] <- b", sep="")))) #colnames(Cumulative.table)[3]<-'SCENARIO Cum.em (tonCo2/ha.yr)'
  
  eval(parse(text=(paste("CumulativeTableSeq <- cbind(CumulativeTableSeq, CumSummarySeqSc", i, ")", sep="")))) #Cumulative.table<-cbind(Cumulative.table, Cum.summary.sc)
  eval(parse(text=(paste("a <- carsName", i, sep=""))))
  b <- paste(a, " Cum.Seq (tonCO2/ha.yr)", sep="")
  eval(parse(text=(paste("colnames(CumulativeTableSeq)[", 3+i-1, "] <- b", sep="")))) #colnames(Cumulative.table)[3]<-'SCENARIO Cum.em (tonCo2/ha.yr)'
}

#====PREPARE TABLE FOR GRAPH AND CREATE GRAPH====
Cum.summary.temp<-Cum.summary
colnames(Cum.summary.temp)[1]<-"Cum.em"
CumSummaryNetEmTemp<-CumSummaryNetEm
colnames(CumSummaryNetEmTemp)[1]<-"Cum.NetEm"
CumSummarySeqTemp<-CumSummarySeq
colnames(CumSummarySeqTemp)[1]<-"Cum.Seq"

class1<-"BASELINE"
Cum.summary.temp<-cbind(class1, Cum.summary.temp)
Cum.summary.temp<-cbind(as.data.frame(Periode), Cum.summary.temp)
CumSummaryNetEmTemp<-cbind(class1, CumSummaryNetEmTemp)
CumSummaryNetEmTemp<-cbind(as.data.frame(Periode), CumSummaryNetEmTemp)
CumSummarySeqTemp<-cbind(class1, CumSummarySeqTemp)
CumSummarySeqTemp<-cbind(as.data.frame(Periode), CumSummarySeqTemp)

for(i in 1:numOfCars){
  eval(parse(text=(paste("Cum.summary.temp.sc", i, " <- Cum.summary.sc", i, sep="")))) #Cum.summary.temp.sc<-Cum.summary.sc1
  eval(parse(text=(paste("colnames(Cum.summary.temp.sc", i, ")[1] <- 'Cum.em'", sep="")))) #colnames(Cum.summary.temp.sc)[1] <- 'Cum.em'
  eval(parse(text=(paste("class1 <- carsName", i, sep="")))) #class1<-"SCENARIO"
  eval(parse(text=(paste("Cum.summary.temp.sc", i, " <- cbind(class1,Cum.summary.temp.sc", i, ")", sep="")))) #Cum.summary.temp.sc <- cbind(class1,Cum.summary.temp.sc)
  eval(parse(text=(paste("Cum.summary.temp.sc", i, " <- cbind(as.data.frame(Periode), Cum.summary.temp.sc", i, ")", sep="")))) #Cum.summary.temp.sc <- cbind(as.data.frame(Periode), Cum.summary.temp.sc)
  
  eval(parse(text=(paste("CumSummaryNetEmScTemp", i, " <- CumSummaryNetEmSc", i, sep="")))) #Cum.summary.temp.sc<-Cum.summary.sc1
  eval(parse(text=(paste("colnames(CumSummaryNetEmScTemp", i, ")[1] <- 'Cum.NetEm'", sep="")))) #colnames(Cum.summary.temp.sc)[1] <- 'Cum.em'
  eval(parse(text=(paste("class1 <- carsName", i, sep="")))) #class1<-"SCENARIO"
  eval(parse(text=(paste("CumSummaryNetEmScTemp", i, " <- cbind(class1,CumSummaryNetEmScTemp", i, ")", sep="")))) #Cum.summary.temp.sc <- cbind(class1,Cum.summary.temp.sc)
  eval(parse(text=(paste("CumSummaryNetEmScTemp", i, " <- cbind(as.data.frame(Periode), CumSummaryNetEmScTemp", i, ")", sep="")))) #Cum.summary.temp.sc <- cbind(as.data.frame(Periode), Cum.summary.temp.sc)
  
  eval(parse(text=(paste("CumSummarySeqScTemp", i, " <- CumSummarySeqSc", i, sep="")))) #Cum.summary.temp.sc<-Cum.summary.sc1
  eval(parse(text=(paste("colnames(CumSummarySeqScTemp", i, ")[1] <- 'Cum.Seq'", sep="")))) #colnames(Cum.summary.temp.sc)[1] <- 'Cum.em'
  eval(parse(text=(paste("class1 <- carsName", i, sep="")))) #class1<-"SCENARIO"
  eval(parse(text=(paste("CumSummarySeqScTemp", i, " <- cbind(class1,CumSummarySeqScTemp", i, ")", sep="")))) #Cum.summary.temp.sc <- cbind(class1,Cum.summary.temp.sc)
  eval(parse(text=(paste("CumSummarySeqScTemp", i, " <- cbind(as.data.frame(Periode), CumSummarySeqScTemp", i, ")", sep="")))) #Cum.summary.temp.sc <- cbind(as.data.frame(Periode), Cum.summary.temp.sc)
  
  if(i==1){
    eval(parse(text=(paste("Cum.summary.tab <- rbind(Cum.summary.temp, Cum.summary.temp.sc", i, ")", sep="")))) #Cum.summary.tab <- rbind(Cum.summary.temp,Cum.summary.temp.sc)
    eval(parse(text=(paste("CumSummaryNetEmTab <- rbind(CumSummaryNetEmTemp, CumSummaryNetEmScTemp", i, ")", sep="")))) 
    eval(parse(text=(paste("CumSummarySeqTab <- rbind(CumSummarySeqTemp, CumSummarySeqScTemp", i, ")", sep="")))) 
  } else {
    eval(parse(text=(paste("Cum.summary.tab <- rbind(Cum.summary.tab, Cum.summary.temp.sc", i, ")", sep=""))))
    eval(parse(text=(paste("CumSummaryNetEmTab <- rbind(CumSummaryNetEmTab, CumSummaryNetEmScTemp", i, ")", sep=""))))
    eval(parse(text=(paste("CumSummarySeqTab <- rbind(CumSummarySeqTab, CumSummarySeqScTemp", i, ")", sep=""))))
  }
}
PLOT1 <- ggplot(data=Cum.summary.tab, aes(x=Periode, y=Cum.em, group=class1, colour=class1)) + ylab("Emisi kumulatif (tonCO2/ha.yr)") + geom_line() + geom_point()+ geom_point(size=3, fill="white")
PLOT2 <- ggplot(data=CumSummaryNetEmTab, aes(x=Periode, y=Cum.NetEm, group=class1, colour=class1)) + ylab("Emisi bersih kumulatif (tonCO2/ha.yr)") + geom_line() + geom_point()+ geom_point(size=3, fill="white")
PLOT3 <- ggplot(data=CumSummarySeqTab, aes(x=Periode, y=Cum.Seq, group=class1, colour=class1)) + ylab("Sequestrasi kumulatif (tonCO2/ha.yr)") + geom_line() + geom_point()+ geom_point(size=3, fill="white")

#====CALCULATE EMISSION REDUCTION====
for(i in 1:numOfCars){
  em.reduction1 <- Cumulative.table[nrow(Cumulative.table),2]
  eval(parse(text=(paste("em.reduction2 <- Cumulative.table[nrow(Cumulative.table),", 3+i-1, "]", sep="")))) #em.reduction2<-Cumulative.table[nrow(Cumulative.table),3]
  eval(parse(text=(paste("em.reduction.sc", i, ".ton <- em.reduction1-em.reduction2", sep=""))))
  eval(parse(text=(paste("em.reduction.sc", i, " <- ((em.reduction1-em.reduction2)/em.reduction1)*100", sep="")))) #em.reduction.sc1<-((em.reduction1-em.reduction2)/em.reduction1)*100
  eval(parse(text=(paste("em.reduction.sc", i, " <- round(em.reduction.sc", i, ", digits=2)", sep="")))) #em.reduction<-round(em.reduction, digits=2)
  
  #kalimat<-paste("Persentase penurunan emisi terhadap baseline adalah " , em.reduction, "%", sep="")
  a<-"Persentase skenario penurunan emisi "
  eval(parse(text=(paste("b <- carsName", i, sep=""))))
  c<-" terhadap baseline adalah "
  d<-" %"
  eval(parse(text=(paste("kalimat", i, "<-paste(a, b, c, em.reduction.sc", i, ", d, sep='')", sep=""))))
}

#tablePersentaseEmCum <- data.frame()
#for(i in 1:numOfCars){
#  eval(parse(text=(paste("cars_temp <- carsName", i, sep='')))) 
#  eval(parse(text=(paste("em_red_temp <- em.reduction.sc", i, sep=''))))
#  table_emisi_temp <- cbind(cars_temp, em_red_temp)
#  colnames(table_emisi_temp)[1] <- 'Scenario'
#  colnames(table_emisi_temp)[2] <- 'Percentage'
#  tablePersentaseEmCum <- rbind(tablePersentaseEmCum, table_emisi_temp)
#}
#tablePersentaseEmCum$Percentage<-as.numeric(as.matrix(tablePersentaseEmCum$Percentage)) 

#====PREPARE COMPARISON OF PLANNING UNIT====
for(i in 1:numOfCars){
  eval(parse(text=(paste("compare.pu", i, " <- merge(TE.Baseline, TE.Scenario", i, ", by='Planning unit')", sep="")))) #compare.pu<-merge(TE.Baseline, TE.Scenario, by="Planning unit")
  eval(parse(text=(paste("compare.pu.fin", i, " <- compare.pu", i, "[1]", sep="")))) #compare.pu.fin<-compare.pu[1]
  eval(parse(text=(paste("compare.pu.fin", i, "$em.reduction <- compare.pu", i, "$Total.x-compare.pu", i, "$Total.y", sep="")))) #compare.pu.fin$em.reduction<-compare.pu$Total.x-compare.pu$Total.y
  eval(parse(text=(paste("sum1 <- sum(compare.pu.fin", i, "$em.reduction)", sep="")))) #sum1<-sum(compare.pu.fin$em.reduction)
  if(sum1==0){
    eval(parse(text=(paste("compare.pu.fin", i, "$percentage <- 0", sep=""))))
  } else {
    eval(parse(text=(paste("compare.pu.fin", i, "$percentage <- (compare.pu.fin", i, "$em.reduction/sum1)*100", sep=""))))
    eval(parse(text=(paste("compare.pu.fin", i, "$percentage <- round(compare.pu.fin", i, "$percentage, digits=2)", sep=""))))
  }
  
  eval(parse(text=(paste("colnames(compare.pu.fin", i, ")[1] <- 'Planning_unit'", sep="")))) #colnames(compare.pu.fin)[1]<-'Planning_unit'
  eval(parse(text=(paste("PLOT2_", i, " <- ggplot(data=compare.pu.fin", i, ", aes(x=Planning_unit, y=percentage)) + xlab('Planning unit') + ylab('Persentase') +  geom_bar(stat='identity')+coord_flip()", sep="")))) #PLOT2<-ggplot(data=compare.pu.fin, aes(x=Planning_unit, y=percentage)) + geom_bar(stat="identity")+coord_flip()
  
  compare.pu.total <- c("Total", sum1, 100)
  eval(parse(text=(paste("compare.pu.fin", i, "$Planning_unit <- as.character(compare.pu.fin", i, "$Planning_unit)", sep=""))))
  eval(parse(text=(paste("compare.pu.fin", i, " <- rbind(compare.pu.fin", i, ", compare.pu.total)", sep="")))) #compare.pu.fin15<-rbind(compare.pu.fin15, compare.pu.total)
  
  eval(parse(text=(paste("colnames(compare.pu.fin", i, ")[1] <- 'Planning unit'", sep=""))))
  eval(parse(text=(paste("colnames(compare.pu.fin", i, ")[2] <- 'Penurunan emisi'", sep=""))))
  eval(parse(text=(paste("colnames(compare.pu.fin", i, ")[3] <- 'Persentase'", sep=""))))
}

#====CALCULATE REDUCTION FROM NET EMISSION====
for(i in 1:numOfCars){
  netEmBaseline <- sum(TableSum.Baseline[6,2:(iteration+2)])*interval
  eval(parse(text=(paste("netEmScenario <- sum(TableSum.Scenario", i, "[6,2:(iteration+2)])*interval", sep="")))) 
  eval(parse(text=(paste("netEmRedSc", i, "Ton <- netEmBaseline-netEmScenario", sep=""))))
  eval(parse(text=(paste("netEmRedSc", i, " <- (netEmRedSc", i, "Ton/netEmBaseline)*100", sep="")))) 
  
  eval(parse(text=(paste("tableNetEmRedTemp <- data.frame(carsName", i, ", netEmRedSc", i, ", netEmRedSc", i, "Ton)  ", sep=""))))
  colnames(tableNetEmRedTemp)[1] <- 'Skenario'
  colnames(tableNetEmRedTemp)[2] <- 'Persen'
  colnames(tableNetEmRedTemp)[3] <- 'Ton'
  if(i==1){
    tableNetEmRed <- tableNetEmRedTemp
  } else {
    tableNetEmRed <- rbind(tableNetEmRed, tableNetEmRedTemp)
  }
}

tableNetEmRed$Persen <- round(tableNetEmRed$Persen, 4)
tableNetEmRed$Ton <- round(tableNetEmRed$Ton)

plotPersentaseEmCum<-ggplot(data=tableNetEmRed, aes(x=Skenario, y=Persen, fill=Skenario)) + geom_bar(stat="identity") + coord_flip() + 
  geom_text(data=tableNetEmRed, aes(x=Skenario, y=Persen, label=Persen),size=3, vjust=0.1) +
  ggtitle(paste("Perbandingan persentase penurunan emisi bersih kumulatif" )) + guides(fill=FALSE) + ylab("Skenario") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

tableNetEmRed$Skenario <- as.character(tableNetEmRed$Skenario)
rowRedNetEm <- c("Total", sum(tableNetEmRed$Persen), sum(tableNetEmRed$Ton))
tableNetEmRed <- rbind(tableNetEmRed, rowRedNetEm)
colnames(tableNetEmRed)[1] <- 'Skenario aksi mitigasi'
colnames(tableNetEmRed)[2] <- 'dalam %'
colnames(tableNetEmRed)[3] <- 'dalam Ton'

#====WRITE REPORT====
title<-"\\b\\fs32 LUMENS-SCIENDO - PERBANDINGAN PROYEKSI EMISI TERHADAP BASELINE\\b0\\fs20"
sub_title1<-"\\b\\fs28 RAD GRK \\b0\\fs20"
sub_title2<-"\\b\\fs28 - 4.1. Skenario Baseline (Forward Looking) \\b0\\fs20"
sub_title3<-"\\b\\fs28 - 4.2. Skenario Mitigasi \\b0\\fs20"
date<-paste("Date : ", as.character(Sys.Date()), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
rtffile <- RTF("LUMENS_SCIENDO-PHB_report.lpr", font.size=9)
addParagraph(rtffile, "\\b\\fs32 Hasil Analisis\\b0\\fs20")
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, title)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, sub_title1)
addParagraph(rtffile, sub_title2)
addParagraph(rtffile, sub_title3)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Hasil analisis pada modul ini digunakan untuk meyusun skenario perubahan lahan dan dampaknya terhadap emisi dimasa yang akan datang. Skenario yang disusun dapat digunakan untuk menyusun baseline berdasarkan pendekatan forward looking atau rencana pembangunan di suatu wilayah. Pada tahap ini pengguna diharapkan dapat membuat proyeksi perubahan penggunaan lahan masa yang akan datang mendasarkan pada dokumen perencanaan pembangunan yang sudah diundangkan seperti RTRW, RPJP, RPJM, Renstra atau rencana pembangunan yang lain.")
addNewLine(rtffile)
addParagraph(rtffile, "Skenario lain yang dapat disusun pada modul ini adalah untuk skenario aksi mitigasi penurunan emisi. Skenario aksi penurunan emisi dimaksudkan untuk menghasilkan aktivitas berbasis lahan yang akan dapat mengurangi emisi masa yang akan datang apabila dibandingkan dengan baseline yang sudah terpilih. Untuk mendapatkan aksi mitigasi yang sesuai diharapkan pengguna memiliki informasi yang cukup terkait aksi mitigasi yang terdapat dalam dokumen perencaan pembangunan atau pihak lain yang akan dilaksanakan atau rencana baru yang diusulkan yang dapat diupayakan hingga pada tahap implementasinya.")
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 1. INTISARI PERHITUNGAN EMISI DARI SKENARIO\\b0 \\fs20"))
addNewLine(rtffile)
addParagraph(rtffile, "Bagian ini menjelaskan hasil perhitungan emisi dari skenario perencanaan penggunaan lahan yang disusun dalam periode tertentu. Informasi yang dihasilkan meliputi jumlah emisi, jumlah sequestrasi dan jumlah emisi bersih dalam periode analisis.")
addNewLine(rtffile)

for(i in 1:numOfCars){
  eval(parse(text=(paste("a <- carsName", i, sep=""))))
  addParagraph(rtffile, paste("\\b \\fs32 Tabel Risalah Emisi Bersih",a,"\\b0 \\fs20"))
  addNewLine(rtffile)
  eval(parse(text=(paste("addTable(rtffile,TableSum.Scenario", 1, ", font.size=8)", sep=""))))
  #addTable(rtffile,TableSum.Scenario, font.size=8)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs32 Tabel Risalah Emisi",a,"untuk setiap unit perencanaan\\b0 \\fs20"))
  addNewLine(rtffile)
  eval(parse(text=(paste("addTable(rtffile,TE.Scenario", 1, ", font.size=8)", sep=""))))
  #addTable(rtffile,TE.Scenario, font.size=8)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs32 Tabel Risalah Sequestrasi",a,"untuk setiap unit perencanaan\\b0 \\fs20"))
  addNewLine(rtffile)
  eval(parse(text=(paste("addTable(rtffile,ST.Scenario", 1, ", font.size=8)", sep=""))))
  #addTable(rtffile,ST.Scenario, font.size=8)
  addNewLine(rtffile)
}

addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 2. PERBANDINGAN TERHADAP BASELINE\\b0 \\fs20"))
addNewLine(rtffile)
addParagraph(rtffile, "Bagian ini menjelaskan hasil perbandingan emisi dari skenario perencanaan penggunaan lahan yang disusun dalam periode tertentu terhadap baseline atau kondisi yang diperkirakan terjadi jika tidak ada intervensi terhadap kondisi saat ini. Informasi yang dihasilkan meliputi jumlah kumulatif dari emisi, emisi bersih, dan sequestrasi dalam periode analisis")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 2.a Perbandingan Emisi Skenario Terhadap Baseline\\b0 \\fs20"))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Grafik\\b0 \\fs20"))
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT1)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel\\b0 \\fs20"))
addTable(rtffile,Cumulative.table, font.size=8)
addNewLine(rtffile)


addParagraph(rtffile, paste("\\b \\fs32 2.b Perbandingan Sequestrasi Skenario Terhadap Baseline\\b0 \\fs20"))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Grafik\\b0 \\fs20"))
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT3)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel\\b0 \\fs20"))
addTable(rtffile,CumulativeTableSeq, font.size=8)
addNewLine(rtffile)

addParagraph(rtffile, paste("\\b \\fs32 2.c. Perbandingan Emisi Bersih Skenario Terhadap Baseline\\b0 \\fs20"))
addNewLine(rtffile)
addParagraph(rtffile, "Grafik ini merupakan salah satu informasi penting untuk menyajikan atau membandingkan perhitungan emisi antara skenario dengan baselinenya. Skenario yang memberikan dampak terhadap perbedaan angka emisi akan ditunjukan dengan adanya perbedaan bentuk grafik yang jelas. Sementara skenario yang tidak memberikan dampak perbedaan emisi yang signifikan akan ditunjukan dengan garfik yang menempel.")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Grafik\\b0 \\fs20"))
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT2)
addNewLine(rtffile)

# addParagraph(rtffile,"Grafik perbandingan emisi bersih scenario terhadap baseline")
# addNewLine(rtffile)
# addParagraph(rtffile, paste("\\b \\fs32 Tabel perbandingan emisi bersih kumulatif\\b0 \\fs20"))
# addNewLine(rtffile)
# addTable(rtffile,CumulativeTableNetEm, font.size=8)
# addNewLine(rtffile)
# 
# for(i in 1:numOfCars){
#   eval(parse(text=(paste("kalimat <- kalimat", i, sep=""))))
#   addParagraph(rtffile, paste("\\b \\fs24", kalimat,"\\b0 \\fs20", sep=""))
#   addNewLine(rtffile)
#   eval(parse(text=(paste("addTable(rtffile,compare.pu.fin", i, ", font.size=8)", sep=""))))
#   #addTable(rtffile,compare.pu.fin, font.size=8)
#   addNewLine(rtffile)
#   eval(parse(text=(paste("addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT2_", i, ")", sep=""))))
#   #addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT2)
#   addNewLine(rtffile)
# }

addParagraph(rtffile, paste("\\b \\fs32 3. TABEL PENURUNAN EMISI BERSIH\\b0 \\fs20"))
addNewLine(rtffile)
addTable(rtffile,tableNetEmRed, font.size=8)
addNewLine(rtffile)

addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plotPersentaseEmCum)
addParagraph(rtffile, paste("Perbandingan persentase emisi bersih kumulatif tahun ", period1, "-", t_1, sep=""))
addNewLine(rtffile)

done(rtffile)

command<-paste("start ", "winword ", Set_Working_Directory, "/LUMENS_SCIENDO-PHB_report.lpr", sep="" )
shell(command)