##[SCIENDO]=group
##Set_Working_Directory=folder
##Abacus_Project_File=file
##period1= number 2005
##period2= number 2010

library(pander)
library(knitr)
library(markdown)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(foreign)
library(rtf)

#THIS PART IS ORIGINALLY WROTE TO ACCESS ABACUS .CAR FILE

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

setwd(Set_Working_Directory)
period <- period2-period1
New_Abacus_Project_file<-readLines(Abacus_Project_File,)
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
write.table(TE, paste(Set_Working_Directory, "/TE.txt",sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
TE <- read.table(paste(Set_Working_Directory, "/TE.txt",sep=""), sep="\t")
eval(parse(text=(paste( "TE$V" ,iteration_number+3, " <- NULL", sep="" ))))
TE[,2:(iteration_number+2)] <- as.numeric(as.character(as.factor(unlist(TE[,2:(iteration_number+2)]))))
file.remove(paste(Set_Working_Directory,  "/TE.txt",sep=""))

baris <- baris+zone_number+3
baris <- baris+zone_number+3

#Sequestration Total (ton CO2-eq/year)
ST <- as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(ST, paste(Set_Working_Directory, "/ST.txt",sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
ST <- read.table(paste(Set_Working_Directory, "/ST.txt",sep=""), sep="\t")
eval(parse(text=(paste("ST$V", iteration_number+3, " <- NULL", sep="" ))))
ST[,2:(iteration_number+2)] <- as.numeric(as.character(as.factor(unlist(ST[,2:(iteration_number+2)]))))
file.remove(paste(Set_Working_Directory, "/ST.txt",sep=""))

baris <- baris+zone_number+3
baris2 <- as.numeric(pmatch('Land Use System\tEmission Total  (ton CO2-eq/year)', New_Abacus_Project_file))
baris <- baris2+2
baris2 <- as.numeric(pmatch('Emissions Associated with Benefit Less Than Threshold ', New_Abacus_Project_file))
baris <- baris2+4
baris <- baris+iteration_number+4

#Total
baris2 <- as.numeric(nrow(as.data.frame(New_Abacus_Project_file)))
Summary <- as.data.frame(New_Abacus_Project_file[baris:baris2])
write.table(Summary, paste(Set_Working_Directory, "/Summary.txt", sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
Summary <- read.table(paste(Set_Working_Directory, "/Summary.txt", sep=""), sep="\t")
eval(parse(text=(paste("Summary$V", iteration_number+3, " <- NULL", sep="" ))))
Summary[,2:(iteration_number+2)] <- as.numeric(as.character(as.factor(unlist(Summary[,2:(iteration_number+2)]))))
file.remove(paste(Set_Working_Directory, "/Summary.txt", sep=""))

NewID <- seq(1,(iteration_number+1))
NewData <- Summary[1,2:(iteration_number+2)]
NewData <- cumsum(t(NewData))
row.number <- length(NewData)
t_1 <- period2
t_2 <- t_1+period
Period.db <- as.data.frame(NULL)
for ( i in 1:(row.number-1)){
  period.int <- paste(t_1,"-",t_2, sep="")
  Period.db <- c(Period.db,period.int)
  t_1 <- t_1+period
  t_2 <- t_1+period
}
Period.db <- as.character(Period.db)
NewYear <- paste(as.character(period1),as.character(period2),sep="-")

SL_overall_data <- c("Cumulative emission (CO2 eq/(ha.yr))")
Period.db <- append(NewYear,Period.db)
SL_overall_data <- data.frame(SL_overall_data, NewID, NewData)
colnames(SL_overall_data)[1] <- "Parameters"
colnames(SL_overall_data)[2] <- "variable"
colnames(SL_overall_data)[3] <- "value"

plot1 <- ggplot(SL_overall_data,aes(variable,value,group=1,fill=Parameters))+ geom_line(colour="red")+
  geom_point(colour="red", size=4, shape=21, fill="white") +
  geom_text(data=SL_overall_data, aes(x=variable, y=value, label=round(value, 1)),size=3, hjust=1.5,vjust=-0.5) +
  scale_x_discrete(breaks=SL_overall_data$variable ,labels=Period.db) +
  xlab('Periode') +  ylab('Emisi kumulatif') +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))

TableSum <- round((Summary[(1:12), (2:(iteration_number+2))]), digits=2)
TableSum <- cbind((Summary[1]), TableSum)
CName <- append("Parameters", Period.db)
colnames(TableSum) <- c(CName)
TableSum = TableSum[-2,]
TableSum = TableSum[-3,]
TableSum = TableSum[-4,]
TableSum = TableSum[-5,]
TableSum = TableSum[-6,]
TableSum = TableSum[-7,]

SL_overall_data$value <- round(SL_overall_data$value, digits=2)
Cum.em <- as.data.frame(cbind(Period.db, SL_overall_data$value))
colnames(Cum.em)[1] <- "Periode"
colnames(Cum.em)[2] <- "Cumulative emission rate (CO2eq/ha.yr)"

TE1 <- round((TE[, (2:(iteration_number+2))]), digits=2)
ST1 <- round((ST[, (2:(iteration_number+2))]), digits=2)
TE <- cbind((TE[1]), TE1)
ST <- cbind((ST[1]), ST1)

colnames(TE) <- c(CName)
colnames(ST) <- c(CName)
TE$Total <- rowSums(TE[, 2:(iteration_number+2)])
ST$Total <- rowSums(ST[, 2:(iteration_number+2)])
TE.total <- sum(TE$Total)
ST.total <- sum(ST$Total)
TE$Percentage <- round(((TE$Total/TE.total)*100), digits=2)
ST$Percentage <- round(((ST$Total/ST.total)*100), digits=2)

TE <- TE[order(-TE$Percentage),]
ST <- ST[order(-ST$Percentage),]

CumNetEmissionTon <- c("Net Emission (ton CO2-eq)")
CumNetDataTon <- Summary[11,2:(iteration_number+2)]*5
CumNetDataTon <- cumsum(t(CumNetDataTon))
CumNetDataTon <- data.frame(CumNetEmissionTon, NewID, CumNetDataTon)
colnames(CumNetDataTon)[1] <- "Parameters"
colnames(CumNetDataTon)[2] <- "variable"
colnames(CumNetDataTon)[3] <- "value"

plot2 <- ggplot(CumNetDataTon,aes(variable,value,group=1,fill=Parameters))+ geom_line(colour="red")+
  geom_point(colour="red", size=4, shape=21, fill="white") +
  geom_text(data=CumNetDataTon, aes(x=variable, y=value, label=round(value, 1)),size=3, hjust=1.5,vjust=-0.5) +
  scale_x_discrete(breaks=CumNetDataTon$variable ,labels=Period.db) +
  xlab('Periode') +  ylab('Emisi bersih kumulatif') +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))

CumNetData <- Summary[9,2:(iteration_number+2)]
CumNetData <- round(cumsum(t(CumNetData)), digits=2)
CumNetEmission <- as.data.frame(cbind(Period.db, round(CumNetDataTon$value), CumNetData))

colnames(CumNetEmission)[1] <- "Periode"
colnames(CumNetEmission)[2] <- "Net Emission (ton CO2-eq)"
colnames(CumNetEmission)[3] <- "Cumulative net emission rate (CO2eq/ha.yr)"

# WRITE REPORT
title<-"\\b\\fs32 LUMENS-SCIENDO Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules : Proyeksi Baseline Emisi Historis \\b0\\fs20"
date<-paste("Date : ", as.character(Sys.Date()), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
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
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot1)
addParagraph(rtffile, paste("\\b \\fs20 Prediksi emisi kumulatif dengan baseline historis\\b0 \\fs20 "))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Emisi kumulatif (ton Co2eq/ha.yr) \\b0 \\fs20"))
addTable(rtffile,Cum.em)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Intisari perhitungan emisi historis \\b0 \\fs20"))
addTable(rtffile,TableSum, font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Prediksi emisi pada unit perencanaan (ton CO2eq/yr) \\b0 \\fs20"))
addTable(rtffile,TE, font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Prediksi sequestrasi pada unit perencanaan (ton CO2eq/yr) \\b0 \\fs20"))
addTable(rtffile,ST, font.size=8)
addNewLine(rtffile)

addParagraph(rtffile, paste("\\b \\fs20 Grafik perbandingan emisi bersih kumulatif dalam periode \\b0 \\fs20"))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot2)
addParagraph(rtffile, paste("\\b \\fs20 Tabel emisi bersih kumulatif \\b0 \\fs20"))
addTable(rtffile,CumNetEmission)
addNewLine(rtffile)

done(rtffile)
