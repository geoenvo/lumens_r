##[QUES]=group
##Look_up_table=file
##passfilenames

library(tiff)
library(foreign)
library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(grid)
library(ggplot2)
library(spatial.tools)
library(rtf)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
lut.peat<-read.table(Look_up_table, header=TRUE, sep=",",)

#READ LUMENS LOG FILE
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)

#READ PEAT DATA
check.peat<-as.integer(exists("peat.index"))
if (check.peat==0) {
library(tcltk)
tkmessageBox(title = "LUMENS process halted", message = "Peat area is not defines. Please add peat map into active database", icon = "error", type = "ok")
} else {
# SELECTING AVAILABLE QUES-C ANALYSIS
QUESC_list<-as.data.frame(ls(pattern="QUESC_database"))
colnames (QUESC_list) [1]<-"Data"
QUESC_list$Usage<-0
repeat{
QUESC_list<-edit(QUESC_list)
if(sum(QUESC_list$Usage)==1){
break
}
}
QUESC_list <- QUESC_list[which(QUESC_list$Usage==1),]
QUESC_list$Usage<-NULL
sel.QUESC<-as.character(QUESC_list[1,1])
peat_t1<-as.integer(substr(sel.QUESC, 16:19, 19))
peat_t2<-as.integer(substr(sel.QUESC, 21:24, 25))
text1<-paste("QUESC_Peat_database_", as.character(peat_t1),"-", as.character(peat_t2), sep="")
eval(parse(text=(paste("QUESC_Peat_database_", as.character(peat_t1),"_", as.character(peat_t2), "<-", sel.QUESC, sep=""))))
eval(parse(text=(paste("cross<-r.brick_", as.character(peat_t1),"_", as.character(peat_t2), sep=""))))
cross<-stack(cross,Peat_1)
cross<-as.data.frame(crosstab(cross))

colnames(cross)[1] ="ID_LC1"
colnames(cross)[2] = "ID_LC2"
colnames(cross)[3] = "ZONE"
colnames(cross)[4] = "PEAT"
colnames(cross)[5] = "COUNT"
colnames(lut.c)[1]="ID_LC1"
colnames(lut.c)[2]="LC_t1"
colnames(lut.c)[3]="CARBON_t1"
data_merge <- merge(cross,lut.c,by="ID_LC1")
colnames(lut.c)[1]="ID_LC2"
colnames(lut.c)[2]="LC_t2"
colnames(lut.c)[3]="CARBON_t2"
data_merge <- as.data.frame(merge(data_merge,lut.c,by="ID_LC2"))
colnames(lut.pu)[1]="ZONE"
colnames(lut.pu)[2]="Z_NAME"
data_merge <- as.data.frame(merge(data_merge,lut.pu,by="ZONE"))
colnames(lut.peat)[1]="ID_LC1"
colnames(lut.peat)[2]="LC_PEAT_t1"
colnames(lut.peat)[3]="PEAT_EM_t1"
data_merge <- as.data.frame(merge(data_merge,lut.peat,by="ID_LC1"))
colnames(lut.peat)[1]="ID_LC2"
colnames(lut.peat)[2]="LC_PEAT_t2"
colnames(lut.peat)[3]="PEAT_EM_t2"
data_merge <- as.data.frame(merge(data_merge,lut.peat,by="ID_LC2"))
data_merge$LC_PEAT_t1<-NULL
data_merge$LC_PEAT_t2<-NULL
data_merge$PEAT[is.na(data_merge$PEAT)] <- 0
data_merge$PEAT_EM_t1[which(data_merge$PEAT!=1)]<-0
data_merge$PEAT_EM_t2[which(data_merge$PEAT!=1)]<-0
rm(cross)

# Calculate carbon stock changes
Spat_res<-(res(ref)^2)/10000
data_merge$CARBON_t1<-data_merge$CARBON_t1
data_merge$CARBON_t2<-data_merge$CARBON_t2
data_merge$ck_em<-as.integer(data_merge$CARBON_t1>data_merge$CARBON_t2)
data_merge$ck_sq<-as.integer(data_merge$CARBON_t1<data_merge$CARBON_t2)
data_merge$em<-(data_merge$CARBON_t1-data_merge$CARBON_t2)*data_merge$ck_em*data_merge$COUNT*3.67*Spat_res
data_merge$sq<-(data_merge$CARBON_t2-data_merge$CARBON_t1)*data_merge$ck_sq*data_merge$COUNT*3.67*Spat_res
data_merge$LU_CHG <- do.call(paste, c(data_merge[c("LC_t1", "LC_t2")], sep = " to "))
data_merge$null<-0
data_merge$nullCek<-data_merge$em+data_merge$sq
# Calculate below ground emission from peat
period<-peat_t2-peat_t1
data_merge$em_peat<-((data_merge$PEAT_EM_t1+data_merge$PEAT_EM_t2)/2)*period*data_merge$COUNT*Spat_res
data_merge$em_tot<-data_merge$em+data_merge$em_peat

#generate area_zone lookup and calculate min area
area_zone<-as.data.frame(freq(pu_pu1))
colnames(area_zone)[1]<-"ID"
colnames(area_zone)[2]<-"COUNT"
colnames(lut.pu)[1]<-"ID"
area_zone<-merge(area_zone, lut.pu, by="ID")
area<-min(sum(area_zone$COUNT), sum(data_merge$COUNT))


#create QUES-C database

#make zonal statistics database
lg<-length(unique(data_merge$ZONE))
zone_lookup<-area_zone
data_zone<-area_zone
data_zone$Z_CODE<-toupper(abbreviate(data_zone$Z_NAME))
for(i in 1:lg){
data_z<-data_merge[which(data_merge$ZONE == i),]
data_zone$Avg_C_t1[which(data_zone$ID == i)]<-sum(data_z$CARBON_t1*data_z$COUNT)/sum(data_z$COUNT)
data_zone$Avg_C_t2[which(data_zone$ID == i)]<-sum(data_z$CARBON_t2*data_z$COUNT)/sum(data_z$COUNT)
data_zone$Rate_em[which(data_zone$ID == i)]<-sum(data_z$em)/(sum(data_z$COUNT)*period)
data_zone$Rate_seq[which(data_zone$ID == i)]<-sum(data_z$sq)/(sum(data_z$COUNT)*period)
data_zone$Rate_em_peat[which(data_zone$ID == i)]<-sum(data_z$em_peat)/(sum(data_z$COUNT)*period)
data_zone$Rate_em_tot[which(data_zone$ID == i)]<-sum(data_z$em_tot)/(sum(data_z$COUNT)*period)
}
data_zone[,5:8]<-round(data_zone[,5:8],digits=3)

# Additional statistic
data_merge2<-subset(data_merge, COUNT>0)

em_above<-sum(data_merge2$em)
em_below<-sum(data_merge2$em_peat)
rate_above<-em_above/(sum(data_merge2$COUNT)*period)
rate_below<-em_below/(sum(data_merge2$COUNT)*period)
percent_below<-(em_below/(em_above+em_below))*100

# Overall emission
overal_em<-cbind(em_above, em_below, rate_above, rate_below, percent_below)
colnames(overal_em)[1]<-"Emission (tCO2)"
colnames(overal_em)[2]<-"Peat Emission (tCO2)"
colnames(overal_em)[3]<-"Emission Rate (tCO2/ha.yr)"
colnames(overal_em)[4]<-"Peat Emission Rate (tCO2/ha.yr)"
colnames(overal_em)[5]<-"Percent Peat Emission (%)"

#Largest source
source.melt <- melt(data = data_merge2, id.vars=c('LU_CHG'), measure.vars=c('em_peat'))
source.cast <- dcast(data = source.melt, formula = LU_CHG ~ ., fun.aggregate = sum)
source.cast2 <- source.cast[order(-source.cast$.),]
source.cast2sel<-head(source.cast2, n=5)
colnames(source.cast2sel)[1]<-"LU_Change"
colnames(source.cast2sel)[2]<-"Peat_Emission_in_tCO"
summ<-sum(source.cast2sel[2])

dirQUESC_peat<-paste(dirname(proj.file), "/QUES/QUES-C/QUESC_peat_analysis_",as.character(peat_t1),"-",as.character(peat_t2), sep="")
dir.create(dirQUESC_peat, mode="0777")
setwd(dirQUESC_peat)

text<-paste("QUESC_peat_emission_database_", peat_t1, "_", peat_t2, sep="")
write.dbf(data_merge2, paste(text, ".dbf", sep=""))
eval(parse(text=(paste(text, "<-data_merge2", sep=""))))
eval(parse(text=(paste("resave(", text, ', file="',proj.file, '")', sep=""))))

#rtf report file
title<-"\\b\\fs40 LUMENS-QUES-C Peat Project Report\\b0\\fs20"
sub_title<-"\\b\\fs32 PERHITUNGAN EMISI PADA LAHAN GAMBUT\\b0\\fs20"
date<-paste("Date : ", date(), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
#area_name_rep<-paste("\\b", "\\fs20", Location, "\\b0","\\fs20")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
rtffile <- RTF("LUMENS_QUES-C_PEAT_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
#addParagraph(rtffile, "Rekonsiliasi unit perencanaan adalah proses untuk menyelesaikan tumpang-tindih ijin dengan merujuk pada peta acuan/referensi fungsi. Rekonsiliasi dilakukan dengan menganalisa kesesuaian fungsi antara data-data ijin dengan data referensi. Data ijin yang dimaksud datapat berupa data konsesi pengelolaan hutan, ijin perkebunan, ijin tambang dan lain sebagainya, Sedangkan data referensi yang digunakan dapat berupa data rencana tata ruang atau penunjukan kawasan. ")
addNewLine(rtffile)

addParagraph(rtffile, "\\b \\fs32 HASIL PERHITUNGAN EMISI GAMBUT\\b0 \\fs20")
addParagraph(rtffile, line)
#addParagraph(rtffile, "Pada bagian ini ditunjukkan hasil proses rekonsiliasi dengan menggunakan tambahan unit perencanaan ")
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, "\\b \\fs18 Tabel Perhitungan Emisi Gambut Keseluruhan\\b0 \\fs18")
addNewLine(rtffile)
addTable(rtffile, overal_em)
addNewLine(rtffile)
addParagraph(rtffile, "\\b \\fs18 Tabel Perhitungan Emisi Gambut Pada Unit Perencanaan\\b0 \\fs18")
addNewLine(rtffile)

addTable(rtffile, data_zone,  font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, "\\b \\fs18 Tabel Perubahan Penggunaan Lahan Yang Menyebabkan Emisi Gambut\\b0 \\fs18")
addNewLine(rtffile)

addTable(rtffile, source.cast2sel)
addNewLine(rtffile)

done(rtffile)

command<-paste("start ", "winword ", dirQUESC_peat, "/LUMENS_QUES-C_PEAT_report.lpr", sep="" )
shell(command)
}
