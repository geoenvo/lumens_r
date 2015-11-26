##[QUES]=group
##Wdir=folder
##Aname=string Bungo
##period1=number 2000
##period2=number 2005
##lu1=raster
##lu2=raster
##zonel=raster
#lu_key=file
#lu_lut=file
#lu_leg=file
##lu_landuse=file
##lu_zone=file
##lu_trajectories_final=output raster
##PreQUES_traj_database=output table
##Overall_trajectories=output table
##Zone_trajectories=output table
##passfilenames


library(raster)
library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(foreign)
library(spatial.tools)
library(markdown)
library(knitr)
library(rtf)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#set project properties
setwd(Wdir)
Area_name=Aname
tab_title<-as.data.frame(Aname)
Year_T1=period1
Year_T2=period2
Period=Year_T1-Year_T2
proj_prop<-as.data.frame(Aname)
proj_prop$Year_T1<-Year_T1
proj_prop$Year_T2<-Year_T2
proj_prop$period <- do.call(paste, c(proj_prop[c("Year_T1", "Year_T2")], sep = " - "))

#load datasets (land use t1, land use t2, zone)
landuse1 <- raster(lu1)
landuse2 <- raster(lu2)
zone <- raster(zonel)

#load look up table (internal to PRE-QUES)
#lookup_traj<-read.table(lu_key, header=TRUE, sep=",")
#name_traj<-read.table(lu_lut, header=TRUE, sep=",")
#leg_traj<-read.table(lu_leg, header=TRUE, sep=",")

#substitute lookup table internal
trj<-c(11:17,22:27,32:37,42:44,46:47,52:57,62:67,77,88)
lookup_traj<-as.data.frame(trj)
remove(trj)
lookup_traj$Traj<-c("Stable natural forest","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to infrastructure","Other")
lookup_traj$Def<-c("Stable forest", "Forest degradation", "Deforestation","Deforestation","Deforestation","Deforestation","Deforestation","Stable forest", "Deforestation","Deforestation","Deforestation","Deforestation","Deforestation", "Reforestation", "Other","Other","Other","Other","Reforestation", "Other","Other","Other","Other","Reforestation","Other","Other","Other","Other","Other","Reforestation", "Other","Other","Other","Other","Other","Other","Other", "Others")
name_traj<-lookup_traj
name_traj$ID_trf<-c(5,3,7,6,1,4,2,3,7,6,1,4,2,8,7,6,1,4,2,8,7,6,4,2,8,7,6,1,4,2,8,7,6,1,4,2,2,9)
ID_T<-c(1:9)
leg_traj<-as.data.frame(ID_T)
remove(ID_T)
leg_traj$Trajectories<-c("Loss to cropland","Loss to infrastructure","Loss to logged-over forest","Loss to bare land and abandoned","Stable natural forest","Recovery to agroforest","Recovery to tree cropping","Recovery to forest","Other")

lu_class<-c(1,2,3,4,5,6,7,8)
lu_class<-as.data.frame(lu_class)
lu_class$Rec_LU<-c("Hutan primer", "Hutan sekunder", "Tanaman pohon monokultur", "Tanaman pohon campuran", "Tanaman pertanian semusim", "Semak, rumput dna lahan terbuka", "Pemukiman", "Lain-lain")


#load look up table (user input)
lookup_l<- read.table(lu_landuse, header=TRUE, sep=",",)
lookup_z <- read.table(lu_zone, header=TRUE, sep=",",)
#lookup_lr<-read.table(lu_reclass, header=TRUE, sep=",")

lookup_l2<-lookup_l
lookup_l2$ID_L<-0
countrow<-nrow(lookup_l2)
x<-rep("---", times=countrow)
xx<-x
lookup_lr_edit<-cbind(lookup_l2,x,xx)
xxx<-rep("---", times=countrow-(nrow(lu_class)))
xxxx<-xxx
V<-cbind(xxx,xxxx)
colnames(V)[1]<-"lu_class"
colnames(V)[2]<-"Rec_LU"
lookup_lr_edit2<-rbind(lu_class,V)
#lookup_lr_edit3<-cbind(lookup_lr_edit2, x,xx)
lookup_lr<-cbind(lookup_lr_edit, lookup_lr_edit2)
lookup_lr<-edit(lookup_lr)
lookup_lr<-subset(lookup_lr, select=c(1, 2, 3))


#Update project properties
Data_T1<-lu1
Data_T2<-lu2
Lookup_LU<-lu_landuse
Lookup_Zone<-lu_zone
proj_prop$Data_T1<-Data_T1
proj_prop$Data_T2<-Data_T2
proj_prop$Lookup_LU<-Lookup_LU
proj_prop$Lookup_Zone<-Lookup_Zone

#set same extent
landuse2<-spatial_sync_raster(landuse2, landuse1, method = "ngb")
zone<-spatial_sync_raster(zone, landuse1, method = "ngb")

# set raster attribute table (RAT)
landuse1<-ratify(landuse1, filename='landuse1.grd',count=TRUE,overwrite=TRUE)
landuse2<-ratify(landuse2, filename='landuse2.grd',count=TRUE,overwrite=TRUE)
zone<-ratify(zone, filename='ratify.grd',count=TRUE,overwrite=TRUE)

#create land use change database
area_lc1<-as.data.frame(levels(landuse1))
area_lc2<-as.data.frame(levels(landuse2))
area_zone<-as.data.frame(levels(zone))
area<-min(sum(area_zone$COUNT), sum(area_lc1$COUNT), sum(area_lc2$COUNT))
levels(landuse1)<-merge((levels(landuse1)),lookup_l,by="ID")
levels(landuse2)<-merge((levels(landuse2)),lookup_l,by="ID")
colnames(lookup_z)[1] = "ID"
levels(zone) <- merge(area_zone,lookup_z,by="ID")
area_lc1<-as.data.frame(levels(landuse1))
area_lc2<-as.data.frame(levels(landuse2))
area_zone<-as.data.frame(levels(zone))
colnames(area_lc1)[2] = "COUNT_LC1"
colnames(area_lc1)[3] = "CLASS_LC1"
colnames(area_lc2)[2] = "COUNT_LC2"
colnames(area_lc2)[3] = "CLASS_LC2"
cross <- as.data.frame(crosstab((stack(landuse1,landuse2,zone))))
colnames(cross)[1] ="ID_LC1"
colnames(cross)[2] = "ID_LC2"
colnames(cross)[3] = "ZONE"
colnames(cross)[4] = "COUNT"
colnames(lookup_l)[1]="ID_LC1"
colnames(lookup_l)[2]="LC_t1"
data_merge <- merge(cross,lookup_l,by="ID_LC1")
colnames(lookup_l)[1]="ID_LC2"
colnames(lookup_l)[2]="LC_t2"
data_merge <- as.data.frame(merge(data_merge,lookup_l,by="ID_LC2"))
colnames(lookup_z)[1]="ZONE"
colnames(lookup_z)[2]="Z_NAME"
data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))

#create trajectories database
colnames(lookup_lr)[1]="ID_LC1"
colnames(lookup_lr)[2]="CLASS1"
colnames(lookup_lr)[3]="ID_L1"
data_merge_tr<-as.data.frame(merge(data_merge,lookup_lr, by="ID_LC1"))
colnames(lookup_lr)[1]="ID_LC2"
colnames(lookup_lr)[2]="CLASS2"
colnames(lookup_lr)[3]="ID_L2"
data_merge_tr<-as.data.frame(merge(data_merge_tr,lookup_lr, by="ID_LC2"))
data_merge_tr$CLASS1<-data_merge_tr$CLASS2<-NULL
data_merge_tr$T1<-data_merge_tr$ID_L1*10
data_merge_tr$T2<-data_merge_tr$ID_L2
data_merge_tr$TR<-data_merge_tr$T1+data_merge_tr$T2
colnames(lookup_traj)[1]="TR"
PreQUES_traj_database<-as.data.frame(merge(data_merge_tr,lookup_traj, by="TR"))
PreQUES_traj_database$Traj_Code<-toupper(abbreviate(PreQUES_traj_database$Traj))
colnames(lookup_lr)[1]="ID"
colnames(lookup_lr)[3]="ID_L"


#create trajectories map
landuse_tr1<-landuse1
landuse_tr2<-landuse2
#lookup_lr<-read.table(lu_reclass, header=TRUE, sep=",")
levels(landuse_tr1)<-merge((levels(landuse_tr1)),lookup_lr, by="ID")
levels(landuse_tr2)<-merge((levels(landuse_tr2)),lookup_lr, by="ID")
landuse_tr1<-deratify(landuse_tr1,'ID_L')
landuse_tr2<-deratify(landuse_tr2,'ID_L')
lu_trajectories<-overlay(landuse_tr1,landuse_tr2,fun=function(x,y){return((x*10)+y)})
lu_trajectories<-ratify(lu_trajectories, filename='lu_trajectories.grd',count=TRUE,overwrite=TRUE)
colnames(name_traj)[1]="ID"
levels(lu_trajectories)<-merge((levels(lu_trajectories)),name_traj,by='ID')
lu_trajectories_final<-deratify(lu_trajectories,'ID_trf')
lu_trajectories_final<-ratify(lu_trajectories_final, filename='lu_trajectories_final.grd',count=TRUE,overwrite=TRUE)
colnames(leg_traj)[1]="ID"
levels(lu_trajectories_final)<-merge((levels(lu_trajectories_final)),leg_traj,by='ID')

#calculate summary statistics by zone and overall
PreQUES_traj_database.melt <- melt(data = PreQUES_traj_database, id.vars=c('Z_NAME','Traj', 'Traj_Code'), measure.vars=c('COUNT'))
PreQUES_traj_database.zone <- dcast(data = PreQUES_traj_database.melt, formula = Z_NAME ~ Traj_Code, fun.aggregate = sum)
PreQUES_traj_database.overal <- dcast(data = PreQUES_traj_database.melt, formula = Traj ~ ., fun.aggregate = sum)

PreQUES_traj_forest.melt <- melt(data = PreQUES_traj_database, id.vars=c('Z_NAME','Def'), measure.vars=c('COUNT'))
PreQUES_traj_forest.zone <- dcast(data = PreQUES_traj_forest.melt, formula = Z_NAME ~ Def, fun.aggregate = sum)
PreQUES_traj_forest.overal <- dcast(data = PreQUES_traj_forest.melt, formula = Def ~ ., fun.aggregate = sum)

PreQUES_traj_drive.melt <- melt(data = PreQUES_traj_database, id.vars=c('Traj','Def'), measure.vars=c('COUNT'))
PreQUES_traj_drive.zone <- dcast(data = PreQUES_traj_drive.melt, formula = Traj ~ Def, fun.aggregate = sum)


#plot trajectories map
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- brewer.pal(11, "Spectral")
myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)

LU1 <- rasterToPoints(lu_trajectories_final);
LU1 <- as.data.frame(LU1)
colnames(LU1) <- c("X","Y","ID_trf")
LU1<-LU1[which(LU1$ID_trf != 0),]
lu.lab<-name_traj
LU1<-merge(LU1, lu.lab, by="ID_trf")
LU1$ID_trf<-as.factor(LU1$ID_trf)
myColors.lu <- myColors[1:length(unique(LU1$ID_trf))]
names(myColors.lu) <- unique(LU1$Traj)
ColScale.lu<-scale_fill_manual(name="Landuse Change", values = myColors.lu )
plot.LU1  <- ggplot(data=LU1) + geom_raster(aes(x=LU1$X, y=LU1$Y, fill=LU1$Traj)) +
ColScale.lu + coord_equal() +
ggtitle(paste("Landuse Trajectories Map of", Area_name, period1, "-", period2 )) +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))

colnames(PreQUES_traj_database.melt)<-c("Zone", "Trajectories","Abbrev", "variable", "Area"); #rename column names
plot_traj<-ggplot(data=PreQUES_traj_database.melt,aes(factor(Zone),Area,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)')+coord_flip()+
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

colnames(PreQUES_traj_forest.melt)<-c("Zone", "Forest_Change","variable", "Area"); #rename column names
plot_def<-ggplot(data=PreQUES_traj_forest.melt,aes(factor(Zone),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

colnames(PreQUES_traj_drive.melt)<-c("Trajectories", "Forest_Change","variable", "Area"); #rename column names
plot_drive<-ggplot(data=PreQUES_traj_drive.melt,aes(factor(Trajectories),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))


colnames(PreQUES_traj_database.overal)<-c("Trajectories", "Area (Ha)")
colnames(PreQUES_traj_database.zone)[1]<-c("Trajectories")
colnames(PreQUES_traj_forest.overal)<-c("Forest cover changes", "Area (Ha)")

#export data
Overall_trajectories<-PreQUES_traj_database.overal
Zone_trajectories<-PreQUES_traj_database.zone
write.dbf(PreQUES_traj_database, "PreQUES_traj_database.dbf")
write.dbf(PreQUES_traj_database.overal, "Overall_trajectories.dbf")
write.dbf(PreQUES_traj_database.zone, "Zone_trajectories.dbf")
writeRaster(lu_trajectories_final, filename="lulcc_trajectories_map.tif", format="GTiff", overwrite=TRUE)

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Pre QUES Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 2: Land Use Trajectory Analysis \\b0\\fs20"
#date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", Area_name, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
rtffile <- RTF("LUMENS_Pre-QUES_Trajectory_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Analisa perubahan tutupan lahan dilakukan untuk mengetahui kecenderungan perubahan tutupan lahan di suatu daerah pada satu kurun waktu. Analisa ini dilakukan dengan menggunakan data peta tutupan lahan pada dua periode waktu yang berbeda. Selain itu, dengan memasukkan data unit perencanaan kedalam proses analisa, dapat diketahui kecenderungan perubahan tutupan lahan pada masing-masing kelas unit perencanaan yang ada. Informasi yang dihasilkan melalui analisa ini dapat digunakan dalam proses perencanaan untuk berbagai hal. Diantaranya adalah: menentukan prioritas pembangunan, mengetahui faktor pemicu perubahan penggunaan lahan, merencanakan skenario pembangunan di masa yang akan datang, dan lain sebagainya.")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs24 ALUR PERUBAHAN PENGGUNAAN LAHAN\\b0 \\fs24", sep=""))
addNewLine(rtffile)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Alur perubahan penggunaan lahan merupakan ringkasan keseluruhan tipe rangkaian perubahan penggunaan lahan yang mungkin terjadi di sebuah daerah. Kategori besar dari alur perubahan lahan dibagi menjadi dua jenis yaitu Loss of tree cover dan recovery of tree cover")
addNewLine(rtffile)

addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot.LU1)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 1. Peta Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_traj)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 2. Grafik Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 1. Luas Area Kelompok Perubahan Penutupan Lahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addTable(rtffile,PreQUES_traj_database.overal,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 2. Luas Area Kelompok Perubahan Lahan di \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 Tiap Unit Perencanaan \\b0 \\fs20", sep=" "))
addTable(rtffile,PreQUES_traj_database.zone,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs24 PERUBAHAN TUTUPAN HUTAN\\b0 \\fs24", sep=""))
addNewLine(rtffile)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Salah satu bentuk alur perubahan penggunaan lahan yang paling banyak mendapatkan perhatian adalah alur perubahan hutan alam menjadi tipe tutupan lahan lainnya (deforestasi) dan perubahan hutan alam primer menjadi hutan alam sekunder (degradasi). Bagian ini memperlihatkan hasil analisa LUMENS terhadap perubahan tutupan hutan di sebuah daerah")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 3. Grafik Perubahan Tutupan Hutan di Berbagai Zona Perencanaan\\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_def)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 3. Luas deforestasi \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addTable(rtffile,PreQUES_traj_forest.overal,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 4. Luas deforestasi berdasarkan zonasi \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addTable(rtffile,PreQUES_traj_forest.zone,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 4. Grafik Perubahan Tutupan Hutan dan alur perubahan yang menyebabkannya\\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_drive)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 5. Luas deforestasi berdasarkan alur perubahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addTable(rtffile,PreQUES_traj_drive.zone,font.size=8)
addNewLine(rtffile)
done(rtffile)

command<-paste("start ", "winword ", Wdir, "/LUMENS_Pre-QUES_Trajectory_report.lpr", sep="" )
shell(command)
