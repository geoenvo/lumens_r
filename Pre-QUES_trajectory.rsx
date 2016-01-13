##Alpha - QUES=group
##Look_up_table=file
##raster.nodata=number 0
##statuscode=output table
##statusmessage=output table

Look_up_table="C:/LUMENS_Papua/Kab_Asmat/5_Data_tabular/Tabel_acuan_tutupan_lahan.csv"
raster.nodata= 0

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
library(rtf)
library(tcltk)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
lut.lc<-read.table(Look_up_table, header=TRUE, sep=",")

#READ LUMENS LOG FILE
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
LUMENS_temp_user <- paste(user_temp_folder,"/LUMENS/temp", sep="")
dir.create(LUMENS_temp_user, mode="0777")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)

#====READ LANDUSE DATA FROM LUMENS DATABASE====
per<-as.data.frame(ls(pattern="freq"))
n<-nrow(per)
if(n==0){
  msgBox <- tkmessageBox(title = "Pre-QUES",
                         message = "No Land Use/Cover found",
                         icon = "info",
                         type = "ok")
  quit()
}
data<-per
data.y<-NULL
for (q in 1:n) {
  data.x<-substr(as.character(factor(data[q,1])), 5, 14)
  data.y<-c(data.y,data.x)
  
}
data<-as.data.frame(data.y)

n<-nrow(data)
command1<-NULL
command2<-NULL
for(i in 1:n) {
  if (i!=n){
    command1<-paste(command1,"period", i, ",", sep="")
    command2<-paste(command2,"landuse_t", i, ",", sep="")
  } else {
    command1<-paste(command1,"period", i, sep="")
    command2<-paste(command2,"landuse_t", i, sep="")
  }
}

#===Check LUMENS Pre-QUES log file====
if (file.exists(paste(user_temp_folder,"/LUMENS/LUMENS_pre_ques.log", sep=""))) {
  log.preques<-read.table(paste(user_temp_folder,"/LUMENS/LUMENS_pre_ques.log", sep=""), sep=",", header=T, row.names=1)
  print("LUMENS Pre-QuES log file is available")
} else {
  log.preques<-data.frame(IDX=NA, 
                          MODULE=NA, 
                          DATE=NA,
                          TIME=NA,
                          LU1=NA,
                          LU2=NA,
                          PU=NA,
                          T1=NA,
                          T2=NA,
                          LOOKUP_LC=NA,
                          LOOKUP_ZONE=NA,
                          NODATA=NA,
                          ANALYSIS_OPTION=NA,
                          OUTPUT_FOLDER=NA, row.names=NULL)
}

#if pu is not exist, use p.admin.df as planning unit reference
data2<-as.data.frame(as.character(ls(pattern="pu_pu")))
n_pu<-nrow(data2)
if (n_pu==0) {
  msgBox <- tkmessageBox(title = "Pre-QUES",
                         message = "No planning unit found. Do you want to use administrative boundary as planning unit?",
                         icon = "question", 
                         type = "yesno", default="yes")
  if(as.character(msgBox)=="no"){
    quit()
  }
  ref[ref==0]<-NA
  lut.pu<-p.admin.df[2]
  lut.pu[2]<-p.admin.df[1]
  pu<-"ref"
} else {
  command3<-NULL
  for(i in 1:n_pu) {
    if (i!=n_pu){
      command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
      command3<-c(command3,command3a)
    } else {
      command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
      command3<-c(command3,command3a)
    }
  }
}

rr<-nrow(per)
command4<-NULL
for(i in 1:rr) {
  if (i!=rr){
    command4<-paste(command4,"freqLanduse_", i, ",", sep="")
  } else {
    command4<-paste(command4,"freqLanduse_", i, sep="")
  }
}
#end create command

#====SELECT DATA TO BE ANALYZED====
eval(parse(text=(paste("year<-c(", command1, ")", sep=""))))
data<-as.data.frame(cbind(data,year))
data$t1<-0
data$t2<-0
colnames(data)[1]<-"data"
data$data<-as.character(data$data)
data3<-data
a<-nrow(data3)
repeat{
  data_temp<-edit(data)
  if(sum(data_temp$t1)==1 & sum(data_temp$t2)==1){
    data_temp$sum<-data_temp$t1+data_temp$t2
    data_temp <- data_temp[which(data_temp$sum==1),]
    n_temp<-nrow(data_temp)
    if(n_temp!=0) {
      data<-data_temp
      break  
    }
  } else {
    msgBox <- tkmessageBox(title = "Pre-QUES",
                           message = "Choose data to be analyzed. Retry?",
                           icon = "question", 
                           type = "retrycancel", default="retry")
    if(as.character(msgBox)=="cancel"){
      quit()
    }
  }
}

data$t1<-NULL
data$t2<-NULL
data$sum<-NULL

n<-nrow(data)
command1<-NULL
T1<-data[1,2]
T2<-data[2,2]

#====SELECT PLANNING UNIT TO BE ANALYZED====
if(n_pu!=0){
  data2<-as.data.frame(cbind(data2,command3))
  data2$usage<-0
  colnames(data2)[1]<-"data"
  colnames(data2)[2]<-"sources"
  data2$data<-as.character(data2$data)
  data2$sources<-as.character(data2$sources)
  data2<-rbind(data2, c("ref", "Administrative", 0))
  data2$usage<-as.integer(data2$usage)  
  repeat{
    data2<-edit(data2)
    if(sum(data2$usage)==1){
      break
    } else {
      msgBox <- tkmessageBox(title = "Pre-QUES",
                             message = "Choose one data as a planning unit. Retry?",
                             icon = "question", 
                             type = "retrycancel", default="retry")
      if(as.character(msgBox)=="cancel"){
        quit()
      }
    }
  } 
  data2 <- data2[which(data2$usage==1),]
  data2$usage<-NULL
  pu<-as.character(data2[1,1])
  if(pu=="ref"){
    ref[ref==0]<-NA
    lut.pu<-p.admin.df[2]
    lut.pu[2]<-p.admin.df[1]
    pu_selected<-0
  } else {
    pu_selected<-substr(pu, 6, 7)
    eval(parse(text=(paste("lut.pu<-lut.pu", pu_selected, sep=""))))
  }
}

#====PROJECTION HANDLING====
for(j in 1:n) {
  input <- as.character(data[j,1])
  eval(parse(text=(paste(input,"[",input, "==", raster.nodata, "]<-NA", sep=""))))
  command1<-paste(command1,input, ",", sep="")
}

#====projection handling====
if (grepl("+units=m", as.character(ref@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref)[1]*res(ref)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(ref@crs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else{
  msgBox <- tkmessageBox(title = "QUES",
                         message = "Raster map projection is unknown",
                         icon = "info",
                         type = "ok")
  quit()
}

#===CHECK EXISTING RASTER BRICK OR CROSSTAB====
setwd(LUMENS_temp_user)
command1<-paste(command1,pu, sep="")
eval(parse(text=(paste("pu_name<-names(",pu[1],")", sep=''))))
check_lucdb<-FALSE
eval(parse(text=(paste("check_crosstab<-file.exists('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
eval(parse(text=(paste("check_rbrick<-file.exists('r.brick_", pu_name ,"_", T1, "_", T2, ".grd')", sep=""))))  
if(check_crosstab){
  eval(parse(text=(paste("data_merge<-read.dbf('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
  #eval(parse(text=(paste("lu.db<-read.dbf('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))
} else if(check_rbrick){
  eval(parse(text=(paste("r.brick<-brick('r.brick_", pu_name ,"_", T1, "_", T2, ".grd')", sep=""))))
  lu.db<-crosstab(r.brick,long=TRUE,useNA=FALSE,progress='-')
  check_lucdb<-TRUE
} else {
  eval(parse(text=(paste("r.brick<-brick(stack(", command1, "), filename='r.brick_",pu_name,"_",T1, "_", T2, "')", sep=""))))
  lu.db<-crosstab(r.brick,long=TRUE,useNA=FALSE,progress='-')
  check_lucdb<-TRUE
}

#====load look up table=====
lookup_lc2<-lut.lc
lookup_l<- lut.lc
lookup_z <- lut.pu
lookup_lc<- lut.lc
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_lc)<-c("ID", "CLASS")
colnames(lookup_z)<-c("ID", "ZONE")

#====CREATE INDIVIDUAL TABLE FOR EACH LANDUSE MAP====
if(check_lucdb){
  eval(parse(text=(paste("area_lc1<-freq", data[1,1], sep=""))))
  eval(parse(text=(paste("area_lc2<-freq", data[2,1], sep=""))))
  colnames(area_lc1)[1] = "ID"
  colnames(area_lc1)[2] = "COUNT_LC1"
  colnames(area_lc2)[1] = "ID"
  colnames(area_lc2)[2] = "COUNT_LC2"
  area_lc1<-merge(area_lc1,lookup_l,by="ID")
  area_lc2<-merge(area_lc2,lookup_l,by="ID")
  colnames(area_lc1)[3] = "CLASS_LC1"
  colnames(area_lc2)[3] = "CLASS_LC2"
  
  sub1.lu.db<-lu.db[n-1]
  sub2.lu.db<-lu.db[n]
  sub3.lu.db<-lu.db[n+1]
  sub4.lu.db<-lu.db[n+2]
  cross<-cbind(sub1.lu.db,sub2.lu.db,sub3.lu.db,sub4.lu.db)
  cross$Freq<-cross$Freq*Spat_res
  
  #Cross
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
  
  #eval(parse(text=(paste("write.dbf(data_merge,'lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))
} else {
  data_merge$COUNT <- data_merge$COUNT*Spat_res
}
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_z)<-c("ID", "ZONE")
eval(parse(text=(paste("area_zone<-as.data.frame(freq(", pu,"))", sep=""))))
colnames(area_zone)[1] = "ID"
colnames(area_zone)[2] = "COUNT_ZONE"
area_zone<-merge(area_zone,lookup_z,by="ID")

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
eval(parse(text=paste("landuse_tr1<-",data[1,1],sep="")))
eval(parse(text=paste("landuse_tr2<-",data[2,1],sep="")))
landuse_tr1<- reclassify(landuse_tr1, cbind(128,NA))
landuse_tr2<- reclassify(landuse_tr2, cbind(128,NA))
landuse_tr1<-ratify(landuse_tr1, filename='lu1.grd',count=TRUE,overwrite=TRUE)
landuse_tr2<-ratify(landuse_tr2, filename='lu2.grd',count=TRUE,overwrite=TRUE)
#lookup_lr<-read.table(lu_reclass, header=TRUE, sep=",")
colnames(lookup_lr)[3]="ID_L1"
levels(landuse_tr1)<-merge((levels(landuse_tr1)),lookup_lr, by="ID")
colnames(lookup_lr)[3]="ID_L2"
levels(landuse_tr2)<-merge((levels(landuse_tr2)),lookup_lr, by="ID")
landuse_tr1<-deratify(landuse_tr1,'ID_L1')
landuse_tr2<-deratify(landuse_tr2,'ID_L2')
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
  ggtitle(paste("Landuse Trajectories Map of", pu_name, T1, "-", T2 )) +
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

preques_folder<-paste(dirname(proj.file), "/QUES/PreQUES/PreQUES_traj_",pu_name,"_",data[1,2],"_",data[2,2], sep="")
dir.create(preques_folder, mode="0777")
setwd(preques_folder) 

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
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", T1)
I_O_period_2_rep<-paste("\\b","\\fs20", T2)
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

command<-paste("start ", "winword ", preques_folder, "/LUMENS_Pre-QUES_Trajectory_report.lpr", sep="" )
shell(command)
