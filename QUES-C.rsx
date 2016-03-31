##Alpha - QUES=group
##landuse_1=string
##landuse_2=string
##planning_unit=string
##lookup_c=string
##raster.nodata=number 0

landuse_1="LC1990"
landuse_2="LC2000"
planning_unit="PUR_final_reconciliation"
lookup_c="carbon"
raster.nodata=0

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

#CREATE FUNCTION TO GET OBJECT FROM RDB
get_from_rdb <- function(symbol, filebase, envir =parent.frame()){
  lazyLoad(filebase = filebase, envir = envir, filter = function(x) x == symbol)
}

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

#SET PATH OF DATA DIRECTORY 
data_dir<-paste(dirname(proj.file), "/DATA/", sep="")
#setwd(data_dir)

get_from_rdb(symbol=paste("list_of_data_luc"), filebase=paste(data_dir, "land_use_cover", sep=""))
get_from_rdb(symbol=paste("list_of_data_pu"), filebase=paste(data_dir, "planning_unit", sep=""))
get_from_rdb(symbol=paste("list_of_data_lut"), filebase=paste(data_dir, "lookup_table", sep=""))

data_luc1<-list_of_data_luc[which(list_of_data_luc$RST_NAME==landuse_1),]
data_luc2<-list_of_data_luc[which(list_of_data_luc$RST_NAME==landuse_2),]
data_pu<-list_of_data_pu[which(list_of_data_pu$RST_NAME==planning_unit),]
data_lut<-list_of_data_lut[which(list_of_data_lut$TBL_NAME==lookup_c),]

T1<-data_luc1$PERIOD
T2<-data_luc2$PERIOD

#==Check LUMENS QUES-C log file===
# if (file.exists(paste(user_temp_folder,"/LUMENS/LUMENS_quesc.log", sep=""))) {
#   log.quesc<-read.table(paste(user_temp_folder,"/LUMENS/LUMENS_quesc.log", sep=""), sep=",", header=T, row.names=1)
#   print("LUMENS QUES-C log file is available")
# } else {
#   log.quesc<-data.frame(IDX=NA, 
#                         MODULE=NA, 
#                         DATE=NA,
#                         TIME=NA,
#                         LU1=NA,
#                         LU2=NA,
#                         PU=NA,
#                         T1=NA,
#                         T2=NA,
#                         LOOKUP_LC=NA,
#                         LOOKUP_C=NA,
#                         LOOKUP_ZONE=NA,
#                         NODATA=NA,
#                         OUTPUT_FOLDER=NA, row.names=NULL)
# }

#====CREATE RUNNING RECORD====
# check_record <- paste(T1, T2, pu_selected, sep="")
# if(exists("run_record")){
#   rec_selected <- run_record[which(run_record$rec==check_record & run_record$modul=="QUESC"),]
#   n_rec <- nrow(rec_selected)
#   if(n_rec==0){
#     new_rec <- data.frame(check_record, T1, T2, pu_selected, "QUESC")
#     colnames(new_rec)[1] <- "rec"
#     colnames(new_rec)[2] <- "T1"
#     colnames(new_rec)[3] <- "T2"
#     colnames(new_rec)[4] <- "pu_selected"    
#     colnames(new_rec)[5] <- "modul"    
#     run_record <- rbind(run_record, new_rec)
#   } else {
#     #print all existing element (rtf, dbf, carbon1, carbon2, sequestration, emission)
#     QUESC.index<-QUESC.index+1
#     eval(parse(text=(paste("pu_name<-names(",pu[1],")", sep=''))))
#     eval(parse(text=(paste("landuse1<-", data[1,1], sep=""))))
#     eval(parse(text=(paste("landuse2<-", data[2,1], sep=""))))
#     lookup_c<- read.table(Look_up_table, header=TRUE, sep=",")
#     
#     dirQUESC<-paste(dirname(proj.file), "/QUES/QUES-C/QUESC_analysis_",pu_name,"_",data[1,2],"_",data[2,2], "_", QUESC.index, sep="")
#     dir.create(dirQUESC, mode="0777")
#     setwd(dirQUESC) 
#     
#     eval(parse(text=(paste("done(rtffileQUESC_", rec_selected$rec, ")", sep=""))))
#     
#     NAvalue(landuse1)<-raster.nodata
#     NAvalue(landuse2)<-raster.nodata
#     rcl.m.c1<-as.matrix(lookup_c[,1])
#     rcl.m.c2<-as.matrix(lookup_c[,3])
#     rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
#     carbon1<-reclassify(landuse1, rcl.m)
#     carbon2<-reclassify(landuse2, rcl.m)
#     chk_em<-carbon1>carbon2
#     chk_sq<-carbon1<carbon2
#     emission<-((carbon1-carbon2)*3.67)*chk_em
#     sequestration<-((carbon2-carbon1)*3.67)*chk_sq
#     
#     writeRaster(carbon1, filename="carbon1.tif", format="GTiff", overwrite=TRUE)
#     writeRaster(carbon2, filename="carbon2.tif", format="GTiff", overwrite=TRUE)
#     writeRaster(emission, filename="emission.tif", format="GTiff", overwrite=TRUE)
#     writeRaster(sequestration, filename="sequestration.tif", format="GTiff", overwrite=TRUE)
#     qmlcarbon1<-paste(dirQUESC, "/carbon1.qml", sep="")
#     qmlcarbon2<-paste(dirQUESC, "/carbon2.qml", sep="")
#     qmlemisi<-paste(dirQUESC, "/emission.qml", sep="")
#     qmlseq<-paste(dirQUESC, "/sequestration.qml", sep="")
#     
#     sink(qmlcarbon1)
#     cat("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>")
#     cat('<qgis version="2.0.0-Taoge" minimumScale="0" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">')
#     cat('  <pipe>')
#     cat('    <rasterrenderer opacity="1" alphaBand="-1" classificationMax="296.703" classificationMinMaxOrigin="CumulativeCutFullExtentEstimated" band="1" classificationMin="0.297" type="singlebandpseudocolor">')
#     cat('      <rasterTransparency/>')
#     cat('      <rastershader>')
#     cat('        <colorrampshader colorRampType="INTERPOLATED" clip="0">')
#     cat('          <item alpha="255" value="5" label="0-5" color="#f7fcf5"/>')
#     cat('          <item alpha="255" value="5" label="0-5" color="#f7fcf5"/>')
#     cat('          <item alpha="255" value="25" label="10-25" color="#bfe5b8"/>')
#     cat('          <item alpha="255" value="50" label="25-50" color="#93d290"/>')
#     cat('          <item alpha="255" value="100" label="50-100" color="#5fba6c"/>')
#     cat('          <item alpha="255" value="200" label="100-200" color="#329b51"/>')
#     cat('          <item alpha="255" value="300" label="200-300" color="#0c7734"/>')
#     cat('          <item alpha="255" value="400" label="300-400" color="#00441b"/>')
#     cat('        </colorrampshader>')
#     cat('      </rastershader>')
#     cat('    </rasterrenderer>')
#     cat('    <brightnesscontrast brightness="0" contrast="0"/>')
#     cat('    <huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>')
#     cat('    <rasterresampler maxOversampling="2"/>')
#     cat('  </pipe>')
#     cat('  <blendMode>0</blendMode>')
#     cat('</qgis>')
#     sink()
#     
#     sink(qmlcarbon2)
#     cat("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>")
#     cat('<qgis version="2.0.0-Taoge" minimumScale="0" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">')
#     cat('  <pipe>')
#     cat('    <rasterrenderer opacity="1" alphaBand="-1" classificationMax="296.703" classificationMinMaxOrigin="CumulativeCutFullExtentEstimated" band="1" classificationMin="0.297" type="singlebandpseudocolor">')
#     cat('      <rasterTransparency/>')
#     cat('      <rastershader>')
#     cat('        <colorrampshader colorRampType="INTERPOLATED" clip="0">')
#     cat('          <item alpha="255" value="5" label="0-5" color="#f7fcf5"/>')
#     cat('          <item alpha="255" value="5" label="0-5" color="#f7fcf5"/>')
#     cat('          <item alpha="255" value="25" label="10-25" color="#bfe5b8"/>')
#     cat('          <item alpha="255" value="50" label="25-50" color="#93d290"/>')
#     cat('          <item alpha="255" value="100" label="50-100" color="#5fba6c"/>')
#     cat('          <item alpha="255" value="200" label="100-200" color="#329b51"/>')
#     cat('          <item alpha="255" value="300" label="200-300" color="#0c7734"/>')
#     cat('          <item alpha="255" value="400" label="300-400" color="#00441b"/>')
#     cat('        </colorrampshader>')
#     cat('      </rastershader>')
#     cat('    </rasterrenderer>')
#     cat('    <brightnesscontrast brightness="0" contrast="0"/>')
#     cat('    <huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>')
#     cat('    <rasterresampler maxOversampling="2"/>')
#     cat('  </pipe>')
#     cat('  <blendMode>0</blendMode>')
#     cat('</qgis>')
#     sink()
#     
#     sink(qmlemisi)
#     cat("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>")
#     cat('<qgis version="2.0.0-Taoge" minimumScale="0" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">')
#     cat('<pipe>')
#     cat('<rasterrenderer opacity="1" alphaBand="-1" classificationMax="262.787" classificationMinMaxOrigin="CumulativeCutFullExtentEstimated" band="1" classificationMin="0" type="singlebandpseudocolor">')
#     cat('<rasterTransparency/>')
#     cat('<rastershader>')
#     cat('<colorrampshader colorRampType="INTERPOLATED" clip="0">')
#     cat('<item alpha="255" value="0" label="0.000000" color="#fff5f0"/>')
#     cat('<item alpha="255" value="34.1623" label="34.162310" color="#fee0d3"/>')
#     cat('<item alpha="255" value="68.3246" label="68.324620" color="#fcbda4"/>')
#     cat('<item alpha="255" value="102.487" label="102.486930" color="#fc9677"/>')
#     cat('<item alpha="255" value="136.649" label="136.649240" color="#fb7050"/>')
#     cat('<item alpha="255" value="170.812" label="170.811550" color="#f14431"/>')
#     cat('<item alpha="255" value="204.974" label="204.973860" color="#d32020"/>')
#     cat('<item alpha="255" value="236.508" label="236.508300" color="#ac1016"/>')
#     cat('<item alpha="255" value="262.787" label="262.787000" color="#67000d"/>')
#     cat('</colorrampshader>')
#     cat('</rastershader>')
#     cat('</rasterrenderer>')
#     cat('<brightnesscontrast brightness="0" contrast="0"/>')
#     cat('<huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>')
#     cat('<rasterresampler maxOversampling="2"/>')
#     cat('</pipe>')
#     cat('<blendMode>0</blendMode>')
#     cat('</qgis>')
#     sink()
#     
#     sink(qmlseq)
#     cat("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>")
#     cat('<qgis version="2.0.0-Taoge" minimumScale="0" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">')
#     cat('<pipe>')
#     cat('<rasterrenderer opacity="1" alphaBand="-1" classificationMax="262.787" classificationMinMaxOrigin="CumulativeCutFullExtentEstimated" band="1" classificationMin="0" type="singlebandpseudocolor">')
#     cat('<rasterTransparency/>')
#     cat('<rastershader>')
#     cat('<colorrampshader colorRampType="INTERPOLATED" clip="0">')
#     cat('<item alpha="255" value="0" label="0.000000" color="#fff5f0"/>')
#     cat('<item alpha="255" value="34.1623" label="34.162310" color="#fee0d3"/>')
#     cat('<item alpha="255" value="68.3246" label="68.324620" color="#fcbda4"/>')
#     cat('<item alpha="255" value="102.487" label="102.486930" color="#fc9677"/>')
#     cat('<item alpha="255" value="136.649" label="136.649240" color="#fb7050"/>')
#     cat('<item alpha="255" value="170.812" label="170.811550" color="#f14431"/>')
#     cat('<item alpha="255" value="204.974" label="204.973860" color="#d32020"/>')
#     cat('<item alpha="255" value="236.508" label="236.508300" color="#ac1016"/>')
#     cat('<item alpha="255" value="262.787" label="262.787000" color="#67000d"/>')
#     cat('</colorrampshader>')
#     cat('</rastershader>')
#     cat('</rasterrenderer>')
#     cat('<brightnesscontrast brightness="0" contrast="0"/>')
#     cat('<huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>')
#     cat('<rasterresampler maxOversampling="2"/>')
#     cat('</pipe>')
#     cat('<blendMode>0</blendMode>')
#     cat('</qgis>')
#     sink()
#     
#     eval(parse(text=(paste("data_merge<-QUESC_database_", pu_name,"_", T1, "_", T2, sep=''))))
#     write.dbf(data_merge, "QUES-C_database.dbf")
#     
#     add.log<-data.frame(IDX=(QUESC.index), 
#                         MODULE="QUES-C", 
#                         DATE=format(Sys.time(), "%d-%m%-%Y"),
#                         TIME=format(Sys.time(), "%X"),
#                         LU1=data[1,1],
#                         LU2=data[2,1],
#                         PU=pu[1],
#                         T1=T1,
#                         T2=T2,
#                         LOOKUP_LC="From DB",
#                         LOOKUP_C=Look_up_table,
#                         LOOKUP_ZONE="From DB",
#                         NODATA=raster.nodata,
#                         OUTPUT_FOLDER=dirQUESC, row.names=NULL)
#     log.quesc<-na.omit(rbind(log.quesc,add.log))
#     write.csv(log.quesc, paste(user_temp_folder,"/LUMENS/LUMENS_quesc.log", sep=""))
#     
#     resave(QUESC.index, file=proj.file)
#     command<-paste("start ", "winword ", dirQUESC, "/LUMENS_QUES-C_report.lpr", sep="" )
#     shell(command)
#     
#     quit()  
#   }
# } else {
#   run_record <- data.frame(check_record, T1, T2, pu_selected, "QUESC")
#   colnames(run_record)[1] <- "rec"
#   colnames(run_record)[2] <- "T1"
#   colnames(run_record)[3] <- "T2"
#   colnames(run_record)[4] <- "pu_selected"
#   colnames(run_record)[5] <- "modul"
# }

#===Load Datasets====
pu_name<-data_pu$RST_NAME # <== planning unit
if (data_pu$RST_DATA=="ref") {
  get_from_rdb(symbol=paste(data_pu$RST_DATA), filebase=paste(data_dir, "planning_unit", sep=""))
  ref[ref==0]<-NA
  zone<-ref
  get_from_rdb(symbol=paste(data_pu$LUT_NAME), filebase=paste(data_dir, "planning_unit", sep=""))
  lookup_z<-p.admin.df
} else {
  get_from_rdb(symbol=paste(data_pu$RST_DATA), filebase=paste(data_dir, "planning_unit", sep=""))
  eval(parse(text=(paste("zone<-", data_pu$RST_DATA, sep=""))))  
  get_from_rdb(symbol=paste(data_pu$LUT_NAME), filebase=paste(data_dir, "planning_unit", sep=""))
  eval(parse(text=(paste("lookup_z<-", data_pu$LUT_NAME, sep=""))))  
}
#landuse first period
get_from_rdb(symbol=paste(data_luc1$RST_DATA), filebase=paste(data_dir, "land_use_cover", sep=""))
eval(parse(text=(paste("landuse1<-", data_luc1$RST_DATA, sep=""))))
landuse1[landuse1==0]<-NA
#landuse second period
get_from_rdb(symbol=paste(data_luc2$RST_DATA), filebase=paste(data_dir, "land_use_cover", sep=""))
eval(parse(text=(paste("landuse2<-", data_luc2$RST_DATA, sep=""))))
landuse2[landuse2==0]<-NA
#landcover lookup table
get_from_rdb(symbol=paste(data_lut$TBL_DATA), filebase=paste(data_dir, "lookup_table", sep=""))
#set lookup table
eval(parse(text=(paste("lookup_c<-", data_lut$TBL_DATA, sep=""))))
lookup_c<-lookup_c[which(lookup_c[1] != raster.nodata),]
lookup_lc<-lookup_c
lookup_ref<-p.admin.df
colnames(lookup_lc)<-c("ID","LC","CARBON")
colnames(lookup_z)<-c("ID", "COUNT_ZONE", "ZONE")
colnames(lookup_ref)<-c("Z_NAME", "ZONE")

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

#====Check Peat Data====
# check_peat<-as.data.frame(as.character(ls(pattern="peat.index")))
# if(nrow(check_peat)!=0){
#   peat_map<-Peat_1*100
#   zone_peat_map<-zone+peat_map 
#   
#   legend_zone_peat<-as.data.frame(freq(zone_peat_map))
#   colnames(legend_zone_peat)[1]="ID"
#   legend_zone_peat<-merge(lut.pu, legend_zone_peat, by="ID", all=T)
#   colnames(legend_zone_peat)[2]="Z_NAME"
#   legend_zone_peat$count<-NULL
#   legend_zone_peat<-legend_zone_peat[which(legend_zone_peat$ID != "NA"),]
#   n_legend_zone_peat<-nrow(legend_zone_peat)
#   
#   #fill Z_NAME for peat
#   legend_zone_peat$Z_NAME<-as.character(legend_zone_peat$Z_NAME)
#   for(i in 1:(n_legend_zone_peat)){
#     if(legend_zone_peat[i,1] > 100){
#       id<-legend_zone_peat[i,1]-100
#       temp<-lut.pu[which(lut.pu$ID == id),]
#       legend_zone_peat[i,2]<-paste(as.character(temp[1,2]), ".gambut", sep="")
#     }
#   }
#   
#   lut.pu<-legend_zone_peat
#   zone<-zone_peat_map
#   lut.pu_peat<-legend_zone_peat
# } 

#====Set Project Properties====
title=location
tab_title<-as.data.frame(title)
period1=T1
period2=T2
period=period2-period1
proj_prop<-as.data.frame(title)
proj_prop$period1<-period1
proj_prop$period2<-period2
proj_prop$period <- do.call(paste, c(proj_prop[c("period1", "period2")], sep = " - "))

#====Carbon Accounting Process====
NAvalue(landuse1)<-raster.nodata
NAvalue(landuse2)<-raster.nodata
rcl.m.c1<-as.matrix(lookup_c[,1])
rcl.m.c2<-as.matrix(lookup_c[,3])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
carbon1<-reclassify(landuse1, rcl.m)
carbon2<-reclassify(landuse2, rcl.m)
chk_em<-carbon1>carbon2
chk_sq<-carbon1<carbon2
emission<-((carbon1-carbon2)*3.67)*chk_em
sequestration<-((carbon2-carbon1)*3.67)*chk_sq

#============================================in progress================================
#===CHECK EXISTING RASTER BRICK OR CROSSTAB====
setwd(LUMENS_temp_user)
command1<-paste(command1,pu, sep="")
eval(parse(text=(paste("pu_name<-names(",pu[1],")", sep=''))))
check_lucdb<-FALSE
eval(parse(text=(paste("check_crosstab<-file.exists('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
eval(parse(text=(paste("check_rbrick<-file.exists('r.brick_", pu_name ,"_", T1, "_", T2, ".grd')", sep=""))))  
if(check_crosstab){
  eval(parse(text=(paste("data_merge<-read.dbf('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
} else if(check_rbrick){
  eval(parse(text=(paste("r.brick<-brick('r.brick_", pu_name ,"_", T1, "_", T2, ".grd')", sep=""))))
  lu.db<-crosstab(r.brick,long=TRUE,useNA=FALSE,progress='-')
  check_lucdb<-TRUE
} else {
  eval(parse(text=(paste("r.brick<-brick(", command1, ", filename='r.brick_",pu_name,"_",T1, "_", T2, "')", sep=""))))
  lu.db<-crosstab(r.brick,long=TRUE,useNA=FALSE,progress='-')
  check_lucdb<-TRUE
}

refStack<-brick(landuse1,landuse2, ref)
refCross<-as.data.frame(crosstab(refStack,long=TRUE,useNA=FALSE,progress='-'))
colnames(refCross)[1] ="ID_LC1"
colnames(refCross)[2] = "ID_LC2"
colnames(refCross)[3] = "ZONE"
colnames(refCross)[4] = "COUNT"
refCross$COUNT<-refCross$COUNT*Spat_res
colnames(lookup_c)[1]="ID_LC1"
colnames(lookup_c)[2]="LC_t1"
colnames(lookup_c)[3]="CARBON_t1"
refDB <- merge(refCross,lookup_c,by="ID_LC1")
colnames(lookup_c)[1]="ID_LC2"
colnames(lookup_c)[2]="LC_t2"
colnames(lookup_c)[3]="CARBON_t2"
refDB <- as.data.frame(merge(refDB,lookup_c,by="ID_LC2"))
refDB <- as.data.frame(merge(refDB,lookup_ref,by="ZONE"))
refMelt<-melt(data = refDB, id.vars=c('ZONE'), measure.vars=c('COUNT'))
refArea<-dcast(data = refMelt, formula = ZONE ~ ., fun.aggregate = sum)

if(check_lucdb) {
  colnames(lu.db)[1] ="ID_LC1"
  colnames(lu.db)[2] = "ID_LC2"
  colnames(lu.db)[3] = "ZONE"
  colnames(lu.db)[4] = "COUNT"
  colnames(lookup_c)[1]="ID_LC1"
  colnames(lookup_c)[2]="LC_t1"
  colnames(lookup_c)[3]="CARBON_t1"
  data_merge <- merge(lu.db,lookup_c,by="ID_LC1")
  
  colnames(lookup_c)[1]="ID_LC2"
  colnames(lookup_c)[2]="LC_t2"
  colnames(lookup_c)[3]="CARBON_t2"
  data_merge <- as.data.frame(merge(data_merge,lookup_c,by="ID_LC2"))
  
  colnames(lookup_z)[1]="ZONE"
  colnames(lookup_z)[2]="Z_NAME"
  data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
  
  original_data<-subset(data_merge, select=-c(CARBON_t1, CARBON_t2))
  eval(parse(text=(paste("write.dbf(original_data, 'lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
  rm(lu.db, original_data)
} else {
  carbon_table <- subset(lookup_lc, select=-LC)
  colnames(carbon_table)[1]="ID_LC1"
  colnames(carbon_table)[2]="CARBON_t1"
  data_merge <- merge(data_merge,carbon_table,by="ID_LC1") 
  colnames(carbon_table)[1]="ID_LC2"
  colnames(carbon_table)[2]="CARBON_t2"
  data_merge <- merge(data_merge,carbon_table,by="ID_LC2") 
}

#====Set Working Directory====
QUESC.index<-QUESC.index+1
dirQUESC<-paste(dirname(proj.file), "/QUES/QUES-C/QUESC_analysis_",pu_name,"_",data[1,2],"_",data[2,2], "_", QUESC.index, sep="")
dir.create(dirQUESC, mode="0777")
setwd(dirQUESC) 

#===Modify Carbon Stock Density for Each Time Series====
data_merge$CARBON_t1<-data_merge$CARBON_t1
data_merge$CARBON_t2<-data_merge$CARBON_t2
data_merge$COUNT<-data_merge$COUNT*Spat_res

data_merge$ck_em<-data_merge$CARBON_t1>data_merge$CARBON_t2
data_merge$ck_sq<-data_merge$CARBON_t1<data_merge$CARBON_t2
data_merge$em<-(data_merge$CARBON_t1-data_merge$CARBON_t2)*data_merge$ck_em*data_merge$COUNT*3.67
data_merge$sq<-(data_merge$CARBON_t2-data_merge$CARBON_t1)*data_merge$ck_sq*data_merge$COUNT*3.67
data_merge$LU_CHG <- do.call(paste, c(data_merge[c("LC_t1", "LC_t2")], sep = " to "))
data_merge$null<-0
data_merge$nullCek<-data_merge$em+data_merge$sq

#===Generate area_zone Lookup and Calculate Min Area
area_zone<-melt(data = data_merge, id.vars=c('ZONE'), measure.vars=c('COUNT'))
area_zone<-dcast(data = area_zone, formula = ZONE ~ ., fun.aggregate = sum)
colnames(area_zone)[1]<-"ID"
colnames(area_zone)[2]<-"COUNT"
area_zone$ID<-as.numeric(as.character(area_zone$ID))
area_zone<-area_zone[with(area_zone, order(ID)),]
colnames(lookup_z)[1]<-"ID"
area_zone<-merge(area_zone, lookup_z, by="ID")
area<-min(sum(area_zone$COUNT), sum(data_merge$COUNT))

#====Generate administrative unit====
colnames(refArea)[1]<-"ID"
colnames(refArea)[2]<-"COUNT"
colnames(p.admin.df)[1]<-"KABKOT"
colnames(p.admin.df)[2]<-"ID"
area_admin<-merge(refArea, p.admin.df, by="ID")

#====Calculate Emission for each Planning Unit====
zone_emission <- as.data.frame(zonal((Spat_res*emission),zone,'sum')) #adjust emission by actual raster area
zone_sequestration <- as.data.frame(zonal((Spat_res*sequestration),zone,'sum'))#adjust sequestration by actual raster area
colnames(zone_emission)[1] = "ID"
colnames(zone_emission)[2] = "Em_tot"
colnames(zone_sequestration)[1] = "ID"
colnames(zone_sequestration)[2]="Sq_tot"
zone_emission<-merge(area_zone,zone_emission,by="ID")
zone_carbon<-merge(zone_emission,zone_sequestration,by="ID")
zone_carbon$Net_em<-zone_carbon$Em_tot-zone_carbon$Sq_tot
zone_carbon$Net_em_rate<-round((zone_carbon$Net_em/zone_carbon$COUNT/period), digits=3)
zone_carbon$Sq_tot<-round(zone_carbon$Sq_tot, digits=3)
#zone_carbon[,4:7]<-round(zone_carbon[,4:7], digits=3)

#====Calculate Emission for each Administrative Unit====
admin_emission <- as.data.frame(zonal((Spat_res*emission),ref,'sum')) #adjust emission by actual raster area
admin_sequestration <- as.data.frame(zonal((Spat_res*sequestration),ref,'sum'))#adjust sequestration by actual raster area
colnames(admin_emission)[1] = "ID"
colnames(admin_emission)[2] = "Em_tot"
colnames(admin_sequestration)[1] = "ID"
colnames(admin_sequestration)[2]="Sq_tot"
admin_emission<-merge(area_admin,admin_emission,by="ID")
admin_carbon<-merge(admin_emission,admin_sequestration,by="ID")
admin_carbon$Net_em<-admin_carbon$Em_tot-admin_carbon$Sq_tot
admin_carbon$Net_em_rate<-round((admin_carbon$Net_em/admin_carbon$COUNT/period), digits=3)
admin_carbon$Sq_tot<-round(admin_carbon$Sq_tot, digits=3)

#====Create Final Summary of Emission Calculation at Landscape Level
fs_id<-c(1,2,3,4,5,6,7)
fs_cat<-c("Period", "Total area", "Total Emisi (Ton CO2-eq)", "Total Sequestrasi (Ton CO2-eq)", "Emisi Bersih (Ton CO2-eq)", "Laju Emisi (Ton CO2-eq/tahun)","Laju emisi per-unit area (Ton CO2-eq/ha.tahun)")
fs_em<-sum(zone_carbon$Em_tot)
fs_sq<-sum(zone_carbon$Sq_tot)
fs_Nem<-fs_em-fs_sq
fs_Rem<-fs_Nem/period
fs_ARem<-fs_Rem/area
fs_summary<-c(proj_prop$period, area,round(fs_em, digits=3),round(fs_sq, digits=3),round(fs_Nem, digits=3),round(fs_Rem, digits=3),round(fs_ARem, digits=3))
fs_table<-data.frame(fs_id,fs_cat,fs_summary)
fs_table$fs_summary<-as.character(fs_table$fs_summary)
colnames(fs_table)<-c("ID", "Kategori", "Ringkasan")

#====CREATE QuES-C Database====
#====Zonal Statistics Database====
lg<-length(unique(data_merge$ZONE))
zone_lookup<-area_zone
data_zone<-area_zone
data_zone$Z_CODE<-toupper(abbreviate(data_zone$Z_NAME))
data_zone$Rate_seq<-data_zone$Rate_em<-data_zone$Avg_C_t2<-data_zone$Avg_C_t1<-0
for(a in 1:lg){
  i<-unique(data_merge$ZONE)[a]
  data_z<-data_merge[which(data_merge$ZONE == i),]
  data_zone<-within(data_zone, {Avg_C_t1<-ifelse(data_zone$ID == i, sum(data_z$CARBON_t1*data_z$COUNT)/sum(data_z$COUNT),Avg_C_t1)}) 
  data_zone<-within(data_zone, {Avg_C_t2<-ifelse(data_zone$ID == i, sum(data_z$CARBON_t2*data_z$COUNT)/sum(data_z$COUNT),Avg_C_t2)}) 
  data_zone<-within(data_zone, {Rate_em<-ifelse(data_zone$ID == i, sum(data_z$em)/(sum(data_z$COUNT)*period),Rate_em)}) 
  data_zone<-within(data_zone, {Rate_seq<-ifelse(data_zone$ID == i, sum(data_z$sq)/(sum(data_z$COUNT)*period),Rate_seq)}) 
}
data_zone[,5:8]<-round(data_zone[,5:8],digits=3)

#====Calculate Largest Source of Emission====
data_merge_sel <- data_merge[ which(data_merge$nullCek > data_merge$null),]
order_sq <- as.data.frame(data_merge[order(-data_merge$sq),])
order_em <- as.data.frame(data_merge[order(-data_merge$em),])

#====Total Emission====
tb_em_total<-as.data.frame(cbind(order_em$LU_CHG, as.data.frame(round(order_em$em, digits=3))))
colnames(tb_em_total)<-c("LU_CHG", "em")
tb_em_total<-aggregate(em~LU_CHG,data=tb_em_total,FUN=sum)
tb_em_total$LU_CODE<-as.factor(toupper(abbreviate(tb_em_total$LU_CHG, minlength=5, strict=FALSE, method="both")))
tb_em_total<-tb_em_total[order(-tb_em_total$em),]
tb_em_total<-tb_em_total[c(3,1,2)]
tb_em_total$Percentage<-as.numeric(format(round((tb_em_total$em / sum(tb_em_total$em) * 100),2), nsmall=2))
tb_em_total_10<-head(tb_em_total,n=10)

#====Zonal Emission====
tb_em_zonal<-as.data.frame(NULL)
for (i in 1:length(zone_lookup$ID)){
  tryCatch({
    a<-(zone_lookup$ID)[i]
    tb_em<-as.data.frame(cbind(order_em$ZONE, order_em$LU_CHG, as.data.frame(round(order_em$em, digits=3))))
    colnames(tb_em)<-c("ZONE","LU_CHG", "em")
    tb_em_z<-as.data.frame(tb_em[which(tb_em$ZONE == a),])
    tb_em_z<-aggregate(em~ZONE+LU_CHG,data=tb_em_z,FUN=sum)
    tb_em_z$LU_CODE<-as.factor(toupper(abbreviate(tb_em_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
    tb_em_z<-tb_em_z[order(-tb_em_z$em),]
    tb_em_z<-tb_em_z[c(1,4,2,3)]
    tb_em_z$Percentage<-as.numeric(format(round((tb_em_z$em / sum(tb_em_z$em) * 100),2), nsmall=2))
    tb_em_z_10<-head(tb_em_z,n=10)
    tb_em_zonal<-rbind(tb_em_zonal,tb_em_z_10)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
rm(tb_em, tb_em_total, tb_em_z, tb_em_z_10)

#====Total Sequestration====
tb_seq_total<-as.data.frame(cbind(order_sq$LU_CHG, as.data.frame(round(order_sq$sq, digits=3))))
colnames(tb_seq_total)<-c("LU_CHG", "seq")
tb_seq_total<-aggregate(seq~LU_CHG,data=tb_seq_total,FUN=sum)
tb_seq_total$LU_CODE<-as.factor(toupper(abbreviate(tb_seq_total$LU_CHG, minlength=5, strict=FALSE, method="both")))
tb_seq_total<-tb_seq_total[order(-tb_seq_total$seq),]
tb_seq_total<-tb_seq_total[c(3,1,2)]
tb_seq_total$Percentage<-as.numeric(format(round((tb_seq_total$seq / sum(tb_seq_total$seq) * 100),2), nsmall=2))
tb_seq_total_10<-head(tb_seq_total,n=10)

#====Zonal Sequestration====
tb_seq_zonal<-as.data.frame(NULL)
for (i in 1:length(zone_lookup$ID)){
  tryCatch({
    a<-(zone_lookup$ID)[i]
    tb_seq<-as.data.frame(cbind(order_sq$ZONE, order_sq$LU_CHG, as.data.frame(round(order_sq$sq, digits=3))))
    colnames(tb_seq)<-c("ZONE","LU_CHG", "seq")
    tb_seq_z<-as.data.frame(tb_seq[which(tb_seq$ZONE == i),])
    tb_seq_z<-aggregate(seq~ZONE+LU_CHG,data=tb_seq_z,FUN=sum)
    tb_seq_z$LU_CODE<-as.factor(toupper(abbreviate(tb_seq_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
    tb_seq_z<-tb_seq_z[order(-tb_seq_z$seq),]
    tb_seq_z<-tb_seq_z[c(1,4,2,3)]
    tb_seq_z$Percentage<-as.numeric(format(round((tb_seq_z$seq / sum(tb_seq_z$seq) * 100),2), nsmall=2))
    tb_seq_z_10<-head(tb_seq_z,n=10)
    tb_seq_zonal<-rbind(tb_seq_zonal,tb_seq_z_10)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
rm(tb_seq, tb_seq_total, tb_seq_z, tb_seq_z_10)

#====Zonal Additional Statistics====
if (((length(unique(data_merge$ID_LC1)))>(length(unique(data_merge$ID_LC2))))){
  dimention<-length(unique(data_merge$ID_LC1))
  name.matrix<-cbind(as.data.frame(data_merge$ID_LC1), as.data.frame(data_merge$LC_t1))
  name.matrix<-unique(name.matrix)
  colnames(name.matrix)<-c("ID","LC")
  name.matrix<-name.matrix[order(name.matrix$ID),]
  name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$LC, minlength=4, method="both"))
} else{
  dimention<-length(unique(data_merge$ID_LC2))
  name.matrix<-cbind(as.data.frame(data_merge$ID_LC2), as.data.frame(data_merge$LC_t2))
  name.matrix<-unique(name.matrix)
  colnames(name.matrix)<-c("ID","LC")
  name.matrix<-name.matrix[order(name.matrix$ID),]
  name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$LC, minlength=4, method="both"))
}

#====Zonal Emission matrix====
e.m.z<-matrix(0, nrow=dimention, ncol=dimention)
em.matrix.zonal<-as.data.frame(NULL)
for (k in 1:length(zone_lookup$ID)){
  for (i in 1:nrow(e.m.z)){
    for (j in 1:ncol(e.m.z)){
      em.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j & data_merge_sel$ZONE==k),]
      e.m.z[i,j]<-as.numeric(round(sum(em.data$em), 2))
    }
  }
  e.m.z<-as.data.frame(e.m.z)
  e.m.z.c<-as.data.frame(cbind(name.matrix$LC_CODE,e.m.z))
  e.m.z.c<-cbind(rep(k,nrow(e.m.z)),e.m.z.c)
  em.matrix.zonal<-rbind(em.matrix.zonal,e.m.z.c)
}
colnames(em.matrix.zonal)<-c("ZONE","LC_CODE",as.vector(name.matrix$LC_CODE))
rm(em.data, e.m.z, e.m.z.c)


#====Total Emission matrix====
e.m<-matrix(0, nrow=dimention, ncol=dimention)
for (i in 1:nrow(e.m)){
  for (j in 1:ncol(e.m)){
    em.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j),]
    e.m[i,j]<-round(sum(em.data$em), digits=2)
  }
}
e.m<-as.data.frame(e.m)
em.matrix.total<-as.data.frame(cbind(name.matrix$LC_CODE,e.m))
colnames(em.matrix.total)<-c("LC_CODE",as.vector(name.matrix$LC_CODE))
rm(em.data, e.m)

#====Zonal Sequestration matrix====
s.m.z<-matrix(0, nrow=dimention, ncol=dimention)
seq.matrix.zonal<-as.data.frame(NULL)
for (k in 1:length(zone_lookup$ID)){
  for (i in 1:nrow(s.m.z)){
    for (j in 1:ncol(s.m.z)){
      seq.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j & data_merge_sel$ZONE==k),]
      s.m.z[i,j]<-round(sum(seq.data$sq), digits=2)
    }
  }
  s.m.z<-as.data.frame(s.m.z)
  s.m.z.c<-as.data.frame(cbind(name.matrix$LC_CODE,s.m.z))
  s.m.z.c<-cbind(rep(k,nrow(s.m.z)),s.m.z.c)
  seq.matrix.zonal<-rbind(seq.matrix.zonal,s.m.z.c)
}
colnames(seq.matrix.zonal)<-c("ZONE","LC_CODE",as.vector(name.matrix$LC_CODE))
rm(seq.data, s.m.z, s.m.z.c)

#====Total Sequestration matrix====
s.m<-matrix(0, nrow=dimention, ncol=dimention)
for (i in 1:nrow(s.m)){
  for (j in 1:ncol(s.m)){
    seq.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j),]
    s.m[i,j]<-round(sum(seq.data$sq), digits=2)
  }
}
s.m<-as.data.frame(s.m)
seq.matrix.total<-as.data.frame(cbind(name.matrix$LC_CODE,s.m))
colnames(seq.matrix.total)<-c("LC_CODE",as.vector(name.matrix$LC_CODE))
rm(seq.data, s.m, order_em, order_sq)

#produce chart and map
#par(mfrow=c(3,2))
#plot(landuse1,main='Land Use Map t1')
#plot(landuse2,main='Land Use Map t2')
#plot(carbon1,main='Carbon Density Map t1')
#plot(carbon2, main='Carbon Density Map t2')


#====Export Analysis Result====
carbontiff1<-carbon1
carbontiff2<-carbon2
writeRaster(carbon1, filename="carbon1.tif", format="GTiff", overwrite=TRUE)
writeRaster(carbon2, filename="carbon2.tif", format="GTiff", overwrite=TRUE)
writeRaster(emission, filename="emission.tif", format="GTiff", overwrite=TRUE)
writeRaster(sequestration, filename="sequestration.tif", format="GTiff", overwrite=TRUE)
qmlcarbon1<-paste(dirQUESC, "/carbon1.qml", sep="")
qmlcarbon2<-paste(dirQUESC, "/carbon2.qml", sep="")
qmlemisi<-paste(dirQUESC, "/emission.qml", sep="")
qmlseq<-paste(dirQUESC, "/sequestration.qml", sep="")

sink(qmlcarbon1)
cat("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>")
cat('<qgis version="2.0.0-Taoge" minimumScale="0" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">')
cat('  <pipe>')
cat('    <rasterrenderer opacity="1" alphaBand="-1" classificationMax="296.703" classificationMinMaxOrigin="CumulativeCutFullExtentEstimated" band="1" classificationMin="0.297" type="singlebandpseudocolor">')
cat('      <rasterTransparency/>')
cat('      <rastershader>')
cat('        <colorrampshader colorRampType="INTERPOLATED" clip="0">')
cat('          <item alpha="255" value="5" label="0-5" color="#f7fcf5"/>')
cat('          <item alpha="255" value="5" label="0-5" color="#f7fcf5"/>')
cat('          <item alpha="255" value="25" label="10-25" color="#bfe5b8"/>')
cat('          <item alpha="255" value="50" label="25-50" color="#93d290"/>')
cat('          <item alpha="255" value="100" label="50-100" color="#5fba6c"/>')
cat('          <item alpha="255" value="200" label="100-200" color="#329b51"/>')
cat('          <item alpha="255" value="300" label="200-300" color="#0c7734"/>')
cat('          <item alpha="255" value="400" label="300-400" color="#00441b"/>')
cat('        </colorrampshader>')
cat('      </rastershader>')
cat('    </rasterrenderer>')
cat('    <brightnesscontrast brightness="0" contrast="0"/>')
cat('    <huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>')
cat('    <rasterresampler maxOversampling="2"/>')
cat('  </pipe>')
cat('  <blendMode>0</blendMode>')
cat('</qgis>')
sink()

sink(qmlcarbon2)
cat("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>")
cat('<qgis version="2.0.0-Taoge" minimumScale="0" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">')
cat('  <pipe>')
cat('    <rasterrenderer opacity="1" alphaBand="-1" classificationMax="296.703" classificationMinMaxOrigin="CumulativeCutFullExtentEstimated" band="1" classificationMin="0.297" type="singlebandpseudocolor">')
cat('      <rasterTransparency/>')
cat('      <rastershader>')
cat('        <colorrampshader colorRampType="INTERPOLATED" clip="0">')
cat('          <item alpha="255" value="5" label="0-5" color="#f7fcf5"/>')
cat('          <item alpha="255" value="5" label="0-5" color="#f7fcf5"/>')
cat('          <item alpha="255" value="25" label="10-25" color="#bfe5b8"/>')
cat('          <item alpha="255" value="50" label="25-50" color="#93d290"/>')
cat('          <item alpha="255" value="100" label="50-100" color="#5fba6c"/>')
cat('          <item alpha="255" value="200" label="100-200" color="#329b51"/>')
cat('          <item alpha="255" value="300" label="200-300" color="#0c7734"/>')
cat('          <item alpha="255" value="400" label="300-400" color="#00441b"/>')
cat('        </colorrampshader>')
cat('      </rastershader>')
cat('    </rasterrenderer>')
cat('    <brightnesscontrast brightness="0" contrast="0"/>')
cat('    <huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>')
cat('    <rasterresampler maxOversampling="2"/>')
cat('  </pipe>')
cat('  <blendMode>0</blendMode>')
cat('</qgis>')
sink()

sink(qmlemisi)
cat("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>")
cat('<qgis version="2.0.0-Taoge" minimumScale="0" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">')
cat('<pipe>')
cat('<rasterrenderer opacity="1" alphaBand="-1" classificationMax="262.787" classificationMinMaxOrigin="CumulativeCutFullExtentEstimated" band="1" classificationMin="0" type="singlebandpseudocolor">')
cat('<rasterTransparency/>')
cat('<rastershader>')
cat('<colorrampshader colorRampType="INTERPOLATED" clip="0">')
cat('<item alpha="255" value="0" label="0.000000" color="#fff5f0"/>')
cat('<item alpha="255" value="34.1623" label="34.162310" color="#fee0d3"/>')
cat('<item alpha="255" value="68.3246" label="68.324620" color="#fcbda4"/>')
cat('<item alpha="255" value="102.487" label="102.486930" color="#fc9677"/>')
cat('<item alpha="255" value="136.649" label="136.649240" color="#fb7050"/>')
cat('<item alpha="255" value="170.812" label="170.811550" color="#f14431"/>')
cat('<item alpha="255" value="204.974" label="204.973860" color="#d32020"/>')
cat('<item alpha="255" value="236.508" label="236.508300" color="#ac1016"/>')
cat('<item alpha="255" value="262.787" label="262.787000" color="#67000d"/>')
cat('</colorrampshader>')
cat('</rastershader>')
cat('</rasterrenderer>')
cat('<brightnesscontrast brightness="0" contrast="0"/>')
cat('<huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>')
cat('<rasterresampler maxOversampling="2"/>')
cat('</pipe>')
cat('<blendMode>0</blendMode>')
cat('</qgis>')
sink()

sink(qmlseq)
cat("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>")
cat('<qgis version="2.0.0-Taoge" minimumScale="0" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">')
cat('<pipe>')
cat('<rasterrenderer opacity="1" alphaBand="-1" classificationMax="262.787" classificationMinMaxOrigin="CumulativeCutFullExtentEstimated" band="1" classificationMin="0" type="singlebandpseudocolor">')
cat('<rasterTransparency/>')
cat('<rastershader>')
cat('<colorrampshader colorRampType="INTERPOLATED" clip="0">')
cat('<item alpha="255" value="0" label="0.000000" color="#fff5f0"/>')
cat('<item alpha="255" value="34.1623" label="34.162310" color="#fee0d3"/>')
cat('<item alpha="255" value="68.3246" label="68.324620" color="#fcbda4"/>')
cat('<item alpha="255" value="102.487" label="102.486930" color="#fc9677"/>')
cat('<item alpha="255" value="136.649" label="136.649240" color="#fb7050"/>')
cat('<item alpha="255" value="170.812" label="170.811550" color="#f14431"/>')
cat('<item alpha="255" value="204.974" label="204.973860" color="#d32020"/>')
cat('<item alpha="255" value="236.508" label="236.508300" color="#ac1016"/>')
cat('<item alpha="255" value="262.787" label="262.787000" color="#67000d"/>')
cat('</colorrampshader>')
cat('</rastershader>')
cat('</rasterrenderer>')
cat('<brightnesscontrast brightness="0" contrast="0"/>')
cat('<huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>')
cat('<rasterresampler maxOversampling="2"/>')
cat('</pipe>')
cat('<blendMode>0</blendMode>')
cat('</qgis>')
sink()

write.dbf(zone_carbon, "emission_by_zone.dbf")
write.dbf(fs_table, "summary_QUES-C.dbf")
write.dbf(data_merge, "QUES-C_database.dbf")
write.dbf(data_zone, "Carbon_Summary.dbf")
write.dbf(em.matrix.total,"Total_Emission_Matrix.dbf ")
write.dbf(seq.matrix.total, "Total_Sequestration_Matrix.dbf")

for (i in 1:length(zone_lookup$ID)){
  em_matrix_z<-em.matrix.zonal[which(em.matrix.zonal$ZONE == i),]
  em_matrix_z$ZONE<-NULL
  seq_matrix_z<-seq.matrix.zonal[which(seq.matrix.zonal$ZONE == i),]
  seq_matrix_z$ZONE<-NULL
  write.dbf(em_matrix_z,paste("Emission_Matrix_Zone_",i,sep=""))
  write.dbf(seq_matrix_z,paste("Sequestration_Matrix_Zone_",i,sep=""))
}

#====Rearrange zone carbon====
zone_carbon_pub<-zone_carbon
colnames(zone_carbon_pub) <- c("ID", "Area (Ha)", "Land cover class", "Total emission (Ton CO2/Ha)", "Total sequestration(Ton CO2/Ha)", "Net emission (Ton CO2/Ha)", "Emission rate (Ton CO2/Ha.yr)")

#====Create Map for report====
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- brewer.pal(11, "Spectral")
myColors8 <- rev(brewer.pal(11, "RdYlGn"))
myColors  <-c(myColors8,myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors8)



#====Landuse 1 map====
myColors.lu <- myColors[1:length(unique(lookup_lc$ID))]
lookup_lc$Colors<-myColors.lu
lu1<-as.data.frame(unique(data_merge$ID_LC1))
colnames(lu1)<-"ID"
lu1<-merge(lu1,lookup_lc, by="ID")
lu1$ID<-as.numeric(as.character(lu1$ID))
lu1<-lu1[order(lu1$ID),]
ColScale.lu1<-scale_fill_manual(name="Tipe tutupan lahan t1", breaks=lu1$ID, labels=lu1$LC, values=lu1$Colors)
plot.LU1<-gplot(landuse1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu1 +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#====Landuse 2 map====
lu2<-as.data.frame(unique(data_merge$ID_LC2))
colnames(lu2)<-"ID"
lu2<-merge(lu2,lookup_lc, by="ID")
lu2$ID<-as.numeric(as.character(lu2$ID))
lu2<-lu2[order(lu2$ID),]
ColScale.lu2<-scale_fill_manual(name="Tipe tutupan lahan t2", breaks=lu2$ID, labels=lu2$LC, values=lu2$Colors)
plot.LU2<-gplot(landuse2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu2 +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

myColors  <-c(myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors6, myColors8)


#====zone map====
myColors.Z <- myColors[1:length(unique(lookup_z$ID))]
ColScale.Z<-scale_fill_manual(name="Kelas Unit Perencanaan", breaks=lookup_z$ID, labels=lookup_z$Z_NAME, values=myColors.Z)
plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.Z +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#====admin map====
myColors.Admin <- myColors[1:(length(unique(p.admin.df$ID))+1)]
ColScale.Admin<-scale_fill_manual(name="Wilayah Administratif", breaks=p.admin.df$ID, labels=p.admin.df$KABKOT, values=myColors.Admin)
plot.Admin<-gplot(ref, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.Admin +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

rm(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6,myColors8)
#====Average Zonal Carbon Rate t1====
rcl.m.c1<-as.matrix(data_zone[,1])
rcl.m.c2<-as.matrix(data_zone[,5])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
Z.Avg.C.t1<-reclassify(zone, rcl.m)
plot.Z.Avg.C.t1<-gplot(Z.Avg.C.t1, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste("Rerata Kerapatan Karbon", location, period1 )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Average Zonal Carbon Rate t2====
rcl.m.c1<-as.matrix(data_zone[,1])
rcl.m.c2<-as.matrix(data_zone[,6])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
Z.Avg.C.t2<-reclassify(zone, rcl.m)
plot.Z.Avg.C.t2<-gplot(Z.Avg.C.t2, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste("Rerata Kerapatan Karbon", location, period2 )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Average Zonal Emission Rate====
rcl.m.c1<-as.matrix(data_zone[,1])
rcl.m.c2<-as.matrix(data_zone[,7])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
Z.Avg.em<-reclassify(zone, rcl.m)
plot.Z.Avg.em<-gplot(Z.Avg.em, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(name="Tingkat Emisi",low = "#fff5f0", high="#67000d", guide="colourbar") +
  ggtitle(paste(" Rerata laju emisi", location, period1, "-", period2 )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Average Zonal Sequestration Rate====
rcl.m.c1<-as.matrix(data_zone[,1])
rcl.m.c2<-as.matrix(data_zone[,8])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
Z.Avg.sq<-reclassify(zone,rcl.m)
plot.Z.Avg.sq<-gplot(Z.Avg.sq, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(name="Tingkat Sequestrasi",low = "#fff5f0", high="#67000d", guide="colourbar") +
  ggtitle(paste("Rerata laju sequestrasi", location, period1, "-", period2 )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Carbon 1 map====
y<-ceiling( maxValue(carbon1)/100)
y<-y*100
plot.C1  <- gplot(carbon1, maxpixels=100000) + geom_raster(aes(fill=value)) + coord_equal() +
  scale_fill_gradient(name="Kerapatan karbon",low = "#FFCC66", high="#003300",limits=c(0,y), breaks=c(0,10,20,50,100,200,300), guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 7),
         legend.key.height = unit(1.5, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Carbon 2 map====
plot.C2  <- gplot(carbon2, maxpixels=100000) + geom_raster(aes(fill=value)) + coord_equal() +
  scale_fill_gradient(name="Kerapatan karbon",low = "#FFCC66", high="#003300",limits=c(0,y), breaks=c(0,10,20,50,100,200,300), guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 7),
         legend.key.height = unit(1.5, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Carbon Emission Map====
plot.E  <- gplot(emission, maxpixels=100000) + geom_raster(aes(fill=value)) + coord_equal() +
  scale_fill_gradient(name="Emisi (ton CO2-eq)",low = "#FFCC66", high="#FF0000", guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Carbon Sequestration Map====
plot.S  <- gplot(sequestration, maxpixels=100000) + geom_raster(aes(fill=value)) + coord_equal() +
  scale_fill_gradient(name="Sequestrasi (ton CO2-eq)",low = "#FFCC66", high="#000033", guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Emission Rate====
emissionRate<-ggplot(data=zone_carbon, aes(x=reorder(Z_NAME, -Net_em_rate), y=(zone_carbon$Net_em_rate))) + geom_bar(stat="identity", fill="Red") +
  geom_text(data=zone_carbon, aes(label=round(Net_em_rate, 1)),size=4) +
  ggtitle(paste("Rerata laju emisi bersih", location, period1,"-", period2 )) + guides(fill=FALSE) + ylab("CO2-eq/ha.yr") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle=20),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

#====Largest emission====
largestEmission<-ggplot(data=tb_em_total_10, aes(x=reorder(LU_CODE, -em), y=(em))) + geom_bar(stat="identity", fill="blue") +
  geom_text(data=tb_em_total_10, aes(x=LU_CODE, y=em, label=round(em, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Sumber emisi terbesar", location )) + guides(fill=FALSE) + ylab("CO2-eq") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

#====Largest Sequestration====
largestSeq<-ggplot(data=tb_seq_total_10, aes(x=reorder(LU_CODE, -seq), y=(seq))) + geom_bar(stat="identity", fill="green") +
  geom_text(data=tb_seq_total_10, aes(x=LU_CODE, y=seq, label=round(seq, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Sumber sequestrasi terbesar", location )) + guides(fill=FALSE) + ylab("CO2-eq") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

printArea <- function(x){
  format(x, digits=15, big.mark=",")
}

printRate <- function(x){
  format(x, digits=15, nsmall=2, decimal.mark=".", big.mark=",")
}

tabel_ket<-proj_descr
row.names(tabel_ket)<-NULL
tabel_ket$Type<-as.character(tabel_ket$Type)
colnames(tabel_ket)<-c("Tipe", "Keterangan")
tabel_ket[1,1]<-"Proyek"
tabel_ket[2,1]<-"Deskripsi"
tabel_ket[3,1]<-"Direktori"
tabel_ket[4,1]<-"Wilayah Analisis"
tabel_ket[5,1]<-"Provinsi"
tabel_ket[6,1]<-"Negara"

#====Create RTF Report File====
title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1HASIL ANALISIS \\par\\b0\\fs20\\ql\\cf1"
title2<-paste("\\pard\\qr\\b\\fs40\\cf1 Modul QUES-C - Analisis Dinamika Cadangan Karbon \\par\\b0\\fs20\\ql\\cf1", sep="")
sub_title<-"\\cf2\\b\\fs32 ANALISIS DINAMIKA CADANGAN KARBON\\cf1\\b0\\fs20"
#rad_grk<-"\\pard\\qr\\b\\fs40\\cf1 Dokumen RAD GRK - Bab 2.3. Permasalahan Emisi GRK \\par\\b0\\fs20\\ql\\cf1"
test<-as.character(Sys.Date())
date<-paste("Date : ", test, sep="")
time_start<-paste("Proses dimulai : ", time_start, sep="")
time_end<-paste("Proses selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
chapter1<-"\\b\\fs32 DATA YANG DIGUNAKAN \\b0\\fs20"
chapter2<-"\\b\\fs32 ANALISIS PADA TINGKAT BENTANG LAHAN \\b0\\fs20"
chapter3<-"\\b\\fs32 ANALISIS PADA TINGKAT UNIT PERENCANAAN \\b0\\fs20"
rtffile <- RTF("LUMENS_QUES-C_report.lpr", font.size=9)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, title1)
addParagraph(rtffile, title2)
#addNewLine(rtffile)
#addParagraph(rtffile, rad_grk)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addTable(rtffile,tabel_ket,font.size=8,col.widths=width)
addPageBreak(rtffile)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Analisis dinamika cadangan karbon dilakukan untuk perubahan cadangan karbon di suatu daerah pada satu kurun waktu. Metode yang digunakan adalah metode Stock Difference. Emisi dihitung sebagai jumlah penurunan cadangan karbon akibat perubahan tutupan lahan terjadi apabila cadangan karbon awal lebih tinggi dari cadangan karbon setelah terjadinya perubahan penggunaan lahan. Sebaliknya, sequestrasi dihitung sebagai jumlah penambahan cadangan karbon akibat perubahan tutupan lahan (cadangan karbon pada penggunaan lahan awal lebih rendah dari cadangan karbon setelah terjadinya perubahan penggunaan lahan).. Analisis ini dilakukan dengan menggunakan data peta tutupan lahan pada dua periode waktu yang berbeda dan tabel acuan kerapatan karbon untuk masing-masing tipe tutupan lahan. Selain itu, dengan memasukkan data unit perencanaan kedalam  analisis, dapat diketahui tingkat perubahan cadangan karbon pada masing-masing kelas unit perencanaan yang ada. Informasi yang dihasilkan melalui analisis ini dapat digunakan dalam proses perencanaan untuk berbagai hal, diantaranya menentukan prioritas aksi mitigasi perubahan iklim, mengetahui faktor pemicu terjadinya emisi, dan merencanakan skenario pembangunan di masa yang akan datang.")
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Data yang digunakan dalam analisis ini adalah data peta penggunaan lahan dan data peta unit perencanaan daerah. Data pendukung yang digunakan adalah peta acuan tipe penggunaan lahan, data acuan kerapatan karbon masing-masing tipe tutupan lahan dan data acuan kelas unit perencanaan.")
addNewLine(rtffile)

text <- paste("\\b \\fs20 Peta penutupan lahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.4, height=4, res=150, plot.LU1 )
rm(plot.LU1)
text <- paste("\\b \\fs20 Peta penutupan lahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_2_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.LU2 )
rm(plot.LU2)
text <- paste("\\b \\fs20 Peta unit perencanaan \\b0 \\fs20 ", area_name_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z )
rm(plot.Z)
text <- paste("\\b \\fs20 Peta wilayah administratif \\b0 \\fs20 ", area_name_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Admin )
rm(plot.Admin)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, chapter2)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Pada bagian ini disajikan hasil analisis dinamika cadangan karbon untuk keseluruhan bentang lahan yang dianalisis. Beberapa bentuk analisis yang dilakukan antara lain: tingkat emisi, tingkat sequestrasi, laju emisi dan tipe perubahan penggunaan lahan yang paling banyak menyebabkan emisi/sequestrasi.")

addNewLine(rtffile)
text <- paste("\\b \\fs20 Peta kerapatan karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, " \\b \\fs20 (dalam Ton C/Ha)\\b0 \\fs20", sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.C1 )
rm(plot.C1)
text <- paste("\\b \\fs20 Peta kerapatan karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_2_rep, " \\b \\fs20 (dalam Ton C/Ha)\\b0 \\fs20", sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.C2 )
addNewLine(rtffile, n=1)
rm(plot.C2)
text <- paste("\\b \\fs20 Peta emisi karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.E )
addNewLine(rtffile, n=1)
rm(plot.E)
text <- paste("\\b \\fs20 Peta penyerapan karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep, sep="")
addParagraph(rtffile, text)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.S )
rm(plot.S)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, "\\b \\fs20 Intisari perhitungan emisi\\b0 \\fs20")
addNewLine(rtffile, n=1)
fs_table[2,3]<-printArea(as.numeric(as.character(fs_table[2,3])))
fs_table[3,3]<-printRate(as.numeric(as.character(fs_table[3,3])))
fs_table[4,3]<-printRate(as.numeric(as.character(fs_table[4,3])))
fs_table[5,3]<-printRate(as.numeric(as.character(fs_table[5,3])))
fs_table[6,3]<-printRate(as.numeric(as.character(fs_table[6,3])))
fs_table[7,3]<-printRate(as.numeric(as.character(fs_table[7,3])))
addTable(rtffile, fs_table)
addNewLine(rtffile, n=1)

addParagraph(rtffile, "\\b \\fs20 Intisari perhitungan emisi per unit perencanaan\\b0 \\fs20")
addNewLine(rtffile, n=1)
data_zone[2]<-printArea(data_zone[2])
addTable(rtffile, data_zone)
addNewLine(rtffile, n=1)

addNewLine(rtffile, n=1)
zone_carbon[2]<-printArea(zone_carbon[2])
zone_carbon[4]<-printRate(zone_carbon[4])
zone_carbon[5]<-printRate(zone_carbon[5])
zone_carbon[6]<-printRate(zone_carbon[6])
addTable(rtffile, zone_carbon)
addNewLine(rtffile, n=1)
addParagraph(rtffile, "\\b \\fs20 Intisari perhitungan emisi per wilayah administrasi\\b0 \\fs20")
addNewLine(rtffile, n=1)
admin_carbon[2]<-printArea(admin_carbon[2])
admin_carbon[4]<-printRate(admin_carbon[4])
admin_carbon[5]<-printRate(admin_carbon[5])
admin_carbon[6]<-printRate(admin_carbon[6])
addTable(rtffile, admin_carbon)
addParagraph(rtffile, "Keterangan : ")
addParagraph(rtffile, "Em_tot = Total Emisi dalam ton CO2-eq ")
addParagraph(rtffile, "Sq_tot = Total Sequestrasi dalam ton CO2-eq ")
addParagraph(rtffile, "Net_em = Total Emisi - Total Sequestrasi dalam ton CO2-eq ")
addParagraph(rtffile, "Net_em_rate = (Total Emisi - Total Sequestrasi) / (luas * periode waktu) dalam ton CO2-eq/ha.tahun ")
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, emissionRate )
addNewLine(rtffile, n=1)
rm(emissionRate)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.C.t1 )
addNewLine(rtffile, n=1)
rm(plot.Z.Avg.C.t1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.C.t2 )
addNewLine(rtffile, n=1)
rm(plot.Z.Avg.C.t2)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.em  )
addNewLine(rtffile, n=1)
rm(plot.Z.Avg.em)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.sq )
rm(plot.Z.Avg.sq)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, "\\b \\fs20 Sumber Emisi Terbesar\\b0 \\fs20")
addNewLine(rtffile, n=1)
tb_em_total_10[3]<-printRate(tb_em_total_10[3])
addTable(rtffile, tb_em_total_10)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestEmission )
addNewLine(rtffile, n=1)
rm(largestEmission)
addParagraph(rtffile, "\\b \\fs20 Sumber sequestrasi terbesar\\b0 \\fs20")
addNewLine(rtffile, n=1)
tb_seq_total_10[3]<-printRate(tb_seq_total_10[3])
addTable(rtffile, tb_seq_total_10)
addNewLine(rtffile, n=1)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestSeq )
addNewLine(rtffile, n=1)
rm(largestSeq)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, chapter3)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Pada bagian ini disajikan hasil analisis dinamika cadangan karbon untuk masing-masing kelas unit perencanaan yang dianalisis. Beberapa bentuk analisis yang dilakukan antara lain: tingkat emisi, tingkat sequestrasi, laju emisi dan tipe perubahan penggunaan lahan yang paling banyak menyebabkan emisi/sequestrasi.")

addNewLine(rtffile)

z.emission.name<-as.vector(NULL)
z.seq.name<-as.vector(NULL)
for(i in 1:length(zone_lookup$ID)){
  tryCatch({
    a<-zone_lookup$ID[i]
    zona<-paste("\\b", "\\fs20", i, "\\b0","\\fs20")
    zona_nm<-paste("\\b", "\\fs20", data_zone$Z_NAME[i], "\\b0","\\fs20")
    zona_ab<-paste("\\b", "\\fs20", data_zone$Z_CODE[i], "\\b0","\\fs20")
    addParagraph(rtffile, "\\b \\fs20 Sumber Emisi terbesar pada \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
    addNewLine(rtffile, n=1)
    
    tb_em_zon<-tb_em_zonal[which(tb_em_zonal$ZONE == a),]
    tb_em_zon$ZONE<-NULL
    tabel_em_zon<-tb_em_zon
    tabel_em_zon[3]<-printRate(tabel_em_zon[3])
    addTable(rtffile, tabel_em_zon)
    addNewLine(rtffile, n=1)
    
    #Largest emission
    largestE.Z<-ggplot(data=tb_em_zon, aes(x=reorder(LU_CODE, -em), y=(em))) + geom_bar(stat="identity", fill="blue") +
      geom_text(data=tb_em_zon, aes(x=LU_CODE, y=em, label=round(em, 1)),size=3, vjust=0.1) +
      ggtitle(paste("Sumber Emisi Terbesar Pada",i, "-", data_zone$Z_CODE[i] )) + guides(fill=FALSE) + ylab("CO2-eq") +
      theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
      theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
            panel.grid.major=element_blank(), panel.grid.minor=element_blank())
    
    png(filename=paste("Largest_Emission_Z_",a,".png", sep=""),
        type="cairo",
        units="in",
        width=6.7,
        height=4,
        res=125)
    print(largestE.Z)
    dev.off()
    
    z.emission.name<-c(z.emission.name, paste("Largest_Emission_Z_",a,".png", sep=""))
    
    addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestE.Z )
    addNewLine(rtffile, n=1)
    
    addParagraph(rtffile, "\\b \\fs20 Sumber Sequestrasi Terbesar Pada \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
    addNewLine(rtffile, n=1)
    
    tb_seq_zon<-tb_seq_zonal[which(tb_seq_zonal$ZONE == a),]
    tb_seq_zon$ZONE<-NULL
    tabel_seq_zon<-tb_seq_zon
    tabel_seq_zon[3]<-printRate(tabel_seq_zon[3])    
    addTable(rtffile, tabel_seq_zon)
    addNewLine(rtffile, n=1)
    
    #Largest Sequestration
    largestS.Z<-ggplot(data=tb_seq_zon, aes(x=reorder(LU_CODE, -seq), y=(seq))) + geom_bar(stat="identity", fill="green") +
      geom_text(data=tb_seq_zon, aes(x=LU_CODE, y=seq, label=round(seq, 1)),size=3, vjust=0.1) +
      ggtitle(paste("Sumber Sequestrasi Terbesar Pada",i, "-", data_zone$Z_CODE[i] )) + guides(fill=FALSE) + ylab("CO2-eq") +
      theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
      theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
            panel.grid.major=element_blank(), panel.grid.minor=element_blank())
    
    png(filename=paste("Largest_Seq_Z_",a,".png", sep=""),
        type="cairo",
        units="in",
        width=6.7,
        height=4,
        res=125)
    print(largestS.Z)
    dev.off()
    
    z.seq.name<-c(z.seq.name, paste("Largest_Seq_Z_",a,".png", sep=""))
    
    addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestS.Z )
    addNewLine(rtffile, n=1)
    
  },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
}
rm(largestE.Z, largestS.Z)

addNewLine(rtffile)
done(rtffile)

eval(parse(text=(paste("rtffileQUESC_", check_record, " <- rtffile", sep=""))))
eval(parse(text=(paste("QUESC_database_", pu_name, "_", data[1,2], "_", data[2,2], " <- data_merge", sep=""))))
eval(parse(text=(paste("resave(rtffileQUESC_", check_record, ",run_record, QUESC_database_", pu_name, "_", data[1,2], "_", data[2,2], ", QUESC.index,lut.c, file=proj.file)", sep=""))))

command<-paste("start ", "winword ", dirQUESC, "/LUMENS_QUES-C_report.lpr", sep="" )
shell(command)

#====write LUMENS log file====
# add.log<-data.frame(IDX=(QUESC.index), 
#                     MODULE="QUES-C", 
#                     DATE=format(Sys.time(), "%d-%m%-%Y"),
#                     TIME=format(Sys.time(), "%X"),
#                     LU1=data[1,1],
#                     LU2=data[2,1],
#                     PU=pu[1],
#                     T1=T1,
#                     T2=T2,
#                     LOOKUP_LC="From DB",
#                     LOOKUP_C=Look_up_table,
#                     LOOKUP_ZONE="From DB",
#                     NODATA=raster.nodata,
#                     OUTPUT_FOLDER=dirQUESC, row.names=NULL)
# log.quesc<-na.omit(rbind(log.quesc,add.log))
# write.csv(log.quesc, paste(user_temp_folder,"/LUMENS/LUMENS_quesc.log", sep=""))

gc()

