##Alpha - DATABASE=group
##type=selection Land Use/Cover; Planning Unit
##data=vector
##attribute_field_id=field data
##period=number 0
##description=string
##attribute_table=string
##statusoutput=output table

library(stringr)
library(spatial.tools)

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

#WRITE VECTORS 
setwd(LUMENS_temp_user)
description<-str_replace_all(string=description, pattern=" ", repl=".")
writeOGR(data, dsn=LUMENS_temp_user, description, overwrite_layer=TRUE, driver="ESRI Shapefile")

shp_dir<-paste(LUMENS_temp_user,"/", description, ".shp", sep="")
file_out<-paste(LUMENS_temp_user, "/", description,  ".tif", sep="")
res<-res(ref)[1]
if (file.exists("C:/Program Files (x86)/LUMENS/bin/gdal_rasterize.exe")){
  gdalraster = "C:/Progra~2/LUMENS/bin/gdal_rasterize.exe "
} else{
  gdalraster = "C:/Progra~1/LUMENS/bin/gdal_rasterize.exe "
}
osgeo_comm<-paste(gdalraster, shp_dir, file_out,"-a IDADM -tr", res, res, "-a_nodata 255 -ot Byte", sep=" ")
system(osgeo_comm)

raster_category<-function(category, raster_data, name, desc) {
  eval(parse(text=(paste(name, "<<-spatial_sync_raster(raster_data, ref, method = 'ngb')", sep=""))))
  eval(parse(text=(paste(name, "<<-", name, "*1",  sep=""))))
  eval(parse(text=(paste("names(",name, ")<<-desc", sep=""))))
  eval(parse(text=(paste(name, "@title<<-category", sep=""))))
}

#SET PATH OF DATA DIRECTORY 
data_dir<-paste(dirname(proj.file), "/DATA/", sep="")
setwd(data_dir)

tif_file<-raster(file_out)
if(type==0){
  category<-"land_use_cover"
  data_name<-"Landuse"
  
  #write index
  landuse.index<-landuse.index+1
  period.index<-period.index+1
  eval(parse(text=(paste("period", period.index, "<-period", sep=""))))
  period_i<-paste("period", period.index, sep="")
  eval(parse(text=(paste(period_i, "<-period", sep="" ))))
  index1<-landuse.index
  
  tryCatch({
    raster_category(category=category, raster_data=tif_file, name=paste(data_name, "_", index1, sep=""), desc=description) 
  }, error=function(e){ 
    statuscode<-0
    statusmessage<-e    
  })
  
  attribute_table<-read.table(attribute_table, sep=",")
  colnames(attribute_table)<-c("Legend", "Classified")
  eval(parse(text=(paste("freq", data_name, "_", landuse.index, "<-attribute_table",  sep=""))))
  #merge with existing freqTable (?)
  #Does it still need to be merged? Worth trying..
  #eval(parse(text=(paste("attribute_table<-as.data.frame(na.omit(freq(", data_name,"_", landuse.index, ")))",  sep=""))))
  
  #write raster detail to csv
  csv_file<-paste(dirname(proj.file),"/DATA/csv_", category, ".csv", sep="")
  if(file.exists(csv_file)){
    list_of_data_luc<-read.table(csv_file, header=TRUE, sep=",", row.names=NULL)
    eval(parse(text=(paste("add_data<-data.frame(RST_DATA='", data_name, "_", landuse.index,"', RST_NAME=names(", data_name,"_", landuse.index, "), PERIOD=", period, ", LUT_NAME='freq", data_name,"_", landuse.index, "', row.names=NULL)", sep=""))))
    list_of_data_luc<-rbind(list_of_data_luc,add_data)
  } else {
    eval(parse(text=(paste("list_of_data_luc<-data.frame(RST_DATA='", data_name, "_", landuse.index,"', RST_NAME=names(", data_name,"_", landuse.index, "), PERIOD=", period, ", LUT_NAME='freq", data_name,"_", landuse.index, "', row.names=NULL)", sep=""))))
  }
  write.table(list_of_data_luc, csv_file, quote=FALSE, row.names=FALSE, sep=",")

  #check existing rdb and rdx, then load to environment
  file_rdb<-paste(category, ".rdb", sep="")
  file_rdx<-paste(category, ".rdx", sep="")
  check_rdb<-file.exists(paste(data_dir, file_rdb, sep=""))
  check_rdx<-file.exists(paste(data_dir, file_rdx, sep=""))
  tmpEnv<-new.env(parent = emptyenv())
  if(check_rdb & check_rdx){
    lazyLoad(category, tmpEnv)  
  } 
  #write new data to new environment
  eval(parse(text=(paste("tmpEnv$", data_name, "_", landuse.index, "<-", data_name, "_", landuse.index, sep="")))) 
  eval(parse(text=(paste("tmpEnv$freq", data_name, "_", landuse.index, "<-freq", data_name,"_", landuse.index, sep="" ))))
  tmpEnv$list_of_data_luc<-list_of_data_luc
  ls.str(tmpEnv) 
  #make lazyLoad database
  if(check_rdb & check_rdx){
    tools:::makeLazyLoadDB(tmpEnv, paste(category, "_temp", sep=""))
    unlink(file_rdb)
    unlink(file_rdx)
    file.rename(paste(category, "_temp.rdb", sep=""), paste(category, ".rdb", sep=""))
    file.rename(paste(category, "_temp.rdx", sep=""), paste(category, ".rdx", sep=""))
  } else {
    tools:::makeLazyLoadDB(tmpEnv, category)
  }
  eval(parse(text=(paste("resave(landuse.index, period.index, ", period_i, ", file=proj.file)", sep=""))))
    
  statuscode<-1
  statusmessage<-"land use/cover data has been added"
} else {
  category<-"planning_unit"
  data_name<-"pu_pu"
  
  #write index
  pu.index<-pu.index+1
  index1<-pu.index
  
  tryCatch({
    raster_category(category=category, raster_data=tif_file, name=paste(data_name, index1, sep=""), desc=description)
  }, error=function(e){ 
    statuscode<-0
    statusmessage<-e 
    print(e)
  })
  
  attribute_table<-read.table(attribute_table, sep=",")
  colnames(attribute_table)<-c(attribute_field_id, "ID")
  #null kolom ketiga
  eval(parse(text=(paste("lut.pu", pu.index, "<-attribute_table",  sep=""))))
  #merge(?)
  
  csv_file<-paste(dirname(proj.file),"/DATA/csv_", category, ".csv", sep="")
  if(file.exists(csv_file)){
    list_of_data_pu<-read.table(csv_file, header=TRUE, sep=",")
    eval(parse(text=(paste("add_data<-data.frame(RST_DATA='", data_name, pu.index,"', RST_NAME=names(", data_name, pu.index, "),", "LUT_NAME='lut.pu", pu.index, "', row.names=NULL)", sep=""))))
    list_of_data_pu<-rbind(list_of_data_pu,add_data)
  } else {
    eval(parse(text=(paste("list_of_data_pu<-data.frame(RST_DATA='", data_name, pu.index,"', RST_NAME=names(", data_name, pu.index, "),", "LUT_NAME='lut.pu", pu.index, "', row.names=NULL)", sep=""))))
  }
  write.table(list_of_data_pu, csv_file, quote=FALSE, row.names=FALSE, sep=",")
  
  #check existing rdb and rdx, then load to environment
  file_rdb<-paste(category, ".rdb", sep="")
  file_rdx<-paste(category, ".rdx", sep="")
  check_rdb<-file.exists(paste(data_dir, file_rdb, sep=""))
  check_rdx<-file.exists(paste(data_dir, file_rdx, sep=""))
  tmpEnv<-new.env(parent = emptyenv())
  if(check_rdb & check_rdx){
    lazyLoad(category, tmpEnv)  
  } 
  #write new data to new environment
  eval(parse(text=(paste("tmpEnv$", data_name, pu.index, "<-", data_name, pu.index, sep="")))) 
  eval(parse(text=(paste("tmpEnv$lut.pu", pu.index, "<-lut.pu", pu.index, sep="" ))))
  tmpEnv$list_of_data_pu<-list_of_data_pu
  ls.str(tmpEnv) 
  #make lazyLoad database
  if(check_rdb & check_rdx){
    tools:::makeLazyLoadDB(tmpEnv, paste(category, "_temp", sep=""))
    unlink(file_rdb)
    unlink(file_rdx)
    file.rename(paste(category, "_temp.rdb", sep=""), paste(category, ".rdb", sep=""))
    file.rename(paste(category, "_temp.rdx", sep=""), paste(category, ".rdx", sep=""))
  } else {
    tools:::makeLazyLoadDB(tmpEnv, category)
  }
  resave(pu.index, file=proj.file)
  
  statuscode<-1
  statusmessage<-"planning unit has been added"
}
 
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)