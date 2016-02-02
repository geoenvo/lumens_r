##Alpha - DATABASE=group
##category=selection Land Use/Cover; Planning Unit
##data=vector
##attribute_field_id=field data
##attribute_field_name=field data
##period=number 0
##description=string
##statusoutput=output table

library(foreign)
library(stringr)

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

#READ VECTORS 
#data<-readOGR(data, "Banyuasin_HGU_48s")
setwd(LUMENS_temp_user)
description<-str_replace_all(string=description, pattern=" ", repl=".")
writeOGR(data, dsn=LUMENS_temp_user, description, driver="ESRI Shapefile")
pu_data_attr<-paste(LUMENS_temp_user,"/",description,".dbf", sep="")
pu_data_attr<-read.dbf(pu_data_attr)
eval(parse(text=(paste("pu_data_attr<-subset(pu_data_attr, select=c(", attribute_field_id, ",", attribute_field_name, "))", sep="")))) 

shp_dir<-paste(LUMENS_temp_user,"/", description, ".shp", sep="")
file_out<-paste(LUMENS_temp_user, "/", description,  ".tif", sep="")
res<-res(ref)[1]
if (file.exists("C:/Program Files (x86)/LUMENS/bin/gdal_rasterize.exe")){
  gdalraster = "C:/Progra~2/LUMENS/bin/gdal_rasterize.exe "
} else{
  gdalraster = "C:/Progra~1/LUMENS/bin/gdal_rasterize.exe "
}
osgeo_comm<-paste(gdalraster, shp_dir, file_out,"-a", attribute_field_id, "-tr", res, res, "-a_nodata 255 -ot Byte", sep=" ")
system(osgeo_comm)

command="raster"
raster_category<-function(category, raster_data, index, name, desc) {
  eval(parse(text=(paste(name,"_", index, "<<-", command,'("', raster_data, '")', sep=""))))
  eval(parse(text=(paste(name,"_", index, "<<-spatial_sync_raster(",name,"_", index, ',', 'ref, method = "ngb")', sep=""))))
  eval(parse(text=(paste(name,"_", index, "<<-", name,"_", index, "*1",  sep=""))))
  eval(parse(text=(paste("names(",name,"_", index, ")<<-desc", sep=""))))
  eval(parse(text=(paste("",name,"_", index, "@title<<-category", sep=""))))
}

tif_file<-file_out
if(category==0){
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
    raster_category(category=category, raster_data=tif_file, index=index1, name=data_name, desc=description) 
  }, error=function(e){ 
    statuscode<-0
    statusmessage<-e    
  })
  
  #Does it still need to be merged? Worth trying..
  #eval(parse(text=(paste("attribute_table<-as.data.frame(na.omit(freq(", data_name,"_", landuse.index, ")))",  sep=""))))
  #merge (?)
  eval(parse(text=(paste("freq", data_name, "_", landuse.index, "<-pu_data_attr",  sep=""))))
  eval(parse(text=(paste("resave(", data_name,"_", landuse.index, ",landuse.index,", period_i, ",period.index,file=lumens_database)", sep=""))))
  
  statuscode<-1
  statusmessage<-"land use/cover data has been added"
} else {
  category<-"planning_unit"
  data_name<-"pu_pu"
  
  #write index
  pu.index<-pu.index+1
  index1<-pu.index
  
  tryCatch({
    raster_category(category=category, raster_data=tif_file, index=index1, name=data_name, desc=description)
  }, error=function(e){ 
    statuscode<-0
    statusmessage<-e    
  })
  
  
  #eval(parse(text=(paste("attribute_table<-as.data.frame(na.omit(freq(", data_name,"_", pu.index, ")))",  sep=""))))
  #merge(?)
  eval(parse(text=(paste("lut.pu", pu.index, "<-pu_data_attr",  sep=""))))
  eval(parse(text=(paste("resave(lut.pu", pu.index, ",", data_name, pu.index, ",pu.index, file=lumens_database)", sep=""))))
  
  statuscode<-1
  statusmessage<-"planning unit has been added"
}
 
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)