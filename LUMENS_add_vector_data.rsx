##Alpha - DATABASE=group
##proj.file=string
##type=selection Land Use/Cover; Planning Unit
##data=vector
##attribute_field_id=field data
##period=number 0
##description=string
##attribute_table=string
##statusoutput=output table

#=Load library
library(stringr)
library(spatial.tools)

#=Load active project 
load(proj.file)

#=Set temporary folder
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}

#=Write temporary vector to temporary folder
setwd(user_temp_folder)
description<-str_replace_all(string=description, pattern=" ", repl=".")
writeOGR(data, dsn=user_temp_folder, description, overwrite_layer=TRUE, driver="ESRI Shapefile")

shp_dir<-paste(user_temp_folder,"/", description, ".shp", sep="")
file_out<-paste(user_temp_folder, "/", description,  ".tif", sep="")
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

#=Set working directory to DATA folder 
data_dir<-paste(dirname(proj.file), "/DATA/", sep="")
setwd(data_dir)

#=Classify raster into two types of input
# type 0: land_use_cover
# type 1: planning_unit
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
  colnames(attribute_table)<-c("ID", "Old_Legend", "Legend", "Classified")
  eval(parse(text=(paste("freqR<-na.omit(as.data.frame(freq(", data_name, "_", landuse.index, ")))", sep=""))))
  colnames(freqR)<-c("ID", "COUNT")
  attribute_table<-merge(attribute_table, freqR, by="ID")
  attribute_table<-subset(attribute_table, select=c('ID', 'COUNT', 'Legend', 'Classified'))
  eval(parse(text=(paste("freq", data_name, "_", landuse.index, "<-attribute_table",  sep=""))))

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

  #check existing rdata and list of data land use/cover (list_of_data_luc)
  file_rdata<-paste(data_dir, category, sep="")
  check_rdata<-file.exists(file_rdata)
  if(check_rdata){
    eval(parse(text=(paste("resave(", data_name, "_", landuse.index, ", freq", data_name, "_", landuse.index, ", list_of_data_luc, file=file_rdata)", sep="")))) 
  } else {
    eval(parse(text=(paste("save(", data_name, "_", landuse.index, ", freq", data_name, "_", landuse.index, ", list_of_data_luc, file=file_rdata)", sep="")))) 
  }
  eval(parse(text=(paste("resave(landuse.index, period.index, ", period_i, ", file=proj.file)", sep=""))))
  #create lazyLoad database
  e = local({load(category); environment()})
  tools:::makeLazyLoadDB(e, category)
    
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
  colnames(attribute_table)<-c("ID", attribute_field_id)
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
  
  #check existing rdata and list of data planning unit (list_of_data_pu)
  file_rdata<-paste(data_dir, category, sep="")
  check_rdata<-file.exists(file_rdata)
  if(check_rdata){
    eval(parse(text=(paste("resave(", data_name, pu.index, ", lut.pu", pu.index, ", list_of_data_pu, file=file_rdata)", sep="")))) 
  } else {
    eval(parse(text=(paste("save(", data_name, pu.index, ", lut.pu", pu.index, ", list_of_data_pu, file=file_rdata)", sep="")))) 
  }
  resave(pu.index, file=proj.file)
  #create lazyLoad database
  e = local({load(category); environment()})
  tools:::makeLazyLoadDB(e, category)
  
  statuscode<-1
  statusmessage<-"planning unit has been added"
}
 
#=Writing final status message (code, message)
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)