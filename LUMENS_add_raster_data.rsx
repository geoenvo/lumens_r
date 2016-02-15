##Alpha - DATABASE=group
##type=selection Land Use/Cover; Planning Unit; Factor
##data=raster
##period=number 0
##description=string
##attribute_table=string
##statusoutput=output table
##passfilenames

library(spatial.tools)

#READ LUMENS LOG FILE
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)
lumens_database<-proj.file

#RASTER COMMAND
command="raster"
raster_category<-function(category, name, desc) {
  eval(parse(text=(paste(name, "<<-", command,'("', data, '")', sep=""))))
  eval(parse(text=(paste(name, "<<-spatial_sync_raster(", name, ',', 'ref, method = "ngb")', sep=""))))
  eval(parse(text=(paste(name, "<<-", name, "*1",  sep=""))))
  eval(parse(text=(paste("names(", name, ")<<-desc", sep=""))))
  eval(parse(text=(paste(name, "@title<<-category", sep=""))))
}

#CLASSIFY RASTER INTO THREE TYPES INPUT 
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
    raster_category(category=category, name=paste(data_name, "_", index1, sep=""), desc=description) 
  }, error=function(e){ 
    statuscode<-0
    statusmessage<-e    
  })
  
  attribute_table<-read.table(attribute_table, sep=",")
  colnames(attribute_table)<-c("ID", "Legend", "Classified")
  eval(parse(text=(paste("freq", data_name, "_", landuse.index, "<-attribute_table",  sep=""))))
  
  eval(parse(text=(paste("resave(", data_name,"_", landuse.index, ",landuse.index,", period_i, ",freq",  data_name,"_", landuse.index, ",period.index,file=lumens_database)", sep=""))))
  
  csv_file<-paste(dirname(lumens_database),"/DATA/csv_", category, ".csv", sep="")
  if(file.exists(csv_file)){
    list_of_data<-read.table(csv_file, header=TRUE, sep=",", row.names=NULL)
    eval(parse(text=(paste("add_data<-data.frame(RST_DATA='", data_name, "_", landuse.index,"', RST_NAME=names(", data_name,"_", landuse.index, "), PERIOD=", period, ", LUT_NAME='freq", data_name,"_", landuse.index, "', row.names=NULL)", sep=""))))
    list_of_data<-rbind(list_of_data,add_data)
  } else {
    eval(parse(text=(paste("list_of_data<-data.frame(RST_DATA='", data_name, "_", landuse.index,"', RST_NAME=names(", data_name,"_", landuse.index, "), PERIOD=", period, ", LUT_NAME='freq", data_name,"_", landuse.index, "', row.names=NULL)", sep=""))))
  }
  write.table(list_of_data, csv_file, quote=FALSE, row.names=FALSE, sep=",")
  
  statuscode<-1
  statusmessage<-"land use/cover data has been added"
} else if(type==1){
  category<-"planning_unit"
  data_name<-"pu_pu"
  
  #write index
  pu.index<-pu.index+1
  index1<-pu.index
  
  tryCatch({
    raster_category(category=category, name=paste(data_name, index1, sep=""), desc=description) 
  }, error=function(e){ 
    statuscode<-0
    statusmessage<-e    
  })
  
  attribute_table<-read.table(attribute_table, sep=",")
  colnames(attribute_table)<-c("ID", "Legend")
  eval(parse(text=(paste("lut.pu", pu.index, "<-attribute_table",  sep=""))))
  
  eval(parse(text=(paste("resave(lut.pu", pu.index, ",", data_name, pu.index, ",pu.index, file=lumens_database)", sep=""))))
  
  csv_file<-paste(dirname(lumens_database),"/DATA/csv_", category, ".csv", sep="")
  if(file.exists(csv_file)){
    list_of_data<-read.table(csv_file, header=TRUE, sep=",")
    eval(parse(text=(paste("add_data<-data.frame(RST_DATA='", data_name, pu.index,"', RST_NAME=names(", data_name, pu.index, "),", "LUT_NAME='lut.pu", pu.index, "', row.names=NULL)", sep=""))))
    list_of_data<-rbind(list_of_data,add_data)
  } else {
    eval(parse(text=(paste("list_of_data<-data.frame(RST_DATA='", data_name, pu.index,"', RST_NAME=names(", data_name, pu.index, "),", "LUT_NAME='lut.pu", pu.index, "', row.names=NULL)", sep=""))))
  }
  write.table(list_of_data, csv_file, quote=FALSE, row.names=FALSE, sep=",")
  
  statuscode<-1
  statusmessage<-"planning unit has been added"
} else if(type==2){
  category<-"factor_data"
  data_name<-"factor"
  
  #write index
  factor.index<-factor.index+1
  index1<-factor.index
  
  tryCatch({
    raster_category(category=category, name=paste(data_name, index1, sep=""), desc=description) 
  }, error=function(e){ 
    statuscode<-0
    statusmessage<-e    
  })
  
  eval(parse(text=(paste("resave(", data_name, factor.index, ",factor.index, file=lumens_database)", sep=""))))
  
  csv_file<-paste(dirname(lumens_database),"/DATA/csv_", category, ".csv", sep="")
  if(file.exists(csv_file)){
    list_of_data<-read.table(csv_file, header=TRUE, sep=",")
    eval(parse(text=(paste("add_data<-data.frame(RST_DATA='", data_name, factor.index,"', RST_NAME=names(", data_name, factor.index, "), row.names=NULL)", sep=""))))
    list_of_data<-rbind(list_of_data,add_data)
  } else {
    eval(parse(text=(paste("list_of_data<-data.frame(RST_DATA='", data_name, factor.index,"', RST_NAME=names(", data_name, factor.index, "), row.names=NULL)", sep=""))))
  }
  write.table(list_of_data, csv_file, quote=FALSE, row.names=FALSE, sep=",")
  
  statuscode<-1
  statusmessage<-"factor data has been added!"
}

statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
