##Alpha - DATABASE=group
##proj.file=string
##type=selection Land Use/Cover; Planning Unit; Factor
##data=raster
##period=number 0
##description=string
##attribute_table=string
##statusoutput=output table
##passfilenames

#=Load library
library(spatial.tools)

#=Load active project 
load(proj.file)

#=Set working directory to DATA folder 
data_dir<-paste(dirname(proj.file), "/DATA/", sep="")
setwd(data_dir)

#=Create raster_category function
# to synchronize all of the data spatial input
command="raster"
raster_category<-function(category, name, desc) {
  eval(parse(text=(paste(name, "<<-", command,'("', data, '")', sep=""))))
  eval(parse(text=(paste(name, "<<-spatial_sync_raster(", name, ',', 'ref, method = "ngb")', sep=""))))
  eval(parse(text=(paste(name, "<<-", name, "*1",  sep=""))))
  eval(parse(text=(paste("names(", name, ")<<-desc", sep=""))))
  eval(parse(text=(paste(name, "@title<<-category", sep=""))))
}

#=Classify raster into three types of input
# type 0: land_use_cover
# type 1: planning_unit
# type 2: factor
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
  
  #create raster data
  tryCatch({
    raster_category(category=category, name=paste(data_name, "_", index1, sep=""), desc=description) 
  }, error=function(e){ 
    statuscode<-0
    statusmessage<-e    
  })
  
  #create attribute table
  attribute_table<-read.table(attribute_table, sep=",")
  colnames(attribute_table)<-c("ID", "COUNT", "Legend", "Classified")
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
  colnames(attribute_table)<-c("ID", "COUNT", "Legend")
  eval(parse(text=(paste("lut.pu", pu.index, "<-attribute_table",  sep=""))))
  
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
  
  csv_file<-paste(dirname(proj.file),"/DATA/csv_", category, ".csv", sep="")
  if(file.exists(csv_file)){
    list_of_data_f<-read.table(csv_file, header=TRUE, sep=",")
    eval(parse(text=(paste("add_data<-data.frame(RST_DATA='", data_name, factor.index,"', RST_NAME=names(", data_name, factor.index, "), row.names=NULL)", sep=""))))
    list_of_data_f<-rbind(list_of_data_f,add_data)
  } else {
    eval(parse(text=(paste("list_of_data_f<-data.frame(RST_DATA='", data_name, factor.index,"', RST_NAME=names(", data_name, factor.index, "), row.names=NULL)", sep=""))))
  }
  write.table(list_of_data_f, csv_file, quote=FALSE, row.names=FALSE, sep=",")

  #check existing rdata and list of data factor (list_of_data_f)
  file_rdata<-paste(data_dir, category, sep="")
  check_rdata<-file.exists(file_rdata)
  if(check_rdata){
    eval(parse(text=(paste("resave(", data_name, factor.index, ", list_of_data_f, file=file_rdata)", sep="")))) 
  } else {
    eval(parse(text=(paste("save(", data_name, factor.index, ", list_of_data_f, file=file_rdata)", sep="")))) 
  }
  resave(factor.index, file=proj.file)
  #create lazyLoad database
  e = local({load(category); environment()})
  tools:::makeLazyLoadDB(e, category)
  
  statuscode<-1
  statusmessage<-"factor data has been added!"
}

#=Writing final status message (code, message)
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
