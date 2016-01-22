##Alpha - DATABASE=group
##category=selection Land Use/Cover; Planning Unit; Factor
##data=raster
##period=number 0
##description=string
##statuscode=output table
##statusmessage=output table

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
raster_category<-function(category, index, name, desc) {
  eval(parse(text=(paste(name,"_", index, "<<-", command,'("', data, '")', sep=""))))
  eval(parse(text=(paste(name,"_", index, "<<-spatial_sync_raster(",name,"_", index, ',', 'ref, method = "ngb")', sep=""))))
  eval(parse(text=(paste(name,"_", index, "<<-", name,"_", index, "*1",  sep=""))))
  eval(parse(text=(paste("names(",name,"_", index, ")<<-desc", sep=""))))
  eval(parse(text=(paste("",name,"_", index, "@title<<-category", sep=""))))
}

#CLASSIFY RASTER INTO THREE CATEGORY INPUT 
if(category==0){
  category<-"land_use_cover"
  data_name<-paste("Landuse",sep="")
  
  landuse.index<-landuse.index+1
  period.index<-period.index+1
  eval(parse(text=(paste("period", period.index, "<-period", sep=""))))
  period_i<-paste("period", period.index, sep="")
  eval(parse(text=(paste(period_i, "<-period", sep="" ))))
  
  index1<-landuse.index
  
  raster_category(category=category, index=index1, name=data_name, desc=description)
  
  eval(parse(text=(paste("freq",data_name, "_", landuse.index, "<-as.data.frame(na.omit(freq(", data_name,"_", landuse.index, ")))",  sep=""))))
  eval(parse(text=(paste("resave(", data_name,"_", landuse.index, ",landuse.index,freq", data_name,"_",landuse.index,",",period_i, ",period.index,file=lumens_database)", sep=""))))
} else if(category==1){
  category<-"planning_unit"
  data_name<-"pu_pu"
  
  pu_rec.index<-pu_rec.index+1
  index<-pu_rec.index
  
  raster_category(category=category, index=index1, name=data_name, desc=description)
  
  eval(parse(text=(paste("lut.pu", pu_rec.index, "<-as.data.frame(na.omit(freq(", data_name,"_", pu_rec.index, ")))",  sep=""))))
  eval(parse(text=(paste("resave(", data_name,pu_rec.index, ",lut.pu",pu_rec.index,",pu_rec.index, file=lumens_database)", sep=""))))
} else {
  category<-"factor_data"
  data_name<-"factor"
  
  SCIENDO1.index<-SCIENDO1.index+1
  index<-SCIENDO1.index
  
  raster_category(category=category, index=index1, name=data_name, desc=description)
  
  eval(parse(text=(paste("resave(", data_name, "_", SCIENDO1.index, ",SCIENDO1.index, file=lumens_database)", sep=""))))
}


statuscode<-1
statusmessage<-"editing attribute table stage"

#====next====