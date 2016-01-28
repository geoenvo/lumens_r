##Alpha - DATABASE=group
##category=selection Land Use/Cover; Planning Unit
##attribute_table=file
##statuscode=output number
##statusmessage=output string

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

attribute_table<-read.table(attribute_table, header=TRUE, sep=",")

#CLASSIFY RASTER INTO THREE CATEGORY INPUT 
if(category==0){
  data_name<-"Landuse"
  eval(parse(text=(paste("freq", data_name, "_", landuse.index, "<-attribute_table",  sep=""))))
  eval(parse(text=(paste("resave(freq", data_name,"_", landuse.index, ",file=lumens_database)", sep=""))))
  statuscode<-1
  statusmessage<-"land use/cover data has been added"
} else {
  data_name<-"lut.pu"
  eval(parse(text=(paste("lut.pu", pu.index, "<-attribute_table",  sep=""))))
  eval(parse(text=(paste("resave(lut.pu", pu.index, ", file=lumens_database)", sep=""))))
  statuscode<-1
  statusmessage<-"planning unit has been added"
}

>statuscode
>statusmessage