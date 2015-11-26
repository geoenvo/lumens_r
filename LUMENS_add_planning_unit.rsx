##[LUMENS]=group
##data=raster
##lut=file
##name=string
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

#IMPORTING DATA INTO LUMENS DATABASE
command="raster"
pu_rec.index<-pu_rec.index+1
data_name<-"pu_pu"
eval(parse(text=(paste(data_name, pu_rec.index, "<-", command,'("', data, '")', sep=""))))
eval(parse(text=(paste(data_name,pu_rec.index, "<-spatial_sync_raster(",data_name,pu_rec.index, ',', 'ref, method = "ngb")', sep=""))))
eval(parse(text=(paste(data_name,pu_rec.index, "<-", data_name,pu_rec.index, "*1",  sep=""))))
eval(parse(text=(paste("lut.pu",pu_rec.index,"<-read.table(lut, header=TRUE, sep=',',)", sep=""))))
eval(parse(text=(paste("names(pu_pu", pu_rec.index, ')<-"', name,'"', sep=""))))
eval(parse(text=(paste("resave(", data_name,pu_rec.index, ",lut.pu",pu_rec.index,",pu_rec.index, file=lumens_database)", sep=""))))


gc()
