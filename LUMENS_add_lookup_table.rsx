##Alpha - DATABASE=group
##description=string
##attribute_table=string
##statusoutput=output table

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

#SET PATH OF DATA DIRECTORY 
data_dir<-paste(dirname(proj.file), "/DATA/", sep="")
setwd(data_dir)

description<-str_replace_all(string=description, pattern=" ", repl=".")

lut.index<-lut.index+1
eval(parse(text=(paste("lut", lut.index, "<-read.table(attribute_table, header=TRUE, sep=",")", sep=""))))

csv_file<-paste(dirname(proj.file),"/DATA/csv_lookup_table.csv", sep="")
if(file.exists(csv_file)){
  list_of_data_lut<-read.table(csv_file, header=TRUE, sep=",")
  eval(parse(text=(paste("add_data<-data.frame(TBL_DATA='lut", lut.index,"', TBL_NAME='", description, "', row.names=NULL)", sep=""))))
  list_of_data_lut<-rbind(list_of_data_lut,add_data)
} else {
  eval(parse(text=(paste("list_of_data_lut<-data.frame(TBL_DATA='lut", lut.index,"', TBL_NAME='", description, "', row.names=NULL)", sep=""))))
}
write.table(list_of_data_lut, csv_file, quote=FALSE, row.names=FALSE, sep=",")

#check existing rdb and rdx, then load to environment
category<-"lookup_table"
file_rdb<-paste(category, ".rdb", sep="")
file_rdx<-paste(category, ".rdx", sep="")
check_rdb<-file.exists(paste(data_dir, file_rdb, sep=""))
check_rdx<-file.exists(paste(data_dir, file_rdx, sep=""))
tmpEnv<-new.env(parent = emptyenv())
if(check_rdb & check_rdx){
  lazyLoad(category, tmpEnv)  
} 
#write new data to new environment
eval(parse(text=(paste("tmpEnv$lut", lut.index, "<-lut", lut.index, sep="")))) 
tmpEnv$list_of_data_lut<-list_of_data_lut
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
resave(lut.index, file=proj.file)

statuscode<-1
statusmessage<-"Lookup table has been added"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
