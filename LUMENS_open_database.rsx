##[LUMENS]=group
##project_file=file
##overview=output raster
##passfilenames

load(project_file)

#Reopen LUMENS.log
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS", sep="")
sink(paste(LUMENS_path_user, "/LUMENS.log", sep=""))
working_directory<-paste(proj_descr[3,2], sep="")
project<-as.character(proj_descr[1,2])
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
cat(working_directory, project, time_start, sep=",")
sink()
overview<-ref