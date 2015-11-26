##[LUMENS]=group
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS", sep="")
sink(paste(LUMENS_path_user, "/LUMENS.log", sep=""))
cat()
sink()
