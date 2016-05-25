##Alpha - DATABASE=group
##new_working_directory=folder
##LUMENS_zip_file=file

#=Set project name
lumens_project<-as.character(unzip(paste(LUMENS_zip_file, sep=""), list=TRUE)$Name[1])
lumens_project<-unlist(strsplit(lumens_project, split = ".", fixed = TRUE))[1]

#=Set new working directory
LUMENS_path <- paste(new_working_directory, "/", lumens_project, sep="")
dir.create(LUMENS_path, mode="0777")
setwd(LUMENS_path)

#CREATE DIRECTORY IN NEW WORKING DIRECTORY
PUR_path <- paste(LUMENS_path, "/PUR", sep="")
QUES_path <- paste(LUMENS_path, "/QUES", sep="")
PreQUES_path <- paste(QUES_path, "/PreQUES", sep="")
QUESC_path <- paste(QUES_path, "/QUES-C", sep="")
QUESB_path <- paste(QUES_path, "/QUES-B", sep="")
QUESH_path <- paste(QUES_path, "/QUES-H", sep="")
TA_path <- paste(LUMENS_path, "/TA", sep="")
SCIENDO_path  <- paste(LUMENS_path, "/SCIENDO", sep="")

dir.create(PUR_path, mode="0777")
dir.create(QUES_path, mode="0777")
dir.create(PreQUES_path, mode="0777")
dir.create(QUESC_path, mode="0777")
dir.create(QUESB_path, mode="0777")
dir.create(QUESH_path, mode="0777")
dir.create(TA_path, mode="0777")
dir.create(SCIENDO_path, mode="0777")

#=Unzipping process to new working directory
# find the LUMENS project file
unzip(LUMENS_zip_file)
LUMENS_project_file<-list.files(path=".", full.names=TRUE, pattern="\\.lpj$")
load(LUMENS_project_file)

#=Change the project default directory after import
row.names(proj_descr)<-NULL
proj_descr$Description<-as.character(proj_descr$Description)
proj_descr[3,2] <- new_working_directory
resave(proj_descr, file=LUMENS_project_file)
