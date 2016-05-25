##Alpha - DATABASE=group
##active_project=string

#=Load active project that will be exported
load(active_project)
directory_project<-dirname(active_project)
LUMENS_project_file<-basename(active_project)
LUMENS_project<-basename(dirname(active_project))

#=Zipping process
# choose the DATA folder and .lpj file then archive and save it to project folder 
setwd(directory_project)
zip(zipfile=LUMENS_project, files=c(LUMENS_project_file, "DATA"))
