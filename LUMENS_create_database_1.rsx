##Alpha - DATABASE=group
##working_directory=folder
##project=string (enter name of the project)
##location=string (enter location)
##province=string (enter province name of your location)
##country=string (enter country name)
##description=string
##data=raster
##admin_attribute=vector
##field_attribute=field admin_attribute
##p.admin.df=output table

library(stringr)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#build LUMENS project folder structure
setwd(working_directory)
project<-str_replace_all(string=project, pattern=" ", repl="_")
LUMENS_path <- paste(working_directory, "/", project, sep="")
PUR_path <- paste(LUMENS_path, "/PUR", sep="")
QUES_path <- paste(LUMENS_path, "/QUES", sep="")
PreQUES_path <- paste(QUES_path, "/PreQUES", sep="")
QUESC_path <- paste(QUES_path, "/QUES-C", sep="")
QUESB_path <- paste(QUES_path, "/QUES-B", sep="")
QUESH_path <- paste(QUES_path, "/QUES-H", sep="")
TA_path <- paste(LUMENS_path, "/TA", sep="")
SCIENDO_path  <- paste(LUMENS_path, "/SCIENDO", sep="")

dir.create(LUMENS_path, mode="0777")
dir.create(PUR_path, mode="0777")
dir.create(QUES_path, mode="0777")
dir.create(PreQUES_path, mode="0777")
dir.create(QUESC_path, mode="0777")
dir.create(QUESB_path, mode="0777")
dir.create(QUESH_path, mode="0777")
dir.create(TA_path, mode="0777")
dir.create(SCIENDO_path, mode="0777")

#create LUMENS.log
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS", sep="")
dir.create(LUMENS_path_user, mode="0777")
sink(paste(LUMENS_path_user, "/LUMENS.log", sep=""))
cat(working_directory, project, time_start, sep=",")
sink()

#WRITE PROJECT PROPERTIES
#project_name<-paste(project,".lpj", sep="")
db_name<-paste(project, ".lpj", sep="")
landuse.index=0
pu.index=0
pu_rec.index=0
lut_carbon.index=0
lut_landuse.index=0
lut_zone.index=0
period.index=0
PUR.index=0
PreQUES.index=0
QUESC.index=0
QUESB.index=0
QUESH.index=0
SCIENDO1.index=0
SCIENDO2.index=0
TA1.index=0
TA2.index=0
ref.index=1
admin.index=1

if (file.exists("C:/Program Files (x86)/LUMENS")){
win.arch = "32bit"
processing.path = "C:/Progra~2/LUMENS/apps/qgis/python/plugins/processing/r/scripts/"
} else{
win.arch = "64bit"
processing.path = "C:/Progra~1/LUMENS/apps/qgis/python/plugins/processing/r/scripts/"
}

#CREATE RESAVE FUNCTION
resave <- function(..., list = character(), file) {
previous  <- load(file)
var.names <- c(list, as.character(substitute(list(...)))[-1L])
for (var in var.names) assign(var, get(var, envir = parent.frame()))
save(list = unique(c(previous, var.names)), file = file)
}

#CREATE PROJECT DESCRIPTION TABLE
proj_descr <- as.data.frame(rbind(project, description, working_directory, location, province, country))

# #CREATE COVERAGE REFERENCE FOR PROJECT
# ref<-raster(data)
# ref<-ref*1
# Ref.name<-names(ref)
# Ref.type<-class(ref)
# Ref.source<-data
# Ref.coord<-as.character(crs(ref))
# Ref.res<-res(ref)
# Ref.xmin<-xmin(ref)
# Ref.xmax<-xmax(ref)
# Ref.ymin<-ymin(ref)
# Ref.ymax<-ymax(ref)
# cov.desc1<-c("Reference name","Reference class", "Reference source", "Reference CRS", "Reference Resolution", "Xmin", "Xmax", "Ymin", "Ymax")
# cov.desc2<-as.data.frame(rbind(Ref.name, Ref.type, Ref.source, Ref.coord, Ref.res, Ref.xmin, Ref.xmax, Ref.ymin, Ref.ymax))
# cov.desc2<-cov.desc2[1]
# cov.desc<-cbind(cov.desc1,cov.desc2)
# colnames(cov.desc)[1]<-"Coverage"
# colnames(cov.desc)[2]<-"Description"


#CREATE COVERAGE REFERENCE FOR PROJECT
ref<-data
ref<-ref*1
Ref.name<-names(ref)
Ref.type<-class(ref)
Ref.coord<-as.character(crs(ref))
Ref.res<-res(ref)
Ref.xmin<-xmin(ref)
Ref.xmax<-xmax(ref)
Ref.ymin<-ymin(ref)
Ref.ymax<-ymax(ref)
cov.desc1<-c("Reference name","Reference class", "Reference CRS", "Reference Resolution", "Xmin", "Xmax", "Ymin", "Ymax")
cov.desc2<-as.data.frame(rbind(Ref.name, Ref.type, Ref.coord, Ref.res, Ref.xmin, Ref.xmax, Ref.ymin, Ref.ymax))
cov.desc2<-cov.desc2[1]
cov.desc<-cbind(cov.desc1,cov.desc2)
colnames(cov.desc)[1]<-"Coverage"
colnames(cov.desc)[2]<-"Description"

test<-c(rownames(proj_descr))
proj_descr<-cbind(test, proj_descr)
colnames(proj_descr)[1]<-"Type"
colnames(proj_descr)[2]<-"Description"
proj_descr<-as.data.frame(proj_descr)
proj.file<-paste(LUMENS_path, "/",project,".lpj", sep="")

save(LUMENS_path_user,
landuse.index,
proj_descr,
ref,
location,
province,
country,
ref.index,
admin.index,
cov.desc,
pu.index,
pu_rec.index,
lut_carbon.index,
lut_landuse.index,
lut_zone.index,
period.index,
PUR.index,
PreQUES.index,
QUESC.index,
QUESB.index,
QUESH.index,
SCIENDO1.index,
SCIENDO2.index,
TA1.index,
TA2.index,
resave,
win.arch,
processing.path,
file=proj.file)

#ATTRIBUTE OF ADMIN
#library(rgdal)
#admin_attribute<-readOGR("C:/LUMENS_Papua/Propinsi_Papua/3_Data_vektor/Pap_kab1/adminoutput1.shp", layer="adminoutput1" )
test1<-as.data.frame(admin_attribute)
test2<-as.character(field_attribute)
# p.admin.df<-aggregate(IDADM~.,data=test1,FUN=mean)
eval(parse(text=(paste("p.admin.df<-aggregate(IDADM~",test2,",data=test1,FUN=mean)", sep=""  ))))
#p.admin.df<-edit(p.admin.df)

#====>> Next
