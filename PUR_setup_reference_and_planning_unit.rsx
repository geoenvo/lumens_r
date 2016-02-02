##Alpha - PUR=group
##ref_data=vector
##Field=field ref_data
##data_name=string
##ref_class=file
##ref_mapping=file
##pu_units=file
##statusoutput=output table

library(foreign)

#READ LUMENS LOG FILE
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)

#Increment index then create working directory based on index
PUR.index=PUR.index+1
resave(PUR.index, file=proj.file)
wd_user<-paste(log.file[1,1], "/", log.file[1,2],"/PUR/PUR_analysis_",PUR.index, sep="")
dir.create(wd_user, mode="0777")
setwd(wd_user)

#Load and merge data with reference map
#ref_data<-readOGR(ref_data, "RTRWP_2014_50N")
sa<-subset(ref_data, select=Field)
tabel_acuan<-read.table(ref_class, header=FALSE, sep=",")
colnames(tabel_acuan)[1]="acuan_kelas"
colnames(tabel_acuan)[2]="acuan_kode"
tabel_mapping<-read.table(ref_mapping, header=FALSE, sep=",")
colnames(tabel_mapping)[1]=Field
colnames(tabel_mapping)[2]="IDS"
tabel_mapping$IDO<-seq(countrow)
ref_map<-merge(sa, tabel_mapping, by=Field)

#Save reference table and map to temporary folder
target_file<-paste(wd_user,"/PURREF_",data_name, ".csv", sep="")
write.csv(tabel_mapping, target_file)
write.csv(tabel_acuan, "hirarki_rekonsiliasi.csv")
wd_usertemp<-paste(wd_user,"/temp", sep="")
writeOGR(ref_map, dsn=wd_usertemp, data_name, driver="ESRI Shapefile")

#Rasterize shapefile using gdal_rasterize
shp_dir<-paste(wd_usertemp,"/", data_name, ".shp", sep="")
file_out<-paste(wd_user,"/", "PURREF_" ,data_name, ".tif", sep="")
kolom_data<-paste('IDO')
res<-res(ref)[1]
if (file.exists("C:/Program Files (x86)/LUMENS/bin/gdal_rasterize.exe")){
  gdalraster = "C:/Progra~2/LUMENS/bin/gdal_rasterize.exe "
} else{
  gdalraster = "C:/Progra~1/LUMENS/bin/gdal_rasterize.exe "
}
# "gdalraster shp_dir file_out -a kolom_data -tr 100 100 -a_nodata 255 -ot Byte" (-a_nodata 65535 <- default to 64Float)
osgeo_comm<-paste(gdalraster, shp_dir, file_out, "-a", kolom_data, "-tr", res, res, "-a_nodata 255 -ot Byte", sep=" ")
system(osgeo_comm)

#Check if file_out is exist (?)

#=================STEP2=================
pu_list<-read.table(pu_units, header=FALSE, sep=",")
n_pu_list<-nrow(pu_list)
for(i in 4:n_pu_list){
  #Set planning unit parameter 
  pu_data<-as.character(pu_list[i,1])
  Field<-as.character(pu_list[i,2])
  data_name<-as.character(pu_list[i,3])
  Type<-as.character(pu_list[i,5])

  #Set workdir and load planning unit map
  wd_data<-dirname(pu_data)
  setwd(wd_data)
  st_area_file<- substr(basename(pu_data), 1, nchar(basename(pu_data)) - 4)
  sa<-readOGR(dsn=wd_data, st_area_file)
  
  #Save reference table and map to temporary folder based on type
  pu_data_attr<-as.data.frame(sa)
  pu_data_attr_unique<-eval(parse(text=(paste("unique(pu_data_attr$",Field, ")", sep=""))))
  pu_data_attr_unique<-as.data.frame(pu_data_attr_unique)
  pu_data_attr_unique$IDS<-as.character(pu_list[i,4])
  colnames(pu_data_attr_unique)[1]<-Field
  if (Type==0) {
    Type<-"PUR"
    TifType<-"PU"
  } else {
    Type<-"PURADD"
    TifType<-"ADD"
  }
  test3<-merge(sa, pu_data_attr_unique, by=Field)
  test3<-test3[,c(Field,"IDS")]
  eval(parse(text=(paste("csv_pu<-paste(wd_usertemp,'/',data_name,'_", Type, ".csv', sep='')", sep=""))))
  write.csv(test3, csv_pu)
  writeOGR(test3, dsn=wd_usertemp, data_name, driver="ESRI Shapefile")
  
  shp_dir<-paste(wd_usertemp,"/", data_name, ".shp", sep="")
  file_out<-paste(wd_usertemp,"/",data_name, "-", TifType,  ".tif", sep="")
  kolom_data<-paste('IDS')
  res<-res(ref)[1]
  if (file.exists("C:/Program Files (x86)/LUMENS/bin/gdal_rasterize.exe")){
    gdalraster = "C:/Progra~2/LUMENS/bin/gdal_rasterize.exe "
  } else{
    gdalraster = "C:/Progra~1/LUMENS/bin/gdal_rasterize.exe "
  }
  osgeo_comm<-paste(gdalraster,shp_dir, file_out,"-a",kolom_data, "-tr", res, res, "-a_nodata 255 -ot Byte", sep=" ")
  system(osgeo_comm)
}

statuscode<-1
statusmessage<-"PUR database has been setup!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)