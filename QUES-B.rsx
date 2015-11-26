##[QUES]=group
##Wdir=folder
##LU1=raster
##LU2=raster
##zone=raster

##year1=number 1990
##year2=number 2000
##location=string
##gridres=number 10000
##windowsize=number 1000
##window.shape= number 0
##raster.nodata= number 0
##classdesc=file
##edgecon=file
##ldabase.preques=file
##habitat.reclass.lookup=file
##zone_lookup=file
##ref.map.id= number 1
##mwfile.init=output raster
##mwfile.final=output raster
##habitat.loss.NA=output raster
##habitat.degradation=output raster
##habitat.gain.NA=output raster
##habitat.recovery=output raster
##passfilenames


#set library which will be used
library(DBI)
library(raster)
library(rasterVis)
library(rgdal)
library(RSQLite)
library(SDMTools)
library(sp)
library(tiff)
library(foreign)
library(rgeos)
library(ggplot2)
library(pracma)
library(spatial.tools)
library(plyr)
library(rtf)
library(gridExtra)

time_start <- proc.time()

#set working directory
setwd(Wdir)
outpath<-paste(getwd())

#time series 1 requirements
lu1<-raster(LU1)
lu1_path<-paste(LU1)

#time series 2 requirements
lu2<-raster(LU2)
lu2_path<-paste(LU2)

#zone map requirements
zone<-raster(zone)

#options: 1 initial landuse as refrence, 2 final landuse as reference, 3 zone map as reference
if (ref.map.id==1){
print("Initial land use/cover map as reference")
ref.map<-lu1
} else if (ref.map.id==2){
print("Final land use/cover map as reference")
ref.map<-lu2
} else if (ref.map.id==3){
print("Planning unit/zone map as reference")
ref.map<-zone
} else {
stop("No map has been selected as reference")
}


#projection handling
if (grepl("+units=m", as.character(ref.map@crs))){
print("Raster maps have projection in meter unit")
Spat_res<-res(ref.map)[1]*res(ref.map)[2]/10000
paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-B will automatically generate data in Ha unit")
} else{
stop("Raster map projection is not in meter unit, please reproject your map")
}

#Extent handling and raster resolution land-cover maps

#checking landuse1
if (as.character(ref.map@crs)==as.character(lu1@crs)){
print("Final land use/cover map has the same projection")
if (res(ref.map)[1]==res(lu1)[1]){
print("initial land use/cover map has the same extent with the reference map")
} else{
print("initial land use/cover map doesn't have the same extent with the reference map, synchronising land-cover map...")
lu1<-spatial_sync_raster(lu1, ref.map, method = "ngb")
}
} else{
print("initial land use/cover map doesn't have the same projection with the reference map, synchronising land-cover map...")
lu1<-spatial_sync_raster(lu1, ref.map, method = "ngb")
}

#checking landuse2
if (as.character(ref.map@crs)==as.character(lu2@crs)){
print("Final land use/cover map has the same projection")
if (res(ref.map)[1]==res(lu2)[1]){
print("Final land use/cover map has the same extent with the reference map")
} else{
print("Final land use/cover map doesn't have the same extent with the reference map, synchronising land-cover map...")
lu2<-spatial_sync_raster(lu2, ref.map, method = "ngb")
}
} else{
print("Final land use/cover map doesn't have the same projection with the reference map, synchronising land-cover map...")
lu2<-spatial_sync_raster(lu2, ref.map, method = "ngb")
}

#checking zone map
if (as.character(ref.map@crs)==as.character(zone@crs)){
print("Planning unit/zone map has the same projection")
if (res(ref.map)[1]==res(zone)[1]){
print("planning unit/zone map has the same extent with the reference map")
} else{
print("planning unit/zone map doesn't have the same extent with the reference map, synchronising land-cover map...")
zone<-spatial_sync_raster(zone, ref.map, method = "ngb")
}
} else{
print("planning unit/zone map doesn't have the same projection with the reference map, synchronising land-cover map...")
zone<-spatial_sync_raster(zone, ref.map, method = "ngb")
}

#grid preparation
xl1<-xmin(lu1)
yl1<-ymin(lu1)
xu1<-xmax(lu1)
yu1<-ymax(lu1)
pjg<-xu1-xl1
lbr<-yu1-yl1
ncellx<-pjg/gridres
ncelly<-lbr/gridres
ncellx<-ceiling(ncellx)
ncelly<-ceiling(ncelly)

newproj<-proj4string(lu1)
r<-raster(xmn=xl1, xmx=xu1, ymn=yl1, ymx=yu1, ncol=ncellx, nrow=ncelly, crs=newproj)
res(r)<-gridres
vals <- 1:ncell(r)
r<-setValues(r,vals)
sampling.rast<-resample(r,lu1, method="ngb"); #sampling grid raster file

#Calculate total Area
allarea.init<-na.omit(as.data.frame(freq(lu1)))
colnames(allarea.init)<-c("ID","COUNT")
totarea.init<-sum(allarea.init$COUNT)
totarea.init<-(totarea.init*Spat_res)

allarea.final<-na.omit(as.data.frame(freq(lu2)))
colnames(allarea.final)<-c("ID","COUNT")
totarea.final<-sum(allarea.final$COUNT)
totarea.final<-(totarea.final*Spat_res)

totarea<-max(totarea.final,totarea.init)

#Apply nodata and synchronize nodata
lu1<- reclassify(lu1, cbind(raster.nodata,NA))
lu2<- reclassify(lu2, cbind(raster.nodata,NA))

lu1.temp <- lu1>=0
lu2.temp <- lu2>=0
lu.nodata.check<-lu1.temp*lu2.temp

lu1<-lu1*lu.nodata.check; #syncronized nodata raster map
lu2<-lu2*lu.nodata.check; #syncronized nodata raster map


#write new raster data
writeRaster(lu1,  filename="lu1", format="GTiff", overwrite=TRUE, NAflag=255)
lu1_path<-paste(getwd(),"/lu1.tif", sep='')
writeRaster(lu2,  filename="lu2", format="GTiff", overwrite=TRUE, NAflag=255)
lu2_path<-paste(getwd(),"/lu2.tif", sep='')

#DEFINE FOCAL AREA
#modify biodiversity lookup table
lookup_bh<- read.table(classdesc, header=TRUE, sep=",")
lookup_z<- read.table(zone_lookup, header=TRUE, sep=",")
colnames(lookup_z)[1]<-c("ZONE")
lookup_bh[lookup_bh==TRUE]<-1
lookup_bh[lookup_bh==FALSE]<-0
lookup_bh[4]<-NULL
colnames(lookup_bh)<-c("ID", "Name", "BIODIV")


#INITIAL FOCAL AREA
#focal area lookup table
foc.area.reclass.init<-merge(allarea.init,lookup_bh,by="ID")
foc.area.reclass.init$COUNT<-NULL
foc.area.reclass.init$Name<-NULL

foc.area.init<- reclassify(lu1, foc.area.reclass.init)
tothab.init<-zonal(foc.area.init, sampling.rast, 'sum')
tothab.init<-as.data.frame(tothab.init)

colnames(tothab.init) <- c("ID", "sum")
tothab.init$sum<-((tothab.init$sum/totarea)*100)

#FINAL FOCAL AREA
foc.area.reclass.final<-merge(allarea.final,lookup_bh,by="ID")
foc.area.reclass.final$COUNT<-NULL
foc.area.reclass.final$Name<-NULL

foc.area.final<- reclassify(lu2, foc.area.reclass.final)
tothab.final<-zonal(foc.area.final, sampling.rast, 'sum')
tothab.final<-as.data.frame(tothab.final)

colnames(tothab.final) <- c("ID", "sum")
tothab.final$sum<-((tothab.final$sum/totarea)*100)


#FRAGSTATS MOVING WINDOW
#Define centroids
polygrid<-rasterToPolygons(r, n=4, na.rm=FALSE, digits=12, dissolve=TRUE)
centro<- gCentroid(polygrid,byid=TRUE)

#Prepare fca file for processing tif
modid=1

internal<-paste('')
cpf<-paste('')
io<-paste('[BAND:1]')
desc<-paste('')
drlib<-paste('GDAL')
drname<-paste('GeoTIFF grid (.tif)')
drid<-paste('63B45E15-C8E5-44f6-A9AB-60E1852CDB5D')

#extent input of file 1
xl1<-xmin(lu1)
yl1<-ymin(lu1)
xu1<-xmax(lu1)
yu1<-ymax(lu1)

#cell size input of file 1
csize1<-xres(lu1)
#row and column size input of file 1
rowc1<-nrow(lu1)
colc1<-ncol(lu1)


aczero="1"
#no data value input
nodata=255
bvalue=999

#common tables input

contab<-read.table(file=edgecon, header=TRUE, sep=',', skip=1)
contab2<-round(contab, digits=2)

#check raster file directory for lu1
dirname_raster<-dirname(lu1_path)
setwd(dirname_raster)

for (i in 1:3){
mwout<-paste(lu1_path,'_mw',i, sep='')
teci.dir<-paste(mwout,"/",list.files(mwout), sep='')
if (file.exists(teci.dir)==TRUE){
mwout2<-paste(lu1_path,'_mw',i, sep='')
unlink(mwout2, recursive=TRUE)
print(paste(i,"deleting previous raster file found, algorithm continue..."))
}else{
print(paste(i,"no previous raster file found, algorithm continue..."))
}
}

#CHECK FRAGSTATS MODEL AVAILABILITY
#if (file.exists("C:/Program Files (x86)/LUMENS//teciuf.fca")){
#  fca<-shQuote("C:/Program Files (x86)/LUMENS//teciuf.fca")
#} else if (file.exists("C:/Program Files/LUMENS//teciuf.fca")){
#  fca<-shQuote("C:/Program Files (x86)/LUMENS//teciuf.fca")
#} else{
#  stop("Fragstats model file is not found, please make sure the file is located in your LUMENS folder in Program files")
#}


#connect to fragstats' .fca file
if (file.exists("C:/Program Files (x86)/LUMENS//teciuf.fca")){
fca<-paste('C:/Program Files (x86)/LUMENS//teciuf.fca')
} else if (file.exists("C:/Program Files/LUMENS//teciuf.fca")){
fca<-paste('C:/Program Files (x86)/LUMENS//teciuf.fca')
} else{
stop("Fragstats model file is not found, please make sure the file is located in your LUMENS folder in Program files")
}

SQLite(max.con = 200, fetch.default.rec = 500, force.reload = FALSE, shared.cache=FALSE)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=as.character(fca))

#delete all record from frg_landscape layer
del<-paste("DELETE FROM frg_landscape_layers")
ll <- dbSendQuery(con, del)

input_desc<-paste("UPDATE frg_table_strings SET value='",classdesc,"' WHERE rec_id=5;",sep="")
input_edge<-paste("UPDATE frg_table_strings SET value='",edgecon,"' WHERE rec_id=2;",sep="")
input_out<-paste("UPDATE frg_table_strings SET value='",outpath,"' WHERE rec_id=6;",sep="")
input_window_size_sqr<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=18;"); #change square window radius
input_window_size_circ<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=19;"); #change circle window radius
input_window_type<-paste("UPDATE frg_table_numerics SET value=",window.shape,"WHERE rec_id=13;")
ll <- dbSendQuery(con, input_desc)
ll <- dbSendQuery(con, input_edge)
ll <- dbSendQuery(con, input_out)
ll <- dbSendQuery(con, input_window_size_sqr)
ll <- dbSendQuery(con, input_window_size_circ)
ll <- dbSendQuery(con, input_window_type)

landlayer1<-paste("INSERT INTO frg_landscape_layers(model_id, name_internal, name_external, io_info, driver_lib, driver_name, driver_id, xll, yll, xur, yur, cell_size, row_count, col_count, allow_class_zero, no_data_value, background_value) VALUES ('",modid,"','",internal,"','",lu1_path,"','",io,"','",drlib,"','",drname,"','",drid,"','",xl1,"','",yl1,"','",xu1,"','",yu1,"','",csize1,"','",rowc1,"','",colc1,"','",aczero,"','",nodata,"','",bvalue,"');",sep="")

ll <- dbSendQuery(con, landlayer1)

if (file.exists("C:/Program Files (x86)/Fragstats 4")){
setwd("C:/Program Files (x86)/Fragstats 4/")
} else{
setwd("C:/Program Files/Fragstats 4/")
}

#execute fragstats for lu1
sysout<-paste(Wdir, "/fragout", sep="")
f <- paste('frg -m',shQuote(fca),' -o',sysout)
system(f)

#delete all record from frg_landscape layer
del<-paste("DELETE FROM frg_landscape_layers")
ll <- dbSendQuery(con, del)


#extent input of file 2
xl2<-xmin(lu2)
yl2<-ymin(lu2)
xu2<-xmax(lu2)
yu2<-ymax(lu2)

#cell size input of file 2
csize2<-xres(lu2)
#row and column size input of file 2
rowc2<-nrow(lu2)
colc2<-ncol(lu2)

#check raster file directory for lu2
dirname_raster<-dirname(lu2_path)
setwd(dirname_raster)

for (i in 1:3){
mwout<-paste(lu2_path,'_mw',i, sep='')
teci.dir<-paste(mwout,"/",list.files(mwout), sep='')
if (file.exists(teci.dir)==TRUE){
mwout2<-paste(lu2_path,'_mw',i, sep='')
unlink(mwout2, recursive=TRUE)
print(paste(i,"deleting previous raster file found, algorithm continue..."))
}else{
print(paste(i,"no previous raster file found, algorithm continue..."))
}
}

landlayer2<-paste("INSERT INTO frg_landscape_layers(model_id, name_internal, name_external, io_info, driver_lib, driver_name, driver_id, xll, yll, xur, yur, cell_size, row_count, col_count, allow_class_zero, no_data_value, background_value) VALUES ('",modid,"','",internal,"','",lu2_path,"','",io,"','",drlib,"','",drname,"','",drid,"','",xl2,"','",yl2,"','",xu2,"','",yu2,"','",csize2,"','",rowc2,"','",colc2,"','",aczero,"','",nodata,"','",bvalue,"');",sep="")
ll <- dbSendQuery(con, landlayer2)

if (file.exists("C:/Program Files (x86)/Fragstats 4")){
setwd("C:/Program Files (x86)/Fragstats 4/")
} else{
setwd("C:/Program Files/Fragstats 4/")
}

#execute fragstats for lu2
sysout<-paste(Wdir, "/fragout", sep="")
f <- paste('frg -m',shQuote(fca),' -o',sysout)
system(f)


#delete all record from frg_landscape layer
del<-paste("DELETE FROM frg_landscape_layers")
ll <- dbSendQuery(con, del)
dbGetStatement(ll)
dbHasCompleted(ll)

#End of Fragstats TECI moving window analysis
setwd(Wdir)
mwout1<-paste(lu1_path,'_mw1', sep='')
teci.dir.init<-paste(mwout1,"/",list.files(mwout1), sep='')
mwout2<-paste(lu2_path,'_mw1', sep='')
teci.dir.final<-paste(mwout2,"/",list.files(mwout2), sep='')


#INITIAL TECI MW Handling
tryCatch({
mwfile.init<-raster(teci.dir.init)
},error=function(e){cat("No moving window output file found, re-check your inputs :",conditionMessage(e), "\n")})

NAvalue(mwfile.init)<-(999*-1)
#extract value from MW TECI with points
tecival.init<-extract(mwfile.init, centro, method='simple', na.rm=T, df=T)

poly.data<-as.data.frame(polygrid,xy=TRUE) #NEW
colnames(poly.data)<-c("ID.centro","x","y","ID.grid") #ID = id grid

#combine dataframe of teci and habitat
colnames(tecival.init)<-c("ID.centro","teci")
colnames(tothab.init)<-c("ID.grid","sum")
ctab<-merge(tothab.init,poly.data,by="ID.grid")
ctab<-merge(ctab,tecival.init,by="ID.centro")
sort.ctab.init <- ctab[order(ctab$teci, decreasing=F, na.last=TRUE), ]
sort.ctab.init <- sort.ctab.init[!(sort.ctab.init$sum==0),]
habcum.init= cumsum(sort.ctab.init$sum)
sumtab1.init<-cbind(sort.ctab.init, Cum.Sum=habcum.init)
cumax<-max(sumtab1.init$Cum.Sum, na.rm=TRUE)
sumtab1.init[nrow(sumtab1.init)+1, ] <- c(sumtab1.init$ID.grid[nrow(sumtab1.init)],100,sumtab1.init$ID.centro[nrow(sumtab1.init)],sumtab1.init$x[nrow(sumtab1.init)],sumtab1.init$y[nrow(sumtab1.init)],100,cumax)
difa.init<-ggplot(sumtab1.init, aes(x =sumtab1.init$teci, y =sumtab1.init$Cum.Sum, xend=100, yend=100)) +
geom_area(position='') + ggtitle(year1) +
labs(x = "Sorted TECI value (%)", y='Cumulative Proportion of Focal Areas (%)')

#Calculate area under the curve
AUC.init = round((trapz(na.omit(sumtab1.init$teci),sumtab1.init$Cum.Sum))/100,digits=2)

#EXPORT DATA
sumtab2.init<-round(sumtab1.init,digits=2)
colnames(sumtab2.init)<-c("ID.centroid","ID.grid","X.cor","Y.cor","Habitat Area (Ha)","TECI(%)", "Cumulative Habitat(%)")
write.table(sumtab2.init, "QUES-B Summary calculation-initial.csv", row.names = FALSE, sep=",")


file.teci.init<-paste('TECI_',location,'_',year1,'_NA',sep='')
file.habitat.name.init<-paste('focal_area_',location,'_',year1, sep='')
writeRaster(mwfile.init, filename=file.teci.init, format="GTiff", overwrite=TRUE)
writeRaster(foc.area.init, filename=file.habitat.name.init, format="GTiff", overwrite=TRUE)


#FINAL TECI MW Handling
tryCatch({
mwfile.final<-raster(teci.dir.final)
},error=function(e){cat("No moving window output file found, re-check your inputs :",conditionMessage(e), "\n")})
NAvalue(mwfile.final)<-(999*-1)
#extract value from MW TECI with points
tecival.final<-extract(mwfile.final, centro, method='simple', na.rm=T, df=T)

poly.data<-as.data.frame(polygrid,xy=TRUE) #NEW
colnames(poly.data)<-c("ID.centro","x","y","ID.grid") #ID = id grid

#combine dataframe of teci and habitat
colnames(tecival.final)<-c("ID.centro","teci")
colnames(tothab.final)<-c("ID.grid","sum")
ctab<-merge(tothab.final,poly.data,by="ID.grid")
ctab<-merge(ctab,tecival.final,by="ID.centro")
sort.ctab.final <- ctab[order(ctab$teci, decreasing=F, na.last=TRUE), ]
sort.ctab.final <- sort.ctab.final[!(sort.ctab.final$sum==0),]
habcum.final= cumsum(sort.ctab.final$sum)
sumtab1.final<-cbind(sort.ctab.final, Cum.Sum=habcum.final)
cumax<-max(sumtab1.final$Cum.Sum, na.rm=TRUE)
sumtab1.final[nrow(sumtab1.final)+1, ] <- c(sumtab1.final$ID.grid[nrow(sumtab1.final)],100,sumtab1.final$ID.centro[nrow(sumtab1.final)],sumtab1.final$x[nrow(sumtab1.final)],sumtab1.final$y[nrow(sumtab1.final)],100,cumax)
difa.final<-ggplot(sumtab1.final, aes(x =sumtab1.final$teci, y =sumtab1.final$Cum.Sum, xend=100, yend=100)) +
geom_area(position='') + ggtitle(year2) +
labs(x = "Sorted TECI value (%)", y='Focal area proportion (%)')

#Calculate area under the curve
AUC.final = round((trapz(na.omit(sumtab1.final$teci),sumtab1.final$Cum.Sum))/100,digits=2)

#EXPORT DATA
sumtab2.final<-round(sumtab1.final,digits=2)
colnames(sumtab2.final)<-c("ID.centroid","ID.grid","X.cor","Y.cor","Habitat Area (Ha)","TECI(%)", "Cumulative Habitat(%)")
write.table(sumtab2.final, "QUES-B Summary calculation-final.csv", row.names = FALSE, sep=",")


file.teci.final<-paste('TECI_',location,'_',year2,'_NA',sep='')
file.habitat.name.final<-paste('focal_area_',location,'_',year2, sep='')
writeRaster(mwfile.final, filename=file.teci.final, format="GTiff", overwrite=TRUE)
writeRaster(foc.area.final, filename=file.habitat.name.final, format="GTiff", overwrite=TRUE)

#Zonal statistics on QUES-B

#generate zonal statistics
zstat.init<-ZonalStat(mwfile.init, zone, FUN = "all")
zstat.init[3]<-NULL
zstat.init[3]<-NULL
zstat.init[3]<-NULL
#rcl.mean.init<-cbind(zstat.init$zone,zstat.init$mean)
#teci_zstat_mean.init<-reclassify(zone, rcl.mean.init);# PU teci value mean


zstat.final<-ZonalStat(mwfile.final, zone, FUN = "all")
zstat.final[3]<-NULL
zstat.final[3]<-NULL
zstat.init[3]<-NULL
#rcl.mean.final<-cbind(zstat.final$zone,zstat.final$mean)
#teci_zstat_mean.final<-reclassify(zone, rcl.mean.final);# PU teci value mean

#SDM Tools fragstats; mean patch area calculation; patch number calculation
foc.area.stats.init<- ClassStat(foc.area.init,bkgd=0, cellsize=(res(foc.area.init)[1]/100))
foc.area.stats.init<-t(as.data.frame(foc.area.stats.init))
foc.area.stats.init<-round(foc.area.stats.init[,1], digits=2)

foc.area.stats.final<- ClassStat(foc.area.final,bkgd=0, cellsize=(res(foc.area.final)[1]/100))
foc.area.stats.final<-t(as.data.frame(foc.area.stats.final))
foc.area.stats.final<-round(foc.area.stats.final[,1], digits=2)

#Combine class STATS
foc.area.stats<-cbind(foc.area.stats.init,foc.area.stats.final)
foc.area.stats.temp1<-foc.area.stats[2:4,c('foc.area.stats.init','foc.area.stats.final')]
total.edge<-(foc.area.stats[6:6,c('foc.area.stats.init','foc.area.stats.final')]*100)
foc.area.stats.temp3<-(foc.area.stats[10:13,c('foc.area.stats.init','foc.area.stats.final')])
foc.area.stats<-rbind(foc.area.stats.temp1,total.edge,foc.area.stats.temp3)
rm(foc.area.stats.temp1,total.edge,foc.area.stats.temp3)

col.init<-paste('class.stats.',year1, sep='')
colnames(foc.area.stats)<-c(paste('class.stats.',year1, sep=''),paste('class.stats.',year2, sep=''))
foc.area.stats.filename<-paste("Focal_area_class_metrics",location,'_',year1,'_',year2,'.csv', sep='')
write.csv(foc.area.stats, foc.area.stats.filename, row.names=TRUE)

#combine teci_zstat with planning unit name

#QUES-B database
dbase.quesb.name<-paste("QuESB_database_", location,'_',year1,'_',year2,'.ldbase', sep='')
save(lu1_path,lu1,year1,lu2_path,lu2,year2,zone,zone_lookup,location,totarea,lookup_bh,polygrid,sumtab1.init,difa.init,AUC.init,foc.area.init,mwfile.init,zstat.init,foc.area.stats.init,sumtab1.final,difa.final,AUC.final,mwfile.final,zstat.final,foc.area.stats.final, file=dbase.quesb.name)
#load(dbase.quesb.name)

#MULTI-TEMPORAL ANALYSIS

#Focal area decrement and increment
chk_loss<-foc.area.init>foc.area.final
chk_gain<-foc.area.init<foc.area.final
foc.area.loss<-(foc.area.init-foc.area.final)*chk_loss;#integration increase
foc.area.gain<-(foc.area.final-foc.area.init)*chk_gain;#integration decrease

if (maxValue(foc.area.gain)==0 & minValue(foc.area.gain)==0){foc.area.gain<-paste("NO FOCAL AREA RECOVERED")} else {foc.area.gain}


#Habitat loss (TECI increment) and Habitat recovery (decrement) except nodata
mwfile.init.chk<-mwfile.init
mwfile.final.chk<-mwfile.final

chk_teci_decrement<-mwfile.init.chk>mwfile.final.chk
chk_teci_decrement <- reclassify(chk_teci_decrement, cbind(0,NA))
chk_teci_increment<-mwfile.init.chk<mwfile.final.chk
chk_teci_increment <- reclassify(chk_teci_increment, cbind(0,NA))
habitat.recovery<-(mwfile.init.chk-mwfile.final.chk)*chk_teci_decrement;#TECI value decrement
habitat.degradation<-(mwfile.final.chk-mwfile.init.chk)*chk_teci_increment;#TECI value increment


#TECI loss and gain in NA data
mwfile.init.NA <- reclassify(mwfile.init.chk, cbind(NA, 999))
mwfile.init.NA<-((mwfile.init.NA/999)==1)

mwfile.final.NA <- reclassify(mwfile.final.chk, cbind(NA, 999))
mwfile.final.NA<-((mwfile.final.NA/999)==1)


#Habitat gain and recovery
habitat.gain.NA<-mwfile.final.chk*mwfile.init.NA;#TECI gain in NA area
habitat.gain.NA<- reclassify(habitat.gain.NA, cbind(0, NA))
habitat.gain.NA<-habitat.gain.NA>0
#habitat.gain.recovery<-mosaic(habitat.recovery, habitat.gain.NA, fun="max")

#Habitat loss and degradation
habitat.loss.NA<-mwfile.init.chk*mwfile.final.NA;#TECI loss in NA area
habitat.loss.NA<- reclassify(habitat.loss.NA, cbind(0, NA))
habitat.loss.NA<- habitat.loss.NA>0
#habitat.loss.degradation<-mosaic(habitat.degradation, habitat.loss.NA, fun="max")

#important variables above:
#habitat.recovery
#habitat.degradation
#habitat.gain.NA
#habitat.loss.NA


#focal area loss evaluation: generate focal area loss map contained with final land-cover types
if (maxValue(chk_loss)>0){
foc.area.loss<-chk_loss*lu2
foc.area.loss <- reclassify(foc.area.loss, cbind(0, NA))
foc.area.loss.att<-na.omit(as.data.frame(freq(foc.area.loss)))
foc.area.loss.att$prop<-(foc.area.loss.att$count/sum(foc.area.loss.att$count))*100

colnames(foc.area.loss.att)[1]<-c("ID")
foc.area.loss.att<-merge(lookup_bh, foc.area.loss.att, by="ID")
foc.area.loss.att$BIODIV<-NULL
colnames(foc.area.loss.att)<-c("ID", "LULC", "Area", "Proportion_of_loss")
foc.area.loss.att$Area<-foc.area.loss.att$Area*(res(foc.area.init)[1]*res(foc.area.init)[2]/10000)
foc.area.loss.att<-arrange(foc.area.loss.att, -Proportion_of_loss)
foc.area.loss.att$Proportion_of_loss<-round(foc.area.loss.att$Proportion_of_loss, digits=2)
foc.area.loss.att.filename<-paste("Focal_area_loss_source",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(foc.area.loss.att, foc.area.loss.att.filename)
} else { print("No focal area loss found")}

if (maxValue(chk_gain)>0) {
foc.area.gain<-chk_gain*lu2
foc.area.gain <- reclassify(foc.area.gain, cbind(0, NA))
foc.area.gain.att<-na.omit(as.data.frame(freq(foc.area.gain)))
foc.area.gain.att$prop<-(foc.area.gain.att$count/sum(foc.area.gain.att$count))*100

colnames(foc.area.gain.att)[1]<-c("ID")
foc.area.gain.att<-merge(lookup_bh, foc.area.gain.att, by="ID")
foc.area.gain.att$BIODIV<-NULL
colnames(foc.area.gain.att)<-c("ID", "LULC", "Area", "Proportion_of_gain")
foc.area.gain.att$Area<-foc.area.gain.att$Area*(res(foc.area.init)[1]*res(foc.area.init)[2]/10000)
foc.area.gain.att<-arrange(foc.area.gain.att, -Proportion_of_gain)
foc.area.gain.att$Proportion_of_gain<-round(foc.area.gain.att$Proportion_of_gain, digits=2)
foc.area.gain.att.filename<-paste("Focal_area_gain_source",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(foc.area.gain.att, foc.area.gain.att.filename)
} else { print("No focal area gain found")}


#zonal stat for focal area gain/loss
lookup_z.area<-as.data.frame(na.omit(freq(zone)))
colnames(lookup_z.area)<-c('ZONE','zone.area')
#lookup_z.area$zone.area<-lookup_z.area$zone.area*Spat_res
#foc.area.change.map<-reclassify((foc.area.final-foc.area.init),cbind(0,NA))
zstat.foc.area.basic<-as.data.frame(zonal((foc.area.final-foc.area.init), zone, fun='sum'))
colnames(zstat.foc.area.basic) =c("ZONE","foc.area.change")
zstat.foc.area<-zstat.foc.area.basic

zstat.foc.area$foc.area.change<-zstat.foc.area$foc.area.change*Spat_res
zstat.foc.area<-merge(lookup_z,zstat.foc.area,by='ZONE')
zstat.foc.area<-merge(zstat.foc.area,lookup_z.area,by='ZONE')
zstat.foc.area$change.proportion<-round((zstat.foc.area$foc.area.change/zstat.foc.area$zone.area)*100, digits=2)
zstat.foc.area<-arrange(zstat.foc.area, foc.area.change)


if (as.character(habitat.loss.NA@crs)==as.character(zone@crs)){
print("Final land use/cover map has the same projection")
if (res(habitat.loss.NA)[1]==res(zone)[1]){
print("zone has the same extent with the habitat map")
} else{
print("zone doesn't have the same extent with the habitat map, synchronising zone map...")
zone<-spatial_sync_raster(zone, habitat.loss.NA, method = "ngb")
}
} else{
print("zone doesn't have the same projection with the habitat map, synchronising zone map...")
zone<-spatial_sync_raster(zone, habitat.loss.NA, method = "ngb")
}


#zonal stat for habitat recovery and degradation
habitat.recovery.0<-reclassify(habitat.recovery, cbind(NA, 0))
zstat.habitat.recovery<-ZonalStat(habitat.recovery.0, zone, FUN = "all")

colnames(zstat.habitat.recovery)[1] ="ZONE"
zstat.habitat.recovery<-merge(lookup_z, zstat.habitat.recovery, by="ZONE")
zstat.habitat.recovery[4]<-NULL
zstat.habitat.recovery[4]<-NULL
zstat.habitat.recovery[4]<-NULL
zstat.habitat.recovery[7]<-NULL
zstat.habitat.recovery$max<-round(zstat.habitat.recovery$max, digits=2)
zstat.habitat.recovery$min<-round(zstat.habitat.recovery$min, digits=2)
zstat.habitat.recovery$mean<-round(zstat.habitat.recovery$mean, digits=2)
zstat.habitat.recovery$sd<-round(zstat.habitat.recovery$sd, digits=2)
zstat.habitat.recovery<-merge(zstat.habitat.recovery, zstat.foc.area.basic, by="ZONE")
zstat.habitat.recovery$norm.mean<-zstat.habitat.recovery$mean/abs(zstat.habitat.recovery$foc.area)
zstat.habitat.recovery$norm.mean<-round(zstat.habitat.recovery$norm.mean, digits=3)
zstat.habitat.recovery<-arrange(zstat.habitat.recovery, -norm.mean)

habitat.degradation.0<-reclassify(habitat.degradation, cbind(NA, 0))
zstat.habitat.degradation<-ZonalStat(habitat.degradation.0, zone, FUN = "all")
colnames(zstat.habitat.degradation)[1] ="ZONE"
zstat.habitat.degradation<-merge(lookup_z, zstat.habitat.degradation, by="ZONE")
zstat.habitat.degradation[4]<-NULL
zstat.habitat.degradation[4]<-NULL
zstat.habitat.degradation[4]<-NULL
zstat.habitat.degradation[7]<-NULL
zstat.habitat.degradation$max<-round(zstat.habitat.degradation$max, digits=2)
zstat.habitat.degradation$min<-round(zstat.habitat.degradation$min, digits=2)
zstat.habitat.degradation$mean<-round(zstat.habitat.degradation$mean, digits=2)
zstat.habitat.degradation$sd<-round(zstat.habitat.degradation$sd, digits=2)
zstat.habitat.degradation<-merge(zstat.habitat.degradation, zstat.foc.area.basic, by="ZONE")
zstat.habitat.degradation$norm.mean<-zstat.habitat.degradation$mean/abs(zstat.habitat.degradation$foc.area)
zstat.habitat.degradation$norm.mean<-round(zstat.habitat.degradation$norm.mean, digits=3)
zstat.habitat.degradation<-arrange(zstat.habitat.degradation, -norm.mean)

#write zonal stats table
tryCatch({
zstat.gain.recover.filename<-paste("Habitat_recovery_zonal_stat_",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(zstat.habitat.recovery, zstat.gain.recover.filename)
},error=function(e){cat("Skipping zonal stats table export process:",conditionMessage(e), "\n")})

tryCatch({
zstat.loss.degradation.filename<-paste("Habitat_degradation_zonal_stat_",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(zstat.habitat.degradation, zstat.loss.degradation.filename)
},error=function(e){cat("Skipping zonal stats table export process:",conditionMessage(e), "\n")})



#PREQUES data handling
if (grepl(".ldbase", as.character(ldabase.preques))){
print(paste("loading Pre-QuES database from", ldabase.preques,sep=' '))
#QuES-B and Pre-QuES integration
#load final database
load(ldabase.preques)
#rename initial variables
prqs.proj.prop<-proj_prop
prqgs.lucdb<-data_merge_sel
prqs.ov.chg<-Ov_chg
prqs.lutm<-cross_temp.melt.dbf
prqs.luchg<-luchg
prqs.luchg.att<-luchg_att
prqs.year.init<-period1
prqs.year.final<-period2
remove(proj_prop,data_merge_sel, Ov_chg, cross_temp.melt.dbf, luchg, luchg_att, period1, period2)

#identify contributing land use change to focal area degradation and loss
habitat.degradation.bol<-habitat.degradation/habitat.degradation; #create boolean degradation map

if (as.character(prqs.luchg@crs)==as.character(habitat.degradation.bol@crs)){
print("Final land use/cover map has the same projection")
if (res(prqs.luchg)[1]==res(habitat.degradation.bol)[1]){
print("change map has the same extent with the habitat degradation map")
} else{
print("change map doesn't have the same extent with the habitat degradation map, synchronising habitat degradation map...")
habitat.degradation.bol<-spatial_sync_raster(habitat.degradation.bol, prqs.luchg, method = "ngb")
}
} else{
print("change map doesn't have the same projection with the habitat degradation map, synchronising habitat degradation map...")
habitat.degradation.bol<-spatial_sync_raster(habitat.degradation.bol, prqs.luchg, method = "ngb")
}

luchg.degradation<-prqs.luchg*habitat.degradation.bol; #focal area degradation LUC
luchg.degradation.att<-na.omit(as.data.frame(freq(luchg.degradation)))
colnames(luchg.degradation.att)<-c("ID","CHANGE")
luchg.degradation.att<-merge(luchg.degradation.att,prqs.luchg.att,by="ID")
luchg.degradation.att<-as.data.frame(cbind(luchg.degradation.att[1],luchg.degradation.att[2],luchg.degradation.att[4],luchg.degradation.att[5],luchg.degradation.att[12],luchg.degradation.att[13], luchg.degradation.att[14]))
luchg.degradation.att<-luchg.degradation.att[ order(-luchg.degradation.att[,2]), ]
tryCatch({
luchg.degradation.db.filename<-paste("LUCHG_degradation_database",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(luchg.degradation.att, luchg.degradation.db.filename)
},error=function(e){cat("Skipping database export process :",conditionMessage(e), "\n")})

#top 10 habitat degradation
luchg.degradation.10<-luchg.degradation.att[1:10,]

#Habitat degradation of focal area due to neighboring changes
tryCatch({
foc.area.row<-lookup_bh[ which(as.character(lookup_bh$BIODIV)==as.character(1)),]
foc.area.luchg.id<-paste(as.character(foc.area.row[1]),'0',as.character(foc.area.row[1]), sep='')
luchg.degradation.focal.area<-luchg.degradation.10[ which(as.character(luchg.degradation.10$ID)==foc.area.luchg.id),]
luchg.degradation.focal.area$LU_CHG<-paste("Stable", luchg.degradation.focal.area$LC_t1, sep=' ')
luchg.degradation.focal.area<-as.data.frame(cbind(luchg.degradation.focal.area[1],luchg.degradation.focal.area[2],luchg.degradation.focal.area[7]))
},error=function(e){cat("Habitat degradation of focal area due to neighboring changes :",conditionMessage(e), "\n")})

#Habitat degradation due to LULCC in situ
tryCatch({
luchg.degradation.10.with.change<-luchg.degradation.10[ which(as.character(luchg.degradation.10$ID_LC2)!=as.character(luchg.degradation.10$ID_LC1)),];#habitat degradation with landuse/landcover change
luchg.degradation.10.with.change<-as.data.frame(cbind(luchg.degradation.10.with.change[1],luchg.degradation.10.with.change[2],luchg.degradation.10.with.change[7]))
colnames(luchg.degradation.10.with.change)<-c("ID", "AREA", "LULC")
},error=function(e){cat("No Habitat degradation due to LULCC in situ :",conditionMessage(e), "\n")})


#Habitat degradation due to neighboring focal area changes
tryCatch({
luchg.degradation.10.no.change<-luchg.degradation.10[ which(as.character(luchg.degradation.10$ID_LC2)==as.character(luchg.degradation.10$ID_LC1)),];#degradation in persistent landuse/landcover
luchg.degradation.10.no.change<-as.data.frame(cbind(luchg.degradation.10.no.change[1],luchg.degradation.10.no.change[2],luchg.degradation.10.no.change[5]))
colnames(luchg.degradation.10.no.change)<-c("ID", "AREA", "LULC")
luchg.degradation.10.no.change$LULC<-paste("Stable", luchg.degradation.10.no.change$LULC, sep=' ')
luchg.degradation.10.no.change<-luchg.degradation.10.no.change[!(luchg.degradation.10.no.change$ID==foc.area.luchg.id),]
},error=function(e){cat("No Habitat degradation due to LULCC in situ :",conditionMessage(e), "\n")})

#important variables in habitat degradation: luchg.degradation.focal.area, luchg.degradation.10.with.change, luchg.degradation.10.no.change

#HABITAT LOSS ANALYSIS
#identify contributing land use change to focal area loss
if (as.character(prqs.luchg@crs)==as.character(habitat.loss.NA@crs)){
print("Final land use/cover map has the same projection")
if (res(prqs.luchg)[1]==res(habitat.loss.NA)[1]){
print("change map has the same extent with the habitat loss map")
} else{
print("change map doesn't have the same extent with the habitat loss map, synchronising habitat loss map...")
prqs.luchg<-spatial_sync_raster(prqs.luchg,habitat.loss.NA, method = "ngb")
}
} else{
print("change map doesn't have the same projection with the habitat loss map, synchronising habitat loss map...")
prqs.luchg<-spatial_sync_raster(prqs.luchg,habitat.loss.NA,  method = "ngb")
}
tryCatch({
luchg.loss<-lu2*habitat.loss.NA; #focal area loss LUC
luchg.loss.att<-na.omit(as.data.frame(freq(luchg.loss)))
colnames(luchg.loss.att)<-c("ID","CHANGE")
luchg.loss.att<-merge(lookup_lc,luchg.loss.att,by="ID")
#luchg.loss.att<-as.data.frame(cbind(luchg.loss.att[1],luchg.loss.att[2],luchg.loss.att[4],luchg.loss.att[5],luchg.loss.att[12],luchg.loss.att[13], luchg.loss.att[14]))
luchg.loss.att<-luchg.loss.att[ order(-luchg.loss.att[,3]), ]
luchg.loss.db.filename<-paste("LUCHG_loss_database",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(luchg.loss.att, luchg.loss.db.filename)
#top 10 subsequent LULC related to habitat loss
luchg.loss.10<-luchg.loss.att[1:10,]
luchg.loss.10$Proportion<-((luchg.loss.10$CHANGE)/(sum(luchg.loss.10$CHANGE)))*Spat_res*100
luchg.loss.10.prop<-as.data.frame(cbind(' ','TOTAL',(sum(luchg.loss.10$CHANGE)), (sum(luchg.loss.10$Proportion))))
luchg.loss.10$Proportion<-round(luchg.loss.10$Proportion,digits=2)
colnames(luchg.loss.10.prop)<-c('ID','CLASS','CHANGE','Proportion')
luchg.loss.10<-rbind(luchg.loss.10,luchg.loss.10.prop)
},error=function(e){cat("Skipping identify contributing land use change to focal area loss :",conditionMessage(e), "\n")})

if (as.character(habitat.loss.NA@crs)==as.character(zone@crs)){
print("Final land use/cover map has the same projection")
if (res(habitat.loss.NA)[1]==res(zone)[1]){
print("zone has the same extent with the habitat map")
} else{
print("zone doesn't have the same extent with the habitat map, synchronising zone map...")
zone<-spatial_sync_raster(zone, habitat.loss.NA, method = "ngb")
}
} else{
print("zone doesn't have the same projection with the habitat map, synchronising zone map...")
zone<-spatial_sync_raster(zone, habitat.loss.NA, method = "ngb")
}


#zonal stat for habitat loss
tryCatch({
habitat.loss.NA.0<-reclassify(habitat.loss.NA, cbind(NA, 0))
zstat.habitat.loss.NA<-ZonalStat(habitat.loss.NA.0, zone, FUN = "sum")
colnames(zstat.habitat.loss.NA)[1] ="ZONE"
zstat.habitat.loss.NA<-merge(lookup_z, zstat.habitat.loss.NA, by="ZONE")
zstat.habitat.loss.NA$Proportion<-((zstat.habitat.loss.NA$sum)/(sum(zstat.habitat.loss.NA$sum)))*Spat_res*100
zstat.habitat.loss.NA.prop<-as.data.frame(cbind(' ','TOTAL',(sum(zstat.habitat.loss.NA$sum)), (sum(zstat.habitat.loss.NA$Proportion))))
zstat.habitat.loss.NA$Proportion<-round(zstat.habitat.loss.NA$Proportion,digits=2)
colnames(zstat.habitat.loss.NA.prop)<-c('ZONE','Z_NAME','sum','Proportion')
zstat.habitat.loss.NA<-zstat.habitat.loss.NA[ order(as.numeric(zstat.habitat.loss.NA$sum), decreasing=TRUE), ]
zstat.habitat.loss.NA<-rbind(zstat.habitat.loss.NA,zstat.habitat.loss.NA.prop)
colnames(zstat.habitat.loss.NA)<-c('ID','ZONE','total.area','Proportion')

},error=function(e){cat("Skipping zonal stat for habitat loss :",conditionMessage(e), "\n")})

#write zonal stats table for habitat loss
tryCatch({
zstat.loss.loss.filename<-paste("Habitat_loss_zonal_stat_",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(zstat.habitat.loss.NA, zstat.loss.loss.filename)
},error=function(e){cat("write zonal stats table for habitat loss :",conditionMessage(e), "\n")})

#important variables in habitat loss: luchg.loss.10, zstat.habitat.loss.NA

#identify contributing land use change to focal area recovery and gain
#HABITAT RECOVERY
habitat.recovery.bol<-habitat.recovery/habitat.recovery; #create boolean recovery map

if (as.character(prqs.luchg@crs)==as.character(habitat.recovery.bol@crs)){
print("Final land use/cover map has the same projection")
if (res(prqs.luchg)[1]==res(habitat.recovery.bol)[1]){
print("change map has the same extent with the habitat recovery map")
} else{
print("change map doesn't have the same extent with the habitat recovery map, synchronising habitat recovery map...")
habitat.recovery.bol<-spatial_sync_raster(habitat.recovery.bol, prqs.luchg, method = "ngb")
}
} else{
print("change map doesn't have the same projection with the habitat recovery map, synchronising habitat recovery map...")
habitat.recovery.bol<-spatial_sync_raster(habitat.recovery.bol, prqs.luchg, method = "ngb")
}

luchg.recovery<-prqs.luchg*habitat.recovery.bol; #focal area recovery LUC
luchg.recovery.att<-na.omit(as.data.frame(freq(luchg.recovery)))
colnames(luchg.recovery.att)<-c("ID","CHANGE")
luchg.recovery.att<-merge(luchg.recovery.att,prqs.luchg.att,by="ID")
luchg.recovery.att<-as.data.frame(cbind(luchg.recovery.att[1],luchg.recovery.att[2],luchg.recovery.att[4],luchg.recovery.att[5],luchg.recovery.att[12],luchg.recovery.att[13], luchg.recovery.att[14]))
luchg.recovery.att<-luchg.recovery.att[ order(-luchg.recovery.att[,2]), ]
tryCatch({
luchg.recovery.db.filename<-paste("LUCHG_recovery_database",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(luchg.recovery.att, luchg.recovery.db.filename)
},error=function(e){cat("Skipping database export process :",conditionMessage(e), "\n")})

#top 10 habitat recovery
luchg.recovery.10<-luchg.recovery.att[1:10,]

#Habitat recovery of focal area due to neighboring changes
tryCatch({
foc.area.row<-lookup_bh[ which(as.character(lookup_bh$BIODIV)==as.character(1)),]
foc.area.luchg.id<-paste(as.character(foc.area.row[1]),'0',as.character(foc.area.row[1]), sep='')
luchg.recovery.focal.area<-luchg.recovery.10[ which(as.character(luchg.recovery.10$ID)==foc.area.luchg.id),]
luchg.recovery.focal.area$LU_CHG<-paste("Stable", luchg.recovery.focal.area$LC_t1, sep=' ')
luchg.recovery.focal.area<-as.data.frame(cbind(luchg.recovery.focal.area[1],luchg.recovery.focal.area[2],luchg.recovery.focal.area[7]))
},error=function(e){cat("Habitat recovery of focal area due to neighboring changes :",conditionMessage(e), "\n")})

#Habitat recovery due to LULCC in situ
tryCatch({
luchg.recovery.10.with.change<-luchg.recovery.10[ which(as.character(luchg.recovery.10$ID_LC2)!=as.character(luchg.recovery.10$ID_LC1)),];#habitat recovery with landuse/landcover change
luchg.recovery.10.with.change<-as.data.frame(cbind(luchg.recovery.10.with.change[1],luchg.recovery.10.with.change[2],luchg.recovery.10.with.change[7]))
colnames(luchg.recovery.10.with.change)<-c("ID", "AREA", "LULC")
},error=function(e){cat("No Habitat recovery due to LULCC in situ :",conditionMessage(e), "\n")})


#Habitat recovery due to neighboring focal area changes
tryCatch({
luchg.recovery.10.no.change<-luchg.recovery.10[ which(as.character(luchg.recovery.10$ID_LC2)==as.character(luchg.recovery.10$ID_LC1)),];#recovery in persistent landuse/landcover
luchg.recovery.10.no.change<-as.data.frame(cbind(luchg.recovery.10.no.change[1],luchg.recovery.10.no.change[2],luchg.recovery.10.no.change[5]))
colnames(luchg.recovery.10.no.change)<-c("ID", "AREA", "LULC")
luchg.recovery.10.no.change$LULC<-paste("Stable", luchg.recovery.10.no.change$LULC, sep=' ')
luchg.recovery.10.no.change<-luchg.recovery.10.no.change[!(luchg.recovery.10.no.change$ID==foc.area.luchg.id),]
},error=function(e){cat("No Habitat recovery due to LULCC in situ :",conditionMessage(e), "\n")})

#important variables in habitat recovery: luchg.recovery.focal.area, luchg.recovery.10.with.change, luchg.recovery.10.no.change

#HABITAT GAIN ANALYSIS
#identify contributing land use change to focal area gain
if (as.character(prqs.luchg@crs)==as.character(habitat.gain.NA@crs)){
print("Final land use/cover map has the same projection")
if (res(prqs.luchg)[1]==res(habitat.gain.NA)[1]){
print("change map has the same extent with the habitat gain map")
} else{
print("change map doesn't have the same extent with the habitat gain map, synchronising habitat gain map...")
prqs.luchg<-spatial_sync_raster(prqs.luchg,habitat.gain.NA, method = "ngb")
}
} else{
print("change map doesn't have the same projection with the habitat gain map, synchronising habitat gain map...")
prqs.luchg<-spatial_sync_raster(prqs.luchg,habitat.gain.NA,  method = "ngb")
}
tryCatch({
luchg.gain<-lu2*habitat.gain.NA; #focal area gain LUC
luchg.gain.att<-na.omit(as.data.frame(freq(luchg.gain)))
colnames(luchg.gain.att)<-c("ID","CHANGE")
luchg.gain.att<-merge(lookup_lc,luchg.gain.att,by="ID")
#luchg.gain.att<-as.data.frame(cbind(luchg.gain.att[1],luchg.gain.att[2],luchg.gain.att[4],luchg.gain.att[5],luchg.gain.att[12],luchg.gain.att[13], luchg.gain.att[14]))
luchg.gain.att<-luchg.gain.att[ order(-luchg.gain.att[,3]), ]
luchg.gain.db.filename<-paste("LUCHG_gain_database",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(luchg.gain.att, luchg.gain.db.filename)
#top 10 subsequent LULC related to habitat gain
luchg.gain.10<-luchg.gain.att[1:10,]
luchg.gain.10$Proportion<-((luchg.gain.10$CHANGE)/(sum(luchg.gain.10$CHANGE)))*Spat_res*100
luchg.gain.10.prop<-as.data.frame(cbind(' ','TOTAL',(sum(luchg.gain.10$CHANGE)), (sum(luchg.gain.10$Proportion))))
luchg.gain.10$Proportion<-round(luchg.gain.10$Proportion,digits=2)
colnames(luchg.gain.10.prop)<-c('ID','CLASS','CHANGE','Proportion')
luchg.gain.10<-rbind(luchg.gain.10,luchg.gain.10.prop)
},error=function(e){cat("Skipping identify contributing land use change to focal area gain :",conditionMessage(e), "\n")})

if (as.character(habitat.gain.NA@crs)==as.character(zone@crs)){
print("Final land use/cover map has the same projection")
if (res(habitat.gain.NA)[1]==res(zone)[1]){
print("zone has the same extent with the habitat map")
} else{
print("zone doesn't have the same extent with the habitat map, synchronising zone map...")
zone<-spatial_sync_raster(zone, habitat.gain.NA, method = "ngb")
}
} else{
print("zone doesn't have the same projection with the habitat map, synchronising zone map...")
zone<-spatial_sync_raster(zone, habitat.gain.NA, method = "ngb")
}


#zonal stat for habitat gain
tryCatch({
habitat.gain.NA.0<-reclassify(habitat.gain.NA, cbind(NA, 0))
zstat.habitat.gain.NA<-ZonalStat(habitat.gain.NA.0, zone, FUN = "sum")
colnames(zstat.habitat.gain.NA)[1] ="ZONE"
zstat.habitat.gain.NA<-merge(lookup_z, zstat.habitat.gain.NA, by="ZONE")
zstat.habitat.gain.NA$Proportion<-((zstat.habitat.gain.NA$sum)/(sum(zstat.habitat.gain.NA$sum)))*Spat_res*100
zstat.habitat.gain.NA.prop<-as.data.frame(cbind(' ','TOTAL',(sum(zstat.habitat.gain.NA$sum)), (sum(zstat.habitat.gain.NA$Proportion))))
zstat.habitat.gain.NA$Proportion<-round(zstat.habitat.gain.NA$Proportion,digits=2)
colnames(zstat.habitat.gain.NA.prop)<-c('ZONE','Z_NAME','sum','Proportion')
zstat.habitat.gain.NA<-zstat.habitat.gain.NA[ order(as.numeric(zstat.habitat.gain.NA$sum), decreasing=TRUE), ]
zstat.habitat.gain.NA<-rbind(zstat.habitat.gain.NA,zstat.habitat.gain.NA.prop)
colnames(zstat.habitat.gain.NA)<-c('ID','ZONE','total.area','Proportion')
},error=function(e){cat("Skipping zonal stat for habitat gain :",conditionMessage(e), "\n")})

#write zonal stats table for habitat gain
tryCatch({
zstat.gain.gain.filename<-paste("Habitat_gain_zonal_stat_",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(zstat.habitat.gain.NA, zstat.gain.gain.filename)
},error=function(e){cat("write zonal stats table for habitat gain :",conditionMessage(e), "\n")})

#important variables in habitat gain: luchg.gain.10, zstat.habitat.gain.NA


} else{
print("No previous Pre-QuES database loaded")
}



#HABITAT CHANGE ANALYSIS
habitat.reclass<- read.table(habitat.reclass.lookup,header=TRUE, sep=",")
habitat.reclass.mat<-as.matrix.data.frame(habitat.reclass[1:nrow(habitat.reclass),1:3], byrow=TRUE)
habitat.rec.init<-reclassify(mwfile.init, habitat.reclass.mat, right=NA)
habitat.rec.final<-reclassify(mwfile.final, habitat.reclass.mat, right=NA)

habitat.rec.init.freq<-as.data.frame(freq(habitat.rec.init))
colnames(habitat.rec.init.freq)<-c('ID','AREA.INITIAL')
habitat.rec.init.freq$AREA.INITIAL<-habitat.rec.init.freq$AREA.INITIAL*Spat_res
habitat.rec.final.freq<-as.data.frame(freq(habitat.rec.final))
colnames(habitat.rec.final.freq)<-c('ID','AREA.FINAL')
habitat.rec.final.freq$AREA.FINAL<-habitat.rec.final.freq$AREA.FINAL*Spat_res

lookup_habitat<-habitat.reclass[1:nrow(habitat.reclass),3:4]
habitat.change<-merge(lookup_habitat, habitat.rec.init.freq, by="ID")
habitat.change<-merge(habitat.change, habitat.rec.final.freq, by="ID")

#backround map
background<-lu1/lu1
#Create Map for report
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- rev(brewer.pal(11, "RdYlGn"))
myColors8 <- "#000000"
myColors9 <- brewer.pal(12, "Set3")

myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)

#Landuse 1 map
area_lc1<-as.data.frame(freq(lu1))
colnames(area_lc1)[1]<-'ID'
area_lc1<-merge(area_lc1, lookup_bh, by='ID')
colnames(area_lc1)[3]<-'CLASS_LC1'
area_lc1[4]<-NULL
myColors.lu <- myColors[1:length(unique(area_lc1$ID))]
ColScale.lu<-scale_fill_manual(name="Land Use Class", breaks=area_lc1$ID, labels=area_lc1$CLASS_LC1, values=myColors.lu)
plot.LU1<-gplot(lu1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
coord_equal() + ColScale.lu +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))

#Landuse 2 map
area_lc2<-as.data.frame(freq(lu2))
colnames(area_lc2)[1]<-'ID'
area_lc2<-merge(area_lc2, lookup_bh, by='ID')
colnames(area_lc2)[3]<-'CLASS_LC2'
area_lc2[4]<-NULL
ColScale.lu<-scale_fill_manual(name="Land Use Class", breaks=area_lc2$ID, labels=area_lc2$CLASS_LC2, values=myColors.lu)
plot.LU2<-gplot(lu2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
coord_equal() + ColScale.lu +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))

myColors  <-c(myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors6)

#zone map
area_zone<-lookup_z
colnames(area_zone)[1]<-'ID'
colnames(area_zone)[2]<-'ZONE'
myColors.Z <- myColors[1:length(unique(area_zone$ID))]
ColScale.Z<-scale_fill_manual(name="Zone Class", breaks=area_zone$ID, labels=area_zone$ZONE, values=myColors.Z)
plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
coord_equal() + ColScale.Z +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))

#Focal Area Change
lookup_loss<-as.data.frame(cbind(0,NA))
lookup_loss<-rbind(lookup_loss, cbind(1,2))
foc.area.loss.reclass<- reclassify(chk_loss, lookup_loss)
foc.area.change<-mosaic(foc.area.init, foc.area.loss.reclass, fun="max")
ID<-as.data.frame(levels(ratify(foc.area.change)));# or as.data.frame(na.omit(freq(foc.area.change)))
Label<-c("Non Focal Area", "Stable Focal Area", "Focal Area Loss")
FAC<-as.data.frame(cbind(ID, Label))
myColors.FAC <- c("#FFCC66", "#003300","#FF0000")
ColScale.FAC<-scale_fill_manual(name="Area Class", breaks=FAC$ID, labels=FAC$Label, values=myColors.FAC)
plot.FAC<-gplot(foc.area.change, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
coord_equal() + ColScale.FAC +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))

#====Habitat extent Map t1====
plot.mw.init<-gplot(mwfile.init, maxpixels=100000) + geom_raster(aes(fill=value)) +
coord_equal() + scale_fill_gradient(low = "#FFCC66", high="#003300", guide="colourbar") +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 8),
legend.key.height = unit(0.375, "cm"),
legend.key.width = unit(0.375, "cm"))

#====Habitat extent Map t2====
plot.mw.fin<-gplot(mwfile.final, maxpixels=100000) + geom_raster(aes(fill=value)) +
coord_equal() + scale_fill_gradient(low = "#FFCC66", high="#003300", guide="colourbar") +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 8),
legend.key.height = unit(0.375, "cm"),
legend.key.width = unit(0.375, "cm"))

#====Habitat loss and degradation====
#plot.background<-gplot(background, maxpixels=100000) + geom_raster(aes(fill=as.factor(value)))
tryCatch({
maxval<-ceiling(maxValue(habitat.degradation)/10)
maxval<-maxval*10
background[background==1]<-(-maxval)
plot.hbt.loss<-merge(habitat.degradation, background, overlap=TRUE)
plot.HD<-gplot(plot.hbt.loss, maxpixels=100000) + geom_raster(aes(fill=value)) +
coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 8),
legend.key.height = unit(0.375, "cm"),
legend.key.width = unit(0.375, "cm"))
background[background==-maxval]<-1
},error=function(e){cat("skipping habitat degradation plot :",conditionMessage(e), "\n")})

tryCatch({
maxval<-ceiling(maxValue(habitat.loss.NA)/10)
#maxval<-maxval*10
background[background==1]<-(-maxval)
plot.hbt.loss<-merge(habitat.loss.NA, background, overlap=TRUE)
plot.HL<-gplot(plot.hbt.loss, maxpixels=100000) + geom_raster(aes(fill=value)) +
coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 8),
legend.key.height = unit(0.375, "cm"),
legend.key.width = unit(0.375, "cm"))
background[background==-maxval]<-1
},error=function(e){cat("skipping habitat loss plot :",conditionMessage(e), "\n")})

#====Habitat gain and recovery ====
#plot.background<-gplot(background, maxpixels=100000) + geom_raster(aes(fill=as.factor(value)))
tryCatch({
maxval<-ceiling(maxValue(habitat.recovery)/10)
maxval<-maxval*10
background[background==1]<-(-maxval)
plot.hbt.gain<-merge(habitat.recovery, background, overlap=TRUE)
plot.HR<-gplot(plot.hbt.gain, maxpixels=100000) + geom_raster(aes(fill=value)) +
coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 8),
legend.key.height = unit(0.375, "cm"),
legend.key.width = unit(0.375, "cm"))
background[background==-maxval]<-1
},error=function(e){cat("skipping habitat recovery plot :",conditionMessage(e), "\n")})

tryCatch({
#plot.background<-gplot(background, maxpixels=100000) + geom_raster(aes(fill=as.factor(value)))
maxval<-ceiling(maxValue(habitat.gain.NA)/10)
#maxval<-maxval*10
background[background==1]<-(-maxval)
plot.hbt.gain<-merge(habitat.gain.NA, background, overlap=TRUE)
plot.HG<-gplot(plot.hbt.gain, maxpixels=100000) + geom_raster(aes(fill=value)) +
coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 8),
legend.key.height = unit(0.375, "cm"),
legend.key.width = unit(0.375, "cm"))
background[background==-maxval]<-1
},error=function(e){cat("skipping habitat gain plot :",conditionMessage(e), "\n")})


#====Habitat change Map t1====
background[background==1]<-0
plot.hb.init<-merge(habitat.rec.init, background, overlap=TRUE)
ID.hbg.chg<-as.data.frame(levels(ratify(habitat.rec.init)))
Label<-c("Most Suitable", "Suitable", "Least Suitable")
HBC<-as.data.frame(cbind(ID.hbg.chg, Label))
myColors.HBC <- c("#999999","#FFCC66", "#003300","#FF0000")
ColScale.HBC<-scale_fill_manual(name="Area Class", breaks=HBC$ID, labels=HBC$Label, values=myColors.HBC)
plot.hb.chg.init<-gplot(plot.hb.init, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
coord_equal() + ColScale.HBC +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))

#====Habitat change Map t2====
plot.hb.final<-merge(habitat.rec.final, background, overlap=TRUE)
plot.hb.chg.final<-gplot(plot.hb.final, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
coord_equal() + ColScale.HBC +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))
background[background==0]<-1

rm(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6, myColors9)

#====Create RTF Report File====
title<-"\\b\\fs32 LUMENS-QUES Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules: Biodiversity Analysis\\b0\\fs20"
test<-as.character(Sys.Date())
date<-paste("Date : ", test, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", year1)
I_O_period_2_rep<-paste("\\b","\\fs20", year2)
chapter1<-"\\b\\fs24 I. DATA INPUT \\b0\\fs20"
chapter2<-"\\b\\fs24 II. FOCAL AREA CHANGES \\b0\\fs20"
chapter3<-"\\b\\fs24 III. MAP OF DISSIMILARITIES FROM FOCAL AREAS RELATIVE TO ZONE/PLANNING UNIT \\b0\\fs20"
chapter4<-"\\b\\fs24 IV. DEGREE OF INTEGRATION OF FOCAL AREA (DIFA) \\b0\\fs20"
chapter5<-"\\b\\fs24 V. HABITAT CHANGE ANALYSIS \\b0\\fs20"
chapter6<-"\\b\\fs24 VI. HABITAT QUALITY COMPARISON \\b0\\fs20"
chapter7<-"\\b\\fs24 VI. TECI ZONAL STATISTICS \\b0\\fs20"
rtffile <- RTF("LUMENS_QUES-B_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addNewLine(rtffile)

text <- paste("\\b \\fs20 Land Use Map of\\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, sep=" ")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.4, height=4, res=150, plot.LU1 )
#rm(plot.LU1)
text <- paste("\\b \\fs20 Land Use Map of\\b0 \\fs20 ", area_name_rep, I_O_period_2_rep, sep=" ")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.LU2 )
#rm(plot.LU2)
text <- paste("\\b \\fs20 Zone Map of\\b0 \\fs20 ", area_name_rep, sep=" ")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z )
#rm(plot.Z)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)


addParagraph(rtffile, chapter2)
addNewLine(rtffile)
text <- paste("\\b \\fs20 Focal Area Change Map of \\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.FAC )
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Subsequent land uses following focal area loss in \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep,I_O_period_2_rep,  sep=" ")
addParagraph(rtffile, text)
addTable(rtffile, foc.area.loss.att)
addParagraph(rtffile, "\\b \\fs20 *Area in Hectares Unit ; Proportion in Percentage (%) \\b0 \\fs20 ")
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Focal Area Change by Zone/Planning Unit  in \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep,I_O_period_2_rep,  sep=" ")
addParagraph(rtffile, text)
addTable(rtffile, zstat.foc.area)
addParagraph(rtffile, "\\b \\fs20 *Area in Hectares Unit ; Proportion in Percentage (%) \\b0 \\fs20 ")
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Focal area class metrics \\b0 \\fs20 ")
addParagraph(rtffile, text)
addTable(rtffile, foc.area.stats, row.names=TRUE)
addNewLine(rtffile, n=1)

addParagraph(rtffile, chapter3)
addNewLine(rtffile)
text <- paste("\\b \\fs20 Map of dissimilarities from focal areas in \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep,'relative to Protected Areas', sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.mw.init )
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Map of dissimilarities from focal areas in \\b0 \\fs20 ",area_name_rep, I_O_period_2_rep,'relative to Protected Areas',sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.mw.fin )
addNewLine(rtffile, n=1)

addParagraph(rtffile, chapter4)
addNewLine(rtffile)
text <- paste("\\b \\fs20 Degree of Integration of Focal Area (DIFA) \\b0 \\fs20 ")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, grid.arrange(difa.init, difa.final, ncol=2))
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Area Under Curve: \\b0 \\fs20 ")
addParagraph(rtffile, text)
text <- paste(I_O_period_1_rep, " : ", AUC.init, "%", "          ;    " ,I_O_period_2_rep, " : ", AUC.final, "%", sep=" ")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)


addParagraph(rtffile, chapter5)
addNewLine(rtffile)
tryCatch({
text <- paste("\\b \\fs20 1.Habitat degradation \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HD)
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Habitat degradation due to LULCC in situ \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, luchg.degradation.10.with.change)
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Habitat degradation due to neighboring focal area change \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, luchg.degradation.10.no.change)
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Habitat degradation of focal area due to neighboring changes \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, luchg.degradation.focal.area)
addNewLine(rtffile, n=1)
},error=function(e){cat("skipping habitat degradation plot :",conditionMessage(e), "\n")})
addNewLine(rtffile, n=1)

tryCatch({
text <- paste("\\b \\fs20 2.Habitat loss \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HL)
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Top 10 habitat loss due adjacent focal area loss \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, luchg.loss.10)
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Habitat loss statistics by zone/planning unit \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, zstat.habitat.loss.NA)
addNewLine(rtffile, n=1)
},error=function(e){cat("skipping habitat loss plot :",conditionMessage(e), "\n")})
addNewLine(rtffile, n=1)

tryCatch({
text <- paste("\\b \\fs20 3.Habitat recovery \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HR)
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Habitat recovery due to LULCC in situ \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, luchg.recovery.10.with.change)
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Habitat recovery due to neighboring focal area change \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, luchg.recovery.10.no.change)
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Habitat recovery of focal area due to neighboring changes \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, luchg.recovery.focal.area)
addNewLine(rtffile, n=1)
},error=function(e){cat("skipping habitat recovery plot :",conditionMessage(e), "\n")})
addNewLine(rtffile, n=1)

tryCatch({
text <- paste("\\b \\fs20 4.Habitat gain \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
addParagraph(rtffile, text)
if (maxValue(chk_gain)>0) {
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HG)
} else {print('skipping habitat gain plot ')}
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Top 10 habitat gain due adjacent focal area loss \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, luchg.gain.10)
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Habitat gain statistics by zone/planning unit \\b0 \\fs20 ", sep="")
addParagraph(rtffile, text)
addTable(rtffile, zstat.habitat.gain.NA)
addNewLine(rtffile, n=1)
},error=function(e){cat("skipping habitat gain plot :",conditionMessage(e), "\n")})


addParagraph(rtffile, chapter6)
addNewLine(rtffile)
tryCatch({
text <- paste("\\b \\fs20 Habitat Quality Comparison in \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, grid.arrange(plot.hb.chg.init, plot.hb.chg.final, ncol=2) )
addNewLine(rtffile, n=1)
addTable(rtffile, habitat.change)
addNewLine(rtffile, n=1)
},error=function(e){cat("skipping Habitat Quality Comparison analysis:",conditionMessage(e), "\n")})
addNewLine(rtffile, n=1)

addParagraph(rtffile, chapter7)
addNewLine(rtffile)
tryCatch({
addTable(rtffile, zstat.habitat.degradation)
addParagraph(rtffile, "\\b \\fs20 *max, min, mean, and sd are total edge contrast index value representing habitat loss and degradation degree \\b0 \\fs20 ")
addParagraph(rtffile, "\\b \\fs20 *foc.area or total focal area in is Hectare unit \\b0 \\fs20 ")
addNewLine(rtffile, n=1)
addTable(rtffile, zstat.habitat.recovery)
addParagraph(rtffile, "\\b \\fs20 *max, min, mean, and sd are total edge contrast index value representing habitat gain and recovery degree \\b0 \\fs20 ")
addParagraph(rtffile, "\\b \\fs20 *foc.area or total focal area is in Hectare unit \\b0 \\fs20 ")
addNewLine(rtffile, n=1)
},error=function(e){cat("TECI Zonal statistics analysis :",conditionMessage(e), "\n")})
addNewLine(rtffile, n=1)
done(rtffile)

time.elapsed<-proc.time() - time_start
time.elapsed
