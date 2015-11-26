##[QUES]=group
##raster_file=raster
##rat=output table
##passfilenames

library(foreign)

r<-raster(raster_file)
raster_dbf<-paste(raster_file, ".vat.dbf", sep="")
if(file.exists(raster_dbf)){
raster_dbf<-read.dbf(raster_dbf)
#edit(raster_dbf)
rat<-raster_dbf
} else {
area<-as.data.frame(freq(r))
#edit(area)
rat<-raster_dbf
}

