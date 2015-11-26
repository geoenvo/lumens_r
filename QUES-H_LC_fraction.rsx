##[QUES]=group
##working_directory=folder
##lc1=raster
##lclook=file
##subcatch=raster
##period1=number 1990
##location=string
##frac.subcatch.lc1=output table
##frac.catch.lc1=output table
##rat.lc1=output raster
##rat.subcatch=output raster
##rat.catch=output raster
##passfilenames

#libraries
library(sp)
library(raster)
library(rgdal)
library(maptools)
library(reshape2)
library(plyr)
library(lattice)
library(hexbin)
library(grid)
library(spatial.tools)


#Set working directory
setwd(working_directory)



#redefine input parameters
lc1<-raster(lc1)
#lc2<-raster(lc2)
#soil<-raster(soil)
subcatch<-raster(subcatch)

#generate catchment file
catch<-subcatch>0

#clip raster layer
#cr.lc1 <- crop(lc1, catch)
#cr.lc2 <- crop(lc2, extent(catch))
#cr.soil <- crop(soil, extent(catch))

#set same extent
m.lc1<-spatial_sync_raster(lc1, catch, method = "ngb")
#m.lc2<-spatial_sync_raster(lc2, catch, method = "ngb")
#m.soil<-spatial_sync_raster(soil, catch, method = "ngb")

#clip raster layer
m.lc1<-m.lc1*catch
#m.lc2<-m.lc2*catch
#m.soil<-m.soil*catch

m.lc1[ m.lc1[] <=0 ] <- NA
#m.lc2[ m.lc2[] <=0 ] <- NA
#m.soil[ m.soil[] <=0 ] <- NA

#lookup tables
#soilook<- read.table(soilook, header=TRUE, sep=",",)
lclook<- read.table(lclook, header=TRUE, sep=",",)


# set raster attribute table (RAT)
nlu1<-paste("rat_lc",'_',location,'_',period1,'.grd',sep='')
#nlu2<-paste("rat_lc",'_',location,'_',period2,'.grd',sep='')
#nsl<-paste("rat_soil",'_',location,'.grd',sep='')
nch<-paste("rat_catchment",'_',location,'.grd',sep='')
ncs<-paste("rat_subcatchment",'_',location,'.grd',sep='')

rat.lc1<-ratify(m.lc1, filename=nlu1,count=TRUE,overwrite=TRUE)
#rat.lc2<-ratify(m.lc2, filename=nlu2,count=TRUE,overwrite=TRUE)
#rat.soil<-ratify(m.soil, filename=nsl,count=TRUE,overwrite=TRUE)
rat.subcatch<-ratify(subcatch, filename=ncs,count=TRUE,overwrite=TRUE)
rat.catch<-ratify(catch, filename=nch,count=TRUE,overwrite=TRUE)


#create land use and soil database
area_lc1<-as.data.frame(levels(rat.lc1))
#area_lc2<-as.data.frame(levels(rat.lc2))
#area_soil<-as.data.frame(levels(rat.soil))
area_subcatch<-as.data.frame(levels(rat.subcatch))
area_catch<-as.data.frame(levels(rat.catch))

#define catchment area
catch.area<-area_catch$COUNT[1]

#area subcatch 0 check
area_subcatch <- area_subcatch[order(area_subcatch$ID, decreasing=F, na.last=TRUE), ]

#if (area_subcatch$ID[1]==0){
#  area_subcatch <- area_subcatch[!(area_subcatch$ID==0),]
#} else{
#  return
#}


#merge landcover data with lookup table
levels(rat.lc1)<-merge((levels(rat.lc1)),lclook,by="ID")
area_lc1<-as.data.frame(levels(rat.lc1))
colnames(area_lc1)[2] = "COUNT_LC"
colnames(area_lc1)[3] = "CLASS_LC"

#levels(rat.lc2)<-merge((levels(rat.lc2)),lclook,by="ID")
#area_lc2<-as.data.frame(levels(rat.lc2))
#colnames(area_lc1)[2] = "COUNT_LC"
#colnames(area_lc1)[3] = "CLASS_LC"

#merge soil data with lookup table
#levels(rat.soil)<-merge((levels(rat.soil)),soilook,by="ID")
#area_soil<-as.data.frame(levels(rat.soil))
#colnames(area_soil)[2] = "COUNT_SOIL"
#colnames(area_soil)[3] = "CLASS_SOIL"

#pivot table
cross.lc1 <- as.data.frame(crosstab((stack(rat.subcatch, rat.lc1))))
colnames(cross.lc1)[2] ="ID_LC"
colnames(cross.lc1)[1] = "SUBCATCH"
colnames(cross.lc1)[3] = "COUNT"

#cross.lc2 <- as.data.frame(crosstab((stack(rat.subcatch,rat.lc2))))
#colnames(cross.lc2)[2] ="ID_LC"
#colnames(cross.lc2)[1] = "SUBCATCH"
#colnames(cross.lc2)[3] = "COUNT"


#cross.soil <- as.data.frame(crosstab((stack(rat.subcatch,rat.soil))))
#colnames(cross.soil)[2] ="ID_SOIL"
#colnames(cross.soil)[1] = "SUBCATCH"
#colnames(cross.soil)[3] = "COUNT"

#rename header soil lookup table
#colnames(soilook)[1]="ID_SOIL"
#colnames(soilook)[2]="SOIL_DESC"

#add lc description to pivot table
colnames(lclook)[1]="ID_LC"
colnames(lclook)[2]="LC_T1"
cross.lc1 <- merge(cross.lc1,lclook,by="ID_LC")
colnames(area_subcatch)[1]="SUBCATCH"
cross.lc1<- merge(cross.lc1, area_subcatch, by="SUBCATCH")
#cross.lc1 <- merge(cross.lc1,soilook,by="ID_SOIL")

#colnames(lclook)[1]="ID_LC"
#colnames(lclook)[2]="LC_T2"
#cross.lc2 <- merge(cross.lc2,lclook,by="ID_LC")
#cross.lc2<- merge(cross.lc2, area_subcatch, by="SUBCATCH")
#cross.lc2 <- merge(cross.lc2,soilook,by="ID_SOIL")

#cross.soil<- merge(cross.soil, soilook, by="ID_SOIL")
#cross.soil<- merge(cross.soil, area_subcatch, by="SUBCATCH")


#Rename COUNT Column to AREA
colnames(cross.lc1) <- c("SUBCATCH","ID_LC","AREA", "CLASS", "SUB_AREA")
#colnames(cross.lc2) <- c("SUBCATCH","ID_LC","AREA", "CLASS", "SUB_AREA")
#colnames(cross.soil) <- c("SUBCATCH","ID_SOIL","AREA", "CLASS", "SUB_AREA")

#sort data by sub-catchment
cross.lc1<-arrange(cross.lc1,SUBCATCH,ID_LC)
#cross.lc2<-arrange(cross.lc2,SUBCATCH,ID_LC)
#cross.soil<-arrange(cross.soil, SUBCATCH, ID_SOIL)

#Calculate fraction area of each data record
cross.lc1["PROP1"]<-(cross.lc1$AREA)/(cross.lc1$SUB_AREA)
#cross.lc2["PROP2"]<-(cross.lc2$AREA)/(cross.lc2$SUB_AREA)
#cross.soil["PROP"]<-(cross.soil$AREA)/(cross.soil$SUB_AREA)

#Count row number of each data
cross.lc1.nrow<-nrow(cross.lc1)
#cross.lc2.nrow<-nrow(cross.lc2)

#Create ID Column
cross.lc1["ID"]<-c(1:cross.lc1.nrow)
#cross.lc2["ID"]<-c(1:cross.lc2.nrow)

#Reorder columns
cross.lc1<- cross.lc1[ ,c(8,3,2,1,5,6,4,7)]
#cross.lc2<- cross.lc2[ ,c(8,3,2,1,5,6,4,7)]

#filter data.lc table
#cross.lc1<-cross.lc1[!(cross.lc1$AREA==0),]
#cross.lc2<-cross.lc2[!(cross.lc2$AREA==0),]


#LANDCOVER & SOIL PROPORTION PER CATCHMENT
#define total area of catchment
#FRACTION OF LC1
prop1<-((area_lc1$COUNT)/catch.area)*100
prop1<-round(prop1, digits=3)
area_lc1["PROP"] <- prop1

#FRACTION OF LC2
#prop2<-((area_lc2$COUNT)/catch.area)*100
#prop2<-round(prop2, digits=3)
#area_lc2["PROP"] <- prop2

#FRACTION OF SOIL
#propsol<-((area_soil$COUNT)/catch.area)*100
#propsol<-round(propsol, digits=3)
#area_soil["PROP"] <- propsol

#OUTPUT TABLES
sublc1<-paste("lc_fraction_of_subcatch",'_',location,'_',period1,'.csv',sep='')
#sublc2<-paste("lc_fraction_of_subcatch",'_',location,'_',period2,'.csv',sep='')
#subsoil<-paste("soil",'_',location,'.csv',sep='')

catlc1<-paste("lc_fraction_of_catch",'_',location,'_',period1,'.csv',sep='')
#catlc2<-paste("lc_fraction_of_catch",'_',location,'_',period2,'.csv',sep='')
#catsol<-paste("soil_fraction_of_catch",'_',location,'.csv',sep='')

write.table(cross.lc1, sublc1, row.names = FALSE, sep=",")
#write.table(cross.lc2, sublc2, row.names = FALSE, sep=",")
#write.table(cross.soil, subsoil, row.names = FALSE, sep=",")

write.table(area_lc1, catlc1, row.names = FALSE, sep=",")
#write.table(area_lc2, catlc2, row.names = FALSE, sep=",")
#write.table(area_soil, catsol, row.names = FALSE, sep=",")

frac.subcatch<-cross.lc1
#frac.subcatch.lc2<-cross.lc2
frac.catch<-area_lc1
#frac.catch.lc2<-area_lc2
#frac.catch.soil<-area_soil

