#QUES-H Dominant Land Use, Soil, Slope HRU Definition
##Alpha - QUES=group
##landuse_map=raster
##soil_map=raster
##slope_map=raster
##sub_map=raster
##lu_lookup=file
##soil_lookup=file
##slope_lookup=file
##location = string
##period = number 2010

##area_lu=output table
##area_soil=output table
##area_slope=output table
##lu_hru=output table
##soil_hru=output table
##slope_hru=output table
##HRU=output table
##passfilenames

library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(spatial.tools)
library(pander)
library(rtf)
library(foreign)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
work_dir<-paste(log.file[1,1], "/", log.file[1,2],"/QUES/QUES-H/DLUSSL", sep="")
dir.create(work_dir)

#set project properties
setwd(work_dir)

#load datasets (landuse,soil,slope,subcatch)
lu<-raster(landuse_map)
soil<-raster(soil_map)
slope<-raster(slope_map)
subcatch<-raster(sub_map)

# load look up table
lookup_lu<-read.table(lu_lookup, header=TRUE, sep=",",)
lookup_so<-read.table(soil_lookup, header=TRUE, sep=",",)
lookup_sl<-read.table(slope_lookup, header=TRUE, sep=",",)

#set same extent
lu<-spatial_sync_raster(lu, subcatch, method = "ngb")
soil<-spatial_sync_raster(soil, subcatch, method = "ngb")
slope<-spatial_sync_raster(slope, subcatch, method = "ngb")

# set raster attribute table (RAT)
lu<-ratify(lu, filename='landuse.grd',count=TRUE,overwrite=TRUE)
soil<-ratify(soil, filename='soil.grd',count=TRUE,overwrite=TRUE)
slope<-ratify(slope, filename='slope.grd',count=TRUE,overwrite=TRUE)
subcatch<-ratify(subcatch, filename="subcatch.grd", count=TRUE, overwrite=TRUE)

#create landuse, soil, slope class
area_lu<-as.data.frame(levels(lu))
area_soil<-as.data.frame(levels(soil))
area_slope<-as.data.frame(levels(slope))
area_subcatch<-as.data.frame(levels(subcatch))
area<-min(sum(area_lu$COUNT), sum(area_soil$COUNT), sum(area_slope$COUNT), sum(area_subcatch$COUNT))
area_sz<-as.numeric(area)
area_lu<-merge(area_lu,lookup_lu,by="ID")
area_soil<-merge(area_soil,lookup_so,by="ID")
area_slope <- merge(area_slope,lookup_sl,by="ID")
area_lu<-as.data.frame(area_lu)
area_soil<-as.data.frame(area_soil)
area_slope<-as.data.frame(area_slope)
colnames(area_lu)[2] = "LU_COUNT"
colnames(area_lu)[3] = "LU_CLASS"
colnames(area_soil)[2] = "SOIL_COUNT"
colnames(area_soil)[3] = "SOIL_CLASS"
colnames(area_slope)[2] = "SLOPE_COUNT"
colnames(area_slope)[3] = "SLOPE_CLASS"
colnames(area_subcatch)[2] = "SUBCATCH_COUNT"
colnames(lookup_lu)[1]="ID_LU"
colnames(lookup_lu)[2]="LU_CLASS"
colnames(lookup_so)[1]="ID_SOIL"
colnames(lookup_so)[2]="SOIL_CLASS"
colnames(lookup_sl)[1]="ID_SLOPE"
colnames(lookup_sl)[2]="SLOPE_CLASS"

#calculating new area size
area_lu$LU_COUNT<-as.numeric(round(area_lu$LU_COUNT / sum(area_lu$LU_COUNT) * area_sz))
area_soil$SOIL_COUNT<-as.numeric(round(area_soil$SOIL_COUNT / sum(area_soil$SOIL_COUNT) * area_sz))
area_slope$SLOPE_COUNT<-as.numeric(round(area_slope$SLOPE_COUNT / sum(area_slope$SLOPE_COUNT) * area_sz))
area_subcatch$SUBCATCH_COUNT<-as.numeric(round(area_subcatch$SUBCATCH_COUNT / sum(area_subcatch$SUBCATCH_COUNT) * area_sz))

# create land use,soil, slope, subcatchment attributes
LUSSL<-crosstab(stack(lu,soil,slope,subcatch))
LUSSL$chkVar1<-as.numeric(is.na(LUSSL$Var1) )
LUSSL$chkVar2<-as.numeric(is.na(LUSSL$Var2) )
LUSSL$chkVar3<-as.numeric(is.na(LUSSL$Var3) )
LUSSL$chkVar4<-as.numeric(is.na(LUSSL$Var4) )
LUSSL$chkNull<-LUSSL$chkVar1+LUSSL$chkVar2+LUSSL$chkVar3+LUSSL$chkVar4
LUSSL<-LUSSL[ which(LUSSL$chkNull < 1),]
LUSSL$chkVar1<-LUSSL$chkVar2<-LUSSL$chkVar3<-LUSSL$chkVar4<-NULL
LUSSL$Var1r<-as.numeric(levels(LUSSL$Var1))[LUSSL$Var1]
LUSSL$Var2r<-as.numeric(levels(LUSSL$Var2))[LUSSL$Var2]
LUSSL$Var3r<-as.numeric(levels(LUSSL$Var3))[LUSSL$Var3]
LUSSL$Var4r<-as.numeric(levels(LUSSL$Var4))[LUSSL$Var4]
LUSSL$ID<-as.factor(((LUSSL$Var1r*10^4+(LUSSL$Var2r*10^2))+(LUSSL$Var3r*10^6))+(LUSSL$Var4r*10^0))
LUSSL$chkNull<-LUSSL$Var1r<-LUSSL$Var2r<-LUSSL$Var3r<-LUSSL$Var4r<-NULL
colnames(LUSSL)[1] ="ID_LU"
colnames(LUSSL)[2] = "ID_SOIL"
colnames(LUSSL)[3] = "ID_SLOPE"
colnames(LUSSL)[4] = "ID_SUB"
colnames(LUSSL)[5] = "AREA"
LUSSL<- merge(LUSSL,lookup_lu,by="ID_LU")
LUSSL<- merge(LUSSL,lookup_so,by="ID_SOIL")
LUSSL<- as.data.frame(merge(LUSSL,lookup_sl,by="ID_SLOPE"))
LUSSL$UNIQCOMB <- do.call(paste, c(LUSSL[c("ID_LU","SOIL_CLASS","SLOPE_CLASS","ID_SUB")], sep="_"))

#HRU map and HRU table
HRU_temp<-overlay(lu,soil,slope, subcatch, fun=function(x,y,z,w){return(x*10^4+(y*10^2)+z*10^6+w*10^0)})
HRU_temp<-ratify(HRU_temp, filename='HRU_temp.grd',count=TRUE,overwrite=TRUE)
levels(HRU_temp)<-merge((levels(HRU_temp)),LUSSL,by="ID")
HRU_temp_att<-as.data.frame(levels(HRU_temp))
HRU_temp_att$AREA<-NULL
colnames(HRU_temp_att)[2]<-"area_hectares"
HRU_temp_att$area_acres<-round(HRU_temp_att$area_hectares * 2.47105)
HRU_temp_att$percentage<-as.numeric(format(round((HRU_temp_att$area_hectares / area_sz * 100),2), nsmall=2))
HRU_temp_att$sub_percentage<-HRU_temp_att$percentage
for(i in 1:length(area_subcatch$ID)){
  HRU_temp_att$sub_percentage[which(HRU_temp_att$ID_SUB == i)]<-HRU_temp_att$area_hectares[which(HRU_temp_att$ID_SUB == i)] / area_subcatch$SUBCATCH_COUNT[which(area_subcatch$ID == i)] * 100
}
HRU_temp_att$sub_percentage<-format(round(HRU_temp_att$sub_percentage,2),nsmall=2)
HRU_temp_att<-HRU_temp_att[,c("ID","UNIQCOMB", "area_hectares", "area_acres","percentage", "sub_percentage", "ID_SUB", "ID_LU", "LU_CLASS", "ID_SOIL","SOIL_CLASS","ID_SLOPE","SLOPE_CLASS")]

#create database for report
subcatch_report<-as.data.frame(area_subcatch)
colnames(subcatch_report)[1]="ID_SUB"
colnames(subcatch_report)[2]="AREA_ha"
subcatch_report$AREA_ha<-as.numeric(round(subcatch_report$AREA_ha / sum(subcatch_report$AREA_ha) * area_sz))

#land use database
lu_report<-crosstab(stack(subcatch,lu))
colnames(lu_report)[1] ="ID_SUB"
colnames(lu_report)[2] = "ID_LU"
colnames(lu_report)[3] = "COUNT"
lu_report<- as.data.frame(merge(lu_report,lookup_lu,by="ID_LU"))
lu_report$chkVar1<-as.numeric(is.na(lu_report$ID_SUB))
lu_report$chkVar2<-as.numeric(is.na(lu_report$ID_LU))
lu_report$chkNull<-lu_report$chkVar1+lu_report$chkVar2
lu_report<-lu_report[ which(lu_report$chkNull < 1),]
lu_report$chkVar1<-lu_report$chkVar2<-lu_report$chkNull<-NULL
lu_report<-as.data.frame(lu_report[,c("ID_SUB","ID_LU","LU_CLASS","COUNT")])
lu_report$area_hectares<-as.numeric(lu_report$COUNT)
lu_report$area_acres<-round(lu_report$area_hectares*2.47105)
lu_report$percentage<-as.numeric(format(round(lu_report$COUNT*100/area_sz, 2), nsmall = 2))
lu_report$COUNT<-NULL
lu_report_sub<-merge(lu_report,subcatch_report,by="ID_SUB")
lu_report_sub$sub_percentage<-as.numeric(format(round(lu_report_sub$area_hectares*100/lu_report_sub$AREA_ha, 2), nsmall = 2))
lu_report_sub$AREA_ha<-lu_report_sub$Percentage<-NULL
area_lu$area_hectares<-as.numeric(area_lu$LU_COUNT)
area_lu$area_acres<-round(area_lu$area_hectares*2.47105)
area_lu$percentage<-format(round(area_lu$area_hectares*100/area_sz, 2), nsmall = 2)
area_lu$ID<-area_lu$LU_COUNT<-NULL

#soil database
soil_report<-crosstab(stack(subcatch,soil))
colnames(soil_report)[1] ="ID_SUB"
colnames(soil_report)[2] = "ID_SOIL"
colnames(soil_report)[3] = "COUNT"
soil_report<- as.data.frame(merge(soil_report,lookup_so,by="ID_SOIL"))
soil_report$chkVar1<-as.numeric(is.na(soil_report$ID_SUB))
soil_report$chkVar2<-as.numeric(is.na(soil_report$ID_SOIL))
soil_report$chkNull<-soil_report$chkVar1+soil_report$chkVar2
soil_report<-soil_report[ which(soil_report$chkNull < 1),]
soil_report$chkVar1<-soil_report$chkVar2<-soil_report$chkNull<-NULL
soil_report<-as.data.frame(soil_report[,c("ID_SUB","ID_SOIL","SOIL_CLASS","COUNT")])
soil_report$area_hectares<-as.numeric(soil_report$COUNT)
soil_report$area_acres<-round(soil_report$area_hectares*2.47105)
soil_report$percentage<-as.numeric(format(round(soil_report$COUNT*100/area_sz, 2), nsmall = 2))
soil_report$COUNT<-NULL
soil_report_sub<-merge(soil_report,subcatch_report,by="ID_SUB")
soil_report_sub$sub_percentage<-as.numeric(format(round(soil_report_sub$area_hectares*100/soil_report_sub$AREA_ha, 2), nsmall = 2))
soil_report_sub$AREA_ha<-soil_report_sub$Percentage<-NULL
area_soil$area_hectares<-as.numeric(area_soil$SOIL_COUNT)
area_soil$area_acres<-round(area_soil$area_hectares*2.47105)
area_soil$percentage<-format(round(area_soil$area_hectares*100/area_sz, 2), nsmall = 2)
area_soil$ID<-area_soil$SOIL_COUNT<-NULL

#slope database
slope_report<-crosstab(stack(subcatch,slope))
colnames(slope_report)[1] ="ID_SUB"
colnames(slope_report)[2] = "ID_SLOPE"
colnames(slope_report)[3] = "COUNT"
slope_report<- as.data.frame(merge(slope_report,lookup_sl,by="ID_SLOPE"))
slope_report$chkVar1<-as.numeric(is.na(slope_report$ID_SUB))
slope_report$chkVar2<-as.numeric(is.na(slope_report$ID_SLOPE))
slope_report$chkNull<-slope_report$chkVar1+slope_report$chkVar2
slope_report<-slope_report[ which(slope_report$chkNull < 1),]
slope_report$chkVar1<-slope_report$chkVar2<-slope_report$chkNull<-NULL
slope_report<-as.data.frame(slope_report[,c("ID_SUB","ID_SLOPE","SLOPE_CLASS","COUNT")])
slope_report$area_hectares<-as.numeric(slope_report$COUNT)
slope_report$area_acres<-round(slope_report$area_hectares*2.47105)
slope_report$percentage<-as.numeric(format(round(slope_report$COUNT*100/area_sz, 2), nsmall = ))
slope_report$COUNT<-NULL
slope_report_sub<-merge(slope_report,subcatch_report,by="ID_SUB")
slope_report_sub$sub_percentage<-as.numeric(format(round(slope_report_sub$area_hectares*100/slope_report_sub$AREA_ha, 2), nsmall = 2))
slope_report_sub$AREA_ha<-slope_report_sub$Percentage<-NULL
area_slope$area_hectares<-as.numeric(area_slope$SLOPE_COUNT)
area_slope$area_acres<-round(area_slope$area_hectares*2.47105)
area_slope$percentage<-format(round(area_slope$area_hectares*100/area_sz, 2), nsmall = 2)
area_slope$ID<-area_slope$SLOPE_COUNT<-NULL

subcatch_report$AREA_Acres<-round(subcatch_report$AREA_ha * 2.47105)
subcatch_report$Percentage<-format(round(area_subcatch$SUBCATCH_COUNT*100/area_sz, 2), nsmall = 2)

#Create Map for report
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- brewer.pal(11, "Spectral")
myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)

#Create Landuse Map
lu.p<- rasterToPoints(lu);
lu.p <- as.data.frame(lu.p)
colnames(lu.p) <- c("X","Y","ID")
lu.p<-lu.p[which(lu.p$ID != 0),]
lu.lab<-lookup_lu
colnames(lu.lab)[1]<-"ID"
lu.p<-merge(lu.p, lu.lab, by="ID")
lu.p$ID<-as.factor(lu.p$ID)
myColors.lu <- myColors[1:length(unique(lu.p$ID))]
names(myColors.lu) <- unique(lu.p$LU_CLASS)
ColScale.lu<-scale_fill_manual(name="Landuse Class", values = myColors.lu )
p  <- ggplot(data=lu.p) + geom_raster(aes(x=lu.p$X, y=lu.p$Y, fill=lu.p$LU_CLASS)) +
  ColScale.lu +
  ggtitle(paste("Landuse Map of", location, period )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#Create Soil Map
soil.p<- rasterToPoints(soil);
soil.p <- as.data.frame(soil.p)
colnames(soil.p) <- c("X","Y","ID")
soil.p<-soil.p[which(soil.p$ID != 0),]
soil.lab<-lookup_so
colnames(soil.lab)[1]<-"ID"
soil.p<-merge(soil.p, soil.lab, by="ID")
soil.p$ID<-as.factor(soil.p$ID)
myColors.soil <- myColors[1:length(unique(soil.p$ID))]
names(myColors.soil) <- unique(soil.p$SOIL_CLASS)
ColScale.soil<-scale_fill_manual(name="Soil Class", values = myColors.soil )
p1  <- ggplot(data=soil.p) + geom_raster(aes(x=soil.p$X, y=soil.p$Y, fill=soil.p$SOIL_CLASS)) +
  ColScale.soil +
  ggtitle(paste("Soil Map of", location, period )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title=element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#Create Slope Map
slope.p<- rasterToPoints(slope);
slope.p <- as.data.frame(slope.p)
colnames(slope.p) <- c("X","Y","ID")
slope.p<-slope.p[which(slope.p$ID != 0),]
slope.lab<-lookup_sl
colnames(slope.lab)[1]<-"ID"
slope.p<-merge(slope.p, slope.lab, by="ID")
slope.p$ID<-as.factor(slope.p$ID)
myColors.slope <- myColors[1:length(unique(slope.p$ID))]
names(myColors.slope) <- unique(slope.p$SLOPE_CLASS)
ColScale.slope<-scale_fill_manual(name="Slope Class", values = myColors.slope )
p2  <- ggplot(data=slope.p) + geom_raster(aes(x=slope.p$X, y=slope.p$Y, fill=slope.p$SLOPE_CLASS)) +
  ColScale.slope +
  ggtitle(paste("Slope Map of", location, period )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title=element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#Create Subcatch Map
subcatch.p<- rasterToPoints(subcatch);
subcatch.p <- as.data.frame(subcatch.p)
colnames(subcatch.p) <- c("X","Y","ID")
subcatch.p<-subcatch.p[which(subcatch.p$ID != 0),]
subcatch.p$ID<-as.factor(subcatch.p$ID)
myColors.subcatch <- myColors[1:length(unique(subcatch.p$ID))]
names(myColors.subcatch) <- unique(subcatch.p$ID)
ColScale.subcatch<-scale_fill_manual(name="Subcatchment Number", values = myColors.subcatch )
p3  <- ggplot(data=subcatch.p) + geom_raster(aes(x=subcatch.p$X, y=subcatch.p$Y, fill=subcatch.p$ID)) +
  ColScale.subcatch +
  ggtitle(paste("Subcatchment Map of", location, period )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title=element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#create LUSSL report
rtffile <- RTF("LUSSL.rtf")
text <- paste("LUMENS LANDUSE/ SOIL/ SLOPE Distribution Report             ", "Date and Time:", Sys.time(), sep="  ")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
text <- paste("Watershed     ", "      area[ha]:", area_sz, "        area[acres]:", round(area_sz*2.47105),"       Number of Subbasins:", length(area_subcatch$ID),sep=" ")
addParagraph(rtffile, text)
text<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
addParagraph(rtffile, text)
addPlot(rtffile, plot.fun=print,width=6.7, height=4, res=300, p3 )
addNewLine(rtffile,n=1)
addPlot(rtffile, plot.fun=print,width=6.7, height=4, res=300, p )
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addPlot(rtffile, plot.fun=print, width=6.7, height=4, res=300, p1 )
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addPlot(rtffile, plot.fun=print, width=6.7, height=4, res=300, p2 )
addNewLine(rtffile, n=1)
addNewLine(rtffile,n=1)
text<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
addParagraph(rtffile, text)
addNewLine(rtffile,n=1)
addTable(rtffile, area_lu)
addNewLine(rtffile, n=1)
addTable(rtffile, area_soil)
addNewLine(rtffile, n=1)
addTable(rtffile, area_slope)
addNewLine(rtffile, n=1)
text<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
addParagraph(rtffile, text)
addNewLine(rtffile,n=1)
for(i in 1:length(area_subcatch$ID)){
  text<-paste("SUBBASIN #       ", i, "       area[ha]:",sum(lu_report_sub$area_hectares[which((lu_report_sub$ID_SUB == i))]),"      area[acres]:", sum(lu_report_sub$area_acres[which((lu_report_sub$ID_SUB == i))]), "       area[percentage]:",subcatch_report$Percentage[which((subcatch_report$ID_SUB == i))],  sep=" ")
  addParagraph(rtffile, text)
  addNewLine(rtffile,n=1)
  y<-lu_report_sub[which((lu_report_sub$ID_SUB == i)),]
  y$ID_SUB<-y$ID_LU<-NULL
  x<-soil_report_sub[which((soil_report_sub$ID_SUB == i)),]
  x$ID_SUB<-x$ID_SOIL<-NULL
  colnames(x)<-gsub("\\."," ",colnames(x))
  z<-slope_report_sub[which((slope_report_sub$ID_SUB == i)),]
  z$ID_SUB<-z$ID_SLOPE<-NULL
  colnames(z)<-gsub("\\."," ",colnames(z))
  z<-z[order(z$SLOPE_CLASS, decreasing=FALSE), ]
  addNewLine(rtffile, n=1)
  addTable(rtffile, y)
  addNewLine(rtffile, n=1)
  addTable(rtffile, x)
  addNewLine(rtffile, n=1)
  addTable(rtffile, z)
  addNewLine(rtffile, n=1)
  text<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
  addParagraph(rtffile, text)
  addNewLine(rtffile,n=1)
}
done(rtffile)

#Dominant Landuse / Soil / Slope HRU Definition

#finding dominant landuse
tlu<-as.data.frame(NULL)
tsoil<-as.data.frame(NULL)
tslope<-as.data.frame(NULL)
for(i in 1:length(area_subcatch$ID)){
  dm_lu<-lu_report_sub[which(lu_report_sub$ID_SUB == i),]
  dm_lu<-dm_lu[order(dm_lu$area_hectares, decreasing=TRUE),]
  dm_lu<-dm_lu[1,]
  dm_lu$area_hectares<-round(dm_lu$area_hectares / as.numeric(dm_lu$sub_percentage) * 100)
  dm_lu$area_acres<-round(dm_lu$area_hectares * 2.47105)
  dm_lu$percentage<-as.numeric(format(round(dm_lu$area_hectares / area_sz * 100, 2), nsmall=2))
  dm_lu$sub_percentage<- as.numeric(format(round(dm_lu$area_hectares / dm_lu$area_hectares *100 , 2),nsmall=2))
  tlu<-rbind(tlu, dm_lu )
}

#finding dominant soil
cb<-as.data.frame(NULL)
soilt<-crosstab(stack(subcatch, lu,soil))
colnames(soilt)[1] = "ID_SUB"
colnames(soilt)[2] ="ID_LU"
colnames(soilt)[3] = "ID_SOIL"
colnames(soilt)[4] = "area_hectares"
soilt$chkVar1<-as.numeric(is.na(soilt$ID_SUB))
soilt$chkVar2<-as.numeric(is.na(soilt$ID_LU))
soilt$chkVar3<-as.numeric(is.na(soilt$ID_SOIL))
soilt$chkNull<-soilt$chkVar1+soilt$chkVar2+soilt$chkVar3
soilt<-soilt[ which(soilt$chkNull < 1),]
soilt$chkVar1<-soilt$chkVar2<-soilt$chkVar3<-soilt$chkNull<-NULL
for(i in 1:length(area_subcatch$ID)){
  a<-soilt[which(soilt$ID_SUB == i),]
  b<-tlu[which(tlu$ID_SUB == i),]
  c<-a[a$ID_LU %in% b$ID_LU,]
  cb<-rbind(cb,c)
}
cb<-cb[which(cb$area_hectares>0),]
cb<- cb[order(cb$ID_LU, cb$ID_SOIL), ]
cb<-unique(cb)
cb$sub_percentage<-cb$percentage<-cb$area_acres<-as.numeric(0)
for (i in 1:length(area_subcatch$ID)){
  dm_soil<-cb[which(cb$ID_SUB == i),]
  dm_soil<-dm_soil[order(dm_soil$area_hectares, decreasing=TRUE),]
  dm_soil<-dm_soil[1,]
  dm_soil$sub_percentage<-as.numeric(format(round(dm_soil$area_hectares / area_subcatch$SUBCATCH_COUNT[which(area_subcatch$ID == i)] * 100 , 2), nsmall=2))
  dm_soil$area_hectares<-round(dm_soil$area_hectares / as.numeric(dm_soil$sub_percentage) * 100)
  dm_soil$area_acres<-round(dm_soil$area_hectares * 2.47105)
  dm_soil$percentage<-as.numeric(format(round(dm_soil$area_hectares / area_sz * 100, 2), nsmall=2))
  dm_soil$sub_percentage<- as.numeric(format(round(dm_soil$area_hectares / dm_soil$area_hectares *100 , 2),nsmall=2))
  tsoil<-rbind(tsoil, dm_soil )
}
soildb<-tsoil
tsoil$ID_LU<-NULL
tsoil<-aggregate( . ~ ID_SUB + ID_SOIL , data = tsoil, sum)
tsoil<-merge(tsoil, lookup_so, by = "ID_SOIL")
tsoil<-tsoil[order(tsoil$ID_SUB, tsoil$ID_SOIL),]
tsoil<-tsoil[,c("ID_SUB","ID_SOIL", "SOIL_CLASS", "area_hectares", "area_acres","percentage", "sub_percentage")]


#finding dominant slope
slo<-crosstab(stack(subcatch, lu,soil, slope))
colnames(slo)[1] = "ID_SUB"
colnames(slo)[2] ="ID_LU"
colnames(slo)[3] = "ID_SOIL"
colnames(slo)[4] = "ID_SLOPE"
colnames(slo)[5] = "area_hectares"
slo$chkVar1<-as.numeric(is.na(slo$ID_SUB))
slo$chkVar2<-as.numeric(is.na(slo$ID_LU))
slo$chkVar3<-as.numeric(is.na(slo$ID_SOIL))
slo$chkVar4<-as.numeric(is.na(slo$ID_SLOPE))
slo$chkNull<-slo$chkVar1+slo$chkVar2+slo$chkVar3+slo$chkVar4
slo<-slo[ which(slo$chkNull < 1),]
slo$chkVar1<-slo$chkVar2<-slo$chkVar3<-slo$chkVar4<-slo$chkNull<-NULL
slohru<-as.data.frame(NULL)
for(i in 1:length(area_subcatch$ID)){
  a<-slo[which(slo$ID_SUB == i),]
  b<-soildb[which(soildb$ID_SUB == i),]
  for (j in 1:unique(length(b$ID_SUB))){
    x<-unique(b$ID_LU[j])
    c<-a[which(a$ID_LU == x),]
    d<-b[which(b$ID_LU == x),]
    e<-c[c$ID_SOIL %in% d$ID_SOIL,]
    slohru<-rbind(slohru,e)
  }
}
slohru<-slohru[which(slohru$area_hectares>0),]
slohru<- slohru[order(slohru$ID_SUB, slohru$ID_LU, slohru$ID_SOIL, slohru$ID_SLOPE), ]
slohru<-unique(slohru)
slohru$sub_percentage<-slohru$percentage<-slohru$area_acres<-as.numeric(0)
for (i in 1:length(area_subcatch$ID)){
  dm_slope<-slohru[which(slohru$ID_SUB == i),]
  dm_slope<-dm_slope[order(dm_slope$area_hectares, decreasing=TRUE),]
  dm_slope<-dm_slope[1,]
  dm_slope$sub_percentage<-as.numeric(format(round(dm_slope$area_hectares / area_subcatch$SUBCATCH_COUNT[which(area_subcatch$ID == i)] * 100 , 2), nsmall=2))
  dm_slope$area_hectares<-round(dm_slope$area_hectares / as.numeric(dm_slope$sub_percentage) * 100)
  dm_slope$area_acres<-round(dm_slope$area_hectares * 2.47105)
  dm_slope$percentage<-as.numeric(format(round(dm_slope$area_hectares / area_sz * 100, 2), nsmall=2))
  dm_slope$sub_percentage<- as.numeric(format(round(dm_slope$area_hectares / dm_slope$area_hectares *100 , 2),nsmall=2))
  tslope<-rbind(tslope, dm_slope )
}
slodb<-tslope
tslope$ID_LU<-tslope$ID_SOIL<-NULL
tslope<-aggregate( . ~ ID_SUB + ID_SLOPE , data = tslope, sum)
tslope<-merge(tslope, lookup_sl, by = "ID_SLOPE")
tslope<-tslope[order(tslope$ID_SUB, tslope$ID_SLOPE),]
tslope<-tslope[,c("ID_SUB","ID_SLOPE", "SLOPE_CLASS", "area_hectares", "area_acres","percentage", "sub_percentage")]

#creating HRU database and attribute table
HRU_fin_att<-slodb
HRU_fin_att<-merge(HRU_fin_att, lookup_lu, by="ID_LU")
HRU_fin_att<-merge(HRU_fin_att, lookup_so, by="ID_SOIL")
HRU_fin_att<-merge(HRU_fin_att, lookup_sl, by="ID_SLOPE")
HRU_fin_att$UNIQCOMB <- do.call(paste, c(HRU_fin_att[c("ID_LU","SOIL_CLASS","SLOPE_CLASS","ID_SUB")], sep="_"))
HRU_fin_att<-HRU_fin_att[order(HRU_fin_att$ID_SUB, HRU_fin_att$ID_LU, HRU_fin_att$ID_SOIL, HRU_fin_att$ID_SLOPE),]
HRU_fin_att$HRU<-1:length(HRU_fin_att$ID_SUB)
HRU_fin_att<-as.data.frame(HRU_fin_att[,c("HRU","UNIQCOMB","area_hectares", "area_acres", "percentage", "sub_percentage","ID_SUB", "ID_LU","LU_CLASS","ID_SOIL","SOIL_CLASS","ID_SLOPE","SLOPE_CLASS")])
HRU<-as.data.frame(HRU_fin_att[,c("ID_SUB","HRU","UNIQCOMB","area_hectares", "area_acres", "percentage", "sub_percentage")])


#creating database for report
lu_hru<-tlu
lu_hru$percentage<-as.numeric(lu_hru$percentage)
lu_hru$ID_SUB<-lu_hru$sub_percentage<-NULL
lu_hru<-aggregate( . ~ ID_LU + LU_CLASS , data = lu_hru, sum)
lu_hru<-lu_hru[order(lu_hru$ID_LU),]
lu_hru$ID_LU<-NULL

soil_hru<-tsoil
soil_hru$percentage<-as.numeric(soil_hru$percentage)
soil_hru$ID_SUB<-soil_hru$sub_percentage<-NULL
soil_hru<-aggregate( . ~ ID_SOIL + SOIL_CLASS , data = soil_hru, sum)
soil_hru<-soil_hru[order(soil_hru$ID_SOIL),]
soil_hru$ID_SOIL<-NULL

slope_hru<-tslope
slope_hru$percentage<-as.numeric(slope_hru$percentage)
slope_hru$ID_SUB<-slope_hru$sub_percentage<-NULL
slope_hru<-aggregate( . ~ ID_SLOPE + SLOPE_CLASS , data = slope_hru, sum)
slope_hru<-slope_hru[order(slope_hru$ID_SLOPE),]
slope_hru$ID_SLOPE<-NULL

#create HRU report
rtffile <- RTF("HRU.rtf")
text <- paste("LUMENS Dominant Landuse / Soil /Slope HRU Distribution Report ", "Date and Time:", Sys.time(), sep="  ")
addParagraph(rtffile, text)
addNewLine(rtffile)
text <- paste("Watershed   ", " area[ha]:", area_sz, "  area[acres]:", round(area_sz*2.47105),"  Number of HRU:", length(HRU$HRU),  " Number of Subbasins:", length(area_subcatch$ID),sep=" ")
addParagraph(rtffile, text)
addNewLine(rtffile,n=1)
addTable(rtffile, lu_hru)
addNewLine(rtffile, n=1)
addTable(rtffile, soil_hru)
addNewLine(rtffile, n=1)
addTable(rtffile, slope_hru)
addNewLine(rtffile, n=3)

text<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
addParagraph(rtffile, text)
addNewLine(rtffile,n=1)
for(i in 1:length(area_subcatch$ID)){
  text<-paste("SUBBASIN #       ", i, "       area[ha]:", tlu$area_hectares[which(tlu$ID_SUB == i)],"      area[acres]:", tlu$area_acres[which(tlu$ID_SUB == i)], "       area[percentage]:", tlu$percentage[which((tlu$ID_SUB == i))],  sep=" ")
  addParagraph(rtffile, text)
  addNewLine(rtffile,n=1)
  y<-tlu[which((tlu$ID_SUB == i)),]
  y$ID_SUB<-y$ID_LU<-NULL
  x<-tsoil[which((tsoil$ID_SUB == i)),]
  x$ID_SUB<-x$ID_SOIL<-NULL
  colnames(x)<-gsub("\\."," ",colnames(x))
  z<-tslope[which((tslope$ID_SUB == i)),]
  z$ID_SUB<-z$ID_SLOPE<-NULL
  colnames(z)<-gsub("\\."," ",colnames(z))
  z<-z[order(z$SLOPE_CLASS, decreasing=FALSE), ]
  w<-HRU[which(HRU$ID_SUB == i),]
  w$ID_SUB<-NULL
  addNewLine(rtffile, n=1)
  addTable(rtffile, y)
  addNewLine(rtffile, n=1)
  addTable(rtffile, x)
  addNewLine(rtffile, n=1)
  addTable(rtffile, z)
  addNewLine(rtffile, n=1)
  addTable(rtffile, w)
  addNewLine(rtffile, n=1)
  text<-paste("--------------------------------------------------------------------------------------------------------------------------------------------")
  addParagraph(rtffile, text)
  addNewLine(rtffile,n=1)
}
done(rtffile)

write.dbf(HRU_fin_att, "HRU.dbf", max_nchar = 254)
write.dbf(HRU_temp_att, "LUSSL.dbf", max_nchar = 254)

