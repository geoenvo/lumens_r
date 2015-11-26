##[TA]=group
##working_directory=folder
##landuse1=raster
##landuse2=raster
##zone=raster
##periode1=number 2010
##periode2=number 2015
##location=string
##carbon_lookup=file
##zone_lookup=file
##npv_lookup=file
##passfilenames

library(R2HTML)
library(raster)
library(rgdal)
library(SDMTools)
library(tiff)
library(foreign)
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
library(knitr)
library(markdown)
library(rtf)

# set working directory
setwd(working_directory)

# load datasets
landuse1 <- raster(landuse1)
landuse2 <- raster(landuse2)
zone <- raster(zone)

# Projection and Extend Handling
if (grepl("+units=m", as.character(landuse1@crs))){
print("Raster maps have projection in meter unit")
Spat_res<-res(landuse1)[1]*res(landuse1)[2]/10000
paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(landuse1@crs))){
print("Raster maps have projection in degree unit")
Spat_res<-res(landuse1)[1]*res(landuse1)[2]*(111319.9^2)/10000
paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else{
stop("Raster map projection is unknown")
}

#Extent handling and raster resolution land-cover maps
if (as.character(landuse1@crs)==as.character(landuse2@crs)){
print("Raster map time series 1 and 2 have the same projection")
if (res(landuse1)[1]==res(landuse2)[1]){
print("Raster map time series 1 and 2 have the same resolution")
if (landuse1@extent==landuse2@extent){
print("Raster map time series 1 and 2 have the same extent")
} else {
print("Raster map time series 1 and 2 don't have the same extent, synchronising land-cover map...")
landuse2<-spatial_sync_raster(landuse2, landuse1, method = "ngb")
}
} else{
print("Raster map time series 1 and 2 don't have the same resolution, synchronising land-cover map...")
landuse2<-spatial_sync_raster(landuse2, landuse1, method = "ngb")
}
} else{
print("Raster map time series 1 and 2 don't have the same projection, synchronising land-cover map...")
landuse2<-spatial_sync_raster(landuse2, landuse1, method = "ngb")
}


# Extent handling and raster resolution land-cover maps
if (as.character(landuse1@crs)==as.character(zone@crs)){
print("Raster map time series 1 and 2 have the same projection")
if (res(landuse1)[1]==res(zone)[1]){
print("Raster map time series 1 and 2 have the same resolution")
if (landuse1@extent==zone@extent){
print("Raster map time series 1 and 2 have the same extent")
} else {
print("Raster map time series 1 and 2 don't have the same extent, synchronising land-cover map...")
zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
}
} else{
print("Raster map time series 1 and 2 don't have the same resolution, synchronising land-cover map...")
zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
}
} else{
print("Raster map time series 1 and 2 don't have the same projection, synchronising land-cover map...")
zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
}

# load look up tables
lookup_c<- read.table(carbon_lookup, header=TRUE, sep=",",)
lookup_n <- read.table(npv_lookup, header=TRUE, sep=",",)
lookup_z <- read.table(zone_lookup, header=TRUE, sep=",",)

# create categorical raster using ratify function
landuse1<-ratify(landuse1, filename='landuse1.grd',count=TRUE,overwrite=TRUE)
landuse2<-ratify(landuse2, filename='landuse2.grd',count=TRUE,overwrite=TRUE)
zone<-ratify(zone, filename='zone.grd',count=TRUE,overwrite=TRUE)

#ZONE
colnames(lookup_z)<-c("ID", "ZONE")
zone_lookup<-merge(levels(zone),lookup_z, by="ID")
zone_lookup$Rowid<-NULL

ratzone<- levels(zone)[[1]]
ratzone$zone<-zone_lookup$ZONE
zone_pl<-zone ; #separating file only for plotting
levels(zone_pl) <- ratzone
#levelplot(zone_pl, col.regions=rainbow)

# set proj prop
title=location
tab_title<-as.data.frame(title)
period1=periode1
period2=periode2
period=period2-period1
proj_prop<-as.data.frame(title)
proj_prop$period1<-period1
proj_prop$period2<-period2
proj_prop$period <- do.call(paste, c(proj_prop[c("period1", "period2")], sep = " - "))

# create raster attribute table usign land cover file look up table
area_lc1<-as.data.frame(levels(landuse1))
area_lc2<-as.data.frame(levels(landuse2))
area_zone<-as.data.frame(levels(zone))
area<-sum(area_zone$COUNT)

#create carbon density maps
levels(landuse1)<-merge((levels(landuse1)),lookup_c,by="ID")
levels(landuse2)<-merge((levels(landuse2)),lookup_c,by="ID")
lookup_n$CLASS<-NULL
levels(landuse1)<-merge((levels(landuse1)),lookup_n,by="ID")
levels(landuse2)<-merge((levels(landuse2)),lookup_n,by="ID")
levels(zone) <- merge(area_zone,lookup_z,by="ID")
area_lc1<-as.data.frame(levels(landuse1))
area_lc2<-as.data.frame(levels(landuse2))
area_zone<-as.data.frame(levels(zone))
#landuse1<-writeRaster(landuse1,"c:/Bungo/temp/landuse1.grd", overwrite=TRUE)
#landuse2<-writeRaster(landuse2,"c:/Bungo/temp/landuse2.grd", overwrite=TRUE)
#zone<-writeRaster(zone,"c:/Bungo/temp/zone.grd", overwrite=TRUE)
carbon1 <- deratify(landuse1,'CARBON')
carbon2 <- deratify(landuse2,'CARBON')
npv1<-deratify(landuse1,'NPV')
npv2<-deratify(landuse2,'NPV')

#create emission and sequestration map
chk_em<-carbon1>carbon2
chk_sq<-carbon1<carbon2
emission <- (carbon1-carbon2)*chk_em*3.67
sequestration<-(carbon2-carbon1)*chk_sq*3.67
npv_chg<-npv2-npv1
opcost<-npv_chg/emission

#export analysis result
carbontiff1<-carbon1
carbontiff2<-carbon2
npvtiff1<-npv1
npvtiff2<-npv2
npvchgtiff<-npv_chg
opcosttiff<-opcost

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Trade-off Analysis (TA) Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 1: Opportunity Cost Map \\b0\\fs20"
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1<-"\\b\\fs24 1.Carbon stock maps \\b0\\fs20"
chapter2<-"\\b\\fs24 2.NPV maps \\b0\\fs20"
chapter3<-"\\b\\fs24 3.Opportunity cost maps \\b0\\fs20"
rtffile <- RTF("LUMENS_TA-1_report.lpr", font.size=9)
if (file.exists("C:/Program Files (x86)/LUMENS")){
addPng (rtffile, "C:/Program Files (x86)/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
} else{
addPng (rtffile, "C:/Program Files/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
}
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addNewLine(rtffile)
C1 <- levelplot(carbon1, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C1 )
addParagraph(rtffile, "\\b\\fs20 Figure 1. Carbon density maps t1\\b0\\fs20.")
addNewLine(rtffile)
C2 <- levelplot(carbon2, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C2 )
addParagraph(rtffile, "\\b\\fs20 Figure 2. Carbon density maps t2\\b0\\fs20.")
addNewLine(rtffile)
C3 <- levelplot(emission, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C3 )
addParagraph(rtffile, "\\b\\fs20 Figure 3. Emission maps t1-t2\\b0\\fs20.")
addNewLine(rtffile)
C4 <- levelplot(emission, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C4 )
addParagraph(rtffile, "\\b\\fs20 Figure 4. Emission maps t1-t2\\b0\\fs20.")
addNewLine(rtffile)
C5 <- levelplot(sequestration, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C5 )
addParagraph(rtffile, "\\b\\fs20 Figure 5. Sequestration maps t1-t2\\b0\\fs20.")
addNewLine(rtffile)
C6 <- levelplot(sequestration, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C6 )
addParagraph(rtffile, "\\b\\fs20 Figure 6. Sequestration maps t1-t2\\b0\\fs20.")
addNewLine(rtffile)
addParagraph(rtffile, chapter2)
addNewLine(rtffile)
C7 <- levelplot(npv1, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C7 )
addParagraph(rtffile, "\\b\\fs20 Figure 7. NPV map t1\\b0\\fs20.")
addNewLine(rtffile)
C8<- levelplot(npv2, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C8 )
addParagraph(rtffile, "\\b\\fs20 Figure 8. NPV map t2\\b0\\fs20.")
addNewLine(rtffile)
C9 <- levelplot(npv_chg, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C9 )
addParagraph(rtffile, "\\b\\fs20 Figure 9. NPV change map t1-t2\\b0\\fs20.")
addNewLine(rtffile)
addParagraph(rtffile, chapter3)
addNewLine(rtffile)
C10 <-gplot(opcost) + geom_tile(aes(fill = value)) + scale_fill_gradient(low = 'white', high = 'blue') + coord_equal()
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C10 )
addParagraph(rtffile, "\\b\\fs20 Figure 10. Opcost map t1-t2\\b0\\fs20.")
addNewLine(rtffile)

done(rtffile)

