##[QUES]=group
##working_directory=folder
##landuse1=raster
##landuse2=raster
##zone=raster
##nodata=number 0
##periode1=number 2010
##periode2=number 2015
##location=string
##carbon_lookup=file
##zone_lookup=file
##carbontiff1=output raster
##carbontiff2=output raster
##emission=output raster
##sequestration=output raster
##zone_carbon=output table
##fs_table=output table
##data_merge=output table
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
library(rstudio)
library(rstudioapi)
library(gWidgetsRGtk2)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#====Set Working Directory====
setwd(working_directory)

#====Load Datasets====
landuse1 <- raster(landuse1) 
landuse2 <- raster(landuse2)
zone <- raster(zone)


#====Projection and Extend Handling====
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

#====Load Lookup Tables====
lookup_c<- read.table(carbon_lookup, header=TRUE, sep=",",)
lookup_z <- read.table(zone_lookup, header=TRUE, sep=",",)
lookup_c<-lookup_c[which(lookup_c[1] != nodata),]
lookup_lc<-lookup_c

#====Set Project Properties====
title=location
tab_title<-as.data.frame(title)
period1=periode1
period2=periode2
period=period2-period1
proj_prop<-as.data.frame(title)
proj_prop$period1<-period1
proj_prop$period2<-period2
proj_prop$period <- do.call(paste, c(proj_prop[c("period1", "period2")], sep = " - "))


#====Carbon Accounting Process====
NAvalue(landuse1)<-nodata
NAvalue(landuse2)<-nodata
rcl.m.c1<-as.matrix(lookup_c[,1])
rcl.m.c2<-as.matrix(lookup_c[,3])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
carbon1<-reclassify(landuse1, rcl.m)
carbon2<-reclassify(landuse2, rcl.m)
chk_em<-carbon1>carbon2
chk_sq<-carbon1<carbon2
emission<-((carbon1-carbon2)*3.67)*chk_em
sequestration<-((carbon2-carbon1)*3.67)*chk_sq
rasterStack<-stack(landuse1,landuse2, zone)
cross<-as.data.frame(crosstab(rasterStack))
colnames(cross)[1] ="ID_LC1"
colnames(cross)[2] = "ID_LC2"
colnames(cross)[3] = "ZONE"
colnames(cross)[4] = "COUNT"
colnames(lookup_c)[1]="ID_LC1"
colnames(lookup_c)[2]="LC_t1"
colnames(lookup_c)[3]="CARBON_t1"
data_merge <- merge(cross,lookup_c,by="ID_LC1")
colnames(lookup_c)[1]="ID_LC2"
colnames(lookup_c)[2]="LC_t2"
colnames(lookup_c)[3]="CARBON_t2"
data_merge <- as.data.frame(merge(data_merge,lookup_c,by="ID_LC2"))
colnames(lookup_z)[1]="ZONE"
colnames(lookup_z)[2]="Z_NAME"
data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
rm(cross)

#===Modify Carbon Stock Density for Each Time Series====
data_merge$CARBON_t1<-data_merge$CARBON_t1*Spat_res
data_merge$CARBON_t2<-data_merge$CARBON_t2*Spat_res

data_merge$ck_em<-data_merge$CARBON_t1>data_merge$CARBON_t2
data_merge$ck_sq<-data_merge$CARBON_t1<data_merge$CARBON_t2
data_merge$em<-(data_merge$CARBON_t1-data_merge$CARBON_t2)*data_merge$ck_em*data_merge$COUNT*3.67
data_merge$sq<-(data_merge$CARBON_t2-data_merge$CARBON_t1)*data_merge$ck_sq*data_merge$COUNT*3.67
data_merge$LU_CHG <- do.call(paste, c(data_merge[c("LC_t1", "LC_t2")], sep = " to "))
data_merge$null<-0
data_merge$nullCek<-data_merge$em+data_merge$sq


#===Generate area_zone Lookup and Calculate Min Area
area_zone<-(freq(zone, useNA="no"))
colnames(area_zone)[1]<-"ID"
colnames(area_zone)[2]<-"COUNT"
colnames(lookup_z)[1]<-"ID"
area_zone<-merge(area_zone, lookup_z, by="ID")
area<-min(sum(area_zone$COUNT), sum(data_merge$COUNT))

#====Calculate Emission for each Planning Unit====
zone_emission <- as.data.frame(zonal((Spat_res*emission),zone,'sum')) #adjust emission by actual raster area
zone_sequestration <- as.data.frame(zonal((Spat_res*sequestration),zone,'sum'))#adjust sequestration by actual raster area
colnames(zone_emission)[1] = "ID"
colnames(zone_emission)[2] = "Em_tot"
colnames(zone_sequestration)[1] = "ID"
colnames(zone_sequestration)[2]="Sq_tot"
zone_emission<-merge(area_zone,zone_emission,by="ID")
zone_carbon<-merge(zone_emission,zone_sequestration,by="ID")
zone_carbon$Net_em<-zone_carbon$Em_tot-zone_carbon$Sq_tot
zone_carbon$Net_em_rate<-round((zone_carbon$Net_em/zone_carbon$COUNT/period), digits=3)
zone_carbon$Sq_tot<-round(zone_carbon$Sq_tot, digits=3)
#zone_carbon[,4:7]<-round(zone_carbon[,4:7], digits=3)

#====Create Final Summary of Emission Calculation at Landscape Level
fs_id<-c(1,2,3,4,5,6,7)
fs_cat<-c("Period", "Total area", "Total Emission (Ton CO2eq)", "Total Sequestration (Ton CO2eq)", "Net emission (Ton CO2eq)", "Emission rate (Ton CO2/yr)","Emission rate per-unit area (Ton CO2eq/ha.yr)")
fs_em<-sum(zone_carbon$Em_tot)
fs_sq<-sum(zone_carbon$Sq_tot)
fs_Nem<-fs_em-fs_sq
fs_Rem<-fs_Nem/period
fs_ARem<-fs_Rem/area
fs_summary<-c(proj_prop$period, area,round(fs_em, digits=3),round(fs_sq, digits=3),round(fs_Nem, digits=3),round(fs_Rem, digits=3),round(fs_ARem, digits=3))
fs_table<-data.frame(fs_id,fs_cat,fs_summary)
colnames(fs_table)<-c("ID", "Category", "Summary")

#====CREATE QuES-C Database====

#====Zonal Statistics Database====
lg<-length(unique(data_merge$ZONE))
zone_lookup<-area_zone
data_zone<-area_zone
data_zone$Z_CODE<-toupper(abbreviate(data_zone$Z_NAME))
for(a in 1:lg){
  i<-unique(data_merge$ZONE)[a]
  data_z<-data_merge[which(data_merge$ZONE == i),]
  data_zone$Avg_C_t1[which(data_zone$ID == i)]<-sum(data_z$CARBON_t1*data_z$COUNT)/sum(data_z$COUNT)
  data_zone$Avg_C_t2[which(data_zone$ID == i)]<-sum(data_z$CARBON_t2*data_z$COUNT)/sum(data_z$COUNT)
  data_zone$Rate_em[which(data_zone$ID == i)]<-sum(data_z$em)/(sum(data_z$COUNT)*period)
  data_zone$Rate_seq[which(data_zone$ID == i)]<-sum(data_z$sq)/(sum(data_z$COUNT)*period)
}
data_zone[,5:8]<-round(data_zone[,5:8],digits=3)

#====Calculate Largest Source of Emission====
data_merge_sel <- data_merge[ which(data_merge$nullCek > data_merge$null),]
order_sq <- as.data.frame(data_merge[order(-data_merge$sq),])
order_em <- as.data.frame(data_merge[order(-data_merge$em),])

#====Total Emission====
tb_em_total<-as.data.frame(cbind(order_em$LU_CHG, as.data.frame(round(order_em$em, digits=3))))
colnames(tb_em_total)<-c("LU_CHG", "em")
tb_em_total<-aggregate(em~LU_CHG,data=tb_em_total,FUN=sum)
tb_em_total$LU_CODE<-as.factor(toupper(abbreviate(tb_em_total$LU_CHG, minlength=5, strict=FALSE, method="both")))
tb_em_total<-tb_em_total[order(-tb_em_total$em),]
tb_em_total<-tb_em_total[c(3,1,2)]
tb_em_total$Percentage<-as.numeric(format(round((tb_em_total$em / sum(tb_em_total$em) * 100),2), nsmall=2))
tb_em_total_10<-head(tb_em_total,n=10)

#====Zonal Emission====
tb_em_zonal<-as.data.frame(NULL)
for (i in 1:length(zone_lookup$ID)){
  a<-(zone_lookup$ID)[i]
  tb_em<-as.data.frame(cbind(order_em$ZONE, order_em$LU_CHG, as.data.frame(round(order_em$em, digits=3))))
  colnames(tb_em)<-c("ZONE","LU_CHG", "em")
  tb_em_z<-as.data.frame(tb_em[which(tb_em$ZONE == a),])
  tb_em_z<-aggregate(em~ZONE+LU_CHG,data=tb_em_z,FUN=sum)
  tb_em_z$LU_CODE<-as.factor(toupper(abbreviate(tb_em_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
  tb_em_z<-tb_em_z[order(-tb_em_z$em),]
  tb_em_z<-tb_em_z[c(1,4,2,3)]
  tb_em_z$Percentage<-as.numeric(format(round((tb_em_z$em / sum(tb_em_z$em) * 100),2), nsmall=2))
  tb_em_z_10<-head(tb_em_z,n=10)
  tb_em_zonal<-rbind(tb_em_zonal,tb_em_z_10)
}
rm(tb_em, tb_em_total, tb_em_z, tb_em_z_10)

#====Total Sequestration====
tb_seq_total<-as.data.frame(cbind(order_sq$LU_CHG, as.data.frame(round(order_sq$sq, digits=3))))
colnames(tb_seq_total)<-c("LU_CHG", "seq")
tb_seq_total<-aggregate(seq~LU_CHG,data=tb_seq_total,FUN=sum)
tb_seq_total$LU_CODE<-as.factor(toupper(abbreviate(tb_seq_total$LU_CHG, minlength=5, strict=FALSE, method="both")))
tb_seq_total<-tb_seq_total[order(-tb_seq_total$seq),]
tb_seq_total<-tb_seq_total[c(3,1,2)]
tb_seq_total$Percentage<-as.numeric(format(round((tb_seq_total$seq / sum(tb_seq_total$seq) * 100),2), nsmall=2))
tb_seq_total_10<-head(tb_seq_total,n=10)

#====Zonal Sequestration====
tb_seq_zonal<-as.data.frame(NULL)
for (i in 1:length(zone_lookup$ID)){
  a<-(zone_lookup$ID)[i]
  tb_seq<-as.data.frame(cbind(order_sq$ZONE, order_sq$LU_CHG, as.data.frame(round(order_sq$sq, digits=3))))
  colnames(tb_seq)<-c("ZONE","LU_CHG", "seq")
  tb_seq_z<-as.data.frame(tb_seq[which(tb_seq$ZONE == i),])
  tb_seq_z<-aggregate(seq~ZONE+LU_CHG,data=tb_seq_z,FUN=sum)
  tb_seq_z$LU_CODE<-as.factor(toupper(abbreviate(tb_seq_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
  tb_seq_z<-tb_seq_z[order(-tb_seq_z$seq),]
  tb_seq_z<-tb_seq_z[c(1,4,2,3)]
  tb_seq_z$Percentage<-as.numeric(format(round((tb_seq_z$seq / sum(tb_seq_z$seq) * 100),2), nsmall=2))
  tb_seq_z_10<-head(tb_seq_z,n=10)
  tb_seq_zonal<-rbind(tb_seq_zonal,tb_seq_z_10)
}
rm(tb_seq, tb_seq_total, tb_seq_z, tb_seq_z_10)

#====Zonal Additional Statistics====
if (((length(unique(data_merge$ID_LC1)))>(length(unique(data_merge$ID_LC2))))){
  dimention<-length(unique(data_merge$ID_LC1))
  name.matrix<-cbind(as.data.frame(data_merge$ID_LC1), as.data.frame(data_merge$LC_t1))
  name.matrix<-unique(name.matrix)
  colnames(name.matrix)<-c("ID","LC")
  name.matrix<-name.matrix[order(name.matrix$ID),]
  name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$LC, minlength=4, method="both"))
} else{
  dimention<-length(unique(data_merge$ID_LC2))
  name.matrix<-cbind(as.data.frame(data_merge$ID_LC2), as.data.frame(data_merge$LC_t2))
  name.matrix<-unique(name.matrix)
  colnames(name.matrix)<-c("ID","LC")
  name.matrix<-name.matrix[order(name.matrix$ID),]
  name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$LC, minlength=4, method="both"))
}

#====Zonal Emission matrix====
e.m.z<-matrix(0, nrow=dimention, ncol=dimention)
em.matrix.zonal<-as.data.frame(NULL)
for (k in 1:length(zone_lookup$ID)){
  for (i in 1:nrow(e.m.z)){
    for (j in 1:ncol(e.m.z)){
      em.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j & data_merge_sel$ZONE==k),]
      e.m.z[i,j]<-as.numeric(round(sum(em.data$em), 2))
    }
  }
  e.m.z<-as.data.frame(e.m.z)
  e.m.z.c<-as.data.frame(cbind(name.matrix$LC_CODE,e.m.z))
  e.m.z.c<-cbind(rep(k,nrow(e.m.z)),e.m.z.c)
  em.matrix.zonal<-rbind(em.matrix.zonal,e.m.z.c)
}
colnames(em.matrix.zonal)<-c("ZONE","LC_CODE",as.vector(name.matrix$LC_CODE))
rm(em.data, e.m.z, e.m.z.c)


#====Total Emission matrix====
e.m<-matrix(0, nrow=dimention, ncol=dimention)
for (i in 1:nrow(e.m)){
  for (j in 1:ncol(e.m)){
    em.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j),]
    e.m[i,j]<-round(sum(em.data$em), digits=2)
  }
}
e.m<-as.data.frame(e.m)
em.matrix.total<-as.data.frame(cbind(name.matrix$LC_CODE,e.m))
colnames(em.matrix.total)<-c("LC_CODE",as.vector(name.matrix$LC_CODE))
rm(em.data, e.m)

#====Zonal Sequestration matrix====
s.m.z<-matrix(0, nrow=dimention, ncol=dimention)
seq.matrix.zonal<-as.data.frame(NULL)
for (k in 1:length(zone_lookup$ID)){
  for (i in 1:nrow(s.m.z)){
    for (j in 1:ncol(s.m.z)){
      seq.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j & data_merge_sel$ZONE==k),]
      s.m.z[i,j]<-round(sum(seq.data$sq), digits=2)
    }
  }
  s.m.z<-as.data.frame(s.m.z)
  s.m.z.c<-as.data.frame(cbind(name.matrix$LC_CODE,s.m.z))
  s.m.z.c<-cbind(rep(k,nrow(s.m.z)),s.m.z.c)
  seq.matrix.zonal<-rbind(seq.matrix.zonal,s.m.z.c)
}
colnames(seq.matrix.zonal)<-c("ZONE","LC_CODE",as.vector(name.matrix$LC_CODE))
rm(seq.data, s.m.z, s.m.z.c)

#====Total Sequestration matrix====
s.m<-matrix(0, nrow=dimention, ncol=dimention)
for (i in 1:nrow(s.m)){
  for (j in 1:ncol(s.m)){
    seq.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j),]
    s.m[i,j]<-round(sum(seq.data$sq), digits=2)
  }
}
s.m<-as.data.frame(s.m)
seq.matrix.total<-as.data.frame(cbind(name.matrix$LC_CODE,s.m))
colnames(seq.matrix.total)<-c("LC_CODE",as.vector(name.matrix$LC_CODE))
rm(seq.data, s.m, order_em, order_sq)

work_dir<-paste(working_directory,"/Result", sep="")
dir.create("Result")
setwd(work_dir)

#====Export Analysis Result====
carbontiff1<-carbon1
carbontiff2<-carbon2
writeRaster(carbon1, filename="carbon1.tif", format="GTiff", overwrite=TRUE)
writeRaster(carbon2, filename="carbon2.tif", format="GTiff", overwrite=TRUE)
writeRaster(emission, filename="emission.tif", format="GTiff", overwrite=TRUE)
writeRaster(sequestration, filename="sequestration.tif", format="GTiff", overwrite=TRUE)
write.dbf(zone_carbon, "emission_by_zone.dbf")
write.dbf(fs_table, "summary_QUES-C.dbf")
write.dbf(data_merge, "QUES-C_database.dbf")
write.dbf(data_zone, "Carbon_Summary.dbf")
write.dbf(em.matrix.total,"Total_Emission_Matrix.dbf ")
write.dbf(seq.matrix.total, "Total_Sequestration_Matrix.dbf")
for (i in 1:length(zone_lookup$ID)){
  em_matrix_z<-em.matrix.zonal[which(em.matrix.zonal$ZONE == i),]
  em_matrix_z$ZONE<-NULL
  seq_matrix_z<-seq.matrix.zonal[which(seq.matrix.zonal$ZONE == i),]
  seq_matrix_z$ZONE<-NULL
  write.dbf(em_matrix_z,paste("Emission_Matrix_Zone_",i,sep=""))
  write.dbf(seq_matrix_z,paste("Sequestration_Matrix_Zone_",i,sep=""))
}

#====Rearrange zone carbon====
zone_carbon_pub<-zone_carbon
colnames(zone_carbon_pub) <- c("ID", "Area (Ha)", "Land cover class", "Total emission (Ton CO2/Ha)", "Total sequestration(Ton CO2/Ha)", "Net emission (Ton CO2/Ha)", "Emission rate (Ton CO2/Ha.yr)")

#====Create Map for report====
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- brewer.pal(11, "Spectral")
myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
rm(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)

#====Landuse 1 map====
myColors.lu <- myColors[1:length(unique(lookup_lc$ID))]
lookup_lc$Colors<-myColors.lu
lu1<-data_merge[,3]
lu1<-as.data.frame(unique(lu1))
colnames(lu1)<-"ID"
lu1<-merge(lu1,lookup_lc, by="ID")
lu1<-lu1[order(lu1$ID),]
ColScale.lu1<-scale_fill_manual(name="Land Use Class", breaks=lu1$ID, labels=lu1$LC, values=lu1$Colors)
plot.LU1<-gplot(landuse1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu1 +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#====Exporting File====
png(filename="Landuse1.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,
    res=125)
print(plot.LU1)
dev.off()
file.name<-"Landuse1.png"

#====Landuse 2 map====
lu2<-data_merge[,2]
lu2<-as.data.frame(unique(lu2))
colnames(lu2)<-"ID"
lu2<-merge(lu2,lookup_lc, by="ID")
lu2<-lu2[order(lu2$ID),]
ColScale.lu2<-scale_fill_manual(name="Land Use Class", breaks=lu2$ID, labels=lu2$LC, values=lu2$Colors)
plot.LU2<-gplot(landuse2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu2 +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#====Exporting File====
png(filename="Landuse2.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.LU2)
dev.off()
file.name<-c(file.name, "Landuse2.png")

#====zone map====
myColors.Z <- myColors[1:length(unique(lookup_z$ID))]
ColScale.Z<-scale_fill_manual(name="Zone Class", breaks=lookup_z$ID, labels=lookup_z$Z_NAME, values=myColors.Z)
plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.Z +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#====Exporting File====
png(filename="Zone.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.Z)
dev.off()

file.name<-c(file.name, "Zone.png")

#====Average Zonal Carbon Rate t1====
rcl.m.c1<-as.matrix(data_zone[,1])
rcl.m.c2<-as.matrix(data_zone[,5])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
Z.Avg.C.t1<-reclassify(zone, rcl.m)
plot.Z.Avg.C.t1<-gplot(Z.Avg.C.t1, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste(" Average Carbon Density of", location, period1 )) + 
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

png(filename="Average_Z_C_t1.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.Z.Avg.C.t1)
dev.off()

file.name<-c(file.name, "Average_Z_C_t1.png")

#====Average Zonal Carbon Rate t2====
rcl.m.c1<-as.matrix(data_zone[,1])
rcl.m.c2<-as.matrix(data_zone[,6])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
Z.Avg.C.t2<-reclassify(zone, rcl.m)
plot.Z.Avg.C.t2<-gplot(Z.Avg.C.t2, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste(" Average Carbon Density of", location, period2 )) + 
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

png(filename="Average_Z_C_t2.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.Z.Avg.C.t2)
dev.off()

file.name<-c(file.name, "Average_Z_C_t2.png")

#====Average Zonal Emission Rate====
rcl.m.c1<-as.matrix(data_zone[,1])
rcl.m.c2<-as.matrix(data_zone[,7])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
Z.Avg.em<-reclassify(zone, rcl.m)
plot.Z.Avg.em<-gplot(Z.Avg.em, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(name="Emission Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste(" Average Emission Rate of", location, period1, "-", period2 )) + 
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

png(filename="Average_Z_E.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.Z.Avg.em)
dev.off()

file.name<-c(file.name, "Average_Z_E.png")

#====Average Zonal Sequestration Rate==== 
rcl.m.c1<-as.matrix(data_zone[,1])
rcl.m.c2<-as.matrix(data_zone[,8])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
Z.Avg.sq<-reclassify(zone,rcl.m)
plot.Z.Avg.sq<-gplot(Z.Avg.sq, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(name="Sequestration Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste(" Average Sequestration Rate of", location, period1, "-", period2 )) + 
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

png(filename="Average_Z_S.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.Z.Avg.sq)
dev.off()

file.name<-c(file.name, "Average_Z_S.png")

#====Carbon 1 map====
y<-ceiling( maxValue(carbon1)/100)
y<-y*100
plot.C1  <- gplot(carbon1, maxpixels=100000) + geom_raster(aes(fill=value)) + coord_equal() +
  scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300",limits=c(0,y), breaks=c(0,10,20,50,100,200,300), guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 7),
         legend.key.height = unit(1.5, "cm"),
         legend.key.width = unit(0.375, "cm"))

png(filename="Average_C_t1.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.C1)
dev.off()

file.name<-c(file.name, "Average_C_t1.png")

#====Carbon 2 map====
plot.C2  <- gplot(carbon2, maxpixels=100000) + geom_raster(aes(fill=value)) + coord_equal() +
  scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300",limits=c(0,y), breaks=c(0,10,20,50,100,200,300), guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 7),
         legend.key.height = unit(1.5, "cm"),
         legend.key.width = unit(0.375, "cm"))

png(filename="Average_C_t2.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.C2)
dev.off()

file.name<-c(file.name, "Average_C_t2.png")


#====Carbon Emission Map====
plot.E  <- gplot(emission, maxpixels=100000) + geom_raster(aes(fill=value)) + coord_equal() +
  scale_fill_gradient(name="Emission (TON CO2eq)",low = "#FFCC66", high="#FF0000", guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

png(filename="Average_C_E.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.E)
dev.off()

file.name<-c(file.name, "Average_C_E.png")

#====Carbon Sequestration Map====
plot.S  <- gplot(sequestration, maxpixels=100000) + geom_raster(aes(fill=value)) + coord_equal() +
  scale_fill_gradient(name="Sequestration (TON CO2eq)",low = "#FFCC66", high="#000033", guide="colourbar") + 
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

png(filename="Average_C_S.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(plot.S)
dev.off()

file.name<-c(file.name, "Average_C_S.png")

#====Emission Rate====
emissionRate<-ggplot(data=zone_carbon, aes(x=reorder(Z_NAME, -Net_em_rate), y=(zone_carbon$Net_em_rate))) + geom_bar(stat="identity", fill="Red") +
  geom_text(data=zone_carbon, aes(label=round(Net_em_rate, 1)),size=4) +
  ggtitle(paste("Net Emmission Rate of", location, period1,"-", period2 )) + guides(fill=FALSE) + ylab("CO2eq/ha.yr") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle=20),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

png(filename="Emission_Rate.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(emissionRate)
dev.off()

chart.name<-"Emission_Rate.png"

#====Largest emission====
largestEmission<-ggplot(data=tb_em_total_10, aes(x=reorder(LU_CODE, -em), y=(em))) + geom_bar(stat="identity", fill="blue") +
  geom_text(data=tb_em_total_10, aes(x=LU_CODE, y=em, label=round(em, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Largest Source of Emission in", location )) + guides(fill=FALSE) + ylab("CO2eq") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

png(filename="Largest_Emission.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(largestEmission)
dev.off()

chart.name<-c(chart.name, "Largest_Emission.png")

#====Largest Sequestration====
largestSeq<-ggplot(data=tb_seq_total_10, aes(x=reorder(LU_CODE, -seq), y=(seq))) + geom_bar(stat="identity", fill="green") +
  geom_text(data=tb_seq_total_10, aes(x=LU_CODE, y=seq, label=round(seq, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Largest Source of Sequestration in", location )) + guides(fill=FALSE) + ylab("CO2eq") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

png(filename="Largest_Sequestration.png", 
    type="cairo",
    units="in", 
    width=6.7, 
    height=4,  
    res=125)
print(largestSeq)
dev.off()

chart.name<-c(chart.name, "Largest_Sequestration.png")


#====Create RTF Report File====
title<-"\\b\\fs32 LUMENS-QUES Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules: Carbon Dynamics Quantification\\b0\\fs20"
test<-as.character(Sys.Date())
date<-paste("Date : ", test, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
chapter1<-"\\b\\fs24 DATA INPUT \\b0\\fs20"
chapter2<-"\\b\\fs24 ANALYSIS AT LANDSCAPE LEVEL \\b0\\fs20"
chapter3<-"\\b\\fs24 ANALYSIS AT PLANNING UNIT LEVEL \\b0\\fs20"
rtffile <- RTF("LUMENS_QUES-C_report.lpr", font.size=9)
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

text <- paste("\\b \\fs20 Peta penutupan lahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.4, height=4, res=150, plot.LU1 )
rm(plot.LU1)
text <- paste("\\b \\fs20 Peta penutupan lahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_2_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.LU2 )
rm(plot.LU2)
text <- paste("\\b \\fs20 Peta unit perencanaan \\b0 \\fs20 ", area_name_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z )
rm(plot.Z)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, chapter2)
addNewLine(rtffile)
text <- paste("\\b \\fs20 Peta kerapatan karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, " \\b \\fs20 (dalam Ton C/Ha)\\b0 \\fs20", sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.C1 )
rm(plot.C1)
text <- paste("\\b \\fs20 Peta kerapatan karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_2_rep, " \\b \\fs20 (dalam Ton C/Ha)\\b0 \\fs20", sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.C2 )
addNewLine(rtffile, n=1)
rm(plot.C2)
text <- paste("\\b \\fs20 Peta emisi karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.E )
addNewLine(rtffile, n=1)
rm(plot.E)
text <- paste("\\b \\fs20 Peta penyerapan karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep, sep="")
addParagraph(rtffile, text)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.S )
rm(plot.S)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, "\\b \\fs20 Intisari perhitungan emisi\\b0 \\fs20")
addNewLine(rtffile, n=1)
addTable(rtffile, fs_table)
addNewLine(rtffile, n=1)

addParagraph(rtffile, "\\b \\fs20 Intisari perhitungan emisi per unit perencanaan\\b0 \\fs20")
addNewLine(rtffile, n=1)
addTable(rtffile, data_zone)
addNewLine(rtffile, n=1)

addNewLine(rtffile, n=1)
addTable(rtffile, zone_carbon)
addParagraph(rtffile, "Note : ")
addParagraph(rtffile, "Em_tot = Total Emission in ton CO2eq ")
addParagraph(rtffile, "Sq_tot = Total Sequestration in ton CO2eq ")
addParagraph(rtffile, "Net_em = Total Emission - Total Sequestration in ton CO2eq ")
addParagraph(rtffile, "Net_em_rate = (Total Emission - Total Sequestration) / (area * period) in ton CO2eq/ha.year ")
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, emissionRate )
addNewLine(rtffile, n=1)
rm(emissionRate)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.C.t1 )
addNewLine(rtffile, n=1)
rm(plot.Z.Avg.C.t1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.C.t2 )
addNewLine(rtffile, n=1)
rm(plot.Z.Avg.C.t2)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.em  )
addNewLine(rtffile, n=1)
rm(plot.Z.Avg.em)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.sq )
rm(plot.Z.Avg.sq)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, "\\b \\fs20 Largest Sources of Emission\\b0 \\fs20")
addNewLine(rtffile, n=1)
addTable(rtffile, tb_em_total_10)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestEmission )
addNewLine(rtffile, n=1)
rm(largestEmission)
addParagraph(rtffile, "\\b \\fs20 Largest Sources of Sequestration\\b0 \\fs20")
addNewLine(rtffile, n=1)
addTable(rtffile, tb_seq_total_10)
addNewLine(rtffile, n=1)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestSeq )
addNewLine(rtffile, n=1)
rm(largestSeq)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, chapter3)
addNewLine(rtffile)

z.emission.name<-as.vector(NULL)
z.seq.name<-as.vector(NULL)
for(i in 1:length(zone_lookup$ID)){
  a<-zone_lookup$ID[i]
  zona<-paste("\\b", "\\fs20", i, "\\b0","\\fs20")
  zona_nm<-paste("\\b", "\\fs20", data_zone$Z_NAME[i], "\\b0","\\fs20")
  zona_ab<-paste("\\b", "\\fs20", data_zone$Z_CODE[i], "\\b0","\\fs20")
  addParagraph(rtffile, "\\b \\fs20 Largest Sources of Emission in Zone \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
  addNewLine(rtffile, n=1)
  
  tb_em_zon<-tb_em_zonal[which(tb_em_zonal$ZONE == a),]
  tb_em_zon$ZONE<-NULL
  addTable(rtffile, tb_em_zon)
  addNewLine(rtffile, n=1)
  
  #Largest emission
  largestE.Z<-ggplot(data=tb_em_zon, aes(x=reorder(LU_CODE, -em), y=(em))) + geom_bar(stat="identity", fill="blue") +
    geom_text(data=tb_em_zon, aes(x=LU_CODE, y=em, label=round(em, 1)),size=3, vjust=0.1) +
    ggtitle(paste("Largest Emission in Zone",i, "-", data_zone$Z_CODE[i] )) + guides(fill=FALSE) + ylab("CO2eq") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  png(filename=paste("Largest_Emission_Z_",a,".png", sep=""), 
      type="cairo",
      units="in", 
      width=6.7, 
      height=4,  
      res=125)
  print(largestE.Z)
  dev.off()
  
  z.emission.name<-c(z.emission.name, paste("Largest_Emission_Z_",a,".png", sep=""))
  
  addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestE.Z )
  addNewLine(rtffile, n=1)
  
  addParagraph(rtffile, "\\b \\fs20 Largest Sources of Sequestration in Zone \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
  addNewLine(rtffile, n=1)
  
  tb_seq_zon<-tb_seq_zonal[which(tb_seq_zonal$ZONE == a),]
  tb_seq_zon$ZONE<-NULL
  addTable(rtffile, tb_seq_zon)
  addNewLine(rtffile, n=1)
  
  #Largest Sequestration
  largestS.Z<-ggplot(data=tb_seq_zon, aes(x=reorder(LU_CODE, -seq), y=(seq))) + geom_bar(stat="identity", fill="green") +
    geom_text(data=tb_seq_zon, aes(x=LU_CODE, y=seq, label=round(seq, 1)),size=3, vjust=0.1) +
    ggtitle(paste("Largest Sequestration in Zone",i, "-", data_zone$Z_CODE[i] )) + guides(fill=FALSE) + ylab("CO2eq") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  png(filename=paste("Largest_Seq_Z_",a,".png", sep=""), 
      type="cairo",
      units="in", 
      width=6.7, 
      height=4,  
      res=125)
  print(largestS.Z)
  dev.off()
  
  z.seq.name<-c(z.seq.name, paste("Largest_Seq_Z_",a,".png", sep=""))
  
  addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestS.Z )
  addNewLine(rtffile, n=1)
  
}
rm(largestE.Z, largestS.Z)
done(rtffile)

LUMENS_GUI<-function(){
  require(gWidgets)
  options(guiToolkit="RGtk2")
  currentenv <- environment()
  MapViewer<-function(.,h,...){
    availMap <- c("Land Use 1", "Land Use 2","Zone","Average Zonal Carbon Density time 1","Average Zonal Carbon Density time 2","Average Zonal Emission","Average Zonal Sequestration", "Carbon Density time 1", "Carbon Density time 2", "Emission Map", "Sequestration Map")
    Mapdb<-cbind(availMap, file.name)
    
    updatePlot <- function(h,...) {
      x <- svalue(map_choice)
      plot.name<-Mapdb[which(Mapdb[,1]==x),]
      plot.select<-plot.name[2]
      img<-gimage(filename=paste(work_dir,"/",plot.select, sep=""))
      changeState <- function(h,...) {
        if(svalue(map_choice)!= x) {
          delete(group2, img)
        } 
      }
      addHandlerClicked(map_choice, handler=changeState)
      add(group2,img)
    }
    
    #Main Interface, 
    group <- ggroup(horizontal=TRUE)
    add(nb, group, label=("LUMENS QUES-C\nMap Viewer"))
    group1<- ggroup(horizontal=FALSE)
    add(group, group1)
    group2<- ggroup(horizontal=TRUE)
    add(group, group2)
    tmp <- gframe("Map Choice", container=group1, horizontal=FALSE)
    map_choice <- gradio(Mapdb[,1], horizontal=FALSE, handler=updatePlot)
    add(tmp, map_choice)
    
  }
  
  ChartViewer<-function(.,h,...){
    PU.lvl<-function(h,...){
      x<-svalue(Anl.lvl)
      
      if (x=="Landscape Level Chart"){
        #delete(tmp, Anl.lvl)
        #combo.box<-combo.box[2:3]
        #Anl.lvl <- gcombobox(combo.box, handler=PU.lvl)
        #delete(group1, tmp)
        #add(group1, tmp)
        #add(tmp, Anl.lvl)
        
        aVailChart<-c("Emission Rate", "Largest Emission", "Largest Sequestration")
        Chartdb<-cbind(aVailChart, chart.name)
        tmp3<-gframe("Select Chart", container=group1, horizontal=FALSE)
        
        updatePlot <- function(h,...) {
          x <- svalue(chart.list)
          plot.name<-Chartdb[which(Chartdb[,1]==x),]
          plot.select<-plot.name[2]
          img<-gimage(filename=paste(work_dir,"/",plot.select, sep=""), container=group2)
          
          changeState <- function(h,...) {
            if(svalue(chart.list)!= x || svalue(Anl.lvl)=="Planning Unit Level Chart") {
              delete(group2, img)
            } 
          }
          addHandlerClicked(chart.list, handler=changeState)
          addHandlerClicked(Anl.lvl, handler=changeState)
        }
        
        chart.list<-gcombobox(Chartdb[,1], selected=1, handler=updatePlot)
        add(tmp3, chart.list)
        
        
        changeState2 <- function(h,...) {
          if(svalue(Anl.lvl)=="Planning Unit Level Chart") {
            delete(tmp3, chart.list)
            delete(group1, tmp3)
          } 
        }

        addHandlerClicked(Anl.lvl, handler=changeState2)
        
      } else if (x=="Planning Unit Level Chart") {
        plot.list<-function(h,...){
          x<-svalue(PU.list)
          y<-Zonedb[which(Zonedb[,1]==x),]
          aVailChart<-c("Largest Emission", "Largest Sequestration")
          Chartdb<-cbind(aVailChart, y[2:3])
          tmp3<-gframe("Select Chart", container=group1)
          
          updatePlot <- function(h,...) {
            a <- svalue(chart.list)
            plot.name<-Chartdb[which(Chartdb[,1]==a),]
            plot.select<-plot.name[2]
            img<-gimage(filename=paste(work_dir,"/",plot.select, sep=""), container=group2)
            
            changeState <- function(h,...) {
              if(svalue(chart.list)!= a || svalue(PU.list)!=x || svalue(Anl.lvl)=="Landscape Level Chart") {
                delete(group2, img)
              } 
            }
            addHandlerClicked(chart.list, handler=changeState)
            addHandlerClicked(Anl.lvl, handler=changeState)
            addHandlerClicked(PU.list, handler=changeState)
          }
          
          chart.list<-gcombobox(Chartdb[,1], selected=1, handler=updatePlot)
          svalue(chart.list)<-as.character(Chartdb[1,1])
          add(tmp3, chart.list)
          
          changeState2 <- function(h,...) {
            if(svalue(Anl.lvl)=="Landscape Level Chart" || svalue(PU.list) != x) {
              delete(tmp3, chart.list)
              delete(group1, tmp3)
            } 
          }
          addHandlerClicked(Anl.lvl, handler=changeState2)
          addHandlerClicked(PU.list, handler=changeState2)
        }
        Zonedb<-cbind(as.character(area_zone$Z_NAME), z.emission.name, z.seq.name)
        tmp2<-gframe("Select Planning Unit", container=group1)
        PU.list<-gcombobox(as.character(area_zone$Z_NAME), selected=1, handler=plot.list)
        add(tmp2,PU.list)
        changeState <- function(h,...) {
          if(svalue(Anl.lvl)=="Landscape Level Chart") {
            delete(tmp2, PU.list)
            delete(group1, tmp2)
          } 
        }
        addHandlerClicked(Anl.lvl, handler=changeState)
      }
    }
    #Main Interface
    group <- ggroup(horizontal=TRUE)
    add(nb, group, label=("LUMENS QUES-C\nChart Viewer"))
    group1<- ggroup(horizontal=FALSE)
    add(group, group1)
    group2<- ggroup(horizontal=TRUE)
    add(group, group2)
    tmp <- gframe("Chart Choice", container=group1)
    combo.box<-c("Landscape Level Chart","Planning Unit Level Chart")
    Anl.lvl <- gcombobox(combo.box, handler=PU.lvl)
    add(tmp, Anl.lvl)
  }
  
  TableViewer<-function(.,h,...){
    PU.lvl<-function(h,...){
      x<-svalue(Anl.lvl)
      if (x=="Landscape Level Table"){
        aVailTable<-c("Overall Emission Rate", "Planning Unit Overall Emission Rate", "Largest Source of Emission", "Largest Source of Sequestration")
        table.2<-cbind(data_zone, zone_carbon[,4:7])
        Table.var<-c("fs_table", "table.2", "tb_em_total_10", "tb_seq_total_10")
        Table.db<-cbind(aVailTable, Table.var)
        tmp3<-gframe("Select Table", container=group1, horizontal=FALSE)
        
        updateTab <- function(h,...) {
          x <- svalue(table.list)
          table.name<-Table.db[which(Table.db[,1]==x),]
          table.select<-table.name[2]
          tbl<-gtable(get(table.select), container=group2, expand=TRUE, fill=TRUE)
          
          changeState <- function(h,...) {
            if(svalue(table.list)!= x || svalue(Anl.lvl)=="Planning Unit Level Table") {
              delete(group2, tbl)
            } 
          }
          addHandlerClicked(table.list, handler=changeState)
          addHandlerClicked(Anl.lvl, handler=changeState)
        }
        
        table.list<-gcombobox(Table.db[,1], selected=1, handler=updateTab)
        svalue(table.list)<-as.character(Table.db[1,1])
        add(tmp3, table.list)
        
        changeState2 <- function(h,...) {
          if(svalue(Anl.lvl)=="Planning Unit Level Table") {
            delete(tmp3, table.list)
            delete(group1, tmp3)
          } 
        }
        addHandlerClicked(Anl.lvl, handler=changeState2)
        
      } else {
        tab.list<-function(h,...){
          x<-svalue(PU.list)
          y<-area_zone[which(area_zone$Z_NAME==x),1]
          aVailTable<-c("Largest Source of Emission", "Largest Source of Sequestration")
          tb_em_zon<-tb_em_zonal[which(tb_em_zonal$ZONE == y),]
          tb_em_zon$ZONE<-NULL
          tb_seq_zon<-tb_seq_zonal[which(tb_seq_zonal$ZONE == y),]
          tb_seq_zon$ZONE<-NULL
          table.var<-c("tb_em_zon", "tb_seq_zon")
          Table.db<-cbind(aVailTable, table.var)
          tmp3<-gframe("Select Table", container=group1)
          
          updateTab <- function(h,...) {
            a <- svalue(table.list)
            table.name<-Table.db[which(Table.db[,1]==a),]
            table.select<-table.name[2]
            tbl<-gtable(get(table.select), container=group2, expand=TRUE, fill=TRUE, multiple=TRUE)
            
            changeState <- function(h,...) {
              if(svalue(table.list)!= a || svalue(PU.list)!=x || svalue(Anl.lvl)=="Landscape Level Table") {
                delete(group2, tbl)
              } 
            }
            addHandlerClicked(table.list, handler=changeState)
            addHandlerClicked(Anl.lvl, handler=changeState)
            addHandlerClicked(PU.list, handler=changeState)
          }
          
          table.list<-gcombobox(Table.db[,1], selected=1, handler=updateTab)
          svalue(table.list)<-as.character(Table.db[1,1])
          add(tmp3, table.list)
          
          changeState2 <- function(h,...) {
            if(svalue(Anl.lvl)=="Landscape Level Table" || svalue(PU.list) != x) {
              delete(tmp3, table.list)
              delete(group1, tmp3)
            } 
          }
          addHandlerClicked(Anl.lvl, handler=changeState2)
          addHandlerClicked(PU.list, handler=changeState2)
        }
        
        tmp2<-gframe("Select Planning Unit", container=group1)
        PU.list<-gcombobox(as.character(area_zone$Z_NAME), selected=1, handler=tab.list)
        add(tmp2,PU.list)
        
        changeState <- function(h,...) {
          if(svalue(Anl.lvl)=="Landscape Level Table") {
            delete(tmp2, PU.list)
            delete(group1, tmp2)
          } 
        }
        addHandlerClicked(Anl.lvl, handler=changeState)
      }
    }
    #Main Interface
    group <- ggroup(horizontal=TRUE)
    add(nb, group, label=("LUMENS QUES-C\nTable Viewer"))
    group1<- ggroup(horizontal=FALSE)
    add(group, group1)
    group2<- ggroup(horizontal=TRUE,expand=TRUE, fill=TRUE)
    add(group, group2, expand=TRUE, fill=TRUE)
    tmp <- gframe("Table Choice", container=group1)
    Anl.lvl <- gcombobox(c("Landscape Level Table","Planning Unit Level Table"), handler=PU.lvl)
    add(tmp, Anl.lvl)
  }
  
  func.quit<-function(h,...){
    dispose(LUMENS.GUI)
    assign("myfunc", TRUE, envir=currentenv)
  }
  
  myfunc <- FALSE
  LUMENS.GUI <- gwindow("LUMENS QUES-C Result Viewer", horizontal=FALSE)
  statusBar = gstatusbar(text=("Ready."),cont=LUMENS.GUI)
  BigGroup <- ggroup(container=LUMENS.GUI)
  nb <- gnotebook(tab.pos=2,closebuttons=TRUE)
  add(BigGroup,nb, expand=TRUE)
  
  #Menu Action
  LClose    = gaction(label="Quit",icon="quit", handler=func.quit)
  LMaps     = gaction(label=("QUES-C Map Viewer"), handler=MapViewer)
  LChart  = gaction(label=("QUES-C Chart Viewer"), handler=ChartViewer)
  LTable  = gaction(label=("QUES-C Table Viewer"), handler=TableViewer)
  # Menu tree
  tmp = list(Session = list(Quit=LClose),
             Modules = list("Map Viewer" = LMaps, "Chart Viewer" = LChart, "Table Viewer"=LTable))
  
  menu = gmenu(tmp,cont=LUMENS.GUI)
  
  addHandlerDestroy(LUMENS.GUI, handler = function( h,...){ assign("myfunc", TRUE, envir=currentenv) } )
  
  while(myfunc == FALSE){
    Sys.sleep(1)
  }
  
}  

LUMENS_GUI()
