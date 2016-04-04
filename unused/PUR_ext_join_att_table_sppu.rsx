##[PUR]=group
##wd=folder
##input_poly=vector
##merged_shp=output vector

library(sp)
library(rgdal)
library(foreign)

setwd (wd)

# PREPARE REFERENCE DATA
sa<-input_poly

#read and modify PUR table
input_table <- read.dbf(paste(wd,'/PUR_overlap_summary.dbf',sep=''))
colnames(input_table)<-c("ID","reconciled")
merged_shp<-merge(sa, input_table, by="ID")

#write joined shapefile
writeOGR(merged_shp, wd, "PUR_reconciled_planning_unit_sppu",driver="ESRI Shapefile", overwrite=TRUE)
