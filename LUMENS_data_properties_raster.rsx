##Alpha - DATABASE=group
##data_file=raster
##data_table=output table
##passfilenames

raster_temp<-raster(data_file)
data_table<-as.data.frame(freq(raster_temp))
data_table<-data_table[order(data_table$value),]
colnames(data_table)[1]<-"ID"
data_table[2]<-NULL
data_table<-na.omit(data_table)
