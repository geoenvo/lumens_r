##[PUR]=group
##WDir=folder
##PUR_spec_data=file
##rec_data=raster
##rec_lookup=file
##loc=string
#PUR_overlap=output table
##PUR2=output raster
##passfilenames

wd <- WDir
PUR_data <- PUR_spec_data
setwd (wd)


library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(PBSmapping)
library(gridExtra)
library(raster)
library(knitr)
library(ggplot2)
library(RColorBrewer)
library(markdown)
library(foreign)
library(plotrix)
library(rtf)


#1 load reconciled raster file and database
rec_data<-raster(rec_data)
freq(rec_data)
#read and add database
#levels(rec.map)<-read.dbf(rec_lookup)
rec_lookup<-read.dbf(rec_lookup)


#create reference map
ref<-rec_data
ref[is.na(ref)]<-0
#ext<-extent(rec.map)
#xy_sa <- abs(apply(as.matrix(bbox(rec.map)), 1, diff))
#n<-1000  # in meter, need to be modified if in degree 0.0009
#ref <- raster(ext, ncol=xy_sa[1]/n, nrow=xy_sa[2]/n)




#2 load special planning unit maps and database
datalist <- read.table(PUR_data, header=FALSE, sep=",")
file <- datalist[1]
alias <- datalist [2]
file.number <-nrow(file)
i=1
command1<-paste()
command2<-paste()
command3<-paste()
for (i in 1:file.number) {
test<-basename (as.character(file[i,]))
input <-substr(basename(test), 1, nchar(basename(test)) - 4)
wd_loop<-dirname(as.character(file[i,]))
eval(parse(text=(paste("pu_v",i,'<-readOGR("', wd_loop,'","', input,'")', sep=""))))
eval(parse(text=(paste("pu",i,'<-','rasterize(pu_v',i,',ref)',sep=""))))
eval(parse(text=(paste("pu", i, "<-deratify(pu", i, ",'", as.character(alias[i,]), "')", sep=""))))
eval(parse(text=(paste("pu", i, "_attribute<-levels(pu",i,")", sep=""))))
eval(parse(text=(paste("pu", i, "_attribute<-as.data.frame(pu", i, "_attribute)", sep=""))))
eval(parse(text=(paste("colnames(pu",i,'_attribute)<-c((paste("Var",i,sep="")),(as.character(alias[', i, ",])))", sep=""))))
eval(parse(text=(paste("pu", i, "[is.na(pu", i, ")]<-0", sep=""))))
if (i!=file.number) {
command1<-paste(command1,"pu", i, ",", sep="")
command2<-paste(command2,"pu",i,"[]",",",sep="")
command3<-paste(command3,"Var",i, ",", sep="")
} else {
command1<-paste(command1,"pu", i, sep="")
command2<-paste(command2,"pu",i,"[]",sep="")
command3<-paste(command3,"Var",i, sep="")
}
}


#3 merge reconciled planning unit and special planning unit
ref_attribute<-as.data.frame(na.omit((freq(rec_data))))
ref.number<-file.number+1
colnames(ref_attribute)<-c((paste("Var",ref.number,sep="")),"rec_reference")
PUR2<-rec_data # initial reference map
command1<-paste(command1,",rec_data",  sep="") # input command for stack input
command2<-paste( command2,",rec_data[]", sep="") # input command to overlay process
command3<-paste( command3,",Var", as.character(ref.number), sep="")# input command to generate unique id
eval(parse(text=(paste("PUR2[]<-as.integer(interaction(", command2, "))", sep="")))) #overlay process
#PUR2<-ratify (PUR2, filename='PUR2.grd', count=TRUE, overwrite=TRUE) ;# generate overlayed planning unit maps
eval(parse(text=(paste("PUR_stack2<-stack(", command1, ")", sep="")))) ;#stack planning unit maps
PUR_db2<-crosstab(PUR_stack2) ;#generate initial database
eval(parse(text=(paste( "PUR_db2<-transform(PUR_db2, unique_id=as.integer(interaction(", command3, ", drop=F)))", sep=""))));#generate unique id column
#PUR_db2<-PUR_db2[ which(PUR_db2$Freq > 0),] ;# filter 0 value
#plot(PUR2)


#add refernce lookup table and special lookup table
#merge refrence planning unit file
colnames(rec_lookup)<-c(paste("Var", as.character(ref.number), sep=""), "Ref_zone", "COUNT")
rec_lookup_mod<-rec_lookup
rec_lookup_mod$COUNT<-NULL
PUR_db2_mod<-merge(PUR_db2, rec_lookup_mod, by=paste("Var", as.character(ref.number), sep=""))





datalist<-as.data.frame(datalist)
command4<-paste()
m<-1
for (m in 1:file.number) {

eval(parse(text=(paste("colnames(pu",m,"_attribute)<-c('Var",m, "',as.character(datalist$V2[",m,"]))", sep=""))))
eval(parse(text=(paste("PUR_db2_mod<-merge(PUR_db2_mod, pu",m, "_attribute, by='Var",m,"', all.x=T)", sep=""))))

if (m!=file.number) {
col_name<-as.character(datalist$V2[m])
command4<-paste(command4,"PUR_db2_mod$", col_name, ",", sep="")
} else {
col_name<-as.character(datalist$V2[m])
command4<-paste(command4,"PUR_db2_mod$", col_name, sep="")
}
}

#merge special planning unit n
#concatenate
eval(parse(text=(paste("overlapping<-as.data.frame(interaction(PUR_db2_mod$Ref_zone,",command4,"),stringsAsFactors=F)", sep=""))))
colnames(overlapping)<-"end_result"
overlapping<-as.matrix(overlapping, stringsAsFactors=T)
overlapping[is.na(overlapping)]<-"change"
#overlapping<-data.frame(end.result=overlapping)
#View(overlapping)

PUR_db_out<-PUR_db2_mod
row.num<-nrow(PUR_db_out)

m<-1
for (m in 1:row.num) {
if (identical(overlapping[m],"change")) {
eval(parse(text=(paste("overlapping[",m,"]<-as.character(PUR_db_out$Ref_zone[",m,"])", sep=""))))
}}
PUR_db_out<-cbind(PUR_db_out, overlapping)
PUR_db_out<-PUR_db_out[ which(PUR_db_out$Freq > 0),]

#produce map and lookup table result
sppu_ref<-as.data.frame(cbind(PUR_db_out$unique_id, as.character(PUR_db_out$end_result)))
colnames(sppu_ref)<-c('value','overlapping')




PUR_overlap<-sppu_ref

#modify att table
colnames(PUR_overlap)[1]<-"ID"
#colnames(overlap_tab)<-c("ID","Rec_phase1")
#levels(PUR_overlap)<-overlap_tab
#PUR_overlap <- deratify(PUR_overlap)
#writeRaster(PUR_overlap, filename="PUR_overlap_area", format="GTiff", overwrite=TRUE)

#PUR_overlap2<-ratify(PUR_overlap, filename='PUR_overlap.grd',count=TRUE,overwrite=TRUE)
#levels(PUR_overlap)<-merge((levels(PUR_overlap)),levels(PUR_overlap2),by="ID")
#plot(PUR_overlap)



#produce map and lookup table result
write.dbf(PUR_overlap, "PUR_overlap_summary")
writeRaster(PUR2, filename="PUR_overlap_area", format="GTiff", overwrite=TRUE)

# Save PUR workspace
project.name<-paste("PUR_special_pu", loc,".lpd", sep='')
save.image(project.name)

#report
