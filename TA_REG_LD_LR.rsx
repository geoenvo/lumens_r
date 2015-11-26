##[TA]=group
##work_dir=folder
##landuse_file=raster
##int_con_file=file
##add_val_file=file
##fin_dem_file=file
##add_val_struc_file=file
##fin_dem_struc_file=file
##sector_file=file
##land.distribution_file=file
##lc.list_file=file
##labour_file=file
##unit=string
##area_name=string
##I_O_period=number 2000
##passfilenames

library(reshape2)
library(ggplot2)
library(raster)
library(foreign)
library(rtf)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#SET WORKING DIRECTORY
setwd(work_dir)

#WRITING TA PROJECT FILE
filename<-paste("TA_projec_", eval(parse(text=(paste("Sys.Date ()")))), ".lms", sep="")
date<-Sys.Date()
sink(filename)
cat("LUMENS TA Module Project File")
cat("\n")
cat(as.character(date))
cat("\n")
cat(landuse_file)
cat("\n")
cat(int_con_file)
cat("\n")
cat(int_con_file)
cat("\n")
cat(add_val_file)
cat("\n")
cat(labour_file)
cat("\n")
cat(fin_dem_file)
cat("\n")
cat(add_val_struc_file)
cat("\n")
cat(fin_dem_struc_file)
cat("\n")
cat(sector_file)
cat("\n")
cat(land.distribution_file)
cat("\n")
cat(lc.list_file)
cat("\n")
cat(unit)
cat("\n")
cat(area_name)
cat("\n")
cat(I_O_period)
sink()

#READ INPUT FILE
nodata_val<-0
landuse <- raster(landuse_file)
int_con<- read.table(int_con_file, header=FALSE, sep=",",)
add_val<- read.table(add_val_file, header=FALSE, sep=",",)
fin_dem<- read.table(fin_dem_file, header=FALSE, sep=",",)
fin_dem_struc<- read.table(fin_dem_struc_file, header=FALSE, sep=",",)
add_val_struc<- read.table(add_val_struc_file, header=FALSE, sep=",",)
sector<- read.table(sector_file, header=FALSE, sep=",",)
labour<- read.table(labour_file, header=FALSE, sep=",",)
land.distribution<-read.table(land.distribution_file, header=FALSE, sep=",",)
lc.list<-read.table(lc.list_file, header=FALSE, sep=",",)
int_con.m<-as.matrix(int_con)
add_val.m<-as.matrix(add_val)
dim<-ncol(int_con.m)

#CALCULATE INVERS LEONTIEF
int_con.ctot<-colSums(int_con.m)
add_val.ctot<-colSums(add_val.m)
fin_con<- 1/(int_con.ctot+add_val.ctot)
t.input.invers<-diag(fin_con)
A<-int_con.m %*% t.input.invers
I<-as.matrix(diag(dim))
I_A<-I-A
Leontief<-solve(I_A)

#GDP
GDP.val<-as.data.frame(add_val.m[2,]+add_val.m[3,])
GDP.val.m<-as.matrix(GDP.val)
GDP.val.m<-as.numeric(GDP.val.m)
OUTPUT.val<-as.data.frame(add_val.m[2,]+add_val.m[3,]+add_val.m[1,]+int_con.ctot)
OUTPUT.val.m<-as.matrix(OUTPUT.val)
OUTPUT.val.m<-as.numeric(OUTPUT.val.m)
GDP<-cbind(sector,GDP.val,OUTPUT.val)
colnames(GDP)[1] <- "SECTOR"
colnames(GDP)[2] <- "SECTOR CLASS"
colnames(GDP)[3] <- "GDP"
colnames(GDP)[4] <- "OUTPUT"
GDP$GDP_PROP<-GDP$GDP/GDP$OUTPUT
order_GDP <- as.data.frame(GDP[order(-GDP$GDP),])
order_GDP10<-head(order_GDP,n=20)
GDP_tot<-as.matrix(GDP$GDP)
GDP_tot<-colSums(GDP_tot)

#LINK LAND SISTRIBUTION FILE WITH LAND USE MAP
#Read land use map and calculate area of land use distribution matrix
landuse<-ratify(landuse, filename='landuse.grd',count=TRUE,overwrite=TRUE)
landuse_area<-as.data.frame(levels(landuse))
landuse_area<-subset(landuse_area,ID !=nodata_val)
landuse_area<-as.matrix(landuse_area$COUNT)
land.distribution_t<-as.matrix(land.distribution)
landuse_area_diag<-diag(as.numeric(as.matrix(landuse_area)))
land.distribution.val<-land.distribution_t %*% landuse_area_diag

#CALCULATE LAND DISTRIBUTION COEFFICIENT MATRIX
land.distribution.ctot<-colSums(land.distribution.val)
land.distribution.rtot<-rowSums(land.distribution.val)
land.distribution.prop<-land.distribution.val %*% diag(1/land.distribution.ctot)
land.distribution.prop.r<-t(land.distribution.val) %*% diag(1/land.distribution.rtot)
land.requirement<-rowSums(land.distribution.val)
fin_dem.rtot<-rowSums(fin_dem)
int_con.rtot<-rowSums(int_con)
demand<-fin_dem.rtot+int_con.rtot
land.requirement.coeff<-land.requirement/demand
land.productivity.coeff<-land.requirement/fin_dem.rtot

#PRODUCE OUTPUT
land.requirement_table<-as.data.frame(land.requirement)
land.requirement.tot<-sum(land.requirement)
land.requirement_table_prop<-as.data.frame(land.requirement/land.requirement.tot)
land.requirement_table<-cbind(sector,land.requirement_table,land.requirement_table_prop,demand,fin_dem.rtot,land.requirement.coeff, land.productivity.coeff)
colnames(land.requirement_table)[1] <- "SECTOR"
colnames(land.requirement_table)[2] <- "CATEGORY"
colnames(land.requirement_table)[3] <- "LR"
colnames(land.requirement_table)[4] <- "LR_PROP"
colnames(land.requirement_table)[5] <- "OUTPUT"
colnames(land.requirement_table)[6] <- "DEMAND"
colnames(land.requirement_table)[7] <- "LRC"
colnames(land.requirement_table)[8] <- "LPC"
land.requirement_table$LR<-round(land.requirement_table$LR)
land.requirement_table$LR_PROP<-round(land.requirement_table$LR_PROP, digits=2)
land.requirement_table$OUTPUT<-round(land.requirement_table$OUTPUT)
land.requirement_table$DEMAND<-round(land.requirement_table$DEMAND)
land.requirement_table$LRC<-round(land.requirement_table$LRC, digits=2)
land.requirement_table$LPC<-round(land.requirement_table$LPC, digits=2)

#land.requirement_table$LRC<-round(land.requirement_table$LRC, digits=2)-->catasthropic error !!!!
#land.requirement_table$LPC<-round(land.requirement_table$LPC, digits=2)-->catasthropic error !!!!
order_land.requirement <- as.data.frame(land.requirement_table[order(-land.requirement_table$LRC),])
order_land.requirement <-head(order_land.requirement,n=20)
LRC_graph<-ggplot(data=order_land.requirement, aes(x=SECTOR, y=LRC, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Land requirement coefficient") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6),
axis.text.x = element_text(size = 8))

#EXPORT OUTPUT
land_distribution_file<-"Land_distribution_matrix.dbf"
write.dbf(land.distribution.prop, land_distribution_file,  factor2char = TRUE, max_nchar = 254)
land_requirement_file<-"Land_requirement_coefficient.dbf"
write.dbf(land.requirement_table, land_requirement_file, factor2char = TRUE, max_nchar = 254)

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Trade-off Analysis (TA) Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 2: Regional economic-Land Requirement Coefficient\\b0\\fs20"
date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1<-"\\b\\fs24 Land Requirement Coefficient \\b0\\fs20"
rtffile <- RTF("LUMENS_TA-3_report.lpr", font.size=9)
if (file.exists("C:/Program Files (x86)/LUMENS")){
addPng (rtffile, "C:/Program Files (x86)/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
} else{
addPng (rtffile, "C:/Program Files/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
}
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
addParagraph(rtffile, "\\b\\fs20 Table 1. Land requirement\\b0\\fs20.")
addTable(rtffile,land.requirement_table,font.size=7.5)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,LRC_graph)
done(rtffile)
