##Alpha - DATABASE=group
##proj.file=file
##p.admin.df=file
##statusoutput=output table

library(rtf)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(rgeos)
library(grid)

load(proj.file)
LUMENS_path<-dirname(proj.file)
project<-as.character(proj_descr[1,2])
setwd(dirname(proj.file))

p.admin.df<-read.table(p.admin.df, header=TRUE, sep=",")

colnames(p.admin.df)[1]="ADMIN_UNIT"

myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- rev(brewer.pal(11, "RdYlGn"))
myColors8 <- "#000000"
myColors9 <- brewer.pal(12, "Set3")

if (0 %in% p.admin.df$IDADM){
  myColors  <-c(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
} else {
  myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
}

field_attribute<-names(p.admin.df)[2]
myColors.lu <- myColors[1:(length(unique(p.admin.df$IDADM))+1)]
ColScale.lu<-scale_fill_manual(name=field_attribute, breaks=c(0, p.admin.df$IDADM), labels=c("NoData", as.character(p.admin.df$ADMIN_UNIT)), values=myColors.lu)
plot3<-gplot(ref, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu + theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=10),
         legend.text = element_text(size=10),
         legend.key.height = unit(0.35, "cm"),
         legend.key.width = unit(0.35, "cm"))

setwd(LUMENS_path)

pu_pu1<-ref
names(pu_pu1)<-"Administrative maps"

resave(p.admin.df,
     pu_pu1,
     file=proj.file)

#CREATE QGIS PROJECT
qgsproject<-paste(LUMENS_path, "/", project, ".qgs", sep="")
sink(qgsproject)
cat("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>")
cat('<qgis projectname="" version="2.0.0-Taoge">')
cat('<title></title>')
cat('<mapcanvas>')
cat('<units>degrees</units>')
cat('<extent>')
cat('<xmin>0</xmin>')
cat('<ymin>0</ymin>')
cat('<xmax>0</xmax>')
cat('<ymax>0</ymax>')
cat('</extent>')
cat('<projections>0</projections>')
cat('<destinationsrs>')
cat('<spatialrefsys>')
cat('<proj4>+proj=longlat +datum=WGS84 +no_defs</proj4>')
cat('<srsid>3452</srsid>')
cat('<srid>4326</srid>')
cat('<authid>EPSG:4326</authid>')
cat('<description>WGS 84</description>')
cat('<projectionacronym>longlat</projectionacronym>')
cat('<ellipsoidacronym>WGS84</ellipsoidacronym>')
cat('<geographicflag>true</geographicflag>')
cat('</spatialrefsys>')
cat('</destinationsrs>')
cat('</mapcanvas>')
cat('<legend updateDrawingOrder="true"/>')
cat('<mapcanvas>')
cat('<units>degrees</units>')
cat('<extent>')
cat('<xmin>0</xmin>')
cat('<ymin>0</ymin>')
cat('<xmax>0</xmax>')
cat('<ymax>0</ymax>')
cat('</extent>')
cat('<projections>0</projections>')
cat('<destinationsrs>')
cat('<spatialrefsys>')
cat('<proj4>+proj=longlat +datum=WGS84 +no_defs</proj4>')
cat('<srsid>3452</srsid>')
cat('<srid>4326</srid>')
cat('<authid>EPSG:4326</authid>')
cat('<description>WGS 84</description>')
cat('<projectionacronym>longlat</projectionacronym>')
cat('<ellipsoidacronym>WGS84</ellipsoidacronym>')
cat('<geographicflag>true</geographicflag>')
cat('</spatialrefsys>')
cat('</destinationsrs>')
cat('</mapcanvas>')
cat('<projectlayers layercount="0"/>')
cat('<properties>')
cat('<SpatialRefSys>')
cat('<ProjectCrs type="QString">EPSG:4326</ProjectCrs>')
cat('</SpatialRefSys>')
cat('<Paths>')
cat('<Absolute type="bool">false</Absolute>')
cat('</Paths>')
cat('<Gui>')
cat('<SelectionColorBluePart type="int">0</SelectionColorBluePart>')
cat('<CanvasColorGreenPart type="int">255</CanvasColorGreenPart>')
cat('<CanvasColorRedPart type="int">255</CanvasColorRedPart>')
cat('<SelectionColorRedPart type="int">255</SelectionColorRedPart>')
cat('<SelectionColorAlphaPart type="int">255</SelectionColorAlphaPart>')
cat('<SelectionColorGreenPart type="int">255</SelectionColorGreenPart>')
cat('<CanvasColorBluePart type="int">255</CanvasColorBluePart>')
cat('</Gui>')
cat('<PositionPrecision>')
cat('<DecimalPlaces type="int">2</DecimalPlaces>')
cat('<Automatic type="bool">true</Automatic>')
cat('</PositionPrecision>')
cat('</properties>')
cat('</qgis>')
sink()

#WRITE REPORT
title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1REPORT \\par\\b0\\fs20\\ql\\cf1"
title2<-paste("\\pard\\qr\\b\\fs40\\cf1 Create LUMENS Project ", "for ", location, ", ", province, ", ", country, "\\par\\b0\\fs20\\ql\\cf1", sep="")
sub_title<-"\\cf2\\b\\fs32 Ringkasan Deskripsi Projek\\cf1\\b0\\fs20"
chapter1<-"\\cf2\\b\\fs28 Deskripsi Projek \\cf1\\b0\\fs20"
chapter2<-"\\cf2\\b\\fs28 Cakupan Geografis Projek \\cf1\\b0\\fs20"
chapter3<-"\\cf2\\b\\fs28 Data-data Acuan Dalam Projek \\cf1\\b0\\fs20"
#time_start<-paste("Proses LUMENS dimulai : ", time_start, sep="")
time_end<-paste("Proses LUMENS selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("-------------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
rtffile <- RTF("LUMENS_Create-Project_report.lpr", font.size=9)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, title1)
addParagraph(rtffile, title2)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addTable(rtffile,proj_descr,font.size=8,col.widths=width)
addPageBreak(rtffile)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, paste("Selamat datang di LUMENS!!. Anda telah berhasil menyusun konfigurasi data-data awal yang akan digunakan dalam perencanaan penggunaan lahan yang mempertimbangkan berbagai fungsi lingkungan. LUMENS project file terdiri dari dua file utama dengan akhiran .lpj dan lpd. Project file yang telah anda buat bernama ", project, ".lpj."))
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Deskripsi projek menyimpan informasi umum yang anda masukkan mengenai projek ini")
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addTable(rtffile,proj_descr,font.size=8,col.widths=width)
addNewLine(rtffile)
addParagraph(rtffile, chapter2)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Cakupan geografis projek menyimpan informasi mengenai cakupan area yang akan digunakan di dalam project, batas-batas koordinat, sistem projeksi serta resolusi spasial yang akan digunakan dalam projek")
addNewLine(rtffile)
addTable(rtffile,cov.desc,font.size=8,col.widths=width)
addNewLine(rtffile)
addPageBreak(rtffile)
addParagraph(rtffile, chapter3)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Berikut ini adalah beberapa data yang akan dijadikan data acuan dalam projek ini")
addNewLine(rtffile)
#addParagraph(rtffile, paste("\\cf4\\b \\fs20 Peta Acuan Dalam Format Raster\\b \\fs20\\cf1", sep=" "))
#addPlot(rtffile,plot.fun=print, width=5,height=3.5,res=150,  plot3)
#addNewLine(rtffile)
addParagraph(rtffile, paste("\\cf4\\b \\fs20 Peta batas administrasi\\b \\fs20\\cf1", sep=" "))
addPlot(rtffile,plot.fun=print, width=6,height=4.5,res=150,  plot3)
addNewLine(rtffile)
done(rtffile)

command<-paste("start ", "winword ", LUMENS_path, "/LUMENS_Create-Project_report.lpr", sep="" )
shell(command)
#CLEAN ENVIRONMENT
rm(list=ls(all.names=TRUE))

statuscode<-1
statusmessage<-"LUMENS database has been created!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
