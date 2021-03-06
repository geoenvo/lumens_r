##Alpha - DATABASE=group
##project=string (enter name of the project)
##working_directory=folder
##description=string
##location=string (enter location)
##province=string (enter province name of your location)
##country=string (enter country name)
##admin_attribute=vector
##field_attribute=field admin_attribute
##spat_res=number 50
##dissolve_table=file
##statusoutput=output table

#=Load library
library(rtf)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(rgeos)
library(grid)
library(jsonlite)

#=Set time start
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#=Create structure folder for LUMENS project 
setwd(working_directory)
project<-str_replace_all(string=project, pattern=" ", repl="_")
LUMENS_path <- paste(working_directory, "/", project, sep="")
PUR_path <- paste(LUMENS_path, "/PUR", sep="")
QUES_path <- paste(LUMENS_path, "/QUES", sep="")
PreQUES_path <- paste(QUES_path, "/PreQUES", sep="")
QUESC_path <- paste(QUES_path, "/QUES-C", sep="")
QUESB_path <- paste(QUES_path, "/QUES-B", sep="")
QUESH_path <- paste(QUES_path, "/QUES-H", sep="")
TA_path <- paste(LUMENS_path, "/TA", sep="")
SCIENDO_path  <- paste(LUMENS_path, "/SCIENDO", sep="")
DATA_path  <- paste(LUMENS_path, "/DATA", sep="")
#help_path  <- paste(LUMENS_path, "/help", sep="")
dir.create(LUMENS_path, mode="0777")
dir.create(PUR_path, mode="0777")
dir.create(QUES_path, mode="0777")
dir.create(PreQUES_path, mode="0777")
dir.create(QUESC_path, mode="0777")
dir.create(QUESB_path, mode="0777")
dir.create(QUESH_path, mode="0777")
dir.create(TA_path, mode="0777")
dir.create(SCIENDO_path, mode="0777")
dir.create(DATA_path, mode="0777")
#dir.create(help_path, mode="0777")

#This variables only to find out the identity of user who create database for the first time 
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS", sep="") 

#=Set reference data
# save as temporary data to DATA directory
setwd(DATA_path)
writeOGR(admin_attribute, dsn=DATA_path, "ref", overwrite_layer=TRUE, driver="ESRI Shapefile")
# rasterizing the polygon data of reference (e.g administrative, such as district or province boundary map) using gdal_rasterize
shp_dir<-paste(DATA_path,"/", "ref.shp", sep="")
file_out<-paste(DATA_path, "/", "ref.tif", sep="")
res<-spat_res
if (file.exists("C:/Program Files (x86)/LUMENS/bin/gdal_rasterize.exe")){
  gdalraster = "C:/Progra~2/LUMENS/bin/gdal_rasterize.exe "
} else{
  gdalraster = "C:/Progra~1/LUMENS/bin/gdal_rasterize.exe "
}
osgeo_comm<-paste(gdalraster, shp_dir, file_out,"-a IDADM -tr", res, res, "-a_nodata 255 -ot Byte", sep=" ")
system(osgeo_comm)
# create an initial coverage reference for LUMENS project
ref<-brick(file_out)
ref<-ref*1
names(ref)<-"Administrative maps"
Ref.name<-names(ref)
Ref.type<-class(ref)
Ref.coord<-as.character(crs(ref))
Ref.res<-res(ref)
Ref.xmin<-xmin(ref)
Ref.xmax<-xmax(ref)
Ref.ymin<-ymin(ref)
Ref.ymax<-ymax(ref)
cov.desc1<-c("Reference name","Reference class", "Reference CRS", "Reference Resolution", "Xmin", "Xmax", "Ymin", "Ymax")
cov.desc2<-as.data.frame(rbind(Ref.name, Ref.type, Ref.coord, Ref.res, Ref.xmin, Ref.xmax, Ref.ymin, Ref.ymax))
cov.desc2<-cov.desc2[1]
cov.desc<-cbind(cov.desc1,cov.desc2)
colnames(cov.desc)[1]<-"Coverage"
colnames(cov.desc)[2]<-"Description"
# load reference attribute from csv dissolve table 
p.admin.df<-read.table(dissolve_table, header=TRUE, sep=",")
colnames(p.admin.df)[2]="ADMIN_UNIT"
# unlink shapefile and raster
unlink(list.files(pattern = "ref"))

#=Set all values, functions, and initial indices to zero, for each index serves as a counter
# e.g landuse.index serve as a counter of landuse numbers
db_name<-paste(project, ".lpj", sep="")
landuse.index=0
pu.index=0
pu_rec.index=0
factor.index=0
lut.index=0
lut_carbon.index=0
lut_landuse.index=0
lut_zone.index=0
period.index=0
PUR.index=0
PreQUES.index=0
QUESC.index=0
QUESB.index=0
QUESH.index=0
SCIENDO1.index=0
SCIENDO2.index=0
TA1.index=0
TA2.index=0
ref.index=1
admin.index=1
# getting an information of windows architecture through the path of LUMENS installation 
if (file.exists("C:/Program Files (x86)/LUMENS")){
  win.arch = "32bit"
  processing.path = "C:/Progra~2/LUMENS/apps/qgis/python/plugins/processing/r/scripts/"
} else{
  win.arch = "64bit"
  processing.path = "C:/Progra~1/LUMENS/apps/qgis/python/plugins/processing/r/scripts/"
}
# prepare some functions and store it to LUMENS project file, so it can be used later
# RESAVE function
resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}
# GET_FROM_RDB function
get_from_rdb <- function(symbol, filebase, envir =parent.frame()){
  lazyLoad(filebase = filebase, envir = envir, filter = function(x) x == symbol)
}
# write project properties into table
proj_descr <- as.data.frame(rbind(project, description, working_directory, location, province, country))
test<-c(rownames(proj_descr))
proj_descr<-cbind(test, proj_descr)
colnames(proj_descr)[1]<-"Type"
colnames(proj_descr)[2]<-"Description"
proj_descr<-as.data.frame(proj_descr)
proj.file<-paste(LUMENS_path, "/",project,".lpj", sep="")

#=Save all params into .RData objects
save(LUMENS_path_user,
landuse.index,
proj_descr,
ref,
p.admin.df,
location,
province,
country,
ref.index,
admin.index,
cov.desc,
pu.index,
pu_rec.index,
factor.index,
lut.index,
lut_carbon.index,
lut_landuse.index,
lut_zone.index,
period.index,
PUR.index,
PreQUES.index,
QUESC.index,
QUESB.index,
QUESH.index,
SCIENDO1.index,
SCIENDO2.index,
TA1.index,
TA2.index,
win.arch,
processing.path,
resave, 
get_from_rdb,
file=proj.file)
# write the properties of reference data to csv lookup table
csv_file<-paste(DATA_path,"/csv_planning_unit.csv", sep="")
eval(parse(text=(paste("list_of_data_pu<-data.frame(RST_DATA='ref', RST_NAME=names(ref), LUT_NAME='p.admin.df', row.names=NULL)", sep=""))))
write.table(list_of_data_pu, csv_file, quote=FALSE, row.names=FALSE, sep=",")
# save it 
save(list_of_data_pu, ref, p.admin.df, file=paste(DATA_path, "/planning_unit", sep=""))

#=Create QGIS project (.qgs)
qgsproject<-paste(DATA_path, "/", project, ".qgs", sep="")
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

#=Create LUMENS Project Report (.lpr)
# arrange numerous colors with RColorBrewer
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
# create an Rplot for reference map
myColors.lu <- myColors[1:(length(unique(p.admin.df$IDADM))+1)]
ColScale.lu<-scale_fill_manual(name=field_attribute, breaks=c(0, p.admin.df$IDADM), labels=c("NoData", as.character(p.admin.df$ADMIN_UNIT)), values=myColors.lu)
plot.admin<-gplot(ref, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu + theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=10),
         legend.text = element_text(size=10),
         legend.key.height = unit(0.35, "cm"),
         legend.key.width = unit(0.35, "cm"))
# write report   
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
addParagraph(rtffile, paste("\\cf4\\b \\fs20 Peta batas administrasi\\b \\fs20\\cf1", sep=" "))
addPlot(rtffile,plot.fun=print, width=6,height=4.5,res=150,  plot.admin)
addNewLine(rtffile)
done(rtffile)
# show result via shell command 
command<-paste("start ", "winword ", DATA_path, "/LUMENS_Create-Project_report.lpr", sep="" )
shell(command)

#=Create HTML result (.html)
# save reference map to png picture format
png("admin.png")
plot(plot.admin)
# formatting table to json and save to variables  
row.names(proj_descr)<-NULL
json_proj_descr_col<-toJSON(colnames(proj_descr))
json_proj_descr<-toJSON(proj_descr)
row.names(cov.desc)<-NULL
json_cov.desc_col<-toJSON(colnames(cov.desc))
json_cov.desc<-toJSON(cov.desc)
# copy d3.v3.min.jd to DATA directory
file.copy(paste(processing.path, "/d3.v3.min.js", sep=""), DATA_path)
# write to HTML
htmlproject<-paste(DATA_path, "/", project, ".html", sep="")
sink(htmlproject)
# set header
cat("<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01//EN' 'http://www.w3.org/TR/html4/strict.dtd'>")
cat("<html lang='en'><head><meta charset='utf-8'/><script src='d3.v3.min.js' charset='utf-8'></script><style>")
cat("td, th { padding: 2px 4px; }")
cat("th { font-weight: bold;border-bottom: 1px solid #000;border-top: 1px solid #000;}")
cat("tr:last-child td{ border-bottom: 1px solid #000; }</style><title>LUMENS Result Viewer</title></head>")
# fill the html body
cat("<body><h1><strong><font style='color:rgb(255,0,0);'>L</font><font style='color:rgb(146,208,80);'>U</font><font style='color:rgb(0,176,240);'>M</font><font style='color:rgb(140,175,71);'>E</font><font style='color:rgb(0,112,192);'>N</font><font style='color:rgb(79,98,40);'>S</font> REPORT</strong><//h1>
    <p>LUMENS Project for", location,", ", province, ", ", country, "</p><div id='container'></div><img src='admin.png' />")
# footer with javascript using D3.js
cat("<script>
function tabulate(data, columns) {
var table = d3.select('#container').append('table'),
thead = table.append('thead'),
tbody = table.append('tbody');

thead.append('tr').selectAll('th').data(columns).enter().append('th').text(function(column) { return column; });

var rows = tbody.selectAll('tr').data(data).enter().append('tr');

var cells = rows.selectAll('td')
.data(function(row) {
  return columns.map(function(column) {
    return {column: column, value: row[column]};
  });
}).enter().append('td').text(function(d) { return d.value; });
return table;
}
")
cat("var projDescr =", json_proj_descr, ";
    var projDescrTable = tabulate(projDescr,", json_proj_descr_col, ");")
cat("var covDesc =", json_cov.desc, ";
    var covDescTable = tabulate(covDesc,", json_cov.desc_col, ");")
cat("</script></body></html>")
sink()

#=Writing final status message (code, message)
statuscode<-1
statusmessage<-"LUMENS database has been created!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)

