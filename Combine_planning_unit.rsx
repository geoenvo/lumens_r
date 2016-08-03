##[PUR]=group

library(tcltk)
library(spatial.tools)
library(R.utils)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(rtf)

#READ LUMENS LOG FILE
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)

#====READ PLANNING UNIT DATA FROM LUMENS DATABASE====
data<-data.frame()
data<-rbind(data, c("ref", names(ref), "combine.pu0"))
colnames(data)[1]="Data"
colnames(data)[2]="Name"
colnames(data)[3]="PU"
data$Data<-as.character(data$Data)
data$Name<-as.character(data$Name)
data$PU<-as.character(data$PU)
combine.pu0<-p.admin.df
combine.pu0<-combine.pu0[,c(2,1)]
colnames(combine.pu0)[1]="ref"
colnames(combine.pu0)[2]="label"
#check existing peat
peat_exist<-FALSE
check_peat<-as.data.frame(as.character(ls(pattern="Peat")))
if(nrow(check_peat)!=0){
  colnames(check_peat)[1]="Data"
  check_peat$Data<-as.character(check_peat$Data)
  for(i in 1:nrow(check_peat)){
    eval(parse(text=(paste("check_peat$Name[", i, "]<-as.character(names(", check_peat[i,1], "))", sep="")))) 
    eval(parse(text=(paste("check_peat$PU[", i, "]<-'combine.pu", i, "'", sep="")))) 
  }
  data<-rbind(data, check_peat) #add peat
  combine.pu1<-data.frame(Peat_1=1, label="Gambut") #masih statis
  peat_exist<-TRUE
}
#check existing planning unit
check_pu<-as.data.frame(as.character(ls(pattern="pu_pu")))
if(nrow(check_pu)!=0){
  colnames(check_pu)[1]="Data"
  check_pu$Data<-as.character(check_pu$Data)
  for(i in 1:nrow(check_pu)){
    eval(parse(text=(paste("check_pu$Name[", i, "]<-as.character(names(", check_pu[i,1], "))", sep="")))) 
    if(peat_exist){
      eval(parse(text=(paste("combine.pu", i+1, "<-lut.pu", i, sep=""))))   
      eval(parse(text=(paste("colnames(combine.pu", i+1, ")[1]<-as.character(names(", check_pu[i,1], "))", sep=""))))  
      eval(parse(text=(paste("colnames(combine.pu", i+1, ")[2]<-'label'", sep=""))))
      eval(parse(text=(paste("check_pu$PU[", i, "]<-'combine.pu", i+1, "'", sep="")))) 
    } else {
      eval(parse(text=(paste("combine.pu", i, "<-lut.pu", i, sep=""))))
      eval(parse(text=(paste("colnames(combine.pu", i, ")[1]<-as.character(names(", check_pu[i,1], "))", sep=""))))  
      eval(parse(text=(paste("colnames(combine.pu", i, ")[2]<-'label'", sep=""))))
      eval(parse(text=(paste("check_pu$PU[", i, "]<-'combine.pu", i, "'", sep="")))) 
    }
  }
  data<-rbind(data, check_pu) #add pu
}

n_pu<-nrow(data)
if (n_pu==0) {
  msgBox <- tkmessageBox(title = "PUR",
                         message = "No planning unit found. Please add planning unit data first.",
                         icon = "info", 
                         type = "ok")
  quit()
} else if(n_pu<2) {
  msgBox <- tkmessageBox(title = "PUR",
                         message = "At least 2 planning unit required.",
                         icon = "info", 
                         type = "ok")
  quit()
} 

#====SELECT PLANNING UNIT TO BE ANALYZED====
data$usage<-0
data$Data<-as.character(data$Data)
data$Name<-as.character(data$Name)
data$PU<-as.character(data$PU)
repeat{
  data<-edit(data)
  if(sum(data$usage)>1){
    break
  } else {
    msgBox <- tkmessageBox(title = "PUR",
                           message = "Choose at least two data to be combined. Retry?",
                           icon = "question", 
                           type = "retrycancel", default="retry")
    if(as.character(msgBox)=="cancel"){
      quit()
    }
  }
} 
data <- data[which(data$usage==1),]
data$usage<-NULL
if(data[1,1]=="ref"){
  ref.ind<-0
} else {
  ref.ind<-1
}

#====PROJECTION HANDLING====
ref2<-ref
ref2[isZero(ref2)]<-NA
command1 <- paste()
command2 <- paste()
#command3 <- paste()
cmd <- paste()
for(j in 1:nrow(data)) {
  input <- as.character(data[j,1])
  
  if(input=="ref"){
    eval(parse(text=(paste(input, "[isZero(", input, ")]<-NA", sep="")))) 
  }
  
  eval(parse(text=(paste("R", j, "<-", input, "*(100^(", j-1, "))", sep=""))))
  if (j != nrow(data)) {
    command1<-paste(command1, input, ",", sep="")
    command2<-paste(command2, input, "[]", ",", sep="")
    #command3<-paste(command3, "Var", j, ",", sep="")
    cmd<-paste(cmd,"R", j, "+", sep="")
  } else {
    command1<-paste(command1, input, sep="")
    command2<-paste(command2, input, "[]", sep="")
    #command3<-paste(command3, "Var", j, sep="")
    cmd<-paste(cmd,"R", j, sep="")
  }
}

#====Save to working directory====
pu_rec.index=pu_rec.index+1
workdir<-paste(log.file[1,1], "/", log.file[1,2],"/PUR/Combine_PU",pu_rec.index, sep="")
dir.create(workdir, mode="0777")
setwd(workdir)

#====COMBINE PLANNING UNIT FILES====
eval(parse(text=(paste("Combine_stack <- stack(", command1, ")", sep="")))) 
eval(parse(text=(paste("Combine<-", cmd, sep=""))))
Combine <- ratify(Combine, count=TRUE)
Combine_db <- levels(Combine)[[1]]

ORI_ID<-Combine_db$ID
NEW_ID<-seq(nrow(Combine_db)) 
rclmat<-cbind(as.matrix(ORI_ID), as.matrix(NEW_ID)) 
Combine<-reclassify(Combine, rclmat)
Combine<-ratify(Combine, count=TRUE)

Combine_db$NEW_ID<-NEW_ID
Combine_db$TEMP_ID<-Combine_db[,1]
k<-0
while(k < nrow(data)) {
  eval(parse(text=(paste("Combine_db$Var", k, "<-Combine_db$TEMP_ID %% 100", sep=""))))  
  Combine_db$TEMP_ID<-floor(Combine_db$TEMP_ID/100)
  k=k+1
}
Combine_db$TEMP_ID<-NULL

#urutin kolom
# names(Combine_db)[ncol(Combine_db)-1]
# Combine_db<-Combine_db[with(Combine_db, order(layer.2)), ]
combine_name<-""
for (x in 1:nrow(data)) {
  eval(parse(text=(paste("name<-data[", x, ", 1]", sep=""))))
  eval(parse(text=(paste("colnames(Combine_db)[",x+3,"]<-name", sep=""))))
  eval(parse(text=(paste("alias<-data[", x, ", 2]", sep=""))))
  if(x == 1){
    combine_name<-paste(combine_name, alias, sep="")
  } else {
    combine_name<-paste(combine_name, alias, sep="_")
  }
  #eval(parse(tex=(paste("Combine_db$", name, "[is.na(Combine_db$", name, ")]<-0", sep=""))))
}
Combine_db <- Combine_db[ which(Combine_db$COUNT > 0),] 
Combine_db<-na.omit(Combine_db) #keep all data, na dijadiin opsi

#looping untuk membuat kolom baru dengan nama attribute baru
for(i in 1:nrow(Combine_db)){
  sublabel=NULL
  for(j in 1:nrow(data)){
    #get label from pu LUT
    eval(parse(text=(paste("comb_name<-data[", j, ",3]", sep=""))))
    eval(parse(text=(paste("pu_name<-colnames(", comb_name, ")[1]", sep=""))))
    eval(parse(text=(paste("pu_label <- ", comb_name, "[which(Combine_db[",i, ",", j+3, "]==", comb_name, "$", pu_name, "),]", sep=""))))
    #append label
    if(j==nrow(data)){
      sublabel<-paste(sublabel, as.character(pu_label$label), sep="")
    } else {
      sublabel<-paste(sublabel, as.character(pu_label$label), "|", sep="")
    }
    #assigning label to new column
    Combine_db$Combined[i]<-sublabel
  }
}

colnames(Combine_db)[3]<-"unique_id"
Combine_db2<-Combine_db[,c('unique_id','Combined')]
Combine_db3<-Combine_db[,c('ID','COUNT','unique_id','Combined')]
colnames(Combine_db2)[1]= "ID"
test1<-unique(Combine_db2)[1]
test2<-unique(Combine_db2)[2]
test3<-cbind(test1,test2)
row.names(test3)<-NULL
#levels(Combine)<-merge(levels(Combine),test3,by="ID") 
Combine1 <- deratify(Combine,'ID')
names(Combine1)<-combine_name

writeRaster(Combine1, filename="Combine", format="GTiff", overwrite=TRUE)
row.names(test3)<-NULL
write.table(test3, "combine_LUT.csv", row.names = FALSE, sep=",")

Combine1<-spatial_sync_raster(Combine1, ref, method = "ngb")
Combine1<-Combine1*1

eval(parse(text=(paste("pu_pu", pu_rec.index, "<-Combine1", sep=""))))
eval(parse(text=(paste("lut.pu", pu_rec.index, "<-test3", sep=""))))
eval(parse(text=(paste("resave(pu_rec.index, pu_pu", pu_rec.index, ", lut.pu", pu_rec.index, ", file=proj.file)", sep=""))))

#FUNCTION FOR PLOTTING
#Create Map for report
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- brewer.pal(11, "Spectral")
myColors  <-c(myColors1,myColors7, myColors2, myColors3, myColors4, myColors5, myColors6)
rm(myColors1,myColors7, myColors2, myColors3, myColors4, myColors5, myColors6)

#Plot 6 (Peta hasil rekonsiliasi)
myColors.PUR.Comb <- myColors[1:length(unique(test3$ID))]
ColScale.PUR.Comb<-scale_fill_manual(name="Planning Unit",breaks=test3$ID, labels=test3$Combined, values = myColors.PUR.Comb )
plot.PUR.Comb  <- gplot(Combine1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  ColScale.PUR.Comb + coord_equal() + ggtitle(paste("Combined Map")) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#====Write report====
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
title<-"\\b\\fs40 LUMENS-PUR Project Report\\b0\\fs20"
sub_title<-"\\b\\fs32 KOMBINASI UNIT PERENCANAAN\\b0\\fs20"
date<-paste("Date : ", date(), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
rtffile <- RTF("LUMENS_PUR_report_combine.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "\\b \\fs32 DATA YANG DIGUNAKAN \\b0 \\fs20")
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "\\b Data acuan \\b0")
addNewLine(rtffile)

addTable(rtffile, data)
addNewLine(rtffile)
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3.73, res=150, plot(Combine_stack))
addNewLine(rtffile)

addParagraph(rtffile, "\\b \\fs32 HASIL KOMBINASI \\b0 \\fs20")
addParagraph(rtffile, line)
addParagraph(rtffile, "Pada bagian ini ditunjukkan hasil proses kombinasi antar planning unit ")
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3.73, res=150, plot.PUR.Comb )
addNewLine(rtffile)
addTable(rtffile, Combine_db3)
addNewLine(rtffile)

done(rtffile)

command<-paste("start ", "winword ", workdir, "/LUMENS_PUR_report_combine.lpr", sep="" )
shell(command)
