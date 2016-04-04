##[PUR]=group
##WDir=folder
##data=file
##ref=file
##rec_hier=file
##res=number 1000
##loc=string
##PUR_dbfinal_stat=output table
##area_rec1=output table
##PUR_rec2=output raster
##passfilenames

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#====set library====
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
library(rasterVis)
library(stringr)

#====set working directory====
location <- loc
PURPermitData <- data
ref_data <- ref
reconciliationClass <- rec_hier
setwd(WDir)

#====PREPARE REFERENCE DATA====
datalist_ref <- read.table(ref_data, header=FALSE, sep=",")
file_ref <- datalist_ref[1]
alias_ref <- datalist_ref [2]
attribute_ref <- datalist_ref [3]
LUT_ref <- datalist_ref[4]
ref_data <- as.character(file_ref[1,])

wd_ref <- dirname(as.character(ref_data))
st_area_file <- substr(basename(ref_data), 1, nchar(basename(ref_data)) - 4)
sa <- readOGR(dsn=wd_ref,st_area_file)

#====step to create a new raster from planning unit====
xlim_sa <- sa@bbox[1,]
ylim_sa <- sa@bbox[2,]
ext <- extent(sa)
xy_sa <- abs(apply(as.matrix(bbox(ext)), 1, diff))
n <- res  # in meter, need to be modified if in degree 0.0009
r <- raster(ext, ncol=xy_sa[1]/n, nrow=xy_sa[2]/n)
ref <- rasterize(sa, r)
ref[is.na(ref)] <- 0
ref <- deratify(ref)

#====PREPARE PLANNING UNIT FILE====
datalist <- read.table(PURPermitData, header=FALSE, sep=",")
file <- datalist[1]
alias <- datalist [2]
attribute <- datalist [3]
LUT <- datalist[4]
file.number <- nrow(file)
command1 <- paste()
command2 <- paste()
command3 <- paste()
for(i in 1:file.number) {
test <- basename(as.character(file[i,]))
input <- substr(basename(test), 1, nchar(basename(test)) - 4)
wd_loop <- dirname(as.character(file[i,]))

#eval(parse(text=(paste("pu_v",i,'<-readOGR(".","', input,'")', sep="")))) ;default dari mas andree
eval(parse(text=(paste("pu_v", i, ' <- readOGR("', wd_loop, '","', input, '")', sep="")))) #pu_v1 <- readOGR("path/to/dir", "shapefile1")
eval(parse(text=(paste("pu", i, ' <- ', 'rasterize(pu_v', i, ', r)', sep="")))) #pu1 <- rasterize(pu_v1, r)
eval(parse(text=(paste("pu", i, " <- deratify(pu", i, ", '", as.character(attribute[i,]), "')", sep="")))) #pu1 <- deratify(pu1, "attribute1")
#eval(parse(text=(paste("PUR<-stack(PUR,","pu",i,")", sep=""))))

eval(parse(text=(paste("pu", i, "_attribute <- levels(pu", i, ")", sep="")))) #pu1_attribute <- levels(pu1)
eval(parse(text=(paste("pu", i, "_attribute <- as.data.frame(pu", i, "_attribute)", sep="")))) #pu1_attribute <- as.data.frame(pu1_attribute)
#eval(parse(text=(paste("pu", i,"_attribute<-pu", i, '_attribute[,c("ID",(as.character(attribute[',i, ",])))]", sep=""))))

eval(parse(text=(paste("colnames(pu", i, '_attribute) <- c((paste("Var", i, sep="")), (as.character(alias[', i, ",])))", sep="")))) #colnames(pu1_attribute) <- c((paste("Var1", sep="")), (as.character(alias[1,])))
eval(parse(text=(paste("pu", i, "_LUT <- read.table(as.character(LUT[", i, ',]), header=TRUE, sep=",")', sep="")))) #pu1_LUT <- read.table(as.character(LUT[1,], header=TRUE, sep=","))
eval(parse(text=(paste("pu", i, "[is.na(pu", i, ")]<-0", sep="")))) #pu1[is.na(pu1)]<-0

if (i != file.number) {
command1<-paste(command1, "pu", i, ",", sep="")
command2<-paste(command2, "pu", i, "[]", ",", sep="")
command3<-paste(command3, "Var",i, ",", sep="")
} else {
command1<-paste(command1, "pu", i, sep="")
command2<-paste(command2, "pu", i, "[]", sep="")
command3<-paste(command3, "Var", i, sep="")
}
}

#====COMBINE PLANNING UNIT FILES AND REFERENCE FILE====
ref_attribute <- as.data.frame(levels(ref)) #referenceAttribute
ref.number <- file.number+1 #numOfReference
colnames(ref_attribute) <- c((paste("Var", ref.number, sep="")), (as.character(alias_ref[1,])))

PUR <- ref #initial reference map, skrg ada 2 variabel dengan nilai sama

command1 <- paste(command1, ",ref", sep="") # input command for stack input
command2 <- paste(command2, ",ref[]", sep="") # input command to overlay process
command3 <- paste(command3, ",Var", as.character(ref.number), sep="") # input command to generate unique id

eval(parse(text=(paste("PUR[] <- as.integer(interaction(", command2, "))", sep="")))) #overlay process
PUR <- ratify(PUR, filename='PUR.grd', count=TRUE, overwrite=TRUE) #generate overlayed planning unit maps

eval(parse(text=(paste("PUR_stack <- stack(", command1, ")", sep="")))) #stack planning unit maps
PUR_db <- crosstab(PUR_stack) ;#generate initial database
#PUR_db<-transform(PUR_db, unique_id=as.integer(interaction(Var1, Var2, Var3, Var4, Var5, drop=TRUE)))
eval(parse(text=(paste("PUR_db <- transform(PUR_db, unique_id=as.integer(interaction(", command3, ", drop=TRUE)))", sep="")))); #generate unique id column
PUR_db <- PUR_db[ which(PUR_db$Freq > 0),] # filter 0 value

#====PREPARE ATTRIBUTE DATA TO BE MERGE====
central_attr <- ref_attribute
colnames(central_attr)[1] = "c1"
colnames(central_attr)[2] = "c2"

#====CREATE CENTRAL ATTRIBUTE====
#add another permits att tables
for(j in 1:file.number) {
eval(parse(text=(paste("temp <- pu",j,"_attribute", sep=""))))
colnames(temp)[1] = "c1"
colnames(temp)[2] = "c2"
central_attr <- rbind(central_attr,temp)
}
add1 <- c(0)
add2 <- c("none")
add <- data.frame(add1,add2); #nodata/none row
colnames(add)[1] = "c1"
colnames(add)[2] = "c2"

central_attr <- rbind(central_attr,add) # add nodata row
c_nrow <- nrow(central_attr)
seq_id <- sequence(c_nrow)
seq <- as.data.frame(seq_id)
central_attr <- cbind(central_attr,seq)
central_attr[1] <- NULL

#====OK

#====EDIT PU_ATTRIBUTE BY ADDING NEW ROW FOR 0 VALUES====
for (k in 1:file.number) {
eval(parse(text=(paste('colnames(add)[1] = "Var', k, '"', sep="")))) #colnames(add)[1]=Var1
eval(parse(text=(paste('colnames(add)[2] = "', as.character(alias[k,]), '"',sep="")))) #colnames(add)[2]="as.character(alias[1,])"
eval(parse(text=(paste("pu", k, "_attribute <- rbind(pu", k, "_attribute, add)", sep="")))) #pu1_attribute <- rbind(pu1_attribute, add)
}

for (q in 1:file.number) {
x2 <- as.character(alias[q,])
x3 <- paste("seq", q, sep="")
x4 <- paste("pu", q, "_attribute", sep="")
colnames(central_attr)[1] <- x2
colnames(central_attr)[2] <- x3
eval(parse(text=(paste("pu", q, "_attribute <- merge(pu", q, "_attribute,central_attr, by=x2)", sep=""))))
}

#====MERGING ATTRIBUTE DATA TO PUR_DATABASE====
PUR_dbmod <- PUR_db
for (l in 1:file.number) {
eval(parse(text=(paste("PUR_dbmod <- merge(PUR_dbmod, pu", l, "_attribute, by='Var", l, "')", sep=""))))
}

colnames(add)[1] = paste("Var", as.character(ref.number), sep="")
colnames(add)[2] = as.character(alias_ref[1,])
ref_attribute <- rbind(ref_attribute, add)
PUR_dbmod <- merge(PUR_dbmod, ref_attribute, by=(paste("Var", ref.number, sep="")))

#====PREPARE RECLASSIFICATION FILE OF PLANNING UNIT AND MERGE RECLASSIFICATION FILE INTO PUR DATABASE====
add3 <- c("none")
add4 <- c("none")
add_11 <- data.frame(add3,add4)

#rec_id1<-c("Conservation", "Production", "Other", "none")
#rec_id2<-c(1,2,3,100)
#rec_idref<-data.frame(rec_id1,rec_id2)

rec_idref <- read.table(reconciliationClass, header=FALSE, sep=",")
colnames(rec_idref) <- c("rec_id1","rec_id2")


for (m in 1:file.number) {
eval(parse(text=(paste("att<-as.character(alias[", m, ",])", sep=""))))
eval(parse(text=(paste("att2<-as.character(LUT[", m, ",])", sep=""))))
look_pu<- read.table(att2, header=TRUE, sep=",")
colnames(look_pu)[1] = att
colnames(look_pu)[2] = paste(att,"_rec", sep="")
colnames(add_11)[1] = att
colnames(add_11)[2] = paste(att,"_rec", sep="")
colnames(rec_idref)[1]= paste(att,"_rec", sep="")
colnames(rec_idref)[2]= paste("rec_id", m, sep="")
look_pu<-rbind(look_pu,add_11)
x1<-paste(att,"_rec", sep="")
look_pu<-merge(look_pu, rec_idref, by=x1)
eval(parse(text=(paste("look_pu", m, "<-look_pu", sep=""))))
PUR_dbmod<-merge(PUR_dbmod, look_pu, by=att)
}
att3<-as.character(LUT_ref[1,])
att4<-as.character(alias_ref[1,])
look_ref<- read.table(att3, header=TRUE, sep=",")
colnames(look_ref)[1] = att4
colnames(look_ref)[2] = "REF"
colnames(add_11)[1] = att4
colnames(add_11)[2] = "REF"
colnames(rec_idref)[1] = "REF"
colnames(rec_idref)[2] = "REF_id"
look_ref<-rbind(look_ref,add_11)
look_ref<-merge(look_ref,rec_idref, by='REF')
PUR_dbmod<-merge(PUR_dbmod, look_ref, by=att4)


#====CONDUCT RECONCILIATION PHASE 1====
for (n in 1:file.number) {
eval(parse(text=(paste("PUR_dbmod<-within(PUR_dbmod,{cek", n, "<-as.numeric(rec_id", n, "==REF_id)})", sep=""))))
}
command4<-paste()
for (p in 1:file.number) {
if (p!=file.number) {
eval(parse(text=(paste("command4<-paste(command4,", '"cek', p, '+', '")', sep=""))))
} else {
eval(parse(text=(paste("command4<-paste(command4,", '"cek', p, '")', sep=""))))
}
}
PUR_dbmod<-within(PUR_dbmod, {reconcile1<-eval(parse(text=(command4)))})
PUR_dbmod<-within(PUR_dbmod, {reconcile_attr<-ifelse(reconcile1==0,as.character(Tata_ruang), "unresolved")})

command5<-paste()
for (r in 1:file.number) {
if (r!=file.number) {
eval(parse(text=(paste("command5<-paste(command5, ", '"(cek",', r,',"*",' , '"seq",' ,r, ', ")+", sep="")', sep="" ))))
}
else {
eval(parse(text=(paste("command5<-paste(command5, ", '"(cek",', r,',"*",' , '"seq",' ,r, ', ")", sep="")', sep="" ))))
}
}
PUR_dbmod<-within(PUR_dbmod, {reconcile_attr2<-ifelse(reconcile1==1, reconcile_attr2<-eval(parse(text=(command5))),100)})

central_attrmod<-central_attr
colnames(central_attrmod)[1]="Rec_phase1"
colnames(central_attrmod)[2]="reconcile_attr2"
add5<- c("none")
add6<- c(100)
add_22<- data.frame(add5,add6)
colnames(add_22)[1]="Rec_phase1"
colnames(add_22)[2]="reconcile_attr2"
central_attrmod<-rbind(central_attrmod, add_22)

PUR_dbfinal<-merge(PUR_dbmod,central_attrmod, by='reconcile_attr2')

PUR_dbfinal<-within(PUR_dbfinal, {
Rec_phase1<-ifelse(Rec_phase1=="none", as.character(reconcile_attr), as.character(Rec_phase1))})

len <- nrow(PUR_dbfinal)
angka = 1
for(s in 1:len){
if(as.character(PUR_dbfinal$Rec_phase1[s])=="unresolved"){
eval(parse(text=(paste("PUR_dbfinal$Rec_phase1[", s, "]<-'unresolved", angka, "'", sep='')))) #PUR_dbfinal$Rec_phase1[50]<-"unresolved"
angka = angka + 1
}
}

#DEFINE NA
#nodata_row<-as.array(which(PUR_dbfinal$unique_id == 1, arr.ind=TRUE)); #separating nodata value from unresolved
#eval(parse(text=(paste("PUR_dbfinal$Rec_phase1[", nodata_row, "]<-'nodata'", sep=""))))
#eval(parse(text=(paste("PUR_dbfinal$reconcile_attr[", nodata_row, "]<-'nodata'", sep=""))))

PUR_dbfinal2<-PUR_dbfinal[,c('unique_id','Rec_phase1')]
colnames(PUR_dbfinal2)[1]= "ID"

levels(PUR)<-merge((levels(PUR)),PUR_dbfinal2,by="ID"); #output shapefile
PUR_rec1 <- deratify(PUR,'Rec_phase1')
PUR_rec2<-ratify(PUR_rec1, filename='PUR_rec1.grd',count=TRUE,overwrite=TRUE) #cuma untuk di merge aja
levels(PUR_rec1)<-merge((levels(PUR_rec1)),levels(PUR_rec2),by="ID")
PUR_rec3<-stack(PUR, PUR_rec1)

#FILTER OUTPUT TABLE
PUR_dbfinal_out<-PUR_dbfinal
n<-1
for (n in 1:file.number) {
eval(parse(text=(paste("PUR_dbfinal_out$Var", n,"<-NULL", sep=""))))
eval(parse(text=(paste("PUR_dbfinal_out$cek", n,"<-NULL", sep=""))))
eval(parse(text=(paste("PUR_dbfinal_out$rec_id", n,"<-NULL", sep=""))))
eval(parse(text=(paste("PUR_dbfinal_out$seq", n,"<-NULL", sep=""))))
}
n<-1
Var.number<-file.number+1
for (n in 1:Var.number) {
eval(parse(text=(paste("PUR_dbfinal_out$Var", n,"<-NULL", sep=""))))
}

PUR_dbfinal_out$reconcile_attr2<-NULL
PUR_dbfinal_out$reconcile1<-NULL
PUR_dbfinal_out$REF_id<-NULL


#WRITE RASTER LAYER
writeRaster(PUR_rec2, filename="PUR_reconciliation_result", format="GTiff", overwrite=TRUE)
writeRaster(PUR, filename="PUR_reconciliation_initial", format="GTiff", overwrite=TRUE)


#STATISTICS

#1 Remove NA/nodata value
#PUR_dbfinal_stat<-PUR_dbfinal_out
#PUR_dbfinal_stat<-PUR_dbfinal_stat[-nodata_row, ]
PUR_dbfinal_stat<-PUR_dbfinal_out

#2 ALL PERMIT AREA over REFERENCE FILE: table and chart
overlapped_stat <- PUR_dbfinal_stat[which(PUR_dbfinal_stat$reconcile_attr=="unresolved"),]
if ("unresolved" %in% PUR_dbfinal_stat$reconcile_attr) {
tot_overlap<-sum(overlapped_stat$Freq)
tot_area<-sum(PUR_dbfinal_stat$Freq)
prop_overlap<-tot_overlap/tot_area*100


ref_overlap<-aggregate( cbind( Freq) ~ Tata_ruang , data = overlapped_stat , FUN = sum )
ref_overlap<-cbind(ref_overlap,round((ref_overlap[2]/tot_overlap*100),3))
colnames(ref_overlap)<-c("Tata_ruang", "area", "proportion")

#permit area overlapping reference chart
lbls <- paste(as.character(ref_overlap$Tata_ruang), as.numeric(as.matrix((ref_overlap[3])))) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

#overlapping map
overlapped_stat2<-overlapped_stat[,c('unique_id','Rec_phase1')]
colnames(overlapped_stat2)[1]= "ID"
PUR_over_ref<-PUR
levels(PUR_over_ref)<-merge((levels(PUR_over_ref)),overlapped_stat2,by="ID", all.x=T)
#modify att table
overlap_tab<-as.data.frame(levels(PUR_over_ref))
overlap_tab$Rec_phase1.x<-NULL
overlap_tab$COUNT<-NULL
colnames(overlap_tab)<-c("ID","Rec_phase1")
levels(PUR_over_ref)<-overlap_tab
PUR_over_ref <- deratify(PUR_over_ref)
writeRaster(PUR_over_ref, filename="PUR_over_reference", format="GTiff", overwrite=TRUE)

PUR_over_ref2<-ratify(PUR_over_ref, filename='PUR_over_ref.grd',count=TRUE,overwrite=TRUE)
levels(PUR_over_ref)<-merge((levels(PUR_over_ref)),levels(PUR_over_ref2),by="ID")
} else {
print("no permit area over reference map left!")
}

n_overlap <- nrow(overlapped_stat)
unresolved_stat <- data.frame()
resolved_stat <- data.frame()
for(x in 1:n_overlap){
if(str_detect(overlapped_stat$Rec_phase1[x], "unresolved")){
unresolved_stat <- rbind(unresolved_stat, overlapped_stat[x,]) #unresolved_stat <- PUR_dbfinal_stat[which(PUR_dbfinal_stat$Rec_phase1=="unresolved"),]
} else {
resolved_stat <- rbind(resolved_stat, overlapped_stat[x,]) #resolved_stat <- overlapped_stat[which(overlapped_stat$Rec_phase1 != "unresolved"),]
}
}

#3 RESOLVED AREA: table and chart
n_resolved <- nrow(resolved_stat)
if (n_resolved==0) { #"unresolved" %in% resolved_stat$Rec_phase1
print("no resolved area found!")
} else {
tot_resolved<-sum(resolved_stat$Freq)
prop_resolved<-tot_resolved/tot_area*100
prop_overlap_resolved<-tot_resolved/tot_overlap*100

ref_resolved<-aggregate( cbind( Freq) ~ Tata_ruang , data = resolved_stat , FUN = sum )
ref_resolved<-cbind(ref_resolved,round((ref_resolved[2]/tot_resolved*100),3))
colnames(ref_resolved)<-c("Tata_ruang", "area", "proportion")

ref_resolved_newPU<-aggregate( cbind( Freq) ~ Rec_phase1 , data = resolved_stat , FUN = sum )
ref_resolved_newPU<-cbind(ref_resolved_newPU,round((ref_resolved_newPU[2]/tot_resolved*100),3))
colnames(ref_resolved_newPU)<-c("Resolved_permit", "area", "proportion")

#RESOLVED AREA CHART BY REFERENCE MAP
lbl_resolved <- paste(as.character(ref_resolved$Tata_ruang), as.numeric(as.matrix((ref_resolved[3])))) # add percents to labels
lbl_resolved <- paste(lbl_resolved,"%",sep="") # ad % to labels

#RESOLVED PERMIT CHART
lbl_resolved_permit <- paste(as.character(ref_resolved_newPU$Resolved_permit), as.numeric(as.matrix((ref_resolved_newPU[3])))) # add percents to labels
lbl_resolved_permit <- paste(lbl_resolved_permit,"%",sep="") # ad % to labels


#resolved area map
resolved_stat2<-resolved_stat[,c('unique_id','Rec_phase1')]
colnames(resolved_stat2)[1]= "ID"
PUR_resolved<-PUR
levels(PUR_resolved)<-merge((levels(PUR_resolved)),resolved_stat2,by="ID", all.x=T)
#modify att table
resolved_tab<-as.data.frame(levels(PUR_resolved))
resolved_tab$Rec_phase1.x<-NULL
resolved_tab$COUNT<-NULL
colnames(resolved_tab)<-c("ID","Rec_phase1")
levels(PUR_resolved)<-resolved_tab
PUR_resolved <- deratify(PUR_resolved)
writeRaster(PUR_resolved, filename="PUR_resolved_area", format="GTiff", overwrite=TRUE)

PUR_resolved2<-ratify(PUR_resolved, filename='PUR_resolved.grd',count=TRUE,overwrite=TRUE)
levels(PUR_resolved)<-merge((levels(PUR_resolved)),levels(PUR_resolved2),by="ID")

}


#4 UNRESOLVED AREA: table and chart
n_unresolved <- nrow(unresolved_stat)
if (n_unresolved != 0) { #"unresolved" %in% unresolved_stat$Rec_phase1
tot_unresolved<-sum(unresolved_stat$Freq)
prop_unresolved<-tot_unresolved/tot_area*100
prop_overlap_unresolved<-tot_unresolved/tot_overlap*100

ref_unresolved<-aggregate( cbind( Freq) ~ Tata_ruang , data = unresolved_stat , FUN = sum )
ref_unresolved<-cbind(ref_unresolved,round((ref_unresolved[2]/tot_unresolved*100),3))
colnames(ref_unresolved)<-c("Tata_ruang", "area", "proportion")

#pie chart
lbls2 <- paste(as.character(ref_unresolved$Tata_ruang), as.numeric(as.matrix((ref_unresolved[3])))) # add percents to labels
lbls2 <- paste(lbls2,"%",sep="") # ad % to labels

#unresolved area map
unresolved_stat2<-unresolved_stat[,c('unique_id','Rec_phase1')]
colnames(unresolved_stat2)[1]= "ID"
PUR_unresolved<-PUR
levels(PUR_unresolved)<-merge((levels(PUR_unresolved)),unresolved_stat2,by="ID", all.x=T)
#modify att table
unresolved_tab<-as.data.frame(levels(PUR_unresolved))
unresolved_tab$Rec_phase1.x<-NULL
unresolved_tab$COUNT<-NULL
colnames(unresolved_tab)<-c("ID","Rec_phase1")
levels(PUR_unresolved)<-unresolved_tab
PUR_unresolved <- deratify(PUR_unresolved)
writeRaster(PUR_unresolved, filename="PUR_unresolved_area", format="GTiff", overwrite=TRUE)

PUR_unresolved2<-ratify(PUR_unresolved, filename='PUR_unresolved.grd',count=TRUE,overwrite=TRUE)
levels(PUR_unresolved)<-merge((levels(PUR_unresolved)),levels(PUR_unresolved2),by="ID")

} else {
print("no unresolved area left!")
}


#5 Reconciliation phase 1 result output
area_rec1<-as.data.frame(levels(PUR_rec1))
#nodata_row2<-as.array(which(area_rec1$ID == 1, arr.ind=TRUE))
#area_rec1<-area_rec1[-nodata_row2, ]

#WRITE DATABASE
write.dbf(PUR_dbfinal_out, "PUR_reconciliation_table_detail")
write.dbf(area_rec1, "PUR_reconciliation_table_summary")

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

#Planning Unit Map
PUM.lab<-as.data.frame(levels(PUR_stack))
y<-ncol(PUM.lab)
y<-y/2
for(i in 1:y){
eval(parse(text=paste("data.",i,"<-PUR_stack[[",i,"]]",sep="")))
eval(parse(text=paste("data.",i,"[data.",i,"[]==0]<-NA",sep="")))
eval(parse(text=paste("myColors.data.",i,"<-myColors[1:length(unique(PUM.lab[,",i*2,"]))]",sep="")))
eval(parse(text=paste("ColScale.data.",i,"<-scale_fill_manual(name='Planning Unit',breaks=unique(PUM.lab[,",i*2-1,"]), labels=unique(PUM.lab[,",i*2,"]), values=myColors.data.",i,")",sep="")))
eval(parse(text=paste("Plot.data.",i,"<-gplot(x=data.",i,",maxpixel=100000)+geom_raster(aes(fill=as.factor(value)))+",
"ColScale.data.",i,"+","ggtitle(paste(colnames(PUM.lab[",i*2,"])))+","coord_equal()+",
"theme(plot.title=element_text(lineheight=5, face='bold'))+",
"theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title=element_text(size=8),
legend.text=element_text(size=6),
legend.key.height = unit(0.25, 'cm'),
legend.key.width = unit(0.25, 'cm'))", sep="")))
}
rm(PUM.lab)

#Plot 3 (Peta ijin diatas peta referensi)
PUR.O.R.lab<-as.data.frame(levels(PUR_over_ref))
PUR.O.R.lab$COUNT<-NULL
myColors.PUR.O.R <- myColors[1:length(unique(PUR.O.R.lab$ID))]
ColScale.PUR.O.R<-scale_fill_manual(name="Planning Unit",breaks=PUR.O.R.lab$ID, labels=PUR.O.R.lab$Rec_phase1, values = myColors.PUR.O.R )
plot.PUR.O.R  <- gplot(PUR_over_ref2) + geom_raster(aes(fill=as.factor(value))) +
ColScale.PUR.O.R +  coord_equal() + ggtitle(paste("Permit Over Reference Map")) +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))

#Plot 4 (Peta permit yang sesuai)
PUR.R.lab<-as.data.frame(levels(PUR_resolved))
PUR.R.lab$COUNT<-NULL
myColors.PUR.R <- myColors[1:length(unique(PUR.R.lab$ID))]
ColScale.PUR.R<-scale_fill_manual(name="Planning Unit", breaks=PUR.R.lab$ID, labels=PUR.R.lab$Rec_phase1, values = myColors.PUR.R )
plot.PUR.R  <- gplot(PUR_resolved2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
ColScale.PUR.R + coord_equal() + ggtitle(paste("Resolved Permit Map")) +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))

#Plot 5 (Peta permit yang belum terselesaikan)
if(exists("PUR_unresolved")){
PUR.U.lab<-as.data.frame(levels(PUR_unresolved))
PUR.U.lab$COUNT<-NULL
myColors.PUR.U <- myColors[1:length(unique(PUR.U.lab$ID))]
ColScale.PUR.U<-scale_fill_manual(name="Planning Unit",breaks=PUR.U.lab$ID, labels=PUR.U.lab$Rec_phase1, values = myColors.PUR.U )
plot.PUR.U  <- gplot(PUR_unresolved2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
ColScale.PUR.U + coord_equal() + ggtitle(paste("Unresolved Permit Map")) +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))
} else {plot.PUR.U<-"Tidak ada data"}
#Plot 6 (Peta hasil rekonsiliasi)
PUR.Rec.lab<-area_rec1
PUR.Rec.lab$COUNT<-NULL
myColors.PUR.Rec <- myColors[1:length(unique(PUR.Rec.lab$ID))]
ColScale.PUR.Rec<-scale_fill_manual(name="Planning Unit",breaks=PUR.Rec.lab$ID, labels=PUR.Rec.lab$Rec_phase1, values = myColors.PUR.Rec )
plot.PUR.Rec  <- gplot(PUR_rec2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
ColScale.PUR.Rec + coord_equal() + ggtitle(paste("Reconciliation Map")) +
theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=8),
legend.text = element_text(size = 6),
legend.key.height = unit(0.25, "cm"),
legend.key.width = unit(0.25, "cm"))

#barplot(reconciliation phase 1 summary)
#Largest Net Emission rate
myColors.RPB <- myColors[1:length(unique(area_rec1$Rec_phase1))]
names(myColors.RPB) <- unique(area_rec1$Rec_phase1)
ColScale.RPB<-scale_fill_manual(values = myColors.RPB)
Rec.phs.bar<-ggplot(data=area_rec1, aes(x=Rec_phase1, y=COUNT, fill=Rec_phase1)) + geom_bar(stat="identity") +coord_flip() + ColScale.RPB +
geom_text(data=area_rec1, aes(x=Rec_phase1, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
ggtitle(paste("Unit Perencanaan Fase 1" )) + guides(fill=FALSE) + ylab("Luas (ha)") +
theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# Save PUR workspace
project.name<-paste("PUR_phase_1_", location,".RData", sep='')
save.image(project.name)

#rtf report file
title<-"\\b\\fs40 LUMENS-PUR Project Report\\b0\\fs20"
sub_title<-"\\b\\fs32 REKONSILIASI UNIT PERENCANAAN MENGGUNAKAN DATA ACUAN\\b0\\fs20"
date<-paste("Date : ", date(), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
rtffile <- RTF("LUMENS_PUR_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Rekonsiliasi unit perencanaan adalah proses untuk menyelesaikan tumpang-tindih ijin dengan merujuk pada peta acuan/referensi fungsi. Rekonsiliasi dilakukan dengan menganalisa kesesuaian fungsi antara data-data ijin dengan data referensi. Data ijin yang dimaksud datapat berupa data konsesi pengelolaan hutan, ijin perkebunan, ijin tambang dan lain sebagainya, Sedangkan data referensi yang digunakan dapat berupa data rencana tata ruang atau penunjukan kawasan. ")
addNewLine(rtffile)
addParagraph(rtffile, "\\b \\fs32 DATA YANG DIGUNAKAN \\b0 \\fs20")
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Data acuan")
addNewLine(rtffile)
addParagraph(rtffile, "Data acuan adalah data yang digunakan sebagai referensi dalam melakukan pengecekan kesesuaian fungsi peta-peta unit perencanaan dengan fungsi referensi. Peta ini dapat berupa peta acuan penunjukan kawasan atau peta tata ruang. Pada prinsipnya, data referensi adalah data dengan tingkat kepastian hukum tertinggi atau data yang apling dipercaya sebagai acuan fungsi unit perencanaan di sebuah daerah")
addNewLine(rtffile)
datalist_ref[1]<-NULL
addTable(rtffile, datalist_ref)
datalist[1]<-NULL
addNewLine(rtffile)
addParagraph(rtffile, "Data ijin")
addNewLine(rtffile)
addParagraph(rtffile, "Data ijin adalah data-data unit perencanaan yang akan digunakan untuk menunjukkan konfigurasi perencanaan penggunaan lahan di sebuah daerah. Data-data dalam bentuk peta ini menggambarkan arahan pengelolaan atau perubahan penggunaan lahan pada sebuah bagian bentang lahan")
addNewLine(rtffile)
addTable(rtffile, datalist)
addNewLine(rtffile)
z<-y/2
for(i in 1:z){
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150,
eval(parse(text=paste("grid.arrange(Plot.data.",(i*2)-1,",Plot.data.",i*2,",ncol=2)",sep=""))) )
}
addNewLine(rtffile)

addParagraph(rtffile, "\\b \\fs32 DATA IJIN YANG TIDAK TERREKONSILIASI \\b0 \\fs20")
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Data ijin yang tidak dapat terekonsiliasi akan tercantum pada bagian ini. Rekonsiliasi berbasis acuan fungsi, tidak dapat dilakukan jika ditemukan dua atau lebih unit perencanaan yang memiliki kesuaian fungsi dengan data acuan/referensi. Jika hal ini terjadi maka proses rekonsiliasi harus dilanjutkan melalui diskusi dengan semau pemangku kepentingan yang terkait ")
addNewLine(rtffile)
if(identical(typeof(plot.PUR.U),"character"))
{
addParagraph(rtffile,plot.PUR.U)
} else {
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.PUR.U )
}

addNewLine(rtffile)
if(identical(as.character(nrow(unresolved_stat)),"0")){
addParagraph(rtffile,"tidak ada data")
} else
{
addTable(rtffile, unresolved_stat)
}
addNewLine(rtffile)

if(identical(exists("lbls2"),"TRUE"))
{
addPlot.RTF(rtffile, plot.fun=print, width=4, height=3, res=150,pie3D(as.numeric(as.matrix((ref_unresolved[2]))),labels = lbls2, col=rainbow(length(lbls)),
main="Unresolved area by reference map", explode=0.1, radius=1, theta=0.9, labelcex=0.8))
} else {
addParagraph(rtffile,"tidak ada data")
}
addNewLine(rtffile)
addParagraph(rtffile, "\\b \\fs32 HASIL REKONSILIASI \\b0 \\fs20")
addParagraph(rtffile, line)
addParagraph(rtffile, "Pada bagian ini ditunjukkan hasil proses rekonsiliasi dengan menggunakan peta referensi ")
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addTable(rtffile, area_rec1)
addNewLine(rtffile)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, Rec.phs.bar )
addNewLine(rtffile)


done(rtffile)
