##Alpha - DATABASE=group
##proj.file=string
##description=string
##attribute_table=string
##statusoutput=output table

#=Load library
library(stringr)

#=Load active project 
load(proj.file)

#=Set working directory to DATA folder 
data_dir<-paste(dirname(proj.file), "/DATA/", sep="")
setwd(data_dir)

description<-str_replace_all(string=description, pattern=" ", repl=".")

lut.index<-lut.index+1
eval(parse(text=(paste("lut", lut.index, "<-read.table(attribute_table, header=TRUE, sep=',')", sep=""))))

csv_file<-paste(dirname(proj.file),"/DATA/csv_lookup_table.csv", sep="")
if(file.exists(csv_file)){
  list_of_data_lut<-read.table(csv_file, header=TRUE, sep=",")
  eval(parse(text=(paste("add_data<-data.frame(TBL_DATA='lut", lut.index,"', TBL_NAME='", description, "', row.names=NULL)", sep=""))))
  list_of_data_lut<-rbind(list_of_data_lut,add_data)
} else {
  eval(parse(text=(paste("list_of_data_lut<-data.frame(TBL_DATA='lut", lut.index,"', TBL_NAME='", description, "', row.names=NULL)", sep=""))))
}
write.table(list_of_data_lut, csv_file, quote=FALSE, row.names=FALSE, sep=",")

category<-"lookup_table"
#check existing rdata
file_rdata<-paste(data_dir, category, sep="")
check_rdata<-file.exists(file_rdata)
if(check_rdata){
  eval(parse(text=(paste("resave(lut", lut.index, ", list_of_data_lut, file=file_rdata)", sep="")))) 
} else {
  eval(parse(text=(paste("save(lut", lut.index, ", list_of_data_lut, file=file_rdata)", sep="")))) 
}
#=Create lazyLoad database
e = local({load(category); environment()})
tools:::makeLazyLoadDB(e, category)
resave(lut.index, file=proj.file)

#=Writing final status message (code, message)
statuscode<-1
statusmessage<-"Lookup table has been added"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
