##[QUES]=group
##Wdir=folder
##Scen=file
##modeling.period=number 21
##location=string
##period1=number 1990
##warm.durat=number 2



library("RNetLogo")

# Begin define user input
use.warm.up<-TRUE
report.results<-TRUE
simulation.start.year<-1990
simulation.duration<-modeling.period
warm.up.duration<-warm.durat
observed.point.id<-0
water.body.id<-20
scenario.file.name<-Scen
output.file.name<-paste(Wdir,'/Output',location,simulation.start.year,'-',(simulation.start.year+simulation.duration-1),'.out',sep='')

# Finish Define user input

# Begin loading GENRIVER model
#NLQuit(nl.obj=my.netlogo2)

if (file.exists("C:/Program Files (x86)/NetLogo 5.0.5")){
nl.path<-("C:/Program Files (x86)/NetLogo 5.0.5")
} else{
nl.path<-("C:/Program Files/NetLogo 5.0.5")
}

my.netlogo2 <- "my.netlogo2"
NLStart(nl.path,
gui = F,
nl.obj = my.netlogo2)
model.path <- paste(nl.path,'/models/GenRiver_mod3.nlogo', sep='')
new.model.path<-paste(Wdir,'/GenRiver_mod3.nlogo', sep='')

file.copy(model.path,new.model.path, overwrite=T)
NLLoadModel(paste(new.model.path , sep="/"),nl.obj=my.netlogo2)
# Finish loading GENRIVER model

#scenario.file.name2<-"C:/JAYAPURA/QUES/QUES-H/UTM/09tes_rev/grimedata.par"
command1 <- paste("set scenario-file-name", ' \"', scenario.file.name,'\"', sep="")
command2 <- paste("set output-file-name", ' \"testing\"',sep="" )
NLCommand(command1,nl.obj = my.netlogo2)
NLCommand(command2,nl.obj = my.netlogo2)

#Begin setup sequences
#NLCommand('set scenario-file-name "C:\\1_AE\\LUMENS\\Hydro\\GENRIVER_Netlogo\\sumberjaya.par"',nl.obj = my.netlogo2)
#NLCommand("set output-file-name" output.file.name,nl.obj = my.netlogo2)
NLCommand("initialize-before-warming-up",nl.obj = my.netlogo2)
NLCommand("make-output-file-header",nl.obj = my.netlogo2)
NLCommand("warm-up",nl.obj = my.netlogo2)
NLCommand("reset",nl.obj = my.netlogo2)
NLCommand("set setup-done-code 1",nl.obj = my.netlogo2)
NLCommand("go",nl.obj = my.netlogo2)
#Finish setup sequences

# Begin simulation execution
#NLCommand("go",nl.obj = my.netlogo2)


# Quitting model
NLQuit(nl.obj=my.netlogo2)
detach(package:RNetLogo)
