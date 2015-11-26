##[SCIENDO]=group
##wd=folder
##luwes_data=file
##scenario=file
##period1=number 0
##period2=number 0
##SCIENDO_LUWES=output table
##SCIENDO_LUWES_summary=output table
##report

library(pander)
library(knitr)
library(markdown)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(foreign)

# set working directory
setwd(wd)
# load datasets
data_merge<-read.dbf(luwes_data)
base<-data_merge
scenario<-read.dbf(scenario)
scenario[is.na(scenario)] <- 0
t1=period1
t2=period2
period<-t2-t1
iteration=5
total<-sum(data_merge$COUNT1_2)
data_merge.melt <- melt(data = data_merge, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT1_2'))
area_lc1 <- dcast(data = data_merge.melt, formula = ID_LC1 ~ ., fun.aggregate = sum)
colnames(area_lc1)[2] ="COUNT"
area_lc2<- dcast(data = data_merge.melt, formula = ID_LC2 ~ ., fun.aggregate = sum)
colnames(area_lc2)[2] ="COUNT"
data_merge.melt <- melt(data = data_merge, id.vars=c('ZONE'), measure.vars=c('COUNT1_2'))
area_zone <- dcast(data = data_merge.melt, formula = ZONE ~ variable, fun.aggregate = sum)

#load and process scenario
data_merge.melt <- melt(data = data_merge, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('COUNT2_3'))
luzone_p <- dcast(data = data_merge.melt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum)
colnames(luzone_p)[3] ="luzone"
luzone_p$key <- do.call(paste, c(luzone_p[c("LC_t1", "Z_NAME")], sep = " in "))
luzone_p$LC_t1<-luzone_p$Z_NAME<-NULL
data_merge$key <- do.call(paste, c(data_merge[c("LC_t1", "Z_NAME")], sep = " in "))
data_merge<-merge(data_merge,luzone_p,by="key")
data_merge$lutm_z<-scenario$lutm_z
data_merge$tpm_z<-scenario$tpm_z
data_merge$COUNT2_3x<-data_merge$tpm_z*data_merge$luzone
data_merge$key<-NULL

#calculate transition probability 1st iteration
x<-as.data.frame(data_merge)
y<-as.data.frame(area_lc1)
z<-as.data.frame(area_lc2)
v<-as.data.frame(area_zone)
colnames(v)[2] ="Z_AREA"
data_merge <- as.data.frame(merge(data_merge,v,by="ZONE"))
ID_LC1_fr<-x$ID_LC1
ID_LC2_to<-x$ID_LC2
ID_LC2_fr<-x$ID_LC1
ID_LC3_to<-x$ID_LC2
ID_LC3_fr<-x$ID_LC1
ID_LC4_to<-x$ID_LC2
ID_LC4_fr<-x$ID_LC1
ID_LC5_to<-x$ID_LC2
ID_LC5_fr<-x$ID_LC1
ID_LC6_to<-x$ID_LC2
ID_LC6_fr<-x$ID_LC1
ID_LC7_to<-x$ID_LC2
ID_LC7_fr<-x$ID_LC1

lutm_database<-data_merge
lutm_database$ID_LC1_fr<-ID_LC1_fr
lutm_database$ID_LC2_to<-ID_LC2_to
lutm_database$ID_LC2_fr<-ID_LC2_fr
lutm_database$COUNT1_2<-x$COUNT1_2
colnames(y)[1] ="ID_LC1_fr"
colnames(y)[2] ="ovcount_t1fr"
colnames(z)[1] ="ID_LC2_to"
colnames(z)[2] ="ovcount_t2to"
lutm_database <- as.data.frame(merge(lutm_database,y,by="ID_LC1_fr"))
lutm_database <- as.data.frame(merge(lutm_database,z,by="ID_LC2_to"))
colnames(z)[1] ="ID_LC2_fr"
colnames(z)[2] ="ovcount_t2fr"
lutm_database <- as.data.frame(merge(lutm_database,z,by="ID_LC2_fr"))
#lutm_database$tpm<-lutm_database$COUNT1_2/lutm_database$ovcount_t1fr <- CHECK IF THIS NEED TO BE UNCOMMENTED
lutm_database$COUNT2_3<-lutm_database$COUNT2_3x #up to here

lutm_database$lutm_landscape<-lutm_database$COUNT2_3/total
lutm_database$lutm_zone<-lutm_database$COUNT2_3/lutm_database$Z_AREA.x
lutm_database$ck_em<-lutm_database$CARBON_t1>lutm_database$CARBON_t2
lutm_database$ck_sq<-lutm_database$CARBON_t1<lutm_database$CARBON_t2
lutm_database$em0<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT1_2*3.67
lutm_database$sq0<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT1_2*3.67
lutm_database$em1<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT2_3*3.67
lutm_database$sq1<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT2_3*3.67
lutm_1<-lutm_database

#calculate transition probability 2nd iteration
lutm_database$ID_LC3_to<-lutm_database$ID_LC2
lutm_database$ID_LC3_fr<-lutm_database$ID_LC1
lutm_database.melt <- melt(data = lutm_database, id.vars=c('ID_LC3_to'), measure.vars=c('COUNT2_3'))
area_lc3 <- dcast(data = lutm_database.melt, formula = ID_LC3_to ~ ., fun.aggregate = sum)
colnames(area_lc3)[1] ="ID_LC3_fr"
colnames(area_lc3)[2] ="ovcount_t3fr"
lutm_database <- as.data.frame(merge(lutm_database,area_lc3,by="ID_LC3_fr"))
lutm_database$COUNT3_4<-lutm_database$tpm*lutm_database$ovcount_t3fr
lutm_database$em2<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT3_4*3.67
lutm_database$sq2<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT3_4*3.67
lutm_2<-lutm_database

#calculate transition probability 3rd iteration
lutm_database$ID_LC4_to<-lutm_database$ID_LC2
lutm_database$ID_LC4_fr<-lutm_database$ID_LC1
lutm_database.melt <- melt(data = lutm_database, id.vars=c('ID_LC4_to'), measure.vars=c('COUNT3_4'))
area_lc4 <- dcast(data = lutm_database.melt, formula = ID_LC4_to ~ ., fun.aggregate = sum)
colnames(area_lc4)[1] ="ID_LC4_fr"
colnames(area_lc4)[2] ="ovcount_t4fr"
lutm_database <- as.data.frame(merge(lutm_database,area_lc4,by="ID_LC4_fr"))
lutm_database$COUNT4_5<-lutm_database$tpm*lutm_database$ovcount_t4fr
lutm_database$em3<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT4_5*3.67
lutm_database$sq3<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT4_5*3.67
lutm_3<-lutm_database

#calculate transition probability 4th iteration
lutm_database$ID_LC5_to<-lutm_database$ID_LC2
lutm_database$ID_LC5_fr<-lutm_database$ID_LC1
lutm_database.melt <- melt(data = lutm_database, id.vars=c('ID_LC5_to'), measure.vars=c('COUNT4_5'))
area_lc5 <- dcast(data = lutm_database.melt, formula = ID_LC5_to ~ ., fun.aggregate = sum)
colnames(area_lc5)[1] ="ID_LC5_fr"
colnames(area_lc5)[2] ="ovcount_t5fr"
lutm_database <- as.data.frame(merge(lutm_database,area_lc5,by="ID_LC5_fr"))
lutm_database$COUNT5_6<-lutm_database$tpm*lutm_database$ovcount_t5fr
lutm_database$em4<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT5_6*3.67
lutm_database$sq4<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT5_6*3.67
lutm_4<-lutm_database

#calculate transition probability 5th iteration (this will be maximum iteration)
lutm_database$ID_LC6_to<-lutm_database$ID_LC2
lutm_database$ID_LC6_fr<-lutm_database$ID_LC1
lutm_database.melt <- melt(data = lutm_database, id.vars=c('ID_LC6_to'), measure.vars=c('COUNT5_6'))
area_lc6 <- dcast(data = lutm_database.melt, formula = ID_LC6_to ~ ., fun.aggregate = sum)
colnames(area_lc6)[1] ="ID_LC6_fr"
colnames(area_lc6)[2] ="ovcount_t6fr"
lutm_database <- as.data.frame(merge(lutm_database,area_lc6,by="ID_LC6_fr"))
lutm_database$COUNT6_7<-lutm_database$tpm*lutm_database$ovcount_t6fr
lutm_database$em5<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT6_7*3.67
lutm_database$sq5<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT6_7*3.67
lutm_5<-lutm_database

#make summary

Parameters<-c("Total emission (CO2 eq)", "Total sequestration (CO2 eq)", "Net emission (CO2 eq)", "Emission rate (CO2 eq/(ha.yr))", "Cumulative emission (CO2 eq/(ha.yr))")

sum_em0<-sum(lutm_database$em0)
sum_sq0<-sum(lutm_database$sq0)
net_em0<-sum(sum_em0-sum_sq0)
rate_em0<-net_em0/(total*period)
cum0<-0
sum_em1<-sum(lutm_database$em1)
sum_sq1<-sum(lutm_database$sq1)
net_em1<-sum(sum_em1-sum_sq1)
rate_em1<-net_em1/(total*period)
cum1<-rate_em0+rate_em1
Base<-c(sum_em0,sum_sq0,net_em0,rate_em0,cum0)
Iteration1<-c(sum_em1,sum_sq1,net_em1,rate_em1,cum1)
summary_SCIENDO_iteration1<-data.frame(Parameters,Base,Iteration1)

sum_em2<-sum(lutm_database$em2)
sum_sq2<-sum(lutm_database$sq2)
net_em2<-sum(sum_em2-sum_sq2)
rate_em2<-net_em2/(total*period)
cum2<-cum1+rate_em2
Iteration2<-c(sum_em2,sum_sq2,net_em2,rate_em2,cum2)
summary_SCIENDO_iteration2<-data.frame(summary_SCIENDO_iteration1,Iteration2)

sum_em3<-sum(lutm_database$em3)
sum_sq3<-sum(lutm_database$sq3)
net_em3<-sum(sum_em3-sum_sq3)
rate_em3<-net_em3/(total*period)
cum3<-cum2+rate_em3
Iteration3<-c(sum_em3,sum_sq3,net_em3,rate_em3,cum3)
summary_SCIENDO_iteration3<-data.frame(summary_SCIENDO_iteration2,Iteration3)

sum_em4<-sum(lutm_database$em4)
sum_sq4<-sum(lutm_database$sq4)
net_em4<-sum(sum_em4-sum_sq4)
rate_em4<-net_em4/(total*period)
cum4<-cum3+rate_em4
Iteration4<-c(sum_em4,sum_sq4,net_em4,rate_em4,cum4)
summary_SCIENDO_iteration4<-data.frame(summary_SCIENDO_iteration3,Iteration4)

sum_em5<-sum(lutm_database$em5)
sum_sq5<-sum(lutm_database$sq5)
net_em5<-sum(sum_em5-sum_sq5)
rate_em5<-net_em5/(total*period)
cum5<-cum4+rate_em5
Iteration5<-c(sum_em5,sum_sq5,net_em5,rate_em5,cum5)
summary_SCIENDO_iteration5<-data.frame(summary_SCIENDO_iteration4,Iteration5)

scenario <- c(cum1,cum2,cum3,cum4,cum5)

#make summary of base
sum_em1<-sum(base$em1)
sum_sq1<-sum(base$sq1)
net_em1<-sum(sum_em1-sum_sq1)
rate_em1<-net_em1/(total*period)
cum1<-rate_em0+rate_em1
sum_em2<-sum(base$em2)
sum_sq2<-sum(base$sq2)
net_em2<-sum(sum_em2-sum_sq2)
rate_em2<-net_em2/(total*period)
cum2<-cum1+rate_em2
sum_em3<-sum(base$em3)
sum_sq3<-sum(base$sq3)
net_em3<-sum(sum_em3-sum_sq3)
rate_em3<-net_em3/(total*period)
cum3<-cum2+rate_em3
sum_em4<-sum(base$em4)
sum_sq4<-sum(base$sq4)
net_em4<-sum(sum_em4-sum_sq4)
rate_em4<-net_em4/(total*period)
cum4<-cum3+rate_em4
sum_em5<-sum(base$em5)
sum_sq5<-sum(base$sq5)
net_em5<-sum(sum_em5-sum_sq5)
rate_em5<-net_em5/(total*period)
cum5<-cum4+rate_em5

#save SCIENDO-LUWES Database
SCIENDO_LUWES<-if(iteration==1){
lutm_1
}else if(iteration==2){
lutm_2
}else if(iteration==3){
lutm_3
}else if(iteration==4){
lutm_4
}else if(iteration==5){
lutm_5
}

#save SCIENDO-LUWES Summary
SCIENDO_LUWES_summary<-if(iteration==1){
summary_SCIENDO_iteration1
}else if(iteration==2){
summary_SCIENDO_iteration2
}else if(iteration==3){
summary_SCIENDO_iteration3
}else if(iteration==4){
summary_SCIENDO_iteration4
}else if(iteration==5){
summary_SCIENDO_iteration5
}

#Remove unnecessary colum
SCIENDO_LUWES$ID_LC6_fr<-SCIENDO_LUWES$ID_LC5_fr<-SCIENDO_LUWES$ID_LC4_fr<-SCIENDO_LUWES$ID_LC3_fr<-SCIENDO_LUWES$ID_LC2_fr<-SCIENDO_LUWES$ID_LC2_to<-SCIENDO_LUWES$ID_LC1_fr<-NULL
SCIENDO_LUWES$em<-SCIENDO_LUWES$sq<-SCIENDO_LUWES$null<-SCIENDO_LUWES$nullCek<-SCIENDO_LUWES$ZONE_ID<-SCIENDO_LUWES$COUNT<-SCIENDO_LUWES$ovcount_t1fr<-SCIENDO_LUWES$ovcount_t2to<-NULL
SCIENDO_LUWES$ovcount_t2fr<-SCIENDO_LUWES$ID_LC3_to<-SCIENDO_LUWES$ovcount_t3fr<-SCIENDO_LUWES$ID_LC4_to<-SCIENDO_LUWES$ovcount_t4fr<-SCIENDO_LUWES$ID_LC5_to<-SCIENDO_LUWES$ovcount_t5fr<-SCIENDO_LUWES$ID_LC6_to<-SCIENDO_LUWES$ovcount_t6fr<-NULL

#<NOT SURE IF WE WILL NEED THIS, TEMPORARY COMMENTED>

#SCIENDO-LUWES Overall LUTM
#lutm_o<-as.data.frame(as.numeric(SCIENDO_LUWES$ID_LC1))
#colnames(lutm_o)[1] ="ID_LC1"
#lutm_o$ID_LC2<-as.numeric(SCIENDO_LUWES$ID_LC2)
#lutm_o$landcover_t1<-SCIENDO_LUWES$LC_t1
#lutm_o$landcover_t2<-SCIENDO_LUWES$LC_t2
#lutm_o$zone<-SCIENDO_LUWES$Z_NAME
#lutm_o$TPM<-SCIENDO_LUWES$tpm
#lutm_o$LUTM<-SCIENDO_LUWES$lutm_landscape
#lutm_o.melt <- melt(data = lutm_o, id.vars=c('ID_LC1','ID_LC2','landcover_t1','landcover_t2'), measure.vars=c('TPM'))
#tpm_matrix <- dcast(data = lutm_o.melt, formula = ID_LC1 + landcover_t1 ~ ID_LC2, fun.aggregate = sum)
#lutm_o.melt <- melt(data = lutm_o, id.vars=c('ID_LC1','ID_LC2','landcover_t1'), measure.vars=c('LUTM'))
#lutm_matrix <- dcast(data = lutm_o.melt, formula = ID_LC1 + landcover_t1 ~ ID_LC2, fun.aggregate = sum)

#SCIENDO-LUWES Zones LUTM
#lutm_z<-SCIENDO_LUWES
#lutm_z$lutm_z<-lutm_z$COUNT1_2/lutm_z$Z_AREA
#lutm_z.melt <- melt(data = lutm_z, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('lutm_z'))
#luzone_p <- dcast(data = lutm_z.melt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum)
#colnames(luzone_p)[3] ="luzone"
#luzone_p$key <- do.call(paste, c(luzone_p[c("LC_t1", "Z_NAME")], sep = " in "))
#luzone_p$LC_t2<-luzone_p$Z_NAME<-NULL
#lutm_z$key <- do.call(paste, c(lutm_z[c("LC_t1", "Z_NAME")], sep = " in "))
#lutm_z<-merge(lutm_z,luzone_p,by="key")
#lutm_z$tpm_z<-lutm_z$lutm_z/lutm_z$luzone
#lutm_z$ZONE<-lutm_z$CARBON_t1<-lutm_z$CARBON_t2<-lutm_z$ck_em<-lutm_z$ck_sq<-lutm_z$LUCHG<-lutm_z$ID_LC1<-lutm_z$ID_LC2<-NULL
#lutm_z$tpm<-lutm_z$COUNT2_3<-lutm_z$lutm_landscape<-lutm_z$em0<-lutm_z$sq0<-lutm_z$em1<-lutm_z$sq1<-lutm_z$COUNT3_4<-lutm_z$em2<-lutm_z$sq2<-lutm_z$COUNT4_5<-lutm_z$em3<-lutm_z$sq3<-lutm_z$COUNT5_6<-lutm_z$em4<-lutm_z$sq4<-lutm_z$COUNT6_7<-lutm_z$em5<-lutm_z$sq5<-NULL
#lutm_z$LU_CHG<-lutm_z$key<-lutm_z$lutm_zone<-lutm_z$tpmx<-lutm_z$A_lczone<-NULL
#lutm_z$Z_AREA<-lutm_z$COUNT1_2<-lutm_z$LC_t1.y<-lutm_z$luzone<-NULL

#write output to file
SCIENDO_LUWES_SP<-SCIENDO_LUWES
write.dbf(SCIENDO_LUWES,"SCIENDO-LUWES_database_SP.dbf")
write.dbf(SCIENDO_LUWES_summary,"SCIENDO-LUWES_summary_SP.dbf")
#write.dbf(lutm_z,"SCIENDO-LUWES_zones_tpm.dbf")
#write.dbf(lutm_z,"SCIENDO-LUWES_zones_tpm_model.dbf")
#write.dbf(lutm_o,"SCIENDO-LUWES_overall_lutm.dbf")
#write.dbf(tpm_matrix,"SCIENDO-LUWES_overall_tpm_matrix.dbf")
#write.dbf(lutm_z,"lutm_z.dbf")

#conduct analysis on the dataset
SL_overall<-SCIENDO_LUWES_summary
SL_analysis<-SCIENDO_LUWES
SL_overall.melt <- melt(data = SL_overall)
SL_overall.melt.cast <- dcast(data = SL_overall.melt, formula = Parameters ~ variable, fun.aggregate = sum, subset = .(Parameters=="Cumulative emission (CO2 eq/(ha.yr))"))
SL_overall_data<- melt(data = SL_overall.melt.cast)
SL_overall_data<-SL_overall_data[-c(1),]
plot1<-ggplot(SL_overall_data,aes(variable,value,fill=Parameters))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x= element_text(angle=0,hjust=1))+ scale_fill_discrete(name="Emission (CO2eq)")+theme(legend.position="bottom")+theme(legend.text = element_text(size=8))
SL_analysis.melt <- melt(data = SL_analysis, id.vars=c('Z_NAME'), measure.vars=c('em0','em1','em2','em3','em4','em5'))
plot2<-ggplot(SL_analysis.melt,aes(variable,value,fill=Z_NAME))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x= element_text(angle=0,hjust=1))+ scale_fill_discrete(name="Emission (CO2eq)")+theme(legend.text = element_text(size=8))
em_by_zone <- dcast(data = SL_analysis.melt, formula = Z_NAME ~ variable, fun.aggregate = sum)
base <- c(cum1,cum2,cum3,cum4,cum5)
iteration<-c('it1','it2','it3','it4','it5')

#compare base and scenario
compare<-data.frame(iteration,base,scenario)
compare.melt <- melt(data = compare, id.vars=c('iteration'), measure.vars=c('base','scenario'))
plot3<-ggplot(compare.melt,aes(iteration,value,fill=variable))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x= element_text(angle=0,hjust=1))+ scale_fill_discrete(name="Emission (CO2eq)")+theme(legend.position="bottom")+theme(legend.text = element_text(size=8))

#write report
#wdGet()
#wdWrite("This document is automatically generated by LUMENS")
#wdSection("Land Use Planning for Multiple Environmental Services")
#wdTitle("SCIENDO:Scenario Development and Simulation",label="R2wd")
#wdWrite("SCIENDO:Land Use Planning for Low Emission Development Strategy adalah bagian dari sub-modul bagian dari LUMENS (Land Use planning for Multiple ENvironmental Services) yang berfungsi untuk melakukan prediksi emisi di masa yang akan datang berdasarkan berbagai skenario",paragraph = TRUE)
#wdPlot(plot1,caption="Predicted emission",height = 4, width = 4,pointsize = 6,paragraph = TRUE)
#wdTable(SCIENDO_LUWES_summary, caption = "Summary of SCIENDO-LUWES Result", caption.pos="above", autoformat=20)
#wdPlot(plot2,caption="Predicted emission by zone",height = 5, width = 5,pointsize = 6,paragraph = TRUE)
#wdTable(em_by_zone, caption = "Summary of SCIENDO-LUWES Result by planning unit", caption.pos="above", autoformat=20)
#wdPlot(plot3,caption="Comparison with baseline",height = 5, width = 5,pointsize = 6,paragraph = TRUE)
#wdTable(compare, caption = "Comparison with baseline", caption.pos="above", autoformat=20)

#rm(list=setdiff(ls(), "SCIENDO_LUWES_SP"))
reports<-paste("
Land Use Planning for Multiple Environmental Services
========================================================
***

# Lembar hasil analisis SCIENDO-Scenario Development and Simulation:
# Prediksi emisi di masa yang akan datang berdasarkan berbagai skenario

***

***
# Predicted emission
```{r fig.width=10, fig.height=9, echo=FALSE}
plot(plot1)
```
***
# Summary of SCIENDO-LUWES Result
```{r fig.width=10, fig.height=9, echo=FALSE}
pandoc.table(SCIENDO_LUWES_summary)

```
***

# Predicted emission by zone
```{r fig.width=10, fig.height=9, echo=FALSE}
plot(plot2)
```
***
# Summary of SCIENDO-LUWES Result by planning unit
```{r fig.width=10, fig.height=9, echo=FALSE}
pandoc.table(em_by_zone)

```
***

# Comparison with baseline
```{r fig.width=10, fig.height=9, echo=FALSE}
plot(plot3)
```
***
# Table comparison with baseline
```{r fig.width=10, fig.height=9, echo=FALSE}
pandoc.table(compare)

```
***
")


#WRITE REPORT
write(reports,file="reporthtml.Rmd")
knit2html("reporthtml.Rmd", options=c("use_xhml"))
