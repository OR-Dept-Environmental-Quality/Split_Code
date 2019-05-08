#split function

splitcomp<-function(deq,splt)
{
  #deq is ordeq dataset
  #splt is split dataset
  
  require(dplyr)
  source("E:/Permit Job/R_Scripts/ShinyNPDES_AWQMS/NameandFraction.R")

  
#####UNITS
#convert units so they match DEQ's data (NOTE: THIS NEEDS MORE TESTING TO MAKE SURE THE CODE WORKS PROPERLY)
#get the units associated with the characteristics for deq and split data
dequnit<-subset(deq,select=c("Char_Name","Result_Unit"))
dequnit<-unique(dequnit)
spltunit<-subset(splt,select=c("Char_Name","Result_Unit"))
spltunit<-unique(spltunit)

#join deq units and split units together, only keep characteristics where units do not agree
unjn<-inner_join(dequnit,spltunit,by=c("Char_Name"),suffix=c(".deq",".splt"))
unjn<-subset(unjn, unjn$Result_Unit.splt!=unjn$Result_Unit.deq)

#get characteristics where we need to convert from mg to ug and also from ug to mg 
#(the most likely units conversions needed, will add more as necessary)
mgug<-subset(unjn, unjn$Result_Unit.splt=="mg/l" & unjn$Result_Unit.deq=="ug/l")
ugmg<-subset(unjn, unjn$Result_Unit.splt=="ug/l" & unjn$Result_Unit.deq=="mg/l")

#if there are any rows in mgug or ugmg then run data through unit conversion function
if (nrow(mgug)!=0) {splt<-unit_conv(splt,mgug$Char_Name,"mg/l","ug/l")}
if (nrow(ugmg)!=0) {splt<-unit_conv(splt,ugmg$Char_Name,"ug/l","mg/l")}
  

###JOIN DEQ AND SPLIT DATA
#use namfrac function to get fraction as part of name for Metals 
#(if we just use sample fraction as part of the join we might miss some characteristics that were uploaded with different sample fractions
#where sample fraction doesn't matter (e.g. Dichlorodifluoromethane has been uploaded in AWQMS under Volatile and Extractable sample fractions))
deq<-namefrac(deq)
splt<-namefrac(splt)
  #need to join datasets on an inner join
  jn<-inner_join(deq,splt, by = c('MLocID',"SampleStartDate","Char_Name","Activity_Type"),suffix=c(".deq",".split"))
  


  #need to add a column to determine what type of QC is needed (eg. RPA, Diff, Microdiff)
  #difference is done when data is less than 5x MRL. don't always get MRL from split lab, use ours to be safe
  jn$qctype<-case_when(jn$Char_Name==c("pH")|jn$Result_Numeric.deq<=(5*jn$MRLValue.deq)|jn$Result_Numeric.split<=(5*jn$MRLValue.deq)~"Diff",
                       jn$Char_Name=="Enterococcus"|jn$Char_Name=="Escherichia coli"|jn$Char_Name=="Fecal Coliform"|jn$Char_Name=="Total Coliform"~"Micro",
                       jn$Result_Numeric.deq>(5*jn$MRLValue.deq)& jn$Result_Numeric.split>(5*jn$MRLValue.deq) ~"RPD"
                       )
                       

  #calculate RPD
  jn$splitRPD<- case_when(jn$qctype=="RPD"~ 
                            (abs(jn$Result_Numeric.deq-jn$Result_Numeric.split)/((jn$Result_Numeric.deq+jn$Result_Numeric.split)/2)*100)
                          )
    
  #Calculate difference for Diff and Micro don't always get MRL from split lab, use ours to be safe 
  jn$splitDiff<- case_when(jn$qctype=="Diff" & jn$Result_Operator.deq=="=" & jn$Result_Operator.split=="=" 
                           ~abs(jn$Result_Numeric.deq-jn$Result_Numeric.split),
                           jn$qctype=="Diff" & jn$Result_Operator.deq!="=" & jn$Result_Operator.split=="=" 
                             ~abs(jn$Result_Numeric.split-jn$MRLValue.deq),
                           jn$qctype=="Diff" & jn$Result_Operator.deq=="=" & jn$Result_Operator.split!="=" 
                             ~abs(jn$Result_Numeric.deq-jn$MRLValue.deq), 
                           jn$qctype=="Micro" & jn$Result_Operator.deq=="=" & jn$Result_Operator.split=="=" 
                             ~abs(log10(jn$Result_Numeric.deq)-log10(jn$Result_Numeric.split)),
                           jn$qctype=="Micro"& jn$Result_Operator.deq=="=" & jn$Result_Operator.split!="=" 
                             ~abs(log10(jn$Result_Numeric.deq)-log10(jn$MRLValue.deq)),
                           jn$qctype=="Micro"& jn$Result_Operator.deq!="=" & jn$Result_Operator.split=="=" 
                             ~abs(log10(jn$Result_Numeric.split)-log10(jn$MRLValue.deq))
                           )
                           
  #round RPD and Diff
  jn$splitDiff<-round(jn$splitDiff,digits=2)
  jn$splitRPD<-round(jn$splitRPD,digits=2)
  
  
  
  #need to return table of important columns
  jn<-subset(jn,select=c("MLocID","Activity_Type","SampleStartDate","SampleStartTime.deq","Char_Name",
                         "Char_Speciation.deq","Result_status.deq","Result_status.split",
                         "Result_Type.deq","Result_Type.split","Result.deq","Result.split",
                         "Result_Unit.deq","Result_Unit.split","Method_Code.deq","Method_Code.split",
                         "Activity_Comment.deq","Result_Comment.deq","lab_Comments.deq",
                         "Activity_Comment.split","Result_Comment.split","lab_Comments.split",
                         "MRLType.deq","MRLValue.deq","MRLUnit.deq","MRLType.split","MRLValue.split","MRLUnit.split",
                         "qctype","splitRPD","splitDiff"
                         ))
    
  return(jn)
}








#test dataset
library(AWQMSdata)
#dat<-AWQMS_Data(startdate='2018-05-01',enddate='2018-05-03',project='Landfill Monitoring',org=c('OREGONDEQ','RVBND_LF(NOSTORETID)'))
#deq<-subset(dat,OrganizationID=='OREGONDEQ')
#rvb<-subset(dat,OrganizationID=='RVBND_LF(NOSTORETID)')

#test<-splitcomp(deq,rvb)

#nomatch<-full_join(deq,rvb,by = c('MLocID',"SampleStartDate","Char_Name","Activity_Type"),suffix=c(".deq",".split") )
#nomatch<-subset(nomatch, is.na(OrganizationID.split)|is.na(OrganizationID.deq))
#nomatch<-subset(nomatch,select=c("OrganizationID.split","OrganizationID.deq",'MLocID',"SampleStartDate","Char_Name","Activity_Type"))


