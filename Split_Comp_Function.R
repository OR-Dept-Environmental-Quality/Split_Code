#split function

splitcomp<-function(deq,splt)
{
  #deq is ordeq dataset
  #splt is split dataset
  
  require(dplyr)
  
  #let's get the extra columns out, causing issues
deq<-subset(deq,select=c("OrganizationID","Project1","MLocID","act_id","Activity_Type",
                         "SampleStartDate","SampleStartTime","Char_Name",
                         "Char_Speciation","Sample_Fraction","CASNumber","Result_status","Result_Type","Result","Result_Numeric",
                         "Result_Operator","Result_Unit","Method_Code","Method_Context",
                         "Activity_Comment","Result_Comment","lab_Comments","QualifierAbbr","QualifierTxt","MDLType","MDLValue","MDLUnit",
                         "MRLType","MRLValue","MRLUnit"))
splt<-subset(splt,select=c("OrganizationID","Project1","MLocID","act_id","Activity_Type",
                          "SampleStartDate","SampleStartTime","Char_Name",
                          "Char_Speciation","Sample_Fraction","CASNumber","Result_status","Result_Type","Result","Result_Numeric",
                          "Result_Operator","Result_Unit","Method_Code","Method_Context",
                          "Activity_Comment","Result_Comment","lab_Comments","QualifierAbbr","QualifierTxt","MDLType","MDLValue","MDLUnit",
                          "MRLType","MRLValue","MRLUnit"))

#convert units so they match DEQ's data (NOTE: THIS NEEDS MORE TESTING TO MAKE SURE THE CODE WORKS PROPERLY)
#get the units associated with the characteristics for deq and split data
dequnit<-subset(deq,select=c("Char_Name","Result_Unit"))
dequnit<-unique(dequnit)
spltunit<-subset(splt,select=c("Char_Name","Result_Unit"))
spltunit<-unique(spltunit)

#join deq units and split units together, only keep characteristics where units do not agree
unjn<-inner_join(dequnit,spltunit,by=c("Char_Name"),suffix=c(".deq",".splt"))
unjn<-subset(unjn, unjn$Result_Unit.split!=unjn$Result_Unit.deq)

#get characteristics where we need to convert from mg to ug and also from ug to mg 
#(the most likely units conversions needed, will add more as necessary)
mgug<-subset(unjn, unjn$Result_Unit.splt=="mg/l" & unjn$Result_Unit.deq=="ug/l")
ugmg<-subset(unjn, unjn$Result_Unit.splt=="ug/l" & unjn$Result_Unit.deq=="mg/l")

#if there are any rows in mgug or ugmg then run data through unit conversion function
if (nrow(mgug)!=0) {splt<-unit_conv(splt,mgug$Char_Name,"mg/l","ug/l")}
if (nrow(ugmg)!=0) {splt<-unit_conv(splt,ugmg$Char_Name,"ug/l","mg/l")}
  

  #need to join datasets on an inner join
  jn<-inner_join(deq,splt, by = c('MLocID',"SampleStartDate","Char_Name","Sample_Fraction","Activity_Type"),suffix=c(".deq",".split"))
  


  #need to add a column to determine what type of QC is needed (eg. RPA, Diff, Microdiff)
  #difference is done when data is less than 5x MRL. don't always get MRL from split lab, use ours to be safe
  jn$qctype<-case_when(jn$Char_Name==c("pH")|jn$Result_Numeric.deq<=(5*jn$MRLValue.deq)|jn$Result_Numeric.split<=(5*jn$MRLValue.deq)~"Diff",
                       jn$Char_Name==c("Enterococcus","Escherichia coli","Fecal Coliform","Total Coliform")~"Micro",
                       jn$Result_Numeric.deq>(5*jn$MRLValue.deq)& jn$Result_Numeric.split>(5*jn$MRLValue.deq) ~"RPD"
                       )
                       

  #calculate RPD
  jn$splitRPD<- case_when(jn$qctype=="RPD"~ 
                            (abs(jn$Result_Numeric.deq-jn$Result_Numeric.split)/mean(c(jn$Result_Numeric.deq,jn$Result_Numeric.split))*100)
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
                           
  
  
  
  #need to return table of important columns
  jn<-subset(jn,select=c("MLocID","Activity_Type","SampleStartDate","SampleStartTime.deq","Char_Name",
                         "Char_Speciation.deq","Sample_Fraction","Result_status.deq","Result_status.split",
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
dat<-AWQMS_Data(startdate='2018-05-01',enddate='2018-05-03',project='Landfill Monitoring',org=c('OREGONDEQ','RVBND_LF(NOSTORETID)'))
deq<-subset(dat,OrganizationID=='OREGONDEQ')
rvb<-subset(dat,OrganizationID=='RVBND_LF(NOSTORETID)')

test<-splitcomp(deq,rvb)




