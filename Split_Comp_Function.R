#split function

splitcomp<-function(deq,splt)
{
  #deq is ordeq dataset
  #splt is split dataset
  
  require(dplyr)
  require(stringr)
  source("NameandFraction.R")

  
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

#If split org did nitrate analysis instead of nitrate-nitrite, and DEQ did nitrate-nitrate, convert split char name to nitrate-nitrite
#so the two can be compared. Often the results are so similar due to nitrite easily converting to nitrate that the two are comparable. 

if(any(deq$Char_Name %in% "Nitrate + Nitrite")) {splt<-splt%>% mutate(Char_Name=str_replace(Char_Name,"Nitrate","Nitrate + Nitrite"))}

  #need to join datasets on an inner join
  jn<-inner_join(deq,splt, by = c('MLocID',"SampleStartDate","Char_Name","Activity_Type"),suffix=c(".deq",".split"))
  


  #need to add a column to determine what type of QC is needed (eg. RPA, Diff, Microdiff)
  #difference is done when data is less than 5x MRL. don't always get MRL from split lab, use ours to be safe
  jn$qctype<-case_when(jn$Char_Name=="Enterococcus"|jn$Char_Name=="Escherichia coli"|jn$Char_Name=="Fecal Coliform"|jn$Char_Name=="Total Coliform"~"Micro",
                       jn$Char_Name==c("pH")|jn$Result_Numeric.deq<=(5*jn$MRLValue.deq)|jn$Result_Numeric.split<=(5*jn$MRLValue.deq)~"Diff",
                       jn$Result_Numeric.deq>(5*jn$MRLValue.deq)& jn$Result_Numeric.split>(5*jn$MRLValue.deq) ~"RPD"
                       )
                       

  #calculate RPD
  jn$splitRPD<- case_when(jn$qctype=="RPD"~ 
                            (abs(jn$Result_Numeric.deq-jn$Result_Numeric.split)/((jn$Result_Numeric.deq+jn$Result_Numeric.split)/2)*100)
                          )
    
  #Calculate difference for Diff and Micro don't always get MRL from split lab, use ours to be safe 
  #if micro result is 0, use DEQ MRL since log of 0 is infinity. 
  jn$splitDiff<- case_when(jn$qctype=="Diff" & jn$Result_Operator.deq=="=" & jn$Result_Operator.split=="=" 
                           ~abs(jn$Result_Numeric.deq-jn$Result_Numeric.split),
                           jn$qctype=="Diff" & jn$Result_Operator.deq!="=" & jn$Result_Operator.split=="=" 
                             ~abs(jn$Result_Numeric.split-jn$MRLValue.deq),
                           jn$qctype=="Diff" & jn$Result_Operator.deq=="=" & jn$Result_Operator.split!="=" 
                             ~abs(jn$Result_Numeric.deq-jn$MRLValue.split), 
                           jn$qctype=="Micro" & (jn$Result_Operator.deq=="="|jn$Result_Operator.deq==">") & 
                             (jn$Result_Operator.split=="="|jn$Result_Operator.split==">") &
                             jn$Result_Numeric.deq!=0 & jn$Result_Numeric.split!=0
                             ~abs(log10(jn$Result_Numeric.deq)-log10(jn$Result_Numeric.split)),
                           jn$qctype=="Micro"& (jn$Result_Operator.deq=="="|jn$Result_Operator.deq==">") & 
                             (jn$Result_Operator.split!="="|jn$Result_Operator.split!=">") &
                             jn$Result_Numeric.deq!=0 & jn$Result_Numeric.split!=0
                             ~abs(log10(jn$Result_Numeric.deq)-log10(jn$MRLValue.deq)),
                           jn$qctype=="Micro"& (jn$Result_Operator.deq!="="|jn$Result_Operator.deq!=">") & 
                             (jn$Result_Operator.split=="="|jn$Result_Operator.split==">") &
                             jn$Result_Numeric.deq!=0 & jn$Result_Numeric.split!=0
                             ~abs(log10(jn$Result_Numeric.split)-log10(jn$MRLValue.deq)),
                           jn$qctype=="Micro" &  (jn$Result_Operator.split=="="|jn$Result_Operator.split==">") & jn$Result_Numeric.deq==0
                           ~abs(log10(jn$MRLValue.deq)-log10(jn$Result_Numeric.split)),
                           jn$qctype=="Micro" & (jn$Result_Operator.deq=="="|jn$Result_Operator.deq==">") & jn$Result_Numeric.split==0
                           ~abs(log10(jn$MRLValue.deq)-log10(jn$Result_Numeric.deq))
                           )
                           
  #round RPD and Diff
  jn$splitDiff<-round(jn$splitDiff,digits=2)
  jn$splitRPD<-round(jn$splitRPD,digits=2)
  
  
  #need to remove samples from join where the split was taken too far apart in time 
  #(30 min for now- ask, may need to be taken down to 15-20)
  jn$timediff.deq<-as.difftime(jn$SampleStartTime.deq,units="mins")
  jn$timediff.split<-as.difftime(jn$SampleStartTime.split,units="mins")
  jn$timediff<-abs(jn$timediff.deq-jn$timediff.split)
  jn<-subset(jn, jn$timediff<=30)
  
  #need to return table of important columns
  #won't include lab comments- they are generic language set by AWQMS and not very helpful
  jn<-subset(jn,select=c("MLocID","Activity_Type","SampleStartDate","SampleStartTime.deq","Char_Name","StationDes.deq",
                         "Char_Speciation.deq","Result_status.deq","Result_status.split",
                         "Result_Type.deq","Result_Type.split","Result_Text.deq","Result_Text.split",
                         "Result_Unit.deq","Result_Unit.split","Method_Code.deq","Method_Code.split",
                         "Activity_Comment.deq","Result_Comment.deq","Activity_Comment.split","Result_Comment.split",
                         "MRLType.deq","MRLValue.deq","MRLUnit.deq","MRLType.split","MRLValue.split","MRLUnit.split",
                         "qctype","splitRPD","splitDiff"
                         ))
  
  
  
    
  return(jn)
}








#test dataset
#library(AWQMSdata)
#dat<-AWQMS_Data(startdate='2019-07-23',enddate='2019-07-23',project='Landfill Monitoring',org=c('OREGONDEQ','NEWBERG_LF(NOSTORETID)'))
#deq<-subset(dat,OrganizationID=='OREGONDEQ')
#splt<-subset(dat,OrganizationID=='NEWBERG_LF(NOSTORETID)')

#test<-splitcomp(deq,rvb)

#nomatch<-full_join(deq,rvb,by = c('MLocID',"SampleStartDate","Char_Name","Activity_Type"),suffix=c(".deq",".split") )
#nomatch<-subset(nomatch, is.na(OrganizationID.split)|is.na(OrganizationID.deq))
#nomatch<-subset(nomatch,select=c("OrganizationID.split","OrganizationID.deq",'MLocID',"SampleStartDate","Char_Name","Activity_Type"))


