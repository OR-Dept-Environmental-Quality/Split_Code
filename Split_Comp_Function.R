#split function

splitcomp<-function(deq,splt)
{
  #deq is ordeq dataset
  #splt is split dataset
  
  require(dplyr)
  require(stringr)
  source("NameandFraction.R")
  source("Unit_Convert_Function.R")
  
#####UNITS
#convert units so they match DEQ's data 
#get the units associated with the characteristics for deq and split data
dequnit<-subset(deq,select=c("Char_Name","Result_Unit"))
dequnit<-unique(dequnit)
spltunit<-subset(splt,select=c("Char_Name","Result_Unit"))
spltunit<-unique(spltunit)

#join deq units and split units together, only keep characteristics where units do not agree
unjn<-inner_join(dequnit,spltunit,by=c("Char_Name"),suffix=c(".deq",".splt"))
unjn<-subset(unjn, unjn$Result_Unit.splt!=unjn$Result_Unit.deq)

#get characteristics where we need to convert from mg to ug and also from ug to mg
#added tolower() to each line to make these case insensitive - DTB 1/6/25
#(the most likely units conversions needed, will add more as necessary)
mgug<-subset(unjn, tolower(unjn$Result_Unit.splt)=="mg/l" & tolower(unjn$Result_Unit.deq)=="ug/l")
ugmg<-subset(unjn, tolower(unjn$Result_Unit.splt)=="ug/l" & tolower(unjn$Result_Unit.deq)=="mg/l")
ngug<-subset(unjn, tolower(unjn$Result_Unit.splt)=="ng/l" & tolower(unjn$Result_Unit.deq)=="ug/l")
ugng<-subset(unjn, tolower(unjn$Result_Unit.splt)=="ug/l" & tolower(unjn$Result_Unit.deq)=="ng/l")

#if there are any rows in mgug or ugmg then run data through unit conversion function
#add ngug and ugng

if (nrow(mgug)!=0) {splt<-unit_conv(splt,mgug$Char_Name,mgug$Result_Unit.splt,mgug$Result_Unit.deq)} # changed the last two variables
if (nrow(ugmg)!=0) {splt<-unit_conv(splt,ugmg$Char_Name,ugmg$Result_Unit.splt,ugmg$Result_Unit.deq)} # from the text versions of the units
if (nrow(ngug)!=0) {splt<-unit_conv(splt,ngug$Char_Name,ngug$Result_Unit.splt,ngug$Result_Unit.deq)} # to be changed to the column in the
if (nrow(ugng)!=0) {splt<-unit_conv(splt,ugng$Char_Name,ugng$Result_Unit.splt,ugng$Result_Unit.deq)} # dataset that should be referenced - DTB 072925

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
  #jn<-inner_join(deq,splt, by = c('MLocID',"SampleStartDate","Char_Name","Activity_Type"),suffix=c(".deq",".split"))
  
  #In circumstances where one org collects a sample when the other doesn't, this line of code allows tables to be combined even when 
  #the stations don't line up. This means the unmatched stations will end up in the Non-Matched Data tab.
  jn<-left_join(deq,splt, by = c('MLocID',"SampleStartDate","Char_Name","Activity_Type"),suffix=c(".deq",".split"))
  


  #need to add a column to determine what type of QC is needed (eg. RPA, Diff, Microdiff)
  #difference is done when data is less than 5x MRL. don't always get MRL from split lab, use ours to be safe
  jn$qctype<-case_when(jn$Result_Operator.split=="<" & jn$Result_Numeric.deq < jn$MRLValue.split ~ NA,
                       jn$Result_Operator.deq=="<" &jn$Result_Numeric.split < jn$MRLValue.deq ~ NA,
                       jn$Char_Name=="Enterococcus"|jn$Char_Name=="Escherichia coli"|jn$Char_Name=="Fecal Coliform"|jn$Char_Name=="Total Coliform"~"Micro",
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
  jn<-subset(jn, jn$timediff<=60)
  
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
  
  #filter out rows were 8260 and 8270 are compared
  jn<-jn %>%
    filter(
      case_when(
        str_starts(Method_Code.deq, "8260") & !str_starts(Method_Code.split, "8260") ~ FALSE,
        str_starts(Method_Code.deq, "8270") & !str_starts(Method_Code.split, "8270") ~ FALSE,
        TRUE ~ TRUE)
    )  
  
    
  return(jn)
}








# test dataset
# library(AWQMSdata)
# dat<-AWQMS_Data(startdate='2023-07-05',enddate='2023-07-07',project='Effluent Monitoring',OrganizationID=c('OREGONDEQ','CITY_RAINIER(NOSTORETID)'))
# deq<-subset(dat,OrganizationID=='OREGONDEQ')
# splt<-subset(dat,OrganizationID=='CITY_RAINIER(NOSTORETID)')
# 
# test<-splitcomp(deq,splt)
# 
# nomatch<-full_join(deq,splt,by = c('MLocID',"SampleStartDate","Char_Name","Activity_Type"),suffix=c(".deq",".split") )
# nomatch<-subset(nomatch, is.na(OrganizationID.split)|is.na(OrganizationID.deq))
# nomatch<-subset(nomatch,select=c("OrganizationID.split","OrganizationID.deq",'MLocID',"SampleStartDate","Char_Name","Activity_Type"))


