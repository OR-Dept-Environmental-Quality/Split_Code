library(tidyverse)
library(AWQMSdata)
options(kableExtra.latex.load_packages=FALSE)
library(kableExtra)
library(tinytex)
library(htmltools)

# Load functions
source("ParamGrp_Function.R")
source("QC_Issue.R")
source("Split_Comp_Function.R")
source("NameandFraction.R")
source("Unit_Convert_Function.R")


# Enter parameters for data pull
StDate <- '2022-11-14'
EndDate <- '2022-11-16'
Project <- 'Landfill Monitoring'
SplOrg <- 'LF_STJOHNS'

# Pull data from AWQMS
All_Data <- AWQMS_Data(startdate = StDate, enddate = EndDate, OrganizationID = c(SplOrg, 'OREGONDEQ'),
                       project = Project, filterQC = FALSE) 

# Modify data from AWQMS
dat <- All_Data %>%
  filter(MLocID != '10000-ORDEQ') %>%
  mutate(MLocID = case_when(
    str_detect(Activity_Type, "Quality Control") ~ paste0(MLocID, "_Dup"),
    TRUE ~ MLocID
  ))
  

# Create tables of DEQ and Split data (effectively lines 169-175 and 184-189 in app.R)
deq <- subset(dat, OrganizationID=='OREGONDEQ') %>%
  select(OrganizationID,Project1,MLocID,StationDes,Lat_DD,Long_DD,act_id,Activity_Type,SampleStartDate,SampleStartTime,SampleStartTZ,
         SampleMedia,SampleSubmedia,Char_Name,Char_Speciation,Sample_Fraction,CASNumber,Result_status,Result_Type,
         Result_Text,Result_Numeric,Result_Operator,Result_Unit,Method_Code,Method_Context,Analytical_Lab,Activity_Comment,
         Result_Comment,QualifierAbbr,MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit) 

splt <- subset(dat, OrganizationID==SplOrg) %>%
  select(OrganizationID,Project1,MLocID,StationDes,Lat_DD,Long_DD,act_id,Activity_Type,SampleStartDate,SampleStartTime,SampleStartTZ,
         SampleMedia,SampleSubmedia,Char_Name,Char_Speciation,Sample_Fraction,CASNumber,Result_status,Result_Type,
         Result_Text,Result_Numeric,Result_Operator,Result_Unit,Method_Code,Method_Context,Analytical_Lab,Activity_Comment,
         Result_Comment,QualifierAbbr,MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit)

# Format data for the stations table from Split Report
meta<-unique(subset(dat,select=c(MLocID,StationDes,SampleStartDate,SampleStartTime))) %>%
  mutate(SampleStartDate = format(as.Date(SampleStartDate), format = "%m-%d-%Y"), 
         SampleStartTime = format(as.POSIXct(SampleStartTime, format = "%H:%M:%OS"), format = "%H:%M:%S"))

# Create stations table (key difference is format = html to view in Rstudio, not format = latex to view in PDF)
StationTable <- kable(meta,format='html',caption="Split Sample Information",booktabs=TRUE,row.names=FALSE)%>%
  kable_styling(latex_options=c("HOLD_position","striped"))

# Display table in RStudio Viewer
browsable(StationTable)

# Step through the Split_Comp_Function.R code
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
#add ng/L as well
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
if (nrow(ugng)!=0) {splt<-unit_conv(splt,ugng$Char_Name,ugng$Result_Unit.splt,ugng$Result_Unit.deq)} # dataset that should be referenced

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
jn<-inner_join(deq,splt, by = c('MLocID',"SampleStartDate","Char_Name","Activity_Type"),suffix=c(".deq",".split")) %>%
  relocate(c(MDLValue.deq, MRLValue.deq, Result_Numeric.split, MDLValue.split, MRLValue.split), .after = Result_Unit.deq)



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

#filter out rows were 8260 and 8270 are compared
jn<-jn %>%
  filter(
    case_when(
      str_starts(Method_Code.deq, "8260") & !str_starts(Method_Code.split, "8260") ~ FALSE,
      str_starts(Method_Code.deq, "8270") & !str_starts(Method_Code.split, "8270") ~ FALSE,
      TRUE ~ TRUE)
    )

unique_combos <- jn %>% 
  distinct(Char_Name,Method_Code.deq,Method_Code.split)

n_unique <- nrow(unique_combos)

# Change dataframe name to match SplitReport_RMarkdown.Rmd
spltcomp <- jn

#add issue column 
spltcomp<-qc_issue(spltcomp)

#add j flag to data below MRL for split data reported to MDL
spltcomp$Result_Text.split<-ifelse(!is.na(spltcomp$MRLValue.split) & substr(spltcomp$Result_Text.split,start=1,stop=1)!="<" & (as.numeric(spltcomp$Result_Text.split)<=spltcomp$MRLValue.split),
                                   paste0(spltcomp$Result_Text.split," J"),
                                   spltcomp$Result_Text.split)
#add in parameter groupings

spltcomp<-param_grp(spltcomp)

#make summary tables conditional, if WWTP split, just have the one table. If it is a landfill split, break it out into groups

### "Landfill Monitoring"

  #get subset of columns, combine characteristic name and speciation
  spltrp<-spltcomp %>%
    select(MLocID,SampleStartDate,SampleStartTime.deq,Char_Name,Char_Speciation.deq,Result_Text.deq,Result_Text.split,
                                   Result_Unit.deq,Method_Code.deq,Method_Code.split,MRLValue.deq,MRLValue.split,splitRPD,splitDiff,issue,param_grp) %>% 
    mutate(SampleStartDate = format(as.Date(SampleStartDate), format = "%m-%d-%Y"), 
           SampleStartTime.deq = format(as.POSIXct(SampleStartTime.deq, format = "%H:%M:%OS"), 
                                        format = "%H:%M:%S"))%>% 
    
    
    #changed to add param_grp
    mutate(Char_Name=paste(Char_Name,' ',ifelse(is.na(Char_Speciation.deq),' ',Char_Speciation.deq))) %>%
    select(-Char_Speciation.deq)
  
  #reformat date
  spltrp$SampleStartDate<-format(spltrp$SampleStartDate,format="%m-%d-%Y")
  
  
  
  #change name of "Biochemical oxygen demand, standard conditions" to "BOD-5" and shorten various other names
  spltrp<-spltrp%>%
    mutate(Char_Name=str_replace(Char_Name,"Biochemical oxygen demand, standard conditions","BOD-5"))%>%
    mutate(Char_Name=str_replace(Char_Name,"Total suspended solids","TSS"))%>%
    mutate(Char_Name=str_replace(Char_Name,"Chemical oxygen demand","COD"))%>%
    mutate(Char_Name=str_replace(Char_Name,"Hardness, Ca, Mg, Total Recoverable","Hardness"))
  
  #this table is huge and unwieldy, try splitting it down by station/date/time/parameter group
  #need to concatenate a column with MLocID and param_grp together so that it splits down accordingly
  spltrp$stparam<-paste0(spltrp$MLocID,spltrp$param_grp)
  lstsplt<-split(spltrp,spltrp$stparam)
  
  for (i in 1:length(lstsplt)){
    #get station, date, and time as variables so we can put them in the table name
    station<-unique(lstsplt[[i]]$MLocID)
    date<-unique(lstsplt[[i]]$SampleStartDate)
    time<-unique(lstsplt[[i]]$SampleStartTime.deq)
    grp<-unique(lstsplt[[i]]$param_grp)
    
    #remove station, date, and time columns
    lstsplt[[i]]$MLocID<-NULL
    lstsplt[[i]]$SampleStartDate<-NULL
    lstsplt[[i]]$SampleStartTime.deq<-NULL
    lstsplt[[i]]$param_grp<-NULL
    lstsplt[[i]]$stparam<-NULL
    
    #need to convert MRLs to character so we don't get a bunch of trailing zeroes (sig figs important), but need to calculate QC criteria
    #so we create MRLNum variable to use and then hide the column later
    lstsplt[[i]]$MRLValue.split<-as.character(lstsplt[[i]]$MRLValue.split)
    lstsplt[[i]]$MRLValue.deq<-as.character(lstsplt[[i]]$MRLValue.deq)
    
    if (grp == "Physical Chem") {
      extra_text <- " \\newline DEQ Nitrate + Nitrite compared to Split Org Nitrate where applicable"
    } else {
      extra_text <- ""
    }
    
    print(kable(lstsplt[[i]],format='html', col.names=c("Analyte","DEQ","Split Lab","Unit","DEQ","Split Lab","DEQ","Split Lab","RPD","Diff","Issue"),
                caption=paste(station,',',date,',',time,grp),booktabs=TRUE,row.names=FALSE,longtable=TRUE)%>%
            kable_styling(latex_options=c("HOLD_position","striped","repeat_header"),font_size=9)%>%
            add_header_above(c(" "=1,"Result"=2," "=1,"Method"=2,"LOQ"=2))%>%
            column_spec(1,width="10em") %>%
            column_spec(5:6,width="5em") %>%
            #hide last column, don't want it shown
            column_spec(11,width="0em",color="white") %>%
            row_spec(which(lstsplt[[i]]$issue=="TRUE"),bold=TRUE)
    )
  }
  
