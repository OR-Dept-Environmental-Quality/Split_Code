#create function that flags QC issue

qc_issue<-function(x) {
  #x is dataframe pulled from AWQMS that has gone through split comparison function
  
  require(dplyr)
  
  #put TRUE if issue, FALSE if no issue
  x$issue<-case_when(x$splitRPD>30 ~ "TRUE",
                     x$splitDiff>(2*x$MRLValue.deq) & !(x$Char_Name=="pH"|x$Char_Name=="Enterococcus"|x$Char_Name=="Escherichia coli"|x$Char_Name=="Fecal Coliform"|x$Char_Name=="Total Coliform") ~ "TRUE",
                     x$splitDiff>0.5 & x$Char_Name=="pH"~"TRUE",
                     x$splitDiff>0.6 & (x$Char_Name=="Enterococcus"|x$Char_Name=="Escherichia coli"|x$Char_Name=="Fecal Coliform"|x$Char_Name=="Total Coliform")~"TRUE")
  
  return(x)
}

  
  #library(AWQMSdata)
  #test<-AWQMS_Data(startdate='2018-05-01', enddate='2018-05-03',org=c("OREGONDEQ","RVBND_LF(NOSTORETID)"),project = "Landfill Monitoring")
  
  #deqData<-subset(test,OrganizationID=="OREGONDEQ")
  #viewOrg<-subset(test,OrganizationID=="RVBND_LF(NOSTORETID)")
  #try1<-splitcomp(deqData,viewOrg)
  #try2<-qc_issue(try1)
  #see<-subset(try2,issue=="TRUE")