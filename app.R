#Script to analyze Inter-Laboratory Split Sample data
#get data from Landfills and WasteWater Treatment Plants, need to compare it with sample that DEQ took
#put all data into AWQMS, pull data from AWQMS and the compare in R
#use a Shiny app to pull out data, view comparison, then use R Markdown to create a report

#To capture any updates to the AWQMSdata package, run this line periodically
#devtools::install_github("TravisPritchardODEQ/AWQMSdata",dependencies = TRUE, force = TRUE, upgrade = FALSE)

print("Initial data queries may take a few minutes.")

library(shiny)
library(AWQMSdata)
library(shinybusy)
library(openxlsx)
library(dplyr)
library(DT)
source("Split_Comp_Function.R")
source("ParamGrp_Function.R")
source("NameandFraction.R")

#attempt to turn off scientific notation
options(scipen=999)

#in case the shinybusy package needs to be installed/reinstalled 
#(this generates the "busy" spinning wheel on the webpage)
#remotes::install_github("dreamRs/shinybusy")

#Query the valid values

# Check to see if saved cache of data exists. If it does not, or is greater than
# 7 days old, query out stations, projects, and organizations and save the cache
if(!file.exists("query_cache.RData") | 
   difftime(Sys.Date() ,file.mtime("query_cache.RData") , units = c("days")) > 7){
  
  organization <- AWQMS_Orgs()
  organization <- organization$OrganizationID
  organization <- sort(organization)
  
  station <- AWQMS_Stations()
  station <- station$MLocID
  station <- sort(station)
  
  project<-AWQMS_Projects()
  project<-sort(project$Project)
  

  save(project, station, organization, file = 'query_cache.RData')
} else {
  load("query_cache.RData")
}

#Define UI
ui<-fluidPage(
  #sidebar with parameter inputs
  sidebarLayout(
    sidebarPanel(
      #permit #
      textInput("permittee",
                label="Permit Number"),
      #report to
      textInput("report",
                label="Report To:"),
      # Add line
      tags$hr(),
      #Add break
      tags$br(),
      # Start Date (make start date six months ago)
      dateInput("startd",
                label = "Select Start Date",
                min = '1949-09-15',
                value = Sys.Date()-182),
      
      # End date
      dateInput("endd",
                label = "Select End Date",
                min = '1900-1-1'),
      
      #split organizations
      selectizeInput("orgs",
                     "Select Split Organization",
                     choices = organization,
                     multiple = FALSE),
      #projects
      selectizeInput("project",
                     "Select Project",
                     choices=project,
                     multiple=TRUE),
      
      # Monitoring locations 
      selectizeInput("monlocs",
                     "Select Monitoring Locations",
                     choices = station,
                     multiple = TRUE), 
      
      #add action button, idea is to not run query until the button is clicked, otherwise it tries to query all 
      #AWQMS data at once and crashes
      actionButton("goButton","Run Query"),
      #add an excel download button
      downloadButton('downloadData', 'Download Data'),
      #add Report download button
      downloadButton('report','Download PDF Report')
    ),
    
    #setup main panel
    mainPanel(
      h1("Inter-Laboratory Split Reporting Tool"),
      #add line
      tags$hr(),
      #add break
      tags$br(),
      
      tabsetPanel(
        #data
        tabPanel("All Data",dataTableOutput("data")),
        #DEQ data
        tabPanel("DEQ Data",dataTableOutput("deqData")),
        #Split org data
        tabPanel("Split Organization Data",dataTableOutput("orgData")),
        #Comparison
        tabPanel("Split Comparison",dataTableOutput("splitData")),
        #table of non matching analytes
        tabPanel("Non Matching Data",dataTableOutput("nomatchdeq")),
        #Conclusions and Summary
        tabPanel("Review",
                 textAreaInput("comp","Comparison",width='1000px',height='400px'),
                 textAreaInput("conc","Conclusion",width='1000px',height='400px'))
      )
    )
  ),
  
  add_busy_spinner(spin="fading-circle")
  )
  
  #define server logic required to display query
  
  server<-function(input,output) {
    
    #have to make dates into strings, otherwise they come out as funny numbers
    #all other variables are reactive 'as is'
    #isolate data so that you have to click a button so that it runs the query using eventReactive.
    data<-eventReactive(input$goButton,{
      
      rstdt<-toString(sprintf("%s",input$startd))
      rendd<-toString(sprintf("%s",input$endd))
      
      #actual query for data
      dat<-AWQMS_Data(startdate=rstdt,enddate=rendd,org=c(input$orgs,"OREGONDEQ"),project=c(input$project),station=c(input$monlocs), filterQC = TRUE)
      
      dat
      
    })
    
    #show data (for QC purposes for now)
    output$data<-renderDataTable({
      data()
    })
    
    #make table of DEQ data
    deqData<-eventReactive(input$goButton,{
      deqData<-subset(data(),OrganizationID=="OREGONDEQ")
      deqData
      
    })
    
    #table of DEQ data for Shiny app view
    output$deqData<-renderDataTable({
      viewDEQ<-subset(deqData(),select=c(Org_Name,Project1,MLocID,StationDes,Lat_DD,Long_DD,act_id,SampleStartDate,SampleStartTime,SampleStartTZ,
                        SampleMedia,SampleSubmedia,Char_Name,Char_Speciation,Sample_Fraction,CASNumber,Result_status,Result_Type,
                        Result_Text,Result_Unit,Method_Code,Method_Context,Analytical_Lab,Activity_Comment,
                        Result_Comment,QualifierAbbr,MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit))
      viewDEQ
    })
    
    #table of split org's data
    orgData<-eventReactive(input$goButton,{
      viewOrg<-subset(data(),OrganizationID==input$orgs)
      viewOrg
    })
    
    #table of Org Data for shiny app view
    output$orgData<-renderDataTable({
      subset(orgData(),select=c(Org_Name,Project1,MLocID,StationDes,Lat_DD,Long_DD,act_id,SampleStartDate,SampleStartTime,SampleStartTZ,
                        SampleMedia,SampleSubmedia,Char_Name,Char_Speciation,Sample_Fraction,CASNumber,Result_status,Result_Type,
                        Result_Text,Result_Operator,Result_Unit,Method_Code,Method_Context,Analytical_Lab,Activity_Comment,
                        Result_Comment,QualifierAbbr,MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit))
    })


  #split comparison
  splitData<-eventReactive(input$goButton,{
    splitdat<-splitcomp(deq=deqData(),splt=orgData())
    splitdat
  })
  
  output$splitData<-renderDataTable({splitData()})
  
  #create table of nonmatching data
  
  nomatch<-eventReactive(input$goButton,{
    #combine name and fraction for deq and split data for better comparison
    deq<-namefrac(deqData())
    org<-namefrac(orgData())
      #do a full join to get everything together
    nomatch<-full_join(deq,org,by = c('MLocID',"SampleStartDate","Char_Name","Activity_Type"),suffix=c(".deq",".split"))
    #pull out the rows that don't have a match
    nomatch<-subset(nomatch, is.na(OrganizationID.split)|is.na(OrganizationID.deq))
    #just get a few rows we want
    nomatch<-subset(nomatch,select=c("OrganizationID.deq","OrganizationID.split",'MLocID',"SampleStartDate","Char_Name","Activity_Type"))
    nomatch
  })

  output$nomatchdeq<-renderDataTable({nomatch()})
  
  #generate workbook to download, easier to work with the data if needed
  dwnld<-eventReactive(input$goButton,{
    wb<-createWorkbook()
    
    #all data
    addWorksheet(wb,"All Data")
    writeDataTable(wb,"All Data",x=data(),tableStyle="none")
    #DEQ data
    addWorksheet(wb,"DEQ Data")
    writeDataTable(wb,"DEQ Data",x=deqData(),tableStyle="none")
    #split data
    addWorksheet(wb,"Split Data")
    writeDataTable(wb,"Split Data",x=orgData(),tableStyle="none")
    #comparsion
    addWorksheet(wb,"Comparison")
    writeDataTable(wb,"Comparison",x=splitData(),tableStyle="none")
    #nonmatching
    addWorksheet(wb,"Non Match")
    writeDataTable(wb,"Non Match",x=nomatch(),tableStyle="none")
    
    wb
  })
  
  # Download button- only works in Chrome
  # gives an excel workbook with multiple sheets
  #set to give NAs as blank cells
  output$downloadData <- downloadHandler(
    
    filename = function() {paste("Landfill_Split_",input$orgs,"_",Sys.Date(),".xlsx", sep="")},
    content = function(file) {
      #sheet with query parameters
      saveWorkbook(dwnld(),file)
    })
  
  #R markdown report
  output$report<-downloadHandler(
    filename = function() {paste(Sys.Date() ,"_SplitReport.pdf", sep="")},
    content=function(file){
      
      #create a file in a temporary directory
      tempReport<-file.path(tempdir(),"SplitReport_Rmarkdown.Rmd")
      #copy our report to the temporary directory file
      file.copy("SplitReport_Rmarkdown.Rmd",tempReport,overwrite=TRUE)
      
      #create list of characteristics
      #set up parameters to pass to our Rmd document
      params<-list(data=data(),
                   split=splitData(),
                   org=orgData(),
                   deq=deqData(),
                   perm=input$permittee,
                   rep=input$report,
                   comp=input$comp,
                   conc=input$conc)
      
      rmarkdown::render(tempReport, output_file=file,
                        params=params,
                        clean=TRUE,
                        envir=new.env(parent= globalenv())
                        
      )
    }
  )
  
  }
  
  
  #run application
  shinyApp(ui=ui,server=server)
  