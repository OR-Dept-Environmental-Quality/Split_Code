#Script to analyze Inter-Laboratory Split Sample data
#get data from Landfills and WasteWater Treatment Plants, need to compare it with sample that DEQ took
#put all data into AWQMS, pull data from AWQMS and the compare in R
#use a Shiny app to pull out data, view comparison, then use R Markdown to create a report

print("Initial data queries may take a few minutes.")

library(shiny)
library(AWQMSdata)
library(shinybusy)

#attempt to turn off scientific notation
options(scipen=999)

#in case the shinybusy package needs to be installed/reinstalled
#remotes::install_github("dreamRs/shinybusy")

#Query the valid values

# Check to see if saved cache of data exists. If it does not, or is greater than
# 7 days old, query out stations and organizations and save the cache
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
      #Split entity
      textInput("org",
                label="Split Organization"),
      #add line
      tags$hr(),
      
      # Start Date (make start date six months ago)
      dateInput("startd",
                label = "Select Start Date",
                min = '1949-09-15',
                value = Sys.Date()-182),
      
      # End date
      dateInput("endd",
                label = "Select End Date",
                min = '1900-1-1'),
      
      #Orgs
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
      
      #add action button, idea is to not run query until the button is clicked)
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
        tabPanel("Split Comparison",dataTableOutput("splitData"))
      )
    )
  ),
  
  add_busy_spinner(spin="fading-circle")
  )
  
  #define server logic required to display query
  
  server<-function(input,output) {
    
    #have to make dates into strings, otherwise they come out as funny numbers
    #all other variables are reactive 'as is' except for reject button
    #isolate data so that you have to click a button so that it runs the query using eventReactive.
    data<-eventReactive(input$goButton,{
      
      rstdt<-toString(sprintf("%s",input$startd))
      rendd<-toString(sprintf("%s",input$endd))
      
      #actual query for data
      dat<-AWQMS_Data(startdate=rstdt,enddate=rendd,org=c(input$orgs,"OREGONDEQ"),project=c(input$project),station=c(input$monlocs))
      
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
                        Result,Result_Unit,Method_Code,Method_Context,Analytical_Lab,Activity_Comment,
                        Result_Comment,lab_Comments,QualifierAbbr,MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit))
      viewDEQ
    })
    
    #table of split org's data
    orgData<-eventReactive(input$goButton,{
      viewOrg<-subset(data(),OrganizationID==input$orgs)
      viewOrg
    })
    
    #table of Org Data
    output$orgData<-renderDataTable({
      subset(orgData(),select=c(Org_Name,Project1,MLocID,StationDes,Lat_DD,Long_DD,act_id,SampleStartDate,SampleStartTime,SampleStartTZ,
                        SampleMedia,SampleSubmedia,Char_Name,Char_Speciation,Sample_Fraction,CASNumber,Result_status,Result_Type,
                        Result,Result_Operator,Result_Unit,Method_Code,Method_Context,Analytical_Lab,Activity_Comment,
                        Result_Comment,lab_Comments,QualifierAbbr,MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit))
    })
  }

  #split comparison
  
  
  #run application
  shinyApp(ui=ui,server=server)
  