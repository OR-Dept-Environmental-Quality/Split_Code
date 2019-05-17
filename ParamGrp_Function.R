##create a function that groups analytes into categories by creating a column that identifies the analyte type
#this will help organize data in charts and tables

param_grp<-function(x){
  #x is dataframe with list of analytes from AWQMS data pull
  #note that this will need to be done before namefrac function is used
  require(dplyr)
  
  #groupings
  
  #need to add all total and dissolved cases to metals that result from namefrac, or else they won't show up
  metals<-c("Cyanide","Cyanides amenable to chlorination (HCN & CN)","Aluminum","Iron","Lead","Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Arsenic, Inorganic",
            "Beryllium","Cadmium","Chromium","Copper","Zinc","Selenium","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Calcium","Cobalt",
            "Lithium","Beryllium","Manganese","Magnesium","Molybdenum","Potassium","Sodium","Uranium","Vanadium","Barium","Boron")
  
  voc<-c("1,1,1,2-Tetrachloroethane", "1,1,1-Trichloroethane", "1,1,2,2-Tetrachloroethane", "1,1,2-Trichloroethane", "1,1-Dichloroethane", 
         "1,1-Dichloroethylene", "1,1-Dichloropropene", "1,2,3-Trichlorobenzene","1,2,3-Trichloropropane", "1,2,4-Trimethylbenzene", 
         "1,2-Dibromo-3-chloropropane", "1,2-Dichloroethane", "1,2-Dichloropropane","1,3-Dichlorobenzene", "1,3-Dichloropropane",
         "1,3,5-Trimethylbenzene", "2,2-Dichloropropane","cis-1,2-Dichloroethylene","trans-1,2-Dichloroethylene", "Acetone", 
         "Benzene", "Bromobenzene", "Carbon disulfide", "Carbon tetrachloride", "Chlorobenzene", "Chlorodibromomethane", "Chloroethane", 
         "Chloroform", "Chloromethane", "cis-1,3-Dichloropropene", "Cumene", "Dibromodichloromethane", "Dichlorodifluoromethane",
         "Dibromomethane", "Ethylbenzene", "p-Cymene","tert-Butylbenzene","Vinyl chloride","Tribromomethane","Tetrachloroethene",
         "Ethylene dibromide","Fluorotrichloromethane", "Halon 1011", "Hexachlorobutadiene", "m-Dichlorobenzene", "Methyl bromide", "Methyl ethyl ketone", 
         "Methyl isobutyl ketone", "Methyl tert-butyl ether", "Methylene chloride", "m-Xylene", "Naphthalene", "n-Butylbenzene","trans-1,3-Dichloropropene",
         "n-Propylbenzene", "o-Chlorotoluene", "o-Dichlorobenzene", "o-Xylene", "p-Chlorotoluene", "p-Dichlorobenzene", "p-Xylene", 
         "sec-Butylbenzene","Styrene","Toluene","Trichloroethene (TCE)", "o-Xylene, mixt. with m-xylene and p-xylene","Xylene","m,p-Xylene")
  
  semiv<-c("1,2,4,5-Tetrachlorobenzene", "1,2,4-Trichlorobenzene", "1-Methylphenanthrene", "1-Methylpyrene", "2,3,4,6-Tetrachlorophenol", 
           "2,3,5,6-Tetrachlorophenol", "2,4,5-Trichlorophenol", "2,4,6-Trichlorophenol", "2,4-Dichlorophenol", "2,4-Dimethylphenol", 
           "2,4-Dinitrophenol", "2,4-Dinitrotoluene", "2,6-Dichlorophenol", "2,6-Dinitrotoluene", "2-Chloronaphthalene", 
           "3,3'-Dichlorobenzidine", "4,6-Dichloro-o-cresol", "Acenaphthene", "Acenaphthylene", "Anthracene", "Azobenzene", "Benz[a]anthracene", 
           "Benzidine", "Benzo(b)fluoranthene", "Benzo[a]pyrene", "Benzo[ghi]perylene", "Benzo[k]fluoranthene", "Bis(2-chloroethoxy)methane", 
           "Bis(2-chloroethyl) ether", "Bis(2-chloroisopropyl) ether", "Butyl benzyl phthalate", "Chrysene", "Di(2-ethylhexyl) adipate", 
           "Di(2-ethylhexyl) phthalate", "Dibenz[a,h]anthracene", "Dibenzofuran", "Dibutyl phthalate", "Diethyl phthalate", "Dimethyl phthalate",
           "Di-n-octyl phthalate", "Fluoranthene", "Fluorene", "Hexachlorobenzene",  "Hexachlorobutadiene", "Hexachlorocyclopentadiene", 
           "Hexachloroethane", "Indeno[1,2,3-cd]pyrene", "Isophorone", "m-Dichlorobenzene", "Naphthalene")
  
  phys<-c("Conductivity",	"Depth, bottom","Depth, from ground surface to well water level","Dissolved oxygen (DO)",
          "Oxidation reduction potential (ORP)", "pH","Temperature, water","Nitrate + Nitrite","Ammonium","Organic carbon",
          "Chemical oxygen demand","Alkalinity, total","Ammonia","Total dissolved solids","Total suspended solids","Total hardness",
          "Hardness, Ca, Mg","Sulfate","Chloride","Silica")
  
  #create group column in dataframe
  x$param_grp<-case_when(x$Char_Name %in% metals ~"Metals",
                         x$Char_Name %in% voc ~ "VOCs",
                         x$Char_Name %in% phys ~ "Physical Chem",
                         x$Char_Name %in% semiv ~ "Semivolatiles")
  
  return(x)
}

#library(AWQMSdata)
#test<-AWQMS_Data(startdate='2018-05-01', enddate='2018-05-03',org=c("OREGONDEQ","RVBND_LF(NOSTORETID)"),project = "Landfill Monitoring")

#try<-param_grp(test)

#unsort<-unique(subset(try,is.na(param_grp),select=c(Char_Name,param_grp,Method_Code)))

#library(openxlsx)
#write.xlsx(unsort,"E:/UnSort.xlsx")