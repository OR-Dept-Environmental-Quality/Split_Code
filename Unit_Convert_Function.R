#' Unit Transform Function
#'
#' Performs unit conversions to standardize units.
#' originally designed for Copper BLM, so focuses on temperature, and ug-mg-ng conversions
#' This function is used in the CuBLM_Transform function, and doesn't need to be run
#' independently.
#' Written by Aliana Britson
#'
#' @param x Dataframe from AWQMSdata function
#' @param char Characteristics to convert
#' @param unit Units to convert from
#' @param conv Units to convert into
#' @return Dataframe with results converted
#' @examples
#'  unit_conv(df,c("Calcium","Chloride","Magnesium","Potassium","Sodium","Sulfate","Organic carbon","Total Sulfate","Sulfide"),"ug/l","mg/l")
#'  unit_conv(df,"Copper","mg/l","ug/l")
#' @export
#'


unit_conv<-function(x,char,unit,conv){
    #x is dataset,char is characteristics,
  #unit is what units the data is currently in,
  #conv is what we want the units to be



  #convert Result_Numeric
  x$Result_Numeric<-dplyr::case_when(x$Char_Name %in% char & tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="deg f" & tolower(conv)=="deg c"~((x$Result_Numeric-32)*0.5556),
                              x$Char_Name %in% char & tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="mg/l"~(x$Result_Numeric*0.001),
                              x$Char_Name %in% char & tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="mg/l" & tolower(conv)=="ug/l"~(x$Result_Numeric*1000),
                              x$Char_Name %in% char & tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="ng/l" & tolower(conv)=="ug/l"~(x$Result_Numeric/1000),
                              x$Char_Name %in% char & tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="ng/l"~(x$Result_Numeric*1000),
                              !(x$Char_Name %in% char &  tolower(x$Result_Unit)==tolower(unit))~x$Result_Numeric)

  #change unit to new unit
  x$Result_Unit<-dplyr::case_when(x$Char_Name %in% char &  tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="deg f" & tolower(conv)=="deg c"~"deg C",
                           x$Char_Name %in% char &  tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="mg/l"~"mg/l",
                           x$Char_Name %in% char &  tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="mg/l" & tolower(conv)=="ug/l"~"ug/l",
                           x$Char_Name %in% char &  tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="ng/l" & tolower(conv)=="ug/l"~"ug/l",
                           x$Char_Name %in% char &  tolower(x$Result_Unit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="ng/l"~"ng/l",
                           !(x$Char_Name %in% char &  tolower(x$Result_Unit)==tolower(unit))~x$Result_Unit)

   #change MDL and MRL values and units
  x$MDLValue<-dplyr::case_when(x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="deg f" & tolower(conv)=="deg c"~((x$MDLValue-32)*0.5556),
                        x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="mg/l"~(x$MDLValue*0.001),
                        x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="mg/l" & tolower(conv)=="ug/l"~(x$MDLValue*1000),
                        x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="ng/l" & tolower(conv)=="ug/l"~(x$MDLValue/1000),
                        x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="ng/l"~(x$MDLValue*1000),
                        !(x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit))~x$MDLValue)

  x$MDLUnit<-dplyr::case_when(x$Char_Name %in% char& tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="deg f"& tolower(conv)=="deg c"~"deg C",
                       x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="mg/l"~"mg/l",
                       x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="mg/l" & tolower(conv)=="ug/l"~"ug/l",
                       x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="ng/l" & tolower(conv)=="ug/l"~"ug/l",
                       x$Char_Name %in% char & tolower(x$MDLUnit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="ng/l"~"ng/l",
                       !(x$Char_Name %in% char&tolower(x$MDLUnit)==tolower(unit))~x$MDLUnit)

  x$MRLValue<-dplyr::case_when(x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="deg f"& tolower(conv)=="deg c"~((x$MRLValue-32)*0.5556),
                        x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="mg/l"~(x$MRLValue*0.001),
                        x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="mg/l" & tolower(conv)=="ug/l"~(x$MRLValue*1000),
                        x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="ng/l" & tolower(conv)=="ug/l"~(x$MRLValue/1000),
                        x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="ng/l"~(x$MRLValue*1000),
                        !(x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit))~x$MRLValue)

  x$MRLUnit<-dplyr::case_when(x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="deg f"& tolower(conv)=="deg c"~"deg C",
                              x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="ug/l"& tolower(conv)=="mg/l"~"mg/l",
                              x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="mg/l"& tolower(conv)=="ug/l"~"ug/l",
                              x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="ng/l" & tolower(conv)=="ug/l"~"ug/l",
                              x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit) & tolower(unit)=="ug/l" & tolower(conv)=="ng/l"~"ng/l",
                              !(x$Char_Name %in% char & tolower(x$MRLUnit)==tolower(unit))~x$MRLUnit)


  #we changed Result_Numeric, now we want to make sure that Result shows the same value by replacing it
  #with result numeric concatenated with < for NDs (or > when it exists)
  x$Result_Text<-paste0(ifelse(!(x$Result_Operator %in% "="),x$Result_Operator,""),x$Result_Numeric)

  return(x)

}


