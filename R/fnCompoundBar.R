#' Produces a compound bar chart by grade, stage (for PTs and IDS), assessor, programme (for BDS & DTH only), station/substn
#' (ISCEs), gender, ethnicity or disability.
#' 
#' @description This function takes two columns of data and produces a compound bar chart with each
#' bar (for variable 1) split by percentage proportions (or frequency values) for variable 2.
#'
#' @usage fnCompoundBar(df, xName=NULL, yName=NULL, percent="Yes", forceSchemeX=NULL, forceSchemeY=NULL)
#'       
#' @param df The name of the data frame.
#' @param xName The column name to group by on the x-axis. Provide column number or name (in quotation marks if column name).
#' @param yName The column name to split the bars by. Provide column number or name (in quotation marks if column name).
#' @param percent Defaults to "Yes" for percentage proportion bars; change to percent="No" for frequency bars.
#' @param forceSchemeX/forceSchemeY Optional. For use when the group scheme determined is incorrect. For "USE" but "UBSE" is required,
#'  "PF" but "PFE" is required, "CIN" but "CN", or Station/Stage but Stage/Station is required, add forceSchemeX/Y="Y".
#'  For Assessor/Stage/Station but Domain is required, add forceSchemeX/Y="D". 
#' @param incl.N By default the sample size will show at the base of each bar where not all sizes are equal. These can be removed
#'  by adding incl.N="No".   
#' 
#' @note
#' This function uses \code{\link[ggplot2]{ggplot2}} for the graphics and outputs a ggplot object which can be
#' customised.
#' 
#' @examples
#' Basic usage: Examples use dataExample in psychometricsPSMD package. 
#' 
#' fnCompoundBar(dataExample,"Stage","Grade.UBSE")
#' 
#' fnCompoundBar(dataExample,"Grade.PF","Gender",percent="No")
#'
#' fnCompoundBar(dataExample,"Assessor.ID","Grade.CNINC")
#'
#' fnCompoundBar(dataExample,5,11,forceSchemeY="UBSE")
#' 
#' fnCompoundBar(dataExample,"Grade.PF","Disability","No",forceSchemeX="PFE")
#' 
#' @author Jo Cockerill, \email{jo.cockerill@plymouth.ac.uk}
#'
#' @export

########################################

fnCompoundBar<-function(df, xName=NULL, yName=NULL, percent="Yes", forceSchemeX=NULL, forceSchemeY=NULL, incl.N="Yes"){
  
  xAxis<-df[[xName]]
  yAxis<-df[[yName]]
  data1<-data.frame(xAxis,yAxis)
  data1<-data1[complete.cases(data1),] 
  if(is.null(forceSchemeX)){forceSchemeX<-""}
  if(is.null(forceSchemeY)){forceSchemeY<-""}
  
  # remove trailing spaces and convert to character strings
  data1<-data.frame(lapply(data1, trimws), stringsAsFactors=FALSE)
  
  # Create table of numbers and covert to dataframe (long)
  myTable<-table(data1$yAxis,data1$xAxis)
  dataNumbers<-as.data.frame(myTable)
  colnames(dataNumbers)<-c("yAxis","xAxis","Number")
  dataNumbers$xAxis<-as.character(dataNumbers$xAxis)
  dataNumbers$yAxis<-as.character(dataNumbers$yAxis)
  
  # Create a table of percentage proportions and covert to dataframe (long)  
  myPropTable<-fnRound(prop.table(myTable,2)*100,1)
  dataPlot<-as.data.frame(myPropTable)
  colnames(dataPlot)<-c("yAxis","xAxis","Prop")
  dataPlot$xAxis<-as.character(dataPlot$xAxis)
  dataPlot$yAxis<-as.character(dataPlot$yAxis)
  
  # Add Numbers column to the dataPlot df
  Number<-dataNumbers$Number
  dataPlot<-cbind(dataPlot,Number)
  
  # Amend character strings for NI/Needs Improvement/Needs improvement grades (as substr,1,1 could be the same for two variables)   
  toChangeX1<-which(grepl("NI",dataPlot$xAxis)| grepl("Needs Improvement",dataPlot$xAxis) | grepl("Needs improvement",dataPlot$xAxis))
  toChangeY1<-which(grepl("NI",dataPlot$yAxis)| grepl("Needs Improvement",dataPlot$yAxis) | grepl("Needs improvement",dataPlot$yAxis))
  dataPlot$xAxis[toChangeX1]<-"I"
  dataPlot$yAxis[toChangeY1]<-"I"
  
  # Amend character strings for Programme codesds improvement grades (as substr,1,1 could be the same for two or more variables)   
  toChangeX2<-which(grepl("BDS",dataPlot$xAxis))
  toChangeX3<-which(grepl("BScDTH",dataPlot$xAxis) | grepl("DTH",dataPlot$xAxis))
  toChangeY2<-which(grepl("BDS",dataPlot$yAxis))
  toChangeY3<-which(grepl("BScDTH",dataPlot$yAxis) | grepl("DTH",dataPlot$yAxis))
  
  dataPlot$xAxis[toChangeX2]<-"D"
  dataPlot$xAxis[toChangeX3]<-"T"
  dataPlot$yAxis[toChangeY2]<-"D"
  dataPlot$yAxis[toChangeY3]<-"T"
  
  
  # Code for xAxis    
  dataPlot$codeX<-substr(dataPlot$xAxis,1,1)
  variables1<-as.character(unique(dataPlot$codeX)) # a list of the unique variables values in this column          
  variables1<-sort(toupper(variables1))
  
  # Loop to get levels as a string
  codeX<-""
  for (i in 1:length(variables1)){           # loop to bring it together as a gradeCode
    text1<-variables1[i]
    codeX<-paste(codeX,text1,sep="") }
  
  # Code for yAxis    
  dataPlot$codeY<-substr(dataPlot$yAxis,1,1)
  variables2<-as.character(unique(dataPlot$codeY)) # a list of the unique variables values in this column          
  variables2<-sort(toupper(variables2))
  
  # Loop to get levels as a string
  codeY<-""
  for (i in 1:length(variables2)){           # loop to bring it together as a gradeCode
    text2<-variables2[i]
    codeY<-paste(codeY,text2,sep="") }   
  
  
  # Determine the xScheme from the codeX character strings  
  xScheme<-NULL
  nCharX<-NULL
  if(codeX=="BESU"|codeX=="BES"|codeX=="ES")      {xScheme<-"UBSE"}  
  if(codeX=="ESU")                                {xScheme<-"USE"}
  if(codeX=="BEHLU"|codeX=="BEHL"| codeX=="EHL")  {xScheme<-"UBLHE"} 
  if(codeX=="EFP"|codeX=="EP")                    {xScheme<-"PFE"}
  if(codeX=="FP"|codeX=="P")                      {xScheme<-"PF"}
  if(codeX=="CIN"|codeX=="CI"|codeX=="CN"|codeX=="N"|codeX=="C") {xScheme<-"NIC"}
  if(codeX=="DT"|codeX=="D")                      {xScheme<-"Programme"}
  if(codeX=="S" & substr(dataPlot$xAxis[1],2,2)=="t"){xScheme<-"Station"}
  if(codeX=="S" & substr(dataPlot$xAxis[1],2,2)=="u"){xScheme<-"SubStn"}
  if(codeX=="FM")                                 {xScheme<-"Gender"}
  if(codeX=="AOW"|codeX=="AW")                    {xScheme<-"Ethnicity"} 
  if(codeX=="NOS"|codeX=="NS"|codeX=="N")         {xScheme<-"Disability"}
  if(codeX=="D")                                  {xScheme<-"Domain"}
  if(codeX=="1234"|codeX=="123456"|codeX=="1789") {xScheme<-"Station"} 
  if(is.null(xScheme)){
    nCharX<-nchar(dataPlot$xAxis)
    nCharXMin<-min(nCharX)
    nCharXMax<-max(nCharX)
    if(nCharXMin==1 & nCharXMax==1) {xScheme<-"Stage"}
    if(nCharXMin==2 & nCharXMax==2) {xScheme<-"SubStn"}
    if(nCharXMin==1 & nCharXMax==2) {xScheme<-"Domain"}
    if(nCharXMax>2)                 {xScheme<-"Assessor"} }
  
  # Determine the yScheme from the codeY character strings  
  yScheme<-NULL
  nCharY<-NULL
  if(codeY=="BESU"|codeY=="BES"|codeY=="ES")      {yScheme<-"UBSE"}  
  if(codeY=="ESU")                                {yScheme<-"USE"}
  if(codeY=="BEHLU"|codeY=="BEHL"| codeY=="EHL")  {yScheme<-"UBLHE"} 
  if(codeY=="EFP"|codeY=="EP")                    {yScheme<-"PFE"}
  if(codeY=="FP"|codeY=="P")                      {yScheme<-"PF"}
  if(codeY=="CIN"|codeY=="CI"|codeY=="CN"|codeY=="N"|codeY=="C") {yScheme<-"NIC"}
  if(codeY=="DT"|codeY=="D")                      {yScheme<-"Programme"}
  if(codeY=="S" & substr(dataPlot$yAxis[1],2,2)=="t"){yScheme<-"Station"}
  if(codeY=="S" & substr(dataPlot$yAxis[1],2,2)=="u"){yScheme<-"SubStn"}
  if(codeY=="FM")                                 {yScheme<-"Gender"}
  if(codeY=="AOW"|codeY=="AW")                    {yScheme<-"Ethnicity"} 
  if(codeY=="NOS"|codeY=="NS"|codeY=="N")         {yScheme<-"Disability"}
  if(codeY=="D")                                  {yScheme<-"Domain"}
  if(codeY=="1234"|codeY=="123456"|codeY=="1789")      {yScheme<-"Station"} 
  if(is.null(yScheme)){
    nCharY<-nchar(dataPlot$yAxis)
    nCharYMin<-min(nCharY)
    nCharYMax<-max(nCharY)
    if(nCharYMin==1 & nCharYMax==1) {yScheme<-"Stage"}
    if(nCharYMin==2 & nCharYMax==2) {yScheme<-"SubStn"}
    if(nCharYMin==1 & nCharYMax==2) {yScheme<-"Domain"}
    if(nCharYMax>2)                 {yScheme<-"Assessor"} }
  
  # Change the xScheme or yScheme if forceSchemeX or forceSchemeY has been used 
  if(forceSchemeX=="Y"){
    xScheme<-switch(xScheme, "USE"="UBSE", "PF"="PFE", "Station"="Stage", "Stage"="Station", "NIC"="NC") }
  
  if(forceSchemeX=="D"){
    xScheme<-switch(xScheme, "Assessor"="Domain", "Stage"="Domain", "Station"="Domain") }
  
  if(forceSchemeY=="Y"){
    yScheme<-switch(yScheme, "USE"="UBSE", "PF"="PFE", "Station"="Stage", "Stage"="Station", "NIC"="NC") }
  
  if(forceSchemeY=="D"){
    YScheme<-switch(yScheme, "Assessor"="Domain", "Stage"="Domain", "Station"="Domain") }
  
  # Convert all abbreviated grades etc in codeX col to long form
  if(xScheme=="UBSE" | xScheme=="USE" | xScheme=="UBLHE"){dataPlot$codeX[dataPlot$codeX=="U"] <-"Unsatisfactory"}
  if(xScheme=="UBSE" | xScheme=="UBLHE")                 {dataPlot$codeX[dataPlot$codeX=="B"] <-"Borderline"}
  if(xScheme=="UBSE" | xScheme=="USE")                   {dataPlot$codeX[dataPlot$codeX=="S"] <-"Satisfactory"}
  if(xScheme=="UBSE" | xScheme=="USE" | xScheme=="UBLHE" | xScheme=="PFE"){dataPlot$codeX[dataPlot$codeX=="E"] <-"Excellent"}
  if(xScheme=="UBLHE")                                   {dataPlot$codeX[dataPlot$codeX=="L"]<-"Low Satisfactory"}
  if(xScheme=="UBLHE")                                   {dataPlot$codeX[dataPlot$codeX=="H"]<-"High Satisfactory"} 
  if(xScheme=="PFE" | xScheme=="PF"){dataPlot$codeX[dataPlot$codeX=="P"]<-"Pass"}
  if(xScheme=="PFE" | xScheme=="PF"){dataPlot$codeX[dataPlot$codeX=="F"]<-"Fail"}
  if(xScheme=="NIC" | xScheme=="NC"){dataPlot$codeX[dataPlot$codeX=="N"]<-"Not competent"}
  if(xScheme=="NIC"){dataPlot$codeX[dataPlot$codeX=="I"]<-"Needs improvement"}
  if(xScheme=="NIC" | xScheme=="NC"){dataPlot$codeX[dataPlot$codeX=="C"]<-"Competent"}
  if(xScheme=="Gender") {dataPlot$codeX[dataPlot$codeX=="F"] <-"Female"}
  if(xScheme=="Gender") {dataPlot$codeX[dataPlot$codeX=="M"] <-"Male"}
  if(xScheme=="Ethnicity") {dataPlot$codeX[dataPlot$codeX=="A"] <-"Asian"}
  if(xScheme=="Ethnicity") {dataPlot$codeX[dataPlot$codeX=="O"] <-"Other"}
  if(xScheme=="Ethnicity") {dataPlot$codeX[dataPlot$codeX=="W"] <-"White"}
  if(xScheme=="Disability") {dataPlot$codeX[dataPlot$codeX=="N"] <-"No known disability"}
  if(xScheme=="Disability") {dataPlot$codeX[dataPlot$codeX=="O"] <-"Other disability"}
  if(xScheme=="Disability") {dataPlot$codeX[dataPlot$codeX=="S"] <-"Specific learning difficulty"} 
  if(xScheme=="Programme") {dataPlot$codeX[dataPlot$codeX=="D"] <-"BDS"}
  if(xScheme=="Programme") {dataPlot$codeX[dataPlot$codeX=="T"] <-"BScDTH"}
  if(xScheme=="Domain")    {dataPlot$codeX<-dataPlot$xAxis}
  if(xScheme=="Station" | xScheme=="SubStn" | xScheme=="Stage" | xScheme=="Assessor") {dataPlot$codeX<-dataPlot$xAxis}
  
  # Convert all abbreviated grades etc in codeY col to long form
  if(yScheme=="UBSE" | yScheme=="USE" | yScheme=="UBLHE"){dataPlot$codeY[dataPlot$codeY=="U"] <-"Unsatisfactory"}
  if(yScheme=="UBSE" | yScheme=="UBLHE")                  {dataPlot$codeY[dataPlot$codeY=="B"] <-"Borderline"}
  if(yScheme=="UBSE" | yScheme=="USE")                    {dataPlot$codeY[dataPlot$codeY=="S"] <-"Satisfactory"}
  if(yScheme=="UBSE" | yScheme=="USE" | yScheme=="UBLHE" | yScheme=="PFE"){dataPlot$codeY[dataPlot$codeY=="E"] <-"Excellent"}
  if(yScheme=="UBLHE")                                     {dataPlot$codeY[dataPlot$codeY=="L"]<-"Low Satisfactory"}
  if(yScheme=="UBLHE")                                     {dataPlot$codeY[dataPlot$codeY=="H"]<-"High Satisfactory"} 
  if(yScheme=="PFE" | yScheme=="PF"){dataPlot$codeY[dataPlot$codeY=="P"]<-"Pass"}
  if(yScheme=="PFE" | yScheme=="PF"){dataPlot$codeY[dataPlot$codeY=="F"]<-"Fail"}
  if(yScheme=="NIC" | yScheme=="NC"){dataPlot$codeY[dataPlot$codeY=="N"]<-"Not competent"}
  if(yScheme=="NIC"){dataPlot$codeY[dataPlot$codeY=="I"]<-"Needs improvement"}
  if(yScheme=="NIC" | yScheme=="NC"){dataPlot$codeY[dataPlot$codeY=="C"]<-"Competent"}
  if(yScheme=="Gender") {dataPlot$codeY[dataPlot$codeY=="F"] <-"Female"}
  if(yScheme=="Gender") {dataPlot$codeY[dataPlot$codeY=="M"] <-"Male"}
  if(yScheme=="Ethnicity") {dataPlot$codeY[dataPlot$codeY=="A"] <-"Asian"}
  if(yScheme=="Ethnicity") {dataPlot$codeY[dataPlot$codeY=="O"] <-"Other"}
  if(yScheme=="Ethnicity") {dataPlot$codeY[dataPlot$codeY=="W"] <-"White"}
  if(yScheme=="Disability") {dataPlot$codeY[dataPlot$codeY=="N"] <-"No known disability"}
  if(yScheme=="Disability") {dataPlot$codeY[dataPlot$codeY=="O"] <-"Other disability"}
  if(yScheme=="Disability") {dataPlot$codeY[dataPlot$codeY=="S"] <-"Specific learning difficulty"} 
  if(yScheme=="Programme") {dataPlot$codeY[dataPlot$codeY=="D"] <-"BDS"}
  if(yScheme=="Programme") {dataPlot$codeY[dataPlot$codeY=="T"] <-"BScDTH"}
  if(yScheme=="Domain")    {dataPlot$codeY<-dataPlot$yAxis}
  if(yScheme=="Station" | yScheme=="SubStn" | yScheme=="Stage" | yScheme=="Assessor") {dataPlot$codeY<-dataPlot$yAxis}
  
  # Factor/order the codeX data so appears in correct order in plot  
  if(xScheme=="UBSE"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("Unsatisfactory","Borderline","Satisfactory","Excellent"),ordered=TRUE )}
  if(xScheme=="USE"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("Unsatisfactory","Satisfactory","Excellent"),ordered=TRUE)}
  if(xScheme=="UBLHE"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("Unsatisfactory","Borderline","Low Satisfactory","High Satisfactory","Excellent"),ordered=TRUE)}
  if(xScheme=="PFE"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("Fail","Pass","Excellent"),ordered=TRUE)}
  if(xScheme=="PF"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("Fail","Pass"),ordered=TRUE)}
  if(xScheme=="NC"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("Not competent","Competent"),ordered=TRUE)}
  if(xScheme=="NIC"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("Not competent","Needs improvement","Competent"),ordered=TRUE)}
  if(xScheme=="Stage"){dataPlot$codeX<-as.numeric(dataPlot$codeX)}  
  if(xScheme=="Assessor"){dataPlot$codeX<-as.numeric(dataPlot$codeX)}
  if(xScheme=="Gender"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("Female","Male"),ordered=TRUE)}
  if(xScheme=="Ethnicity"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("Asian","Other","White"),ordered=TRUE)}
  if(xScheme=="Disability"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("No known disability","Other disability","Specific learning difficulty"),ordered=TRUE)}
  if(xScheme=="Programme"){dataPlot$codeX<-factor(dataPlot$codeX, levels=c("BDS","BScDTH"),ordered=TRUE)}
  if(xScheme=="Stage" | xScheme=="Assessor"){dataPlot$codeX<-as.numeric(dataPlot$codeX)}
  if(xScheme=="Station" &  substr(dataPlot$codeX[1],1,1)=="S"){dataPlot$codeX<-as.numeric(substr(dataPlot$codeX,4,6))}
  if(xScheme=="Station" & !substr(dataPlot$codeX[1],1,1)=="S"){dataPlot$codeX<-as.numeric(dataPlot$codeX)}
  if(xScheme=="SubStn"  &  substr(dataPlot$codeX[1],1,1)=="S"){dataPlot$codeX<-substr(dataPlot$codeX,4,6)}
  if(xScheme=="SubStn"  & !substr(dataPlot$codeX[1],1,1)=="S"){dataPlot$codeX<-dataPlot$codeX}
  if(xScheme=="Domain" &  substr(dataPlot$codeX[1],1,1)=="D"){dataPlot$codeX<-as.numeric(substr(dataPlot$codeX,7,8))}
  if(xScheme=="Domain" & !substr(dataPlot$codeX[1],1,1)=="D"){dataPlot$codeX<-as.numeric(dataPlot$codeX)}
  
  # Factor/order the codeY data so appears in correct order in plot (and ordered by Median for Assessors)   
  if(yScheme=="UBSE"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("Excellent","Satisfactory","Borderline","Unsatisfactory"),ordered=TRUE )}
  if(yScheme=="USE"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("Excellent","Satisfactory","Unsatisfactory"),ordered=TRUE)}
  if(yScheme=="UBLHE"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("Excellent","High Satisfactory","Low Satisfactory","Borderline","Unsatisfactory"),ordered=TRUE)}
  if(yScheme=="PFE"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("Excellent","Pass","Fail"),ordered=TRUE)}
  if(yScheme=="PF"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("Pass","Fail"),ordered=TRUE)}
  if(yScheme=="NC"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("Competent","Not competent"),ordered=TRUE)}
  if(yScheme=="NIC"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("Competent","Needs improvement","Not competent"),ordered=TRUE)}
  if(yScheme=="Stage"|yScheme=="Assessor"){dataPlot$codeY<-as.numeric(dataPlot$codeY)}  
  if(yScheme=="Gender"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("Female","Male"),ordered=TRUE)}
  if(yScheme=="Ethnicity"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("Asian","Other","White"),ordered=TRUE)}
  if(yScheme=="Disability"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("No known disability","Other disability","Specific learning difficulty"),ordered=TRUE)}
  if(yScheme=="Programme"){dataPlot$codeY<-factor(dataPlot$codeY, levels=c("BDS","BScDTH"),ordered=TRUE)}
  if(yScheme=="Stage" | yScheme=="Assessor"){dataPlot$codeY<-as.numeric(dataPlot$codeY)}
  if(yScheme=="Station" &  substr(dataPlot$codeY[1],1,1)=="S"){dataPlot$codeY<-as.numeric(substr(dataPlot$codeY,4,6))}
  if(yScheme=="Station" & !substr(dataPlot$codeY[1],1,1)=="S"){dataPlot$codeY<-as.numeric(dataPlot$codeY)}
  if(yScheme=="SubStn"  &  substr(dataPlot$codeY[1],1,1)=="S"){dataPlot$codeY<-factor(substr(dataPlot$codeY,4,6))}
  if(yScheme=="SubStn"  & !substr(dataPlot$codeY[1],1,1)=="S"){dataPlot$codeY<-dataPlot$codeY}
  if(yScheme=="Domain" &  substr(dataPlot$codeY[1],1,1)=="D"){dataPlot$codeY<-as.numeric(substr(dataPlot$codeY,7,8))}
  if(yScheme=="Domain" & !substr(dataPlot$codeY[1],1,1)=="D"){dataPlot$codeY<-as.numeric(dataPlot$codeY)}
  
  # Messages and Warnings re schemes
  if(xScheme=="Gender")      {print("A bar is provided for each Gender")}
  if(xScheme=="Ethnicity")   {print("A bar is provided for each Ethnicity")}
  if(xScheme=="Disability")  {print("A bar is provided for each Disability")}
  if(xScheme=="UBSE")        {print("A bar is provided for each UBSE grade")}
  if(xScheme=="Programme")   {print("A bar is provided for each Programme")}
  if(xScheme=="UBLHE")       {print("A bar is provided for each UBLHE grade")}
  if(xScheme=="PFE")         {print("A bar is provided for each PFE grade")}
  if(xScheme=="NC")          {print("A bar is provided for each NC grade")}
  if(xScheme=="SubStn")      {print("A bar is provided for each ISCE Substation")}
  if(xScheme=="Domain")      {print("A bar is provided for each Domain")}
  if(xScheme=="Assessor")    {cat(paste('A bar is provided for each Assessor; add the parameter forceSchemeX="',"D",'" for Domain.\n',sep=""))} 
  if(xScheme=="Station")     {cat(paste('A bar is provided for each ISCE Station; add the parameter forceSchemeX="',"Y",'" for Stage or forceSchemeX="',"D",'" for Domain.\n',sep=""))} 
  if(xScheme=="Station" & codeX=="S") {print("A bar is provided for each ISCE Station")} 
  if(xScheme=="Stage")       {cat(paste('A bar is provided for each Stage; add the parameter forceSchemeX="',"Y",'" for Station or forceSchemeX="',"D",'" for Domain.\n',sep=""))} 
  if(xScheme=="USE")         {cat(paste('A bar is provided for each USE grade; if UBSE is required add the parameter forceSchemeX="',"Y",'".\n',sep=""))} 
  if(xScheme=="PF")          {cat(paste('A bar is provided for each PF grade; if PFE is required add the parameter forceSchemeX="',"Y",'".\n',sep=""))}
  if(xScheme=="NIC" &  grepl("I",codeX)) {print("A bar is provided for each NIC judgement")}
  if(xScheme=="NIC" & !grepl("I",codeX)) {cat(paste('A bar is provided for each NIC judgement; if NC is required add the parameter forceSchemeX="',"Y",'".\n',sep=""))}
  
  # Messages and Warnings re schemes
  if(yScheme=="Gender")      {print("Each bar is split by Gender")}
  if(yScheme=="Ethnicity")   {print("Each bar is split by Ethnicity")}
  if(yScheme=="Disability")  {print("Each bar is split by Disability")}
  if(yScheme=="UBSE")        {print("Each bar is split by UBSE grade")}
  if(yScheme=="Programme")   {print("Each bar is split by Programme")}
  if(yScheme=="UBLHE")       {print("Each bar is split by UBLHE grade")}
  if(yScheme=="PFE")         {print("Each bar is split by PFE grade")}
  if(yScheme=="NC")          {print("Each bar is split by NC grade")}
  if(yScheme=="SubStn")      {print("Each bar is split by ISCE Substation")}
  if(yScheme=="Domain")      {print("Each bar is split by Domain")}
  if(yScheme=="Assessor")    {cat(paste('Each bar is split by Assessor; add the parameter forceSchemeX="',"D",'" for Domain.\n',sep=""))} 
  if(yScheme=="Station")     {cat(paste('Each bar is split by ISCE Station; add the parameter forceSchemeX="',"Y",'" for Stage or forceSchemeX="',"D",'" for Domain.\n',sep=""))} 
  if(yScheme=="Station" & codeX=="S") {print("Each bar is split by ISCE Station")} 
  if(yScheme=="Stage")       {cat(paste('Each bar is split by Stage; add the parameter forceSchemeX="',"Y",'" for Station or forceSchemeX="',"D",'" for Domain.\n',sep=""))} 
  if(yScheme=="USE")         {cat(paste('Each bar is split by USE grade; if UBSE is required add the parameter forceSchemeY="',"Y",'".\n',sep=""))} 
  if(yScheme=="PF")          {cat(paste('Each bar is split by PF grade; if PFE is required add the parameter forceSchemeY="',"Y",'".\n',sep=""))}
  if(yScheme=="NIC"&  grepl("I",codeY)) {print("Each bar is split by NIC judgement")}
  if(yScheme=="NIC"& !grepl("I",codeY)) {cat(paste('Each bar is split by NIC judgement; if NC is required add the parameter forceSchemeY="',"Y",'".\n',sep=""))}
  
  # Colours for fill (yScheme)
  if(yScheme=="UBSE"){colFill<-c("#3D52A1","#86BB6A","#E68B33","#D92120")}
  if(yScheme=="USE"){colFill<-c("#3D52A1","#86BB6A","#D92120")}
  if(yScheme=="UBLHE"){colFill<-c("#3D52A1","#6DB388","#B1BE4E","#E68B33","#D92120")}
  if(yScheme=="PFE"){colFill<-c("#3D52A1","#86BB6A","#D92120")}
  if(yScheme=="PF"){colFill<-c("#86BB6A","#D92120")}
  if(yScheme=="NIC"){colFill<-c("#86BB6A","#E68B33","#D92120")}
  if(yScheme=="NC"){colFill<-c("#86BB6A","#D92120")} 
  if(yScheme=="Assessor"){colFill<-c("#AA4455","#AA7744","#AAAA44","#44AA77","#4477AA","#BB4455","#BB7744","#BBAA44","#44BB77","#4477BB","#AA4477","#AA7777")[1:length(unique(dataPlot$codeY))]} 
  if(yScheme=="Stage"){colFill<-fnColours("Stage")}
  if(yScheme=="Station"|yScheme=="SubStn"|yScheme=="Domain"){colFill<-c("#AA4455","#AA7744","#AAAA44","#44AA77","#4477AA","#AA4466","#AA7755","#AAAA55","#44AA88","#4477BB","#AA4477","#AA7777")[1:length(unique(dataPlot$codeY))]} # 12 should be the max required
  if(yScheme=="Gender"){colFill<-fnColours("Gender")}
  if(yScheme=="Ethnicity"){colFill<-fnColours("Ethnicity")}
  if(yScheme=="Disability"){colFill<-fnColours("Disability")}
  if(yScheme=="Programme"){colFill<-fnColours("Gender")} 
  
  # label for x-axis
  labelX<-NULL
  if(xScheme=="Assessor"){labelX<-"Assessor ID"}
  if(xScheme=="SubStn"){labelX<-"Substation"}
  if(xScheme=="Stage"|xScheme=="Gender"|xScheme=="Ethnicity"|xScheme=="Disability"|xScheme=="Station"|xScheme=="Programme"|xScheme=="Domain"){labelX<-xScheme}
  if(is.null(labelX)){labelX<-"Grade"}
  
  # label for y-axis
  labelY<-NULL
  if(yScheme=="Assessor"){labelY<-"Assessor ID"}
  if(yScheme=="SubStn"){labelY<-"Substation"}
  if(yScheme=="Stage"|yScheme=="Gender"|yScheme=="Ethnicity"|yScheme=="Disability"|yScheme=="Station"|yScheme=="Programme"|yScheme=="Domain"){labelY<-yScheme}
  if(yScheme=="NIC"){labelY<-"Judgement"}
  if(is.null(labelY)){labelY<-"Grade"}
  
  # for y-axis scale and numbers
  nEach<-tapply(dataPlot$Number,dataPlot$codeX,sum)
  nEach[is.na(nEach)]<-0
  nEachMax<-max(nEach)
  nBars<-length(nEach)
  
  # to order the character values for Assessor, Station & Domain schemes 
  if(xScheme=="Assessor" | xScheme=="Station" | xScheme=="Stage" | xScheme=="Domain"){dataPlot$codeX<-factor(dataPlot$codeX)}
  if(yScheme=="Assessor" | yScheme=="Station" | yScheme=="Stage" | yScheme=="Domain"){dataPlot$codeY<-factor(dataPlot$codeY)}  
  
  # Plot of counts 
  if(percent=="No"){   
    # For the y-axis breaks
    if(nEachMax>50){yBreaks<-seq(0,nEachMax,10)}  
    if(nEachMax>17){yBreaks<-seq(0,nEachMax,5)}
    if(nEachMax<18){yBreaks<-c(0,2,4,6,8,10,12,14,16,18)}  
    if(nEachMax<10){yBreaks<-c(0,2,4,6,8,10)} 
    
    p<-ggplot(dataPlot, aes(x=codeX, y=Number, fill=codeY, order=codeY)) +
      geom_bar(stat="identity") +
      xlab(labelX) +
      ylab("Frequency") +
      guides(fill=guide_legend(title=labelY)) +
      scale_fill_manual(values=colFill, drop=FALSE) + 
      scale_x_discrete(drop=FALSE) + theme_psmd() +
      scale_y_continuous(breaks=yBreaks)
    
    # Loop to add text for total number in each bar (e.g. "n=2") where the numbers are different for each bar
    if(length(unique(nEach))!=1 & incl.N=="Yes"){
    
    for (i in 1:nBars){
      p<-p+geom_text(x=i,y=-(0.02*nEachMax),label=paste0("n=",nEach[i]), size=3, colour="#5c6b6b")}
  } }
  
  # Plot of percentages
  if(percent=="Yes"){
    p<-ggplot(dataPlot, aes(x=codeX, y=Prop, fill=codeY, order=codeY)) +
      geom_bar(stat="identity") +
      xlab(labelX) +
      ylab("Percentage") +
      guides(fill=guide_legend(title=labelY)) +
      scale_fill_manual(values=colFill, drop=FALSE) + 
      scale_x_discrete(drop=FALSE) + theme_psmd() +
      scale_y_continuous(breaks=seq(0,100,20))
    
    # Loop to add text for total number in each bar (e.g. "n=2") where the numbers are different for each bar
    if(length(unique(nEach))!=1 & incl.N=="Yes"){
      
    for (i in 1:nBars){
      p<-p+geom_text(x=i,y=-1,label=paste0("n=",nEach[i]), size=3, colour="#5c6b6b")}
  } }
  
  p
  #
  ##
  ### 
} #### END of FUNCTION
