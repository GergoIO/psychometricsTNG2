#' A function to determine which column or columns in a data frame contain (numeric) scores
#' 
#' @description fnDetermineScoreColumns is designed to determine which column or columns in a data frame contain (numeric) scores. It operates based on a series of conditionals which evaluate aspects such as whether the variable is numeric, whether the values have a range typical of scoring schemes, or have ranges narrower or broader than scoring schemes used in PSMD. 
#' 
#' @usage fnDetermineScoreColumns(Dataframe)
#' 
#' @param Dataframe Dataframe is a data frame of which you want to determine the possible score column(s).
#' 
#' @note
#' DZ250417: This is currently an experimental function and will be updated to handle real data more accurately.
#' DZ250417: Build in a probabilistic element to provide the single best-guess if length(Possible.Score.Columns)>1.
#' 
#' @examples  
#' fnDetermineScoreColumns.(dataExample) will return a list of column names for the columns the function thinks could be scores.
#' In the case of dataExamples, this is multiple possibilities. In reality, there are likely to be fewer. 
#' 
#' @author Dr Daniel Zahra, \email{daniel.zahra@plymouth.ac.uk}
#' 
#' @export

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### fnDetermineScoreColumns - last updated 290617 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Function designed to list columns in a dataframe which could be scores / identify a score column

# Inputs
#
# Dataframe Single dataframe with at least one column
#        

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Dataframe<-dataExample
#fnDetermineScoreColumns(dataExample)

fnDetermineScoreColumns<-function(Dataframe){

# Rename argument to something shorter  

  Data<-Dataframe  

# Check input type is a dataframe, warning if FALSE, continue if TRUE.

if(is.data.frame(Data)==FALSE){
  
warning(paste("Dataframe argument should be a data.frame, even if it only has one column"))
  
  }else{
    
#Remove any rows with NA    

    Data<-Data[complete.cases(Data),]

# Tabulate criteria

for(i in colnames(Data)){
  
  # Create empty data.frame (Table.ColDetails)
  
  if(i==colnames(Data)[1]){Table.ColDetails<-data.frame(
    ColName=character(),
    Numeric=character(),
    Pos.Percent=character(),
    Reasonable.Range=character(),
    Not.Response=character(),
    Not.Stage=character(),
    Not.Assessor.ID=character()
    )}
  
  # Determine row-entry for i
  
  Temp<-data.frame(
    ColName=i,
    Numeric=is.numeric(Data[[i]]),
    Pos.Percent=ifelse(is.numeric(Data[[i]])==TRUE,
                       ifelse(max(Data[[i]])<=100,TRUE,FALSE),FALSE),
    Reasonable.Range=ifelse(is.numeric(Data[[i]])==TRUE,
      ifelse((max(Data[[i]])-min(Data[[i]]))==length(Data[[i]])-1,FALSE,TRUE),FALSE),
    Not.Response=ifelse(is.numeric(Data[[i]])==TRUE,
      ifelse(length(subset(Data[[i]],Data[[i]]==-0.25 | Data[[i]]==0 | Data[[i]]==1))==length(Data[[i]]),FALSE,TRUE),FALSE),
    Not.Stage=ifelse(is.numeric(Data[[i]])==TRUE,
                     ifelse(max(Data[[i]])>5,TRUE,FALSE),FALSE), # Also excludes element scores if max<=5
    Not.Assessor.ID=ifelse(is.numeric(Data[[i]])==TRUE,
                           ifelse(max(Data[[i]])>100,FALSE,TRUE),FALSE)
    )
  
  # Add to Table.ColDetails
 
   Table.ColDetails<-rbind(Table.ColDetails,Temp)
  
  # Identify Possible Score column(s)
   
  if(i==colnames(Data)[length(colnames(Data))]){
    Table.ColDetails$Pos.Score<-ifelse((
      Table.ColDetails$Numeric==TRUE & 
      #Table.ColDetails$Pos.Percent==TRUE & 
      Table.ColDetails$Reasonable.Range==TRUE & 
      Table.ColDetails$Not.Response==TRUE & 
      Table.ColDetails$Not.Stage==TRUE & 
      Table.ColDetails$Not.Assessor.ID==TRUE),TRUE,FALSE)
  }}

# List columns which could be scores
    
Possible.Score.Columns<-Table.ColDetails$ColName[Table.ColDetails$Pos.Score==TRUE] 

# Look for column headers containing 'Score', 'Final', 'Raw', and 'Percentage'  variants

Score.Variants<-c("Score","score")
for(i in Score.Variants){
if(i==Score.Variants[1]){Score<-character()}
  Temp<-colnames(Data)[grep(i,colnames(Data))]
  Score<-c(Score,Temp)
if(i==Score.Variants[length(Score.Variants)]){Score<-unique(Score)}}

Final.Variants<-c("Final","final")
for(i in Final.Variants){
if(i==Final.Variants[1]){Final<-character()}
  Temp<-colnames(Data)[grep(i,colnames(Data))]
  Final<-c(Final,Temp)
if(i==Final.Variants[length(Final.Variants)]){Final<-unique(Final)}}

Raw.Variants<-c("Raw","raw")
for(i in Raw.Variants){
if(i==Raw.Variants[1]){Raw<-character()}
  Temp<-colnames(Data)[grep(i,colnames(Data))]
  Raw<-c(Raw,Temp)
if(i==Raw.Variants[length(Raw.Variants)]){Raw<-unique(Raw)}}

Percentage.Variants<-c("Percentage","Perc","Percent")
for(i in Percentage.Variants){
if(i==Percentage.Variants[1]){Percentage<-character()}
  Temp<-colnames(Data)[grep(i,colnames(Data))]
  Percentage<-c(Percentage,Temp)
if(i==Percentage.Variants[length(Percentage.Variants)]){Percentage<-unique(Percentage)}}

# Edit if no columns found

if(length(Possible.Score.Columns)==0){Final.Score<-"No columns are likely to contain scores."}
if(length(Score)==0){Score<-"No columns headers include 'Score'."}
if(length(Final)==0){Final<-"No possibilities found"}
if(length(Raw)==0){Raw<-"No possibilities found"}
if(length(Percentage)==0){Percentage<-"No possibilities found"}

# Compile

Conclusions<-list()
Conclusions[["Possibilities"]]<-Possible.Score.Columns
Conclusions[["Scores"]]<-Score
Conclusions[["Final Scores"]]<-Final
Conclusions[["Raw Scores"]]<-Raw
Conclusions[["Percent Scores"]]<-Percentage

# Determin most likely candidate (prioritises percentage scores, then raw, then final)

if(length(Conclusions[["Percent Scores"]])==1 & Conclusions[["Percent Scores"]]!="No possibilities found"){
  Most.Likely<-Conclusions[["Percent Scores"]]}else{
    if(length(Conclusions[["Raw Scores"]])==1& Conclusions[["Raw Scores"]]!="No possibilities found"){
      Most.Likely<-Conclusions[["Raw Scores"]]}else{
        if(length(Conclusions[["Final Scores"]])==1& Conclusions[["Final Scores"]]!="No possibilities found"){
          Most.Likely<-Conclusions[["Final Scores"]]}else{
            Most.Likely<-"No idea"}}}

Conclusions[["Most Likely"]]<-Most.Likely

# Return list, Possible.Score.Columns

return(Conclusions)  

} # Close data.frame checking conditional  
} # Close function 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
##
##
##
##
##
####
###
##
#