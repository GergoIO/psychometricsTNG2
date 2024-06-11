#' Produces a floating barplot of the percentage grade ranges by stage for a MCQ/PT.
#' 
#' @description This function takes one or two dfs of grade boundaries and min/max scores
#' and produces a grade range barplot as a ggplot2 object.
#.
#' @usage fnGradeRange(df1, df2=NULL, grades="UBSE")
#'
#' @param df1 The name of the data frame (e.g. the Grade Boundaries table) with the stages as
#' columns. Column 1 to contain the name of the data within each row. Can include
#' rows for the min and max scores.
#' @param df2 The name of the dataframe with the min and max scores (e.g. the Descriptive Stats
#' table) if not present in df1. Stages must be as columns and df2 must have the same
#' number of columns as df1. Column 1 to contain the name of the data within each row.
#' @param grades Specify the grade group, in inverted commas ("UBSE" (dafault) or "US").
#' 
#' @examples
#' testData1<-data.frame(Value=c("Minimum","Maximum","Borderline boundary",
#' "Satisfactory boundary","Excellent boundary"),Y1=c(0.0,18.2,1.4,2.6,15),
#' Y2=c(9.2,59.6,12.3,15,35.4),Y3=c(18.2,55.8,20.9,29.1,51.3),
#' Y4=c(29.4,72,30.1,34.6,58),Y5=c(38,72,NA,45.09,NA))
#' 
#' testData2<-data.frame(Value=c("Minimum","Maximum","Borderline boundary",
#' "Satisfactory boundary","Excellent boundary"), Y2=c(1.25,54,-4.2,7.12,38.23),
#' Y3=c(36,72,22.6,34.14,62.84),Y4=c(44.75,78.25,34.26,45.45,69.06),
#' Y5=c(45.25,85.2,39.9,52.2,75.3))
#' 
#' testData3<-data.frame(Value=c("Minimum","Maximum","Satisfactory boundary"),
#' Y1=c(20.93,49.80,25.1),Y2=c(41.8,68.8,49.2))
#' 
#' fnGradeRange(testData1)
#' 
#' fnGradeRange(testData2)
#' 
#' fnGradeRange(testData3,grades="US")
#'
#' @export

fnGradeRange<-function(df1, df2=NULL, grades="UBSE"){ 
  
  library(ggplot2)
  library(stringr)
  library(psychometricsPSMD)
  library(tidyr)
  
  # manipulate the data (one dataframe)
  if(is.null(df2)){
    df1[,1]<-sapply(df1[,1],as.character)  # col1 as character
    df1[,2:ncol(df1)]<-sapply(df1[,2:ncol(df1)],as.numeric) # all other cols numeric 
    
    rowMins<-which(grepl("min",df1[,1],ignore.case=TRUE))
    rowUB<-which(grepl(paste("Borderline",sep="|"),df1[,1],ignore.case=TRUE))
    rowBS<-which(grepl(paste("Satisfactory",sep="|"),df1[,1],ignore.case=FALSE))
    rowSE<-which(grepl(paste("Excellent",sep="|"),df1[,1],ignore.case=TRUE))
    rowMaxs<-which(grepl("max",df1[,1],ignore.case=TRUE))
    df<-rbind(df1[rowMins,],df1[rowUB,],df1[rowBS,],df1[rowSE,],df1[rowMaxs,])
  }
  
  # check and manipulate the data (two dataframes)
  if(!is.null(df2)){  
    
    stopifnot(class(df2)=="data.frame")
    stopifnot(ncol(df1)==ncol(df2))
    
    colnames(df2)<-colnames(df1)
    
    df1[,1]<-sapply(df1[,1],as.character)  # to ensure col1 is character
    df2[,1]<-sapply(df2[,1],as.character)  # to ensure col1 is character
    df3<-rbind(df1,df2)
    df3[,2:ncol(df3)]<-sapply(df3[,2:ncol(df3)],as.numeric) 
    
    rowMins<-which(grepl("min",df3[,1],ignore.case=TRUE))
    rowUB<-which(grepl(paste("Borderline",sep="|"),df3[,1],ignore.case=TRUE))
    rowBS<-which(grepl(paste("Satisfactory",sep="|"),df3[,1],ignore.case=FALSE))
    rowSE<-which(grepl(paste("Excellent",sep="|"),df3[,1],ignore.case=TRUE))
    rowMaxs<-which(grepl("max",df3[,1],ignore.case=TRUE))
    df<-rbind(df3[rowMins,],df3[rowUB,],df3[rowBS,],df3[rowSE,],df3[rowMaxs,])
  }
  
  # check there are the correct number of rows of data to proceed
  if(grades=="UBSE"){stopifnot(nrow(df)==5)}
  if(grades=="US")  {stopifnot(nrow(df)==3)}
  
  # Any NA values? (will apply for BMBS Y5 as no B or E grades)
  if(any(is.na(df)==TRUE)){
    t1<-which(is.na(df), arr.ind=TRUE)  # identify row/cols of any blank or NAs
    t2<-t1
    t2[,1]<-t2[,1]+1     # to select the col of the row numbers and take the next value
    df[t1]<-df[t2]
    df[,2:ncol(df)]<-sapply(df[,2:ncol(df)],as.numeric) 
  }
  
  # Any NEGATIVES???
  if(min(df[1,c(2:ncol(df))])<0){negADD<-20} else {negADD<-0}
  
  # extract Stages
  regexp<-"[[:digit:]]+"
  stages<-as.numeric(str_extract(colnames(df),regexp),na.rm=TRUE)  # to extract the numeric values from the colnames
  stages<-stages[!is.na(stages)]  # remove NA values
  position<-seq(1:length(stages)) # the position of each stage (as stages may not start at 1!)
  
  
  ### Grades UBSE LOOP ### 
  if(grades=="UBSE"){
    tableRanges<-as.data.frame(matrix(nrow=1,ncol=7))  # empty df to populate
    
    for (i in seq(position)) {
      
      # Grey area
      if(!negADD==0){v1<-negADD+df[1,i+1]}  # where there are -ve min values
      if(negADD==0) {v1<-df[1,i+1]}  # where all min values are +ve
      
      # Red area
      if(df[2,i+1]>df[1,i+1] & df[5,i+1]>df[2,i+1]) {v2<-df[2,i+1]-df[1,i+1]}              
      if(df[2,i+1]>df[1,i+1] & df[5,i+1]<df[2,i+1]) {v2<-df[5,i+1]-df[1,i+1]}
      if(df[2,i+1]<df[1,i+1]) {v2<-0}  
      
      # Orange area
      if(df[3,i+1]>df[1,i+1] & df[2,i+1]>=df[1,i+1] & df[5,i+1]>df[3,i+1]) {v3<-df[3,i+1]-df[2,i+1]}     
      if(df[3,i+1]>df[1,i+1] & df[2,i+1]<=df[1,i+1]) {v3<-df[3,i+1]-df[1,i+1]} 
      if(df[3,i+1]<=df[1,i+1]){v3<-0}
      
      # Green area
      if(df[4,i+1]>df[1,i+1] & df[3,i+1]>=df[1,i+1] & df[5,i+1]>df[4,i+1]) {v4<-df[4,i+1]-df[3,i+1]}     
      if(df[4,i+1]>df[1,i+1] & df[3,i+1]>=df[1,i+1] & df[5,i+1]<df[4,i+1]) {v4<-df[5,i+1]-df[3,i+1]}
      if(df[4,i+1]>df[1,i+1] & df[3,i+1]<=df[1,i+1] & df[5,i+1]<df[4,i+1]) {v4<-df[5,i+1]-df[1,i+1]} 
      if(df[4,i+1]>df[1,i+1] & df[3,i+1]<=df[1,i+1] & df[5,i+1]>df[4,i+1]) {v4<-df[4,i+1]-df[1,i+1]} 
      if(df[3,i+1]<=df[1,i+1] & df[4,i+1]<=df[1,i+1] | df[3,i+1]>=df[5,i+1] & df[4,i+1]>=df[5,i+1] ){v4<-0} 
      
      # Blue area
      if(df[5,i+1]>df[1,i+1] & df[4,i+1]>=df[1,i+1]) {v5<-df[5,i+1]-df[4,i+1]}    
      if(df[5,i+1]>df[1,i+1] & df[4,i+1]<=df[1,i+1]) {v5<-df[5,i+1]-df[1,i+1]}
      if(df[5,i+1]<df[4,i+1]) {v5<-0}
      
      # Grey area
      v6<-100-df[5,i+1]           # 100-max
      
      temp1<-c(stages[i],v1,v2,v3,v4,v5,v6)
      tableRanges<-rbind(tableRanges,temp1)   
      
    }
    
    tableRanges<-tableRanges[-1,]
    colnames(tableRanges)<-c("Stage","Min","U","B","S","E","Max")
    
    dataRangesLong<-gather(data=tableRanges, key=Grade, value=Range, Min,U,B,S,E,Max)
    dataRangesLong$Grade<-factor(dataRangesLong$Grade,levels=c("Max","E","S","B","U","Min"),ordered=TRUE)
    dataRangesLong$Stage<-factor(dataRangesLong$Stage)
    
    # plot of grade ranges UBSE
    plot1<-ggplot(dataRangesLong,aes(x=Stage,y=Range,fill=Grade)) + geom_bar(stat="identity") + 
      scale_fill_manual(values=c(NA,"#3D52A1","#86BB6A","#E68B33","#D92120",NA), breaks=c("E","S","B","U"), labels=c("Excellent","Satisfactory","Borderline","Unsatisfactory")) +
      ylab("Score (%)") + xlab("Stage") + theme_psmd() + if(negADD>0){scale_y_continuous(breaks=seq(0,120,20), labels=c(-20,0,20,40,60,80,100))} else {scale_y_continuous(breaks=seq(0,100,20), limits=c(0,100))}
    
  } # END OF UBSE LOOP
  
  
  ### Grades US LOOP ###
  if(grades=="US") {
    tableRanges<-as.data.frame(matrix(nrow=1,ncol=5))  # empty df to populate
    
    for (i in seq(position)) {
      
      # Grey area
      if(!negADD==0){v1<-negADD+df[1,i+1]}  # where there are -ve min values
      if(negADD==0) {v1<-df[1,i+1]}  # where all min values are +ve                                                                  
      
      # Red area
      if(df[2,i+1]>df[1,i+1] & df[3,i+1]>df[2,i+1]) {v2<-df[2,i+1]-df[1,i+1]}            
      if(df[2,i+1]>df[1,i+1] & df[3,i+1]<df[2,i+1]) {v2<-df[3,i+1]-df[1,i+1]}
      if(df[2,i+1]<=df[1,i+1]) {v2<-0} 
      
      # Green area
      if(df[3,i+1]>df[2,i+1] & df[2,i+1]>df[1,i+1]) {v3<-df[3,i+1]-df[2,i+1]}     
      if(df[3,i+1]>df[2,i+1] & df[2,i+1]<df[1,i+1]) {v3<-df[3,i+1]-df[1,i+1]} 
      if(df[3,i+1]<=df[2,i+1]) {v3<-0}
      
      # Grey area
      v4<-100-df[3,i+1]          
      
      temp1<-c(stages[i],v1,v2,v3,v4)
      tableRanges<-rbind(tableRanges,temp1)   
      
    }
    
    tableRanges<-tableRanges[-1,]
    colnames(tableRanges)<-c("Stage","Min","U","S","Max")
    
    dataRangesLong<-gather(data=tableRanges, key=Grade, value=Range, Min,U,S,Max)
    dataRangesLong$Grade<-factor(dataRangesLong$Grade,levels=c("Max","S","U","Min"),ordered=TRUE)
    dataRangesLong$Stage<-factor(dataRangesLong$Stage)
    
    # plot of grade ranges US
    plot1<-ggplot(dataRangesLong,aes(x=Stage,y=Range,fill=Grade)) + geom_bar(stat="identity") + 
      scale_fill_manual(values=c(NA,"#86BB6A","#D92120",NA), breaks=c("S","U"), labels=c("Satisfactory","Unsatisfactory")) +
      ylab("Score (%)") + xlab("Stage") + theme_psmd() + if(negADD>0){scale_y_continuous(breaks=seq(0,120,20), labels=c(-20,0,20,40,60,80,100))} else {scale_y_continuous(breaks=seq(0,100,20), limits=c(0,100))}
    
  } # END OF US LOOP
  
  plot1
  
} # END OF FUNCTION 
