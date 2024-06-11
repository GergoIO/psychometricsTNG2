#' Produces a heatmap of the skills and subjects for the BMBS Progress Tests.
#' 
#' @description This function takes the Answer Key dataframe, the skill and
#' subject columns along with any removed items and produces a heatmap in ggplot2.
#' This function requires ggplot2, plyr and reshape packages. 
#.
#' @usage fnHeatmap(df, colSkill="Skill", colSubject="Subject", itemsRemoved=NULL, zerosBlank="Yes")
#'       
#' @param df The name of the answer key data frame in item number order.
#' @param colSkill The Skill column name (must be in quotation marks) or column 
#' number within the data frame (default colname is "Skill").This should contain
#' the Skills as uppercase values.
#' @param colSubject The Subject column name (must be in quotation marks) or 
#' column number within the data frame (default colname is "Subject"). This should
#' contain the Subjects as numeric values. 
#' @param itemsRemoved Use if any items have been removed (optional). A vector
#' of the item numbers which have been removed from the test.
#' @param zerosBlank Defaults to "Yes" so zero values do not show in the heatmap tiles.
#' Add parameter zerosBlank="No" for zero values to show.
#' 
#' @examples 
#' Item<-seq(1:20)
#' Skill<-c("G","G","H","I","F","S","T","G","G","M","G","G","H","I","F","S","F","G","G","M")
#' Subject<-c(40,2030,1080,71,20,330,1110,2300,1500,1275,970,370,1110,570,1510,340,460,450,50,460)
#' answerKey<-data.frame(Item,Skill,Subject) 
#' fnHeatmap(answerKey)
#' fnHeatmap(answerKey, itemsRemoved=c(2,12))
#' fnHeatmap(answerKey, zerosBlank="No")
#' 
#' @export

##############################################################

fnHeatmap<-function(df, colSkill="Skill", colSubject="Subject", itemsRemoved=NULL, zerosBlank="Yes") {
  Skill<-c("D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T")
  skillName<-c("History-taking","Clinical exam","Diagnostic tests","Diagnosis","Management plan","Prescribing","Communication skills","Multicultural Medicine","Ethics",                               "History of disease","Prevention and promotion","Epidemiological data","Anatomy","Physiology","Biochemistry","Pathology","Other")
  skillList<-data.frame(Skill,skillName)
  
  no<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13,13,13,13,13,13,13,14,14,14,14,14,14,14,15,15,15,15,15,15,15, 15,16,16,16,16,16,16,16,16,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,18)
  Subject<-c(110,240,245,270,710,2010,2020,31,140,150,750,810,850,860,880,975,1380,2030,2040,2320,460,840,2050,2060,2070,2080,2090,10,20,70,71,100,160,330,490,500,640,650,760,870,1000,1200,1395,1510,510,570,740,1300,800,670,1020,1150,1210,1340,2300,1080,1110,1140,1350,2100,2110,2120,2130,430,470,550,1100,1390,2140,2150,2160,2170,2180,590,660,790,910,2190,2200,2210,2220,2230,80,130,180,205,420,440,450,511,1040,1180,2240,2280,2290,40,90,200,350,480,610,620,971,980,1310,360,370,390,410,560,580,720,730,960,1050,1280,1320,1370,1400,1500,2250,680,820,1160,1161,1170,1220,1230,1250,1410,1420,1430,1440,30,50,190,320,340,700,1090,210,920,970,1260,1270,1365,2260,2270,220,280,540,770,830,1189,1190,1275,120,250,260,310,380,520,530,585,930,940,950,1030,1060,1115,1120,1130,1290,1480,1490,1600)
  subjectName<-c("Blood & Lymph","Blood & Lymph","Blood & Lymph","Blood & Lymph","Blood & Lymph","Blood & Lymph","Blood & Lymph","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Cardiovascular","Child Health","Child Health","Child Health", "Child Health","Child Health","Child Health","Child Health","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","Digestive","ENT","ENT","ENT","ENT","ENT","Eyes","Eyes","Eyes","Eyes","Eyes","Eyes","General Duties","General Duties","General Duties","General Duties","General Duties","General Duties","General Duties","General Duties","Homeostatic","Homeostatic","Homeostatic","Homeostatic","Homeostatic","Homeostatic","Homeostatic","Homeostatic","Homeostatic","Homeostatic","Infectious Diseases","Infectious Diseases","Infectious Diseases","Infectious Diseases","Infectious Diseases","Infectious Diseases","Infectious Diseases","Infectious Diseases","Infectious Diseases","Mental Health","Mental Health","Mental Health","Mental Health","Mental Health","Mental Health","Mental Health","Mental Health","Mental Health","Mental Health","Mental Health","Mental Health","Mental Health","Musculoskeletal","Musculoskeletal","Musculoskeletal","Musculoskeletal","Musculoskeletal","Musculoskeletal","Musculoskeletal","Musculoskeletal","Musculoskeletal","Musculoskeletal","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Neurological","Renal","Renal","Renal","Renal","Renal","Renal","Renal","Renal","Renal","Renal","Renal","Renal","Respiratory","Respiratory","Respiratory","Respiratory","Respiratory","Respiratory","Respiratory","Seriously Ill Patient","Seriously Ill Patient","Seriously Ill Patient","Seriously Ill Patient","Seriously Ill Patient","Seriously Ill Patient","Seriously Ill Patient","Seriously Ill Patient","Skin","Skin","Skin","Skin","Skin","Skin","Skin","Skin","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Women's Health","Unclassified")
  subjectList<-data.frame(no,Subject,subjectName)
  
  Skill<-df[[colSkill]]
  Subject<-df[[colSubject]]  
  dataHeatmap1<-data.frame(Skill,Subject)
  
  if(!is.null(itemsRemoved)){
    dataHeatmap1<-dataHeatmap1[-itemsRemoved,]}
  
  dataHeatmap2<-join(dataHeatmap1, subjectList, by = "Subject") # populates a column for the 'subject.name' by referencing the SubjectList (the numeric list)
  dataHeatmap2<-join(dataHeatmap2, skillList,   by = "Skill")   # populates a column for the 'skill.name' by referencing the SkillList (the alphabetic list)
  
  numbersHeatmap<-table(dataHeatmap2$subjectName,dataHeatmap2$skillName) # creates a frequency table
  
  dataHeatmap3<-as.data.frame.matrix(numbersHeatmap)            # convert it to a dataframe
  
  dataHeatmap3$Subject=rownames(dataHeatmap3)                   # make rownames (the subjects) into a Subject column
  
  dataHeatmap4<-melt(dataHeatmap3)                              # melt Data2 so have three columns of Subject, variable (Skill) and value (frequency)
  
  colnames(dataHeatmap4)[2]<-"Skill"                            # rename variable column to Skill
  colnames(dataHeatmap4)[3]<-"Frequency"                        # rename value column to Frequency
  
  t<-aggregate(Frequency~Subject,dataHeatmap4,sum)
  totalAll<-sum(t[2])   # the total number of items used in plot
  
  if(zerosBlank=="Yes"){dataHeatmap4$Zero<-ifelse(dataHeatmap4$Frequency==0,"Yes","No")}
  
  
  # Creat the plot for test
  p<-ggplot(dataHeatmap4, aes(x=Skill,y=Subject)) +                
    geom_tile(aes(fill = Frequency),colour = "white") +                      # sets as tiles with base colour being white 
    scale_fill_gradient(limits=c(0,10),low = "white", high = "#3D52A1") +    # a zero tile will be white, moving to blue (the excellent colour) as frequencies go up 
    theme_grey(base_size = 10) +             # sets the tick mark (axes) text size
    theme(panel.background=element_rect(fill="#4477AA")) +                   # sets the background colour of the 'totals' grid area 
    theme(panel.grid.major=element_line(colour="#4477AA")) +   # sets the gridline colour of the 'totals' grid area to match background colour so not visible  
    xlab("Skill") +																					
    ylab("Subject") +
    theme(legend.position = "none") +     # removes the legend (as will add numbers within tiles)
    theme(axis.text.x=element_text(angle=45,hjust=1)) + # angles the x-axis labels
    theme(axis.ticks=element_blank()) +   # removes tick marks from x and y axes
    geom_text(aes(label=Frequency), data=cbind(aggregate(Frequency~Subject, dataHeatmap4, sum), Skill=" Total")) +
    geom_text(aes(label=Frequency), data=cbind(aggregate(Frequency~Skill, dataHeatmap4, sum), Subject=" Total")) +
    geom_text(x=1,y=1,label=paste0(totalAll))
  
  if(zerosBlank=="Yes"){p<-p+geom_text(aes(label=Frequency, colour=Zero, size=2.5)) + scale_colour_manual(values=c("black","white"))}# adds numbers to tiles - zeros white so do not show
  if(zerosBlank=="No") {p<-p+geom_text(aes(label=Frequency, size=2.5))} # adds numbers to tiles
  p
  
} # END OF FUNCTION
#########################


