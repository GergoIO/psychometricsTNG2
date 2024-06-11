#' Historgram, overlaid with boxplot and grade boundaries for degree classifications from percentage scores.
#' 
#' @description fnDRHistogram creates a histogram of scores overlaid with a boxplot, with shaded boundaries for degree classifications (Fail/3rd/2:2/2:1/1st).
#' 
#' The output from fnDRHistogram is a ggplot object which can then be saved or customised as needed.  
#' 
#' @usage fnDRHistogram(Data, Boundaries) 
#' 
#' @param Data Data should be a vector of values representing percentage scores, e.g. c(20, 33, 15, 76, 50) or Data.All$Scores.
#' 
#' The input for Data will be coerced into a numeric vector. This may result in NAs.
#' 
#' @param Boundaries Boundaries should be a vector of four values, reflecting the lowest scores required for a 3rd, 2:2, 2:1, and 1st classification.

#'  @note Not as robust as other functions in it's checking for inputs. Colours are based on fnColours, change to this function will change classification colours in fnDRHistogram
#' 
#' @examples
#' 
#' # Basic Usage; Data examples from Data.Example in PSMD.Psychometrics
#' 
#'   fnDRHistogram(Data=c(30,34,54,67,70,72,49,80,54,59))
#'   
#' @author Dr Daniel Zahra, \email{daniel.zahra@plymouth.ac.uk}
#'
#' @export

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### fnDRHistogram - last updated 180121 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Function designed to plot score distribution, overlay histogram, and shade deegree classifications

# Inputs
#
# Data        Single vector containing all data points; e.g. c("U", "S", "S", "B", "E") or Data.All$Grades
#             Can be of any data type.
#             Can handle any combination of abbreviations and/or full-text labels
#             (Within reason; add any unusual ones to Data.Scheme)
#
# Boundaries  Numeric vector specifying FOUR boundaries for degree class; minimum scores for 3rd, 2:2, 2:1, and 1st
#             If not specified, default is c(40,50,60,70)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# TO DEVELOP
#
# Better error checking and warning messages for wider range of inputs
#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fnDRHistogram<-function(Data=NA, Boundaries=c(40,50,60,70)){

Boundaries<-as.numeric(Boundaries)
  
# Test for Data being NA/missing, and correct number of Boundaries (4), return warnings
  
  if(is.na(Data)[1]){warning("Data missing")}else{
    if(is.data.frame(Data)){warning("Data is a dataframe, please specify column")}else{
    if(length(Boundaries)>4){warning("Too many boundaries specified")}else{
      if(length(Boundaries)<4){warning("Too few boundaries specified")}else{
        
# Create plot if Data exists and length(Boundaries)==4  

Scores<-as.numeric(Data)
Data.Scores<-data.frame(Scores=Scores)
MaxFreq<-max(as.numeric(strsplit(toString(table(Data.Scores$Scores)),", ")[[1]]))
        
Plot.1<-ggplot()+
  geom_rect(aes(xmin=0, xmax=Boundaries[1], ymin=0, ymax=Inf), fill=fnColours(DataOrScheme="UBLSHSE")[1], alpha=0.25)+
  geom_rect(aes(xmin=Boundaries[1], xmax=Boundaries[2], ymin=0, ymax=Inf), fill=fnColours(DataOrScheme="UBLSHSE")[2], alpha=0.25)+
  geom_rect(aes(xmin=Boundaries[2], xmax=Boundaries[3], ymin=0, ymax=Inf), fill=fnColours(DataOrScheme="UBLSHSE")[3], alpha=0.25)+
  geom_rect(aes(xmin=Boundaries[3], xmax=Boundaries[4], ymin=0, ymax=Inf), fill=fnColours(DataOrScheme="UBLSHSE")[4], alpha=0.25)+
  geom_rect(aes(xmin=Boundaries[4], xmax=100, ymin=0, ymax=Inf), fill=fnColours(DataOrScheme="UBLSHSE")[5], alpha=0.25)+
  geom_bar(data=Data.Scores, aes(x=Scores), fill="grey10")+
  scale_x_continuous(name="Score (%)", breaks= seq(0, 100, by=10), limits=c(0,100), expand = c(0,0))+
  scale_y_continuous(name="Frequency", breaks= seq(0, MaxFreq+2, by=1))+
  theme_psmd()+
  theme(panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 90, vjust=0.5, size=8))+
  geom_boxplot(data=Data.Scores, aes(x=Scores, y=MaxFreq+1), fill="grey75")+
  annotate("text", x = (Boundaries[1]/2), y = MaxFreq+2, label = "Fail", colour=fnColours(DataOrScheme="UBLSHSE")[1], size=4, fontface="bold")+
  annotate("text", x = ((Boundaries[2]-Boundaries[1])/2+Boundaries[1]), y = MaxFreq+2, label = "3rd", colour=fnColours(DataOrScheme="UBLSHSE")[2], size=4, fontface="bold")+
  annotate("text", x = ((Boundaries[3]-Boundaries[2])/2+Boundaries[2]), y = MaxFreq+2, label = "2:2", colour=fnColours(DataOrScheme="UBLSHSE")[3], size=4, fontface="bold")+
  annotate("text", x = ((Boundaries[4]-Boundaries[3])/2+Boundaries[3]), y = MaxFreq+2, label = "2:1", colour=fnColours(DataOrScheme="UBLSHSE")[4], size=4, fontface="bold")+
  annotate("text", x = ((100-Boundaries[4])/2+Boundaries[4]), y = MaxFreq+2, label = "1st", colour=fnColours(DataOrScheme="UBLSHSE")[5], size=4, fontface="bold")

# Make plot available outside function
return(Plot.1)

      }}}}}

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