#' An example dataset.
#' 
#' @description An example data frame with 20 rows and 18 variables: 
#' 
#' \itemize{
#'   \item Student -- ID Student identification numbers (Numeric)
#'   \item Gender. Student Gender ("Male"/"Female", String)
#'   \item Ethnicity -- Student Ethnicity ("White"/"Asian"/"Other", String)
#'   \item Disability -- Student Disability ("No known disability"/"Specific learning difficulty"/"Other disability", String)
#'   \item Stage -- Student stage (Numeric)
#'   \item Score.Perc -- Student percentage scores, two decimal places (Numeric)
#'   \item Score.Raw -- Student raw scores, integers (Numeric)
#'   \item Score.Restricted -- Student raw scores, integers between 0-25 (Numeric)
#'   \item Score.Neg -- Student percentage scores including some negative values (Numeric)
#'   \item Grade.UBSE -- Grades in a UBSE scheme (Abbreviated String)
#'   \item Grade.USE -- Grades in a USE scheme (Abbreviated String)
#'   \item Grade.CNINC -- Grades in a CNINC scheme (String)
#'   \item Grade.PF -- Grades in a PF scheme (Abbreviated String)
#'   \item Grade.PFE -- Grades in a PFE scheme (Abbreviated String)
#'   \item Responses.CIDK -- Responses in a CIDK scheme (Abbreviated String)
#'   \item Responses.CIDK.Numeric -- Responses in a CIDK scheme (Numeric)
#'   \item Assessor.ID -- Assessor identification numbers (Numeric)
#'   \item Element --Element scores, integers between 0-4 (Numeric)
#' }
#' 
#' @author Dr Daniel Zahra, \email{daniel.zahra@plymouth.ac.uk}
#' 
#' @export

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### PSMD.Psychometrics -- dataExample #### 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dataExample<-data.frame(
  Student.ID=c(1:20),
  Gender=rep(c("Male","Male","Female","Female","Female"),4),
  Ethnicity=rep(c("White","White","White","Asian","Other"),4),
  Disability=rep(c("No known disability","Other disability","No known disability","Specific learning difficulty","No known disability","No known disability","No known disability","No known disability","Specific learning difficulty","No known disability"),2),
  Stage=c(1,1,1,1,1,2,2,2,2,2,3,3,3,4,4,4,4,5,5,5),
  Score.Perc=c(5.22,10.98,15.24,17.66,25.89,30.00,36.74,30.55,40.89,46.98,55.41,60.76,65.36,70.00,78.62,71.00,68.44,60.35,81.56,97.68),
  Score.Raw=c(5,10,15,17,25,30,36,30,40,46,55,60,65,70,78,71,68,60,81,97),
  Score.Restricted=c(5,0,15,17,25,11,12,7,13,15,22,25,25,19,19,15,24,18,21,8),
  Score.Neg=c(-5.22,10.98,15.24,17.66,25.89,-10.48,36.74,30.55,40.89,46.98,55.41,60.76,65.36,70.00,78.62,-0.25,68.44,60.35,81.56,97.68),
  Grade.UBSE=c("U","B","S","S","E","U","B","S","S","E","S","S","E","B","S","S","E","B","S","E"),
  Grade.USE=c("U","S","S","S","E","U","S","S","S","E","S","S","E","S","S","S","E","S","S","E"),
  Grade.CNINC=c("NC","C","NI","C","C","NC","C","NI","C","NI","NI","C","C","NC","NI","C","C","NC","C","C"),
  Grade.PF=c("F","F","P","P","P","F","F","P","P","P","P","F","P","F","F","P","P","F","P","P"),
  Grade.PFE=c("F","F","P","P","E","F","F","P","P","E","P","P","E","F","P","P","E","F","P","P"),
  Response.CIDK=c("C","C","DK","I","C","DK","DK","C","C","I","DK","I","C","C","C","C","C","C","I","DK"),
  Response.CIDK.Numeric=c(1,1,0,-0.25,1,0,0,1,1,-0.25,0,-0.25,1,1,1,1,1,1,-0.25,0),
  Assessor.ID=c(109,109,114,458,458,12,12,109,114,109,758,758,758,458,4,4,114,12,114,109),
  Score.Element=c(1,2,3,4,1,2,3,4,1,1,2,3,1,2,3,4,3,2,3,3)
  )

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
