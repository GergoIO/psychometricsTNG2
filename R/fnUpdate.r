#' A function to automate updating the psychometricsPSMD package
#' 
#' @description A function to automate updating the psychometricsPSMD package. .
#'
#'Adapted from an anonymous post at Statistically Significant; http://alandgraf.blogspot.co.uk/2012/06/rounding-in-r.html
#' 
#' @usage fnUpdate(PackageName, Reinstall)
#' 
#' @param PackageName Can be omitted if you want to update psychometricsPSMD as it defaults to this. Specifying any other name should update the named package, but ssee 'Note' section.
#' @param Reinstall Reinstall determines whether the package is reinstalled and reattached regardless of its current status (e.g. When Reinstall==TRUE, the package named in PackageName will be reinstalled and reattached even if it is already installed and/or attached). Set Reinstall=TRUE for updating psychometricsPSMD.
#' 
#' @note DZ260417: The argument character.only = TRUE works for library(), but possibly not for install.packages. So although PackageName in fnUpdate() could be changed to install and attach any package, it might hit errors at the install.package() phase. PackageName currently defaults to psychometricsPSMD though, so unless specified otherwise, fnUpdate handles psychometricsPSMD. 
#' 
#' @examples  fnUpdate(Reinstall = TRUE) will force a clean install and reattachment of psychometricsPSMD.
#'
#' @author Dr Daniel Zahra, \email{daniel.zahra@plymouth.ac.uk}
#' 
#' @export

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### fnUpdate (DZ) #### 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Function to automate updating psychometricsPSMD package

# DZ notes to self:
# The argument character.only = TRUE works for library(), but possibly not for install.packages
# So although PackageName in Fn.Update() could be changed to install and attach any package, it might hit errors at the install.package() phase.
# PackageName currently defaults to psychometricsPSMD though, so unless specified otherwise, Fn.Update handles psychometricsPSMD

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


fnUpdate<-function(PackageName="psychometricsPSMD", Reinstall=FALSE){
  
  # Defaults to check for psychometricsPSMD but can be usesd for other pakages by changing the PackageName argument
  
  # Set check defaults

  Package.Installed<-"No"
  Package.Attached<-"No"
  Devtools.Installed<-"No"
  Devtools.Attached<-"No"
  
  # Set authoristaion key for the private repository on PlymouthUniversitySD (generated to DZ account but works)
  GITHUB_PAT="2e5fa579f757ed9bd45f8aacc94e40a14fd44d71"
  
  # List installed and attached packages
  
  Packages.Installed<-data.frame(installed.packages())$Package
  Session.Info<-sessionInfo()
  Packages.Attached<-c(Session.Info$basePkgs,names(Session.Info$otherPkgs))
  
  # Check if devtools is attached. Install and/or attach as necessary.
  
  if(length(Packages.Attached[Packages.Attached=="devtools"])==1){
    Devtools.Attached<-"Yes"
    print("Package already attached -- devtools")}
  if(length(Packages.Installed[Packages.Installed=="devtools"])==1){Devtools.Installed<-"Yes"}
  if(Devtools.Attached=="No"){
      if(Devtools.Installed=="No"){
      install.packages("devtools")
      print("Installed Package -- devtools")}
    library("devtools")
    print("Attached Package -- devtools")}
  
  # Check is package is attached and/or installed
  
  if(length(Packages.Installed[Packages.Installed==PackageName])==1){Package.Installed<-"Yes"}
  if(length(Packages.Attached[Packages.Attached==PackageName])==1){Package.Attached<-"Yes"}
  
  # Install and/or attach PackageName as required
  
  if(Package.Installed=="Yes" & Package.Attached=="Yes"){
    print(paste("Package already installed -- ",PackageName,sep=""))
    print(paste("Package already attached -- ",PackageName,sep=""))}
  
  if(Package.Installed=="No" & Package.Attached=="No"){
    if(PackageName=="psychometricsPSMD"){
      install_github("PlymouthUniversitySD/psychometricsPSMD", force=TRUE, auth_token=GITHUB_PAT, private = TRUE)
      print(paste("Package installed -- ",PackageName,sep=""))
      }else{
        install.packages(PackageName, character.only = TRUE)
        print(paste("Package installed -- ",PackageName,sep=""))}
    library(PackageName, character.only = TRUE)
    print(paste("Package attached -- ",PackageName,sep=""))
    print(paste("NOTE: You may need to restart your R and/or RStudio session for help files to load correctly.",sep=""))}
  
  if(Package.Installed=="Yes" & Package.Attached=="No"){
    library(PackageName, character.only = TRUE)
    Pacakage.Attached<-"Yes"
    print(paste("Package attached -- ",PackageName,sep=""))}
  
  # Force reinstall if Reinstall==TRUE

  if(Reinstall==TRUE){
    if(PackageName=="psychometricsPSMD"){detach("package:psychometricsPSMD", unload=TRUE)}
    remove.packages(PackageName)
    print(paste("Forcing package reinstall -- ",PackageName,sep=""))
    if(PackageName=="psychometricsPSMD"){
      install_github("PlymouthUniversitySD/psychometricsPSMD", force=TRUE, auth_token=GITHUB_PAT, private = TRUE)
      print(paste("Package installed -- ",PackageName,sep=""))
      }else{
        install.packages(PackageName, character.only = TRUE)
        print(paste("Package installed -- ",PackageName,sep=""))}
    library(PackageName, character.only = TRUE)
    print(paste("Package attached -- ",PackageName,sep=""))
    print(paste("NOTE: You may need to restart your R and/or RStudio session for help files to load correctly.",sep=""))}
  
  } # Close function