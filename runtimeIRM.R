#runtimeIRM
library(data.table)
library(sqldf)

# probability distribution for Majors
createMajorProb<-function(){
  majorVecNames<-c("Liberal Arts","Business","Science")
  majorVecProb<-c(.5, .25, .25)
  names(majorVecProb)<-majorVecNames
  return (majorVecProb)
}

# probability distribution for cohort transitions
createXtionProb<-function(XmatProb){
  XmatNames<-c("EN-EN","EN-NE","EN-GR", "NE-EN","NE-NE","NE-GR", "GR-EN","GR-NE","GR-GR")
  names(XmatProb)<-XmatNames
  return (XmatProb)
}

#create a cohort of x +-y% cohorts
cohortID<-function(startYear,size,sizeVar, majorVecProb) {
  # initial conditions below used for testing
  # size<-1000;sizeVar<-.1;startYear=2010
  
  # how big is cohort
  max<-size * (1 + runif(n=1 ,max=sizeVar))
  
  # create ids
  ids<-1:max
  ID<-sample(ids, size=max, replace=FALSE) 
  
  # create data table
  cohort<-as.data.table(ID)
  #cohort<-data.frame(ID, row.names=ID)
  
  #name of the cohort
  cohort$Name<-replicate(max, paste("C", startYear, sep=""))
  
  # startYear
  cohort$Year<-startYear
  
  # given everyone a program
  cohort$Major<- sample(x=names(majorVecProb), size = max, replace = TRUE, prob = majorVecProb)
  #all students are enrolled
  cohort$Enrolled<-TRUE
  cohort$NonEnrolled<-FALSE
  cohort$Graduated<-FALSE
  
  return(cohort) 
}


# create transition matrix
createCohortXmat<-function(vecProb){
  xmatLabel<-c("Enrolled", "NonEnrolled", "Graduated")
  xmatProb<-matrix(vecProb, nrow=3, byrow=TRUE, dimnames=list(xmatLabel,xmatLabel))
  return (xmatProb)
}

# create transition array
createCohortXarray<-function(Xmats){
  xmatYear<
  xmatLabel<-c("Enrolled", "NonEnrolled", "Graduated")
  xmatProb<-array(data=Xmats, nrow=3, byrow=TRUE, dimnames=list(xmatLabel,xmatLabel))
  return (xmatProb)
}


cohortUpdate<-function(cohort, xmat){
  
  #start with previous year
  cohortUp<-cohort
  # add 1 to yeare
  cohortUp$Year<-cohortUp$Year + 1
  
  #currently enrolled
  cohortUp_EN<-cohortUp$Enrolled==TRUE
  #nonenrolld rate
  cohortUp_NE<-rbinom(nrow(cohortUp), 1, xmat[1,2])==1
  #when these two are true
  cohortUp_ENNE<-(cohortUp_NE == TRUE) & (cohortUp_EN == TRUE)
  # set nonenrolled to true
  cohortUp$NonEnrolled[cohortUp_ENNE]<-TRUE
  # and enrolled to false
  cohortUp$Enrolled[cohortUp_ENNE]<-FALSE
  
  
  #currently enrolled
  cohortUp_EN<-cohortUp$Enrolled==TRUE
  #graduates
  cohortUp_GR<-rbinom(nrow(cohortUp), 1, xmat[1,3])==1 
  #when these two are true
  cohortUp_ENGR<-(cohortUp_GR == TRUE) & (cohortUp_EN == TRUE)
  # set graduated to true
  cohortUp$Graduated[cohortUp_ENGR]<-TRUE
  # and enrolled to false
  cohortUp$Enrolled[cohortUp_ENGR]<-FALSE
  
  return(cohortUp)
  
}
