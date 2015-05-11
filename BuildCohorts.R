
source("runtimeIRM.R")

rm(cohort)

#get major probabikity vector
majorVecProb<- createMajorProb()


#build starting cohorts
C2010_Y0<-cohortID(startYear=2010, size=1000, sizeVar=.1, majorVecProb= majorVecProb)
C2011_Y0<-cohortID(startYear=2011, size=1000, sizeVar=.1, majorVecProb= majorVecProb)
C2012_Y0<-cohortID(startYear=2012, size=1000, sizeVar=.1, majorVecProb= majorVecProb)
C2013_Y0<-cohortID(startYear=2013, size=1000, sizeVar=.1, majorVecProb= majorVecProb)
C2014_Y0<-cohortID(startYear=2014, size=1000, sizeVar=.1, majorVecProb= majorVecProb)
C2015_Y0<-cohortID(startYear=2015, size=1000, sizeVar=.1, majorVecProb= majorVecProb)
C2016_Y0<-cohortID(startYear=2016, size=1000, sizeVar=.1, majorVecProb= majorVecProb)


grRate<-c(0.0, 0.0, 1.0)
neRate<-c(0.0 ,1.0, 0.0)


#build y1 results
enRate<-c(0.8, 0.2, 0.0)
prob1<-createXtionProb(c(enRate,neRate,grRate))
xmatProb1<-createCohortXmat(prob1)
C2010_Y1<-cohortUpdate(C2010_Y0, xmatProb1)

enRate<-c(0.35, 0.1, 0.5)
prob2<-createXtionProb(c(enRate,neRate,grRate))
xmatProb2<-createCohortXmat(prob2)
C2010_Y2<-cohortUpdate(C2010_Y1, xmatProb2 )

enRate<-c(0.1, 0.1, 0.8)
prob2<-createXtionProb(c(enRate,neRate,grRate))
xmatProb2<-createCohortXmat(prob3)
C2010_Y3<-cohortUpdate(C2010_Y2, xmatProb3 )

sum(C2010_Y3$NonEnrolled)
sum(C2010_Y3$Enrolled)
sum(C2010_Y3$Graduated)




prob4<-c(0.0, 0.1, 0.9, 0.0 ,1.0, 0.0, 0.0, 0.0, 1.0)
xmatProb4<-createCohortXmat(prob4)


prob5<-c(0.0, 0.0, 1.0, 0.0 ,1.0, 0.0, 0.0, 0.0, 1.0)
xmatProb5<-createCohortXmat(prob5)


x<-list(xmatProb1,xmatProb2)

nc1<-c(nrow(z),0,0)
(mc1<-round(nc1*xmatProb1))
nc2<-colSums(mc1)
(mc2<-round(nc1*xmatProb2))
nc3<-colSums(mc2)
(mc3<-nc3*xmatProb3)
nc4<-colSums(mc3)
(mc4<-round(nc4*xmatProb4))
nc5<-colSums(mc4)
(mc5<-round(nc5*xmatProb5))
