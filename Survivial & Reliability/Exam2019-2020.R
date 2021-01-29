#======================
# Loading the dataset
#======================
leukem<-read.table(file="C:/Users/kebro/OneDrive/KU_Leuven/Survivial & Reliability/Leuk.dat",na.strings=".",
colClasses=c(NA,"character",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"character","character",NA,NA,"character",NA),col.names=c("ID","DateStudy","Treat","Sex","Age","FAB","Karnof","WhiteCells",
"Platelets","Hemoglobin","Eval","CR","NumbCR","DateCR","DateFollow","StatusFollow","StatusBone","DateBone","Incl"))
head(leukem)
attach(leukem)

#==================================
# Creating time to event variables
#==================================
DatumStudy<-as.Date(DateStudy,"%m%d%y")
DatumCR<-as.Date(DateCR,"%m%d%y")
DatumFollow<-as.Date(DateFollow,"%m%d%y")
DatumBone<-as.Date(DateBone,"%m%d%y")

TimeCR<-difftime(DatumCR,DatumStudy)
TimeSurv<-difftime(DatumFollow,DatumStudy)
TimeBone<-difftime(DatumBone,DatumStudy)
TimeEvent<-data.frame(DatumStudy,DatumCR,DatumFollow,DatumBone,TimeCR,TimeBone)
head(TimeEvent)

#================================================
# Creating the observed time to event variables
#================================================
IndCR<-1*I(CR=="Y")
TimetoCR<-TimeCR
TimetoCR[IndCR==0]<-TimeSurv[IndCR==0]
IndSurv<-1*I(StatusFollow=="D")
IndBone<-1*I(StatusBone=="Y")
TimetoBone<-TimeBone
TimetoBone[IndBone==0]<-TimeSurv[IndBone==0]
TimeObs<-data.frame(TimetoCR,IndCR,TimetoBone,IndBone,TimeSurv,IndSurv)
head(TimeObs)

Leukefinal<-data.frame(leukem,TimeEvent,TimeObs)
head(Leukefinal)

