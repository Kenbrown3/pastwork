#testing multiple smoothing intervals

#This determines the input data change to dec. 10 data when needed
datain=dataset_HMM

#which smoothing intervals should be tested
lowerbound=50
upperbound=5000
interval=50
sequence=seq(lowerbound,upperbound,by=interval)

#number of hidden states
nclust=3

#result  matrix, view to examine which smoothing intervals are the most unique
matrixdiag=matrix(rep(rep(0,length(sequence)),nclust),nrow=nclust)
colnames(matrixdiag)=sequence

#matrix for AIC values
aicmatrix=matrix(rep(0,length(sequence)),nrow=1)
colnames(aicmatrix)=sequence

#filling of the result matrix, change dependencies above to get a new matrix
colind=1
for(i in sequence){
  dataset_HMM2=datain
  dataset_HMM2$smoothdata_in=rollmean(dataset_HMM2$input_values,i,fill=NA,align="right")
  dataset_HMM2$smoothdata_out=rollmean(dataset_HMM2$output_values,i,fill=NA,align="right")
  modsmooth <- depmix(smoothdata_out ~ smoothdata_in, family = gaussian(), nstates = nclust,data=dataset_HMM2)
  fitsmooth <- fit(modsmooth)
  matrixdiag[1:nclust,colind]=
    matrix(getpars(fitsmooth)[seq(nclust+1,(nclust+1)*nclust,by=nclust+1)],nrow=nclust)
  aicmatrix[colind]=AIC(fitsmooth)
  colind=colind+1
}