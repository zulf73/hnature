# Assume that the given sequence is
# Markov and estimate the parameters
# directly by MLE.  Check whether 
# L2 distribution is better than 
# e=0.77
library(ramify)
wvs7<-readRDS("wvs7.rds")
data0<-na.omit( data.matrix(wvs7[,vars]))
data<-flatten(data0)
mcFit<-markovchainFit(data,laplacian=1,confidencelevel = 0.99)
b3<-check_moral_markov(parsFromTransitionMatrix(mcFit$estimate))
