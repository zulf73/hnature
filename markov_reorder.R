# Reorder variables according to
# stationary distribution of generator
b<-check_moral_markov(res2$solution)
steady<-matrix.power(transitionMatrixFromPars(res2$solution),
                     50)[1,]
steady.dist<-rep(0,16)
for (r in 1:16){ 
  steady.dist[r]<-norm(b$I[r,]-steady,type="2")
}
var.idx<-order( steady.dist, decreasing=T)
backup.vars<-vars
vars<-backup.vars[var.idx]
