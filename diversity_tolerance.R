# Effect of Actual Diversity on Tolerance 
tols<-matrix(0,nrow=6,ncol=6)
for (r in 1:6){ 
  A.racial<-table(na.omit(polv[polv$eth==eths[r],c("Q12","Q19")]))
  A.religious<-table(na.omit(polv[polv$eth==eths[r],c("Q12","Q23")]))
  A.language<-table(na.omit(polv[polv$eth==eths[r],c("Q12","Q26")]))
  tols[r,1]<-A.racial[1,2]/sum(A.racial[,2])
  tols[r,2]<-A.racial[1,1]/sum(A.racial[,1])
  tols[r,3]<-A.religious[1,2]/sum(A.religious[,2])
  tols[r,4]<-A.religious[1,1]/sum(A.religious[,1])
  tols[r,5]<-A.language[1,2]/sum(A.language[,2])
  tols[r,6]<-A.language[1,1]/sum(A.language[,1])
}
df<-data.frame(tols)
colnames(df)<-c("race.nondiv","race.div", 
                "relig.nondiv","relig.div",
                "language.nondiv","language.div")
row.names(df)<-eths

