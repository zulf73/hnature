vt.rsq<-rep(0,5)
ft.rsq<-rep(0,5)
t5<-rep(0,5)
t5[2:5]<-out$t
t50<-1:5
out<-det.trust.pars( log(abs(wtt2)+0.1))
for (k in 1:5){vt.rsq[k]<-summary(lm( log(abs(wtt2[,k])+0.01)~t5 ))$r.squared}
for (k in 1:5){ft.rsq[k]<-summary(lm( log(abs(wtt2[,k])+0.01)~t50 ))$r.squared}
xtable(data.frame(fixed=ft.rsq*100,var=vt.rsq*100))

time_rsq_comp<-function(data, out,do.sort=T){
  vt.rsq<-rep(0,5)
  ft.rsq<-rep(0,5)
  t5<-rep(0,5)
  if (do.sort){
    t5[2:5]<- t5[2:5]+sort(out$t)
  } else {
    t5[2:5]<- t5[2:5]+out$t
  }
  t50<-1:5
  for (k in 1:5){vt.rsq[k]<-summary(lm( log(abs(data[,k])+0.01)~t5 ))$r.squared}
  for (k in 1:5){ft.rsq[k]<-summary(lm( log(abs(data[,k])+0.01)~t50 ))$r.squared}
  data.frame(fixed=ft.rsq*100,var=vt.rsq*100)
}

theta_table<-function( ethid ){
  thetas<-matrix(0,nrow=5,ncol=5)
  thetas[1,]<-as.numeric(trfam.out$theta[ ethid,2:6])
  thetas[3,]<-as.numeric(trknown.out$theta[ethid,2:6])
  thetas[2,]<-as.numeric(trnbd.out$theta[ethid,2:6])
  thetas[4,]<-as.numeric(trstrangers.out$theta[ethid,2:6])
  thetas[5,]<-as.numeric(trothernat.out$theta[ethid,2:6])
  thetas
}
