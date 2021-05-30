mvar<-c("Q177",
        "Q178",
        "Q179",
        "Q180",
        "Q181",
        "Q182",
        "Q183",
        "Q184",
        "Q185",
        "Q186",
        "Q187",
        "Q188",
        "Q189",
        "Q190",
        "Q191",
        "Q192",
        "Q193",
        "Q194",
        "Q195"
)

eth.mid.sd<-function(){
  ss<-rep(0,length(mvar))
  for (j in 1:length(mvar)){
    A<-rownrm(table(na.omit(polv[,c("eth",mvar[j])])))
    s<-sd(rowSums(A[,1:5]))
    print(paste(mvar[j],s))
    ss[j]<-s
  }
  data.frame(var=mvar, ethsd=ss)
}

  
