# We let x be the parameters of the subdiagonal
# of P in form appropriate for an optimiser
check_moral_markov<-function( x ){
  
  P <- transitionMatrixFromPars( x )
  states <- as.character(1:10)
  mcVals = new("markovchain", 
               states = states,
               transitionMatrix = P,          
               name = "Vals")
  nvars<-length(vars)
  Imtx <- matrix( 0, nrow=nvars,ncol=10)
  Jmtx <- matrix( 0, nrow=nvars, ncol=10)
  I<-nrm(table(polv[,"Q177"]))
  Imtx[1,] <- I
  M<-10000
  Y1<-sample(1:10,M,replace=TRUE,prob=I)
  all.Y<-matrix(0,nrow=M,ncol=nvars)
  all.Y[,1]<-Y1
  for (kk in 1:M) {
    #print(kk)
    outs <- markovchainSequence(n = nvars-1, markovchain = mcVals, 
                                t0 = all.Y[kk,1], 
                                include.t0 = TRUE )
    all.Y[kk,1:nvars]<-outs  
  }
  
  for ( r in 1:nvars ){
    iv<-nrm(table(polv[,vars[r]]))
    jv<-nrm(table(all.Y[,r]))
    ivn<-rep(0,10)
    jvn<-rep(0,10)
    for ( kc in 1:10 ){
      a<-iv[as.character(kc)]
      b<-jv[as.character(kc)]
      if (!is.null(a)){
        ivn[kc]<-a
      }
      if (!is.null(b)){
        jvn[kc]<-b
      }
      
    }
    Imtx[r,]<-ivn
    Jmtx[r,]<-jvn
  }
  
  print('calculating l2 distance')  
  l2.dist <- 0
  hits<-0
  residuals.df<-data.frame()
  for (r in 1:nvars){
    d1<- norm(abs(Imtx[r,]),type="2")
    d2<- norm(abs(Jmtx[r,]),type="2")
    d <- norm( abs(Imtx[r,] - Jmtx[r,] ), type="2")
    #print(paste("r=",r,"d1=",d1,"d2=",d2,"d=",d))
    if (!is.na(d)){
      hits<-hits+1
      residuals.df<-rbind( residuals.df, Imtx[r,]-Jmtx[r,])
      l2.dist <- l2.dist + d^2
    }
  }
  l2.dist<-sqrt(l2.dist)
  print(paste('hits=',hits))
  print(l2.dist)
  l2.dist
  names(residuals.df)<-as.character(1:10)
  list(I=Imtx,J=Jmtx,res=residuals.df)
}

rownrm<-function(A){
  n<-dim(A)[1]
  B<-A
  for (k in 1:n){
    B[k,]<-nrm(B[k,])
  }
  B
}

tts<-function(var){
  X<-na.omit(polv[,c("eth",var)])
  print(dim(X))
  weth<-unique(as.character(as_factor(X[,1])))
  N<-dim(weth)[1]
  out<-data.frame()
  for (j in 2:N){
    for (k in 1:j){
      print(weth[j,1])
      x<-X[which(as.character(as_factor(X$eth))
                 ==weth[j,1]),2]
      print(x)
      y<-X[which(as.character(as_factor(X$eth))
                 ==weth[k,1]),2]
      print(y)
      res<-t.test(x,y)$p.value
      rbind(c(var,weth[j,1],weth[k,1],res))
    } 
  }
  out
}

moral.table<-function( var ){
  X<-table(polv[,c("eth",var)])
  N<-dim(X)[1]
  for( j in 1:N){
    X[j,]<-nrm(X[j,])
  }
  X  
}


moral.table.data<-function( var, data){
  X<-t(table(data[,c(var)]))
  N<-dim(X)[1]
  print(N)
  for( j in 1:N){
    X[j,]<-nrm(X[j,])
  }
  X  
}

tts2<-function(var){
  X<-table(polv[,c("eth",var)])
  weth<-row.names(X)
  N<-length(weth)
  for( j in 1:N){
    X[j,]<-nrm(X[j,])
  }
  out<-data.frame()
  for (j in 2:N){
    for (k in 1:(j-1)){
      print(weth[j])
      x<-as.numeric(X[j,])
      print(x)
      y<-as.numeric(X[k,])
      print(y)
      res<-t.test(x,y,paired=T)
      print(names(res))
      print(paste("diff=",norm(x-y,type="2")))
      print(res)
      out<-rbind(out,c(var,weth[j],weth[k],res$statistic,res$p.value))
    } 
  }
  names(out)<-c("var","eth1","eth2","tstat","pval")
  out
}

moral.diff<-function(){
  mvars <-names(polv)[4:22]
  out<-data.frame()
  for (v in mvars){
    mdiff<-tts2( v )
    out<-rbind(out,mdiff)
  }
  out
}

plt.moral.table<-function(v){
  A<-moral.table(v)
  eth<-row.names(A)
  B<-data.frame()
  for (k in 1:6){
    for (r in 1:10){
      B<-rbind(B,c( eth[k], as.numeric(r), A[k,r]))
    }
  }
  names(B)<-c("eth", "x", "y")
  B$x<-as.numeric(B$x)
  B$y<-as.numeric(B$y)
  print(head(B))
  p<-ggplot(B,aes(x=x,y=y,group=eth)) + 
    geom_line(aes(color=eth),size=1) +
    ggtitle(v)
  
  list(plt=p,data=B)
}


variation.eth<-function(v){
  A<-moral.table(v)
  eth<-row.names(A)
  
  mv <- colSums(A)/6.0
  A0 <- matrix( 0, nrow=6, ncol=10)
  for (r in 1:6){ A0[r,]<-mv }
  S <- A - A0
  explained.var<-rep(0,6)
  for (s in 1:6){
    explained.var[s]<-sum(S[s,]^2)/sum(A[s,]^2)
  }
  data.frame(eth=eth,explained=explained.var)
}

mean.var.eth<-function(){
  a <- matrix(0,nrow=6,ncol=1)
  eth<-NULL
  for (r in 1:19){
    b<-variation.eth(mvars[r])
    a<-a+b$explained
    eth<-b$eth
  }
  a<-a/19.0
  data.frame(eth=eth, explained=a*100)
}


moral.comp<-function( vars, data1, data2 ){
  N<-length(vars)
  out<-data.frame()
  for (r in 1:N){
    v1<-as.vector(moral.table.data( vars[r],data1))
    v2<-as.vector(moral.table.data( vars[r],data2))
    # match size
    if (length(v1) != length(v2) ){
      next
    }
    dv <- v1-v2
    e <- sum(dv^2)/(sum(v2^2))
    print(paste(vars[r],e))
    out<-rbind(out, c(vars[r],e) )
  }
  names(out)<-c("var", "explained")
  out
}

stack.plot<-function( A, title, position="topleft", t=NULL){
  if (is.null(t)){
    t<-1:dim(A)[2]
  }
  par(mar=c(3.5,2,2,3))
  q<-plot(t, A[1,], type='l',lwd=3,ylim=c(0,1.0), col=1+1, main=title)
  n<-dim(A)[1]
  for ( j in 2:n){
    lines(t,A[j,], lwd=3, col=j+1)
  }
  legend(position,legend=row.names(A),col=seq(2,1+n),lwd=3,
         cex=0.5)
  q
}

pref.eth<-function(v,data){
  A<-rownrm(table(data[,c("eth",v)]))
  print(dim(A))
  eth<-row.names(A)
  
  mv <- colSums(A)/6.0
  A0 <- matrix( 0, nrow=6, ncol=dim(A)[2])
  for (r in 1:6){ A0[r,]<-mv }
  S <- A - A0
  explained.var<-rep(0,6)
  for (s in 1:6){
    explained.var[s]<-sum(S[s,]^2)/sum(A[s,]^2)
  }
  data.frame(eth=eth,explained=explained.var*100)
}

