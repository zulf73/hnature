# Exercises in ggplot
# for graphical displays
library(ggplot2)

curves.df<-function( var ){
D<-moral.table( var)
z<-colMeans(D)
D<-rbind(D,z)
row.names(D)[7]<-"avg"

D.df<-data.frame()
for (r in 1:nrow(D)){
  for (s in 1:ncol(D)){
    e<-row.names(D)[r]
    L<-as.numeric(colnames(D)[s])
    v<-as.numeric(D[r,s])
    D.df<-rbind(D.df, c(e,L,v))
  }
}
names(D.df)<-c("eth","lev","val")
D.df$lev<-as.numeric(D.df$lev)
D.df$val<-as.numeric(D.df$val)
D.df
}

eth.variance<-function( var, title, pngfile ){
  ddf<-curves.df(var)
  ddf.stats<-ddf %>% group_by(lev) %>% summarise_at(vars('val'),funs(mean,sd))
  CairoPNG(filename=pngfile)
  ggplot(data=ddf.stats, aes(x=lev,y=mean)) + 
    geom_ribbon(aes(x=lev,min=mean-sd,max=mean+sd),
                alpha=0.2,fill='blue') +
    geom_line(lwd=2) + ggtitle(title)
  dev.off()
}
