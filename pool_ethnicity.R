# Create ethnicity table 
# by mapping various detailed
# country-based ethnicities to
# broader groups
library(haven)
# Problem is WVS Q290 has too many 
# gradations when we want to deal 
# with a few classes that can allow 
# us to overcome ethnic prejudices
ethnicities<-unique(as.character(as_factor(wvs7$Q290)))

reduced_ethnicities<-function(){
  mapeth<-rep("Other",length(ethnicities))
  mapeth[c(1,7,15,33,38,43,65,107,154,210,214)]<-"White"
  mapeth[c(3,223,222,51,52,53,54,55,56,45,39,36,11)]<-"Black"
  mapeth[c(6,13,19,20,21,22,67,70)]<-"Indian"
  mapeth[c(4,17,48,97,98,99,100)]<-"Arab"
  mapeth[c(5,14,16,62,63,64,68,72,73,74,75,76,77,
           78,79,80,81,82,83)]<-"East Asian"
  data.frame(key=ethnicities,val=mapeth)
}

ethnicity_map<-function( v ){
  emap<-reduced_ethnicities()
  w<-sapply(as.character(as_factor(v)),function(x) { out<-emap[which(emap$key==x),]$val; if(is.null(out)){out<-"Other"};return(out)})
  #print(head(w))
  #w<-append(w,"Other")
  as.factor(unlist(w))
}


# Analysis Commands for Justifiability of
# Political Violence
#> plot(log(terror.race[1,]+1e-6),type='l',lwd=3,col=2,main="Log Justifiability of Political Violence by Ethnicity")
#> for(k in 2:6){lines(log(terror.race[k,]+1e-6),lwd=3,col=k+1)}
#> legend("topright",legend=row.names(terror.race), col = 2:7, lwd=3,cex=0.5)
