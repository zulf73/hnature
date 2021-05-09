# data preparation to understand 
# factors behind ultraviolence 
# support (political violence is justified 9-10)

polv <- na.omit(wvs7[,c("Q290","Q46", "Q57","Q275","Q192")])
polv <- na.omit(wvs7[,c("Q290","Q46", "Q57","Q275","Q192")])
extreme<-polv[which(polv$Q192 == 9 | polv$Q192 == 10),]
extreme$eth<-ethnicity_map(extreme$Q290)
extreme<-extreme[,c("eth","Q192","Q46","Q57","Q275")]

# Financial satisfaction Q50
nrm<-function(v) v/sum(v)