german.morals<- wvs7[as.character(as_factor(wvs7$B_COUNTRY))=="Germany",c("B_COUNTRY", vars)]
nongerman.morals<-wvs7[as.character(as_factor(wvs7$B_COUNTRY))!="Germany",c("B_COUNTRY", vars)]