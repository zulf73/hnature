# total partners
p18tot<-pdata$h2rr2a
p18tot<-p18tot + (pdata$h2rr2a == 1 && pdata$h2rr2b==1)
p18tot<-p18tot + (pdata$h2rr2b == 1 && pdata$h2rr2c==1)
p18tot<-p18tot + (pdata$h2rr2c == 1 && pdata$h2rr2d==1)
p18tot[p18tot>4]<-0

phappy <- pdata$h2fs11
phappy[ phappy>3]<-0


tf <- table( p18tot, phappy)
ptf <- prop.table(tf)
