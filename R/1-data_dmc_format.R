#extra data formatting required for DMC
load("img/okdats_manifest.RData")
okdats$PM<-as.character(okdats$PM)
okdats$PM[okdats$PM=="NWC"]<- "Nc"
okdats$PM[okdats$PM=="WC"]<- "Wc"
okdats$PM<-factor(okdats$PM)
okdats$S<-as.character(okdats$S)
okdats$S[okdats$S=="Word"]<-"ww"
okdats$S[okdats$S=="Nonword"]<-"nn"
okdats$S[okdats$S=="PMN"]<-"pn"
okdats$S[okdats$S=="PMW"]<-"pw"
okdats$S<-factor(okdats$S)
okdats$R<-as.character(okdats$R)
okdats$R[okdats$R=="Word"]<-"W"
okdats$R[okdats$R=="Nonword"]<-"N"
okdats$R[okdats$R=="PM"]<-"P"
okdats$R<-factor(okdats$R)
okdats<- okdats[,c(1,6,2,3,7,8)]
names(okdats)[3]<- "E"
names(okdats)[4]<- "D"
okdats$D<-as.character(okdats$D)
okdats$D[okdats$D=="1"]<-"one"
okdats$D[okdats$D=="2"]<-"two"
okdats$D<-factor(okdats$D)
okdats$E <- factor(as.character(okdats$E), labels= c("I", "U"))
okdats$S <- factor(okdats$S, levels=c("nn", "ww", "pn", "pw"))

save(okdats, file = "img/okdats.RData")
