codes <- read.delim("data/codes")
fns <- list.files("data")
fns <- fns[!fns=="codes"]
sns <- as.numeric(gsub(".txt", "", fns))

key_combos <- list(
  c(Nonword= "d", Word= "s", PM= "j"), 
  c(Nonword= "s", Word= "d", PM= "j"), 
  c(Nonword= "j", Word= "k", PM= "d"),
  c(Nonword= "k", Word= "j", PM= "d")
)

for (i in 1:length(fns)){
  #clean up data frame, nicer names
  #add trials, subject N
  dat <- read.delim(paste("data/", fns[i], sep=""), stringsAsFactors = FALSE)
  #remove any spare columns
  dat<- dat[,1:8]
  dat<- cbind(sns[i], dat[,!colnames(dat) %in% "s"])
  dat$trial <- rep(1:660,times=4)
  names(dat) <- c( "s", "PM","day", "item",
           "C","S","R","RT", "ispmcue", "trial")
  dat$ispmcue[dat$ispmcue=="j"|dat$ispmcue=="d"] <- "PM"
  
  #Get correct keybalance basesd on codes
  keybalance <- key_combos[[codes[codes$subject==sns[i],2]]]
  #Save off responses where participants presses non-response key
  datwrongkey <- dat[!dat$R %in% keybalance,]
  if (i==1) datswrongkey <- as.data.frame(datwrongkey) else datswrongkey <- 
                                                rbind(datswrongkey, datwrongkey)
  
  #Assign keys
  dat$S <- factor(dat$S,levels= keybalance[1:2],
                            labels= names(keybalance)[1:2])
  
  dat$PM <- factor(dat$PM,levels= keybalance[1:2],
                            labels= names(keybalance)[1:2]) 
  
  dat$PM <- factor(dat$PM, levels=c("Nonword", "Word"),
                       labels=c("NWC", "WC"))

  dat$R <- factor(dat$R,levels=keybalance, labels=names(keybalance)) 
  
  #Find trials following LD errors (to discouraged response),
  #PM stimuli and PM responses
  baderr <-   (dat$R == "Word" &
     dat$S == "Nonword" &
     dat$PM == "WC") |
  (dat$R == "Nonword" & dat$S == "Word" & dat$PM == "NWC")
  badS <- dat$ispmcue=="PM"
  badR <- dat$R=="PM"
  #protect badR and baderr against NAs (for responses not in key lineup)
  baderr[is.na(baderr)] <- FALSE
  badR[is.na(badR)] <- FALSE
  
  #Look 1 ahead for discouraged LD errors and 2 ahead for PM errors
  #to find trials to cut
  #Note that I cut the first two trials of each quarter and 
  #block anyway, so there is no issue cutting at the end of quarters
  
  baderrs <- c(FALSE, head(baderr, -1))
  badSs <- c(FALSE, head(badS, -1))|c(FALSE,FALSE, head(badS, -2))
  badRs <- c(FALSE, head(badR, -1))|c(FALSE,FALSE, head(badR, -2))
  dat$trialcut <- baderrs|badSs|badRs
  
  #cut said trials and also any
  #trials where participants pressed an impossible response key
  dat <- dat[!dat$trialcut,]
  dat <- dat[!is.na(dat$R),]
  if (i==1)  dats <- dat  else dats <- rbind(dats,dat)
}

#Remove trials after breaks
okdats <-
  dats[!(dats$trial %in% 
           c(1, 2, 111, 112, 221, 222, 331, 332, 441, 442, 551, 552)), ]

okdats$RT <- okdats$RT/1000
okdats$C[okdats$ispmcue=="PM" & okdats$R!="PM"]<- F
okdats$day<-as.factor(okdats$day)
okdats$S <- as.character(okdats$S)
okdats$S[okdats$ispmcue=="PM"&okdats$S=="Word"]<- "PMW"
okdats$S[okdats$ispmcue=="PM"&okdats$S=="Nonword"]<- "PMN"
okdats$S<- factor(okdats$S)
okdats$s<-factor(okdats$s)

okdats <- clean(okdats)
okdats <- okdats[,!colnames(okdats) %in% "trialcut"]
##Saving file for subsequent computational modelling and "manifest analysis"
save(okdats,file="img/okdats_manifest.RData")
