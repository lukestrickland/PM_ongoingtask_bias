source("dmc/dmc.R")
source("dmc/dmc_extras.R")
source ("R/0-functions.R")
load_model ("LBA","lbaN_B.R")
load("samples/samples_top.RData")

msds <- get.msds(samples_top[definitely_biased])
msds <- label_msds(msds)

Bs <- msds[grep("B", rownames(msds)),]

ggplot(Bs, aes(factor(E),M)) + 
  geom_point(stat = "identity",aes(shape=E), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2)) +
  xlab("Emphasis") + ylab("Threshold") +
  geom_line(aes(group=day, y=M), linetype=2) +
  facet_grid(. ~ R)

#just examine ongoing task thresholds
ggplot(Bs[Bs$R!="PM",], aes(factor(E),M)) + 
  geom_point(stat = "identity",aes(shape=day), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2)) +
  xlab("Emphasis") + ylab("Threshold") +
  geom_line(aes(group=day, y=M), linetype=2)+
  facet_grid(. ~ R) 

mvs <- msds[!is.na(msds$S),]

ggplot(mvs, aes(factor(S),M)) + 
  geom_point(aes(shape=E)) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2))+
  xlab("Accumulator") + ylab("Accumulation Rate") + 
  geom_line(aes(group=E, y=M), linetype=2) +
  facet_grid(day~R,scales = "free", space = "free")

ggplot(mvs[mvs$R!="PM",], aes(E,M)) + 
  geom_point(aes(shape=day, col= isPM), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2, col=isPM))+
  ylab("Accumulation Rate") +
  geom_line(aes(group=interaction(day, isPM), col=isPM, y=M), linetype=2) +
  facet_grid(ot_match~ot_correct, scales="free")

ggplot(mvs[mvs$R!="PM" &mvs$isPM=="PM",], aes(E,M)) + 
  geom_point(aes(shape=day, col= isPM), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2, col=isPM))+
  ylab("Accumulation Rate") +
  geom_line(aes(group=interaction(day, isPM), col=isPM, y=M), linetype=2) +
  facet_grid(ot_match~ot_correct, scales="free")

ggplot(mvs[mvs$R!="PM" &mvs$isPM=="nonPM",], aes(E,M)) + 
  geom_point(aes(shape=day, col= isPM), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2, col=isPM))+
  ylab("Accumulation Rate") +
  geom_line(aes(group=interaction(day, isPM), col=isPM, y=M), linetype=2) +
  facet_grid(ot_match~ot_correct, scales="free")