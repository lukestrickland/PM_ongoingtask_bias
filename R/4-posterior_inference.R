source("dmc/dmc.R")
source("dmc/dmc_extras.R")
source ("R/functions.R")
load_model ("LBA","lbaN_B.R")

load("samples/samples_top.RData")
#Reviewer suggestion: try vary t0 by day.
# load("samples/samples_top_t0.RData")
samples_top <- samples_top_t0

msds <- get.msds(samples_top)
msds <- label_msds(msds)

Bs <- msds[grep("B", rownames(msds)),]

ggplot(Bs, aes(factor(E),M)) + 
  geom_point(stat = "identity",aes(shape=E), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2)) +
  xlab("Emphasis") + ylab("Threshold") +
  geom_line(aes(group=day, y=M), linetype=2) +
  facet_grid(. ~ R)

#just examine ongoing task thresholds
Bs_OT<- ggplot(Bs[Bs$R!="PM",], aes(factor(E),M)) + 
  geom_point(stat = "identity",aes(shape=day), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2)) +
  xlab("Bias Condition") + ylab("Threshold") +
  geom_line(aes(group=day, y=M), linetype=2)+
  facet_grid(. ~ R)  + theme(legend.position = "none")

Bs_PM <- ggplot(Bs[Bs$R=="PM",], aes(factor(E),M)) + 
  geom_point(stat = "identity",aes(shape=day), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2)) +
  xlab("") + ylab("") +
  geom_line(aes(group=day, y=M), linetype=2) +
  facet_grid(. ~ R)+ scale_shape_discrete(name="Day")

#these 2 are equivalent

N_shift_B <- function(thetas) (thetas[,"B.I.one.N",, drop=F] + thetas[,"B.I.two.N",, drop=F])/2 - 
                              (thetas[,"B.U.one.N",, drop=F] + thetas[,"B.U.two.N",, drop=F])/2


N_shift_B <- function(thetas) ((thetas[,"B.I.one.N",, drop=F] - thetas[,"B.U.one.N",, drop=F]) +
                              (thetas[,"B.I.two.N",, drop=F] - thetas[,"B.U.two.N",, drop=F]) )/2


zandp(samples_top, N_shift_B )


W_shift_B <- function(thetas) (thetas[,"B.U.one.W",, drop=F] + thetas[,"B.U.two.W",, drop=F])/2 -
                              (thetas[,"B.I.one.W",, drop=F] + thetas[,"B.I.two.W",, drop=F])/2 

W_shift_B <- function(thetas) ((thetas[,"B.U.one.W",, drop=F] - thetas[,"B.I.one.W",, drop=F]) +
                              (thetas[,"B.U.two.W",, drop=F] - thetas[,"B.I.two.W",, drop=F]) )/2
  
zandp(samples_top, W_shift_B )  
                              

P_shift_B <- function(thetas) (thetas[,"B.I.one.P",, drop=F] + thetas[,"B.I.two.P",, drop=F])/2 -
                              (thetas[,"B.U.one.P",, drop=F] + thetas[,"B.U.two.P",, drop=F])/2 


P_shift_B <- function(thetas) ((thetas[,"B.I.one.P",, drop=F] - thetas[,"B.U.one.P",, drop=F]) +
                              (thetas[,"B.I.two.P",, drop=F] - thetas[,"B.U.two.P",, drop=F]) )/2
  
zandp(samples_top, P_shift_B)  
                              

grid.arrange(Bs_OT, Bs_PM, layout_matrix=matrix(nrow=1, ncol=3, data=c(1,1,2)))

#reviewer suggestion: word vs non-word thresholds

overall_w_bias <- function(thetas) (thetas[,"B.U.one.N",, drop=F] + thetas[,"B.U.two.N",, drop=F] +
                                 thetas[,"B.I.one.N",, drop=F] + thetas[,"B.I.two.N",, drop=F])/4 - 
                              (thetas[,"B.U.one.W",, drop=F] + thetas[,"B.U.two.W",, drop=F] +
                                 thetas[,"B.I.one.W",, drop=F] + thetas[,"B.I.two.W",, drop=F])/4

zandp(samples_top, overall_w_bias)  



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

colnames(mvs)[4] <- "Day"

ggplot(mvs[mvs$R!="PM" &mvs$isPM=="PM",], aes(E,M)) + 
  geom_point(aes(shape=Day), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2))+
  ylab("Accumulation Rate") + xlab("Bias Condition") +
  geom_line(aes(group=interaction(Day, isPM), y=M), linetype=2) +
  facet_grid(ot_match~ot_correct, scales="free")

#this one
ggplot(mvs[mvs$R!="PM" &mvs$isPM=="nonPM",], aes(E,M)) + 
  geom_point(aes(shape=Day), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2))+
  ylab("Accumulation Rate") +
  geom_line(aes(group=interaction(Day, isPM), y=M), linetype=2) +
  facet_grid(ot_match~ot_correct, scales="free") +xlab("Bias Condition")

mvs2 <- mvs
mvs2$ot_match[mvs2$R=="PM"] <- "PM"
ggplot(mvs2[mvs2$isPM=="PM",], aes(E,M)) + 
  geom_point(aes(shape=Day), size=3) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.2))+
  ylab("Accumulation Rate") + xlab("Bias Condition") +
  geom_line(aes(group=interaction(Day, isPM), y=M), linetype=2) +
  facet_grid(ot_match~ot_correct, scales="free")



###Check differential reactive control across match/mismatch accumulators


w_bias_shift <- function(thetas) ((thetas[,"mean_v.IwwW1",, drop=F] - thetas[,"mean_v.UwwW1",, drop=F]) +
                                 (thetas[,"mean_v.IwwW2",, drop=F] - thetas[,"mean_v.UwwW2",, drop=F]))/2

zandp(samples_top, w_bias_shift)

we_bias_shift <- function(thetas) ((thetas[,"mean_v.InnW1",, drop=F] - thetas[,"mean_v.UnnW1",, drop=F]) +
                                 (thetas[,"mean_v.InnW2",, drop=F] - thetas[,"mean_v.UnnW2",, drop=F]))/2

zandp(samples_top, we_bias_shift)



n_bias_shift <- function(thetas) ((thetas[,"mean_v.UnnN1",, drop=F] - thetas[,"mean_v.InnN1",, drop=F]) +
                                 (thetas[,"mean_v.UnnN2",, drop=F] - thetas[,"mean_v.InnN2",, drop=F]))/2

zandp(samples_top, n_bias_shift)



ne_bias_shift <- function(thetas) ((thetas[,"mean_v.UwwN1",, drop=F] - thetas[,"mean_v.IwwN1",, drop=F]) +
                                 (thetas[,"mean_v.UwwN2",, drop=F] - thetas[,"mean_v.IwwN2",, drop=F]))/2

zandp(samples_top, ne_bias_shift)




PM_pn <- function(thetas) ((thetas[,"mean_v.IpnP1",, drop=F] - thetas[,"mean_v.UpnP1",, drop=F]) +
                               (thetas[,"mean_v.IpnP2",, drop=F] - thetas[,"mean_v.UpnP2",, drop=F]))/2


PM_pw <- function(thetas) ((thetas[,"mean_v.IpwP1",, drop=F] - thetas[,"mean_v.UpwP1",, drop=F]) +
                               (thetas[,"mean_v.IpwP2",, drop=F] - thetas[,"mean_v.UpwP2",, drop=F]))/2


zandp(samples_top, PM_pn)
zandp(samples_top, PM_pw)


PMw_d <- function(thetas)  ((thetas[,"mean_v.IpwP2",, drop=F]  - thetas[,"mean_v.IpwP1",, drop=F])+
                            (thetas[,"mean_v.UpwP2",, drop=F]  - thetas[,"mean_v.UpwP1",, drop=F]))/2   
                            
zandp(samples_top, PMw_d)

PMn_d <- function(thetas)  ((thetas[,"mean_v.IpnP2",, drop=F]  - thetas[,"mean_v.IpnP1",, drop=F])+
                            (thetas[,"mean_v.UpnP2",, drop=F]  - thetas[,"mean_v.UpnP1",, drop=F]))/2   
                            
zandp(samples_top, PMn_d)



Ireac_Match_w <- function(thetas) ((thetas[,"mean_v.IwwW1",, drop=F] - thetas[,"mean_v.IpwW1",, drop=F]) +
                                 (thetas[,"mean_v.IwwW2",, drop=F] - thetas[,"mean_v.IpwW2",, drop=F]))/2

Ireac_Match_n <- function(thetas) ((thetas[,"mean_v.InnN1",, drop=F] - thetas[,"mean_v.IpnN1",, drop=F]) +
                                 (thetas[,"mean_v.InnN2",, drop=F] - thetas[,"mean_v.IpnN2",, drop=F]))/2

Ireac_misMatch_w <- function(thetas) ((thetas[,"mean_v.IwwN1",, drop=F] - thetas[,"mean_v.IpwN1",, drop=F]) +
                                 (thetas[,"mean_v.IwwN2",, drop=F] - thetas[,"mean_v.IpwN2",, drop=F]))/2

Ireac_misMatch_n <- function(thetas) ((thetas[,"mean_v.InnW1",, drop=F] - thetas[,"mean_v.IpnW1",, drop=F]) +
                                 (thetas[,"mean_v.InnW2",, drop=F] - thetas[,"mean_v.IpnW2",, drop=F]))/2

zandp(samples_top, Ireac_Match_w)
zandp(samples_top, Ireac_Match_n)

zandp(samples_top, Ireac_misMatch_w)
zandp(samples_top, Ireac_misMatch_n)


Ureac_Match_w <- function(thetas) ((thetas[,"mean_v.UwwW1",, drop=F] - thetas[,"mean_v.UpwW1",, drop=F]) +
                                 (thetas[,"mean_v.UwwW2",, drop=F] - thetas[,"mean_v.UpwW2",, drop=F]))/2

Ureac_Match_n <- function(thetas) ((thetas[,"mean_v.UnnN1",, drop=F] - thetas[,"mean_v.UpnN1",, drop=F]) +
                                 (thetas[,"mean_v.UnnN2",, drop=F] - thetas[,"mean_v.UpnN2",, drop=F]))/2

Ureac_misMatch_w <- function(thetas) ((thetas[,"mean_v.UwwN1",, drop=F] - thetas[,"mean_v.UpwN1",, drop=F]) +
                                 (thetas[,"mean_v.UwwN2",, drop=F] - thetas[,"mean_v.UpwN2",, drop=F]))/2

Ureac_misMatch_n <- function(thetas) ((thetas[,"mean_v.UnnW1",, drop=F] - thetas[,"mean_v.UpnW1",, drop=F]) +
                                 (thetas[,"mean_v.UnnW2",, drop=F] - thetas[,"mean_v.UpnW2",, drop=F]))/2

zandp(samples_top, Ureac_Match_w)
zandp(samples_top, Ureac_Match_n)

zandp(samples_top, Ureac_misMatch_w)
zandp(samples_top, Ureac_misMatch_n)





pw_bias_shift <- function(thetas) ((thetas[,"mean_v.UpwW1",, drop=F] - thetas[,"mean_v.IpwW1",, drop=F]) +
                                 (thetas[,"mean_v.UpwW2",, drop=F] - thetas[,"mean_v.IpwW2",, drop=F]))/2

zandp(samples_top, pw_bias_shift)

pwe_bias_shift <- function(thetas) ((thetas[,"mean_v.UpnW1",, drop=F] - thetas[,"mean_v.IpnW1",, drop=F]) +
                                 (thetas[,"mean_v.UpnW2",, drop=F] - thetas[,"mean_v.IpnW2",, drop=F]))/2

zandp(samples_top, pwe_bias_shift)


pn_bias_shift <- function(thetas) ((thetas[,"mean_v.IpnN1",, drop=F] - thetas[,"mean_v.UpnN1",, drop=F]) +
                                 (thetas[,"mean_v.IpnN2",, drop=F] - thetas[,"mean_v.UpnN2",, drop=F]))/2

zandp(samples_top, pn_bias_shift)



pne_bias_shift <- function(thetas) ((thetas[,"mean_v.UpwN1",, drop=F] - thetas[,"mean_v.IpwN1",, drop=F]) +
                                 (thetas[,"mean_v.UpwN2",, drop=F] - thetas[,"mean_v.IpwN2",, drop=F]))/2

zandp(samples_top, pne_bias_shift)



#How much is a shift of 0.1 compared with actual shift

get_thetashiftNI12 <- function(thetas) ((thetas[,"B.I.one.N",, drop=F] - 
                                      thetas[,"B.U.one.N",, drop=F] ) +
                                   (thetas[,"B.I.two.N",, drop=F] - 
                                      thetas[,"B.U.two.N",, drop=F] )) /2

thetashiftNI12 <- group.inference.dist(samples_top, get_thetashiftNI12)

mean(thetashiftNI12 )

get_thetashiftWU12 <- function(thetas) ((thetas[,"B.U.one.W",, drop=F] - 
                                      thetas[,"B.I.one.W",, drop=F] ) +
                                   (thetas[,"B.U.two.W",, drop=F] - 
                                      thetas[,"B.I.two.W",, drop=F] )) /2

thetashiftWU12 <- group.inference.dist(samples_top, get_thetashiftWU12)

mean(thetashiftWU12)





