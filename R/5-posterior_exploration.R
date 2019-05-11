source("R/0-functions.R")
source("dmc/dmc.R")
source("dmc/dmc_extras.R")
load_model ("LBA","lbaN_B.R")
load("samples/samples_top.RData")
theme_set(theme_simple())
load("~/OTbias_PM/img/pp_top.RData")

get.pc <- function(df){
  out<- cbind(df,(df$mean/df$data) *100)
  names(out)[length(out)] <- "pc"
  out
}

###

# get.diff.OT.Rtype <- function(df) {
# 
#   NcW <- length(df$RT[df$E=="I" & df$R=="W" & (df$S=="ww"|df$S=="nn")])/
#     length(df$RT[df$E=="I"& (df$S=="ww"|df$S=="nn")])
#   
#   WcW <- length(df$RT[df$E=="U" & df$R=="W"& (df$S=="ww"|df$S=="nn")])/
#   length(df$RT[df$E=="U"& (df$S=="ww"|df$S=="nn")])
#   
#   NcN <- length(df$RT[df$E=="I" & df$R=="N"& (df$S=="ww"|df$S=="nn")])/
#   length(df$RT[df$E=="I"& (df$S=="ww"|df$S=="nn")])
#   
#   WcN <- length(df$RT[df$E=="U" & df$R=="N"& (df$S=="ww"|df$S=="nn")])/
#   length(df$RT[df$E=="U"& (df$S=="ww"|df$S=="nn")])
# 
#   out <- c(NcW-WcW, NcN-WcN)
#   names(out) <- c("NcW-WcW", "NcN-WcN")
#   out
# }
# 
# get.effects.dmc(PP, get.diff.OT.Rtype)


av.posts <- c("B.*.one.N"  ,  "B.*.two.N"  ,  "B.*.one.W"   ,
  "B.*.two.W")

PPs_avthres <- avps.h.post.predict.dmc(samples_top, save.simulation=T, av.posts=
                            av.posts)
# 
# get.effects.dmc(PPs_avthres, get.diff.OT.Rtype)


rates <- colnames(samples_top[[1]]$theta)[grep("mean_v", colnames(samples_top[[1]]$theta))]
OTrates <- rates[!grepl("P", rates)]
Urates <- OTrates[grepl("U", OTrates)]
avrates <- gsub("U", "*", Urates)

PPs_avrates <- avps.h.post.predict.dmc(samples_top, save.simulation=T, av.posts=
                          avrates)

# 
# get.effects.dmc(PPs_avrates, get.diff.OT.Rtype)



PPs_avthresrates <- avps.h.post.predict.dmc(samples_top, save.simulation=T, av.posts=
                            c(av.posts, avrates))
# 
# get.effects.dmc(PPs_avthresrates, get.diff.OT.Rtype)


get.diff.PM.Rtype <- function(df) {

  NcW <- length(df$RT[df$E=="I" & df$R=="W" & (df$S=="pw"|df$S=="pn")])/
    length(df$RT[df$E=="I"& (df$S=="pw"|df$S=="pn")])
  
  WcW <- length(df$RT[df$E=="U" & df$R=="W"& (df$S=="pw"|df$S=="pn")])/
  length(df$RT[df$E=="U"& (df$S=="pw"|df$S=="pn")])
  
  NcN <- length(df$RT[df$E=="I" & df$R=="N"& (df$S=="pw"|df$S=="pn")])/
  length(df$RT[df$E=="I"& (df$S=="pw"|df$S=="pn")])
  
  WcN <- length(df$RT[df$E=="U" & df$R=="N"& (df$S=="pw"|df$S=="pn")])/
  length(df$RT[df$E=="U"& (df$S=="pw"|df$S=="pn")])

  out <- c(NcW-WcW+WcN-NcN)
  names(out) <- c("NcW-WcW+WcN-NcN")
  out
}

full_shift <- get.effects.dmc(PP, get.diff.PM.Rtype)
full_shift$model <- "Full Model"

avthres_shift <- get.effects.dmc(PPs_avthres, get.diff.PM.Rtype)
avthres_shift$model <- "Avg Thresholds"

avrates_shift <- get.effects.dmc(PPs_avrates, get.diff.PM.Rtype)
avrates_shift$model <- "Avg Rates"

avthresrates_shift <- get.effects.dmc(PPs_avthresrates, get.diff.PM.Rtype)
avthresrates_shift$model <- "Avg Thresholds and Rates"

all_effects <- rbind(full_shift, avthres_shift, avrates_shift, avthresrates_shift)

all_effects$model <- factor(all_effects$model, levels=c("Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates"))
get.pc(all_effects)


PM_bias <- ggplot(all_effects, aes(y=mean, x=model))+ geom_point(size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) + geom_hline(aes(yintercept=data), linetype=2)+
  ylab("Nc non-word decrease + Wc word decrease\n (PM trials)") +xlab("Model")+ylim(-0.04,0.18)




get.diff.OT.Rtype <- function(df) {


  NcW <- length(df$RT[df$E=="I" & df$R=="W" & (df$S=="ww"|df$S=="nn")])/
    length(df$RT[df$E=="I"& (df$S=="ww"|df$S=="nn")])
  
  WcW <- length(df$RT[df$E=="U" & df$R=="W"& (df$S=="ww"|df$S=="nn")])/
  length(df$RT[df$E=="U"& (df$S=="ww"|df$S=="nn")])
  
  NcN <- length(df$RT[df$E=="I" & df$R=="N"& (df$S=="ww"|df$S=="nn")])/
  length(df$RT[df$E=="I"& (df$S=="ww"|df$S=="nn")])
  
  WcN <- length(df$RT[df$E=="U" & df$R=="N"& (df$S=="ww"|df$S=="nn")])/
  length(df$RT[df$E=="U"& (df$S=="ww"|df$S=="nn")])
  
  out <- c(NcW-WcW+WcN-NcN)
  names(out) <- c("NcW-WcW+WcN-NcN")
  out
}

full_shift <- get.effects.dmc(PP, get.diff.OT.Rtype)
full_shift$model <- "Full Model"

avthres_shift <- get.effects.dmc(PPs_avthres, get.diff.OT.Rtype)
avthres_shift$model <- "Avg Thresholds"

avrates_shift <- get.effects.dmc(PPs_avrates, get.diff.OT.Rtype)
avrates_shift$model <- "Avg Rates"

avthresrates_shift <- get.effects.dmc(PPs_avthresrates, get.diff.OT.Rtype)
avthresrates_shift$model <- "Avg Thresholds and Rates"

all_effects <- rbind(full_shift, avthres_shift, avrates_shift, avthresrates_shift)

all_effects$model <- factor(all_effects$model, levels=c("Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates"))

get.pc(all_effects)


OT_bias <- ggplot(all_effects, aes(y=mean, x=model))+ geom_point(size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) + geom_hline(aes(yintercept=data), linetype=2)+
  ylab("Nc non-word decrease + Wc word decrease\n (non-PM trials)") +xlab("")+ylim(-0.04,0.18)


grid.arrange(OT_bias, PM_bias)







get.diff.PM.RT <- function(df) {

  NcW <- mean(df$RT[df$E=="I" & df$R=="W" & (df$S=="pw"|df$S=="pn")])
  WcW <- mean(df$RT[df$E=="U" & df$R=="W"& (df$S=="pw"|df$S=="pn")])
  
  NcN <- mean(df$RT[df$E=="I" & df$R=="N"& (df$S=="pw"|df$S=="pn")])
  WcN <- mean(df$RT[df$E=="U" & df$R=="N"& (df$S=="pw"|df$S=="pn")])

  out <- c((WcW-NcW) + (NcN-WcN))
  names(out) <- c("(WcW-NcW) + (NcN-WcN)")
  out
}

full_shift <- get.effects.dmc(PP, get.diff.PM.RT)
full_shift$model <- "Full Model"

avthres_shift <- get.effects.dmc(PPs_avthres, get.diff.PM.RT)
avthres_shift$model <- "Avg Thresholds"

avrates_shift <- get.effects.dmc(PPs_avrates, get.diff.PM.RT)
avrates_shift$model <- "Avg Rates"

avthresrates_shift <- get.effects.dmc(PPs_avthresrates, get.diff.PM.RT)
avthresrates_shift$model <- "Avg Thresholds and Rates"

all_effects <- rbind(full_shift, avthres_shift, avrates_shift, avthresrates_shift)

all_effects$model <- factor(all_effects$model, levels=c("Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates"))
get.pc(all_effects)


PM_bias <- ggplot(all_effects, aes(y=mean, x=model))+ geom_point(size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) + geom_hline(aes(yintercept=data), linetype=2)+
  ylab("Nc non-word RT increase + Wc word RT increase\n (PM trials)") +xlab("Model")




get.diff.OT.RT <- function(df) {

  NcW <- mean(df$RT[df$E=="I" & df$R=="W" & (df$S=="ww"|df$S=="nn")])
  WcW <- mean(df$RT[df$E=="U" & df$R=="W"& (df$S=="ww"|df$S=="nn")])
  
  NcN <- mean(df$RT[df$E=="I" & df$R=="N"& (df$S=="ww"|df$S=="nn")])
  WcN <- mean(df$RT[df$E=="U" & df$R=="N"& (df$S=="ww"|df$S=="nn")])

  out <- c((WcW-NcW) + (NcN-WcN))
  names(out) <- c("(WcW-NcW) + (NcN-WcN)")
  out
}

full_shift <- get.effects.dmc(PP, get.diff.OT.RT)
full_shift$model <- "Full Model"

avthres_shift <- get.effects.dmc(PPs_avthres, get.diff.OT.RT)
avthres_shift$model <- "Avg Thresholds"

avrates_shift <- get.effects.dmc(PPs_avrates, get.diff.OT.RT)
avrates_shift$model <- "Avg Rates"

avthresrates_shift <- get.effects.dmc(PPs_avthresrates, get.diff.OT.RT)
avthresrates_shift$model <- "Avg Thresholds and Rates"

all_effects <- rbind(full_shift, avthres_shift, avrates_shift, avthresrates_shift)

all_effects$model <- factor(all_effects$model, levels=c("Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates"))

get.pc(all_effects)

OT_bias <- ggplot(all_effects, aes(y=mean, x=model))+ geom_point(size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) + geom_hline(aes(yintercept=data), linetype=2)+
  ylab("Nc non-word RT increase + Wc word RT increase\n (nonPM trials)") +xlab("Model")


grid.arrange(OT_bias, )


OT_bias+ylim(-0.08,0.2)
PM_bias+ylim(-0.08,0.2)



get.pc(all_effects)



get.diff.OT.RT <- function(df) {


  NcW <- length(df$RT[df$E=="I" & df$R=="W" & (df$S=="ww"|df$S=="nn")])/
    length(df$RT[df$E=="I"& (df$S=="ww"|df$S=="nn")])
  
  WcW <- length(df$RT[df$E=="U" & df$R=="W"& (df$S=="ww"|df$S=="nn")])/
  length(df$RT[df$E=="U"& (df$S=="ww"|df$S=="nn")])
  
  NcN <- length(df$RT[df$E=="I" & df$R=="N"& (df$S=="ww"|df$S=="nn")])/
  length(df$RT[df$E=="I"& (df$S=="ww"|df$S=="nn")])
  
  WcN <- length(df$RT[df$E=="U" & df$R=="N"& (df$S=="ww"|df$S=="nn")])/
  length(df$RT[df$E=="U"& (df$S=="ww"|df$S=="nn")])
  
  out <- c(NcW-WcW+WcN-NcN)
  names(out) <- c("NcW-WcW+WcN-NcN")
  out
}

full_shift <- get.effects.dmc(PP, get.diff.OT.RT)
full_shift$model <- "Full Model"

avthres_shift <- get.effects.dmc(PPs_avthres, get.diff.OT.RT)
avthres_shift$model <- "Avg Thresholds"

avrates_shift <- get.effects.dmc(PPs_avrates, get.diff.OT.RT)
avrates_shift$model <- "Avg Rates"

avthresrates_shift <- get.effects.dmc(PPs_avthresrates, get.diff.OT.RT)
avthresrates_shift$model <- "Avg Thresholds and Rates"

all_effects <- rbind(full_shift, avthres_shift, avrates_shift, avthresrates_shift)

all_effects$model <- factor(all_effects$model, levels=c("Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates"))


OT_bias <- ggplot(all_effects, aes(y=mean, x=model))+ geom_point(size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) + geom_hline(aes(yintercept=data), linetype=2)+
  ylab("Nc non-word decrease + Wc word decrease\n (dashed line = data)") +xlab("")+ylim(-0.04,0.18)


grid.arrange(OT_bias, PM_bias)










# 
# 
# 
# ot_rates <- colnames(samples_top[[1]]$theta)[
#   grep("ww|nn", colnames(samples_top[[1]]$theta))]
# 
# o1 <- order(paste(substr(ot_rates,8,8), substr(ot_rates,10,12)))
# 
# 
# pmot_rates <- colnames(samples_top[[1]]$theta)[
#   grepl("(pw|pn)", colnames(samples_top[[1]]$theta)) & ! grepl(
#     "P",colnames(samples_top[[1]]$theta)
#   )]
# 
# o2 <- order(paste(substr(pmot_rates,8,8), substr(pmot_rates,10,12)))
# 
# cbind(ot_rates[o1], pmot_rates[o2])
# 
# 
# PP_noreac <- pickps.h.post.predict.dmc(samples_top, save.simulation=T,
#                                        pickps_set = ot_rates[o1],
#                                        pickps_other=pmot_rates[o2])
# 
# effects <- get.effects.dmc(PP, fun= get.diff.normalized.ldC)
# effects$S <- c("Word", "Non-word")
# effects$model <- "full"
# 
# noreac_effects <- get.effects.dmc(PP_noreac, fun= get.diff.normalized.ldC)
# noreac_effects$S <- c("Word", "Non-word")
# noreac_effects$model <- "no reactive"
# 
# all_effects <- rbind(effects, noreac_effects)
# 
# 
# effects <- get.effects.dmc(PP[!names(PP) %in% not_very_biased], fun= get.diff.normalized.ldC)
# effects$S <- c("Word", "Non-word")
# effects$model <- "full"
# 
# noreac_effects <- get.effects.dmc(PP_noreac[!names(PP_noreac) %in% not_very_biased], fun= get.diff.normalized.ldC)
# noreac_effects$S <- c("Word", "Non-word")
# noreac_effects$model <- "no reactive"
# 
# all_effects <- rbind(effects, noreac_effects)
# 
# 
# 
# Ottrial_effects <- get.effects.dmc(PP, fun= get.diff.OT.normalized.ldC)
# Ottrial_effects$S <- c("Word", "Non-word")
# Ottrial_effects$model <- "OT trial"
# Ottrial_effects$mean <- NA
# Ottrial_effects$lower <- NA
# Ottrial_effects$upper <- NA
# 
# all_effects <- rbind(effects, noreac_effects, Ottrial_effects )
# all_effects[7:8,] <- all_effects[5:6,]
# 
# all_effects$model[5:6] <- "no reactive"
# all_effects$model[7:8] <- "full"
# 
# all_effects$isPM <- "PM trial"
# all_effects$isPM[5:8] <- "nonPM trial"
# 
# 
# 
# #just examine ongoing task thresholds
# ggplot(all_effects, aes(factor(S),mean)) + 
#   geom_point(stat = "identity",aes(col=model), size=3) +
#   geom_errorbar(aes(ymax = upper, ymin = lower, width = 0.2, col = model)) +
#   geom_point(aes(y= data, col=isPM), pch=21, size=4, colour="black")+
#   xlab("Emphasis") + ylab("Bias Effect")
# 
# 
# 
# 
# 
# effects <- get.effects.dmc(PP[names(PP) %in% definitely_biased], fun= get.diff.PM.OT.normalized.ldC)
# effects$S <- c("Word", "Non-word")
# effects$model <- "full"
# 
# noreac_effects <- get.effects.dmc(PP_noreac[names(PP_noreac) %in% definitely_biased], fun= get.diff.PM.OT.normalized.ldC)
# noreac_effects$S <- c("Word", "Non-word")
# noreac_effects$model <- "no reactive"
# 
# all_effects <- rbind(effects, noreac_effects)
# 
# ggplot(all_effects, aes(factor(S),mean)) + 
#   geom_point(stat = "identity",aes(col=model), size=3) +
#   geom_errorbar(aes(ymax = upper, ymin = lower, width = 0.2, col = model)) +
#   geom_point(aes(y= data, col=isPM), pch=21, size=4, colour="black")+
#   xlab("Emphasis") + ylab("Bias Magnification on PM trials") +  geom_hline(yintercept=0, linetype=2)
# 
# 
# 
# av.posts <- c("B.*.one.N"  ,  "B.*.two.N"  ,  "B.*.one.W"   ,
#   "B.*.two.W")
# 
# PPs_avthres <- avps.h.post.predict.dmc(samples_top, save.simulation=T, av.posts=
#                             av.posts)
# 
# 
# 
# rates <- colnames(samples_top[[1]]$theta)[grep("p", colnames(samples_top[[1]]$theta))]
# OTrates <- rates[!grepl("P", rates)]
# Irates <- OTrates[!grepl("I", OTrates)]
# avrates <- gsub("U", "*", Irates)
# 
# 
# PPs_avthres <- avps.h.post.predict.dmc(samples_top, save.simulation=T, av.posts=
#                             c(av.posts, avrates))
# 
# 
# #  + 
# 
# #load a model where no c onstant sdvF
# library("abind")
# special_samples <- samples_top
# 
# sdvf <- array(0.01, dim=c(168,1,180))
# dimnames(sdvf) <- list(NULL, "sd_v.f", NULL)
#   
# for (i in 1:length(special_samples)){
#   
#   special_samples[[i]]$theta <- abind(
#     special_samples[[i]]$theta, sdvf, along=2
#   )
#   
# }
# 
# PP_nosv1 <- pickps.h.post.predict.dmc(special_samples, save.simulation=T,
#                                        pickps_set = "sd_v.t",
#                                        pickps_other="sd_v.f",
#                                       special_model = model_top)
# 
# PP_nosv1 <- pickps.h.post.predict.dmc(special_samples, save.simulation=T,
#                                        pickps_set = c(),
#                                        pickps_other=c(),
#                                       special_model = model_top)
# 
# 
# 
# 
# 
# mapmeanv <-
#   empty.map(list(
#     S = c("nn", "ww", "pn", "pw"),
#     E = c("I", "U"),
#     D = c("one", "two"),
#     R = c("N", "W", "P")
#   ), 
#   levels=c(
#    "InnN1","IwwN1","IpnN1","InnW1",
#    "IwwW1","IpwN1","UpwN1","UpnW1",
#    "IpnW1","IpwW1","IpnP1","IpwP1", 
#    "UnnN1","UwwN1","UpnN1", "UnnW1",
#    "UwwW1","UpwW1","UpnP1","UpwP1",
#    "InnN2","IwwN2","IpnN2","InnW2",
#    "IwwW2","IpwN2","UpwN2","UpnW2",
#    "IpnW2","IpwW2","IpnP2","IpwP2",
#    "UnnN2","UwwN2","UpnN2", "UnnW2",
#    "UwwW2", "UpwW2","UpnP2","UpwP2",
#    "fa"
#    )
#   )
# 
# mapmeanv[1:48] <- c(
#  "InnN1","IwwN1","IpnN1","IpwN1",
#  "UnnN1","UwwN1","UpnN1","UpwN1",
#  "InnN2","IwwN2","IpnN2","UpwN2",
#  "UnnN2","UwwN2","UpnN2","IpwN2",
# 
#  "InnW1","IwwW1","IpnW1","IpwW1",
#  "UnnW1","UwwW1","UpnW1","UpwW1",
#  "InnW2","IwwW2","IpnW2","IpwW2",
#  "UnnW2","UwwW2","UpnW2","UpwW2",
# 
#  "fa","fa","IpnP1","IpwP1",
#  "fa","fa","UpnP1","UpwP1",
#  "fa","fa","IpnP2","IpwP2",
#  "fa","fa","UpnP2","UpwP2"
# )
# 
# mapsdv <-
#   empty.map(list(
#     S = c("nn", "ww", "pn", "pw"),
#     E = c("I", "U"),
#     D = c("one", "two"),
#     R = c("N", "W", "P")
#   ),
#   levels = c("t", "f"))
# 
# mapsdv[1:48] <- c(
#   rep(c("t","f","f","f"), 4), 
#   rep(c("f","t","f","f"), 4), 
#   rep(c("f","f","t","t"), 4)
# )
# 
# #Specify model. Note that I am using the flexible lba (lbaN_B) which allows
# #specifying different numbers of accumulators because it uses a faster 
# #way to calculate likelihoods. Hence setting N=3 as a constant.
# 
# model_top <- model.dmc(
#   factors = list(
#     S = c("nn", "ww", "pn", "pw"),
#     E = c("I", "U"),
#     D = c("one", "two")
#   ), responses = c("N", "W", "P"),
#   p.map = list(
#     A = "1", B = c("E", "D", "R"),
#     t0 = c("1"), mean_v = c("MAPMV"),
#     sd_v = c("MAPSDV"),st0 = "1",N = "1"
#   ),
#   match.map = list(
#     M = list( ww = "W", nn = "N", 
#               pn = "P", pw = "P"
#     ),
#     MAPMV = mapmeanv,
#     MAPSDV = mapsdv
#   ),
#   constants = c( st0 = 0, N = 3)
# )
# 
