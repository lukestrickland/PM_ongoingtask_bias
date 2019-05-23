source("R/functions.R")
source("dmc/dmc.R")
source("dmc/dmc_extras.R")
load_model ("LBA","lbaN_B.R")
load("samples/samples_top.RData")
theme_set(theme_simple())
load("~/OTbias_PM/img/PP.RData")

####Simulate posterior predictives with null effects on some parameters
# av.posts <- c("B.*.one.N"  ,  "B.*.two.N"  ,  "B.*.one.W"   ,
#   "B.*.two.W")
# 
# PPs_avthres <- avps.h.post.predict.dmc(samples_top, save.simulation=T, av.posts=
#                                                                         av.posts,
#                                                                        n.post=200)
# 
# save(PPs_avthres, file="img/PPs_avthres.RData")

load("img/PPs_avthres.RData")

# rates <- colnames(samples_top[[1]]$theta)[grep("mean_v", 
#                                                colnames(samples_top[[1]]$theta))]
# OTrates <- rates[!grepl("P", rates)]
# Urates <- OTrates[grepl("U", OTrates)]
# avrates <- gsub("U", "*", Urates)
# 
# PPs_avrates <- avps.h.post.predict.dmc(samples_top, save.simulation=T, av.posts=
#                                                                         avrates,
#                                        n.post=200)
# 
# save(PPs_avrates, file= "img/PPs_avrates.RData")

load("img/PPs_avrates.RData")

# PPs_avthresrates <- avps.h.post.predict.dmc(samples_top, save.simulation=T, av.posts=
#                                                              c(av.posts, avrates),
#                                             n.post=200)
# 
# save(PPs_avthresrates, file= "img/PPs_avthresrates.RData")

load("img/PPs_avthresrates.RData")
# 
# #Make special samples object with basically no rate variability
# samples_nosv <- samples_top
# 
# sdvf <- array(0.00001, dim=c(168,1,180))
# dimnames(sdvf) <- list(NULL, "sd_v.f", NULL)
# 
# for (i in 1:length(samples_nosv)){
# 
#   samples_nosv[[i]]$theta <- abind(
#     samples_nosv[[i]]$theta, sdvf, along=2
#   )
#   
#   samples_nosv[[i]]$theta[,"sd_v.t",] <- 0.00001
# 
# }
# 
# #have to create a "special" model where sdv_f is not fixed in order
# #to simulate this
# source("R/5.5-specify_model_top_noscalingparameter.R")
# 
# PP_nosv1 <- pickps.h.post.predict.dmc(samples_nosv, save.simulation=T,
#                                        pickps_set = c(),
#                                        pickps_other=c(),
#                                       special_model = model_top,
#                                       n.post=200)
# 
# save(PP_nosv1, file= "img/PP_nosv1.RData")

load("img/PP_nosv1.RData")

# 
# #Make special samples with imposing big bias boost
# samples_bigBshift <- samples_top
# 
# for (i in 1:length(samples_bigBshift)){
#   
#   samples_bigBshift[[i]]$theta[,"B.I.one.N",] <- 
#     samples_bigBshift[[i]]$theta[,"B.I.one.N",] + 0.1
#   samples_bigBshift[[i]]$theta[,"B.I.two.N",] <- 
#     samples_bigBshift[[i]]$theta[,"B.I.two.N",] + 0.1
#   
#   samples_bigBshift[[i]]$theta[,"B.U.one.W",] <- 
#     samples_bigBshift[[i]]$theta[,"B.U.one.W",] + 0.1
#   samples_bigBshift[[i]]$theta[,"B.U.two.W",] <- 
#     samples_bigBshift[[i]]$theta[,"B.U.two.W",] + 0.1
# 
# }
# 
# PP_bigBshift <- h.post.predict.dmc(samples_bigBshift,
#                      save.simulation = TRUE,
#                      cores = length(samples_bigBshift),
#                      n.post=200)
# 
# save(PP_bigBshift, file= "img/PP_bigBshift.RData")

load("img/PP_bigBshift.RData")


# Examine relevant posterior predicted quantities

full_shift_Rtype_PM <- get.effects.dmc(PP, get.diff.PM.Rtype)
full_shift_Rtype_PM$model <- "Full Model"

avthres_shift_Rtype_PM <- get.effects.dmc(PPs_avthres, get.diff.PM.Rtype)
avthres_shift_Rtype_PM$model <- "Avg Thresholds"

avrates_shift_Rtype_PM <- get.effects.dmc(PPs_avrates, get.diff.PM.Rtype)
avrates_shift_Rtype_PM$model <- "Avg Rates"

avthresrates_shift_Rtype_PM <- get.effects.dmc(PPs_avthresrates, get.diff.PM.Rtype)
avthresrates_shift_Rtype_PM$model <- "Avg Thresholds and Rates"

all_effects_Rtype_PM <- rbind(full_shift_Rtype_PM, 
                              avthres_shift_Rtype_PM, 
                              avrates_shift_Rtype_PM, 
                              avthresrates_shift_Rtype_PM)

all_effects_Rtype_PM$model <- factor(all_effects_Rtype_PM$model, levels=c(
                                                        "Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates"
                                                        )
                                     )

get.pc.effect.predicted(all_effects_Rtype_PM)

PM_bias_Rtype <- ggplot(all_effects_Rtype_PM, aes(y=mean, x=model))+ 
  geom_point(size=3) + geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_hline(aes(yintercept=data), linetype=2)+ ylab(
    "Nc non-word decrease + Wc word decrease\n (PM trials)") +
  xlab("Model")+ ylim(-0.04,0.18)


full_shift_Rtype_nonPM <- get.effects.dmc(PP, get.diff.OT.Rtype)
full_shift_Rtype_nonPM$model <- "Full Model"

avthres_shift_Rtype_nonPM <- get.effects.dmc(PPs_avthres, get.diff.OT.Rtype)
avthres_shift_Rtype_nonPM$model <- "Avg Thresholds"

avrates_shift_Rtype_nonPM <- get.effects.dmc(PPs_avrates, get.diff.OT.Rtype)
avrates_shift_Rtype_nonPM$model <- "Avg Rates"

avthresrates_shift_Rtype_nonPM <- get.effects.dmc(PPs_avthresrates, get.diff.OT.Rtype)
avthresrates_shift_Rtype_nonPM$model <- "Avg Thresholds and Rates"

all_effects_Rtype_nonPM <- rbind(full_shift_Rtype_nonPM, 
                                  avthres_shift_Rtype_nonPM, 
                                  avrates_shift_Rtype_nonPM, 
                                  avthresrates_shift_Rtype_nonPM)

get.pc.effect.predicted(all_effects_Rtype_nonPM)

all_effects_Rtype_nonPM$model <- factor(all_effects_Rtype_nonPM$model, levels=c(
                                                        "Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates"
                                                        )
                                        )

OT_bias_Rtype <- ggplot(all_effects_Rtype_nonPM, aes(y=mean, x=model))+ geom_point(size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) + 
  geom_hline(aes(yintercept=data), linetype=2)+ ylab(
    "Nc non-word decrease + Wc word decrease\n (non-PM trials)") +
  xlab("")+ylim(-0.04,0.18)

grid.arrange(OT_bias_Rtype, PM_bias_Rtype)



full_shift_RT_PM <- get.effects.dmc(PP, get.diff.PM.RT)
full_shift_RT_PM$model <- "Full Model"

avthres_shift_RT_PM <- get.effects.dmc(PPs_avthres, get.diff.PM.RT)
avthres_shift_RT_PM$model <- "Avg Thresholds"

avrates_shift_RT_PM <- get.effects.dmc(PPs_avrates, get.diff.PM.RT)
avrates_shift_RT_PM$model <- "Avg Rates"

avthresrates_shift_RT_PM <- get.effects.dmc(PPs_avthresrates, get.diff.PM.RT)
avthresrates_shift_RT_PM$model <- "Avg Thresholds and Rates"

all_effects_RT_PM <- rbind(full_shift_RT_PM, avthres_shift_RT_PM, avrates_shift_RT_PM, avthresrates_shift_RT_PM)

all_effects_RT_PM$model <- factor(all_effects_RT_PM$model, levels=c("Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates"))
get.pc.effect.predicted(full_shift_RT_PM)


PM_bias_RT <- ggplot(all_effects_RT_PM, aes(y=mean, x=model))+ geom_point(size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) + geom_hline(aes(yintercept=data), linetype=2)+
  ylab("Nc non-word RT increase + Wc word RT increase\n (PM trials)") +xlab("Model")


full_shift_RT_nonPM <- get.effects.dmc(PP, get.diff.OT.RT)
full_shift_RT_nonPM$model <- "Full Model"

avthres_shift_RT_nonPM <- get.effects.dmc(PPs_avthres, get.diff.OT.RT)
avthres_shift_RT_nonPM$model <- "Avg Thresholds"

avrates_shift_RT_nonPM <- get.effects.dmc(PPs_avrates, get.diff.OT.RT)
avrates_shift_RT_nonPM$model <- "Avg Rates"

avthresrates_shift_RT_nonPM <- get.effects.dmc(PPs_avthresrates, get.diff.OT.RT)
avthresrates_shift_RT_nonPM$model <- "Avg Thresholds and Rates"

all_effects_RT_nonPM  <- rbind(full_shift_RT_nonPM, avthres_shift_RT_nonPM, avrates_shift_RT_nonPM, avthresrates_shift_RT_nonPM)

all_effects_RT_nonPM$model <- factor(all_effects_RT_nonPM$model, levels=c("Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates"))

get.pc.effect.predicted(all_effects_RT_nonPM)

OT_bias_RT <- ggplot(all_effects_RT_nonPM, aes(y=mean, x=model))+ geom_point(size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) + geom_hline(aes(yintercept=data), linetype=2)+
  ylab("Nc non-word RT increase + Wc word RT increase\n (nonPM trials)") +xlab("Model")

grid.arrange(PM_bias_RT, OT_bias_RT)



full_shift_PMperf <- get.effects.dmc(PP, get.diff.PM.perf)
full_shift_PMperf$model <- "Full Model"

avthres_shift_PMperf <- get.effects.dmc(PPs_avthres, get.diff.PM.perf)
avthres_shift_PMperf$model <- "Avg Thresholds"

avrates_shift_PMperf <- get.effects.dmc(PPs_avrates, get.diff.PM.perf)
avrates_shift_PMperf$model <- "Avg Rates"

avthresrates_shift_PMperf <- get.effects.dmc(PPs_avthresrates, get.diff.PM.perf)
avthresrates_shift_PMperf$model <- "Avg Thresholds and Rates"

nosv1_shift_PMperf <- get.effects.dmc(PP_nosv1, get.diff.PM.perf)
nosv1_shift_PMperf$model <- "No rate variability"

bigB_shift_PMperf <- get.effects.dmc(PP_bigBshift, get.diff.PM.perf)
bigB_shift_PMperf$model <- ">2x shift in bias"


all_effects_PMperf <- rbind(full_shift_PMperf, avthres_shift_PMperf, 
                            avrates_shift_PMperf, avthresrates_shift_PMperf,
                     nosv1_shift_PMperf, bigB_shift_PMperf)

all_effects_PMperf$model <- factor(all_effects_PMperf$model, levels=c("Full Model",
                                                        "Avg Rates",
                                                        "Avg Thresholds",
                                                        "Avg Thresholds and Rates",
                                                        "No rate variability",
                                                        ">2x shift in bias"))

ggplot(all_effects_PMperf, aes(y=mean, x=model))+ 
  geom_hline(aes(yintercept=0), linetype=1,col=2)+ geom_point(size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) + 
  geom_hline(aes(yintercept=data), linetype=2)+
  ylab("Nc non-word PM increase + Wc word PM increase\n") + xlab("Model")


all_effects_PMperf
