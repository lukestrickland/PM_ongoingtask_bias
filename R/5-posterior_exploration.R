source("R/0-functions.R")
source("dmc/dmc.R")
source("dmc/dmc_extras.R")
load_model ("LBA","lbaN_B.R")
load("samples/samples_top.RData")
theme_set(theme_simple())



ot_rates <- colnames(samples_top[[1]]$theta)[
  grep("ww|nn", colnames(samples_top[[1]]$theta))]

o1 <- order(paste(substr(ot_rates,8,8), substr(ot_rates,10,12)))


pmot_rates <- colnames(samples_top[[1]]$theta)[
  grepl("(pw|pn)", colnames(samples_top[[1]]$theta)) & ! grepl(
    "P",colnames(samples_top[[1]]$theta)
  )]

o2 <- order(paste(substr(pmot_rates,8,8), substr(pmot_rates,10,12)))

cbind(ot_rates[o1], pmot_rates[o2])


PP_noreac <- pickps.h.post.predict.dmc(samples_top, save.simulation=T,
                                       pickps_set = ot_rates[o1],
                                       pickps_other=pmot_rates[o2])

effects <- get.effects.dmc(PP, fun= get.diff.normalized.ldC)
effects$S <- c("Word Trial", "Non-word Trial")
effects$model <- "full"

noreac_effects <- get.effects.dmc(PP_noreac, fun= get.diff.normalized.ldC)
noreac_effects$S <- c("Word Trial", "Non-word Trial")
noreac_effects$model <- "no reactive"

all_effects <- rbind(effects, noreac_effects)


effects <- get.effects.dmc(PP[!names(PP) %in% not_very_biased], fun= get.diff.normalized.ldC)
effects$S <- c("Word Trial", "Non-word Trial")
effects$model <- "full"

noreac_effects <- get.effects.dmc(PP_noreac[!names(PP_noreac) %in% not_very_biased], fun= get.diff.normalized.ldC)
noreac_effects$S <- c("Word Trial", "Non-word Trial")
noreac_effects$model <- "no reactive"

all_effects <- rbind(effects, noreac_effects)



Ottrial_effects <- get.effects.dmc(PP, fun= get.diff.OT.normalized.ldC)
Ottrial_effects$S <- c("Word Trial", "Non-word Trial")
Ottrial_effects$model <- "OT trial"
Ottrial_effects$mean <- NA
Ottrial_effects$lower <- NA
Ottrial_effects$upper <- NA

all_effects <- rbind(effects, noreac_effects, Ottrial_effects )
all_effects[7:8,] <- all_effects[5:6,]

all_effects$model[5:6] <- "no reactive"
all_effects$model[7:8] <- "full"

all_effects$isPM <- "PM trial"
all_effects$isPM[5:8] <- "nonPM trial"



#just examine ongoing task thresholds
ggplot(all_effects, aes(factor(S),mean)) + 
  geom_point(stat = "identity",aes(col=model), size=3) +
  geom_errorbar(aes(ymax = upper, ymin = lower, width = 0.2, col = model)) +
  geom_point(aes(y= data, col=isPM), pch=21, size=4, colour="black")+
  xlab("Emphasis") + ylab("Improvement to ldt with relevant caution instructions") +
  geom_line(aes(y=data, group=interaction(isPM, model)), linetype=2) + facet_grid(.~model)

