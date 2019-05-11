source("R/functions.R")
library(dplyr)
load("img/okdats_manifest.RData")

#LDT accuracy analysis
#GLM probit model of LDT response accuracy
ldacc_model <-
  glmer(C ~ S * PM * day + (1 |
                        s),
        data = okdats[!okdats$ispmcue=="PM" &
                        okdats$R != "PM", ],
        family = binomial(link = "probit"))

Anova(ldacc_model, type = "II")

#get participant avg accuracies
participant_ldt_accs <-
  okdats %>% group_by(s, S, PM, day) %>% filter(R != "PM" & !ispmcue=="PM") %>% 
  summarise(y = mean(C))

#get mean of mean accs
ldt_accs <-
  participant_ldt_accs %>% group_by(S, PM) %>% summarise(mean_meanacc = mean(meanacc))

ldt_accs %>% mutate (mean_meanacc=mean_meanacc*100)

#get within-subjects standard errors
se2(participant_ldt_accs, facs=c("S", "PM")) *100

#get mean and se for day factor
participant_ldt_accs %>% group_by(day) %>% summarise(mean_meanacc = mean(y))
se2(participant_ldt_accs, facs=c("day")) *100

#post-hoc comparisons 
nw_accs <- participant_ldt_accs %>% filter(S == "Nonword") %>% group_by(s, PM) %>% 
  summarise(y = mean(y))

t.test (nw_accs$y[nw_accs$PM=="NWC"], nw_accs$y[nw_accs$PM=="WC"], paired=T)

w_accs <- participant_ldt_accs %>% filter(S == "Word") %>% group_by(s, PM) %>% 
  summarise(y = mean(y))

t.test (w_accs$y[w_accs$PM=="NWC"], w_accs$y[w_accs$PM=="WC"], paired=T)

d_accs <- participant_ldt_accs %>% group_by(s, day) %>% 
  summarise(y = mean(y))

t.test (d_accs$y[d_accs$day=="1"], d_accs$y[d_accs$day=="2"], paired=T)


#LDT RT analysis
participant_ldt_cRTs <-
  okdats %>% group_by(s, S, PM, day) %>% 
  filter(R != "PM" & !ispmcue=="PM" & as.character(S)==as.character(R)) %>% 
  summarise(y = mean(RT)) 

ldRT_model <-
  lmer(meanRT ~ S * PM * day + (1 | s),
        data = participant_ldt_cRTs)

Anova(ldRT_model, type = "II")

ldt_cRTs <-
  participant_ldt_cRTs %>% group_by(S, PM) %>% summarise(mean_meancRT = mean(y))

ldt_cRTs

se2(participant_ldt_cRTs, facs=c("S", "PM"))

participant_ldt_cRTs %>% group_by(day) %>% summarise(mean_meancRT = mean(y))
se2(participant_ldt_cRTs, facs="day")

nw_cRTs <- participant_ldt_cRTs %>% filter(S == "Nonword") %>% group_by(s, PM) %>% 
  summarise(y = mean(y))

t.test (nw_cRTs$y[nw_cRTs$PM=="NWC"], nw_cRTs$y[nw_cRTs$PM=="WC"], paired=T)

w_cRTs <- participant_ldt_cRTs %>% filter(S == "Word") %>% group_by(s, PM) %>% 
  summarise(y = mean(y))

t.test (w_cRTs$y[w_cRTs$PM=="NWC"], w_cRTs$y[w_cRTs$PM=="WC"], paired=T)

d_cRTs <- participant_ldt_cRTs %>% group_by(s, day) %>% 
  summarise(y = mean(y))
t.test (d_cRTs$y[d_cRTs$day=="1"], d_cRTs$y[d_cRTs$day=="2"], paired=T)

###PM accuracy and correct RT
#get participant avg accuracies and ses for plots

PMacc_model <-
  glmer(R=="PM" ~ S * PM * day + (1 |
                        s),
        data = okdats[okdats$ispmcue=="PM",],
        family = binomial(link = "probit"))

Anova(PMacc_model, type = "II")

participant_PM_accs <-
  okdats %>% group_by(s, S, PM, day) %>% filter(isPMcue=="PM") %>% 
  summarise(meanacc = mean(R=="PM")) 

PM_accs <-
  participant_PM_accs %>% group_by(S, PM, day) %>% summarise(mean_meanacc = mean(meanacc))

participant_PM_accs %>% group_by(S, PM) %>% summarise(mean_meanacc = mean(meanacc))

se2(participant_PM_accs, facs=c("S", "PM"), dvnam="meanacc")  

participant_PM_cRTs <-
  okdats %>% group_by(s, S, PM, day) %>% 
  filter(R == "PM" & isPM) %>% 
  summarise(meanRT = mean(RT)) %>% group_by(s) %>% 
  mutate(M = length(meanRT))

PM_cRTs <-
  participant_PM_cRTs %>% group_by(S, PM, day) %>% summarise(mean_meanRT = mean(meanRT))

PMRT_model <-
  lmer(meanRT ~ S * PM * day + (1 | s),
        data = participant_PM_cRTs)

Anova(PMRT_model, type = "II")

PM_cRTs_se <-
  participant_PM_cRTs %>% group_by(S, PM, day) %>% summarise(se_meanRT = se2(meanRT, M[1]))

#PM error type analysis
PM <- okdats[okdats$ispmcue=="PM",]; PM$S = factor(as.character(PM$S))

PMwe <- arr2df(tapply(PM$R=="Word",
                      list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
PMnwe <- arr2df(tapply(PM$R=="Nonword",
                       list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
PMwe$type <- "w"
PMnwe$type <- "n"

errors <-rbind(PMwe, PMnwe)
errors <- errors[,c(1,6,2,3,4,5)]

errors %>% group_by(type, PM) %>% summarise(C=mean(y))

merr.lmer <- lmer(y ~ S*PM*type*day+(1|s), data=errors)
Anova(merr.lmer)

PMweRT <- arr2df(tapply(PM$RT[PM$R=="Word"],
                        list(s=PM$s[PM$R=="Word"],PM=PM$PM[PM$R=="Word"], 
                             day = PM$day[PM$R=="Word"], S= PM$S[PM$R=="Word"]), mean))

PMnweRT <- arr2df(tapply(PM$RT[PM$R=="Nonword"],list(s=PM$s[PM$R=="Nonword"],
                                                     PM=PM$PM[PM$R=="Nonword"], day = 
                                                       PM$day[PM$R=="Nonword"], 
                                                     S= PM$S[PM$R=="Nonword"]), mean))

errorRTs <- rbind(PMweRT, PMnweRT)
errorRTs$type<- c(rep("werror", length(PMweRT$y)), 
                  rep("nwerror", length(PMnweRT$y)))

mErt.lmer <- lmer(y ~ S*PM*type*day+(1|s), data=errorRTs)
Anova(mErt.lmer,type="II")

mneffects(errorRTs, list("S","type", c("PM", "S"),
                         c("S", "type"), c("PM", "type"),
                         c("PM", "type", "S"), digits=2))

e_wRTs <- errorRTs %>% filter(type == "werror") %>% group_by(s, PM) %>% 
  summarise(y = mean(y, na.rm=T))

e_wRTs %>% group_by(PM) %>% summarise(mean(y, na.rm=T))

t.test(e_wRTs$y[e_wRTs$PM=="WC"], e_wRTs$y[e_wRTs$PM=="NWC"], paired=T)

e_wRTs <- errorRTs %>% filter(type == "nwerror") %>% group_by(s, PM) %>% 
  summarise(y = mean(y, na.rm=T))

e_wRTs %>% group_by(PM) %>% summarise(mean(y, na.rm=T))

t.test(e_wRTs$y[e_wRTs$PM=="WC"], e_wRTs$y[e_wRTs$PM=="NWC"], paired=T)



