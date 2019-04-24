source("R/0-functions.R")
# 
# #reads in data from separate files, also creates global variable datswrongkey
# #with bad key presses (not one of assigned response keys)
# dats <- getdat()
# 
# dats$RT <- dats$RT/1000
# dats$item<- as.character(dats$item)
# dats$ispmcue<- as.character(dats$ispmcue)
# dats$ispmcue[dats$ispmcue=="j"|dats$ispmcue=="d"] <- "PM"
# dats$C[dats$ispmcue=="PM" & dats$R!="PM"]<- F
# dats$day<-as.factor(dats$day)
# names(dats)[2] <- "PM"
# dats$S <- as.character(dats$S)
# dats$S[dats$ispmcue=="PM"&dats$S=="Word"]<- "PMW"
# dats$S[dats$ispmcue=="PM"&dats$S=="Nonword"]<- "PMN"
# dats$S<- factor(dats$S)
# 
# # percentage of key presses not corresponding to a response
# length(datswrongkey$RT)/ (length(datswrongkey$RT) + length(dats$RT)) * 100
# 
# # check for bad subjects
# fast <- tapply(dats$RT<0.200,list(Day=dats$day,PM = dats$PM, s=dats$s),mean)
# slow <- tapply(dats$RT>5,list(Day=dats$da,PM = dats$PM,s=dats$s),mean)
# 
# # cut first two trials of each quarter
# okdats <- dats
# okdats <-
#   okdats[!(okdats$trial %in% c(1, 2, 111, 112, 221, 222, 331, 332, 441, 442, 551, 552)), ] 
# 
# # PM trials and 2 after 
# #Missingtrials checks whether any trials were dropped (wrong key press) before the current trial
# missingtrials <- abs(okdats$trial-1-c(F,okdats$trial[-length(okdats$trial)]))
# 
# #finds trials after a bad response (with long timeout message)
# delaycut <-
#   (okdats$R == "Word" &
#      okdats$S == "Nonword" &
#      okdats$PM == "WC") |
#   (okdats$R == "Nonword" & okdats$S == "Word" & okdats$PM == "NWC")
# 
# cutD = c(FALSE, head(delaycut, -1)) & missingtrials < 1
# 
# okdats <- okdats[!cutD, ]
# 
# #cut the trials 2 after PMs and FAs, but not PMs and FAs themselves
# #Reset missingtrials to also include trials which were cut because they're after an error delay
# 
# badS <- okdats$ispmcue=="PM"
# missingtrials <- abs(okdats$trial-1-c(F,okdats$trial[-length(okdats$trial)]))
# cutS = ifelse( c(FALSE,head(badS,-1)) & missingtrials<2 |
#                  c(FALSE,FALSE,head(badS,-2)) & (missingtrials+c(0,head(missingtrials,-1)) < 1),
#                TRUE,
#                FALSE
# )
# 
# badR <- okdats$R=="PM"; nbadr=length(badR) 
# cutR = ifelse( (c(FALSE,head(badR,-1)) & missingtrials<2 |
#                  c(FALSE,FALSE,head(badR,-2)) & (missingtrials+c(0,head(missingtrials,-1)) < 1)),
#                TRUE,
#                FALSE
# )
# 
# okdats <- okdats[!cutS & ! cutR,]
# okdats$S <- factor(as.character(okdats$S))
# okdats$R <- factor(as.character(okdats$R))
# 
# # IQR 3SD clean
# okdats <- clean(okdats)
# ##Saving file for subsequent computational modelling
# save(okdats,file="img/okdats_manifest.RData")

load("img/okdats_manifest.RData")

#Columns to look at lexical decision correctness and PM/nonPM trial status
#for subsequent modelling.
okdats$ldS <- okdats$S
okdats$ldS[okdats$S=="PMW"] <- "Word"
okdats$ldS[okdats$S=="PMN"] <- "Nonword"
okdats$ldC <- okdats$C
okdats$ldC[okdats$S=="PMW" & okdats$R=="Word"] <- TRUE
okdats$ldC[okdats$S=="PMN" & okdats$R=="Nonword"] <- TRUE
okdats$ldC[okdats$R=="PM"] <- FALSE
okdats$isPM <- grepl("PM", okdats$S)

#Manipulation check (ldt accuracies)
okdats %>% group_by(S, PM, day) %>% filter(R != "PM" & !isPM) %>%  summarise(meanacc = mean(C)) %>% 
  arrange(day)

#get participant avg accuracies and ses for plots

participant_ldt_accs <-
  okdats %>% group_by(s, S, PM, day) %>% filter(R != "PM" & !isPM) %>% 
  summarise(meanacc = mean(C)) %>% group_by(s) %>% 
  mutate(M = length(meanacc))

#possible bad data: p22. 50% non-word accuracy with nw caution
participant_ldt_accs[participant_ldt_accs$meanacc<0.7,]

#Get mean of mean of mean response accuracies for plots
ldt_accs <-
  participant_ldt_accs %>% group_by(S, PM, day) %>% summarise(mean_meanacc = mean(meanacc))

ldt_accs_se <-
  participant_ldt_accs %>% group_by(S, PM, day) %>% summarise(se_meanacc = se2(meanacc, M[1]))

plot.df <- full_join(ldt_accs, ldt_accs_se)
names(plot.df)[names(plot.df) %in% c("mean_meanacc", "se_meanacc")] <-
  c("M", "SE")

theme_set(theme_simple())
ggplot(plot.df, aes(PM, M)) +
  geom_point(stat = "identity",
             aes(col = factor(day)),
             size = 3) + geom_errorbar(aes(
               ymax = M + SE,
               ymin = M - SE, col=factor(day)
             )) +geom_line(aes(y=M, group=day, col=day), linetype=2) + facet_grid(.~ S) + 
              ylab("Accuracy") +ylim(0.7,1)

ldacc_model <-
  glmer(C ~ S * PM * day + (1 |
                        s),
        data = okdats[!okdats$isPM &
                        okdats$R != "PM", ],
        family = binomial(link = "probit"))

Anova(ldacc_model, type = "II")

#response times
participant_ldt_cRTs <-
  okdats %>% group_by(s, S, PM, day) %>% 
  filter(R != "PM" & !isPM & as.character(S)==as.character(R)) %>% 
  summarise(meanRT = mean(RT)) %>% group_by(s) %>% 
  mutate(M = length(meanRT))

#Get mean of mean of mean response accuracies for plots
ldt_cRTs <-
  participant_ldt_cRTs %>% group_by(S, PM, day) %>% summarise(mean_meanRT = mean(meanRT))

ldt_cRTs_se <-
  participant_ldt_cRTs %>% group_by(S, PM, day) %>% summarise(se_meanRT = se2(meanRT, M[1]))

plot.df <- full_join(ldt_cRTs, ldt_cRTs_se)
names(plot.df)[names(plot.df) %in% c("mean_meanRT", "se_meanRT")] <-
  c("M", "SE")

ggplot(plot.df, aes(PM, M)) +
  geom_point(stat = "identity",
             aes(col = factor(day)),
             size = 3) + geom_errorbar(aes(
               ymax = M + SE,
               ymin = M - SE, col=factor(day)
             )) +geom_line(aes(y=M, group=day, col=day), linetype=2) + facet_grid(.~ S) + 
              ylab("RT") 

ldRT_model <-
  lmer(meanRT ~ S * PM * day + (1 | s),
        data = participant_ldt_cRTs)

Anova(ldRT_model, type = "II")

###PM accuracy and correct RT
#get participant avg accuracies and ses for plots

participant_PM_accs <-
  okdats %>% group_by(s, S, PM, day) %>% filter(isPM) %>% 
  summarise(meanacc = mean(R=="PM")) %>% group_by(s) %>% 
  mutate(M = length(meanacc))

#check possible no-PM participants. All have at least one PM response somewhere
#but definitely possible some of these neglected PM goal entirely (e.g. p12 
#all of day2)
lowPM <- participant_PM_accs[participant_PM_accs$meanacc<0.1,]
table(lowPM$s, lowPM$day)

#Get mean of mean of mean response accuracies for plots
PM_accs <-
  participant_PM_accs %>% group_by(S, PM, day) %>% summarise(mean_meanacc = mean(meanacc))

PM_accs_se <-
  participant_PM_accs %>% group_by(S, PM, day) %>% summarise(se_meanacc = se2(meanacc, M[1]))

plot.df <- full_join(PM_accs, PM_accs_se)
names(plot.df)[names(plot.df) %in% c("mean_meanacc", "se_meanacc")] <-
  c("M", "SE")

ggplot(plot.df, aes(PM, M)) +
  geom_point(stat = "identity",
             aes(col = factor(day)),
             size = 3) + geom_errorbar(aes(
               ymax = M + SE,
               ymin = M - SE, col=factor(day)
             )) +geom_line(aes(y=M, group=day, col=day), linetype=2) + facet_grid(.~ S) + 
              ylab("Accuracy") 

PMacc_model <-
  glmer(R=="PM" ~ S * PM * day + (1 |
                        s),
        data = okdats[okdats$isPM,],
        family = binomial(link = "probit"))

Anova(PMacc_model, type = "II")


#response times
participant_PM_cRTs <-
  okdats %>% group_by(s, S, PM, day) %>% 
  filter(R == "PM" & isPM) %>% 
  summarise(meanRT = mean(RT)) %>% group_by(s) %>% 
  mutate(M = length(meanRT))

#Get mean of mean of mean response accuracies for plots
PM_cRTs <-
  participant_PM_cRTs %>% group_by(S, PM, day) %>% summarise(mean_meanRT = mean(meanRT))

PM_cRTs_se <-
  participant_PM_cRTs %>% group_by(S, PM, day) %>% summarise(se_meanRT = se2(meanRT, M[1]))

plot.df <- full_join(PM_cRTs, PM_cRTs_se)
names(plot.df)[names(plot.df) %in% c("mean_meanRT", "se_meanRT")] <-
  c("M", "SE")

ggplot(plot.df, aes(PM, M)) +
  geom_point(stat = "identity",
             aes(col = factor(day)),
             size = 3) + geom_errorbar(aes(
               ymax = M + SE,
               ymin = M - SE, col=factor(day)
             )) +geom_line(aes(y=M, group=day, col=day), linetype=2) + facet_grid(.~ S) + 
              ylab("RT") 

PMRT_model <-
  lmer(meanRT ~ S * PM * day + (1 | s),
        data = participant_PM_cRTs)

Anova(PMRT_model, type = "II")

###

participant_pm_ldt_accs <-
  okdats %>% group_by(s, S, PM, day) %>% filter(R != "PM" & isPM) %>% 
  summarise(meanacc = mean(ldC)) %>% group_by(s) %>% 
  mutate(M = length(meanacc))

#Get mean of mean of mean response accuracies for plots
pm_ldt_accs <-
  participant_pm_ldt_accs %>% group_by(S, PM, day) %>% summarise(mean_meanacc = mean(meanacc))

pm_ldt_accs_se <-
  participant_pm_ldt_accs %>% group_by(S, PM, day) %>% summarise(se_meanacc = se2(meanacc, M[1]))

plot.df <- full_join(pm_ldt_accs, pm_ldt_accs_se)
names(plot.df)[names(plot.df) %in% c("mean_meanacc", "se_meanacc")] <-
  c("M", "SE")

ggplot(plot.df, aes(PM, M)) +
  geom_point(stat = "identity",
             aes(col = factor(day)),
             size = 3) + geom_errorbar(aes(
               ymax = M + SE,
               ymin = M - SE, col=factor(day)
             )) +geom_line(aes(y=M, group=day, col=day), linetype=2) + facet_grid(.~ S) + 
              ylab("Accuracy") +ylim(0.7,1)

pm_ldacc_model <-
  glmer(ldC ~ S * PM * day + (1 |
                        s),
        data = okdats[okdats$isPM &
                        okdats$R != "PM", ],
        family = binomial(link = "probit"))

Anova(pm_ldacc_model, type = "II")


participant_pm_ldt_errs <-
  okdats %>% group_by(s, S, PM, day) %>% filter(isPM) %>% mutate(lderr= 
                                                                   (S=="PMW" & R=="Nonword")|
                                                                   (S=="PMN" & R =="Word")) %>% 
  summarise(meanerr = mean(lderr)) %>% group_by(s) %>% 
  mutate(M = length(meanerr))

#Get mean of mean of mean response erruracies for plots
pm_ldt_errs <-
  participant_pm_ldt_errs %>% group_by(S, PM, day) %>% summarise(mean_meanerr = mean(meanerr))

pm_ldt_errs_se <-
  participant_pm_ldt_errs %>% group_by(S, PM, day) %>% summarise(se_meanerr = se2(meanerr, M[1]))

plot.df <- full_join(pm_ldt_errs, pm_ldt_errs_se)
names(plot.df)[names(plot.df) %in% c("mean_meanerr", "se_meanerr")] <-
  c("M", "SE")

ggplot(plot.df, aes(PM, M)) +
  geom_point(stat = "identity",
             aes(col = factor(day)),
             size = 3) + geom_errorbar(aes(
               ymax = M + SE,
               ymin = M - SE, col=factor(day)
             )) +geom_line(aes(y=M, group=day, col=day), linetype=2) + facet_grid(.~ S) + 
              ylab("lexical decision error responses")

okdats <- okdats %>% mutate(lderr= (S=="PMW" & R=="Nonword")|(S=="PMN" & R =="Word"))

#failed badly to converge
pm_lderr_model <-
  glmer(lderr ~ S * PM * day + (1 |s),
        data = okdats[okdats$isPM,],
        family = binomial(link = "probit"))
Anova(pm_lderr_model, type = "II")

Anova(pm_lderr_model, type = "II")

participant_pm_ldt_errs_aov <- participant_pm_ldt_errs[,-6]

colnames(participant_pm_ldt_errs_aov)[5] <-"y"

wsAnova(as.data.frame(participant_pm_ldt_errs_aov))




#response times
participant_ldt_cRTs <-
  okdats %>% 
  mutate(lderr= (S=="PMW" & R=="Nonword")|(S=="PMN" & R =="Word")) %>% 
  group_by(s, S, PM, day) %>% 
  filter(R != "PM" & isPM) %>% 
  summarise(meanRT = mean(RT)) %>% group_by(s) %>% 
  mutate(M = length(meanRT))

#Get mean of mean of mean response accuracies for plots
ldt_cRTs <-
  participant_ldt_cRTs %>% group_by(S, PM, day) %>% summarise(mean_meanRT = mean(meanRT))

ldt_cRTs_se <-
  participant_ldt_cRTs %>% group_by(S, PM, day) %>% summarise(se_meanRT = se2(meanRT, M[1]))

plot.df <- full_join(ldt_cRTs, ldt_cRTs_se)
names(plot.df)[names(plot.df) %in% c("mean_meanRT", "se_meanRT")] <-
  c("M", "SE")

ggplot(plot.df, aes(PM, M)) +
  geom_point(stat = "identity",
             aes(col = factor(day)),
             size = 3) + geom_errorbar(aes(
               ymax = M + SE,
               ymin = M - SE, col=factor(day)
             )) +geom_line(aes(y=M, group=day, col=day), linetype=2) + facet_grid(.~ S) + 
              ylab("RT") 






#Final analysis - effects of PM trial status on ldt accuracy/bias effect

threshold_not_affected <- c("1"  ,"11" ,"19" ,"2"  ,"24",
                            "26", "29", "18", "23","27",
                            "28", "30", "5",  "7",  "8")

noPM_dats <- okdats %>% filter(R!="PM" & !s %in% threshold_not_affected)

noPM_dats <- okdats %>% filter(R!="PM")


participant_ldC_alltrials  <-
  okdats %>% filter(!s %in% threshold_not_affected) %>% group_by(s, ldS, PM,isPM, day) %>% filter(R!="PM") %>% 
  summarise(meanacc = mean(ldC)) %>% group_by(s) %>% 
  mutate(M = length(meanacc))

#%>% filter(!s %in% threshold_not_affected)

#Get mean of mean of mean response erruracies for plots
ldC_alltrials <-
  participant_ldC_alltrials %>% group_by(ldS, isPM, PM, day) %>% summarise(mean_meanacc = mean(meanacc))

ldC_alltrials_se <-
  participant_ldC_alltrials %>% group_by(ldS, isPM, PM, day) %>% summarise(se_meanacc = se2(meanacc, M[1]))

plot.df <- full_join(ldC_alltrials, ldC_alltrials_se)
names(plot.df)[names(plot.df) %in% c("mean_meanacc", "se_meanacc")] <-
  c("M", "SE")

ggplot(plot.df, aes(PM, M)) +
  geom_point(stat = "identity",
             aes(col = factor(isPM)),
             size = 3) + geom_errorbar(aes(
               ymax = M + SE,
               ymin = M - SE, col=factor(isPM)
             )) +geom_line(aes(y=M, group=isPM, col=isPM), linetype=2) + facet_grid(day~ ldS) + 
              ylab("lexical decision error responses")



ldt.glmer <- glmer(ldC ~ ldS*PM*isPM*day+(1|s), data=noPM_dats,family=binomial(link = "probit"))
Anova(ldt.glmer,type="II")





PMonly <- okdats %>% filter((S=="PMN"|S=="PMW") & s %in% definitely_biased)
pm.glmer <- glmer(C ~ S*PM+(1|s), data=PMonly,family=binomial(link = "probit"))
Anova(pm.glmer,type="II")

PMonly %>% group_by(S, PM) %>% summarise(mean(C))

noPM_dats %>% group_by(isPM, ldS) %>%  summarise(meanacc = mean(ldC))
noPM_dats %>% group_by(isPM, ldS, PM) %>%  summarise(meanacc = mean(ldC)) %>% arrange(isPM, ldS)

###Test for significant effect - PM trials vs non-PM trials, OT LDT




source("LSAnova.R")


okdats



PM <- okdats[okdats$ispmcue=="PM",]; PM$S = factor(as.character(PM$S))

PMwe <- arr2df(tapply(PM$R=="Word",list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
wsAnova(PMwe)
mneffects(PMwe, list("S", c("PM", "S"), digits=2))

PMnwe <- arr2df(tapply(PM$R=="Nonword",list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
wsAnova(PMnwe)
mneffects(PMnwe, list("S", c("PM", "S"), digits=2))


PMwe$type <- "w"
PMnwe$type <- "n"
errors <-rbind(PMwe, PMnwe)
errors <- errors[,c(1,6,2,3,4,5)]
wsAnova(errors)
mneffects(errors, list(c("PM", "S", "type"), c("PM", "type"), c("S", "type"), c("S", "type", "day"), digits=2))




PMweRT <- arr2df(tapply(PM$RT[PM$R=="Word"],list(s=PM$s[PM$R=="Word"],PM=PM$PM[PM$R=="Word"], day = PM$day[PM$R=="Word"], S= PM$S[PM$R=="Word"]), mean))
PMnweRT <- arr2df(tapply(PM$RT[PM$R=="Nonword"],list(s=PM$s[PM$R=="Nonword"],PM=PM$PM[PM$R=="Nonword"], day = PM$day[PM$R=="Nonword"], S= PM$S[PM$R=="Nonword"]), mean))

errorRTs <- rbind(PMweRT, PMnweRT)
errorRTs$type<- c(rep("werror", length(PMweRT$y)), rep("nwerror", length(PMnweRT$y)))

require("lme4")
mErt.lmer <- lmer(y ~ S*PM*type*day+(1|s), data=errorRTs)
Anova(mErt.lmer,type="II")
mneffects(errorRTs, list("S","type", c("PM", "S"), c("S", "type"), c("PM", "type"), c("PM", "type", "S"), digits=2))



PMe <- okdats[okdats$ispmcue=="PM" & !okdats$R=="PM",]; PMe$S = factor(as.character(PMe$S)); PMe$R = factor(as.character(PMe$R))
PMe$y <- PMe$R=="Word"


resp.glmer <- glmer(y ~ S*PM*day+(1|s), data=PMe,family=binomial(link = "probit"))
Anova(resp.glmer,type="II")


PM:type 
type
PM    nwerror werror
NWC   0.179  0.215
WC    0.231  0.178

~ 9% shift




###LDT


ldt.glmer <- glmer(C ~ S*PM*day+(1|s), data=ldt,family=binomial(link = "probit"))
Anova(ldt.glmer,type="II")


ldtCrt.lmer <- lmer(y ~ item*PM*day+(1|s), data=drt)
Anova(ldtCrt.lmer,type="II")

PMnwe <- arr2df(tapply(PM$R=="Nonword",list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
wsAnova(PMnwe)
mneffects(PMnwe, list("S", c("PM", "S"), digits=2))






library(lme4)
library(car)
mcrt.lmer <- lmer(y ~ S*PM*type*day+(1|s), data=errors)
Anova(mcrt.lmer,type="II")




PM arr2df(tapply(PMes$R=="Word",list(s=PMes$s,PM=PMes$PM, day = PMes$day, S= PMes$S), mean))

PMes <- PM[!PM$R=="PM",]; PMes$R = factor(as.character(PMes$R))
PMes$R<-as.logical(factor(PMes$R));
PMetype <- arr2df(tapply(PMes$R=="Word",list(s=PMes$s,PM=PMes$PM, day = PMes$day, S= PMes$S), mean))
wsAnova(PMetype)
mneffects(PMetype, list("S", c("PM", "S"), digits=2))


library(nlme)

Anova(lme)

lme4

glmer

dPMacc <- arr2df(tapply(PM$C,list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

PM:S 
S
PM      PMN   PMW
NWC 0.598 0.614
WC  0.567 0.614

dPMacc <- arr2df(tapply(PM$R=="Word",list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

PM:S 
S
PM      PMN   PMW
NWC 0.063 0.367
WC  0.014 0.343

dPMacc <- arr2df(tapply(PM$R=="Nonword",list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

PM:S 
S
PM      PMN   PMW
NWC 0.339 0.019
WC  0.419 0.043

Word <- okdats[okdats$S=="Word",]
Word$S<- factor(as.character(Word$S))
wordR<- arr2df(tapply(Word$R=="Word",list(s=Word$s,PM=Word$PM, day = Word$day, S= Word$S), mean))
mneffects(wordR,list("day","S", c("PM", "S"), digits=2))

PM:S 
S
PM     Word
NWC 0.931
WC  0.880


nonwordR<- arr2df(tapply(Word$R=="Nonword",list(s=Word$s,PM=Word$PM, day = Word$day, S= Word$S), mean))
mneffects(nonwordR,list("day","S", c("PM", "S"), digits=2))

PM:S 
S
PM     Word
NWC 0.068
WC  0.118

PMR<- arr2df(tapply(Word$R=="PM",list(s=Word$s,PM=Word$PM, day = Word$day, S= Word$S), mean))
mneffects(PMR,list("day","S", c("PM", "S"), digits=2))
PM:S 
S
PM     Word
NWC 0.002
WC  0.002


Nonword <- okdats[okdats$S=="Nonword",]
Nonword$S<- factor(as.character(Nonword$S))
NonwordR<- arr2df(tapply(Nonword$R=="Nonword",list(s=Nonword$s,PM=Nonword$PM, day = Nonword$day, S= Nonword$S), mean))
mneffects(NonwordR,list("day","S", c("PM", "S"), digits=2))

PM:S 
S
PM    Nonword
NWC   0.882
WC    0.961


nonwordR<- arr2df(tapply(Nonword$R=="Word",list(s=Nonword$s,PM=Nonword$PM, day = Nonword$day, S= Nonword$S), mean))
mneffects(nonwordR,list("day","S", c("PM", "S"), digits=2))

PM:S 
S
PM    Nonword
NWC   0.117
WC    0.039


PMR<- arr2df(tapply(Nonword$R=="PM",list(s=Nonword$s,PM=Nonword$PM, day = Nonword$day, S= Nonword$S), mean))
mneffects(PMR,list("day","S", c("PM", "S"), digits=2))

PM:S 
S
PM    Nonword
NWC   0.001
WC    0.001





PM <- okdats[okdats$ispmcue=="PM",]
PM$S<- factor(as.character(PM$S))
PM <- PM[PM$R=="PM",] ; PM$R <- factor(as.character(PM$R))
dPMacc <- arr2df(tapply(PM$RT,list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

PM:S 
S
PM      PMN   PMW
NWC 0.959 0.945
WC  0.922 0.966


PM <- okdats[okdats$ispmcue=="PM",]
PM$S<- factor(as.character(PM$S))
PM <- PM[PM$R=="Word",] ; PM$R <- factor(as.character(PM$R))
dPMacc <- arr2df(tapply(PM$RT,list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

PM:S 
S
PM      PMN   PMW
NWC 0.865 0.767
WC  0.987 0.785

PM <- okdats[okdats$ispmcue=="PM",]
PM$S<- factor(as.character(PM$S))
PM <- PM[PM$R=="Nonword",] ; PM$R <- factor(as.character(PM$R))
dPMacc <- arr2df(tapply(PM$RT,list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

PM:S 
S
PM      PMN   PMW
NWC 0.944 1.102
WC  0.881 0.881




Word <- okdats[okdats$S=="Word",]
Word$S<- factor(as.character(Word$S))
Word <- Word[Word$R=="Word",] ; Word$R <- factor(as.character(Word$R))
dPMacc <- arr2df(tapply(Word$RT,list(s=Word$s,PM=Word$PM, day = Word$day, S= Word$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

Word <- okdats[okdats$S=="Word",];Word$S<- factor(as.character(Word$S))
Word <- Word[Word$R=="Nonword",] ; Word$R <- factor(as.character(Word$R))
dPMacc <- arr2df(tapply(Word$RT,list(s=Word$s,PM=Word$PM, day = Word$day, S= Word$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

Word <- okdats[okdats$S=="Word",];Word$S<- factor(as.character(Word$S))
Word <- Word[Word$R=="PM",] ; Word$R <- factor(as.character(Word$R))
dPMacc <- arr2df(tapply(Word$RT,list(s=Word$s,PM=Word$PM, day = Word$day, S= Word$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

Nonword <- okdats[okdats$S=="Nonword",]
Nonword$S<- factor(as.character(Nonword$S))
Nonword <- Nonword[Nonword$R=="Nonword",] ; Nonword$R <- factor(as.character(Nonword$R))
dPMacc <- arr2df(tapply(Nonword$RT,list(s=Nonword$s,PM=Nonword$PM, day = Nonword$day, S= Nonword$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

Nonword <- okdats[okdats$S=="Nonword",]
Nonword$S<- factor(as.character(Nonword$S))
Nonword <- Nonword[Nonword$R=="Word",] ; Nonword$R <- factor(as.character(Nonword$R))
dPMacc <- arr2df(tapply(Nonword$RT,list(s=Nonword$s,PM=Nonword$PM, day = Nonword$day, S= Nonword$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))

Nonword <- okdats[okdats$S=="Nonword",]
Nonword$S<- factor(as.character(Nonword$S))
Nonword <- Nonword[Nonword$R=="PM",] ; Nonword$R <- factor(as.character(Nonword$R))
dPMacc <- arr2df(tapply(Nonword$RT,list(s=Nonword$s,PM=Nonword$PM, day = Nonword$day, S= Nonword$S), mean))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))


######^^okdats is the data frame for the model




## Traditional anova analyses
#Cut PMs, FAs and 2 after for the LDT frame
ldt <- anovas[!badR & !badS & !cutS &!cutR,]
ldt$S <- factor(as.character(ldt$S))
ldt$R <- factor(as.character(ldt$R))
#Cut 2 after PM FAs for PM and FA analysis
anovas<- anovas[!cutS & !cutR, ]
PMframebeforecut <- length(anovas$RT[anovas$ispmcue=="PM"])
ldtframebeforecut  <- length(ldt$RT)
anovas <- clean(anovas)
ldt <- clean(ldt)
PM <- anovas[anovas$ispmcue=="PM",]
PM$PM<- factor(as.character(PM$PM))
PM$S<- factor(as.character(PM$S))
PM <- PM

# percentage outlying RTS (<0.2 or >3 SDs) excluded
(PMframebeforecut - length (PM$RT)) /PMframebeforecut  * 100
(ldtframebeforecut - length (ldt$RT)) /ldtframebeforecut  * 100

########## Analyses ########################
source("LSAnova.R") 

dacc <- arr2df(tapply(ldt$C,list(s=ldt$s, PM = ldt$PM, day = ldt$day, item=ldt$S),mean))
wsAnova(dacc)

ldt.glmer <- glmer(C ~ S*PM*day+(1|s), data=ldt,family=binomial(link = "probit"))
Anova(ldt.glmer,type="II")

mneffects(dacc,list("day", "PM", "item", c("PM", "item", "day"), c("PM", "item")),digits=5)
se(dacc, facs=c("PM", "item"))

#Comparisons nonword responses
dacc$item <- as.numeric(factor(dacc$item))
bonfnwdacc <- data.frame()
bonfnwdacc <- tapply(dacc$y[dacc$item==1], list(subject= dacc$s[dacc$item==1], PM = factor(dacc$PM[dacc$item==1])), mean)
t.test (bonfnwdacc[,1], bonfnwdacc[,2], paired=T)

#Comparisons word responses
bonfwdacc <- data.frame()
bonfwdacc <- tapply(dacc$y[dacc$item==2], list(subject= dacc$s[dacc$item==2], PM = factor(dacc$PM[dacc$item==2])), mean)
t.test (bonfwdacc[,1], bonfwdacc[,2], paired=T)

# Mean RT analysis
drt <- arr2df(tapply(ldt$RT[ldt$C],list(s=ldt$s[ldt$C], day = ldt$day[ldt$C], item=ldt$S[ldt$C], PM= ldt$PM[ldt$C]),mean))
wsAnova(drt)

ldtCrt.lmer <- lmer(y ~ item*PM*day+(1|s), data=drt)
Anova(ldtCrt.lmer,type="II")



mneffects(drt,list("day", "PM", "item", c("PM", "item"), c("day", "item"),c("PM", "item")),digits=3)
se(drt, facs=c("PM", "item"))

ldtCrt.lmer <- lmer(y ~ item*PM*day+(1|s), data=drt)
Anova(ldtCrt.lmer,type="II")




drt <- arr2df(tapply(ldt$RT[!ldt$C],list(s=ldt$s[!ldt$C], day = ldt$day[!ldt$C], item=ldt$S[!ldt$C], PM= ldt$PM[!ldt$C]),mean))
wsAnova(drt)
mneffects(drt,list("day", "PM", "item", c("PM", "item"), c("day", "item"),c("PM", "item")),digits=3)



#Comparisons nw
bonfnwdrt <- data.frame()
bonfnwdrt <- tapply(drt$y[drt$item=="Nonword"], list(subject= drt$s[drt$item=="Nonword"], PM = factor(drt$PM[drt$item=="Nonword"])), mean)
t.test (bonfnwdrt[,1], bonfnwdrt[,2], paired=T)
# w
bonfwdrt <- data.frame()
bonfwdrt <- tapply(drt$y[drt$item=="Word"], list(subject= drt$s[drt$item=="Word"], PM = factor(drt$PM[drt$item=="Word"])), mean)
t.test (bonfwdrt[,1], bonfwdrt[,2], paired=T)

#graph settings
require("ggplot2")
require("reshape2")
require("plyr")


theme_luke <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      panel.background = element_rect(fill="white"),
      panel.grid.minor.y = element_blank(),
      legend.key = element_rect(fill="white", colour= "white"),
      strip.background = element_rect(fill="white")
      
    )   
}
theme_set(theme_luke())


##graph RT
graphdrt <- list()
sedrt <- list()
drt$item <- as.numeric(factor(drt$item))
for (i in 1:2)
{
  graphdrt[[i]] <- tapply(drt$y[drt$item==i], list(PM = factor(drt$PM)[drt$item==i], day = drt$day[drt$item==i]), mean)
  sedrt[[i]] <- se(drt[drt$item==i,], facs = c("PM", "day"))
}


means.df <- melt(graphdrt, value.name = "mean")
ses.df   <- melt(sedrt,   value.name = "se")
plot.df <- join(means.df, ses.df)
plot.df$L1 <- factor(as.character(plot.df$L1), labels= c("Nonword", "Word"))
levels(plot.df$PM) <- c( "NWc", "Wc")
ggplot(plot.df, aes(factor(day),mean)) + 
  geom_point(stat = "identity",aes(shape=PM, colour=PM), size=3, group=PM) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se, colour=PM)) +
  facet_grid(. ~ L1) +
  xlab("Day") + ylab("RT (seconds)") +
  ylim(0.6,1.2)

#False Alarms
FA <- okdats[!okdats$S =="PMW" & !okdats$S =="PMN",]
FA$S <- factor(as.character(FA$S))
FA$fa <- FA$R=="PM"
FAs <- arr2df(tapply(FA$fa,list(s=FA$s,PM=FA$PM, item=FA$S, day = FA$day), mean))
FAs <- FAs [!FAs$PM=="C",]; FAs$PM <- factor (as.character(FAs$PM))
wsAnova(FAs)
mneffects(FAs,list("PM","item", "day", c("item", "PM")),digits=4)
FAs[order(FAs$y),]

#PM analysis
# dPMacc <- dPMacc[!dPMacc$s %in% bad,]
pm.glmer <- glmer(C ~ S*PM*day+(1|s), data=PM,family=binomial(link = "probit"))
Anova(pm.glmer,type="II")

dPMacc <- arr2df(tapply(PM$C,list(s=PM$s,PM=PM$PM, day = PM$day, S= PM$S), mean))
wsAnova(dPMacc)

s<-(dPMacc[dPMacc$y==0,])
bad<-as.character(factor(as.character(s$s)))
mneffects(dPMacc,list("day","S", c("PM", "S", "day"), c("PM", "S"), digits=2))
se(dPMacc, facs= c("day"))
se(dPMacc, facs= c("PM", "S"))

######################### GRAPH PM acc ##############################################
graphdPMacc <- tapply(dPMacc$y, list(PM=factor(dPMacc$PM), day=factor(dPMacc$day), S= dPMacc$S), mean, na.rm=T)
sedPMacc <- se2(dPMacc, facs= c("PM", "day", "S"))

means.df <- melt(graphdPMacc, value.name = "mean")
ses.df   <- melt(sedPMacc,   value.name = "se")
plot.df <- join(means.df, ses.df)
plot.df$day <- factor(as.character(plot.df$day))
levels(plot.df$day) <- c( "Day 1", "Day 2", "Day 3")
levels(plot.df$PM) <- c("Important", "Unimportant")
ggplot(plot.df, aes(factor(day), mean)) + 
  geom_point(stat = "identity",aes(shape=PM, colour=PM), size=3, group=PM) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se, colour=PM)) +
  facet_grid(. ~ S) +
  ylab("Proportion of Correct PM Responses") +
  scale_y_continuous(limits = c(0.4, 0.9)) 

######## PM RTs

dPMrt <-arr2df(tapply(PM$RT[PM$C],list(s=PM$s[PM$C],PM=PM$PM[PM$C],day = PM$day[PM$C], S= PM$S[PM$C]),mean))
PMCrt.lmer<- lmer(y ~ S*PM*day+(1|s), data=dPMrt)
Anova(PMCrt.lmer,type="II")
wsAnova(dPMrt)
mneffects(dPMrt,list("PM", "S", "day", c("day", "S"), c("PM", "S"), c("PM", "S", "day")),digits=3)
se2(dPMrt, facs="day")
se2



dPMrt <-arr2df(tapply(PM$RT,list(s=PM$s,PM=PM$PM,day = PM$day, S= PM$S),mean))
mneffects(dPMrt,list("PM", "S", "day", c("day", "S"), c("PM", "S"), c("PM", "S", "day")),digits=3)

dPMrt <-arr2df(tapply(PM$RT[!PM$C],list(s=PM$s[!PM$C],PM=PM$PM[!PM$C],day = PM$day[!PM$C], S= PM$S[!PM$C]),mean))
wsAnova(dPMrt)
mneffects(dPMrt,list("PM", "S", "day", c("day", "S"), c("PM", "S"), c("PM", "S", "day")),digits=3)



#comparisons
# there are NAs for PM rts (where PM acc was 0 for a block for a day for a PM type), so the t.test uses different means to the ANOVA 
#there is a difference in means in the t.test only because it must use complete.cases, comparing the means without complete.cases on suggests no difference
#nws
bonfnwdPMrt <- data.frame()
bonfnwdPMrt <- tapply(dPMrt$y[dPMrt$S=="PMN"], list(subject= dPMrt$s[dPMrt$S=="PMN"], PM = factor(dPMrt$PM[dPMrt$S=="PMN"])), mean, na.rm=T)
t.test (bonfnwdPMrt[,1], bonfnwdPMrt[,2], paired=T)
#ws
bonfwdPMrt <- data.frame()
bonfwdPMrt <- tapply(dPMrt$y[dPMrt$S=="PMW"], list(subject= dPMrt$s[dPMrt$S=="PMW"], PM = factor(dPMrt$PM[dPMrt$S=="PMW"])), mean)
t.test (bonfwdPMrt[,1], bonfwdPMrt[,2], paired=T)

#graph PM RTs
graphdPMrt <- tapply(dPMrt$y,list(factor(dPMrt$PM), factor(dPMrt$day)), mean, na.rm=T)
sedPMrt <- se2(dPMrt, facs= c("PM", "day"))
dev.off()
for(i in 1:3){
  par(lab=c(1,5,7))
  par(new=TRUE)
  plot(x=i, y = graphdPMrt[1,i], ylim = c(0.4, 1.000), xlim = c(0.3, 3.5), xlab = "day", ylab = "RT", xaxt = "n", col=2, pch=2, main = "PM hits")
  add.bars(xval = i, graphdPMrt[1,i], sedPMrt[1,i], col = 2)
  points(x=i, graphdPMrt[2,i], col = 3, pch = 3)
  add.bars(xval = i, graphdPMrt[2,i], sedPMrt[2,i], col = 3)
}
#########################################################################

















