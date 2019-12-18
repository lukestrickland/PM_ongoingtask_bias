source("dmc/dmc.R")
source("dmc/dmc_extras.R")
load_model ("LBA","lbaN_B.R")
load("samples/samples_top.RData")
theme_set(theme_simple())

# PP <-
#   h.post.predict.dmc(samples_top,
#                      save.simulation = TRUE,
#                      cores = length(samples_top),
#                      n.post=200)
# save(PP, file="img/PP.RData")

load("img/PP.RData")

#get data summaries
# fit_summaries <- GET.fitgglist.dmc(PP, factors = c("S", "E"))
# save(fit_summaries, file="img/fit_summaries_top.RData")
load("img/fit_summaries_top.RData")

#convert factor names back 
fit_summaries <- lapply(fit_summaries, function(x) {
  x$E <-  factor(as.character(x$E), labels = c("Nc", "Wc"))
  x
})

df_relabel <- function(df) {
  
df$S <- factor(df$S, levels=c("nn", "pn", "pw", "ww"),
                   labels=c("Non-word Trial", "PM Non-word Trial", "PM Word Trial", "Word Trial"))

df$R <- factor(df$R, levels=c("N", "P", "W"),
                   labels=c("Non-word Response", "PM Response", "Word Response"))
df
}

#Model fits to ongoing task performance
OT_acc <- fit_summaries$pps[(fit_summaries$pps$S == "ww" &
                           fit_summaries$pps$R == "W") |
                          (fit_summaries$pps$S == "nn" &
                             fit_summaries$pps$R == "N"),]

OT_plot<-ggplot.RP.dmc(df_relabel(OT_acc), xaxis="E") +ylab ("Response Proportion") +xlab("") +ylim(0.84,0.98)
OT_plot

OT_RTs <- fit_summaries$RTs[(fit_summaries$RTs$S == "ww"|
                           fit_summaries$RTs$S == "nn") &
                             fit_summaries$RTs$R != "P",]

OT_RTs_plot<-ggplot.RT.dmc(df_relabel(OT_RTs), xaxis="E") +xlab("Bias Condition") +ylim(0.3,2.4)
OT_RTs_plot

grid.arrange(OT_plot, OT_RTs_plot, layout_matrix = cbind(c(1,2,2), c(1,2,2))) 

#Model fits to all the intricate PM trial data
## Take only the PM accuracies and drop the R column.
PM_acc <-
  fit_summaries$pps[(fit_summaries$pps$S == "pn" &
                      fit_summaries$pps$R == "P") |
                     (fit_summaries$pps$S == "pw" &
                        fit_summaries$pps$R == "P"), ]

PM_plot<-ggplot.RP.dmc(df_relabel(PM_acc), xaxis="E")+xlab("")+ylab("")

#correct ldt responses on PM trials
PM_err_ldC <-
  fit_summaries$pps[(fit_summaries$pps$S == "pn" &
                       fit_summaries$pps$R == "N") |
                      (fit_summaries$pps$S == "pw" &
                         fit_summaries$pps$R == "W") 
                    , ]
                         


PMerrC_plot <-ggplot.RP.dmc(df_relabel(PM_err_ldC), xaxis="E") +xlab("") +ylim(0.28,0.44)

#incorrect ldt on PM trials
PM_err_ldI <-
  fit_summaries$pps[(fit_summaries$pps$S == "pn" &
                       fit_summaries$pps$R == "W") |
                      (fit_summaries$pps$S == "pw" &
                         fit_summaries$pps$R == "N"),]

PMerrI_plot<-ggplot.RP.dmc(df_relabel(PM_err_ldI), xaxis="E") + ylim(0,0.15)+xlab("Bias Condition")+ylab("")

grid.arrange(PM_plot, PMerrC_plot, PMerrI_plot)


PM_RTs <-
  fit_summaries$RTs[(fit_summaries$RTs$S == "pn" &
                       fit_summaries$RTs$R == "P") |
                      (fit_summaries$RTs$S == "pw" &
                         fit_summaries$RTs$R == "P"),]

PM_RTs_plot <-ggplot.RT.dmc(df_relabel(PM_RTs),xaxis="E") +xlab("")+ylab("") +ylim(0.3,2.4)

PMerr_RTs_ldC <-
  fit_summaries$RTs[(fit_summaries$RTs$S == "pn" &
                       fit_summaries$RTs$R == "N") |
                      (fit_summaries$RTs$S == "pw" &
                         fit_summaries$RTs$R == "W"), ]

PMerrC_RTs_plot <-ggplot.RT.dmc(df_relabel(PMerr_RTs_ldC), xaxis="E") +xlab("")+ylim(0.3,2.4)

PMerr_RTs_ldI <-
  fit_summaries$RTs[(fit_summaries$RTs$S == "pn" &
                       fit_summaries$RTs$R == "W") |
                      (fit_summaries$RTs$S == "pw" &
                         fit_summaries$RTs$R == "N"), ]


PMerrI_RTs_plot <-ggplot.RT.dmc(df_relabel(PMerr_RTs_ldI), xaxis="E")+xlab("Bias Condition")+ylab("")+ylim(0.3,2.4)

grid.arrange(PM_RTs_plot, PMerrC_RTs_plot,PMerrI_RTs_plot)
