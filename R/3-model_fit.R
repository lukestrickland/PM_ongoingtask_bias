rm(list=ls())
setwd("~/OTbias_PM")
source("dmc/dmc.R")
load_model ("LBA","lbaN_B.R")
load("~/OTbias_PM/sampling/FFdstartpoints.RData")

PP <-
  h.post.predict.dmc(FFdsamples,
                     save.simulation = TRUE,
                     cores = length(FFdsamples))

save(PP, file="img/pp_top.RData")

sim <- do.call(rbind, PP)
# sim <- do.call(rbind, PPnoBs)
# Do the same for the data
data <- lapply(PP, function(x) attr(x, "data"))
data <- do.call(rbind, data)
GGLIST <- get.fitgglist.dmc(sim,data, factors=c("E", "S"))
pp.obj<- GGLIST[[1]]

pp.obj$E <- factor(as.character(pp.obj$E), labels=c("Nc", "Wc"))
RT.obj <- GGLIST[[2]]

RT.obj <- RT.obj[!((RT.obj$S=="nn"|RT.obj$S=="ww")& RT.obj$R=="P"),]


## Take only the PM accuracies and drop the R column.
PM.acc.obj <- pp.obj[(pp.obj$S=="pn" & pp.obj$R=="P")|
                            (pp.obj$S=="pw" & pp.obj$R=="P"),]
                          # !(names(pp.obj) %in% "R")]

PM<-ggplot.RP.dmc(PM.acc.obj, xaxis="E")
PM
#Shift in errors check

## Take only the PM accuracies and drop the R column.
PM.acc.obj <- pp.obj[(pp.obj$S=="pn" & pp.obj$R=="N")|
                            (pp.obj$S=="pw" & pp.obj$R=="W") |
                       (pp.obj$S=="pn" & pp.obj$R=="W")|
                            (pp.obj$S=="pw" & pp.obj$R=="N")
                       ,]
                          # !(names(pp.obj) %in% "R")]


PMerr<-ggplot.RP.dmc(PM.acc.obj, xaxis="E") 


PM.acc.obj<- pp.obj[(pp.obj$S=="pn" & pp.obj$R=="W")|
                            (pp.obj$S=="pw" & pp.obj$R=="N"),]
                          # !(names(pp.obj) %in% "R")]



PMerr1<-ggplot.RP.dmc(PM.acc.obj, xaxis="E")

grid.arrange(PM, PMerr,PMerr1)


RT.obj <- GGLIST[[2]]
RT.obj$E <- factor(as.character(RT.obj$E), labels=c("Nc", "Wc"))
## Take only the PM accuracies and drop the R column.
PM.RT.obj <- RT.obj[(RT.obj$S=="pn" & RT.obj$R=="P")|
                            (RT.obj$S=="pw" & RT.obj$R=="P"),]
                          # !(names(RT.obj) %in% "R")]

PM<-ggplot.RT.dmc(PM.RT.obj,xaxis="E")

#Shift in errors check

## Take only the PM accuracies and drop the R column.
PM.RT.obj <- RT.obj[(RT.obj$S=="pn" & RT.obj$R=="N")|
                            (RT.obj$S=="pw" & RT.obj$R=="W"),]
                          # !(names(RT.obj) %in% "R")]


PMerr<-ggplot.RT.dmc(PM.RT.obj, xaxis="E") 

PM.RT.obj<- RT.obj[(RT.obj$S=="pn" & RT.obj$R=="W")|
                            (RT.obj$S=="pw" & RT.obj$R=="N"),]
                          # !(names(RT.obj) %in% "R")]



PMerr1<-ggplot.RT.dmc(PM.RT.obj, xaxis="E")

grid.arrange(PM, PMerr,PMerr1)





RT.obj <- GGLIST[[2]]

RT.obj <- RT.obj[!((RT.obj$S=="ww"|RT.obj$S=="nn") & RT.obj$R=="P"),]

## Take only the PM accuracies and drop the R column.
PM.RT.obj <- RT.obj[(RT.obj$S=="pn" & RT.obj$R=="P")|
                            (RT.obj$S=="pw" & RT.obj$R=="P"),]
                          # !(names(RT.obj) %in% "R")]

PM<-ggplot.RT.dmc(PM.RT.obj,xaxis="E", do.quantiles=T)

#Shift in errors check

## Take only the PM accuracies and drop the R column.
PM.RT.obj <- RT.obj[(RT.obj$S=="pn" & RT.obj$R=="N")|
                            (RT.obj$S=="pw" & RT.obj$R=="W"),]
                          # !(names(RT.obj) %in% "R")]


PMerr<-ggplot.RT.dmc(PM.RT.obj, xaxis="E", do.quantiles=T) 

PM.RT.obj<- RT.obj[(RT.obj$S=="pn" & RT.obj$R=="W")|
                            (RT.obj$S=="pw" & RT.obj$R=="N"),]
                          # !(names(RT.obj) %in% "R")]



PMerr1<-ggplot.RT.dmc(PM.RT.obj, xaxis="E", do.quantiles=T)

grid.arrange(PM, PMerr,PMerr1)






