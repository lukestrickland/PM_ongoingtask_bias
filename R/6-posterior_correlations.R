##Note these exploratory analyses not included in paper due to low power (32 participants)
#coding is extremely shabby but may come back and use for something else later


samples_top <- samples_top[!names(samples_top) %in% not_very_biased]

data<- get.hdata.dmc(samples_top)

for (i in unique(data$s)) {
  effects<- get.diff.PM.OT.normalized.ldC(data[data$s==i,])
  if (i ==1) out <- effects else out <- rbind(out,effects)
  out
}

dimnames(out)[[1]] <- unique(data$s)

thetas <- get.theta.array(samples_top)

DIM <- dim(thetas)
DIM[2] <- 1
eff.array <- array(NA, dim=DIM)


Nbias <-
  apply(thetas[, grepl("B.I.*.N", colnames(thetas)), , ] - thetas[, grepl("B.U.*.N", colnames(thetas)), , ], c(1, 3, 4),
        mean)      
dim(samples_top[[1]]$theta)
dim(Nbias) <- c(168,1, 180,32)


Wbias <-
  apply(thetas[, grepl("B.U.*.W", colnames(thetas)), , ] - thetas[, grepl("B.I.*.W", colnames(thetas)), , ], c(1, 3, 4),
        mean)      
dim(Wbias) <- c(168,1, 180,32)

# 

biasshift <-
  apply((thetas[, grepl("B.I.*.N", colnames(thetas)), , ] - thetas[, grepl("B.U.*.N", colnames(thetas)), , ]) +
          (thetas[, grepl("B.U.*.W", colnames(thetas)), , ] - thetas[, grepl("B.I.*.W", colnames(thetas)), , ]), c(1, 3, 4),
        mean)
dim(biasshift) <- c(168,1, 180,32)

diffs<- abind(Nbias, Wbias, biasshift, along=2)


diffs<- abind(Nbias, Wbias, along=2)

colnames(diffs) <- c("Nbias", "Wbias", "biasshift")

cor_Nbias_wadvantage<- get.cors(diffs, out[,"reacmagNbias"])
cor_Wbias_nadvantage<- get.cors(diffs, out[,"reacmagWbias"])
# Tcor <- lapply(Tcor, function(x) x[1:13])
save(Tcor, file="Tcor_full.RData")



get.sampcors <- function(thetas, data) {
  
  CORS <- apply(thetas, c(1,2,3), function(x) cor(x, data))
  post_medians <- apply(CORS, c(2), mean)
  post_LCI <- apply(CORS, c(2), quantile, probs=0.025)
  post_HCI <- apply(CORS, c(2), quantile, probs=0.975)
  out <- list(post_medians, post_LCI, post_HCI)
  names(out) <- c("medians", "LCI", "HCI")
  out
}

cor_Nbias_wadvantage<- get.sampcors(diffs, out[,"reacmagNbias"])
cor_Wbias_nadvantage<- get.sampcors(diffs, out[,"reacmagWbias"])



test <- apply( thetas[,grepl("B.I.one.N", colnames(thetas)),,,drop=F], c(1,3,4),
                mean)      
dim(test) <- c(168,1, 180,32)

get.cors(test, out[,"reacmagNbias"])
