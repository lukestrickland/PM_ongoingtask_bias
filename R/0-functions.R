
label_msds <- function(msds) {
  msds$E <- NA
  msds$day <- NA
  msds$R <- NA
  msds$S <- NA
  msds$ot_correct <- NA
  msds$isPM <- NA
  msds$ot_match <- NA
  
  old_rnames <- rownames(msds)
  
  msds$E[grep ("I", old_rnames)] <- "Nc"
  msds$E[grep ("U", old_rnames)] <- "Wc"
  
  msds$R[grep ("N", old_rnames)] <- "Non-word"
  msds$R[grep ("W", old_rnames)] <- "Word"
  msds$R[grep ("P", old_rnames)] <- "PM"
  
  msds$day[grep ("one", old_rnames)] <- "One"
  msds$day[grep ("two", old_rnames)] <- "Two"
  msds$day[grep ("1$", old_rnames)] <- "One"
  msds$day[grep ("2$", old_rnames)] <- "Two"
  
  msds$S[grep ("nn", old_rnames)] <- "Non-word Trial"
  msds$S[grep ("ww", old_rnames)] <- "Word Trial"
  msds$S[grep ("pw", old_rnames)] <- "PMW Trial"
  msds$S[grep ("pn", old_rnames)] <- "PMN Trial"
  
  msds$ot_correct[grep ("mean_v.*n", old_rnames)] <- "Non-word"
  msds$ot_correct[grep ("mean_v.*w", old_rnames)] <- "Word"
  
  msds$ot_match[!(msds$R == "PM") & !is.na(msds$S)] <- "Mismatch"
  msds$ot_match[msds$ot_correct == msds$R] <- "Match"
  
  
  msds$isPM[!grepl("mean_v.*p", old_rnames) &
              !is.na(msds$S)] <- "nonPM"
  msds$isPM[grep ("mean_v.*p", old_rnames)] <- "PM"
  
  msds$day <- factor(msds$day, levels = c("One", "Two"))
  msds$R <- factor(msds$R, levels = c("Non-word", "Word", "PM"))
  msds$E <- factor(msds$E)
  msds$S <- factor(msds$S,
                   c("Non-word Trial", "PMN Trial",
                     "Word Trial", "PMW Trial"))
  msds
}


get.normalized.ldI <- function(df) {
  
  NcpwN <- length(df$RT[df$S=="pw" & df$E=="I" & df$R=="N"])/
    length(df$RT[df$S=="pw" & df$E=="I" & df$R!="P"])
  
  WcpwN <- length(df$RT[df$S=="pw" & df$E=="U" & df$R=="N"])/
  length(df$RT[df$S=="pw" & df$E=="U" & df$R!="P"])
  
  NcpnW <- length(df$RT[df$S=="pn" & df$E=="I" & df$R=="W"])/
    length(df$RT[df$S=="pn" & df$E=="I" & df$R!="P"])
  
  WcpnW <- length(df$RT[df$S=="pn" & df$E=="U" & df$R=="W"])/
  length(df$RT[df$S=="pn" & df$E=="U" & df$R!="P"])
  
  out <- c(NcpwN,  WcpwN, NcpnW, WcpnW)
  names(out) <- c("NcpwN",  "WcpwN", "NcpnW", "WcpnW")
  out
}


get.normalized.ldC <- function(df) {
  
  NcpwW <- length(df$RT[df$S=="pw" & df$E=="I" & df$R=="W"])/
    length(df$RT[df$S=="pw" & df$E=="I" & df$R!="P"])
  
  WcpwW <- length(df$RT[df$S=="pw" & df$E=="U" & df$R=="W"])/
  length(df$RT[df$S=="pw" & df$E=="U" & df$R!="P"])
  
  NcpnN <- length(df$RT[df$S=="pn" & df$E=="I" & df$R=="N"])/
    length(df$RT[df$S=="pn" & df$E=="I" & df$R!="P"])
  
  WcpnN <- length(df$RT[df$S=="pn" & df$E=="U" & df$R=="N"])/
  length(df$RT[df$S=="pn" & df$E=="U" & df$R!="P"])
  
  out <- c(NcpwW,  WcpwW, NcpnN, WcpnN)
  names(out) <- c("NcpwW",  "WcpwW", "NcpnN", "WcpnN")
  out
}

get.diff.normalized.ldC <- function(df) {
  
  NcpwW <- length(df$RT[df$S=="pw" & df$E=="I" & df$R=="W"])/
    length(df$RT[df$S=="pw" & df$E=="I" & df$R!="P"])
  
  WcpwW <- length(df$RT[df$S=="pw" & df$E=="U" & df$R=="W"])/
  length(df$RT[df$S=="pw" & df$E=="U" & df$R!="P"])
  
  NcpnN <- length(df$RT[df$S=="pn" & df$E=="I" & df$R=="N"])/
    length(df$RT[df$S=="pn" & df$E=="I" & df$R!="P"])
  
  WcpnN <- length(df$RT[df$S=="pn" & df$E=="U" & df$R=="N"])/
  length(df$RT[df$S=="pn" & df$E=="U" & df$R!="P"])
  
  out <- c(NcpwW - WcpwW,  WcpnN - NcpnN)
  names(out) <- c("NcpwW - WcpwW",  "WcpnN - NcpnN")
  out
}

label_effects <- function (effects) {
  effects$E <- NA
  effects$R <- NA
  effects$S <- NA
  effects$E[grep("Nc", rownames(effects))] <- "Nc"
  effects$E[grep("Wc", rownames(effects))] <- "Wc"
  effects$R[grep("N$", rownames(effects))] <- "N"
  effects$R[grep("W$", rownames(effects))] <- "W"
  effects$S[grep("w", rownames(effects))] <- "Word Trial"
  effects$S[grep("n", rownames(effects))] <- "Non-word Trial"
  effects
}


get.diff.OT.normalized.ldC <- function(df) {
  
  NcwwW <- length(df$RT[df$S=="ww" & df$E=="I" & df$R=="W"])/
    length(df$RT[df$S=="ww" & df$E=="I" & df$R!="P"])
  
  WcwwW <- length(df$RT[df$S=="ww" & df$E=="U" & df$R=="W"])/
  length(df$RT[df$S=="ww" & df$E=="U" & df$R!="P"])
  
  NcnnN <- length(df$RT[df$S=="nn" & df$E=="I" & df$R=="N"])/
    length(df$RT[df$S=="nn" & df$E=="I" & df$R!="P"])
  
  WcnnN <- length(df$RT[df$S=="nn" & df$E=="U" & df$R=="N"])/
  length(df$RT[df$S=="nn" & df$E=="U" & df$R!="P"])
  
  out <- c(NcwwW - WcwwW,  WcnnN - NcnnN)
  names(out) <- c("NcwwW - WcwwW",  "WcnnN - NcnnN")
  out
}


