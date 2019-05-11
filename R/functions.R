
clean <- function(df) {
  dfc <- df
  n=tapply(df$RT,list(df$s),length)
  ns=tapply(df$RT,list(df$s),length)
  mn=tapply(df$RT,list(df$s),mean)
  sd=tapply(df$RT,list(df$s),IQR)
  upper <- mn+3*(sd/1.349)
  lower <- 0.2
  bad <- logical(dim(df)[1])
  levs <- paste(df$s,sep=".")
  for (i in levels(df$s)){
    lev <- i    
    bad[levs==lev] <- df[levs==lev,"RT"] > upper[i] | df[levs==lev,"RT"] < lower
  }
  df=df[!bad,]
  nok=tapply(df$RT,list(df$s),length)
  pbad=100-100*nok/n
  nok=tapply(df$RT,list(df$s),length)
  pbad=100-100*nok/ns
  print(sort(round(pbad,5)))  
  print(mean(pbad,na.rm=T))
  df
}

wsAnova=function(dat,SStype=3,spss=F) {
  has.car=require(car)  
  if (!has.car) return("No \"car\"package, no ANOVA\n") 
  for (i in 1:(dim(dat)[2]-1)) dat[,i] <- factor(dat[,i])
  dat=dat[do.call(order,dat[,-dim(dat)[2]]),]
  snams=levels(dat[,1]); ns=length(snams)
  dvnam=names(dat)[dim(dat)[2]]
  facn=names(dat)[-c(1,dim(dat)[2])]
  nifac=length(facn)
  idata=data.frame(dat[dat[,1]==snams[1],facn])
  names(idata)=facn
  for (i in facn) 
    if (i==facn[1]) ifacn=as.character(idata[,1]) else 
      ifacn=paste(ifacn,as.character(idata[,i]),sep=".")
  facnr=facn[nifac:1]
  e.mv=matrix(unlist(tapply(dat[,dvnam],dat[,facnr],function(x){x})),
              ncol=length(ifacn),dimnames=list(snams,ifacn))
  print(summary(Anova(lm(e.mv ~ 1),
                idata=idata,type=SStype, 
                idesign=formula(paste("~",paste(facn,collapse="*")))),
          multivariate=FALSE))
  if (spss) {
    e.mv=cbind.data.frame(s=row.names(e.mv),e.mv)
    row.names(e.mv)=NULL
    e.mv
  }
}

mneffects=function(df,elist,digits=3,err=F,vars=F,dvnam="y") {
  df <- df[,c(names(df)[names(df)!=dvnam],dvnam)]
  dvnam=dim(df)[2]
  for (i in 1:(dim(df)[2]-1)) df[,i] <- factor(df[,i])  
  for (i in 1:length(elist)) {
    cat(paste(paste(elist[[i]],collapse=":"),"\n"))
    mns=tapply(df[,dvnam],df[,elist[[i]]],mean,na.rm=T)
    if (err) print(round(plogis(mns),digits)) else
      if (vars) print(round(sqrt(mns),digits))  else
        print(round(mns,digits))    
    cat("\n")
  }  
}


se=function(df,facs,sfac="s",dvnam="y",ws=TRUE,ci="SE") {
  df <- df[,c(names(df)[names(df)!=dvnam],dvnam)]
  dvnam=dim(df)[2]
  for (i in 1:(dim(df)[2]-1)) df[,i] <- factor(df[,i])  
  if (ws) {
    smns <- tapply(df[,dvnam],df[,sfac],mean)
    smn <- df[,sfac]
    levels(smn) <- smns
    df[,dvnam] <- df[,dvnam]-as.numeric(as.character(smn))  
  }
  mn=tapply(df[,dvnam],df[,facs],mean)
  se=tapply(df[,dvnam],df[,facs],sd)
  ns <- length(levels(df[,sfac]))
  if (ws) {
    m <- prod(dim(se))
    ns <- ns*(m-1)/m
  }
  if (is.na(ci)) mn else {
    if (ci=="SE") se/sqrt(ns) else
     qt(1-(100-ci)/200,ns-1)*se/sqrt(ns)
  }
}

se2 <- function(df,facs,sfac="s",dvnam="y",ws=TRUE,ci="SE") {
  df <- as.data.frame(df)
  df <- df[,c(names(df)[names(df)!=dvnam],dvnam)]
  dvnam=dim(df)[2]
  for (i in 1:(dim(df)[2]-1)) df[,i] <- factor(df[,i])  
  if (ws) {
    smns <- tapply(df[,dvnam],df[,sfac],mean, na.rm=T)
    smn <- df[,sfac]
    levels(smn) <- smns
    df[,dvnam] <- df[,dvnam]-as.numeric(as.character(smn))  
  }
  mn=tapply(df[,dvnam],df[,facs],mean, na.rm=T)
  se=tapply(df[,dvnam],df[,facs],sd, na.rm=T)
  ns <- length(levels(df[,sfac]))
  if (ws) {
    m <- prod(dim(se))
    ns <- ns*(m-1)/m
  }
  if (is.na(ci)) mn else {
    if (ci=="SE") se/sqrt(ns) else
     qt(1-(100-ci)/200,ns-1)*se/sqrt(ns)
  }
}

add.bars=function(mn,se,xvals=NA,len=.1,antiprobit=FALSE,col="black") {
  
  plotbars <- function(x,m,l,h,len,col="black") {
    for (j in 1:length(x)) arrows(x[j],m[j],x[j],l[j],length=len,angle=90,col=col)
    for (j in 1:length(x)) arrows(x[j],m[j],x[j],h[j],length=len,angle=90,col=col)    
  }
  
  if (any(is.na(xvals))) if (is.matrix(mn)) 
    xvals <- as.numeric(dimnames(mn)[[2]]) else
    xvals <- as.numeric(factor(names(mn)))
  lo <- mn-se
  hi <- mn+se
  if (antiprobit) {
    mn=pnorm(mn)
    lo=pnorm(lo)
    hi=pnorm(hi)
  }
  if (!is.matrix(mn)) 
      plotbars(xvals,mn,lo,hi,col=col,len=len) else
    for (i in 1:dim(mn)[1]) 
      plotbars(x=xvals,m=mn[i,],l=lo[i,],h=hi[i,],len=len,col=col)
}    

arr2df=function(arr) {
  if (is.null(dim(arr))) out=data.frame(y=arr) else {
    dn=dimnames(arr)
    if (length(dn)==1) {
      out=cbind.data.frame(factor(dn[[1]],dn[[1]]),arr)
      names(out)=c(names(dn),"y")
      row.names(out)=NULL
    } else {
      tmp=vector(mode="list",length=length(dn))
      names(tmp)=names(dn)
      k=1
      for (j in names(dn)) {
        n=length(dn[[j]])
        tmp[[j]]=gl(n,k,length(arr),dn[[j]])
        k=k*n
      }
      out=cbind(data.frame(tmp),y=as.vector(arr))
      row.names(out)=NULL
    }
  }
  out
}


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
