load("img/okdats.RData")
source("dmc/dmc.R")
load_model("LBA", "lbaN_B.R")

#set up map for evidence accumulation rates
#Note that nonword caution is coded as I and word caution as U, to avoid
#grepping problems in DMC's model.dmc function.

mapmeanv <-
  empty.map(list(
    S = c("nn", "ww", "pn", "pw"),
    E = c("I", "U"),
    D = c("one", "two"),
    R = c("N", "W", "P")
  ), 
  levels=c(
   "InnN1","IwwN1","IpnN1","InnW1",
   "IwwW1","IpwN1","UpwN1","UpnW1",
   "IpnW1","IpwW1","IpnP1","IpwP1", 
   "UnnN1","UwwN1","UpnN1", "UnnW1",
   "UwwW1","UpwW1","UpnP1","UpwP1",
   "InnN2","IwwN2","IpnN2","InnW2",
   "IwwW2","IpwN2","UpwN2","UpnW2",
   "IpnW2","IpwW2","IpnP2","IpwP2",
   "UnnN2","UwwN2","UpnN2", "UnnW2",
   "UwwW2", "UpwW2","UpnP2","UpwP2",
   "fa"
   )
  )

mapmeanv[1:48] <- c(
 "InnN1","IwwN1","IpnN1","IpwN1",
 "UnnN1","UwwN1","UpnN1","UpwN1",
 "InnN2","IwwN2","IpnN2","UpwN2",
 "UnnN2","UwwN2","UpnN2","IpwN2",

 "InnW1","IwwW1","IpnW1","IpwW1",
 "UnnW1","UwwW1","UpnW1","UpwW1",
 "InnW2","IwwW2","IpnW2","IpwW2",
 "UnnW2","UwwW2","UpnW2","UpwW2",

 "fa","fa","IpnP1","IpwP1",
 "fa","fa","UpnP1","UpwP1",
 "fa","fa","IpnP2","IpwP2",
 "fa","fa","UpnP2","UpwP2"
)

mapsdv <-
  empty.map(list(
    S = c("nn", "ww", "pn", "pw"),
    E = c("I", "U"),
    D = c("one", "two"),
    R = c("N", "W", "P")
  ),
  levels = c("t", "f"))

mapsdv[1:48] <- c(
  rep(c("t","f","f","f"), 4), 
  rep(c("f","t","f","f"), 4), 
  rep(c("f","f","t","t"), 4)
)

#Specify model. Note that I am using the flexible lba (lbaN_B) which allows
#specifying different numbers of accumulators because it uses a faster 
#way to calculate likelihoods. Hence setting N=3 as a constant.

model_top <- model.dmc(
  factors = list(
    S = c("nn", "ww", "pn", "pw"),
    E = c("I", "U"),
    D = c("one", "two")
  ), responses = c("N", "W", "P"),
  p.map = list(
    A = "1", B = c("E", "D", "R"),
    t0 = c("1"), mean_v = c("MAPMV"),
    sd_v = c("MAPSDV"),st0 = "1",N = "1"
  ),
  match.map = list(
    M = list( ww = "W", nn = "N", 
              pn = "P", pw = "P"
    ),
    MAPMV = mapmeanv,
    MAPSDV = mapsdv
  ),
  constants = c(sd_v.f = 1, st0 = 0, N = 3)
)

p_vector_top <-   c(
  t0=.3,
  A=0.5,
  sd_v.t = 1, 
  
  B.I.one.N=1, B.U.one.N=1,
  B.I.two.N=1, B.U.two.N=1,    
  B.I.one.W=1, B.U.one.W=1,    
  B.I.two.W=1, B.U.two.W=1,
  B.I.one.P=1, B.U.one.P=1,     
  B.I.two.P=1, B.U.two.P=1,
  
  mean_v.UpwW1=1,mean_v.UpnW1=0,mean_v.UwwW1=1,mean_v.UnnW1=1,
  mean_v.IpwW1=1,mean_v.IpnW1=0,mean_v.IwwW1=1,mean_v.InnW1=1,
  mean_v.UpwN1=0,mean_v.UpnN1=1,mean_v.UwwN1=1, mean_v.UnnN1=1,
  mean_v.IpwN1=0,mean_v.IpnN1=1,mean_v.IwwN1=1, mean_v.InnN1=1,
  mean_v.UpwP1= 1, mean_v.UpnP1= 1,
  mean_v.IpwP1= 1, mean_v.IpnP1= 1,
  
  mean_v.UpwW2=1,mean_v.UpnW2=0,mean_v.UwwW2=1,mean_v.UnnW2=1,
  mean_v.IpwW2=1,mean_v.IpnW2=0,mean_v.IwwW2=1,mean_v.InnW2=1,
  mean_v.UpwN2=0,mean_v.UpnN2=1,mean_v.UwwN2=1, mean_v.UnnN2=1,
  mean_v.IpwN2=0,mean_v.IpnN2=1,mean_v.IwwN2=1, mean_v.InnN2=1,
  mean_v.UpwP2= 1, mean_v.UpnP2= 1,
  mean_v.IpwP2= 1, mean_v.IpnP2= 1,
  mean_v.fa=-1
)

check.p.vector(p_vector, model_top)

#Set priors. Uniform distribution for t0 between 0 and 1. 
#Normal distribution for other parameters with mean equal to p_vector.
#prior SDs = 1 for thresholds, 2 for rates.

p_prior_top <-   prior.p.dmc(
  p1=c(t0=1, p_vector_top[-(1)]),
  dists=  c(rep("beta", 1), rep("tnorm", length(p_vector_top)-1)),
  p2= c(rep(1,1),1,1,rep(1,12),rep(2,41)),
  lower=c(rep(0.1,1),0,0,rep(0,12), rep(NA, 41)),
  upper=c(rep(1,1),10,rep(NA, length(p_vector_top)-2))
)

dm_top <- data.model.dmc(okdats,model_top)

samples_top <- h.samples.dmc(nmc = 180,p_prior_top,dm_top, thin=20)
#Initially save samples in main directory. Makes it easier to run them
#using the grid (as run.grid.dmc requires a dmc folder with the samples
#to source)
save(samples_top, file="samples_top.RData")