load("img/okdats.RData")
source("dmc/dmc.R")
load_model("LBA", "lbaN_B.R")

.IC.d#set up map for evidence accumulation rates
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
 "InnN2","IwwN2","IpnN2","IpwN2",
 "UnnN2","UwwN2","UpnN2","UpwN2",

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


#Thresholds fixed across conditions
model_fixedB <- model.dmc(
  factors = list(
    S = c("nn", "ww", "pn", "pw"),
    E = c("I", "U"),
    D = c("one", "two")
  ), responses = c("N", "W", "P"),
  p.map = list(
    A = "1", B = c("D", "R"),
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

p_vector_fixedB <-   c(
  t0=.3,
  A=0.5,
  sd_v.t = 1, 
  
  B.one.N=1,  B.two.N=1,  
  B.one.W=1,  B.two.W=1, 
  B.one.P=1,  B.two.P=1, 
  
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

p_prior_fixedB <-   prior.p.dmc(
  p1=c(t0=1, p_vector_fixedB[-(1)]),
  dists=  c(rep("beta", 1), rep("tnorm", length(p_vector_fixedB)-1)),
  p2= c(rep(1,1),1,1,rep(1,6),rep(2,41)),
  lower=c(rep(0.1,1),0,0,rep(0,6), rep(NA, 41)),
  upper=c(rep(1,1),10,rep(NA, length(p_vector_fixedB)-2))
)

dm_fixedB <- data.model.dmc(okdats,model_fixedB)

samples_fixedB <- h.samples.dmc(nmc = 180,p_prior_fixedB,dm_fixedB, thin=20)
save(samples_fixedB, file="samples_fixedB.RData")

# 
# #Rates fixed across conditions
# mapmeanv_nocond <-
#   empty.map(list(
#     S = c("nn", "ww", "pn", "pw"),
#     E = c("I", "U"),
#     D = c("one", "two"),
#     R = c("N", "W", "P")
#   ), 
#   levels=c(
#    "nnN1","wwN1","pnN1","nnW1",
#    "wwW1","pwN1", "pnW1","pwW1",
#    "pnP1","pwP1", "nnN2","wwN2",
#    "pnN2","nnW2","wwW2","pwN2",
#    "pnW2","pwW2", "pnP2","pwP2",
#    "fa"
#    )
#   )
# 
# mapmeanv_nocond[1:48] <- c(
#  "nnN1","wwN1","pnN1","pwN1",
#  "nnN1","wwN1","pnN1","pwN1",
#  "nnN2","wwN2","pnN2","pwN2",
#  "nnN2","wwN2","pnN2","pwN2",
# 
#  "nnW1","wwW1","pnW1","pwW1",
#  "nnW1","wwW1","pnW1","pwW1",
#  "nnW2","wwW2","pnW2","pwW2",
#  "nnW2","wwW2","pnW2","pwW2",
# 
#  "fa","fa","pnP1","pwP1",
#  "fa","fa","pnP1","pwP1",
#  "fa","fa","pnP2","pwP2",
#  "fa","fa","pnP2","pwP2"
# )
# 
# 
# 
# model_fixedV <- model.dmc(
#   factors = list(
#     S = c("nn", "ww", "pn", "pw"),
#     E = c("I", "U"),
#     D = c("one", "two")
#   ), responses = c("N", "W", "P"),
#   p.map = list(
#     A = "1", B = c("E", "D", "R"),
#     t0 = c("1"), mean_v = c("MAPMV"),
#     sd_v = c("MAPSDV"),st0 = "1",N = "1"
#   ),
#   match.map = list(
#     M = list( ww = "W", nn = "N", 
#               pn = "P", pw = "P"
#     ),
#     MAPMV = mapmeanv_nocond,
#     MAPSDV = mapsdv
#   ),
#   constants = c(sd_v.f = 1, st0 = 0, N = 3)
# )
# 
# p_vector_fixedV <-   c(
#   t0=.3,
#   A=0.5,
#   sd_v.t = 1, 
#   
#   B.I.one.N=1, B.U.one.N=1,
#   B.I.two.N=1, B.U.two.N=1,    
#   B.I.one.W=1, B.U.one.W=1,    
#   B.I.two.W=1, B.U.two.W=1,
#   B.I.one.P=1, B.U.one.P=1,     
#   B.I.two.P=1, B.U.two.P=1,
# 
#   mean_v.pwW1=1,mean_v.pnW1=0,mean_v.wwW1=1,mean_v.nnW1=1,
#   mean_v.pwN1=0,mean_v.pnN1=1,mean_v.wwN1=1, mean_v.nnN1=1,
#   mean_v.pwP1= 1, mean_v.pnP1= 1,
#   mean_v.pwW2=1,mean_v.pnW2=0,mean_v.wwW2=1,mean_v.nnW2=1,
#   mean_v.pwN2=0,mean_v.pnN2=1,mean_v.wwN2=1, mean_v.nnN2=1,
#   mean_v.pwP2= 1, mean_v.pnP2= 1,
#   mean_v.fa=-1
# )
# 
# check.p.vector(p_vector_fixedV, model_fixedV)
# 
# p_prior_fixedV <-   prior.p.dmc(
#   p1=c(t0=1, p_vector_fixedV[-(1)]),
#   dists=  c(rep("beta", 1), rep("tnorm", length(p_vector_fixedV)-1)),
#   p2= c(rep(1,1),1,1,rep(1,12),rep(2,21)),
#   lower=c(rep(0.1,1),0,0,rep(0,12), rep(NA, 21)),
#   upper=c(rep(1,1),10,rep(NA, length(p_vector_fixedV)-2))
# )
# 
# dm_fixedV <- data.model.dmc(okdats,model_fixedV)
# 
# samples_fixedV <- h.samples.dmc(nmc = 180,p_prior_fixedV,dm_fixedV, thin=20)
# save(samples_fixedV, file="samples_fixedV.RData")

#rates fixed over d1/d2
mapmeanv_nod <-
  empty.map(list(
    S = c("nn", "ww", "pn", "pw"),
    E = c("I", "U"),
    D = c("one", "two"),
    R = c("N", "W", "P")
  ), 
  levels=c(
   "InnN","IwwN","IpnN","InnW",
   "IwwW","IpwN","UpwN","UpnW",
   "IpnW","IpwW","IpnP","IpwP", 
   "UnnN","UwwN","UpnN", "UnnW",
   "UwwW","UpwW","UpnP","UpwP",
   "fa"
   )
  )

mapmeanv_nod[1:48] <- c(
 "InnN","IwwN","IpnN","IpwN",
 "UnnN","UwwN","UpnN","UpwN",
 "InnN","IwwN","IpnN","IpwN",
 "UnnN","UwwN","UpnN","UpwN",

 "InnW","IwwW","IpnW","IpwW",
 "UnnW","UwwW","UpnW","UpwW",
 "InnW","IwwW","IpnW","IpwW",
 "UnnW","UwwW","UpnW","UpwW",

 "fa","fa","IpnP","IpwP",
 "fa","fa","UpnP","UpwP",
 "fa","fa","IpnP","IpwP",
 "fa","fa","UpnP","UpwP"
)


model_fixed_day_V <- model.dmc(
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
    MAPMV = mapmeanv_nod,
    MAPSDV = mapsdv
  ),
  constants = c(sd_v.f = 1, st0 = 0, N = 3)
)

p_vector_fixed_day_V <-   c(
  t0=.3,
  A=0.5,
  sd_v.t = 1, 
  
  B.I.one.N=1, B.U.one.N=1,
  B.I.two.N=1, B.U.two.N=1,    
  B.I.one.W=1, B.U.one.W=1,    
  B.I.two.W=1, B.U.two.W=1,
  B.I.one.P=1, B.U.one.P=1,     
  B.I.two.P=1, B.U.two.P=1,
  
  mean_v.UpwW=1,mean_v.UpnW=0,mean_v.UwwW=1,mean_v.UnnW=1,
  mean_v.IpwW=1,mean_v.IpnW=0,mean_v.IwwW=1,mean_v.InnW=1,
  mean_v.UpwN=0,mean_v.UpnN=1,mean_v.UwwN=1, mean_v.UnnN=1,
  mean_v.IpwN=0,mean_v.IpnN=1,mean_v.IwwN=1, mean_v.InnN=1,
  mean_v.UpwP= 1, mean_v.UpnP= 1,
  mean_v.IpwP= 1, mean_v.IpnP= 1,
  mean_v.fa=-1
)

p_prior_fixed_day_V <-   prior.p.dmc(
  p1=c(t0=1, p_vector_fixed_day_V[-(1)]),
  dists=  c(rep("beta", 1), rep("tnorm", length(p_vector_fixed_day_V)-1)),
  p2= c(rep(1,1),1,1,rep(1,12),rep(2,21)),
  lower=c(rep(0.1,1),0,0,rep(0,12), rep(NA, 21)),
  upper=c(rep(1,1),10,rep(NA, length(p_vector_fixed_day_V)-2))
)

dm_fixed_day_V <- data.model.dmc(okdats,model_fixed_day_V)

samples_fixed_day_V <- h.samples.dmc(nmc = 180,p_prior_fixed_day_V,dm_fixed_day_V, thin=20)
save(samples_fixed_day_V, file="samples_fixed_day_V.RData")

##Try special model with rate bias fixed
load_model("LBA", "lbaN_B_fixedbias.R")



mapmeanv_noOTbias <-
  empty.map(list(
    S = c("nn", "ww", "pn", "pw"),
    E = c("I", "U"),
    D = c("one", "two"),
    R = c("N", "W", "P")
  ), 
  levels=c(
    "nnN1","wwN1","pnN1","nnW1",
   "wwW1","pwN1", "pnW1","pwW1",
    "nnN2","wwN2",
   "pnN2","nnW2","wwW2","pwN2",
   "pnW2","pwW2",
   "IpnP1","IpwP1", 
   "UpnP1","UpwP1",
   "IpnP2","IpwP2",
   "UpnP2","UpwP2",
   "fa"
   )
  )

mapmeanv_noOTbias[1:48] <- c(
 "nnN1","wwN1","pnN1","pwN1",
 "nnN1","wwN1","pnN1","pwN1",
 "nnN2","wwN2","pnN2","pwN2",
 "nnN2","wwN2","pnN2","pwN2",

 "nnW1","wwW1","pnW1","pwW1",
 "nnW1","wwW1","pnW1","pwW1",
 "nnW2","wwW2","pnW2","pwW2",
 "nnW2","wwW2","pnW2","pwW2",

 "fa","fa","IpnP1","IpwP1",
 "fa","fa","UpnP1","UpwP1",
 "fa","fa","IpnP2","IpwP2",
 "fa","fa","UpnP2","UpwP2"
)

mapVshift <-
  empty.map(list(
    S = c("nn", "ww", "pn", "pw"),
    E = c("I", "U"),
    D = c("one", "two"),
    R = c("N", "W", "P")
  ), 
  levels=c(
   "NN1","WN1","NW1",
   "WW1",
   "NN2","WN2",
   "NW2","WW2",
   "zero"
   )
  )

mapVshift[1:48] <- c(
 "zero", "zero", "zero", "zero",
 "NN1","WN1","NN1","WN1",
 "zero", "zero", "zero", "zero",
 "NN2","WN2","NN2","WN2",

 "zero", "zero", "zero", "zero",
 "NW1","WW1","NW1","WW1",
 "zero", "zero", "zero", "zero",
 "NW2","WW2","NW2","WW2",

 "zero", "zero", "zero", "zero",
 "zero", "zero", "zero", "zero",
 "zero", "zero", "zero", "zero",
 "zero", "zero", "zero", "zero"
)


model_bias_reac_separate <- model.dmc(
  factors = list(
    S = c("nn", "ww", "pn", "pw"),
    E = c("I", "U"),
    D = c("one", "two")
  ), responses = c("N", "W", "P"),
  p.map = list(
    A = "1", B = c("E", "D", "R"),
    t0 = c("1"), mean_v = c("MAPMV"),
    sd_v = c("MAPSDV"),st0 = "1",Vshift = "MAPVSHIFT",
    N = "1"
  ),
  match.map = list(
    M = list( ww = "W", nn = "N", 
              pn = "P", pw = "P"
    ),
    MAPMV = mapmeanv_noOTbias,
    MAPSDV = mapsdv,
    MAPVSHIFT = mapVshift
  ),
  constants = c(sd_v.f = 1, st0 = 0, N = 3,
                Vshift.zero=0)
)

p_vector_bias_reac_separate  <-   c(
  t0=.3,
  A=0.5,
  sd_v.t = 1, 
  
  B.I.one.N=1, B.U.one.N=1,
  B.I.two.N=1, B.U.two.N=1,    
  B.I.one.W=1, B.U.one.W=1,    
  B.I.two.W=1, B.U.two.W=1,
  B.I.one.P=1, B.U.one.P=1,     
  B.I.two.P=1, B.U.two.P=1,
  
  
  
  mean_v.pwW1=1,mean_v.pnW1=0,mean_v.wwW1=1,mean_v.nnW1=1,
  mean_v.pwN1=0,mean_v.pnN1=1,mean_v.wwN1=1, mean_v.nnN1=1,
  mean_v.pwW2=1,mean_v.pnW2=0,mean_v.wwW2=1,mean_v.nnW2=1,
  mean_v.pwN2=0,mean_v.pnN2=1,mean_v.wwN2=1, mean_v.nnN2=1,
  
  Vshift.NN1=0,   Vshift.WN1 =0,   Vshift.NW1=0,    Vshift.WW1=0,   
  Vshift.NN2 =0,   Vshift.WN2 =0,   Vshift.NW2 =0,   Vshift.WW2 =0,  

  mean_v.UpwP1= 1, mean_v.UpnP1= 1,
  mean_v.IpwP1= 1, mean_v.IpnP1= 1,
  mean_v.UpwP2= 1, mean_v.UpnP2= 1,
  mean_v.IpwP2= 1, mean_v.IpnP2= 1,
  mean_v.fa=-1
)

check.p.vector(p_vector_bias_reac_separate, model_bias_reac_separate)

p_prior_bias_reac_separate  <-   prior.p.dmc(
  p1=c(t0=1, p_vector_bias_reac_separate[-(1)]),
  dists=  c(rep("beta", 1), rep("tnorm", length(p_vector_bias_reac_separate)-1)),
  p2= c(rep(1,1),1,1,rep(1,12),rep(2,33)),
  lower=c(rep(0.1,1),0,0,rep(0,12), rep(NA, 33)),
  upper=c(rep(1,1),10,rep(NA, length(p_vector_bias_reac_separate)-2))
)

dm_bias_reac_separate  <- data.model.dmc(okdats,model_bias_reac_separate)

samples_bias_reac_separate <- h.samples.dmc(nmc = 180,p_prior_bias_reac_separate 
                                            ,dm_bias_reac_separate , thin=20)
save(samples_bias_reac_separate, file="samples_bias_reac_separate.RData")

simulate.dmc(p_vector_bias_reac_separate, model_bias_reac_separate, n=10000) %>% 
  group_by(S,E, D) %>% summarise(mean(R=="N")) %>% arrange(D)

