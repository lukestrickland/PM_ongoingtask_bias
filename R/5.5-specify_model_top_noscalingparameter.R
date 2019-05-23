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
  constants = c( st0 = 0, N = 3)
)