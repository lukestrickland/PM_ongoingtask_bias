rm(list=ls())
setwd("~/OTbias_PM")
source("dmc/dmc.R")
load_model ("LBA", "lbaN_B.R")

run.grid.dmc("samples_top",model.dir ="LBA", 
             model.file="lbaN_B.R",user="ljs392",n.add=60, wall.hours = 300,
             GB = 3)