rm(list=ls())
setwd("~/OTbias_PM")
source("dmc/dmc.R")
load_model ("LBA", "lbaN_B_fixedbias.R")

run.grid.dmc("samples_bias_reac_separate",model.dir ="LBA",
             model.file="lbaN_B_fixedbias.R",user="ljs392",n.add=60, wall.hours = 300,
             GB = 3)

# 
# rm(list=ls())
# setwd("~/OTbias_PM")
# source("dmc/dmc.R")
# load_model ("LBA", "lbaN_B.R")
# 
# run.grid.dmc("samples_top",model.dir ="LBA",
#              model.file="lbaN_B.R",user="ljs392",n.add=60, wall.hours = 300,
#              GB = 3)
# 
# run.grid.dmc("samples_fixedB",model.dir ="LBA", 
#              model.file="lbaN_B.R",user="ljs392",n.add=60, wall.hours = 300,
#              GB = 3)
# 
# run.grid.dmc("samples_fixed_day_V",model.dir ="LBA", 
#              model.file="lbaN_B.R",user="ljs392",n.add=60, wall.hours = 300,
#              GB = 3)