# Testing template for climpact.loader
source("climpact2_0.3.r")

indices=c("tx95t")
minfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_1950-2009_tasmin.nc"
maxfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_1950-2009_tasmax.nc"
precfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_1950-2009_pracc_fl.nc"

climpact.loader(
precfile=precfilename,
tsminfile=minfilename,
tsmaxfile=maxfilename,
precname="pracc_fl",
tsminname="tasmin",
tsmaxname="tasmax",
indices=indices,
baserange=c(2000,2001),
identifier="NARCliM_test")
