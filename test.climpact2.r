# Testing template for climpact.loader
source("climpact2_0.2.r")

indices=c("tm5a","tx95t","rxnday")
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
tempqtiles=c(0.1,0.5,0.9,0.95),
ntxntn_consecdays=8,
rxnday_n=7,
indices=indices,
baserange=c(1961,1990),
#time_format="%Y%m%d",
identifier="NARCliM_test")
