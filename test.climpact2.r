# Testing template for climpact.loader
source("climpact2_0.3.r")

indices=c("tx95t")
minfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_1950-2009_tasmin.reduced.nc"
maxfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_1950-2009_tasmax.reduced.nc"
precfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_1950-2009_pracc_fl.reduced.nc"

#startss()
#q()

climpact.loader(
precfile=precfilename,
tsminfile=minfilename,
tsmaxfile=maxfilename,
precname="pracc_fl",
tsminname="tasmin",
tsmaxname="tasmax",
tempqtiles=c(0.1,0.6,0.9,0.95),
precqtiles=c(0.1,0.9,0.95,0.99),
write_quantiles=FALSE,
#quantile_file = "CCRC_NARCliM-test_DAY_1961_1990_quantiles.nc",
ntxntn_consecdays=7,
ntxbntnb_consecdays=7,
rxnday_n=7,
wsdin_n=7,
indices=indices,
baserange=c(1961,1990),
#time_format="%Y%m%d",
identifier="NARCliM-test")
