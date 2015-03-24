# Testing template for climpact.loader
source("climpact2_0.1.r")

indices=c("fdm2")
minfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_2000-2002_tasmin.nc"
maxfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_2000-2002_tasmax.nc"
precfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_2000-2002_pracc_fl.nc"

#minfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_1950-2009_tasmin.nc"
#maxfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_1950-2009_tasmax.nc"
#precfilename = "/srv/ccrc/data43/z3506872/datasets/sample_narclim/CCRC_NARCliM_DAY_1950-2009_pracc_fl.nc"	#"../data/NCEP1.precip.daily.2006-2009.native.reduced.nc"

climpact.loader(precfile=precfilename,
tsminfile=minfilename,
tsmaxfile=maxfilename,
precname="pracc_fl",
tsminname="tasmin",
tsmaxname="tasmax",
indices=indices,
freq="annual",
identifier="ci",
lonname="lon",
latname="lat",
quantiles=NULL,
#precqtiles=NULL,
#tempqtiles=NULL,
usern_csdin=NULL,
usern_wsdin=NULL,
baserange=c(2000,2001))
