# ------------------------------------------------ #
# ClimPACT2
# Sample script showing how climpact.loader is called.
# ------------------------------------------------ #
 
source("climpact2.r")

indices=c("fd")
minfilename = "file.with.tmin.nc"
maxfilename = "file.with.tmax.nc"
precfilename = "file.with.precip.nc"

climpact.loader(
  precfile=precfilename,
  tsminfile=minfilename,
  tsmaxfile=maxfilename,
  precname="pracc_fl",
  tsminname="tasmin",
  tsmaxname="tasmax",
  tempqtiles=c(0.1,0.9,0.95),
  precqtiles=c(0.1,0.9,0.95,0.99),
  write_quantiles=FALSE,
  rxnday_n=7,
  ntxntn_n=6,
  ntxbntnb_n=6,
  wsdin_n=6,
  indices=indices,
  baserange=c(1961,1990),
  #time_format="%Y%m%d",
  #cores=4,
  identifier="ClimPACT2_test")
