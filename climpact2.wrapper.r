# ------------------------------------------------ #
# ClimPACT2
# Sample script showing how climpact.loader is called.
# ------------------------------------------------ #
 
source("climpact2.r")

indices=c("all")
minfilename = "climpact2.sampledata.gridded.1991-2010.nc"	# file with tmin data
maxfilename = "climpact2.sampledata.gridded.1991-2010.nc"	# file with tmax data
precfilename = "climpact2.sampledata.gridded.1991-2010.nc"	# file with precip data

climpact.loader(
  precfile=precfilename,
  tsminfile=minfilename,
  tsmaxfile=maxfilename,
  precname="precip",
  tsminname="tmin",
  tsmaxname="tmax",
  tempqtiles=c(0.1,0.5,0.9,0.95),
  precqtiles=c(0.1,0.9,0.95,0.99),
  write_quantiles=FALSE,
  rxnday_n=7,
  ntxntn_n=6,
  ntxbntnb_n=6,
  wsdin_n=6,
  indices=indices,
  baserange=c(1991,2000),
  #time_format="%Y%m%d",
  #cores=4,
  identifier="ClimPACT2_test")
