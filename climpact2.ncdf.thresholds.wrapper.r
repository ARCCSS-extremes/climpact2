# ------------------------------------------------
# This wrapper script calls the 'create.thresholds.from.file' function from the modified climdex.pcic.ncdf package
# to create thresholds, using data and parameters provided by the user.
# ------------------------------------------------

library(climdex.pcic.ncdf)
# list of one to three input files. e.g. c("a.nc","b.nc","c.nc")
input.files = c("./sample_data/climpact2.sampledata.gridded.1991-2010.nc")

# list of variable names according to above file(s)
vars=c(tmax="tmax", tmin="tmin", prec="precip")

# output file name
output.file = "./output/thresholds.1991-1997.nc"

# author data
author.data=list(institution="My University", institution_id="MU")

# reference period
base.range=c(1991,1997)

# number of cores to use (or FALSE)
parallel = FALSE

# print messages?
verbose=TRUE



######################################
# Do not modify without a good reason.

fclimdex.compatible=FALSE

create.thresholds.from.file(input.files,output.file,author.data,variable.name.map=vars,base.range=base.range,parallel=parallel,verbose=verbose,fclimdex.compatible=fclimdex.compatible)
