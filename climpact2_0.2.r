# Climpact2
# University of New South Wales
#
# Climpact2 combines the CRAN package climdex.pcic, developed by the Pacific Climate Impacts Consortium, with the Climpact code, developed at UNSW by XXXX. 
# Science users are intended to access the indices through the climpact.loader function while non-specialists will be able to use the GUI.
# 
# nherold, 2015.

library("climdex.pcic")
library("ncdf4")
library("PCICt")
library(compiler)
options(warn=1)

# climpact.loader
#
# This function reads in provided 3 dimensional netCDF files and calculates specified indices
#
# INPUT
#  tsminfile: min temperature file
#  tsmaxfile: max temperature file
#  precfile: daily precipitation file
#  tsminname: name of min temperature variable in tsminfile
#  tsmaxname: name of max temperature variable in tsmaxfile
#  precname: name of daily precipitation variable in precfile
#  indices: a list specifying which indices to calculate. See XX for a list of supported indices.
#  identifier: an optional string to aid identifying output files (e.g. this may be the particular model the indices are being calculated on).
#  lonname: name of longitude dimension.
#  latname: name of latitude dimension.
#  baserange: year range that will be used for percentile calculations.
#  freq: frequency at which to calculate indices (only applies to certain indices, can be either "annual" or "monthly").
#  quantiles: quantiles that have been pre-computed.
#  tempqtiles: temperature percentiles to calculate.
#  precqtiles: precipitation percentiles to calculate.
#  max.missing.days: maximum missing days under which indices will still be calculated.
#  min.base.data.fraction.present: 
#  northern.hemisphere:
#  usern_csdin: user specified number of days for CSDIn.
#  usern_wsdin: user specified number of days for WSDIn.
#
# OUTPUT
#  A single netCDF file for each index specified in indices.
#
climpact.loader <- function(tsminfile=NULL,tsmaxfile=NULL,precfile=NULL,tsminname="tsmin",tsmaxname="tsmax",precname="prec",timename="time",indices=NULL,identifier=NULL,lonname="lon",latname="lat",baserange=c(1961,1990),
freq=c("monthly","annual"),quantiles=NULL,tempqtiles=c(0.1,0.9),precqtiles=c(0.1,0.9),max.missing.days=c(annual=15, monthly=3),min.base.data.fraction.present=0.1,northern.hemisphere=TRUE,usern_csdin=NULL,usern_wsdin=NULL,
canspanyears=FALSE,gslmode=c("GSL", "GSL_first", "GSL_max", "GSL_sum"),rx5day_centermean=FALSE,usern_ntxntn=6,usern_hddheat=18,time_format)
{
# Initial checks
# 1) at least one file is provided,
# 2) at least one index is provided, 
# 3) that all indices are valid,
# 4) that if csdin/wsdin are specified then the corresponding n values are too,
# TO ADD: if a tsmax index is specified but tsmax file is not. BOMB.
# TO ADD: FILES EXIST!! This should be first thing!
	indexlist <- readLines("./index.master.list")
	if(all(is.null(tsminfile),is.null(tsmaxfile),is.null(precfile))) stop("Must provide at least one filename for tsmin, tsmax and/or prec.")
	if(is.null(indices)) stop("Must provide a list of indices to calculate. See XX for list.")
        if(any(!indices %in% indexlist)) stop(paste("One or more indices"," are unknown. See XX for list.",sep=""))
        if("csdin" %in% indices && is.null(usern_csdin)) stop("csdin requested but usern_csdin variable not passed to climpact.loader")
        if("wsdin" %in% indices && is.null(usern_wsdin)) stop("wsdin requested but usern_wsdin variable not passed to climpact.loader")

# Set constants
	cal <- "gregorian"
	tsmin <- tsmax <- prec <- NULL
	tsmintime <- tsmaxtime <- prectime <- NULL

# Load files and variables. Assumedly this is a memory intensive step for large variables. Way to improve this? Read incrementally?
        if(!is.null(tsminfile)) { nc_tsmin=nc_open(tsminfile); tsmin <- ncvar_get(nc_tsmin,tsminname) ; refnc=nc_tsmin}
        if(!is.null(tsmaxfile)) { nc_tsmax=nc_open(tsmaxfile); tsmax <- ncvar_get(nc_tsmax,tsmaxname) ; refnc=nc_tsmax}
        if(!is.null(precfile)) { nc_prec=nc_open(precfile); prec <- ncvar_get(nc_prec,precname) ; refnc=nc_prec}

# Convert to Celcius
	if(exists("nc_tsmin")) if (ncatt_get(nc_tsmin,tsminname,"units")[2] == "K") tsmin = tsmin-273.15
        if(exists("nc_tsmax")) if (ncatt_get(nc_tsmax,tsmaxname,"units")[2] == "K") tsmax = tsmax-273.15

# Set up coordinate variables for writing to netCDF. If irregular grid then create x/y indices, if regular grid read in lat/lon coordinates.
	if(length(dim(ncvar_get(refnc,latname))) > 1) {irregular = TRUE} else {irregular = FALSE}	# determine if irregular

	if(irregular){					# If an irregular grid is being used
	        lat = 1:dim(ncvar_get(refnc,latname))[2]
        	lon = 1:dim(ncvar_get(refnc,lonname))[1]
                londim <- ncdim_def("x", "degrees_east", as.double(lon))	# creates object of class ncdim4!
                latdim <- ncdim_def("y", "degrees_north", as.double(lat))
	} else{						# else regular grid
                lat = ncvar_get(refnc,latname)
                lon = ncvar_get(refnc,lonname)
                londim <- ncdim_def("lon", "degrees_east",lon)
                latdim <- ncdim_def("lat", "degrees_north",lat)
	}

# Get the time
        time = get.time(refnc,timename,time_format) #ncvar_get(refnc,"time") ; print(time)
        yeardate = unique(format(time,format="%Y"))
        nmonths = length(yeardate)*12
        nyears = length(yeardate)

	if(!is.null(tsminfile)) { tsmintime = time }; if(!is.null(tsmaxfile)) { tsmaxtime = time }; if(!is.null(precfile)) { prectime = time }

# Compile climdexinput function for performance. Then create a 2D array of climdex input objects.
# This is memory intensive (uses ~70% of memory on a storm server - sometimes bombs) but should be quicker than calculating input objects repeatedly for each index... needs testing
	cicompile <- cmpfun(climdexInput.raw)
#	ciarray = array(list(),c(length(lon),length(lat)))
#
#	for(j in 1:length(lat)){
#		for(i in 1:length(lon)){
#			ciarray[[i,j]] = cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=prec[i,j,],tmin.dates=tsmintime,tmax.dates=tsmaxtime,prec.dates=prectime,prec.qtiles=precqtiles,temp.qtiles=tempqtiles,quantiles=quantiles,base.range=baserange)
#		}
#	}

# Loop through index list; get index, read variable, calculate index on grid, write out index on gridded netcdf
        print("********* CALCULATING INDICES")
        for(a in 1:length(indices)){
	# Fetch and compile index function
	        indexfun = match.fun(paste("climdex",indices[a],sep="."))
		indexcompile = cmpfun(indexfun)

	# Create index call string (better way to do this? Ability to use |?)
		options(useFancyQuotes=FALSE)
		indexparam = "cio"
		switch(indices[a],
			cdd={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",canspanyears,"))",sep="")},
			csdin={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",canspanyears,"))",sep="")},
			cwd={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",canspanyears,"))",sep="")},
	                dtr={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
			gsl.mode={indexparam = paste("array(indexcompile(",indexparam,",gsl.mode=",dQuote(gslmode),"))",sep="")},
			rnnmm={indexparam = paste("array(indexcompile(",indexparam,",threshold=",rnnm_threshold,"))",sep="")},
			rx1day={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
	                rx5day={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),",center.mean.on.last.day=",rx5day_centermean,"))",sep="")},
	                tn10p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
			tn90p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
			tnn={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
			tnx={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
			tx10p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
			tx90p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
			txn={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
			txx={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},
			wsdi={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",canspanyears,"))",sep="")},
                        ntxntn={indexparam = paste("array(indexcompile(",indexparam,",usern_ntxntn=",usern_ntxntn,"))",sep="")},
			tx95t={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},

		{ indexparam = paste("array(indexcompile(",indexparam,"))",sep="")} )
		print(eval(indexparam))

	# Determine whether index to be calculated will be daily (currently only for tx95t), monthly or annual
		if(indices[a] == "tx95t") {period = "365days"} else {
	                if(!is.null(formals(indexfun)$freq)) {
				if(!is.null(freq)){
					if(freq[1] == "monthly") {period = "monthly"} else {period = "annual"}
				} else {period = "monthly"}
			} else {period = "annual"}
		}

	# Create empty array depending on whether index is monthly or annual
		if(period == "monthly") {index = array(NA,c(length(lon),length(lat),nmonths))} else if(period == "annual") { index = array(NA,c(length(lon),length(lat),nyears))} else {index = array(NA,c(length(lon),length(lat),365))}

		for(j in 1:length(lat)){
			for(i in 1:length(lon)){
			cio = cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=prec[i,j,],tmin.dates=tsmintime,tmax.dates=tsmaxtime,prec.dates=prectime,prec.qtiles=precqtiles,temp.qtiles=tempqtiles,quantiles=quantiles,base.range=baserange)
			index[i,j,] = eval(parse(text=indexparam)) #array(indexcompile(eval(parse(text=paste("cio",indexparam,sep=""))))) #(parse(text=indexparam))))

#                        index[i,j,] = array(indexcompile(ciarray[[i,j]]))
			}
		}

		index3d = index

	# Transpose dimensions to time,lat,lon
	        index3d_trans = aperm(index3d,c(2,3,1))

	# Write out
	# NOTE: ncdf4 seems to only support numeric types for dimensions.
		if(period == "monthly"){	# if monthly data
                        timedim <- ncdim_def("time",paste("months since ",yeardate[1],"-01-01",sep=""),0.5:(nmonths-0.5))
		} else if(period == "annual"){								# else assume annual data
	                timedim <- ncdim_def("time",paste("years since ",yeardate[1],"-01-01",sep=""),0.5:(nyears-0.5))
		} else {
			timedim <- ncdim_def("time","days since 0001-01-01",0.5:364.5)
		}

		outfile = paste(indices[a],"CLIMPACT",identifier,yeardate[1],yeardate[length(yeardate)],"nc",sep=".")
	        indexcdf <- ncvar_def(indices[a],"units",list(londim,latdim,timedim),-1,prec="float")
	        system(paste("rm -f ",outfile,sep=""))

                if(irregular){
			print("WORKING ON IRREGULAR GRID")
                        loncdf <- ncvar_def(lonname,"degrees_east",list(londim,latdim),-1,prec="float")
                        latcdf <- ncvar_def(latname,"degrees_north",list(londim,latdim),-1,prec="float")
	                tmpout = nc_create(outfile,list(indexcdf,loncdf,latcdf),force_v4=TRUE)
			lon2d = ncvar_get(refnc,lonname)
			lat2d = ncvar_get(refnc,latname)
                        ncvar_put(tmpout,indexcdf,index3d)
			ncvar_put(tmpout,loncdf,lon2d);ncvar_put(tmpout,latcdf,lat2d)
			ncatt_put(tmpout,indexcdf,"coordinates","lon lat")
			rm(loncdf,latcdf,lon2d,lat2d)
		}else{tmpout = nc_create(outfile,list(indexcdf),force_v4=TRUE);ncvar_put(tmpout,indexcdf,index3d_trans)}

	# metadata
        	ncatt_put(tmpout,0,"created_on",system("date",intern=TRUE))
	        ncatt_put(tmpout,0,"created_by",system("whoami",intern=TRUE))

	        nc_close(tmpout)
                if(irregular) {system(paste("module load nco; ncks -C -O -x -v x,y",outfile,outfile,sep=" "))}

	# Clean up for next iteration
		rm(timedim,indexcdf,tmpout,outfile,index3d,index3d_trans)

	# Report back
		print(paste(paste("climdex",indices[a],sep=".")," completed.",sep=""))
	}
}

# get.origin
#
# This function gets the origin date from a time variable's units attribute. Assumes the attribute is structured as "units since YYYY-MM-DD..."
get.origin <- function(time_att=NULL){ return(gsub(",", "", unlist(strsplit(time_att, split=" "))[3]))}

# get.time
#
# This function returns the time dimension of a given netcdf file as a PCICt object
#
# INPUT
#  nc: ncdf4 reference object
#  timename: name of time variable in nc
# OUTPUT
#  PCICT object
get.time <- function(nc=NULL,timename=NULL,time_format=NULL)
{
	ftime = ncvar_get(nc,timename)
	time_att = ncatt_get(nc,timename,"units")[2]

        if(grepl("hours",time_att)) {print("Time coordinate in hours, converting to seconds...") ;ftime = ftime*60*60}
        if(grepl("days",time_att)) {print("Time coordinate in days, converting to seconds...") ;ftime = ftime*24*60*60}

#        return(as.PCICt(ftime,cal="365_day",origin,format="%Y%m%d"))
	return(as.PCICt(ftime,cal="gregorian",origin=get.origin(time_att=time_att[[1]]),format=time_format))
}

# A universal wrapper function for the climdex (and climpact?) functions. This is needed simply to pass a ci object to the index functions, instead of a list object.
#index.wrap <- function(ci=NULL,index=NULL,freq=NULL)
#{
#	indexfun = match.fun(paste("climdex",index,sep="."))
#	return(indexfun(ci))
#}

##############
# NEW CLIMPACT FUNCTIONS THAT SHOULD WORK

climdex.fd2 <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 2, "<") * ci@namasks$annual$tmin) }

climdex.fdm2 <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, -2, "<") * ci@namasks$annual$tmin) }

climdex.fdm20 <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, -20, "<") * ci@namasks$annual$tmin) }

# same as climdex.wsdi except user specifies min.length (which is passed as an additional parameter 'usern_wsdin').
climdex.wsdin <- function(ci, spells.can.span.years=FALSE,usern_wsdin) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(threshold.exceedance.duration.index(ci@data$tmax, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q90, ">", spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual'], min.length=usern_wsdin) * ci@namasks$annual$tmax) }

# same as climdex.csdi except user specifies min.length (which is passed as an additional parameter 'usern_csdin').
climdex.csdin <- function(ci, spells.can.span.years=FALSE,usern_csdin) { stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); return(threshold.exceedance.duration.index(ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmin$outbase$q10, "<", spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual'], min.length=usern_csdin) * ci@namasks$annual$tmin) }

# same as climdex.tr except >= 5C
climdex.tm5a <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 5, ">=") * ci@namasks$annual$tmin) }

# same as climdex.tr except < 5C
climdex.tm5b <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 5, "<") * ci@namasks$annual$tmin) }

# same as climdex.tr except >=10C
climdex.tm10a <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 10, ">=") * ci@namasks$annual$tmin) }

# same as climdex.tr except <10C
climdex.tm10b <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 10, "<") * ci@namasks$annual$tmin) }

# same as climdex.tr except >=30C
climdex.su30 <- function(ci) { stopifnot(!is.null(ci@data$tmax)); return(number.days.op.threshold(ci@data$tmax, ci@date.factors$annual, 30, ">=") * ci@namasks$annual$tmax) }

# same as climdex.tr except >=35C
climdex.su35 <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 35, ">=") * ci@namasks$annual$tmin) }

##############
# NEW CLIMPACT FUNCTIONS THAT HOPEFULLY WORK

# same as climdex.tx90p, except for 50th percentile. 
# NOT SURE ABOUT IMPLEMENTATION. HOW ARE QUANTILES HANDLED? Should be able to submit 'temp.qtiles=c(0.5)' to climdexInput.raw?
# UPDATE::: changed "qtiles=c(0.10,0.90)" to "qtiles" in get.temp.var.quantiles. This should allow the code to handle any specified percentiles.
#       ::: contd. Not sure why this was hard coded.
#       ::: contd. Contacted James Hiebert who maintains climdex and he agrees it's a bug, will be fixed in a future CRAN release.
climdex.tx50p <- function(ci, freq=c("monthly", "annual")) {
stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(percent.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q50, ci@quantiles$tmax$inbase$q50, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax)
}

##### TO CHECK/FIX ##### Need to understand how percentiles are calculated inside the base range. Currently this function reports the out of base 95th percentile (which I interpret to be what the index defines anyway). 
######################## In climdex.pcic this has dimensions (365,nyears,nyears-1), not sure why.
climdex.tx95t <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(ci@quantiles$tmax$outbase$q95) }

# A bastardised form of wsdi.
# This function needs the new function dual.threshold.exceedance.duration.index, which was based on threshold.exceedance.duration.index
climdex.ntxntn <- function(ci, spells.can.span.years=FALSE,usern_ntxntn) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax) || (!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin))); 
return(dual.threshold.exceedance.duration.index(ci@data$tmax, ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q95,ci@quantiles$tmin$outbase$q95, ">",">", min.length1=usern_ntxntn,min.length2=usern_ntxntn,spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual']) * ci@namasks$annual$tmax) }

# Needed by climdex.ntxntn
# This was modified from the climdex function threshold.exceedance.duration.index to calculate the number of min.lengthn consecutive days where 
# daily.temp1 is op1 thresholds1 and daily.temp2 is op2 thresholds2.
# NOTE: Currently this may be calculated incorrectly - there should only be one min.length parameter...
dual.threshold.exceedance.duration.index <- function(daily.temp1, daily.temp2, date.factor, jdays, thresholds1, thresholds2, op1=">", op2=">", min.length1=6, min.length2=6, spells.can.span.years=TRUE, max.missing.days) {
  stopifnot(is.numeric(c(daily.temp1,daily.temp2, thresholds1,thresholds2, min.length1,min.length2)), is.factor(date.factor),
            is.function(match.fun(op1)),is.function(match.fun(op2)),
            min.length1 > 0,min.length2 > 0)
  f1 <- match.fun(op1)
  f2 <- match.fun(op2)

  na.mask1 <- get.na.mask(is.na(daily.temp1 + thresholds1[jdays]), date.factor, max.missing.days)
  na.mask2 <- get.na.mask(is.na(daily.temp2 + thresholds2[jdays]), date.factor, max.missing.days)
  na.mask_combined = na.mask1 & na.mask2

  if(spells.can.span.years) {
    periods1 <- select.blocks.gt.length(f1(daily.temp1, thresholds1[jdays]), min.length1 - 1)
    periods2 <- select.blocks.gt.length(f2(daily.temp2, thresholds2[jdays]), min.length2 - 1)
    periods_combined = periods1 & periods2	# an array of booleans
    return(tapply.fast(periods_combined, date.factor, sum) * na.mask_combined)
  } else {
    return(tapply.fast(1:length(daily.temp1), date.factor, function(idx) { 
	periods1 = select.blocks.gt.length(f1(daily.temp1[idx], thresholds1[jdays[idx]]), min.length1 - 1)#   * na.mask1
	periods2 = select.blocks.gt.length(f2(daily.temp2[idx], thresholds2[jdays[idx]]), min.length2 - 1)#   * na.mask2
	periods_combined = periods1 & periods2
	return(sum(periods_combined)) })*na.mask_combined)
  }
}

climdex.hddheat <- function(ci,Tb=18) {
        Tbarr = ci@data$tavg
        Tbarr = Tb
	stopifnot(is.numeric(ci@data$tavg),is.numeric(Tb),any(ci@data$tavg < Tbarr))
# need a tapply here
	return(sum(Tbarr - ci@data$tavg))
}



########################
## BEYOND HERE IS TEST CODE THAT DOES NOT INTERFERE WITH THE ABOVE


# unmodded
get.na.mask <- function(x, f, threshold) {
  return(c(1, NA)[1 + as.numeric(tapply.fast(is.na(x), f, function(y) { return(sum(y) > threshold) } ))])
}

# unmodded
tapply.fast <- function (X, INDEX, FUN = NULL, ..., simplify = TRUE) {
  FUN <- if (!is.null(FUN))
    match.fun(FUN)

  if (length(INDEX) != length(X))
    stop("arguments must have same length")

  if (is.null(FUN))
    return(INDEX)

  namelist <- levels(INDEX)
  ans <- lapply(split(X, INDEX), FUN, ...)

  ans <- unlist(ans, recursive = FALSE)
  names(ans) <- levels(INDEX)
  return(ans)
}

#' Select blocks of TRUE values of sufficient length.
#' 
#' Produces a sequence of booleans of the same length as input, with sequences
#' of TRUE values shorter than n replaced with FALSE.
#' 
#' This function takes a series of booleans and returns a sequence of booleans
#' of equal length, with all sequences of TRUE of length \code{n} or shorter
#' replaced with sequences of FALSE. NA values are replaced with
#' \code{na.value}.
#' 
#' @param d Sequence of booleans.
#' @param n Longest sequence of TRUE to replace with FALSE.
#' @param na.value Values to replace NAs with.
#' @return A vector of booleans, with the length \code{n} or less sequences of
#' TRUE replaced with FALSE.
dual.select.blocks.gt.length <- function(d1,d2,n1,n2, na.value=FALSE) {
  stopifnot(is.logical(d1),is.logical(d2),is.numeric(n1),is.numeric(n2))

  if(n1 < 1 && n2 < 1)
    return(d)

  if(n >= length(d1))
    return(rep(FALSE, length(d)))

  d1[is.na(d1)] <- na.value
  d2[is.na(d2)] <- na.value

print("dual.select.blocks.gt.length")
print(length(d1))
print(d1)

  d1a <- Reduce(function(x, y) { return(c(rep(FALSE, y), d1[1:(length(d1) - y)]) & x) }, 1:n, d1)
print(d1a)
  return(Reduce(function(x, y) { return(c(d1a[(y + 1):length(d1a)], rep(FALSE, y)) | x) }, 1:n, d1a))
}

