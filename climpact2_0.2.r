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
library(SPEI)

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
freq=c("monthly","annual"),quantiles=NULL,tempqtiles=c(0.1,0.9),precqtiles=c(0.1,0.9),max.missing.days=c(annual=15, monthly=3),min.base.data.fraction.present=0.1,northern.hemisphere=TRUE,csdin_n=5,wsdin_n=5,
cdd_spells.can.span.years=TRUE,cwd_spells.can.span.years=TRUE,csdin_spells.can.span.years=FALSE,wsdin_spells.can.span.years=FALSE,csdi_spells.can.span.years=FALSE,wsdi_spells.can.span.years=FALSE,ntxntn_spells.can.span.years=FALSE,
gslmode=c("GSL", "GSL_first", "GSL_max", "GSL_sum"),rx5day_centermean=FALSE,ntxntn_consecdays=5,hddheat_n=18,time_format=NULL,rxnday_n=5,rxnday_center.mean.on.last.day=FALSE,spei_n=1,hwn_min.base.data.fraction.present=0.1,hwn_n=5)
{
# Initial checks
# 1) at least one file is provided,
# 2) at least one index is provided, 
# 3) that all indices are valid,
# TO ADD: if a tsmax index is specified but tsmax file is not. BOMB.
# TO ADD: FILES EXIST!! This should be first thing!
	indexlist <- readLines("./index.master.list")
	if(all(is.null(tsminfile),is.null(tsmaxfile),is.null(precfile))) stop("Must provide at least one filename for tsmin, tsmax and/or prec.")
	if(is.null(indices)) stop("Must provide a list of indices to calculate. See XX for list.")
        if(any(!indices %in% indexlist)) stop(paste("One or more indices"," are unknown. See XX for list.",sep=""))

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
                lon2d = ncvar_get(refnc,lonname)
                lat2d = ncvar_get(refnc,latname)
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
			cdd={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",cdd_spells.can.span.years,"))",sep="")},
			csdi={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",csdi_spells.can.span.years,"))",sep="")},
			cwd={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",cwd_spells.can.span.years,"))",sep="")},
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
			wsdi={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",wsdi_spells.can.span.years,"))",sep="")},
                        csdin={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",csdin_spells.can.span.years,",n=",csdin_n,"))",sep="")},
                        wsdin={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",wsdin_spells.can.span.years,",n=",wsdin_n,"))",sep="")},
                        ntxntn={indexparam = paste("array(indexcompile(",indexparam,",consecdays=",ntxntn_consecdays,",spells.can.span.years=",ntxntn_spells.can.span.years,"))",sep="")},
			tx95t={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},

                        rxnday={indexparam = paste("array(indexcompile(",indexparam,",center.mean.on.last.day=",rxnday_center.mean.on.last.day,",n=",rxnday_n,"))",sep="")},spei={indexparam = paste("array(indexcompile(",indexparam,",n=",spei_n,"))",sep="")},
			hwn={
				# find corrected call for retrieving lat
				if (irregular) { latstr="lat2d[j,i]" } else { latstr="lat[j]" }
				indexparam = paste("array(indexcompile(",indexparam,",base.range=c(",baserange[1],",",baserange[2],"),n=",hwn_n,",min.base.data.fraction.present=",
				hwn_min.base.data.fraction.present,",lat=",latstr,"))",sep="")},

		{ indexparam = paste("array(indexcompile(",indexparam,"))",sep="")} )
		print(eval(indexparam))

	# Determine whether index to be calculated will be daily (currently only for tx95t), monthly or annual
		if(indices[a] == "tx95t") {period = "365days"} else if (indices[a] == "rxnday") { period = "monthly" } else {
	                if(!is.null(formals(indexfun)$freq)) {
				if(!is.null(freq)){
					if(freq[1] == "monthly") {period = "monthly"} else {period = "annual"}
				} else {period = "monthly"}
			} else {period = "annual"}
		}

	# Create empty array depending on whether index is monthly or annual
		if(period == "monthly") {index = array(NA,c(length(lon),length(lat),nmonths))} else if(period == "annual") { index = array(NA,c(length(lon),length(lat),nyears))} else {index = array(NA,c(length(lon),length(lat),365))}

	# Calculate the index
		for(j in 1:length(lat)){
			for(i in 1:length(lon)){
				cio = cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=prec[i,j,],tmin.dates=tsmintime,tmax.dates=tsmaxtime,prec.dates=prectime,prec.qtiles=precqtiles,
					temp.qtiles=tempqtiles,quantiles=quantiles,base.range=baserange)
				index[i,j,] = eval(parse(text=indexparam))

#	                        index[i,j,] = array(indexcompile(ciarray[[i,j]]))
			}
		}
print(length(index[1,1,]))
	# Transpose dimensions to time,lat,lon
	        index3d_trans = aperm(index,c(1,2,3))#(2,3,1))

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
                        ncvar_put(tmpout,indexcdf,index)
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
		rm(timedim,indexcdf,tmpout,outfile,index3d_trans)

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

	# Bit of a hack for non-model datasets. Requires user to specify "time_format" in climpact.loader
	if(!is.null(time_format)) {
		string = (apply(ftime,1,toString))
		dates = (as.Date(string,time_format))
		rm(ftime) ; ftime = array(1,length(dates)) ; ftime = (as.character(dates))

		split = substring(time_format, seq(1,nchar(time_format),2), seq(2,nchar(time_format),2))
		time_format = paste(split[1],split[2],split[3],sep="-")
	        return(as.PCICt(ftime,cal="gregorian",format=time_format))
	} else {
	        if(grepl("hours",time_att)) {print("Time coordinate in hours, converting to seconds...") ; ftime = ftime*60*60}
        	if(grepl("days",time_att)) {print("Time coordinate in days, converting to seconds...") ; ftime = ftime*24*60*60}

		return(as.PCICt(ftime,cal="gregorian",origin=get.origin(time_att=time_att[[1]]),format=time_format))
	}
}

# A universal wrapper function for the climdex (and climpact?) functions. This is needed simply to pass a ci object to the index functions, instead of a list object.
#index.wrap <- function(ci=NULL,index=NULL,freq=NULL)
#{
#	indexfun = match.fun(paste("climdex",index,sep="."))
#	return(indexfun(ci))
#}

##############
# NEW CLIMPACT INDICES THAT SHOULD WORK
##############

# fd2
# Annual count when TN < 2ºC
# same as climdex.fd except < 2
climdex.fd2 <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 2, "<") * ci@namasks$annual$tmin) }

# fdm2
# Annual count when TN < -2ºC
# same as climdex.fd except < -2
climdex.fdm2 <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, -2, "<") * ci@namasks$annual$tmin) }

# fdm20
# Annual count when TN < -20ºC
# same as climdex.fd except < -20
climdex.fdm20 <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, -20, "<") * ci@namasks$annual$tmin) }

# wsdin
# Annual count of days with at least n consecutive days when TX>90th percentile where n>= 2 (and max 10)
# same as climdex.wsdi except user specifies number of consecutive days
climdex.wsdin <- function(ci, spells.can.span.years=FALSE,n=5) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(threshold.exceedance.duration.index(ci@data$tmax, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q90, ">", spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual'], min.length=n) * ci@namasks$annual$tmax) }

# csdin
# Annual count of days with at least n consecutive days when TN<10th percentile where n>= 2 (and max 10)
# same as climdex.csdi except user specifies number of consecutive days
climdex.csdin <- function(ci, spells.can.span.years=FALSE,n=5) { stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); return(threshold.exceedance.duration.index(ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmin$outbase$q10, "<", spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual'], min.length=n) * ci@namasks$annual$tmin) }

# tm5a
# Annual count when TM >= 5ºC
# same as climdex.tr except >= 5C
climdex.tm5a <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 5, ">=") * ci@namasks$annual$tmin) }

# tm5b
# Annual count when TM < 5ºC
# same as climdex.tr except < 5C
climdex.tm5b <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 5, "<") * ci@namasks$annual$tmin) }

# tm10a
# Annual count when TM >= 10ºC
# same as climdex.tr except >=10C
climdex.tm10a <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 10, ">=") * ci@namasks$annual$tmin) }

# tm10b
# Annual count when TM < 10ºC
# same as climdex.tr except <10C
climdex.tm10b <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 10, "<") * ci@namasks$annual$tmin) }

# su30
# Annual count when TX >= 30ºC
# same as climdex.tr except >=30C
climdex.su30 <- function(ci) { stopifnot(!is.null(ci@data$tmax)); return(number.days.op.threshold(ci@data$tmax, ci@date.factors$annual, 30, ">=") * ci@namasks$annual$tmax) }

# su35
# Annual count when TX > = 35ºC
# same as climdex.tr except >=35C
climdex.su35 <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 35, ">=") * ci@namasks$annual$tmin) }

# HDDheat
# Annual sum of Tb-TM (where Tb is a user-defined location-specific base temperature and TM < Tb)
climdex.hddheat <- function(ci,Tb=18) { Tbarr = array(Tb,length(ci@data$tavg)); stopifnot(is.numeric(ci@data$tavg),is.numeric(Tb),any(ci@data$tavg < Tbarr)) ;return(tapply.fast(Tbarr - ci@data$tavg,ci@date.factors$annual,sum))*ci@namasks$annual }

# CDDcold
# Annual sum of TM-Tb (where Tb is a user-defined location-specific base temperature and TM > Tb)
climdex.cddcold <- function(ci,Tb=18) { Tbarr = array(Tb,length(ci@data$tavg)); stopifnot(is.numeric(ci@data$tavg),is.numeric(Tb),any(ci@data$tavg > Tbarr)) ;return(tapply.fast(ci@data$tavg - Tbarr,ci@date.factors$annual,sum))*ci@namasks$annual }

# GDDgrow
# Annual sum of TM-Tb (where Tb is a user-defined location-specific base temperature and TM >Tb)
climdex.gddgrow <- function(ci,Tb=10) { Tbarr = array(Tb,length(ci@data$tavg)); stopifnot(is.numeric(ci@data$tavg),is.numeric(Tb),any(ci@data$tavg > Tbarr)) ;return(tapply.fast(ci@data$tavg - Tbarr,ci@date.factors$annual,sum))*ci@namasks$annual }

# Rxnday
# Monthly maximum consecutive n-day precipitation (up to a maximum of 10)
# Same as rx5day except specifying a monthly frequency and accepting user specified number of consecutive days
climdex.rxnday <- function(ci, center.mean.on.last.day=FALSE,n=5) { stopifnot(!is.null(ci@data$prec)); return(nday.consec.prec.max(ci@data$prec, ci@date.factors$monthly, n, center.mean.on.last.day) * ci@namasks$monthly$prec) }

##############
# NEW CLIMPACT INDICES THAT HOPEFULLY WORK
##############

# tx50p
# Percentage of days of days where TX>50th percentile
# same as climdex.tx90p, except for 50th percentile
#    NOT SURE ABOUT IMPLEMENTATION. HOW ARE QUANTILES HANDLED? Should be able to submit 'temp.qtiles=c(0.5)' to climdexInput.raw?
#    UPDATE::: changed "qtiles=c(0.10,0.90)" to "qtiles" in get.temp.var.quantiles. This should allow the code to handle any specified percentiles.
#          ::: contd. Not sure why this was hard coded.
#          ::: contd. Contacted James Hiebert who maintains climdex and he agrees it's a bug, will be fixed in a future CRAN release.
climdex.tx50p <- function(ci, freq=c("monthly", "annual")) {
stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(percent.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q50, ci@quantiles$tmax$inbase$q50, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax)
}

# tx95t
# Value of 95th percentile of TX
##### TO CHECK/FIX ##### Need to understand how percentiles are calculated inside the base range. Currently this function reports the out of base 95th percentile (which I interpret to be what the index defines anyway). 
######################## In climdex.pcic this has dimensions (365,nyears,nyears-1), not sure why.
climdex.tx95t <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(ci@quantiles$tmax$outbase$q95) }

# ntxntn
# Annual count of n consecutive days where both TX > 95th percentile and TN > 95th percentile, where n >= 2 (and max of 10)
# This function needs the new function dual.threshold.exceedance.duration.index, which was based on threshold.exceedance.duration.index
climdex.ntxntn <- function(ci, spells.can.span.years=FALSE,consecdays=5) { 
	stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax) || (!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)))
	return(dual.threshold.exceedance.duration.index(ci@data$tmax, ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q95,ci@quantiles$tmin$outbase$q95, 
		">",">", consecdays=consecdays,spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual']) * ci@namasks$annual$tmax) }

# ntxbntnb
# Annual count of n consecutive days where both TX < 5th percentile and TN < 5th percentile, where n >= 2 (and max of 10)
# This function needs the new function dual.threshold.exceedance.duration.index, which was based on threshold.exceedance.duration.index
climdex.ntxbntnb <- function(ci, spells.can.span.years=FALSE,consecdays=5) {
        stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax) || (!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)))
        return(dual.threshold.exceedance.duration.index(ci@data$tmax, ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q5,ci@quantiles$tmin$outbase$q5,
                "<","<", consecdays=consecdays,spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual']) * ci@namasks$annual$tmax) }

# dual.threshold.exceedance.duration.index
# calculates the number of consecdays consecutive days where op1 and op2 operating on daily.temp1 and daily.temp2 respectively are satisfied.
dual.threshold.exceedance.duration.index <- function(daily.temp1, daily.temp2, date.factor, jdays, thresholds1, thresholds2, op1=">", op2=">", consecdays, spells.can.span.years, max.missing.days) {
  stopifnot(is.numeric(c(daily.temp1,daily.temp2, thresholds1,thresholds2, consecdays)), is.factor(date.factor),
            is.function(match.fun(op1)),is.function(match.fun(op2)),
            consecdays > 0,length(daily.temp1)==length(daily.temp2))
  f1 <- match.fun(op1)
  f2 <- match.fun(op2)

  na.mask1 <- get.na.mask(is.na(daily.temp1 + thresholds1[jdays]), date.factor, max.missing.days)
  na.mask2 <- get.na.mask(is.na(daily.temp2 + thresholds2[jdays]), date.factor, max.missing.days)
  na.mask_combined = na.mask1 & na.mask2

  if(spells.can.span.years) {
    periods1 <- f1(daily.temp1, thresholds1[jdays])	#select.blocks.gt.length(f1(daily.temp1, thresholds1[jdays]), min.length1 - 1)
    periods2 <- f2(daily.temp2, thresholds2[jdays])	#select.blocks.gt.length(f2(daily.temp2, thresholds2[jdays]), min.length2 - 1)
    periods_combined = select.blocks.gt.length(periods1 & periods2,consecdays)	# an array of booleans
    return(tapply.fast(periods_combined, date.factor, sum) * na.mask_combined)
  } else {
    return(tapply.fast(1:length(daily.temp1), date.factor, function(idx) { 
	periods1 = f1(daily.temp1[idx], thresholds1[jdays[idx]])	#select.blocks.gt.length(f1(daily.temp1[idx], thresholds1[jdays[idx]]), min.length1 - 1)#   * na.mask1
	periods2 = f2(daily.temp2[idx], thresholds2[jdays[idx]])	#select.blocks.gt.length(f2(daily.temp2[idx], thresholds2[jdays[idx]]), min.length2 - 1)#   * na.mask2
	periods_combined = select.blocks.gt.length(periods1 & periods2,consecdays)
	return(sum(periods_combined)) })*na.mask_combined)
  }
}

# SPEI
# Measure of “drought” using the Standardised Precipitation Evapotranspiration Index on time scales of 3, 6 and 12 months. No missing data are allowed to calculate SPEIflex. 
# Attempting to use the R package SPEI for the SPEI and SPI indices.
climdex.spei <- function(ci,n=6) { stopifnot(is.numeric(ci@data$prec),is.numeric(n),n>0) ; 
	ci@data$prec[length(ci@data$prec)]=ci@data$prec[length(ci@data$prec)-1] ; print(sum(is.na(ci@data$prec)));print((spei(ci@data$prec,n))$fitted) 
print(str(ci))
q()
	return(spei(ci@data$prec,n))
}

# hwn
# 
# Heat wave indices
climdex.hwn <- function(ci,base.range=c(1961,1990),n,min.base.data.fraction.present,lat) {
	stopifnot(!is.null(lat))

# step 1. Get 90th percentiles. Try using climdex's get.outofbase.quantiles function for this.
	print(base.range)
	min_max_quantiles <- get.outofbase.quantiles(ci@data$tmax,ci@data$tmin,ci@data$prec,tmax.dates=ci@dates,tmin.dates=ci@dates,prec.dates=ci@dates,base.range=base.range,n=n,temp.qtiles=0.9,prec.qtiles=0.9,
							min.base.data.fraction.present=min.base.data.fraction.present)
	mean_quantiles <- get.outofbase.quantiles(ci@data$tavg,ci@data$tmin,ci@data$prec,tmax.dates=ci@dates,tmin.dates=ci@dates,prec.dates=ci@dates,base.range=base.range,n=n,temp.qtiles=0.9,prec.qtiles=0.9,
							min.base.data.fraction.present=min.base.data.fraction.present)
	print(str(min_max_quantiles))
	print(str(mean_quantiles))
print(lat)
q()

# step 2. Determine if "conditions" (ambiguous word used in climpact manual) have persisted for >= 3 days. If so, count number of summer heat waves.
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

