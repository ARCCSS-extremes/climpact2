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
library(tcltk)

options(warn=1)
software_id = "0.2"

# climpact.loader
#
# This function reads in provided 3 dimensional netCDF files and calculates specified indices.
# No unit conversion is done except for Kelvin to Celcius.
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
ntxbntnb_spells.can.span.years=FALSE,ntxbntnb_consecdays=5,gslmode=c("GSL", "GSL_first", "GSL_max", "GSL_sum"),rx5day_centermean=FALSE,ntxntn_consecdays=5,hddheat_n=18,time_format=NULL,rxnday_n=5,rxnday_center.mean.on.last.day=FALSE,
spei_n=1,hwn_min.base.data.fraction.present=0.1,hwn_n=5)
{
# Initial checks
# 1) at least one file is provided,
# 2) at least one index is provided, 
# 3) that all indices are valid,
# TO ADD: if a tsmax index is specified but tsmax file is not. BOMB.
# TO ADD: FILES EXIST!! This should be first thing!
	indexfile = "./index.master.list"
	indexlist <- readLines(indexfile)
	if(all(is.null(tsminfile),is.null(tsmaxfile),is.null(precfile))) stop("Must provide at least one filename for tsmin, tsmax and/or prec.")
	if(is.null(indices)) stop(paste("Must provide a list of indices to calculate. See ",indexfile," for list.",sep=""))
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
        yeardate = unique(format(time,format="%Y"))		# get unique years
	monthdate = (unique(format(time,format="%Y-%m")))	# get unique year-month dates

	# for NARCliM we will use hours since origin. For other datasets (that may not have an origin) we use years or months since the first time step.
	if(is.null(time_format)) {
		time_att = ncatt_get(refnc,timename,"units")[2]
		origin=get.origin(time_att=time_att[[1]])
		months_as_hours = as.numeric(as.Date(paste(monthdate,"-01",sep="")) - as.Date(origin))*24
		years_as_hours = as.numeric(as.Date(paste(yeardate,"-01-01",sep="")) - as.Date(origin))*24
	        nmonths = length(months_as_hours)
        	nyears = length(years_as_hours)
	} else {
		nmonths = length(yeardate)*12
		nyears = length(yeardate)
	}

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
                        ntxbntnb={indexparam = paste("array(indexcompile(",indexparam,",consecdays=",ntxbntnb_consecdays,",spells.can.span.years=",ntxbntnb_spells.can.span.years,"))",sep="")},
			tx95t={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="")},

                        rxnday={indexparam = paste("array(indexcompile(",indexparam,",center.mean.on.last.day=",rxnday_center.mean.on.last.day,",n=",rxnday_n,"))",sep="")},spei={indexparam = paste("array(indexcompile(",indexparam,",n=",spei_n,"))",sep="")},
			hw={
				# find correct call for retrieving lat
				if (irregular) { latstr="lat2d[j,i]" } else { latstr="lat[j]" }
				indexparam = paste("array(indexcompile(",indexparam,",base.range=c(",baserange[1],",",baserange[2],"),n=",hwn_n,",min.base.data.fraction.present=",
				hwn_min.base.data.fraction.present,",lat=",latstr,"))",sep="")},

		{ indexparam = paste("array(indexcompile(",indexparam,"))",sep="")} )
		print(eval(indexparam))

	# Determine whether index to be calculated will be daily (currently only for tx95t), monthly or annual
		if(indices[a] == "tx95t") {period = "DAY"} else if (indices[a] == "rxnday") { period = "MON" } else {
	                if(!is.null(formals(indexfun)$freq)) {
				if(!is.null(freq)){
					if(freq[1] == "monthly") {period = "MON"} else {period = "ANN"}
				} else {period = "MON"}
			} else {period = "ANN"}
		}

	# Create empty array depending on whether index is monthly or annual
		if(period == "MON") {index = array(NA,c(length(lon),length(lat),nmonths))} else if(period == "ANN") { index = array(NA,c(length(lon),length(lat),nyears))} else {index = array(NA,c(length(lon),length(lat),365))}

	# Calculate the index
		for(j in 1:length(lat)){
			for(i in 1:length(lon)){
				cio = cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=prec[i,j,],tmin.dates=tsmintime,tmax.dates=tsmaxtime,prec.dates=prectime,prec.qtiles=precqtiles,
					temp.qtiles=tempqtiles,quantiles=quantiles,base.range=baserange)

				index[i,j,] = eval(parse(text=indexparam))
#	                        index[i,j,] = array(indexcompile(ciarray[[i,j]]))
			}
		}

	# Transpose dimensions to time,lat,lon
	        index3d_trans = aperm(index,c(1,2,3))#(2,3,1))

	# Write out
	# NOTE: ncdf4 seems to only support numeric types for dimensions.

	# Make time dimension according to time format
		if(is.null(time_format)) {	# IF no time format supplied work with hours since.
	                if(period == "MON") { timedim <- ncdim_def("time",paste("hours since ",origin,sep=""),months_as_hours) } 
			else if(period == "ANN") { timedim <- ncdim_def("time",paste("hours since ",origin,sep=""),years_as_hours) } 
			else { timedim <- ncdim_def("time","days since 0001-01-01",0.5:364.5) }
		} else {			# ELSE use number of months or years since first time step.
			if(period == "MON") { timedim <- ncdim_def("time",paste("months since ",yeardate[1],"-01-01",sep=""),0.5:(nmonths-0.5)) } 
			else if(period == "ANN"){ timedim <- ncdim_def("time",paste("years since ",yeardate[1],"-01-01",sep=""),0.5:(nyears-0.5)) } 
			else { timedim <- ncdim_def("time","days since 0001-01-01",0.5:364.5) }
		}

		outfile = paste(paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],indices[a],sep="_"),".nc",sep="")
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
			rm(loncdf,latcdf)
		}else{tmpout = nc_create(outfile,list(indexcdf),force_v4=TRUE);ncvar_put(tmpout,indexcdf,index3d_trans)}

	# metadata
        	ncatt_put(tmpout,0,"created_on",system("date",intern=TRUE))
	        ncatt_put(tmpout,0,"created_by",system("whoami",intern=TRUE))
                ncatt_put(tmpout,0,"software_version",software_id)
                ncatt_put(tmpout,0,"base_period",paste(baserange[1],"-",baserange[2],sep=""))

	        nc_close(tmpout)
                if(irregular) {system(paste("module load nco; ncks -C -O -x -v x,y",outfile,outfile,sep=" "))}

        # Report back
                print(paste(outfile," completed.",sep=""))

	# Clean up for next iteration
		rm(timedim,indexcdf,tmpout,outfile,index3d_trans)
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
print(length(spei(ci@data$prec,n)$fitted))
q()
	return(spei(ci@data$prec,n))
}

# hw
# Heat wave indices. From Perkins and Alexander (2013)
# This function will return a 3D dataset of dimensions [definition,aspect,year].
# HEAT WAVE DEFINITIONS:
#    - TX90p
#    - TN90p
#    - EHF (Excess heat factor)
# HEAT WAVE ASPECTS:
#    - HWM: heat wave magnitude
#    - HWA: heat wave amplitude
#    - HWN: heat wave number
#    - HWD: heat wave duration
#    - HWF: heat wave frequency
#
#### NOTE: NEED TO DEAL WITH FIRST 33 DAYS OF RECORD THAT ARE NA...
climdex.hw <- function(ci,base.range=c(1961,1990),n=15,min.base.data.fraction.present,lat) {
	stopifnot(!is.null(lat))

# step 1. Get/calculate the three definitions of a heat wave. Try using climdex's get.outofbase.quantiles function for this (EVEN NEEDED? climdex.raw GETS THESE ALREADY).
	# Get 90th percentile of tavg for EHIsig calculation below
	tavg90p <- get.outofbase.quantiles(ci@data$tavg,ci@data$tmin,tmax.dates=ci@dates,tmin.dates=ci@dates,base.range=base.range,n=n,temp.qtiles=0.95,prec.qtiles=0.9,
							min.base.data.fraction.present=min.base.data.fraction.present)

	# recalculate tavg here to ensure it is based on tmax/tmin
	tavg = (ci@data$tmax - ci@data$tmin)/2	#ci@data$tavg

	# get shells for the following three variables
	EHIaccl = array(NA,length(tavg))
	EHIsig = array(NA,length(tavg))
	EHF = array(NA,length(tavg))

	# make an array of repeating 1:365 to reference the right day for percentiles
	annualrepeat = array(1:365,length(tavg))

	# Calculate EHI values and EHF for each day of the given record. Must start at day 33 since the previous 32 days are required for each calculation.
	for (a in 33:length(ci@data$tavg)) {
		EHIaccl[a] = ((tavg[a] + tavg[a-1] + tavg[a-2])/3) - (sum(tavg[(a-32):(a-3)])/30)
		EHIsig[a] = ((tavg[a] + tavg[a-1] + tavg[a-2])/3) - unlist(tavg90p$tmax[1])[annualrepeat[a]] #[(a %% 365)]
		EHF[a] = max(1,EHIaccl[a])*EHIsig[a]
	}

print(lat)

# step 2. Determine if tx90p, tn90p or EHF conditions have persisted for >= 3 days. If so, count number of summer heat waves.

	# create an array of booleans for each definition identifying runs 3 days or longer where conditions are met. i.e. for TX90p, TN90p, EHF.
	tx90p_boolean = array(FALSE,length(ci@quantiles$tmax$outbase$q90))
        tn90p_boolean = array(FALSE,length(ci@quantiles$tmin$outbase$q90))
	EHF_boolean = array(FALSE,length(EHF))

	# TO DO: loop through each element of tmax/tmin and compare to corresponding days percentile. 
	#  - also, create correct array size (in time dimension) back in netCDF loader.
	print(length(ci@data$tmax))
	print(length(ci@quantiles$tmax$outbase$q90))

	# make repeating sequences of percentiles
	tx90p_arr <- array(ci@quantiles$tmax$outbase$q90,length(ci@data$tmax))
        tn90p_arr <- array(ci@quantiles$tmin$outbase$q90,length(ci@data$tmin))

	# Record which days had temperatures higher than 90p or where EHF > 0 
	tx90p_boolean <- (ci@data$tmax > tx90p_arr)
	tn90p_boolean <- (ci@data$tmin > tn90p_arr)
	EHF_boolean <- (EHF > 0)

	# Remove runs that are < 3 days long
	tx90p_boolean <- select.blocks.gt.length(tx90p_boolean,2)
        tn90p_boolean <- select.blocks.gt.length(tn90p_boolean,2)
	EHF_boolean <- select.blocks.gt.length(EHF_boolean,2)

# Step 3. Calculate aspects for each definition.
	hw_index <- array(NA,c(3,5,length(levels(ci@date.factors$annual))))
        hw1_index <- array(NA,c(5,length(levels(ci@date.factors$annual))))
        hw2_index <- array(NA,c(5,length(levels(ci@date.factors$annual))))
        hw3_index <- array(NA,c(5,length(levels(ci@date.factors$annual))))
print("BREAK")
print(levels(ci@date.factors$annual))
print(str(ci))

#			hw_index[1,1,] <- tapply.fast(tx90p_boolean,ci@date.factors$annual,function(idx) { mean(ci@data$tmax[idx]) } )
#                        hw_index[1,2,] <- tapply.fast(tx90p_boolean,ci@date.factors$annual,function(idx) { max(ci@data$tmax[idx]) } )
#                        hw_index[1,3,] <- tapply.fast(tx90p_boolean,ci@date.factors$annual,function(idx) { length(rle(tx90p_boolean)$lengths) } )
#                        hw_index[1,4,] <- tapply.fast(tx90p_boolean,ci@date.factors$annual,function(idx) { max(rle(tx90p_boolean)$lengths) } )
#                        hw_index[1,5,] <- tapply.fast(tx90p_boolean,ci@date.factors$annual,function(idx) { sum(rle(tx90p_boolean)$lengths) } )

	hw1_index <- get.hw.aspects(hw1_index,tx90p_boolean,ci@date.factors$annual,ci@data$tmax)
        hw2_index <- get.hw.aspects(hw2_index,tn90p_boolean,ci@date.factors$annual,ci@data$tmin)
        hw3_index <- get.hw.aspects(hw3_index,EHF_boolean,ci@date.factors$annual,EHF)
q()
}

# heat wave aspects as per Perkins and Alexander (2013)
get.hw.aspects <- function(aspect.array,boolean.str,date.factors,daily.data) {
print(daily.data)
	aspect.array[1,] <- tapply.fast(boolean.str,date.factors,function(idx) { mean(daily.data[idx]) } )
        aspect.array[2,] <- tapply.fast(boolean.str,date.factors,function(idx) { max(daily.data[idx]) } )
        aspect.array[3,] <- tapply.fast(boolean.str,date.factors,function(idx) { length(rle(boolean.str)$lengths) } )
        aspect.array[4,] <- tapply.fast(boolean.str,date.factors,function(idx) { max(rle(tx90p_boolean)$lengths) } )
        aspect.array[5,] <- tapply.fast(boolean.str,date.factors,function(idx) { sum(rle(tx90p_boolean)$lengths) } )
	return(aspect.array)
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








# GUI STUFF

# fonts for different texts in GUI.

startss <- function(){
logo_require    <- FALSE
 start1<-tktoplevel(bg='white')
fontHeading     <- tkfont.create(family = "times", size = 40, weight = "bold", slant = "italic")

fontHeading1    <- tkfont.create(family = "times", size = 20, weight = "bold")

fontHeading2    <- tkfont.create(family = "times", size = 14, weight = "bold")

fontTextLabel   <- tkfont.create(family = "times", size = 12)

fontFixedWidth  <- tkfont.create(family = "courier", size = 12)

font_small  <- "times 12"

font_big    <- "times 15 bold"

font_err    <- "times 13 bold"

grey_font <- tkfont.create(family = "times", size = 30, weight = "bold", slant = "italic") #'times 20 grey bold'


  # search logo files in current working directory.

  no.logo <- FALSE;

  dir0 <- getwd();

  logo1 <- "WMOLogo.gif";

  logo2 <- "UNSW.gif";

  logo3 <- "coess.gif";



  if (logo_require == TRUE)

  {  # Users want to search another directory for the logo files.

    while (file.exists(paste(dir0, "/", logo1, sep = "")) == FALSE)

    {

      dir0 <- tk_choose.dir(getwd(), caption = "Select directory containing three .gif files");

      if (is.na(dir0) == TRUE)

      {

        no.logo <- TRUE;

        break();

      }

    }

  }

  if (file.exists(paste(dir0, "/", logo1, sep = "")) == FALSE | 

      file.exists(paste(dir0, "/", logo2, sep = "")) == FALSE |

      file.exists(paste(dir0, "/", logo3, sep = "")) == FALSE) no.logo <- TRUE; # find logos?



  tkwm.geometry(start1, "+0+0"); # position in upper left corner of screen

  tkwm.title(start1, "ClimPACT test version");



  # Show logos on upper half of the main window, or "no logos available".  

  if (no.logo == FALSE)

  {  # with logos

    logo1 <- paste(dir0, "/", logo1, sep = "");

    logo2 <- paste(dir0, "/", logo2, sep = "");

    logo3 <- paste(dir0, "/", logo3, sep = "");



    img  <- tkimage.create("photo", file = logo1);

    img2 <- tkimage.create("photo", file = logo2, width = 0);

    img3 <- tkimage.create("photo", file = logo3, width = 0);



    left  <- tklabel(start1, image = img2);

    right <- tklabel(start1, image = img3);

    tkgrid(left, right);

    tkgrid.configure(left,  sticky = "e");

    tkgrid.configure(right, sticky = "w");

  } else

  {    # no logos, show a help button.

    help.logo <- function()

    {

      tkmessageBox(message=paste('You can see this help because the logo files are not in the working directory of R!\n\nThree files are needed for the logos:\n',

        logo1,', ',logo2,', and ',logo3,'\nAnd they must be in the working directory of R.\n\n',

        'If you have those files, you can put them in the working directory, or set "logo_require=T" in line 57 of the source code and try again... ',

        sep = ''), icon = 'question');

    }



    right <- tkbutton(start1, text = " ? ", command = help.logo, bg = "white", foreground = "light grey", width = 2);



    left <- tklabel(start1, text = "  no logos available  ", font = grey_font, width = 30, bg = "white", foreground = "light grey");



    # The following 2 lines could be used later if we want a wanring message when logo files are not available.

    #tkgrid(left,right)   

    #tkgrid.configure(left,sticky="e"); tkgrid.configure(right,sticky="w")

  }



  # lower half of the window.

  tkgrid(tklabel(start1, text = "    ", bg = "white"));

  tkgrid(tklabel(start1, text = "             ClimPACT             ", font = fontHeading, width = 30, bg = "white"), columnspan = 2);

  tkgrid(tklabel(start1, text = "    ", bg = "white"));

    

    start.but   <- tkbutton(start1, text = "Load Data and Run QC", command = getfile0, width = 30, font = fontHeading2, bg = "white");

    cal.but     <- tkbutton(start1, text = "Indices Calculation", command = nastat, width = 30, font = fontHeading2, bg = "white");

    cancel.but  <- tkbutton(start1, text = "Exit", command = done, width = 30, font = fontHeading2, bg = "white");

    help.but    <- tkbutton(start1, text = "About", command = get_help, width = 30, font = fontHeading2, bg = "white");

    license.but <- tkbutton(start1, text = "Software Licence Agreement", command = license, width = 30, font = fontHeading2, bg = "white");

    tkgrid(start.but,   columnspan = 2);

    tkgrid(cal.but,     columnspan = 2);

    tkgrid(help.but,    columnspan = 2);

    tkgrid(license.but, columnspan = 2);

    tkgrid(cancel.but,  columnspan = 2);

    tkgrid(tklabel(start1, text = "", bg = "white"));

    if (no.logo == FALSE) tkgrid(tklabel(start1, image = img), columnspan = 2);

    tkfocus(start1);

}
