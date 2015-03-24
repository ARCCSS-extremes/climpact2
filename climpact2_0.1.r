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
freq=NULL,quantiles=NULL,tempqtiles=c(0.1,0.9),precqtiles=c(0.1,0.9),max.missing.days=c(annual=15, monthly=3),min.base.data.fraction.present=0.1,n=5,northern.hemisphere=TRUE,usern_csdin=NULL,usern_wsdin=NULL)
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

# Load files and variables. Assumedly this is a memory intensive step for large variables. Way to improve this? Read incrementally?
        if(!is.null(tsminfile)) { nc_tsmin=nc_open(tsminfile); tsmin <- ncvar_get(nc_tsmin,tsminname) ; refnc=nc_tsmin}
        if(!is.null(tsmaxfile)) { nc_tsmax=nc_open(tsmaxfile); tsmax <- ncvar_get(nc_tsmax,tsmaxname) ; refnc=nc_tsmax}
        if(!is.null(precfile)) { nc_prec=nc_open(precfile); prec <- ncvar_get(nc_prec,precname) ; refnc=nc_prec}

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
        time = get.time(refnc,timename) #ncvar_get(refnc,"time") ; print(time)
        yeardate = unique(format(time,format="%Y"))
        nmonths = length(yeardate)*12
        nyears = length(yeardate)

# Compile climdexinput function for performance. Then create a 2D array of climdex input objects.
	cicompile <- cmpfun(climdexInput.raw)
	ciarray = array(list(),c(length(lon),length(lat)))

	for(j in 1:length(lat)){
		for(i in 1:length(lon)){
			ciarray[[i,j]] = cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=prec[i,j,],tmin.dates=time,tmax.dates=time,prec.dates=time,prec.qtiles=precqtiles,temp.qtiles=tempqtiles,quantiles=quantiles,base.range=baserange)
		}
	}

# Loop through index list; get index, read variable, calculate index on grid, write out index on gridded netcdf
        print("********* CALCULATING INDICES")
        for(a in 1:length(indices)){
	# Fetch and compile index function
	        indexfun = match.fun(paste("climdex",indices[a],sep="."))
		indexcompile = cmpfun(indexfun)

	# Determine whether index to be calculated will be monthly (otherwise annual)
                if(!is.null(formals(indexfun)$freq)) {
			if(!is.null(freq)){
				if(freq[1] == "monthly") {monthly = TRUE} else {monthly = FALSE}
			} else {monthly = TRUE}
		} else {monthly = FALSE}

#		time = get.time(refnc,timename) #ncvar_get(refnc,"time") ; print(time)
#		yeardate = unique(format(time,format="%Y"))
#		nmonths = length(yeardate)*12
#		nyears = length(yeardate)
#		print(nmonths)

	# Create empty array depending on whether index is monthly or annual
		if(monthly==TRUE) {index = array(NA,c(length(lon),length(lat),nmonths))} else{ index = array(NA,c(length(lon),length(lat),nyears))}

		for(j in 1:length(lat)){
			for(i in 1:length(lon)){
#			cio = cicompile(tmin=tsmin[j,i,],tmax=tsmax[j,i,],prec=prec[j,i,],tmin.dates=time,tmax.dates=time,prec.dates=time,prec.qtiles=precqtiles,temp.qtiles=tempqtiles,quantiles=quantiles,base.range=baserange)
#			index[i,j,] = array(indexcompile(cio,freq=freq))

                        index[i,j,] = array(indexcompile(ciarray[[i,j]]))
			}
		}

		index3d = index

	# Transpose dimensions to time,lat,lon
	        index3d_trans = aperm(index3d,c(2,3,1))

	# Write out
	# NOTE: ncdf4 seems to only support numeric types for dimensions.
		if(monthly == TRUE){	# if monthly data
                        timedim <- ncdim_def("time",paste("months since ",yeardate[1],"-01-01",sep=""),0.5:(nmonths-0.5))
		} else{								# else assume annual data
	                timedim <- ncdim_def("time",paste("years since ",yeardate[1],"-01-01",sep=""),0.5:(nyears-0.5))
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
                system(paste("module load nco; ncks -C -O -x -v x,y",outfile,outfile,sep=" "))

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
get.time <- function(nc=NULL,timename=NULL)
{
	ftime = ncvar_get(nc,timename)
	time_att = ncatt_get(nc,timename,"units")[2]

        if(grepl("hours",time_att)) {print("Time coordinate in hours, converting to seconds...") ;ftime = ftime*60*60}
        if(grepl("days",time_att)) {print("Time coordinate in days, converting to seconds...") ;ftime = ftime*24*60*60}

	return(as.PCICt(ftime,cal="gregorian",origin=get.origin(time_att=time_att[[1]])))
}

# A universal wrapper function for the climdex (and climpact?) functions. This is needed simply to pass a ci object to the index functions, instead of a list object.
#index.wrap <- function(ci=NULL,index=NULL,freq=NULL)
#{
#	indexfun = match.fun(paste("climdex",index,sep="."))
#	return(indexfun(ci))
#}


##############
##############
# NEW FUNCTIONS FROM CLIMPACT

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

# same as climdex.tx90p, except for 50th percentile. 
# NOT SURE ABOUT IMPLEMENTATION. HOW ARE QUANTILES HANDLED? Should be able to submit 'temp.qtiles=c(0.5)' to climdexInput.raw?
# UPDATE::: changed "qtiles=c(0.10,0.90)" to "qtiles" in get.temp.var.quantiles. This should allow the code to handle any specified percentiles.
#       ::: contd. Not sure why this was hard coded.
climdex.tx50p <- function(ci, freq=c("monthly", "annual")) {
print(str(ci@quantiles$tmax$inbase))
print("outbase")
print(str(ci@quantiles$tmax$outbase))
stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(percent.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q50, ci@quantiles$tmax$inbase$q50, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax)
}

##### TO CHECK/FIX ##### Need to understand how percentiles are calculated inside the base range. 
######################## In climdex.pcic this has dimensions (365,nyears,nyears-1), not sure why.
climdex.tx95t <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(ci@quantiles$tmax$inbase$q95) }

climdex.ntxntn <- function(ci, usern1,usern2) { stopifnot((!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)) || (!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin))); 
return(dual.threshold.exceedance.duration.index(ci@data$tmax,ci@data$tmin,ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q90,ci@quantiles$tmin$outbase$q90,">",">",
spells.can.span.years=spells.can.span.years, max.missing.days1=ci@max.missing.days['annual'],min.length1=usern1,min.length2=usern2) * ci@namasks$annual$tmax) }
