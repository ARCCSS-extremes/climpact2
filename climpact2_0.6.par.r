# Climpact2
# University of New South Wales
#
# Climpact2 combines the CRAN package climdex.pcic, developed by the Pacific Climate Impacts Consortium, with the Climpact code, developed at UNSW by XXXX. 
# Science users are intended to access the indices through the climpact.loader function while non-specialists will be able to use the GUI.
# 
# nherold, 2015.

library(ncdf4)
library(climdex.pcic)
library(PCICt)
library(compiler)
library(foreach)
library(doParallel)
library(SPEI)
library(tcltk)
library(abind)

options(warn=1)
enableJIT(3)

software_id = "0.6"

# climpact.loader
#
# This function reads in one to three netCDF files and calculates the specified indices.
# No unit conversion is done except for Kelvin to Celcius.
#
# INPUT
#  tsminfile: min temperature netCDF file. Must be gridded daily data.
#  tsmaxfile: max temperature netCDF file. Must be gridded daily data.
#  precfile: daily precipitation netCDF file. Must be gridded daily data.
#  tsminname: name of min temperature variable in tsminfile
#  tsmaxname: name of max temperature variable in tsmaxfile
#  precname: name of daily precipitation variable in precfile
#  indices: a list specifying which indices to calculate. See XX for a list of supported indices. Specify "all" to calculate all indices.
#  identifier: an optional string to aid identifying output files (e.g. this may be the particular model/dataset the indices are being calculated on).
#  lonname: name of longitude dimension.
#  latname: name of latitude dimension.
#  write_quantiles: boolean specifying whether to write percentiles to a file for later use.
#  quantile_file: netCDF file created from a previous execution of climpact.loader with write_quantiles set to TRUE.
#  cores: specify the number of cores to use for processing. Default is to use one core.
#  tempqtiles: temperature percentiles to calculate.
#  precqtiles: precipitation percentiles to calculate.
#  baserange: year range that will be used for percentile calculations.
#  max.missing.days: maximum missing days under which indices will still be calculated.
#  min.base.data.fraction.present: minimum fraction of data required for a quantile to be calculated for a particular day.
#  freq: frequency at which to calculate indices (only applies to certain indices, can be either "annual" or "monthly"). Used for all relevant indices.
#  ... additional parameters: any parameters that are defined for specific indices (see the manual) can be specified by 
#      prefixing the index name followed by an underscore. For example, the spells.can.span.years parameter for the climdex index wsdi can be specified by passing wsdi_spells.can.span.years.
#
# OUTPUT
#  A single netCDF file for each index specified in indices.
#
climpact.loader <- function(tsminfile=NULL,tsmaxfile=NULL,precfile=NULL,tsminname="tsmin",tsmaxname="tsmax",precname="prec",timename="time",indices=NULL,identifier=NULL,lonname="lon",latname="lat",baserange=c(1961,1990),
freq=c("monthly","annual"),tempqtiles=c(0.1,0.9),precqtiles=c(0.1,0.9),max.missing.days=c(annual=15, monthly=3),min.base.data.fraction.present=0.1,csdin_n=5,csdin_spells.can.span.years=FALSE,wsdin_n=5,wsdin_spells.can.span.years=FALSE,
cdd_spells.can.span.years=TRUE,cwd_spells.can.span.years=TRUE,csdi_spells.can.span.years=FALSE,wsdi_spells.can.span.years=FALSE,ntxntn_spells.can.span.years=FALSE,ntxntn_n=5,ntxbntnb_spells.can.span.years=FALSE,
ntxbntnb_n=5,gslmode=c("GSL", "GSL_first", "GSL_max", "GSL_sum"),rx5day_centermean=FALSE,hddheat_n=18,time_format=NULL,rxnday_n=5,rxnday_center.mean.on.last.day=FALSE,rnnm_threshold=1,
spei_n=1,hwn_n=5,write_quantiles=FALSE,quantile_file=NULL,cores=NULL)
{
# Initial checks
# 1) at least one file is provided,
# 2) at least one index is provided, 
# 3) that all indices are valid,
# TO ADD: if a tsmax index is specified but tsmax file is not. BOMB.
# TO ADD: FILES EXIST!! This should be first thing! Including qtile file if specified.
	indexfile = "index.master.list"
	indexlist <- (read.table(indexfile,sep="\t"))
	units <- as.character(indexlist[,2])
	desc <- as.character(indexlist[,3])

	if(indices[1] == "all") indices = as.character(indexlist[,1])
	if(all(is.null(tsminfile),is.null(tsmaxfile),is.null(precfile))) stop("Must provide at least one filename for tsmin, tsmax and/or prec.")
	if(is.null(indices)) stop(paste("Must provide a list of indices to calculate. See ",indexfile," for list.",sep=""))
        if(any(!indices %in% indexlist[,1])) stop(paste("One or more indices are unknown. See ",indexfile," for list.",sep=""))
	if(!is.null(quantile_file) && write_quantiles==TRUE) stop("Cannot both write quantiles AND read in quantiles from a file.")
	if(!is.null(cores) && !is.numeric(cores)) stop("cores must be an integer (and should be less than the cores available on your computer)")

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

# Set up parallel options if required
        acomb <- function(...) { if(indices[a] == "hw") { abind(..., along=5) } else { return(abind(..., along=3)) } }
        acomb_qtile <- function(...) { abind(..., along=5) }
        if(!is.null(cores)) {
                cl <- makeCluster(cores)
                registerDoParallel(cl) }
        exportlist <- c("get.na.mask","dual.threshold.exceedance.duration.index","get.hw.aspects","tapply.fast","indexcompile")

# Set up coordinate variables for writing to netCDF. If irregular grid then create x/y indices, if regular grid read in lat/lon coordinates.
	if(length(dim(ncvar_get(refnc,latname))) > 1) {irregular = TRUE} else {irregular = FALSE}	# determine if irregular

	if(irregular){					# If an irregular grid is being used
	        lat = 1:dim(ncvar_get(refnc,latname))[2]
        	lon = 1:dim(ncvar_get(refnc,lonname))[1]
                londim <- ncdim_def("x", "degrees_east", as.double(lon))	# creates object of class ncdim4!
                latdim <- ncdim_def("y", "degrees_north", as.double(lat))
                lon2d = ncvar_get(refnc,lonname)
                lat2d = ncvar_get(refnc,latname)
		exportlist <- c(exportlist,"lat2d")
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
		months_as_hours = as.numeric(as.Date(paste(monthdate,"-01",sep="")) - as.Date(origin))*24	# convert days to hours
		years_as_hours = as.numeric(as.Date(paste(yeardate,"-01-01",sep="")) - as.Date(origin))*24
	        nmonths = length(months_as_hours)
        	nyears = length(years_as_hours)
	} else {
		nmonths = length(yeardate)*12
		nyears = length(yeardate)
	}

	if(!is.null(tsminfile)) { tsmintime = time }; if(!is.null(tsmaxfile)) { tsmaxtime = time }; if(!is.null(precfile)) { prectime = time }

# Get quantiles if given by user
	if(!is.null(quantile_file)) {
		nc_qtiles = nc_open(quantile_file)
		if(!is.null(ncvar_get(nc_qtiles,"tmin"))) { tminqtiles = ncvar_get(nc_qtiles,"tmin") ; tnames = ncvar_get(nc_qtiles,"tqtile") }
                if(!is.null(ncvar_get(nc_qtiles,"tmax"))) { tmaxqtiles = ncvar_get(nc_qtiles,"tmax") ; tnames = ncvar_get(nc_qtiles,"tqtile") }
                if(!is.null(ncvar_get(nc_qtiles,"tavg"))) { tavgqtiles = ncvar_get(nc_qtiles,"tavg") ; tnames = ncvar_get(nc_qtiles,"tqtile") }
                if(!is.null(ncvar_get(nc_qtiles,"prec"))) { precipqtiles = ncvar_get(nc_qtiles,"prec") ; pnames = ncvar_get(nc_qtiles,"pqtile") }
	}

# Compile climdexinput function for performance.
	cicompile <- cmpfun(climdexInput.raw)

# Loop through index list; get index, read variable, calculate index on grid, write out index on gridded netcdf
        print("***************************************")
        print("********* CALCULATING INDICES *********")
	print("***************************************")
        for(a in 1:length(indices)){
#	loop1 <- foreach(a=1:(length(indices[1])),.packages=eval(match.fun(paste("climdex",indices[a],sep=".")))) %dopar% {

	# Fetch and compile index function
	        indexfun = match.fun(paste("climdex",indices[a],sep="."))
		indexcompile = cmpfun(indexfun)

	# Create index call string (better way to do this? Somehow to use |...). For performance reasons reduce the number of percentiles to calculate for each index.
		options(useFancyQuotes=FALSE)
		indexparam = "cio"

		tempqtiles_tmp = tempqtiles ; precqtiles_tmp = precqtiles
		switch(indices[a],
			cdd={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",cdd_spells.can.span.years,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			csdi={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",csdi_spells.can.span.years,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			cwd={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",cwd_spells.can.span.years,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
	                dtr={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			gsl.mode={indexparam = paste("array(indexcompile(",indexparam,",gsl.mode=",dQuote(gslmode),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			rnnmm={indexparam = paste("array(indexcompile(",indexparam,",threshold=",rnnm_threshold,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			rx1day={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
	                rx5day={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),",center.mean.on.last.day=",rx5day_centermean,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
                        r95ptot={indexparam = paste("array(indexcompile(",indexparam,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.95) ; precqtiles_tmp = c(0.95) } },
                        r99ptot={indexparam = paste("array(indexcompile(",indexparam,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.99) ; precqtiles_tmp = c(0.99) } },
	                tn10p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			tn90p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.9) ; precqtiles_tmp = c(0.9) } },
			tnn={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			tnx={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			tx10p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
                        tx50p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.5) ; precqtiles_tmp = c(0.5) } },
			tx90p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.9) ; precqtiles_tmp = c(0.9) } },
			txn={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			txx={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			wsdi={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",wsdi_spells.can.span.years,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.9) ; precqtiles_tmp = c(0.9) } },
                        csdin={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",csdin_spells.can.span.years,",n=",csdin_n,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
                        wsdin={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",wsdin_spells.can.span.years,",n=",wsdin_n,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = c(0.9) ; precqtiles_tmp = c(0.9) } },
                        ntxntn={indexparam = paste("array(indexcompile(",indexparam,",n=",ntxntn_n,",spells.can.span.years=",ntxntn_spells.can.span.years,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = c(0.95) ; precqtiles_tmp = c(0.95) } },
                        ntxbntnb={indexparam = paste("array(indexcompile(",indexparam,",n=",ntxbntnb_n,",spells.can.span.years=",ntxbntnb_spells.can.span.years,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = c(0.05) ; precqtiles_tmp = c(0.05) } },
			tx95t={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.95) ; precqtiles_tmp = c(0.95) } },
                        rxnday={indexparam = paste("array(indexcompile(",indexparam,",center.mean.on.last.day=",rxnday_center.mean.on.last.day,",n=",rxnday_n,"))",sep="")},spei={indexparam = paste("array(indexcompile(",indexparam,",n=",spei_n,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = c(0.1) } },
			hw={if(irregular) { latstr="lat2d[i,j]" } else { latstr="lat[j]" } ;
				indexparam = paste("array(indexcompile(",indexparam,",base.range=c(",baserange[1],",",baserange[2],"),n=",hwn_n,",min.base.data.fraction.present=",
				min.base.data.fraction.present,",lat=",latstr,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1,0.9) ; precqtiles_tmp = c(0.1,0.9) } },

		{ indexparam = paste("array(indexcompile(",indexparam,"))",sep="") ; tempqtiles_tmp <- precqtiles_tmp <- NULL } )
		print(paste("diag: index call: ",eval(indexparam)),sep="")

	# Determine whether index to be calculated will be daily (currently only for tx95t), monthly or annual
		if(indices[a] == "tx95t") {period = "DAY"} else if (indices[a] == "rxnday") { period = "MON" } else {
	                if(!is.null(formals(indexfun)$freq)) {
				if(!is.null(freq)){
					if(freq[1] == "monthly") {period = "MON"} else {period = "ANN"}
				} else {period = "MON"}
			} else {period = "ANN"}
		}

	# array size depending on whether index is daily, monthly, annual or heatwave
		if(indices[a] == "hw") { index = array(NA,c(3,5,length(lon),length(lat),nyears)) } else if(period == "MON") {index = array(NA,c(length(lon),length(lat),nmonths))} 
		else if(period == "ANN") { index = array(NA,c(length(lon),length(lat),nyears))} else {index = array(NA,c(length(lon),length(lat),365))}

	# create quantile arrays dimensions if quantile file is requested. [lon,lat,days,percentiles,base]
		if(write_quantiles == TRUE) { tminqtiles_array <- tmaxqtiles_array <- tavgqtiles_array <- array(NA,c(length(lon),length(lat),365,length(tempqtiles),2)) 
				precipqtiles_array = array(NA,c(length(lon),length(lat),365,length(precqtiles))) }

        # If quantiles are requested, record quantiles. This will only happen once.
                if (write_quantiles == TRUE) {
			print("WRITING QUANTILES")
			qtilefetch = array(NA,c(3,length(tempqtiles),length(lon),length(lat),365))
			innert = qtilefetch[,,,1,]	#array(NA,c(3,length(tempqtiles),length(lon),365))
			innerp = array(NA,c(1,length(precqtiles),length(lon),365))

			qtilefetch<-foreach(j=1:length(lat),.combine='acomb_qtile',.export=c(ls(envir=globalenv()),objects(),"get.outofbase.quantiles")) %dopar% {
				library(climdex.pcic)
				for(i in 1:length(lon)){
	                                cio = cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=NULL,tmin.dates=tsmintime,tmax.dates=tsmaxtime,prec.dates=NULL,prec.qtiles=NULL,
                                        temp.qtiles=tempqtiles_tmp,quantiles=NULL,base.range=baserange)

		                        tavgqtiles = get.outofbase.quantiles(cio@data$tavg,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=baserange,temp.qtiles=tempqtiles,prec.qtiles=NULL)
        		                for (l in 1:length(tempqtiles)) {
	                        	innert[1,l,i,] = cio@quantiles$tmin$outbase[[l]]
	        	                innert[2,l,i,] = cio@quantiles$tmax$outbase[[l]]
        	        	        innert[3,l,i,] = tavgqtiles$tmax$outbase[[l]] }       # while this is named tmax it is in fact tavg, see call for tavgqtiles creation.
				}
                                innert # return the array
			}
			tmp = aperm(qtilefetch,c(3,5,4,2,1))
			tminqtiles_array = tmp[,,,,1] ; tmaxqtiles_array = tmp[,,,,2] ; tavgqtiles_array = tmp[,,,,3]

			rm(qtilefetch) 
                        qtilefetch<-foreach(j=1:length(lat),.combine='acomb_qtile',.export=c(objects(),"get.outofbase.quantiles")) %dopar% {
                                for(i in 1:length(lon)){
                                        cio = cicompile(tmin=NULL,tmax=NULL,prec=prec[i,j,],tmin.dates=NULL,tmax.dates=NULL,prec.dates=prectime,prec.qtiles=precqtiles_tmp,
                                        temp.qtiles=NULL,quantiles=NULL,base.range=baserange)

                                       for (l in 1:length(precqtiles)) { innerp[1,l,i,] = cio@quantiles$prec[[l]] }
				}
				innerp # return the array
			}
			precipqtiles_array = aperm(qtilefetch,c(3,5,4,2,1))

			rm(tmp,innert,innerp,qtilefetch)
        	}

	# Calculate the index. Parallelise outer loop.
#print(ls(envir=globalenv()))
#print(objects())
		j = 1
                if(indices[a] == "hw") { test = index[,,,1,] } else { test = index[,1,] } # create a dummy shell for the parrallel loop.

		index<-foreach(j=1:length(lat),.combine='acomb',.export=c(exportlist,objects())) %dopar% {		# ls(envir=globalenv()),objects()
#	                loop3 <- foreach(i=1:(length(lon))) %dopar% {	#"indexcompile","lat2d"
                       library(climdex.pcic)	# done here as each core needs access to library
                       for(i in 1:length(lon)){
				# DO QUANTILE WORK IF NECESSARY
				# If quantiles are provided, create the quantile list to feed climdexinput.raw and make tempqtiles and precqtiles NULL if not already/
		                if(!is.null(quantile_file)) {
		                        quantiles = list()
		                        if(!is.null(tminqtiles)) { 
		                                tminlist=vector("list", length(tnames))
		                                names(tminlist) <- paste("q",tnames,sep="")
		                                for (l in 1:length(tnames)) { tminlist[[l]]=tminqtiles[i,j,,l,1] }
		                                quantiles$tmin$outbase = tminlist
                                                quantiles$tmin$inbase = tminlist
		                        }
		                        if(!is.null(tmaxqtiles)) {
		                                tmaxlist=vector("list", length(tnames))
		                                names(tmaxlist) <- paste("q",tnames,sep="")
		                                for (l in 1:length(tnames)) { tmaxlist[[l]]=tmaxqtiles[i,j,,l,1] }
		                                quantiles$tmax$outbase = tmaxlist
                                                quantiles$tmax$inbase = tmaxlist
		                        }
                                        if(!is.null(tavgqtiles)) {
                                                tavglist=vector("list", length(tnames))
                                                names(tavglist) <- paste("q",tnames,sep="")
                                                for (l in 1:length(tnames)) { tavglist[[l]]=tavgqtiles[i,j,,l,1] }
                                                quantiles$tavg$outbase = tavglist
                                                quantiles$tavg$inbase = tavglist
                                        }
		                        if(!is.null(precipqtiles)) {
		                                preclist=vector("list", length(pnames))
		                                names(preclist) <- paste("q",pnames,sep="")
		                                for (l in 1:length(pnames)) { preclist[[l]]=precipqtiles[i,j,,l] }
		                                quantiles$prec = preclist
		                        }
					tempqtiles_tmp <- precqtiles_tmp <- NULL
		                } else { quantiles = NULL }

				# Calculate climdex input object and index
				cio = cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=prec[i,j,],tmin.dates=tsmintime,tmax.dates=tsmaxtime,prec.dates=prectime,prec.qtiles=precqtiles_tmp,
					temp.qtiles=tempqtiles_tmp,quantiles=quantiles,base.range=baserange)
#print(str(cio))
				# Need a separate way to write out heat wave indices... better way to do this?
				if(indices[a] == "hw") { test[,,i,] = eval(parse(text=indexparam)) }
				else { test[i,] = eval(parse(text=indexparam)) }
			}
		# return a vector/list/array
			test
		}

	# Transpose dimensions to time,lat,lon
	        if(indices[a] == "hw") { index3d_trans = aperm(index,c(3,5,4,2,1)) } else { index3d_trans = aperm(index,c(1,3,2)) }

# Write out
# NOTE: ncdf4 seems to only support numeric types for dimensions.
	# write out qunatiles if requested
		if(write_quantiles == TRUE) {
			qfile = paste(paste("CCRC",identifier,period,baserange[1],baserange[2],"quantiles",sep="_"),".nc",sep="")
			system(paste("rm -f ",qfile,sep=""))
			tqnames = tempqtiles*100	#as.numeric(substr(names(cio@quantiles$tmin$outbase),2,3))
			pqnames = precqtiles*100	#as.numeric(substr(names(cio@quantiles$prec),2,3))

			# create time, quantile and inbase/outbase dimensions
			timedim <- ncdim_def("time","days",1:365) ; tqdim <- ncdim_def("tqtile","unitless",tqnames) ; pqdim <- ncdim_def("pqtile","unitless",pqnames)

			# create variable ncdf objects
                        tmincdf = ncvar_def(paste("tmin",sep=""),"C",list(londim,latdim,timedim,tqdim),-1,prec="float")
                        tmaxcdf = ncvar_def(paste("tmax",sep=""),"C",list(londim,latdim,timedim,tqdim),-1,prec="float")
                        tavgcdf = ncvar_def(paste("tavg",sep=""),"C",list(londim,latdim,timedim,tqdim),-1,prec="float")
                        preccdf = ncvar_def(paste("prec",sep=""),"mm/day",list(londim,latdim,timedim,pqdim),-1,prec="float")
			qout = nc_create(qfile,list(tmincdf,tmaxcdf,tavgcdf,preccdf),force_v4=TRUE)

			# write out data
			ncvar_put(qout,tmincdf,tminqtiles_array) ; ncvar_put(qout,tmaxcdf,tmaxqtiles_array) ; ncvar_put(qout,tavgcdf,tavgqtiles_array) ; ncvar_put(qout,preccdf,precipqtiles_array)

			write_quantiles = FALSE         # only need to write once

			rm(timedim,tqdim,pqdim,tmincdf,tmaxcdf,preccdf,qout)
		}

	# create time dimension according to time format
		if(is.null(time_format)) {	# IF no time format supplied work with hours since.
	                if(period == "MON") { timedim <- ncdim_def("time",paste("hours since ",origin,sep=""),months_as_hours) } 
			else if(period == "ANN") { timedim <- ncdim_def("time",paste("hours since ",origin,sep=""),years_as_hours) } 
			else { timedim <- ncdim_def("time","days since 0001-01-01",1:365) }
		} else {			# ELSE use number of months or years since first time step.
			if(period == "MON") { timedim <- ncdim_def("time",paste("months since ",yeardate[1],"-01-01",sep=""),0.5:(nmonths-0.5)) } 
			else if(period == "ANN"){ timedim <- ncdim_def("time",paste("years since ",yeardate[1],"-01-01",sep=""),0.5:(nyears-0.5)) } 
			else { timedim <- ncdim_def("time","days since 0001-01-01",1:365) }
		}

	# create output file name customised for 'n' indices if needed
		switch(indices[a],
			wsdin={ outfile = paste(paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste("wsdi",wsdin_n,sep=""),sep="_"),".nc",sep="") },
			csdin={ outfile = paste(paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste("csdi",csdin_n,sep=""),sep="_"),".nc",sep="") },
			rxnday={ outfile = paste(paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste("rx",rxnday_n,"day",sep=""),sep="_"),".nc",sep="") },
                        ntxntn={ outfile = paste(paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste(ntxntn_n,"tx",ntxntn_n,"tn",sep=""),sep="_"),".nc",sep="") },
                        ntxbntnb={ outfile = paste(paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste(ntxbntnb_n,"txb",ntxbntnb_n,"tnb",sep=""),sep="_"),".nc",sep="") },
			{ outfile = paste(paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],indices[a],sep="_"),".nc",sep="") } )

	# create ncdf variable objects
	        if(indices[a] == "hw") { 
			hw_defdim <- ncdim_def("heat_wave_definition","1=tx90,2=tn90,3=EHF",1.0:3.0) ; hw_aspdim <- ncdim_def("heat_wave_aspect","1=HWM,2=HWA,3=HWN,4=HWD,5=HWF",1.0:5.0)
			hwmcdf <- ncvar_def("HWM","degC",list(londim,latdim,timedim,hw_defdim),-1,longname="Heat wave magnitude",prec="float")
			hwacdf <- ncvar_def("HWA","degC",list(londim,latdim,timedim,hw_defdim),-1,longname="Heat wave amplitude",prec="float")
			hwncdf <- ncvar_def("HWN","heat waves",list(londim,latdim,timedim,hw_defdim),-1,longname="Heat wave number",prec="float")
			hwdcdf <- ncvar_def("HWD","days",list(londim,latdim,timedim,hw_defdim),-1,longname="Heat wave duration",prec="float")
			hwfcdf <- ncvar_def("HWF","days",list(londim,latdim,timedim,hw_defdim),-1,longname="Heat wave frequency",prec="float") ; varlist <- list(hwmcdf,hwacdf,hwncdf,hwdcdf,hwfcdf) 
		} else { indexcdf <- ncvar_def(indices[a],units[a],list(londim,latdim,timedim),-1,longname=desc[a],prec="float") ; varlist <- list(indexcdf) }

	        system(paste("rm -f ",outfile,sep=""))

                if(irregular){
			print("WORKING ON IRREGULAR GRID...")
                        loncdf <- ncvar_def(lonname,"degrees_east",list(londim,latdim),-1,prec="float")
                        latcdf <- ncvar_def(latname,"degrees_north",list(londim,latdim),-1,prec="float")
			varlist[[length(varlist)+1]] <- loncdf ; varlist[[length(varlist)+1]] <- latcdf
	                tmpout = nc_create(outfile,varlist,force_v4=TRUE)
			ncvar_put(tmpout,loncdf,lon2d) ; ncvar_put(tmpout,latcdf,lat2d)
#			ncatt_put(tmpout,indexcdf,"coordinates","lon lat")
			rm(loncdf,latcdf)
		} else { tmpout = nc_create(outfile,varlist,force_v4=TRUE) }

        # write out variables
		if(indices[a] == "hw") { ncvar_put(tmpout,hwmcdf,index3d_trans[,,,1,]) ; ncvar_put(tmpout,hwacdf,index3d_trans[,,,2,]) ; ncvar_put(tmpout,hwncdf,index3d_trans[,,,3,])
                                ncvar_put(tmpout,hwdcdf,index3d_trans[,,,4,]) ; ncvar_put(tmpout,hwfcdf,index3d_trans[,,,5,]) } else { ncvar_put(tmpout,indexcdf,index3d_trans) }

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
		suppressWarnings(rm(timedim,indexcdf,tmpout,outfile,index3d_trans))
	}
	if(exists("cl")) { stopCluster(cl) }
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
		return(as.PCICt(ftime,cal="gregorian",origin=get.origin(time_att=time_att[[1]])))
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
climdex.hddheat <- function(ci,Tb=18) { Tbarr = array(Tb,length(ci@data$tavg)); stopifnot(is.numeric(ci@data$tavg),is.numeric(Tb)) ;return(tapply.fast(Tbarr - ci@data$tavg,ci@date.factors$annual,sum))*ci@namasks$annual }

# CDDcold
# Annual sum of TM-Tb (where Tb is a user-defined location-specific base temperature and TM > Tb)
climdex.cddcold <- function(ci,Tb=18) { Tbarr = array(Tb,length(ci@data$tavg)); stopifnot(is.numeric(ci@data$tavg),is.numeric(Tb)) ;return(tapply.fast(ci@data$tavg - Tbarr,ci@date.factors$annual,sum))*ci@namasks$annual }

# GDDgrow
# Annual sum of TM-Tb (where Tb is a user-defined location-specific base temperature and TM >Tb)
climdex.gddgrow <- function(ci,Tb=10) { Tbarr = array(Tb,length(ci@data$tavg)); stopifnot(is.numeric(ci@data$tavg),is.numeric(Tb)) ;return(tapply.fast(ci@data$tavg - Tbarr,ci@date.factors$annual,sum))*ci@namasks$annual }

# Rxnday
# Monthly maximum consecutive n-day precipitation (up to a maximum of 10)
# Same as rx5day except specifying a monthly frequency and accepting user specified number of consecutive days
climdex.rxnday <- function(ci, center.mean.on.last.day=FALSE,n=5) { stopifnot(!is.null(ci@data$prec)); return(nday.consec.prec.max(ci@data$prec, ci@date.factors$monthly, n, center.mean.on.last.day) * ci@namasks$monthly$prec) }

# tx95t
# Value of 95th percentile of TX
##### TO CHECK/FIX ##### Need to understand how percentiles are calculated inside the base range. Currently this function reports the out of base 95th percentile (which I interpret to be what the index defines anyway). 
######################## In climdex.pcic this has dimensions (365,nyears,nyears-1), not sure why.
climdex.tx95t <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(ci@quantiles$tmax$outbase$q95) }

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

# ntxntn
# Annual count of n consecutive days where both TX > 95th percentile and TN > 95th percentile, where n >= 2 (and max of 10)
# This function needs the new function dual.threshold.exceedance.duration.index, which was based on threshold.exceedance.duration.index
climdex.ntxntn <- function(ci, spells.can.span.years=FALSE,n=5) { 
	stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax) || (!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)))
	return(dual.threshold.exceedance.duration.index(ci@data$tmax, ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q95,ci@quantiles$tmin$outbase$q95, 
		">",">", n=n,spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual']) * ci@namasks$annual$tmax) }

# ntxbntnb
# Annual count of n consecutive days where both TX < 5th percentile and TN < 5th percentile, where n >= 2 (and max of 10)
# This function needs the new function dual.threshold.exceedance.duration.index, which was based on threshold.exceedance.duration.index
climdex.ntxbntnb <- function(ci, spells.can.span.years=FALSE,n=5) {
        stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax) || (!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)))
        return(dual.threshold.exceedance.duration.index(ci@data$tmax, ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q5,ci@quantiles$tmin$outbase$q5,
                "<","<", n=n,spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual']) * ci@namasks$annual$tmax) }

# dual.threshold.exceedance.duration.index
# calculates the number of n consecutive days where op1 and op2 operating on daily.temp1 and daily.temp2 respectively are satisfied.
dual.threshold.exceedance.duration.index <- function(daily.temp1, daily.temp2, date.factor, jdays, thresholds1, thresholds2, op1=">", op2=">", n, spells.can.span.years, max.missing.days) {
  stopifnot(is.numeric(c(daily.temp1,daily.temp2, thresholds1,thresholds2, n)), is.factor(date.factor),
            is.function(match.fun(op1)),is.function(match.fun(op2)),
            n > 0,length(daily.temp1)==length(daily.temp2))
  f1 <- match.fun(op1)
  f2 <- match.fun(op2)
  na.mask1 <- get.na.mask(is.na(daily.temp1 + thresholds1[jdays]), date.factor, max.missing.days)
  na.mask2 <- get.na.mask(is.na(daily.temp2 + thresholds2[jdays]), date.factor, max.missing.days)
  na.mask_combined = na.mask1 & na.mask2

  if(spells.can.span.years) {
    periods1 <- f1(daily.temp1, thresholds1[jdays])	#select.blocks.gt.length(f1(daily.temp1, thresholds1[jdays]), min.length1 - 1)
    periods2 <- f2(daily.temp2, thresholds2[jdays])	#select.blocks.gt.length(f2(daily.temp2, thresholds2[jdays]), min.length2 - 1)
    periods_combined = select.blocks.gt.length(periods1 & periods2,n)	# an array of booleans
    return(tapply.fast(periods_combined, date.factor, sum) * na.mask_combined)
  } else {
    return(tapply.fast(1:length(daily.temp1), date.factor, function(idx) { 
	periods1 = f1(daily.temp1[idx], thresholds1[jdays[idx]])	#select.blocks.gt.length(f1(daily.temp1[idx], thresholds1[jdays[idx]]), min.length1 - 1)#   * na.mask1
	periods2 = f2(daily.temp2[idx], thresholds2[jdays[idx]])	#select.blocks.gt.length(f2(daily.temp2[idx], thresholds2[jdays[idx]]), min.length2 - 1)#   * na.mask2
	periods_combined = select.blocks.gt.length(periods1 & periods2,n)
	return(sum(periods_combined)) })*na.mask_combined)
  }
}

# SPEI
# MAKE A ONE LINER!!!!!!*****
# Measure of “drought” using the Standardised Precipitation Evapotranspiration Index on time scales of 3, 6 and 12 months. No missing data are allowed to calculate SPEIflex. 
# Attempting to use the R package SPEI for the SPEI and SPI indices.
climdex.spei <- function(ci,n=c(3,6,12)) { stopifnot(is.numeric(ci@data$prec),is.numeric(n),n>0) ; 
	ci@data$prec[length(ci@data$prec)]=ci@data$prec[length(ci@data$prec)-1] ; print(sum(is.na(ci@data$prec)));print((spei(ci@data$prec,n))$fitted) 
print(str(ci))
print(length(spei(ci@data$prec,n)$fitted))
	speitest <- spei(ci@data$prec,n,na.rm=TRUE)
print(speitest)
q()
	return(spei(ci@data$prec,n))
}

# SPI
climdex.spi <- function(ci,n=c(3,6,12)) { stopifnot(is.numeric(ci@data$prec),is.numeric(n),n>0) ; 
spitest <- spi(ci@data$prec,n,na.rm=TRUE)
print(spitest)
q()
return()
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
climdex.hw <- function(ci,base.range=c(1961,1990),n=15,min.base.data.fraction.present,lat) {
	stopifnot(!is.null(lat))

# step 1. Get/calculate the three definitions of a heat wave. Try using climdex's get.outofbase.quantiles function for this (EVEN NEEDED? climdex.raw GETS THESE ALREADY).
	# Get 90th percentile of tavg for EHIsig calculation below
	# recalculate tavg here to ensure it is based on tmax/tmin. Then get 15 day moving windows of percentiles.
	tavg = (ci@data$tmax - ci@data$tmin)/2
        tavg90p <- suppressWarnings(get.outofbase.quantiles(tavg,ci@data$tmin,tmax.dates=ci@dates,tmin.dates=ci@dates,base.range=base.range,n=15,temp.qtiles=0.9,prec.qtiles=0.9,
                                                        min.base.data.fraction.present=min.base.data.fraction.present))
	TxTn90p <- suppressWarnings(get.outofbase.quantiles(ci@data$tmax,ci@data$tmin,tmax.dates=ci@dates,tmin.dates=ci@dates,base.range=base.range,n=15,temp.qtiles=0.9,prec.qtiles=0.9,
                                                        min.base.data.fraction.present=min.base.data.fraction.present))

	# get shells for the following three variables
	EHIaccl = array(NA,length(tavg))
	EHIsig = array(NA,length(tavg))
	EHF = array(NA,length(tavg))

	# make an array of repeating 1:365 to reference the right day for percentiles
	annualrepeat = array(1:365,length(tavg))

	# Calculate EHI values and EHF for each day of the given record. Must start at day 33 since the previous 32 days are required for each calculation.
	for (a in 33:length(ci@data$tavg)) {
		EHIaccl[a] = (sum(tavg[a],tavg[a-1],tavg[a-2],na.rm=TRUE)/3) - (sum(tavg[(a-32):(a-3)],na.rm=TRUE)/30)
		EHIsig[a] = (sum(tavg[a],tavg[a-1],tavg[a-2],na.rm=TRUE)/3) - as.numeric(unlist(tavg90p$tmax[1])[annualrepeat[a]]) #[(a %% 365)]
		EHF[a] = max(1,EHIaccl[a])*EHIsig[a]
	}
#	print(lat)

# step 2. Determine if tx90p, tn90p or EHF conditions have persisted for >= 3 days. If so, count number of summer heat waves.

	# create an array of booleans for each definition identifying runs 3 days or longer where conditions are met. i.e. for TX90p, TN90p, EHF.
	tx90p_boolean = array(FALSE,length(ci@quantiles$tmax$outbase$q90))
        tn90p_boolean = array(FALSE,length(ci@quantiles$tmin$outbase$q90))
	EHF_boolean = array(FALSE,length(EHF))

	# make repeating sequences of percentiles
	tx90p_arr <- array(TxTn90p$tmax$outbase$q90,length(ci@data$tmax))
        tn90p_arr <- array(TxTn90p$tmin$outbase$q90,length(ci@data$tmin))

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

        hw_index[1,,] <- get.hw.aspects(hw1_index,tx90p_boolean,ci@date.factors$annual,ci@date.factors$monthly,ci@data$tmax,lat)
        hw_index[2,,] <- get.hw.aspects(hw2_index,tn90p_boolean,ci@date.factors$annual,ci@date.factors$monthly,ci@data$tmin,lat)
        hw_index[3,,] <- get.hw.aspects(hw3_index,EHF_boolean,ci@date.factors$annual,ci@date.factors$monthly,EHF,lat)

	rm(tavg,tavg90p,TxTn90p,EHIaccl,EHIsig,EHF,annualrepeat,tx90p_boolean,tn90p_boolean,EHF_boolean,tx90p_arr,tn90p_arr,hw1_index,hw2_index,hw3_index)
	return(hw_index)
}

# return array of zeros or ones for each year supplied, indicating number of leap days in each year.
leapdays <- function(year) { if(!is.numeric(year)) stop("year must be of type numeric") ; return(0 + (year %% 4 == 0)) }

# heat wave aspects as per Perkins and Alexander (2013). HWM, HWA, HWN, HWD, HWF. 
# EHF definition is updated (personal comms Perkins 2015).
get.hw.aspects <- function(aspect.array,boolean.str,yearly.date.factors,monthly.date.factors,daily.data,lat) {
	month <- substr(monthly.date.factors,6,7)
	# Move daily.data forward 6 months so that SH summer data is in the middle of the year. Then retrieve winter months (which will really be summer data)
	# Then also need to add 1 to yearly date factors.
	daily.data = ifelse(boolean.str=="TRUE",daily.data,NA)			# remove daily data that is not considered a heat wave.

	if(lat < 0) {
		daily.data[!month %in% c("11","12","01","02","03")] <- NA	# step1. Remove NDJFM months from daily data and boolean array
		boolean.str[!month %in% c("11","12","01","02","03")] <- NA
		daily.data2 <- array(NA,length(daily.data))
		boolean.str2 <- array(NA,length(boolean.str))
		ind1 <- length(daily.data)-179

		daily.data2[180:length(daily.data)] <- daily.data[1:ind1]	# step2. Move data time-series and boolean array forward around 6 months. Don't need to be exact as data just needs to be in the right year.
		boolean.str2[180:length(boolean.str)] <- boolean.str[1:ind1]

		daily.data2[1:366] <- NA 					# step3. Remove data from first year since it has only a partial summer.  #; daily.data2[ind2:length(daily.data2)] <- NA
		daily.data <- daily.data2
		boolean.str2[1:366] <- NA
		boolean.str <- boolean.str2
	} else { daily.data[!month %in% c("05","06","07","08","09")] <- NA ; boolean.str[!month %in% c("05","06","07","08","09")] <- NA }

	aspect.array[1,] <- tapply.fast(daily.data,yearly.date.factors,function(idx) { mean(idx,na.rm=TRUE) } )
        aspect.array[2,] <- tapply.fast(daily.data,yearly.date.factors,function(idx) { suppressWarnings(max(idx,na.rm=TRUE)) } )
        aspect.array[3,] <- tapply.fast(boolean.str,yearly.date.factors,function(idx) { runlength = rle(as.logical(idx)) ; return(length(runlength$lengths[!is.na(runlength$values) & runlength$values=="TRUE"])) } )
        aspect.array[4,] <- tapply.fast(boolean.str,yearly.date.factors,function(idx) { runlength = rle(as.logical(idx)) ; return(suppressWarnings(max(runlength$lengths[runlength$values=="TRUE"],na.rm=TRUE))) } )
        aspect.array[5,] <- tapply.fast(boolean.str,yearly.date.factors,function(idx) { runlength = rle(as.logical(idx)) ; return(sum(runlength$lengths[runlength$values=="TRUE"],na.rm=TRUE)) } )
	aspect.array[2,] <- ifelse(aspect.array[2,]=="-Inf",NaN,aspect.array[2,])
	aspect.array[4,] <- ifelse(aspect.array[4,]=="-Inf",NaN,aspect.array[4,])
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
