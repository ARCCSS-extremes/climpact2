# ------------------------------------------------ #
# ClimPACT2
# University of New South Wales
# ------------------------------------------------ #
#
# This file constitutes version 2 of ClimPACT, an R package designed to calculate the ETCCDI and ETSCI indices (and some others). ClimPACT2 combines the CRAN package climdex.pcic, developed by the 
# Pacific Climate Impacts Consortium, with additional code containing ETSCI indices and a netCDF loader, which was developed at the University of New South Wales. Science users are intended to access 
# the indices through the climpact.loader function contained in this file. This function accepts gridded netCDF files of tmin, tmax and precip and produces gridded datasets of the requested indices. 
# Conversely, non-specialists are able to use the GUI (accessed by sourcing climpact2.GUI.r) to calculate point data from ASCII files.
#
# This package is available on github https://github.com/ARCCSS-extremes/climpact2. See this site for specific version history.
# 
# nherold, December 2015.

# Nullify some objects to suppress spurious warning messages
caltype <<- nc_tsmin <<- nc_tsmax <<- nc_prec <<- yeardate <<- origin <<- latstr <<- latdim <<- londim <<- j <<- tsmin <<- tsmax <<- prec <<- tsmintime <<- tsmaxtime <<- prectime <<- missingval <<- cicompile <<- tminqtiles_array <<- tmaxqtiles_array <<-  
tminqtiles <<- tnames <<- tmaxqtiles <<- tavgqtiles <<- precipqtiles <<- pnames <<- tavgqtileshw <<- tminqtileshw <<- tmaxqtileshw <<- tminraw <<- tmaxraw <<- precraw <<- timeraw <<- northern.hemisphere <<- tavgqtiles_array <<- precipqtiles_array <<- 
tavg05_95p <<- tavg0595qtiles <<- NULL

# Load global libraries and enable compilation.
library(climdex.pcic)
library(PCICt)
library(SPEI,quietly=TRUE)
options(warn=1)
software_id = "1.1.2"

# climpact.loader
#
# This function reads in one to three netCDF files and calculates the specified indices.
#
# INPUT
#  tsminfile: min temperature netCDF file. Must be gridded daily data.
#  tsmaxfile: max temperature netCDF file. Must be gridded daily data.
#  precfile: daily precipitation netCDF file. Must be gridded daily data.
#  tsminname: name of min temperature variable in tsminfile
#  tsmaxname: name of max temperature variable in tsmaxfile
#  precname: name of daily precipitation variable in precfile
#  timename: name of time variable in input files
#  indices: a list specifying which indices to calculate. See XX for a list of supported indices. Specify "all" to calculate all indices.
#  identifier: an optional string to aid identifying output files (e.g. this may be the particular model/dataset the indices are being calculated on).
#  lonname: name of longitude dimension.
#  latname: name of latitude dimension.
#  baserange: reference period for calculating percentile indices.
#  freq: frequency at which to calculate relevant indices.
#  write_quantiles: boolean specifying whether to write percentiles to a file for later use.
#  quantile_file: netCDF file created from a previous execution of climpact.loader with write_quantiles set to TRUE.
#  cores: specify the number of cores to use for processing. Default is to use one core.
#  tempqtiles: temperature percentiles to calculate.
#  precqtiles: precipitation percentiles to calculate.
#  max.missing.days: maximum missing days under which indices will still be calculated.
#  min.base.data.fraction.present: minimum fraction of data required for a quantile to be calculated for a particular day.
#  output_dir: directory where index files will be created.
#  time_format: if the time variable in tsminfile, tsmaxfile and precfile are not in "units since YYYY-MM-DD" then this parameter must be passed specifying the date format. Uses standard notations as specified in
#	http://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
#  ... additional parameters: any parameters that are defined for specific indices (see the manual) can be specified by 
#      prefixing the index name followed by an underscore. For example, the spells.can.span.years parameter for the climdex index wsdi can be specified by passing wsdi_spells.can.span.years.
#
# OUTPUT
#  A single netCDF file for each index specified in indices. As well as a quantile file if write_quantiles is set to TRUE.
#
climpact.loader <- function(tsminfile=NULL,tsmaxfile=NULL,precfile=NULL,tsminname="tsmin",tsmaxname="tsmax",precname="prec",timename="time",indices=NULL,identifier=NULL,lonname="lon",latname="lat",baserange=c(1961,1990),
freq=c("monthly","annual"),tempqtiles=c(0.1,0.9),precqtiles=c(0.1,0.9),max.missing.days=c(annual=15, monthly=3),min.base.data.fraction.present=0.1,csdin_n=5,csdin_spells.can.span.years=FALSE,wsdin_n=5,wsdin_spells.can.span.years=FALSE,
cdd_spells.can.span.years=TRUE,cwd_spells.can.span.years=TRUE,csdi_spells.can.span.years=FALSE,wsdi_spells.can.span.years=FALSE,ntxntn_n=5,
ntxbntnb_n=5,gslmode="GSL",rx5day_centermean=FALSE,hddheat_n=18,cddcold_n=18,gddgrow_n=10,time_format=NULL,rxnday_n=7,rxnday_center.mean.on.last.day=FALSE,rnnm_threshold=1,
spei_scale=3,spi_scale=c(3,6,12),hwn_n=5,write_quantiles=FALSE,quantile_file=NULL,cores=NULL,output_dir="./",hw_ehf="PA13")
{
# modules needed for parallel and netCDF
        library(ncdf4)
        library(compiler)
        library(foreach)
        library(doParallel)
        library(abind)
        enableJIT(3)

# Read in climate index data. Do indices check.
	indexfile <- "index.master.list"
	usr.indices <- indices
	indexlist <- read.table(indexfile,sep="\t")
        if(is.null(indices)) stop(paste("Must provide a list of indices to calculate. See ",indexfile," for list of valid indices.",sep=""))
        if(all(!indices %in% indexlist[,1],!indices[1]=="all")) stop(paste("One or more indices are unknown. See ",indexfile," for list of valid indices.",sep=""))

	tmin.ind <- c("fd","fd2","fdm2","tr","tnn","tnx","csdi","csdin","tn10p","tn90p")
	tmax.ind <- c("id","su","txx","txn","wsdi","wsdin","tx50p","tx95t","tx10p","tx90p","su30","su35")
	prec.ind <- c("cdd","cwd","r10mm","r20mm","rx1day","rx5day","prcptot","sdii","r95p","r99p","r95ptot","r99ptot","rxnday","rnnmm","spi")
	tmin.tmax.ind <- c("dtr","ntxntn","ntxbntnb","hw","gsl","tm5a","tm5b","tm10a","tm10b","hddheat","cddcold","gddgrow","hw")
	tmin.tmax.prec.ind <- c("spei")


# File and other checks
	if(all(is.null(tsminfile),is.null(tsmaxfile),is.null(precfile))) stop("Must provide at least one filename for tmin, tmax or precip.")
	if(!is.null(quantile_file) && write_quantiles==TRUE) stop("Cannot both write quantiles AND read in quantiles from a file.")
	if(!is.null(cores) && !is.numeric(cores)) stop("cores must be an integer (and should be less than the total number of cores available on your computer).")

# Set constants
	cal <- "gregorian"
	missingval <<- 9.96921e36	# arbitrarily copy NCL's floating point missing value
        assign("cores", cores, envir = .GlobalEnv)
        assign("tsmaxname", tsmaxname, envir = .GlobalEnv)
        assign("tsminname", tsminname, envir = .GlobalEnv)
        exportlist <- c("get.na.mask","dual.threshold.exceedance.duration.index","get.hw.aspects","tapply.fast","indexcompile")

# Load tmin, tmax and prec files and variables. Assumedly this is a memory intensive step for large variables. Way to improve this? Read incrementally?
# Retrieve index information.
	if(usr.indices[1] == "all") indices <- character(0) # && all(!is.null(tsminfile),!is.null(tsmaxfile),!is.null(precfile))) { indices <- as.character(indexlist[,1]) }
        if(!is.null(tsminfile)) { nc_tsmin<<-nc_open(tsminfile); tsmin <<- ncvar_get(nc_tsmin,tsminname) ; refnc<-nc_tsmin ; if(usr.indices[1] == "all") indices <- c(indices,tmin.ind) }
        if(!is.null(tsmaxfile)) { nc_tsmax<<-nc_open(tsmaxfile); tsmax <<- ncvar_get(nc_tsmax,tsmaxname) ; refnc<-nc_tsmax ; if(usr.indices[1] == "all") indices <- c(indices,tmax.ind) }
        if(!is.null(precfile)) { nc_prec<<-nc_open(precfile); prec <<- ncvar_get(nc_prec,precname) ; refnc<-nc_prec ; if(usr.indices[1] == "all") indices <- c(indices,prec.ind) }
	if(all(!is.null(tsminfile),!is.null(tsmaxfile),usr.indices[1] == "all")) indices <- c(indices,tmin.tmax.ind)
	if(all(!is.null(tsminfile),!is.null(tsmaxfile),!is.null(precfile),usr.indices[1] == "all")) indices <- c(indices,tmin.tmax.prec.ind)
        units <- as.character(indexlist[match(indices,indexlist[,1]),2])
        desc <- as.character(indexlist[match(indices,indexlist[,1]),3])

# Convert Kelvin or Fahrenheit to Celcius. And m or cm to mm. This is the only unit conversion done.
	if(!is.null(nc_tsmin)) if (ncatt_get(nc_tsmin,tsminname,"units")[2] == "K" || ncatt_get(nc_tsmin,tsminname,"units")[2] == "degK") tsmin <<- tsmin-273.15
        if(!is.null(nc_tsmax)) if (ncatt_get(nc_tsmax,tsmaxname,"units")[2] == "K" || ncatt_get(nc_tsmax,tsmaxname,"units")[2] == "degK") tsmax <<- tsmax-273.15
        if(!is.null(nc_tsmin)) if (ncatt_get(nc_tsmin,tsminname,"units")[2] == "F") tsmin <<- (tsmin-32)/1.8
        if(!is.null(nc_tsmax)) if (ncatt_get(nc_tsmax,tsmaxname,"units")[2] == "F") tsmax <<- (tsmax-32)/1.8
        if(!is.null(nc_prec)) if (ncatt_get(nc_prec,precname,"units")[2] == "m") prec <<- prec*1000
        if(!is.null(nc_prec)) if (ncatt_get(nc_prec,precname,"units")[2] == "cm") prec <<- prec*10

# Perform basic temp and prec checks
        basic.qc(tsmin,tsmax,prec)

# Set up coordinate variables for writing to netCDF. If irregular grid then create x/y indices, if regular grid read in lat/lon coordinates.
	if(length(dim(ncvar_get(refnc,latname))) > 1) {irregular = TRUE} else {irregular = FALSE}	# determine if irregular

	if(irregular){					# If an irregular grid is being used
	        lat = 1:dim(ncvar_get(refnc,latname))[2]
        	lon = 1:dim(ncvar_get(refnc,lonname))[1]
                londim <- ncdim_def("x", "degrees_east", as.double(lon))	# creates object of class ncdim4
                latdim <- ncdim_def("y", "degrees_north", as.double(lat))
                lon2d = ncvar_get(refnc,lonname)
                lat2d = ncvar_get(refnc,latname)
		exportlist <- c(exportlist,"lat2d")
	} else {					# else regular grid
                lat = ncvar_get(refnc,latname)
                lon = ncvar_get(refnc,lonname)
                londim <- ncdim_def("lon", "degrees_east",lon)
                latdim <- ncdim_def("lat", "degrees_north",lat)
	}

# Get the time
        time <- get.time(refnc,timename,time_format)
        time <- trunc(time,"days")      # truncate dates to only %Y-%m-%d so that last day is not cut off inadvertantly
        yeardate <<- unique(format(time,format="%Y"))		# get unique years
	tmp.seq = seq(as.Date(paste(yeardate[1],"01","01",sep="-")),as.Date(paste(yeardate[length(yeardate)],"12","31",sep="-")),by = "1 day")
	monthdate = unique(format(tmp.seq,format="%Y-%m"))

	# For NARCliM we will use hours since origin. For other datasets (that may not have an origin) we use years or months since the first time step.
	# All time variables based on hours since origin will assume a time of 00:00:00. E.g. "hours since 1949-12-01 00:00:00".
	# Furthermore, monthly time variables will be indexed at the 15th of each month, annual variables on the 1st July each year.
	if(is.null(time_format)) {
		time_att = ncatt_get(refnc,timename,"units")[2]
		origin <<- get.origin(time_att=time_att[[1]])
		months_as_hours = as.numeric(as.Date(paste(monthdate,"-15",sep="")) - as.Date(origin))*24	# convert days to hours
		years_as_hours = as.numeric(as.Date(paste(yeardate,"-07-01",sep="")) - as.Date(origin))*24
	        nmonths = length(months_as_hours)
        	nyears = length(years_as_hours)
	} else {
		nmonths = length(yeardate)*12
		nyears = length(yeardate)
	        ftime = ncvar_get(refnc,timename)
                string = apply(ftime,1,toString)
                origin <<- as.Date(string[1],time_format)
                months_as_hours = as.numeric(as.Date(paste(monthdate,"-15",sep="")) - origin)*24       # convert days to hours
                years_as_hours = as.numeric(as.Date(paste(yeardate,"-07-01",sep="")) - origin)*24
	}

# Create time_bnds for monthly and yearly indices
#	month_time_bnds <- array(NA,c(length(monthdate),2))
#	days_in_months <- c(31,28,31,30,31,30,31,31,30,31,30,31)
#	for (i in 1:length(monthdate)) {
#		whatmonth <- as.numeric(substr(monthdate[i],6,7))
#		add_days <- days_in_months[whatmonth]
#		if(leapdays(as.numeric(substr(monthdate[i],1,4))) == 1 & whatmonth == 2) { add_days <- as.numeric(add_days) + 1 }
#		month_time_bnds[i,1] <- months_as_hours[i] - (difftime(as.Date(paste(monthdate[i],"15",sep="-")),as.Date(paste(monthdate[i],"01",sep="-")),units="hours"))
#		month_time_bnds[i,2] <- months_as_hours[i] + (difftime(as.Date(paste(monthdate[i],add_days,sep="-")),as.Date(paste(monthdate[i],"15",sep="-")),units="hours") + 24) # need to add a day to account for 00:00:00 timing.
#	}
#
#	year_time_bnds <- array(NA,c(length(yeardate),2))
#        for (i in 1:length(yeardate)) {
#                year_time_bnds[i,1] <- years_as_hours[i] - (difftime(as.Date(paste(yeardate[i],"07","01",sep="-")),as.Date(paste(yeardate[i],"01","01",sep="-")),units="hours"))
#		year_time_bnds[i,2] <- years_as_hours[i] + (difftime(as.Date(paste(yeardate[i],"12","31",sep="-")),as.Date(paste(yeardate[i],"07","01",sep="-")),units="hours") + 24) # need to add a day to account for 00:00:00 timing.
#	}

	if(!is.null(tsminfile)) { tsmintime <<- time }; if(!is.null(tsmaxfile)) { tsmaxtime <<- time }; if(!is.null(precfile)) { prectime <<- time }

# Read quantiles if provided by user
	if(!is.null(quantile_file)) {
		nc_qtiles = nc_open(quantile_file)
		if(!is.null(ncvar_get(nc_qtiles,"tmin"))) { tminqtiles = ncvar_get(nc_qtiles,"tmin") ; tnames = ncvar_get(nc_qtiles,"tqtile") }
                if(!is.null(ncvar_get(nc_qtiles,"tmax"))) { tmaxqtiles = ncvar_get(nc_qtiles,"tmax") ; tnames = ncvar_get(nc_qtiles,"tqtile") }
                if(!is.null(ncvar_get(nc_qtiles,"tavg"))) { tavgqtiles = ncvar_get(nc_qtiles,"tavg") ; tnames = ncvar_get(nc_qtiles,"tqtile") }
                if(!is.null(ncvar_get(nc_qtiles,"tavg05_95p"))) { tavg0595qtiles = ncvar_get(nc_qtiles,"tavg05_95p") }
                if(!is.null(ncvar_get(nc_qtiles,"prec"))) { precipqtiles = ncvar_get(nc_qtiles,"prec") ; pnames = ncvar_get(nc_qtiles,"pqtile") }
                if(!is.null(ncvar_get(nc_qtiles,"tmin90p_hw"))) { tminqtileshw = ncvar_get(nc_qtiles,"tmin90p_hw") }
                if(!is.null(ncvar_get(nc_qtiles,"tmax90p_hw"))) { tmaxqtileshw = ncvar_get(nc_qtiles,"tmax90p_hw") }
                if(!is.null(ncvar_get(nc_qtiles,"tavg90p_hw"))) { tavgqtileshw = ncvar_get(nc_qtiles,"tavg90p_hw") }
                if(!is.null(ncvar_get(nc_qtiles,"tminraw"))) { tminraw = ncvar_get(nc_qtiles,"tminraw") }
                if(!is.null(ncvar_get(nc_qtiles,"tmaxraw"))) { tmaxraw = ncvar_get(nc_qtiles,"tmaxraw") }
                if(!is.null(ncvar_get(nc_qtiles,"precraw"))) { precraw = ncvar_get(nc_qtiles,"precraw") }
                if(!is.null(ncvar_get(nc_qtiles,"base_time"))) { timeraw = ncvar_get(nc_qtiles,"base_time") }
tavg0595qtiles = aperm(tavg0595qtiles,c(2,3,1))
	}
print(dim(tavg0595qtiles))
print(dim(tavgqtiles))
# Compile climdexinput function for performance.
	cicompile <<- cmpfun(climdexInput.raw)
	exportlist <- c(exportlist,"cicompile")

# Begin main portion of the program. Loop through index list; get index, read variable, calculate index on grid, write out index on gridded netcdf
        print("***************************************")
        print("********* CALCULATING INDICES *********")
	print("***************************************")
        for(a in 1:length(indices)){
	# Fetch and compile index function
	        indexfun = match.fun(paste("climdex",indices[a],sep="."))
		indexcompile = cmpfun(indexfun)

	# Create index call string. For performance reasons we have the following switch command to, among other things, reduce the number of percentiles to calculate for each index.
	# !!!!! It is worth noting here that the below method of calculating a climdex input object for each index was chosen at the time of development because memory issues in R 
	# !!!!! led to substantial problems (e.g. well over 200 GB of memory being used). This is linked to R's penchant for modifying arrays in place. This should be able to be 
	# !!!!! worked around and thus provide a significant boost in compute time. It is hoped a later version of Climpact2 will achieve this.
		options(useFancyQuotes=FALSE)
		indexparam = "cio"

		tempqtiles_tmp <<- tempqtiles ; precqtiles_tmp <<- precqtiles
		if(irregular) { latstr<<-"lat2d[i,j]" } else { latstr<<-"lat[j]" }
		switch(indices[a],
			cdd={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",cdd_spells.can.span.years,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			csdi={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",csdi_spells.can.span.years,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = NULL } },
			cwd={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",cwd_spells.can.span.years,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
	                dtr={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			gsl={indexparam = paste("array(indexcompile(",indexparam,",gsl.mode=",dQuote(gslmode),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			rnnmm={indexparam = paste("array(indexcompile(",indexparam,",threshold=",rnnm_threshold,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			rx1day={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
	                rx5day={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),",center.mean.on.last.day=",rx5day_centermean,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
                        r95ptot={indexparam = paste("array(indexcompile(",indexparam,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = c(0.95) } },
                        r99ptot={indexparam = paste("array(indexcompile(",indexparam,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = c(0.99) } },
                        r95p={indexparam = paste("array(indexcompile(",indexparam,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = c(0.95) } },
                        r99p={indexparam = paste("array(indexcompile(",indexparam,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = c(0.99) } },
	                tn10p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = NULL } },
			tn90p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.9) ; precqtiles_tmp = NULL } },
			tnn={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			tnx={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			tx10p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = NULL } },
                        tx50p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.5) ; precqtiles_tmp = NULL } },
			tx90p={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.9) ; precqtiles_tmp = NULL } },
			txn={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			txx={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			wsdi={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",wsdi_spells.can.span.years,"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.9) ; precqtiles_tmp = NULL } },
                        csdin={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",csdin_spells.can.span.years,",n=",csdin_n,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = c(0.1) ; precqtiles_tmp = NULL } },
                        wsdin={indexparam = paste("array(indexcompile(",indexparam,",spells.can.span.years=",wsdin_spells.can.span.years,",n=",wsdin_n,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = c(0.9) ; precqtiles_tmp = NULL } },
                        ntxntn={indexparam = paste("array(indexcompile(",indexparam,",n=",ntxntn_n,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = c(0.95) ; precqtiles_tmp = NULL } },
                        ntxbntnb={indexparam = paste("array(indexcompile(",indexparam,",n=",ntxbntnb_n,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = c(0.05) ; precqtiles_tmp = NULL } },
			tx95t={indexparam = paste("array(indexcompile(",indexparam,",freq=",dQuote(freq[1]),"))",sep="") ; if(!write_quantiles) {tempqtiles_tmp = c(0.95) ; precqtiles_tmp = NULL } },
                        rxnday={indexparam = paste("array(indexcompile(",indexparam,",center.mean.on.last.day=",rxnday_center.mean.on.last.day,",n=",rxnday_n,"))",sep="") ; 
				if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL }},
			hddheat={indexparam = paste("array(indexcompile(",indexparam,",Tb=",hddheat_n,"))",sep="") ;
                                if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
                        cddcold={indexparam = paste("array(indexcompile(",indexparam,",Tb=",cddcold_n,"))",sep="") ;
                                if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
                        gddgrow={indexparam = paste("array(indexcompile(",indexparam,",Tb=",gddgrow_n,"))",sep="") ;
                                if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			spei={indexparam = paste("array(indexcompile(",indexparam,",scale=",spei_scale,",lat=",latstr,",ref.start=",paste("c(",baserange[1],",1)",sep=""),
				",ref.end=",paste("c(",baserange[2],",1)",sep=""),",basetmin=tnraw",",basetmax=txraw",",baseprec=praw,basetime=btime","))") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
                        spi={indexparam = paste("array(indexcompile(",indexparam,",scale=",spi_scale,",ref.start=",paste("c(",baserange[1],",1)",sep=""),",ref.end=",paste("c(",
				baserange[2],",1)",sep=""),",baseprec=praw,basetime=btime","))") ; if(!write_quantiles) {tempqtiles_tmp = NULL ; precqtiles_tmp = NULL } },
			hw={	indexparam = paste("array(indexcompile(",indexparam,",base.range=c(",baserange[1],",",baserange[2],"),pwindow=",hwn_n,",min.base.data.fraction.present=",
                                min.base.data.fraction.present,",lat=",latstr,",tavg90p=tavg90p",",tn90p=tn90p",",tx90p=tx90p",",tavg05p=tavg05p",",tavg95p=tavg95p",",ehfdef=hw_ehf","))",sep="")
				if(!write_quantiles) {tempqtiles_tmp = c(0.1,0.9) ; precqtiles_tmp = NULL } },
		{ indexparam = paste("array(indexcompile(",indexparam,"))",sep="") ; tempqtiles_tmp <- precqtiles_tmp <- NULL } )
		print(paste("diag: index call: ",(eval(indexparam)),sep=""))

	# Determine whether index to be calculated will be daily (currently only for tx95t), monthly or annual
		if(indices[a] == "tx95t") {period = "DAY"} else if (indices[a] == "rxnday" || indices[a] == "spei" || indices[a] == "spi") { period = "MON" } else {
	                if(!is.null(formals(indexfun)$freq)) {
				if(!is.null(freq)){
					if(freq[1] == "monthly") {period = "MON"} else {period = "ANN"}
				} else {period = "MON"}
			} else {period = "ANN"}
		}

	# array size depending on whether index is daily, monthly, annual, heatwave or SPEI/SPI
		if(indices[a] == "hw") { index = array(NA,c(4,5,length(lon),length(lat),nyears)) } 				# additional dimensions of 3,5 for 3 definitions and 5 aspects of heat waves
		else if (indices[a] == "spei" | indices[a] == "spi") { index = array(NA,c(3,length(lon),length(lat),nmonths))} 	# an additional dimension of size 3 for default 3, 6 and 12 month calculations
		else if(period == "MON") {index = array(NA,c(length(lon),length(lat),nmonths))} else if(period == "ANN") { index = array(NA,c(length(lon),length(lat),nyears))} else {index = array(NA,c(length(lon),length(lat),365))}

	# create quantile arrays if quantile file is requested. [lon,lat,days,percentiles,base]
		if(write_quantiles == TRUE) { tminqtiles_array <<- tmaxqtiles_array <<- tavgqtiles_array <<- array(NA,c(length(lon),length(lat),365,length(tempqtiles),2)) 
				precipqtiles_array <<- array(NA,c(length(lon),length(lat),365,length(precqtiles),2)) 
				tavg05_95p_array <<- array(NA,c(length(lon),length(lat),2)) }

        # If quantiles are requested, record and write quantiles. This will only happen once.
                if (write_quantiles == TRUE) { record.quantiles(refnc,timename,time_format,tempqtiles,precqtiles,lat,lon,baserange,identifier,period,londim,latdim,output_dir,time) ; write_quantiles = FALSE }

        # Set up parallel options if required. acomb puts index arrays back together after multiple processors have done their stuff.
                acomb <- function(...) { if(indices[a] == "hw") { abind(..., along=5) } 
			else if (indices[a] == "spei" | indices[a] == "spi") { abind(...,along=4) } 
			else { return(abind(..., along=3)) } }

                if(!is.null(cores)) {
                        cl <- makeCluster(cores)
                        registerDoParallel(cl) }

	# Calculate the index. Parallelise outer loop. Chunking and parrelising inner loop may lead to more efficiencies.
		j = 1
                if(indices[a] == "hw") { test = index[,,,1,] } 
		else if(indices[a] == "spei" | indices[a] == "spi") { test = index[,,1,] }
		else { test = index[,1,] } # create a dummy shell for the parrallel loop.
		#opts <- list(chunkSize=4)
		index <- foreach(j=1:length(lat),.combine='acomb',.export=c(ls(envir=globalenv()),objects(),"get.na.mask","tapply.fast"),.packages=c('climdex.pcic','SPEI')) %dopar% {		# ls(envir=globalenv()),objects()
                        for(i in 1:length(lon)){
				# DO QUANTILE WORK IF NECESSARY
				# If quantiles are provided, create the quantile list to feed climdexinput.raw and make tempqtiles and precqtiles NULL if not already/
		                if(!is.null(quantile_file)) {
		                        quantiles = list()
		                        if(!is.null(tminqtiles)) { 
		                                tminlist=vector("list", length(tnames))
		                                names(tminlist) <- paste("q",tnames,sep="")
		                                for (l in 1:length(tnames)) { tminlist[[l]]=tminqtiles[i,j,,l] }
		                                quantiles$tmin$outbase = tminlist
                                                quantiles$tmin$inbase = tminlist
		                        }
		                        if(!is.null(tmaxqtiles)) {
		                                tmaxlist=vector("list", length(tnames))
		                                names(tmaxlist) <- paste("q",tnames,sep="")
		                                for (l in 1:length(tnames)) { tmaxlist[[l]]=tmaxqtiles[i,j,,l] }
		                                quantiles$tmax$outbase = tmaxlist
                                                quantiles$tmax$inbase = tmaxlist
		                        }
                                        if(!is.null(tavgqtiles)) {
                                                tavglist=vector("list", length(tnames))
                                                names(tavglist) <- paste("q",tnames,sep="")
                                                for (l in 1:length(tnames)) { tavglist[[l]]=tavgqtiles[i,j,,l] }
                                                quantiles$tavg$outbase = tavglist
                                                quantiles$tavg$inbase = tavglist
                                        }
		                        if(!is.null(precipqtiles)) {		
					# precip quantiles do not exist for each day, but for the entire reference period. Due to the noisiness of this variable in time-space.
		                                preclist=vector("list", length(pnames))
		                                names(preclist) <- paste("q",pnames,sep="")
		                                for (l in 1:length(pnames)) { preclist[[l]]=precipqtiles[i,j,l] }
		                                quantiles$prec = preclist
		                        }
#                                        if(!is.null(tavg0595qtiles)) { 
#                                                tavg0595list=vector("list", 2)
#                                                names(tavg0595list) <- paste("q",c("05","95"),sep="")
#                                                for (l in 1:2) { tavg0595list[[l]]=tavg0595qtiles[i,j,l] }
#                                                quantiles$tavg0595 = tavg0595list
#                                        }

					tempqtiles_tmp <- precqtiles_tmp <- NULL
		                } else { quantiles <- NULL }

				# Calculate climdex input object
				cio <<- cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=prec[i,j,],tmin.dates=tsmintime,tmax.dates=tsmaxtime,prec.dates=prectime,prec.qtiles=precqtiles_tmp,
					temp.qtiles=tempqtiles_tmp,quantiles=quantiles,base.range=baserange)
#print(dim(tavg0595qtiles))
				# Call index function and store in different variable if hw, or spei or spi.
				if(indices[a] == "hw") { if(exists("tavgqtileshw")) { tavg90p = tavgqtileshw[i,j,] ; tn90p = tminqtileshw[i,j,] ; tx90p = tmaxqtileshw[i,j,] ; tavg05p = tavg0595qtiles[j,1,i] ; tavg95p = tavg0595qtiles[j,2,i] } 
					else { tavg90p = NULL ; tx90p = NULL ; tn90p = NULL ; tavg05p = NULL ; tavg95p = NULL }
					test[,,i,] = eval(parse(text=indexparam)) }
				else if(indices[a] == "spei") { if(!is.null(precraw)) { tnraw <- tminraw[i,j,] ; txraw <- tmaxraw[i,j,] ; praw <- precraw[i,j,] ; btime <- timeraw } else { tnraw <- txraw <- praw <- btime <- NULL }
					test[,i,] = eval(parse(text=indexparam)) }
                                else if(indices[a] == "spi") { if(!is.null(precraw)) { praw <- precraw[i,j,] ; btime <- timeraw ; tnraw <- txraw <- NULL } else { tnraw <- txraw <- praw <- btime <- NULL }
                                        test[,i,] = eval(parse(text=indexparam)) }
				else if(indices[a] == "gsl") { if(eval(parse(text=latstr)) <= 0) { cio@northern.hemisphere<<-FALSE} else cio@northern.hemisphere<<-TRUE ;test[i,] = eval(parse(text=indexparam))}
				else { test[i,] = eval(parse(text=indexparam)) }
			}

		# return a vector/list/array
			test
		}

	# Transpose dimensions to time,lat,lon
	        if( indices[a] == "hw") { index3d_trans = aperm(index,c(3,5,4,2,1)) }
		else if (indices[a] == "spi" | indices[a] == "spei") { index3d_trans = aperm(index,c(2,4,3,1)) }
		else { index <- ifelse(index=="NaN",NA,index) ; index3d_trans = aperm(index,c(1,3,2)) }

# WRITE DATA TO FILE
# NOTE: ncdf4 seems to only support numeric types for dimensions.
	# create time dimension according to time format
		if(is.null(time_format)) {	# IF no time format supplied, work with hours since some date.
	                if(period == "MON") { timedim <- ncdim_def("time",paste("hours since ",origin," 00:00:00",sep=""),months_as_hours,calendar=toString(caltype)) } 
			else if(period == "ANN") { timedim <- ncdim_def("time",paste("hours since ",origin," 00:00:00",sep=""),years_as_hours,calendar=toString(caltype)) } 
			else { timedim <- ncdim_def("time","days since 0001-01-01",1:365,calendar=toString(caltype)) }
		} else {			# ELSE use number of months or years since first time step.
			if(period == "MON") { timedim <- ncdim_def("time",paste("months since ",yeardate[1],"-01-01",sep=""),0.5:(nmonths-0.5),calendar=toString(caltype)) } 
			else if(period == "ANN"){ timedim <- ncdim_def("time",paste("years since ",yeardate[1],"-01-01",sep=""),0.5:(nyears-0.5),calendar=toString(caltype)) } 
			else { timedim <- ncdim_def("time","days since 0001-01-01",1:365,calendar=toString(caltype)) }
		}

	# create output directory and file name customised for 'n' indices if needed
		system(paste("if ! [ -d ",output_dir," ]; then mkdir ",output_dir,"; fi"))
		switch(indices[a],
			wsdin={ outfile = paste(output_dir,paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste("wsdi",wsdin_n,sep=""),sep="_"),"_bc.nc",sep="") },
			csdin={ outfile = paste(output_dir,paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste("csdi",csdin_n,sep=""),sep="_"),"_bc.nc",sep="") },
			rxnday={ outfile = paste(output_dir,paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste("rx",rxnday_n,"day",sep=""),sep="_"),"_bc.nc",sep="") },
                        ntxntn={ outfile = paste(output_dir,paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste(ntxntn_n,"tx",ntxntn_n,"tn",sep=""),sep="_"),"_bc.nc",sep="") },
                        ntxbntnb={ outfile = paste(output_dir,paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste(ntxbntnb_n,"txb",ntxbntnb_n,"tnb",sep=""),sep="_"),"_bc.nc",sep="") },
                        rnnmm={ outfile = paste(output_dir,paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],paste("r",rnnm_threshold,"mm",sep=""),sep="_"),"_bc.nc",sep="") },
			{ outfile = paste(output_dir,paste("CCRC",identifier,period,yeardate[1],yeardate[length(yeardate)],indices[a],sep="_"),"_bc.nc",sep="") } )

	# create ncdf variable objects
	        if(indices[a] == "hw") { 
			hw_defdim <- ncdim_def("heat_wave_definition","1=tx90,2=tn90,3=EHF",1.0:3.0) ; hw_aspdim <- ncdim_def("heat_wave_aspect","1=HWM,2=HWA,3=HWN,4=HWD,5=HWF",1.0:5.0)
			hwmcdf_tx90 <- ncvar_def("HWM_Tx90","degC",list(londim,latdim,timedim),missingval,longname="Heat wave magnitude",prec="float")
                        hwmcdf_tn90 <- ncvar_def("HWM_Tn90","degC",list(londim,latdim,timedim),missingval,longname="Heat wave magnitude",prec="float")
                        hwmcdf_EHF <- ncvar_def("HWM_EHF","degC^2",list(londim,latdim,timedim),missingval,longname="Heat wave magnitude",prec="float")
                        hwmcdf_ECF <- ncvar_def("CWM_ECF","degC^2",list(londim,latdim,timedim),missingval,longname="Cold wave magnitude",prec="float")

			hwacdf_tx90 <- ncvar_def("HWA_Tx90","degC",list(londim,latdim,timedim),missingval,longname="Heat wave amplitude",prec="float")
                        hwacdf_tn90 <- ncvar_def("HWA_Tn90","degC",list(londim,latdim,timedim),missingval,longname="Heat wave amplitude",prec="float")
                        hwacdf_EHF <- ncvar_def("HWA_EHF","degC^2",list(londim,latdim,timedim),missingval,longname="Heat wave amplitude",prec="float")
                        hwacdf_ECF <- ncvar_def("CWA_ECF","degC^2",list(londim,latdim,timedim),missingval,longname="Cold wave amplitude",prec="float")

			hwncdf_tx90 <- ncvar_def("HWN_Tx90","heat waves",list(londim,latdim,timedim),missingval,longname="Heat wave number",prec="float")
                        hwncdf_tn90 <- ncvar_def("HWN_Tn90","heat waves",list(londim,latdim,timedim),missingval,longname="Heat wave number",prec="float")
                        hwncdf_EHF <- ncvar_def("HWN_EHF","heat waves",list(londim,latdim,timedim),missingval,longname="Heat wave number",prec="float")
                        hwncdf_ECF <- ncvar_def("CWN_ECF","heat waves",list(londim,latdim,timedim),missingval,longname="Cold wave number",prec="float")

			hwdcdf_tx90 <- ncvar_def("HWD_Tx90","days",list(londim,latdim,timedim),missingval,longname="Heat wave duration",prec="float")
                        hwdcdf_tn90 <- ncvar_def("HWD_Tn90","days",list(londim,latdim,timedim),missingval,longname="Heat wave duration",prec="float")
                        hwdcdf_EHF <- ncvar_def("HWD_EHF","days",list(londim,latdim,timedim),missingval,longname="Heat wave duration",prec="float")
                        hwdcdf_ECF <- ncvar_def("CWD_ECF","days",list(londim,latdim,timedim),missingval,longname="Cold wave duration",prec="float")

                        hwfcdf_tx90 <- ncvar_def("HWF_Tx90","days",list(londim,latdim,timedim),missingval,longname="Heat wave frequency",prec="float")
                        hwfcdf_tn90 <- ncvar_def("HWF_Tn90","days",list(londim,latdim,timedim),missingval,longname="Heat wave frequency",prec="float")
			hwfcdf_EHF <- ncvar_def("HWF_EHF","days",list(londim,latdim,timedim),missingval,longname="Heat wave frequency",prec="float")
                        hwfcdf_ECF <- ncvar_def("CWF_ECF","days",list(londim,latdim,timedim),missingval,longname="Cold wave frequency",prec="float")

			varlist <- list(hwmcdf_tx90,hwmcdf_tn90,hwmcdf_EHF,hwacdf_tx90,hwacdf_tn90,hwacdf_EHF,hwncdf_tx90,hwncdf_tn90,hwncdf_EHF,hwdcdf_tx90,hwdcdf_tn90,hwdcdf_EHF,hwfcdf_tx90,hwfcdf_tn90,hwfcdf_EHF,
				hwmcdf_ECF,hwacdf_ECF,hwncdf_ECF,hwdcdf_ECF,hwfcdf_ECF)
		} else if (indices[a] == "spi" | indices[a] == "spei") {
			scale_dim <- ncdim_def("scale","3,6,12 months",1:3)
			spcdf <- ncvar_def(indices[a],"unitless",list(londim,latdim,timedim,scale_dim),missingval,prec="float")
			varlist <- list(spcdf)
		} else { indexcdf <- ncvar_def(indices[a],units[a],list(londim,latdim,timedim),missingval,longname=desc[a],prec="float") ; varlist <- list(indexcdf) }

	# create time_bnds ncdf object
#                bndsdim <- ncdim_def("bnds","none",1:2)
#                time_bndscdf <- ncvar_def("time_bnds",paste("hours since ",origin," 00:00:00",sep=""),list(bndsdim,timedim),missingval,prec="double")
#		varlist[[length(varlist)+1]] <- time_bndscdf

	# remove any previous file
	        system(paste("rm -f ",outfile,sep=""))

	# If irregular grid, create 2D lat/lon coordinate variables
                if(irregular){
			print("WORKING ON IRREGULAR GRID...")
                        loncdf <- ncvar_def(lonname,"degrees_east",list(londim,latdim),missingval,prec="float")
                        latcdf <- ncvar_def(latname,"degrees_north",list(londim,latdim),missingval,prec="float")
			varlist[[length(varlist)+1]] <- loncdf ; varlist[[length(varlist)+1]] <- latcdf
	                tmpout = nc_create(outfile,varlist,force_v4=TRUE)
			ncvar_put(tmpout,loncdf,lon2d) ; ncvar_put(tmpout,latcdf,lat2d)
			rm(loncdf,latcdf)
		} else { tmpout = nc_create(outfile,varlist,force_v4=TRUE) }

        # write out variables and 'coordinates' attribute if on a irregular grid.
        # Includes NARCliM kludge for cell_method attribute.
                if(indices[a] == "hw") { ncvar_put(tmpout,hwmcdf_tx90,index3d_trans[,,,1,1]) ; ncvar_put(tmpout,hwmcdf_tn90,index3d_trans[,,,1,2]) ; ncvar_put(tmpout,hwmcdf_EHF,index3d_trans[,,,1,3]) ; ncvar_put(tmpout,hwmcdf_ECF,index3d_trans[,,,1,4])
					ncvar_put(tmpout,hwacdf_tx90,index3d_trans[,,,2,1]) ; ncvar_put(tmpout,hwacdf_tn90,index3d_trans[,,,2,2]) ; ncvar_put(tmpout,hwacdf_EHF,index3d_trans[,,,2,3]) ; ncvar_put(tmpout,hwacdf_ECF,index3d_trans[,,,2,4])
					ncvar_put(tmpout,hwncdf_tx90,index3d_trans[,,,3,1]) ; ncvar_put(tmpout,hwncdf_tn90,index3d_trans[,,,3,2]) ; ncvar_put(tmpout,hwncdf_EHF,index3d_trans[,,,3,3]) ; ncvar_put(tmpout,hwncdf_ECF,index3d_trans[,,,3,4])
					ncvar_put(tmpout,hwdcdf_tx90,index3d_trans[,,,4,1]) ; ncvar_put(tmpout,hwdcdf_tn90,index3d_trans[,,,4,2]) ; ncvar_put(tmpout,hwdcdf_EHF,index3d_trans[,,,4,3]) ; ncvar_put(tmpout,hwdcdf_ECF,index3d_trans[,,,4,4])
					ncvar_put(tmpout,hwfcdf_tx90,index3d_trans[,,,5,1]) ; ncvar_put(tmpout,hwfcdf_tn90,index3d_trans[,,,5,2]) ; ncvar_put(tmpout,hwfcdf_EHF,index3d_trans[,,,5,3]) ; ncvar_put(tmpout,hwfcdf_ECF,index3d_trans[,,,5,4])
                        if (irregular) { for (var in varlist) { ncatt_put(tmpout,var,"coordinates","lon lat") } }
                        for (var in varlist) { ncatt_put(tmpout,var,"cell_method","time point values 3600.0 seconds") } }
                else if (indices[a] == "spi" | indices[a] == "spei") {
                        ncvar_put(tmpout,spcdf,index3d_trans[,,,])
                        if (irregular) { ncatt_put(tmpout,spcdf,"coordinates","lon lat") }
                        ncatt_put(tmpout,indices[a],"cell_method","time point values 3600.0 seconds")
                } else { ncvar_put(tmpout,indexcdf,index3d_trans) ; if (irregular) ncatt_put(tmpout,indexcdf,"coordinates","lon lat") ; ncatt_put(tmpout,indices[a],"cell_method","time point values 3600.0 seconds") }

	# write out time_bnds variable
#		if(period=="MON") ncvar_put(tmpout,time_bndscdf,aperm(month_time_bnds,c(2,1)))
#		else if(period=="ANN") ncvar_put(tmpout,time_bndscdf,aperm(year_time_bnds,c(2,1)))
#		ncatt_put(tmpout,time_bndscdf,"calendar","standard")

	# -----------------------#
	# METADATA
        # -----------------------#

        	ncatt_put(tmpout,0,"Climpact2_data_created_on",system("date",intern=TRUE))
	        ncatt_put(tmpout,0,"Climpact2_data_created_by_userid",system("whoami",intern=TRUE))
                ncatt_put(tmpout,0,"Climpact2_contact","nicholas.herold@unsw.edu.au")
                ncatt_put(tmpout,0,"Climpact2_version",software_id)
                ncatt_put(tmpout,0,"Climpact2_R_version",as.character(getRversion()))
                ncatt_put(tmpout,0,"Climpact2_base_period",paste(baserange[1],"-",baserange[2],sep=""))
                ncatt_put(tmpout,0,"Climpact2_github","https://github.com/ARCCSS-extremes/climpact2")

	# write out global attributes for heatwaves
		if(indices[a] == "hw") {
			ncatt_put(tmpout,0,"Climpact2_HWM_definition","Average magnitude of all heat wave events in a season")
			ncatt_put(tmpout,0,"Climpact2_HWA_definition","Hottest day of the hottest heat wave (defined by HWM)")
			ncatt_put(tmpout,0,"Climpact2_HWF_definition","The number of days contributing to heat wave events in a season")
			ncatt_put(tmpout,0,"Climpact2_HWD_definition","The number of days of the longest heat wave event in a season")
			ncatt_put(tmpout,0,"Climpact2_HWN_definition","The number of individual heat wave events in a season")
			ncatt_put(tmpout,0,"Climpact2_Heatwave_summer","Defined as November - March in the Southern Hemisphere and May - September in the Nothern Hemisphere")
			ncatt_put(tmpout,0,"Climpact2_Heatwave_years","Each heatwave time-step refers to the year that the summer of interest began (e.g. 2009 in the Southern Hemisphere means the summer that started in November 2009)")
			if(hw_ehf=="BOM") { ncatt_put(tmpout,0,"Climpact2_Heatwave_years_EHF","EHF (Excess Heat Factor) calculated according to Nairn and Fawcett (2013)") }
			else if (hw_ehf=="PA13") { ncatt_put(tmpout,0,"Climpact2_Heatwave_years_EHF","EHF (Excess Heat Factor) calculated according to Perkins and Alexander (2013), with some modifications (Perkins personal comms)") }
			ncatt_put(tmpout,0,"Climpact2_CWM_definition","Average magnitude of all cold wave events in a season")
			ncatt_put(tmpout,0,"Climpact2_CWA_definition","Coldest day of the coldest cold wave (defined by CWM)")
			ncatt_put(tmpout,0,"Climpact2_CWF_definition","The number of days contributing to cold wave events in a season")
			ncatt_put(tmpout,0,"Climpact2_CWD_definition","The number of days of the longest cold wave event in a season")
			ncatt_put(tmpout,0,"Climpact2_CWN_definition","The number of individual cold wave events in a season")
			ncatt_put(tmpout,0,"Climpact2_ECF_definition","Excess Cold Factor as defined by Nairn and Fawcett (2013)")
		}

	# write out global attributes from input file. Assumes all input files have the same global attributes.
	        globatt <- ncatt_get(refnc,0)
		if(length(globatt)>0) {
		for(i in 1:length(globatt)) { if(names(globatt)[i] == "date" || names(globatt)[i] == "author" || names(globatt)[i] == "contact" || 
			names(globatt)[i] == "creation_date") {} else {ncatt_put(tmpout,0,names(globatt)[i],globatt[[i]])} } }

	# write out coordinate variable attributes from input file. Assumes all input files have the same attributes for their coordinate variables
#		attcopy <- c(latname,lonname)#,timename)
#		for (j in 1:length(attcopy)) {
#			tmpatt <- ncatt_get(refnc,attcopy[j])
#			if(length(tmpatt)>0) { for(i in 1:length(tmpatt)) { if(names(tmpatt)[i] == "_FillValue") {} else ncatt_put(tmpout,attcopy[j],names(tmpatt)[i],tmpatt[[i]]) } }
#		}

	        nc_close(tmpout)

	# do last minute attribute changes in NCO, needs to be done after the ncdf4 file is closed.
	# Not necessary for data and was included for the NARCliM project.
                if(irregular) {system(paste("module load nco; ncks -C -O -x -v x,y,bnds",outfile,outfile,sep=" "))}

        # copy arbitrary variables stored in 'varcopy' from input file to output file
	# Not necessary for data and was included for the NARCliM project.
                varcopy <- c("Rotated_pole")
#                for (j in 1:length(varcopy)) {
#                        if(any(names(refnc$var)==varcopy[j])) {
#                                system(paste("module load nco; ncks -A -v",varcopy[j],refnc$filename,outfile,sep=" "))
#                               tmpnc2 <- ncvar_get(refnc,"Rotated_pole")
#                               tmpvarcdf <- ncvar_def(varcopy[j],"",list(),prec="char")
#                               tmpvarput <- ncvar_put(tmpout,tmpvarcdf,tmpnc2)
#                               tmpatt <- ncatt_get(refnc,varcopy[j])
#                               for(i in 1:length(tmpatt)) { ncatt_put(tmpout,varcopy[j],names(tmpatt)[i],tmpatt[[i]]) }
#                               rm(tmpatt)
#                        }
#                }

        # Report back
                print(paste(outfile," completed.",sep=""))

	# Clean up for next iteration
		suppressWarnings(rm(timedim,indexcdf,tmpout,outfile,index3d_trans))
	        if(exists("cl")) { stopCluster(cl) }
	}
}

# record.quantiles
#
# Calculates quantiles for user-provided dataand writes them to file.
# Records standard ETCCDI percentiles calculated by climdex.pcic.
# Also records 15 day running window quantiles for heatwave indices.
# Also records tmax, tmin and prec for the base period as these are required for future spei/spi calculations.
record.quantiles <- function(nc=NULL,timename=NULL,time_format=NULL,tempqtiles,precqtiles,lat,lon,baserange,identifier,period,londim,latdim,output_dir,time) {
        print("CALCULATING QUANTILES")
# If temp variables exist, prepare arrays to store heat wave quantiles
	if(!is.null(tsmin) && !is.null(tsmax)) { tminqtiles_arrayhw = tminqtiles_array ; tmaxqtiles_arrayhw = tmaxqtiles_array ; tavgqtiles_arrayhw = tavgqtiles_array }

        acomb_qtile <- function(...) { abind(..., along=5) }
        if(!is.null(cores)) {
                cl <- makeCluster(cores)
                registerDoParallel(cl) }

        yeardate2 <- format(time,format="%Y")
        base.time <- time[which(yeardate2 >= baserange[1] & yeardate2 <= baserange[2])]

	tavg05_95p = array(NA,c(length(lon),length(lat),2))	# Will store the 5th and 95 percentiles of tavg for each grid cell. Not using the climdex.pcic method (where a 15 day window is used to derive running percentiles).
        qtilefetch = array(NA,c(3,length(tempqtiles),length(lon),length(lat),365))

        innert = qtilefetch[,,,1,]
	innerthw = array(NA,c(3,1,length(lon),365))	# three variables (tx,tn,tavg), and only 90th percentile for each at each lat/lon
        innertminmaxprec = array(NA,c(3,1,length(lon),length(tsmax[1,1,which(yeardate2 >= baserange[1] & yeardate2 <= baserange[2])])))
        innerp = array(NA,c(1,length(precqtiles),length(lon),1)) #365))

# Record temperatures quantiles
        if(!is.null(tsmin) && !is.null(tsmax)) {
                qtilefetch<-foreach(j=1:length(lat),.combine='acomb_qtile',.export=c(ls(envir=globalenv()),objects(),"get.outofbase.quantiles","cicompile"),.packages='climdex.pcic') %dopar% {
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
                tminqtiles_array <<- tmp[,,,,1] ; tmaxqtiles_array <<- tmp[,,,,2] ; tavgqtiles_array <<- tmp[,,,,3]
                rm(qtilefetch,tmp,innert)
        }

        if(exists("cl")) {
                stopCluster(cl)
                cl <- makeCluster(cores)
                registerDoParallel(cl) }

# Record heatwave temperature quantiles (these are different to the above as they are based on a 15-day moving window, unlike climdex which specifies 5 days). Only need 90th percentiles for tmin,tmax and tavg.
        if(!is.null(tsmin) && !is.null(tsmax)) {
                qtilefetch<-foreach(j=1:length(lat),.combine='acomb_qtile',.export=c(ls(envir=globalenv()),objects(),"get.outofbase.quantiles","cicompile"),.packages='climdex.pcic') %dopar% {
                        for(i in 1:length(lon)){
                                cio = cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=NULL,tmin.dates=tsmintime,tmax.dates=tsmaxtime,prec.dates=NULL,prec.qtiles=NULL,temp.qtiles=tempqtiles_tmp,quantiles=NULL,base.range=baserange)
				tx90p = suppressWarnings(get.outofbase.quantiles(cio@data$tmax,tmin=NULL,prec=NULL,tmax.dates=cio@dates,tmin.dates=NULL,prec.dates=NULL,base.range=baserange,temp.qtiles=c(0.9),prec.qtiles=NULL,n=15))
                                tavg90p_tn90p = suppressWarnings(get.outofbase.quantiles(cio@data$tavg,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=baserange,temp.qtiles=c(0.9),prec.qtiles=NULL,n=15))
                                innerthw[1,1,i,] = tavg90p_tn90p$tmin$outbase$q90
                                innerthw[2,1,i,] = tx90p$tmax$outbase$q90
                                innerthw[3,1,i,] = tavg90p_tn90p$tmax$outbase$q90
                        }
                        innerthw # return the array
                }
                tmp = aperm(qtilefetch,c(3,5,4,2,1))
                tminqtiles_arrayhw = tmp[,,,,1] ; tmaxqtiles_arrayhw = tmp[,,,,2] ; tavgqtiles_arrayhw = tmp[,,,,3]
                rm(qtilefetch,tmp,innerthw)
        }

        if(exists("cl")) {
                stopCluster(cl)
                cl <- makeCluster(cores)
                registerDoParallel(cl) }

# Record 5th and 95th quantile for tavg. Not a running quantile.
        if(!is.null(tsmin) && !is.null(tsmax)) {
                for(j in 1:length(lat)){
                        for(i in 1:length(lon)){
                                cio = cicompile(tmin=tsmin[i,j,],tmax=tsmax[i,j,],prec=NULL,tmin.dates=tsmintime,tmax.dates=tsmaxtime,prec.dates=NULL,prec.qtiles=NULL,temp.qtiles=NULL,quantiles=NULL,base.range=baserange)
                                # get base period indices for daily data. Inefficient to have the following two lines inside the loop.
                                factor.numeric = as.numeric(levels(cio@date.factors$annual))[cio@date.factors$annual]
                                ind = which(factor.numeric >= baserange[1] & factor.numeric <= baserange[2])

                                tavg05_95p[i,j,1] = quantile(cio@data$tavg[ind],0.05,na.rm=TRUE)        # record 5th and 95th percentiles of entire base period
                                tavg05_95p[i,j,2] = quantile(cio@data$tavg[ind],0.95,na.rm=TRUE)
                        }
                }
        }

# Record precipitation quantiles
        if(!is.null(prec)) {
                qtilefetch<-foreach(j=1:length(lat),.combine='acomb_qtile',.export=c(objects(),ls(envir=globalenv()),"get.outofbase.quantiles","cicompile"),.packages='climdex.pcic') %dopar% {
                        for(i in 1:length(lon)){
                                cio = cicompile(tmin=NULL,tmax=NULL,prec=prec[i,j,],tmin.dates=NULL,tmax.dates=NULL,prec.dates=prectime,prec.qtiles=precqtiles_tmp,
                                temp.qtiles=NULL,quantiles=NULL,base.range=baserange)
                                for (l in 1:length(precqtiles)) { innerp[1,l,i,] = cio@quantiles$prec[[l]] }
                        }
                        innerp # return the array
                }
                tmp <- aperm(qtilefetch,c(3,5,4,2,1))
                precipqtiles_array <<- tmp[,,,,1]
		rm(qtilefetch,tmp,innerp)
        }

        if(exists("cl")) { stopCluster(cl) }

# Write quantiles to file
        system(paste("if ! [ -d ",output_dir," ]; then mkdir ",output_dir,"; fi"))
        qfile = paste(output_dir,paste("CCRC",identifier,period,baserange[1],baserange[2],"quantiles",sep="_"),".nc",sep="")
        system(paste("rm -f ",qfile,sep=""))
        tqnames = tempqtiles*100
        pqnames = precqtiles*100

        # create time, quantile and inbase/outbase dimensions
        timedim <- ncdim_def("time","days",1:365) ; tqdim <- ncdim_def("tqtile","unitless",tqnames) ; pqdim <- ncdim_def("pqtile","unitless",pqnames) ; tavgqdim <- ncdim_def("tavgtile","unitless",c(5,95))

        # create another time dimension for the base period
        time <- get.time(nc,timename,time_format)
        yeardate <<- unique(format(time,format="%Y"))           # get unique years
	daydate <- unique(format(time,format="%Y-%m-%d"))
	days_as_hours = as.numeric(as.Date(daydate[which(yeardate2 >= baserange[1] & yeardate2 <= baserange[2])]) - as.Date(origin))*24

        base.years <- yeardate[as.character(yeardate) >= as.character(baserange[1]) & as.character(yeardate) <= as.character(baserange[2])]
        base.time.hours <- days_as_hours
        basetimedim <- ncdim_def("base_time",paste("hours since ",origin," 00:00:00",sep=""),base.time.hours)

        # create variable ncdf objects
        tmincdf = ncvar_def(paste("tmin",sep=""),"degC",list(londim,latdim,timedim,tqdim),missingval,prec="float")
        tmaxcdf = ncvar_def(paste("tmax",sep=""),"degC",list(londim,latdim,timedim,tqdim),missingval,prec="float")
        tavgcdf = ncvar_def(paste("tavg",sep=""),"degC",list(londim,latdim,timedim,tqdim),missingval,prec="float")
        preccdf = ncvar_def(paste("prec",sep=""),"mm/day",list(londim,latdim,pqdim),missingval,prec="float")

        tmincdfhw = ncvar_def(paste("tmin90p_hw",sep=""),"degC",list(londim,latdim,timedim),missingval,prec="float")
        tmaxcdfhw = ncvar_def(paste("tmax90p_hw",sep=""),"degC",list(londim,latdim,timedim),missingval,prec="float")
        tavgcdfhw = ncvar_def(paste("tavg90p_hw",sep=""),"degC",list(londim,latdim,timedim),missingval,prec="float")
        tavg05_95cdf = ncvar_def(paste("tavg05_95p",sep=""),"degC",list(londim,latdim,tavgqdim),missingval,prec="float")

	tminrawcdf = ncvar_def(paste("tminraw",sep=""),"degC",list(londim,latdim,basetimedim),missingval,prec="double")
        tmaxrawcdf = ncvar_def(paste("tmaxraw",sep=""),"degC",list(londim,latdim,basetimedim),missingval,prec="double")
        precrawcdf = ncvar_def(paste("precraw",sep=""),"mm/day",list(londim,latdim,basetimedim),missingval,prec="double")

        qout = nc_create(qfile,list(tmincdf,tmaxcdf,tavgcdf,preccdf,tmincdfhw,tmaxcdfhw,tavgcdfhw,tminrawcdf,tmaxrawcdf,precrawcdf,tavg05_95cdf),force_v4=TRUE)

        # write out data
        ncvar_put(qout,tmincdf,tminqtiles_array) ; ncvar_put(qout,tmaxcdf,tmaxqtiles_array) ; ncvar_put(qout,tavgcdf,tavgqtiles_array) ; ncvar_put(qout,preccdf,precipqtiles_array)
	if(exists("tminqtiles_arrayhw")) { ncvar_put(qout,tmincdfhw,tminqtiles_arrayhw) ; ncvar_put(qout,tmaxcdfhw,tmaxqtiles_arrayhw) ; ncvar_put(qout,tavgcdfhw,tavgqtiles_arrayhw) } # if HW variables exist, write them out.
	if(!is.null(prec)) { ncvar_put(qout,precrawcdf,prec[,,which(yeardate2 >= baserange[1] & yeardate2 <= baserange[2])]) }
	if(!is.null(tsmin)) { ncvar_put(qout,tminrawcdf,tsmin[,,which(yeardate2 >= baserange[1] & yeardate2 <= baserange[2])]) }
	if(!is.null(tsmax)) { ncvar_put(qout,tmaxrawcdf,tsmax[,,which(yeardate2 >= baserange[1] & yeardate2 <= baserange[2])]) }
	if(!is.null(tsmax) && !is.null(tsmin)) { ncvar_put(qout,tavg05_95cdf,tavg05_95p) }

	nc_close(qout)
	print(paste(qfile," created.",sep=""))
        rm(timedim,tqdim,pqdim,tmincdf,tmaxcdf,preccdf,tavgcdfhw,tmaxcdfhw,tmincdfhw,qout,tminrawcdf,tmaxrawcdf,precrawcdf)
}

# get.origin
#
# This function gets the origin date from a time variable's units attribute. Assumes the attribute is structured as "units since YYYY-MM-DD..."
get.origin <- function(time_att=NULL){ return(gsub(",", "", unlist(strsplit(time_att, split=" "))[3]))}

# get.time
#
# This function returns the time dimension of a given netcdf file as a PCICt object
get.time <- function(nc=NULL,timename=NULL,time_format=NULL)
{
	ftime = ncvar_get(nc,timename)
	time_att = ncatt_get(nc,timename,"units")[2]
	caltype <<- ncatt_get(nc,timename,"calendar")[2]

	# Bit of a hack for non-model datasets. Requires user to specify "time_format" in climpact.loader
	if(!is.null(time_format)) {
                string = (apply(ftime,1,toString))
                dates = (as.Date(string,time_format))
                rm(ftime) ; ftime = array(1,length(dates)) ; ftime = (as.character(dates))

                split = substring(time_format, seq(1,nchar(time_format),2), seq(2,nchar(time_format),2))
                time_format = paste(split[1],split[2],split[3],sep="-")
                return(as.PCICt(ftime,cal=caltype[[1]],format=time_format))
	} else {
                if(grepl("hours",time_att)) { ftime=ftime*60*60 } # dates.tmp = as.Date(ftime/24,origin=get.origin(time_att=time_att[[1]])) }
                if(grepl("days",time_att)) { ftime=ftime*86400 } #dates.tmp = as.Date(ftime,origin=get.origin(time_att=time_att[[1]])) }

                origin.pcict <- as.PCICt(get.origin(time_att=time_att[[1]]),cal=caltype[[1]])
                dat <- origin.pcict+(ftime)
#                dates.tmp = as.Date(ftime/86400,origin=get.origin(time_att=time_att[[1]]),format="%Y-%m-%d")
#                dat <- as.PCICt(as.character(dates.tmp),cal=caltype[[1]])

                return(dat)
	}
}

# basic.qc
#
# This function checks for invalid and unfeasible values
# Specifically, check for
#  - negative precip
#  - abs(temp) > 70
#  
basic.qc <- function(tmin=NULL,tmax=NULL,prec=NULL)
{
	lowert = -100
	uppert = 100
	upperprec = 5000
	if(!is.null(tmin)) if (any(!is.na(tmin) & tmin < lowert & tmin > uppert)) stop(paste("tmin data has values < ",lowert," and/or > ",uppert,sep=""))
        if(!is.null(tmax)) if (any(!is.na(tmax) & tmax < lowert & tmax > uppert)) stop(paste("tmax data has values < ",lowert," and/or > ",uppert,sep=""))
        if(!is.null(prec)) if (any(!is.na(prec) & prec < 0 & prec > upperprec)) stop(paste("prec data has values < 0 and/or > ",upperprec,sep=""))
}

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
climdex.wsdin <- function(ci, spells.can.span.years=FALSE,n=5) { stopifnot(!is.null(ci@data$tmax) || !is.null(ci@quantiles$tmax)); return(threshold.exceedance.duration.index(ci@data$tmax, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q90, ">", spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual'], min.length=n) * ci@namasks$annual$tmax) }

# csdin
# Annual count of days with at least n consecutive days when TN<10th percentile where n>= 2 (and max 10)
# same as climdex.csdi except user specifies number of consecutive days
climdex.csdin <- function(ci, spells.can.span.years=FALSE,n=5) { stopifnot(!is.null(ci@data$tmin) || !is.null(ci@quantiles$tmin)); return(threshold.exceedance.duration.index(ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmin$outbase$q10, "<", spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual'], min.length=n) * ci@namasks$annual$tmin) }

# tm5a
# Annual count when TM >= 5ºC
# same as climdex.tr except >= 5C
climdex.tm5a <- function(ci) { stopifnot(!is.null(ci@data$tavg)); return(number.days.op.threshold(ci@data$tavg, ci@date.factors$annual, 5, ">=") * ci@namasks$annual$tavg) }

# tm5b
# Annual count when TM < 5ºC
# same as climdex.tr except < 5C
climdex.tm5b <- function(ci) { stopifnot(!is.null(ci@data$tavg)); return(number.days.op.threshold(ci@data$tavg, ci@date.factors$annual, 5, "<") * ci@namasks$annual$tavg) }

# tm10a
# Annual count when TM >= 10ºC
# same as climdex.tr except >= 10C
climdex.tm10a <- function(ci) { stopifnot(!is.null(ci@data$tavg)); return(number.days.op.threshold(ci@data$tavg, ci@date.factors$annual, 10, ">=") * ci@namasks$annual$tavg) }

# tm10b
# Annual count when TM < 10ºC
# same as climdex.tr except < 10C
climdex.tm10b <- function(ci) { stopifnot(!is.null(ci@data$tavg)); return(number.days.op.threshold(ci@data$tavg, ci@date.factors$annual, 10, "<") * ci@namasks$annual$tavg) }

# su30
# Annual count when TX >= 30ºC
# same as climdex.tr except >= 30C
climdex.su30 <- function(ci) { stopifnot(!is.null(ci@data$tmax)); return(number.days.op.threshold(ci@data$tmax, ci@date.factors$annual, 30, ">=") * ci@namasks$annual$tmax) }

# su35
# Annual count when TX >= 35ºC
# same as climdex.tr except >= 35C
climdex.su35 <- function(ci) { stopifnot(!is.null(ci@data$tmax)); return(number.days.op.threshold(ci@data$tmax, ci@date.factors$annual, 35, ">=") * ci@namasks$annual$tmax) }

# HDDheat
# Annual sum of Tb-TM (where Tb is a user-defined location-specific base temperature and TM < Tb). Recomputes climdex input object to re-test newly created tavg array for NA criteria.
climdex.hddheat <- function(ci,Tb=18) { 
        stopifnot(is.numeric(Tb),!is.null(ci@data$tavg))
	Tbarr = array(Tb,length(ci@data$tavg))
	tavg.tmp <- ci@data$tavg 
	tavg.tmp = ifelse(tavg.tmp >= Tbarr,NaN,tavg.tmp)
        tavg.tmp = ifelse(is.na(tavg.tmp),NaN,tavg.tmp)
	return(tapply.fast(Tbarr - tavg.tmp,ci@date.factors$annual,sum,na.rm=TRUE)*ci@namasks$annual$tavg)}

# CDDcold
# Annual sum of TM-Tb (where Tb is a user-defined location-specific base temperature and TM > Tb)
climdex.cddcold <- function(ci,Tb=18) { 
        stopifnot(is.numeric(Tb),!is.null(ci@data$tavg))
	Tbarr = array(Tb,length(ci@data$tavg))
        tavg.tmp <- ci@data$tavg
        tavg.tmp = ifelse(tavg.tmp <= Tbarr,NaN,tavg.tmp)
        tavg.tmp = ifelse(is.na(tavg.tmp),NaN,tavg.tmp)
	return(tapply.fast(tavg.tmp - Tbarr,ci@date.factors$annual,sum,na.rm=TRUE)*ci@namasks$annual$tavg)}

# GDDgrow
# Annual sum of TM-Tb (where Tb is a user-defined location-specific base temperature and TM > Tb)
climdex.gddgrow <- function(ci,Tb=10) {
        stopifnot(is.numeric(Tb),!is.null(ci@data$tavg))
	Tbarr = array(Tb,length(ci@data$tavg))
        tavg.tmp <- ci@data$tavg
        tavg.tmp = ifelse(tavg.tmp <= Tbarr,NaN,tavg.tmp)
        tavg.tmp = ifelse(is.na(tavg.tmp),NaN,tavg.tmp)
	return(tapply.fast(tavg.tmp - Tbarr,ci@date.factors$annual,sum,na.rm=TRUE)*ci@namasks$annual$tavg)}

# Rxnday
# Monthly maximum consecutive n-day precipitation (up to a maximum of 10)
# Same as rx5day except specifying a monthly frequency and accepting user specified number of consecutive days
climdex.rxnday <- function(ci, center.mean.on.last.day=FALSE,n=7,freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$prec),is.numeric(n),is.logical(center.mean.on.last.day)); return(nday.consec.prec.max(ci@data$prec, ci@date.factors[[match.arg(freq)]], n, center.mean.on.last.day) * ci@namasks[[match.arg(freq)]]$prec) }

# r95p as per Donat et al. (2013). This is the same as r95ptot in climdex and will need correcting in that package
climdex.r95p <- function(ci) { stopifnot(!is.null(ci@data$prec),!is.null(ci@quantiles$prec)); return(total.precip.op.threshold(ci@data$prec, ci@date.factors$annual, ci@quantiles$prec['q95'], ">") * ci@namasks$annual$prec) }

# r99p as per Donat et al. (2013). This is the same as r99ptot in climdex and will need correcting in that package
climdex.r99p <- function(ci) { stopifnot(!is.null(ci@data$prec),!is.null(ci@quantiles$prec)); return(total.precip.op.threshold(ci@data$prec, ci@date.factors$annual, ci@quantiles$prec['q99'], ">") * ci@namasks$annual$prec) }

# r95ptot
# This function replaces an identically named function in the climdex.pcic package. This is the correct definition.
climdex.r95ptot <- function(ci) { stopifnot(!is.null(ci@data$prec),!is.null(ci@quantiles$prec)); prcptot <- total.precip.op.threshold(ci@data$prec, ci@date.factors$annual, 1, ">=") * ci@namasks$annual$prec
		r95p <- total.precip.op.threshold(ci@data$prec, ci@date.factors$annual, ci@quantiles$prec['q95'], ">") * ci@namasks$annual$prec
		return(100*r95p/prcptot) }

# r99ptot
# This function replaces an identically named function in the climdex.pcic package. This is the correct definition.
climdex.r99ptot <- function(ci) { stopifnot(!is.null(ci@data$prec),!is.null(ci@quantiles$prec)); prcptot <- total.precip.op.threshold(ci@data$prec, ci@date.factors$annual, 1, ">=") * ci@namasks$annual$prec
                r99p <- total.precip.op.threshold(ci@data$prec, ci@date.factors$annual, ci@quantiles$prec['q99'], ">") * ci@namasks$annual$prec
                return(100*r99p/prcptot) }

# tx95t
# Value of 95th percentile of TX
climdex.tx95t <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$tmax),!is.null(ci@quantiles$tmax)); return(ci@quantiles$tmax$outbase$q95) }

# tx50p
# Percentage of days of days where TX>50th percentile
# same as climdex.tx90p, except for 50th percentile
climdex.tx50p <- function(ci, freq=c("monthly", "annual")) {
	stopifnot(!is.null(ci@data$tmax),!is.null(ci@quantiles$tmax$outbase$q50))
	return(percent.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q50, ci@quantiles$tmax$inbase$q50, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax) }

# ntxntn
# Annual count of n consecutive days where both TX > 95th percentile and TN > 95th percentile, where n >= 2 (and max of 10)
# This function needs the new function dual.threshold.exceedance.duration.index, which was based on threshold.exceedance.duration.index
climdex.ntxntn <- function(ci, n=5) { 
	stopifnot(!is.null(ci@data$tmax),!is.null(ci@quantiles$tmax),!is.null(ci@data$tmin),!is.null(ci@quantiles$tmin))
	return(dual.threshold.exceedance.duration.index(ci@data$tmax, ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q95,ci@quantiles$tmin$outbase$q95, 
		">",">", n=n, max.missing.days=ci@max.missing.days['annual']) * ci@namasks$annual$tmax) }

# ntxbntnb
# Annual count of n consecutive days where both TX < 5th percentile and TN < 5th percentile, where n >= 2 (and max of 10)
# This function needs the new function dual.threshold.exceedance.duration.index, which was based on threshold.exceedance.duration.index
climdex.ntxbntnb <- function(ci, n=5) {
        stopifnot(!is.null(ci@data$tmax),!is.null(ci@quantiles$tmax),!is.null(ci@data$tmin),!is.null(ci@quantiles$tmin))
        return(dual.threshold.exceedance.duration.index(ci@data$tmax, ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q5,ci@quantiles$tmin$outbase$q5,
                "<","<", n=n, max.missing.days=ci@max.missing.days['annual']) * ci@namasks$annual$tmax) }

# prcptot
# Modified from climdex.pcic to calculate monthly or annual values
climdex.prcptot <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$prec)); return(total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1, ">=") * ci@namasks[[match.arg(freq)]]$prec) }

climdex.mean.temp <- function(cio,freq=frequency) { stopifnot(!is.null(cio@data$tmean)); return(suppressWarnings(tapply.fast(cio@data$tmean, cio@date.factors[[match.arg(freq)]], mean, na.rm=TRUE)) * cio@namasks[[match.arg(freq)]]$tmin * cio@namasks[[match.arg(freq)]]$tmax) }

climdex.mean.min.temp <- function(cio,freq=frequency) { stopifnot(!is.null(cio@data$tmin)); return(suppressWarnings(tapply.fast(cio@data$tmin, cio@date.factors[[match.arg(freq)]], mean, na.rm=TRUE)) * cio@namasks[[match.arg(freq)]]$tmin) }

climdex.mean.max.temp <- function(cio,freq=frequency) { stopifnot(!is.null(cio@data$tmax)); return(suppressWarnings(tapply.fast(cio@data$tmax, cio@date.factors[[match.arg(freq)]], mean, na.rm=TRUE)) * cio@namasks[[match.arg(freq)]]$tmax) }

# dual.threshold.exceedance.duration.index
# calculates the number of n consecutive days where op1 and op2 operating on daily.temp1 and daily.temp2 respectively are satisfied.
dual.threshold.exceedance.duration.index <- function(daily.temp1, daily.temp2, date.factor, jdays, thresholds1, thresholds2, op1=">", op2=">", n, max.missing.days) {
	stopifnot(is.numeric(c(daily.temp1,daily.temp2, thresholds1,thresholds2, n)), is.factor(date.factor),is.function(match.fun(op1)),is.function(match.fun(op2)),n > 0,length(daily.temp1)==length(daily.temp2))
	f1 <- match.fun(op1)
	f2 <- match.fun(op2)
	na.mask1 <- get.na.mask(is.na(daily.temp1 + thresholds1[jdays]), date.factor, max.missing.days)
	na.mask2 <- get.na.mask(is.na(daily.temp2 + thresholds2[jdays]), date.factor, max.missing.days)
	na.mask_combined = na.mask1 & na.mask2
	
	return(tapply.fast(1:length(daily.temp1), date.factor, function(idx) {
	      periods1 = f1(daily.temp1[idx], thresholds1[jdays[idx]])
	      periods2 = f2(daily.temp2[idx], thresholds2[jdays[idx]])
	      periods_combined = select.blocks.gt.length(periods1 & periods2,n)
	
	      # only consider events as separate if they are separated by more than k days.
	      k <- 3
	      invert_periods_combined = !periods_combined					# make TRUE = FALSE and vice versa
	      invert_periods_combined = select.blocks.gt.length(invert_periods_combined,k)	# make any run of k or less days of 'TRUE' values (i.e. FALSE) equal FALSE.
	      periods_combined = !invert_periods_combined					# invert array again.
	      runlength = rle(periods_combined)
	      return(length(runlength$lengths[runlength$values==TRUE])) }) * na.mask_combined)
}

# SPEI. From the SPEI CRAN package.
# INPUT:
#    - climdex input object
#    - scale
#    - kernal
#    - distribution
#    - fit
#    - na.rm
#    - ref.start
#    - ref.end
#    - lat: latitude of data. Used to estimate PET.
#    - basetmin: daily tmin values for the base period. This is passed to the function in the case where reference data needs to be calculated but is not included in the daily data in the climdex input object (i.e. is from another dataset).
#    - basetmax: daily tmax values for the base period. Needed as above.
#    - baseprec: daily precipitation values for the base period. Needed as above.
#    - basetime: daily dates for the base period. Needed as above.
# OUTPUT:
#    - a monthly (as per the index definition) time-series of SPEI values.
climdex.spei <- function(ci,scale=c(3,6,12),kernal=list(type='rectangular',shift=0),distribution='log-Logistic',fit='ub-pwm',ref.start=NULL,ref.end=NULL,lat=NULL,basetmin=NULL,basetmax=NULL,baseprec=NULL,basetime=NULL) { 
        scale <- c(3,6,12)      # hard-coded for Climpact2 definition.
        stopifnot(is.numeric(scale),all(scale>0),!is.null(ci@data$prec))
        if(is.null(ci@data$tmin) | is.null(ci@data$tmax) | is.null(ci@data$prec)) stop("climdex.spei requires tmin, tmax and precip.")
	if(!is.null(basetime)) computefuture = TRUE else computefuture = FALSE

        ts.start <- c(as.numeric(format(ci@dates[1],format="%Y")),1)
        ts.end <- c(as.numeric(format(ci@dates[length(ci@dates)],format="%Y")),12)

# If using a base period from a previous time series:
#  Concatenate base period tmin, tmax and prec that is passed by user. Beginning of the time series will have base period data, end of the time series will have data from ci, any data in the middle will remain NA.
#  This is done because SPEI needs the base period data to use as a reference and does not have an option to read this in separately. Thus, we construct a synthetic time series that dates from the base period to the end of the 
#  current periods' data (i.e. the data in the 'ci' object).
# If the data in 'ci' includes the base period (computefuture is false) then skip this step.
        if(computefuture){
	# construct dates
		beg = as.Date(paste(ref.start[1],"01","01",sep="-"))
		end = as.Date(paste(ts.end[1],"12","31",sep="-"))
		dat.seq = seq(beg,end,by = "1 day")
		diffdat = (dat.seq - as.Date(origin))*86400
		spidates = as.PCICt(as.numeric(diffdat),cal="gregorian",origin)

		spitmin <- spitmax <- spiprec <- spifactor <- array(NA,length(spidates))
		spitmin[1:length(basetmin)] = basetmin
	        spitmax[1:length(basetmax)] = basetmax
	        spiprec[1:length(baseprec)] = baseprec
	
		spitmin[(length(spitmin)-length(ci@data$tmin)+1):length(spitmin)] = ci@data$tmin
	        spitmax[(length(spitmax)-length(ci@data$tmax)+1):length(spitmax)] = ci@data$tmax
	        spiprec[(length(spiprec)-length(ci@data$prec)+1):length(spiprec)] = ci@data$prec
	        spifactor = factor(format(spidates,format="%Y-%m"))
	
	# change ts.start when doing future runs since SPEI can't handle ref.start values after ts.end values, I think!
		ts.start = ref.start
	} else {
		spitmin = ci@data$tmin
		spitmax = ci@data$tmax
		spiprec = ci@data$prec
		spifactor = ci@date.factors$monthly
	}

# get monthly means of tmin and tmax. And monthly total precip.
        tmax_monthly <- as.numeric(tapply.fast(spitmax,spifactor,mean,na.rm=TRUE))
        tmin_monthly <- as.numeric(tapply.fast(spitmin,spifactor,mean,na.rm=TRUE))
        prec_sum <- as.numeric(tapply.fast(spiprec,spifactor,function(x) { if(all(is.na(x))) { return(NA) } else { return(sum(x,na.rm=TRUE)) } } ))	# Needed this function since summing a series of NA with na.rm = TRUE results in zero instead of NA.

	tmax_monthly[tmax_monthly=="NaN"] <- NA
	tmin_monthly[tmin_monthly=="NaN"] <- NA

# calculate PET
        pet <- hargreaves(tmin_monthly,tmax_monthly,lat=lat,Pre=prec_sum,na.rm=TRUE)

# calculate SPEI
        x <- array(NA,c(3,length(pet)))
        for (i in 1:length(x[,1])) {
                spei_col <- spei(ts(prec_sum-pet,freq=12,start=ts.start,end=ts.end),scale=scale[i],ref.start=ref.start,ref.end=ref.end,distribution=distribution,fit=fit,kernal=kernal,na.rm=TRUE)
                tmpvar <- spei_col$fitted

        # remove NA, -Inf and Inf values which most likely occur due to unrealistic values in P or PET. This almost entirely occurs in ocean regions and varies depending on the fitting distribution used.
#                tmpvar[is.na(tmpvar)] <- NaN
#                tmpvar <- ifelse(tmpvar=="-Inf",-2.33,tmpvar)
#                tmpvar <- ifelse(tmpvar=="Inf",2.33,tmpvar)
                tmpvar <- ifelse(tmpvar=="-Inf",NA,tmpvar)
                tmpvar <- ifelse(tmpvar=="Inf",NA,tmpvar)

                tmpvar <- ifelse(tmpvar=="NaNf",NA,tmpvar)
                tmpvar <- ifelse(tmpvar=="NaN",NA,tmpvar)

                x[i,] <- tmpvar
        }
        rm(tmpvar)

# - Strip back off all data not part of the original time series.
# - Another kludge here relates to an ostensible bug in the SPEI function. When SPEI is fed a series of NA values followed by valid data, it returns values of SPEI/SPI for those NA values, when it shouldn't.
#    The author has been alerted to this problem. But this means that when a synthetic time series has been made for scenarios using reference data from a different dataset, the initial SPEI/SPI values need
#    to be manually removed. The first 2, 5 and 11 values for each final time series needs NA'ing, corresponding to 3, 6 and 12 month calculation periods.
	if(computefuture) {
		x <- x[,(length(x[1,])-length(unique(ci@date.factors$monthly))+1):length(x[1,])]
		for(i in 1:length(scale)) { x[i,1:(scale[i]-1)] <- NA }
	}

        return((x))
}

# SPI. From the SPEI CRAN package.
# INPUT:
#    - climdex input object
#    - scale
#    - kernal
#    - distribution
#    - fit
#    - na.rm
#    - ref.start
#    - ref.end
#    - baseprec: daily precipitation values for the base period. This is passed to the function in the case where reference data needs to be calculated but is not included in the daily data in the climdex input object (i.e. is from another dataset).
#    - basetime: daily dates for the base period. Needed as above.
# OUTPUT:
#    - a monthly (as per the index definition) time-series of SPI values.
climdex.spi <- function(ci,scale=c(3,6,12),kernal=list(type='rectangular',shift=0),distribution='Gamma',fit='ub-pwm',ref.start=NULL,ref.end=NULL,lat=NULL,baseprec=NULL,basetime=NULL) {
        scale <- c(3,6,12)      # hard-coded for Climpact2 definition.
        stopifnot(is.numeric(scale),all(scale>0),!is.null(ci@data$prec))
        if(is.null(ci@data$prec)) stop("climdex.spi requires precip.")
        if(!is.null(basetime)) computefuture = TRUE else computefuture = FALSE

        ts.start <- c(as.numeric(format(ci@dates[1],format="%Y")),1)
        ts.end <- c(as.numeric(format(ci@dates[length(ci@dates)],format="%Y")),12)

# If using a base period from a previous time series:
#  Concatenate base period tmin, tmax and prec that is passed by user. Beginning of the time series will have base period data, end of the time series will have data from ci, any data in the middle will remain NA.
#  This is done because SPEI needs the base period data to use as a reference and does not have an option to read this in separately. Thus, we construct a synthetic time series that dates from the base period to the end of the 
#  current periods' data (i.e. the data in the 'ci' object).
# If the data in 'ci' includes the base period (computefuture is false) then skip this step.
        if(computefuture){
                # construct dates
                beg = as.Date(paste(ref.start[1],"01","01",sep="-"))
                end = as.Date(paste(ts.end[1],"12","31",sep="-"))
                dat.seq = seq(beg,end,by = "1 day")
                diffdat = (dat.seq - as.Date(origin))*86400
                spidates = as.PCICt(as.numeric(diffdat),cal="gregorian",origin)

                spiprec <- spifactor <- array(NA,length(spidates))
                spiprec[1:length(baseprec)] = baseprec

                spiprec[(length(spiprec)-length(ci@data$prec)+1):length(spiprec)] = ci@data$prec
                spifactor = factor(format(spidates,format="%Y-%m"))

        # change ts.start when doing future runs since SPEI can't handle ref.start values after ts.end values, I think!
                ts.start = ref.start
        } else {
                spitmin = ci@data$tmin
                spitmax = ci@data$tmax
                spiprec = ci@data$prec
                spifactor = ci@date.factors$monthly
        }

# get monthly total precip.
        prec_sum <- as.numeric(tapply.fast(spiprec,spifactor,sum,na.rm=TRUE))

# calculate spi
        x <- array(NA,c(3,length(prec_sum)))
        for (i in 1:3) {
                spi_col <- spi(ts(prec_sum,freq=12,start=ts.start,end=ts.end),scale=scale[i],ref.start=ref.start,ref.end=ref.end,distribution=distribution,fit=fit,kernal=kernal,na.rm=TRUE)
                tmpvar <- (spi_col$fitted)

        # remove NA, -Inf and Inf values which most likely occur due to unrealistic values in P. This almost entirely occurs in ocean regions and varies depending on the fitting distribution used.
#                tmpvar[is.na(tmpvar)] = NaN
#                tmpvar <- ifelse(tmpvar=="-Inf",-2.33,tmpvar)
#                tmpvar <- ifelse(tmpvar=="Inf",2.33,tmpvar)
                tmpvar <- ifelse(tmpvar=="-Inf",NA,tmpvar)
                tmpvar <- ifelse(tmpvar=="Inf",NA,tmpvar)

                tmpvar <- ifelse(tmpvar=="NaNf",NA,tmpvar)
                tmpvar <- ifelse(tmpvar=="NaN",NA,tmpvar)

                x[i,] <- tmpvar
        }
        rm(tmpvar)

# - Strip back off all data not part of the original time series.
# - Another kludge here relates to an ostensible bug in the SPEI function. When SPEI is fed a series of NA values followed by valid data, it returns values of SPEI/SPI for those NA values, when it shouldn't.
#    The author has been alerted to this problem. But this means that when a synthetic time series has been made for scenarios using reference data from a different dataset, the initial SPEI/SPI values need
#    to be manually removed. The first 2, 5 and 11 values for each final time series needs NA'ing, corresponding to 3, 6 and 12 months calculation periods.
        if(computefuture) {
                x <- x[,(length(x[1,])-length(unique(ci@date.factors$monthly))+1):length(x[1,])]
                for(i in 1:length(scale)) { x[i,1:(scale[i]-1)] <- NA }
        }

        return((x))
}

# hw
# Calculate heat wave indices. From Perkins and Alexander (2013) with slight modifications (personal comms Perkins 2015).
# INPUT:
#    - climdex input object
#    - base range: a pair of integers indicating beginning and ending year of base period.
#    - pwindow: number of days to apply a moving window for calculating percentiles. Hard-coded to 15 currently to ensure user does not deviate from definitions.
#    - min.base.data.fraction.present: minimum fraction of data required to calculate percentiles.
#    - lat: latitude of current grid cell (required for determining hemisphere).
#    - tavg90p: mean temperature 90th percentiles for base period. Only when base period is outside of the current data.
#    - tn90p: minimum temperature 90th percentiles for base period. Only when base period is outside of the current data.
#    - tx90p: maximum temperature 90th percentiles for base period. Only when base period is outside of the current data.
# OUTPUT: This function will return a 3D dataset of dimensions [definition,aspect,years], with corresponding lengths [3,5,nyears].
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
climdex.hw <- function(ci,base.range=c(1961,1990),pwindow=15,min.base.data.fraction.present,lat,tavg90p=NULL,tn90p=NULL,tx90p=NULL,tavg05p=NULL,tavg95p=NULL,ehfdef="PA13") {
        stopifnot(!is.null(lat),!is.null(ci@data$tmin),!is.null(ci@data$tmax))
# step 1. Get data needed for the three definitions of a heat wave. 
        # recalculate tavg here to ensure it is based on tmax/tmin. Then get 15 day moving windows of percentiles.
        tavg = (ci@data$tmax + ci@data$tmin)/2

        if(any(is.null(tavg90p),is.null(tx90p),is.null(tn90p))) { 
			# retrieve indices corresponding to base period, for calculation of quantiles for ECF and EHF_BOM
			factor.numeric = as.numeric(levels(ci@date.factors$annual))[ci@date.factors$annual]
			ind = which(factor.numeric >= base.range[1] & factor.numeric <= base.range[2])
                if(ehfdef == "BOM") {
                        tavg95p <- quantile(tavg[ind],0.95,na.rm=TRUE)
                }
                # need to reference 'tmax' data slot in tavg90p because of naming convention in get.outofbase.quantiles
                tavg90p <- suppressWarnings(get.outofbase.quantiles(tavg,ci@data$tmin,tmax.dates=ci@dates,tmin.dates=ci@dates,base.range=base.range,n=15,temp.qtiles=0.9,prec.qtiles=0.9,
                                                                min.base.data.fraction.present=min.base.data.fraction.present))
                tavg90p <- tavg90p$tmax$outbase$q90
                TxTn90p <- suppressWarnings(get.outofbase.quantiles(ci@data$tmax,ci@data$tmin,tmax.dates=ci@dates,tmin.dates=ci@dates,base.range=base.range,n=15,temp.qtiles=0.9,prec.qtiles=0.9,
                                                                min.base.data.fraction.present=min.base.data.fraction.present))
                tn90p <- TxTn90p$tmin$outbase$q90
                tx90p <- TxTn90p$tmax$outbase$q90 
		tavg05p <- quantile(tavg[ind],0.05,na.rm=TRUE) }

        # take any non leap year to create 365 month-day factors
        beg = as.Date("2001-01-01",format="%Y-%m-%d")
        end = as.Date("2001-12-31",format="%Y-%m-%d")
        dat.seq = seq(beg,end,by = "1 day")
        fact = factor(format(dat.seq,format="%m-%d"))

        # create date sequence from beginning of record to end, then create month-day factors
        beg2 = as.Date(paste(ci@date.factors$annual[1],"01","01",sep="-"))
        end2 = as.Date(paste(ci@date.factors$annual[length(ci@date.factors$annual)],"12","31",sep="-"))
        dat.seq2 = seq(beg2,end2,by = "1 day")
        fact2 = factor(format(dat.seq2,format="%m-%d"))
#        if (any(caltype=="gregorian",caltype=="standard")) { fact2 <- fact2[fact2!="02-29"] }

	# remove leap days from factors and temperature time series
	tmax <- ci@data$tmax[!fact2 %in% as.factor("02-29")]
	tmin <- ci@data$tmin[!fact2 %in% as.factor("02-29")]
	tavg <- tavg[!fact2 %in% as.factor("02-29")]
	monthly.factors <- ci@date.factors$monthly[!fact2 %in% as.factor("02-29")]
	annual.factors <- ci@date.factors$annual[!fact2 %in% as.factor("02-29")]

	fact2 <- fact2[!fact2 %in% as.factor("02-29")]

        # get shells for the following three variables and duplicate
        EHIaccl <- EHIsig <- EHF <- array(NA,length(tavg))
        ECIaccl <- ECIsig <- ECF <- array(NA,length(tavg))

        # assign the 365 percentiles to the entire time series based on date factors (so as to account for leap years) - February 29 days will be NA.
        annualrepeat_tavg90 <- annualrepeat_tavg05 <- array(NA,length(tavg))
        if(ehfdef == "BOM") {
                annualrepeat_tavg90 = array(tavg95p,length(tavg))
        } else if (ehfdef == "PA13") {
                annualrepeat_tavg90 = tavg90p[match(fact2,fact)]
        }
	annualrepeat_tavg05 <- array(tavg05p,length(tavg)) 

        # Calculate EHI/ECI values and EHF/ECF for each day of the given record. Must start at day 33 since the previous 32 days are required for each calculation.
        for (a in 33:length(tavg)) {
                EHIaccl[a] = (sum(tavg[a],tavg[a-1],tavg[a-2],na.rm=TRUE)/3) - (sum(tavg[(a-32):(a-3)],na.rm=TRUE)/30)
                EHIsig[a] = (sum(tavg[a],tavg[a-1],tavg[a-2],na.rm=TRUE)/3) - as.numeric(unlist(annualrepeat_tavg90[a]))  #as.numeric(unlist(tavg90p$tmax[1])[annualrepeat[a]]) #[(a %% 365)]
                EHF[a] = max(1,EHIaccl[a],na.rm=TRUE)*EHIsig[a]

                ECIaccl[a] = (sum(tavg[a],tavg[a-1],tavg[a-2],na.rm=TRUE)/3) - (sum(tavg[(a-32):(a-3)],na.rm=TRUE)/30)
                ECIsig[a] = (sum(tavg[a],tavg[a-1],tavg[a-2],na.rm=TRUE)/3) - as.numeric(unlist(annualrepeat_tavg05[a]))  #as.numeric(unlist(tavg90p$tmax[1])[annualrepeat[a]]) #[(a %% 365)]
                ECF[a] = min(-1,ECIaccl[a],na.rm=TRUE)*(-1*ECIsig[a])
        }

# step 2. Determine if tx90p, tn90p or EHF conditions have persisted for >= 3 days. If so, count number of summer heat waves.
        # assign the 365 percentiles to the entire time series based on date factors (so as to account for leap years) - February 29 days will be NA.
        tx90p_arr <- array(NA,length(tmax))
        tx90p_arr <- tx90p[match(fact2,fact)]
        tn90p_arr <- array(NA,length(tmin))
        tn90p_arr <- tn90p[match(fact2,fact)]

        # Record which days had temperatures higher than 90p or where EHF > 0 
        tx90p_boolean <- (tmax > tx90p_arr)
        tn90p_boolean <- (tmin > tn90p_arr)
        EHF_boolean <- (EHF > 0)
	ECF_boolean <- (ECF < 0)

        # Remove runs that are < 3 days long
        tx90p_boolean <- select.blocks.gt.length(tx90p_boolean,2)
        tn90p_boolean <- select.blocks.gt.length(tn90p_boolean,2)
        EHF_boolean <- select.blocks.gt.length(EHF_boolean,2)
	ECF_boolean <- select.blocks.gt.length(ECF_boolean,2)

# Step 3. Calculate aspects for each definition.
	hw_index <- array(NA,c(4,5,length(levels(annual.factors))))
        hw1_index <- array(NA,c(5,length(levels(annual.factors))))
        hw2_index <- array(NA,c(5,length(levels(annual.factors))))
        hw3_index <- array(NA,c(5,length(levels(annual.factors))))
        hw4_index <- array(NA,c(5,length(levels(annual.factors))))

        hw_index[1,,] <- get.hw.aspects(hw1_index,tx90p_boolean,annual.factors,monthly.factors,tmax,lat,ehfdef)
        hw_index[2,,] <- get.hw.aspects(hw2_index,tn90p_boolean,annual.factors,monthly.factors,tmin,lat,ehfdef)
        hw_index[3,,] <- get.hw.aspects(hw3_index,EHF_boolean,annual.factors,monthly.factors,EHF,lat,ehfdef,ehf=TRUE)
        hw_index[4,,] <- get.hw.aspects(hw4_index,ECF_boolean,annual.factors,monthly.factors,ECF,lat,ehfdef,ecf=TRUE)

        rm(tavg,tavg90p,EHIaccl,EHIsig,EHF,tx90p_boolean,tn90p_boolean,EHF_boolean,tx90p_arr,tn90p_arr,hw1_index,hw2_index,hw3_index,tn90p,tx90p,beg,end,beg2,end2,dat.seq,dat.seq2,fact,fact2,ECIaccl,ECIsig,ECF)
	return(hw_index)
}

# leapdays
# INPUT:
#    - year: an array of years.
# OUTPUT:
#    - an array of zeros and ones for each year supplied, indicating number of leap days in those years.
leapdays <- function(year) { if(!is.numeric(year)) stop("year must be of type numeric") ; return(0 + (year %% 4 == 0)) }

# get.hw.aspects
# Calculate heat wave aspects as per Perkins and Alexander (2013). HWM, HWA, HWN, HWD, HWF. 
# EHF definition is updated (personal comms Perkins 2015). Changes include using the 90th percentile (instead of 95th) and using 15-day window running percentiles,
# instead of climatological percentiles. Thus each day of the calendar year has a unique percentile associated with it.
#
# INPUT:
#    - aspect.array: empty array used to hold aspects.
#    - boolean.str: an array of booleans indicating the existence of a heatwave for each day.
#    - yearly.date.factors: annual date factors from climdex.input object.
#    - monthly.date.factors: monthly date factors from climdex.input object.
#    - daily.data: daily values of either TX, TN or EHF.
#    - lat: latitude of current grid cell.
# OUTPUT:
#    - aspect.array: filled with calculated aspects.
get.hw.aspects <- function(aspect.array,boolean.str,yearly.date.factors,monthly.date.factors,daily.data,lat,ehfdef,ehf=FALSE,ecf=FALSE) {
	month <- substr(monthly.date.factors,nchar(as.character(levels(monthly.date.factors)[1]))-1,nchar(as.character(levels(monthly.date.factors)[1])))

	daily.data.full = daily.data # make a copy of all daily data
	boolean.str.full = boolean.str # make a copy of all boolean data

	nyears = length(levels(yearly.date.factors))
	extended_window = 90
	aspect_ind = 1 # keep track of the index of the aspect array to store data in

	for (year in levels(yearly.date.factors)[1]:levels(yearly.date.factors)[nyears]) {
		if((ehf==TRUE && ehfdef=="BOM") || ecf==TRUE) { summer_indices = which(yearly.date.factors %in% as.factor(year)) } else {
		if(lat < 0) {
			summer_indices = which(monthly.date.factors %in% as.factor(paste(year,"-11",sep="")) | monthly.date.factors %in% as.factor(paste(year,"-12",sep="")) | monthly.date.factors %in% as.factor(paste(year+1,"-01",sep="")) |
	        	        monthly.date.factors %in% as.factor(paste(year+1,"-02",sep="")) | monthly.date.factors %in% as.factor(paste(year+1,"-03",sep="")))
		} else {
	                summer_indices = which(monthly.date.factors %in% as.factor(paste(year,"-05",sep="")) | monthly.date.factors %in% as.factor(paste(year,"-06",sep="")) | monthly.date.factors %in% as.factor(paste(year,"-07",sep="")) |
	                        monthly.date.factors %in% as.factor(paste(year,"-08",sep="")) | monthly.date.factors %in% as.factor(paste(year,"-09",sep="")))
		} }

	        extended_indices = seq((summer_indices[1]),(summer_indices[length(summer_indices)]+extended_window),1)
	        extended_data = daily.data.full[extended_indices]
	        extended_boolean = boolean.str.full[extended_indices]
	        rle_extended_boolean = rle(as.logical(extended_boolean))
		last_day_of_hw_season = (length(extended_boolean)-extended_window)
	
	        # indices of extended_data that include heatwaves that start during season and end before end of season.
	        truevals = which((rle_extended_boolean$lengths)>=3 & cumsum(rle_extended_boolean$lengths)<=last_day_of_hw_season & rle_extended_boolean$values==TRUE)
	
	        # if the first heatwave exists on the first day of season, but is part of a heatwave from the previous season, then remove this heatwave since we don't want to count that in this season.
	        if(!all(is.na(extended_boolean[1:3])) && !all(is.na(boolean.str.full[(summer_indices[1])-1]))) {
			if(all(extended_boolean[1:3]==TRUE) && length(truevals)>0 && boolean.str.full[(summer_indices[1])-1]==TRUE) { truevals = truevals[-1] } }
	
		# indices of heatwave(s) that end after season.
		extvals = which((rle_extended_boolean$lengths)>=3 & cumsum(rle_extended_boolean$lengths)>last_day_of_hw_season & rle_extended_boolean$values==TRUE)
	
		# check for heatwaves that started near end of season and continued afterward.
		if(!is.na(last_day_of_hw_season)) {
			if(length(extvals)>0 && cumsum(rle_extended_boolean$lengths)[extvals[1]-1]<last_day_of_hw_season) 
	                # then the next heatwave actually started in summer and should be counted
	        {
	                truevals = c(truevals,extvals[1]) 
	        } }
	
	        nhw = length(truevals)  # number of heatwaves
	        hwm = array(NA,nhw)  # array to store heatwave mean temperature
		hwa = array(NA,nhw)
	        if(!is.na(nhw) && nhw>0){
	                for (i in 1:nhw) { # over each run
					if(truevals[i]==1) { i1 = 1 } else { i1 = cumsum(rle_extended_boolean$lengths)[truevals[i]-1] + 1 }      # "+1" to begin on day 1 of heat wave - not the last day of the non-heatwave
	                                i2 = cumsum(rle_extended_boolean$lengths)[truevals[i]]
	                                hwm[i] = mean(extended_data[i1:i2],na.rm=TRUE)
	                                if(ecf==TRUE) { hwa[i] = min(extended_data[i1:i2],na.rm=TRUE) } 
	                                else { hwa[i] = max(extended_data[i1:i2],na.rm=TRUE) }
	                }
	        }
	
	        hwm2 = mean(hwm,na.rm=TRUE)
		hwn = nhw
	
		# HWM
	        if(is.nan(hwm2) || is.na(hwm2)) { aspect.array[1,aspect_ind] = NA } else { aspect.array[1,aspect_ind] = hwm2 }
		# HWA
		if(is.nan(hwm2) || is.na(hwm2)) { aspect.array[2,aspect_ind] = NA } else { if (ecf==TRUE) {aspect.array[2,aspect_ind] = hwa[which.min(hwm)] } else {aspect.array[2,aspect_ind] = hwa[which.max(hwm)] } }
		# HWN
		aspect.array[3,aspect_ind] = hwn
		# HWD
		if(is.nan(hwm2) || is.na(hwm2)) { aspect.array[4,aspect_ind] = NA } else { aspect.array[4,aspect_ind] = max(rle_extended_boolean$lengths[truevals],na.rm=TRUE) }
		# HWF
		aspect.array[5,aspect_ind] = sum(rle_extended_boolean$lengths[truevals],na.rm=TRUE)
	
	        aspect_ind = aspect_ind + 1
	}

	rm(summer_indices,extended_indices,extended_data,extended_boolean,rle_extended_boolean,truevals,nhw,hwm,hwa,hwm2,last_day_of_hw_season,extvals,daily.data.full,boolean.str.full,ehf,ecf)

	aspect.array[2,] <- ifelse(aspect.array[2,]=="-Inf",NA,aspect.array[2,])
	aspect.array[4,] <- ifelse(aspect.array[4,]=="-Inf",NA,aspect.array[4,])
	if (lat<0) { aspect.array[,length(aspect.array[1,])] <- NA }	# If in southern hemisphere, remove last year since there is only half a summer (can risk removing 366 days since it won't infringe on the previous summer)
	return(aspect.array)
}

# nullify
# Currently not used.
# defines NULL global variables. Done to prevent numerous warning messages printing. Has no impact on code. This is currently done at the top of the code.
nullify <<- function() {
#	tsmin <<- tsmax <<- prec <<- tsmintime <<- tsmaxtime <<- prectime <<- NULL
	vars = c("tsmintime","tsmaxtime","prec")
	for(i in 1:length(vars)) {
		vars[i] <- NULL
		assign("\"vars[i]\"","\"vars[i]\"", envir = .GlobalEnv)
	}
}

###############################
# Due seemingly to a bug in R or improper handling in this code these functions are copied from climdex.pcic so that they can be properly referenced by workers when running in parallel.
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
