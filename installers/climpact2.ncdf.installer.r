# ------------------------------------------------
# This script checks that the appropriate packages are installed for running ClimPACT2.
# nherold, May 2016
# ------------------------------------------------

linux.packages <- c("abind","bitops","Rcpp","caTools","PCICt","SPEI","climdex.pcic","ncdf4","snow","udunits2","functional","proj4")

#print(installed.packages()[,"Package"])
print("",quote=FALSE)
print("******************************",quote=FALSE)
print("",quote=FALSE)
cat("Calculating the ClimPACT2 indices on netCDF data requires that PROJ4 and UDUNITS2 be installed on your opreating system
prior to running this script. If the following R packages fail to install ensure that these two programs are installed.
>>- PRESS ENTER TO CONTINUE -<<")
x <- readLines(con="stdin", 1)
	
# Install unix-specific packages
if(.Platform$OS.type == "unix") {
	print("",quote=FALSE)
	print("This is a unix-based OS, checking for additional R packages.",quote=FALSE)
	print("******************************",quote=FALSE)
	for (package in 1:length(linux.packages)) {
	        if(linux.packages[package] %in% installed.packages()[,"Package"]) { print(paste(linux.packages[package],"... installed.",sep=""),quote=FALSE)
	        } else { print(paste(linux.packages[package],"... not installed. Installing...",sep=""),quote=FALSE)
	        install.packages(linux.packages[package]) }
	}
	if(!"ncdf4.helpers" %in% installed.packages()[,"Package"]) {
		print("ncdf4.helpers... not installed. Installing...",quote=FALSE)
		install.packages("./pcic_packages/ncdf4.helpers_0.3-3.tar.gz",repos=NULL,type="source")
	} else print("ncdf4.helpers... installed.",quote=FALSE)

	print("",quote=FALSE)
	cat("A modified version of climdex.pcic.ncdf needs to be installed. If a version is already installed it will be overwritten. 
If you do not install this modified version you will not be able to calculate the indices on netCDF data (but will still
be able to use the GUI). Install the modified version? (y/n)")
	x <- readLines(con="stdin", 1)

	if(x=="y") { install.packages("./pcic_packages/climdex.pcic.ncdf.climpact.tar.gz",repos=NULL,type="source") 
	} else { print("Not installing modified climdex.pcic.ncdf.",quote=FALSE) }
} else print("THIS IS NOT A UNIX-BASED OPERATING SYSTEM; NOT INSTALLING R PACKAGES.")

print("",quote=FALSE)
print("******************************",quote=FALSE)
print(paste("R version ",as.character(getRversion())," detected.",sep=""),quote=FALSE)
print("Checking complete.",quote=FALSE)
