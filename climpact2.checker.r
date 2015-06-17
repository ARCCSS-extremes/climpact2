# This script checks that the appropriate packages are installed for running climpact2.r.
# nherold, May 2015

list.of.packages <- c(  "ncdf4",
			"bitops",
                        "Rcpp",
                        "caTools",
			"PCICt",
			"foreach",
			"doParallel",
			"abind",
			"SPEI")

print("******************************")
print("***** INSTALLED PACKAGES *****")
print("******************************")
print(installed.packages()[,"Package"])

# If any other packages not installed download and install from CRAN.
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
	print("******************************")
	print(paste("Installing the following required packages...",new.packages,sep=""))
	install.packages(new.packages,repos='http://cran.us.r-project.org') }

# Remove any currently installed version of climdex.pcic, then install according to whether in unix or windows environment.
if("climdex.pcic" %in% installed.packages()[,"Package"]) { remove.packages("climdex.pcic") }

if(.Platform$OS.type == "unix") { 
	system(paste("if [ ! -e climdex.pcic.tar.gz ]; then wget https://github.com/ARCCSS-extremes/climpact2/blob/master/climdex.pcic.tar.gz; fi")) 
	install.packages("climdex.pcic.tar.gz",repos=NULL,type="source")
} else { 
	install.packages("climdex.pcic_1.1-5.1.zip",repos=NULL,type="win.binary")
}

print("******************************")
print(paste("R version ",as.character(getRversion())," detected.",sep=""))
print("Checking complete.")
