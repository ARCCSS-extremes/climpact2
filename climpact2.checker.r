# This script checks that the appropriate packages are installed for running climpact2.r.
# nherold, May 2015

list.of.packages <- c(  "ncdf4",
			"PCICt",
			"foreach",
			"caTools",
			"Rcpp",
			"doParallel",
			"abind",
			"SPEI")

print("******************************")
print("***** INSTALLED PACKAGES *****")
print("******************************")
print(installed.packages())

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# If any other packages not installed download and install from CRAN.
if(length(new.packages)) {
	print("******************************")
	print(paste("Installing the following required packages...",new.packages,sep=""))
	install.packages(new.packages) }

# If climdex.pcic not installed and the .tar.gz file is not in current directory, then download modified copy from climpact2 github and install.
if(!("climdex.pcic" %in% installed.packages()[,"Package"])) {
        system(paste("if [ ! -e climdex.pcic.tar.gz ]; then wget https://github.com/heroldn/climpact2/blob/master/climdex.pcic.tar.gz; fi"))
        print("******************************")
        print(paste("Installing climdex.pcic...",new.packages,sep=""))
        install.packages("climdex.pcic.tar.gz",repos=NULL,type="source") }

print("******************************")
print(paste("R version ",as.character(getRversion())," detected.",sep=""))
print("Checking complete.")
