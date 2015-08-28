# This script checks that the appropriate packages are installed for running climpact2.r.
# nherold

gui.packages <- c("bitops","Rcpp","caTools","PCICt","SPEI","climdex.pcic")
linux.packages <- c("ncdf4","foreach","doParallel","abind")

print("******************************")
print("***** INSTALLED PACKAGES *****")
print("******************************")
print(installed.packages()[,"Package"])

# Install/update packages needed for ClimPACT2 GUI.
if(length(gui.packages)) {
	print("******************************")
	print(paste("Installing the following required packages...",new.packages,sep=""))
	install.packages(gui.packages) }

# Install Linux-specific packages
if(.Platform$OS.type == "unix") { install.packages(linux.packages) } 

print("******************************")
print(paste("R version ",as.character(getRversion())," detected.",sep=""))
print("Checking complete.")
