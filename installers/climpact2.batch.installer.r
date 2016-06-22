# ------------------------------------------------
# This script checks that the appropriate packages are installed for running ClimPACT2.
# nherold, May 2016
# ------------------------------------------------

gui.packages <- c("bitops","Rcpp","caTools","PCICt","SPEI","climdex.pcic","foreach","doParallel")

#print(installed.packages()[,"Package"])

print("",quote=FALSE)
print("Checking for required R packages.",quote=FALSE)
print("******************************",quote=FALSE)

print("",quote=FALSE)
print("This is a unix-based OS, checking for additional R packages.",quote=FALSE)
print("******************************",quote=FALSE)
for (package in 1:length(gui.packages)) {
        if(gui.packages[package] %in% installed.packages()[,"Package"]) { print(paste(gui.packages[package],"... installed.",sep=""),quote=FALSE)
        } else { print(paste(gui.packages[package],"... not installed. Installing...",sep=""),quote=FALSE)
        install.packages(gui.packages[package]) }
}

print("",quote=FALSE)
print("******************************",quote=FALSE)
print(paste("R version ",as.character(getRversion())," detected.",sep=""),quote=FALSE)
print("Checking complete.",quote=FALSE)
