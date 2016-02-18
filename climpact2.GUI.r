# ------------------------------------------------ #
# ClimPACT2 GUI
# University of New South Wales
# ------------------------------------------------ #
#
# This file constitutes the graphical user interface for ClimPACT2. It also contains code that plots graphs, writes out data
# and calculates SPEI/SPI indices.
#
# This package is available on github https://github.com/ARCCSS-extremes/climpact2. See this site for specific version history.
#
# nherold, January 2016.
#
#
#
# BUGS
#   - Currently SPEI/SPI are calculated via the old ClimPACT code. This is because the CRAN package for SPEI/SPI does not
#     ostenisbly support large runs of NA values. When this occurs real numbers are included in the output where NA values
#     should occur.
#
#
# TECHNICAL NOTES
#   - warnings are suppressed on lsfit to prevent numerous messages on the removal of missing values that may exist in the
#     users data.
#
# 
# HISTORY OF CHANGES
#   This file is a heavily modified version of climpact.r from the original ClimPACT. The major change
#   that has taken place in ClimPACT2 is that the calculation of the indices is almost entirely taken care of by the R package
#   climdex.pcic, with several exceptions such as the heatwave indices and SPEI/SPI. 
#
#   Several people contributed significantly to the development of the original ClimPACT software. For posterity and credit, below is a 
#   list of the key names and dates.
#
#   Programmed by Yujun Ouyang,Mar,2004
#   rewritten by Yang Feng, July 2004
#   version 1.0, 2004-10-14
#   modified, 2006-01-24
#   modified, 2007-03-23
#   modified, 2007-11-26
#   modified, 2008-05-05
#   modified, 2008-05-06
#   modified, 2008-06-16
#   modified, 2012-05-30
#   Sandra add new indices
#   Hongang to check Sandra's code and add new indices - from 2012-11-05
#   modified 2013, James Goldie - overhaul of code

# Remove all previously opened devices and variables from memory.
graphics.off()
rm(list = ls(all = TRUE))

library(tcltk)
tclRequire("BWidget")

# ------------------------------------------------ #
# Global variables
# ------------------------------------------------ #

# Nullify objects globally to avoid warning messages.
reading.pb <<- hwquantiles <<- process.pb <<- pb <<- orig.name.user <<- qc.yes <<- nordaytem1 <<- outthresdir <<- quantiles <<- cio <<- ofilename <<- infor1 <<- orig.name <<- title.station <<- outlogdir <<- thres.calc <<- 
add.data <<- add.data.name <<- yeardate2 <<- add.default <<- wait <<- out <<- speidata <<- ref.start <<- ts.end <<- basetmin <<- basetmax <<- baseprec <<- start.but <<- cal.but <<- ttmp <<- outqcdir <<- nordaytem1 <<- NULL

start1<-tktoplevel(bg='white')

# Fonts 
fontHeading     <- tkfont.create(family = "times", size = 40, weight = "bold", slant = "italic")
fontHeading1    <- tkfont.create(family = "times", size = 18)#, weight = "bold")
fontHeading2    <- tkfont.create(family = "times", size = 14, weight = "bold")
fontTextLabel   <- tkfont.create(family = "arial", size = 12)
fontFixedWidth  <- tkfont.create(family = "courier", size = 12)
font_small  <- "arial 12"
font_small_bold  <- "arial 12 bold"
font_big    <- "arial 15 bold"
font_err    <- "times 13 bold"
grey_font <- tkfont.create(family = "arial", size = 30, weight = "bold", slant = "italic") #'times 20 grey bold'

# Global variables
running.zero.allowed.in.temperature = 4
stdt=4
nstddev<-tclVar(stdt)
dayim <- as.integer(c(31,28,31,30,31,30,31,31,30,31,30,31))  # day # in a month
dayim2 <- as.integer(c(31,29,31,30,31,30,31,31,30,31,30,31))
temp.quantiles.default = c(0.05,0.1,0.5,0.9,0.95)
prec.quantiles.default = c(0.05,0.1,0.5,0.9,0.95,0.99)
barplot_flag    <- TRUE
loaded <- FALSE
min_trend       <- 10 

# Initial index parameter values
stations<-tclVar(paste(" ")); stdt<-tclVar(paste("4"))
base.year.start.tcl<-tclVar(paste("1971"));base.year.end.tcl<-tclVar(paste("2000"))
latentry=tclVar(''); lonentry<-tclVar('') ; add.data.name.entry <-tclVar('')
Entry4<-tclVar(paste("0"))
Entry5<-tclVar(paste("0"))
Entry6<-tclVar(paste("20"));Entry7<-tclVar(paste("0"))
Entry8<-tclVar(paste("20"));Entry9<-tclVar(paste("0"))
Entry12<-tclVar(paste("25"))
Entry13<-tclVar(paste("2"))
Entry14<-tclVar(paste("2"))
Entry15<-tclVar(paste("3"))
Entry16<-tclVar(paste("2"))
Entry17<-tclVar(paste("30"))
Entry20<-tclVar(paste("18"))
Entry21<-tclVar(paste("18"))
Entry22<-tclVar(paste("10"))
Entry23<-tclVar(paste("24"))
Entry24<-tclVar(paste("0"))

# Initial value for check box buttons
cbvalue<-c()
for(i in 1:100){
	aux<-tclVar(init=1)
	cbvalue<-c(cbvalue,as.character(aux))
	tclvalue(cbvalue[i])<-TRUE
}
selectAll<-function() { for(i in 1:length.indices) tclvalue(cbvalue[i])=TRUE }
selectNone<-function() { for(i in 1:length.indices) tclvalue(cbvalue[i])=FALSE }

# Read in climate index data
indexfile <- "index.master.list"
indexlist <- (read.table(indexfile,sep="\t"))
indices <- as.character(indexlist[,1])
length.indices = length(indices)+5		# +5 to include extra indices for GUI-only climpact2
units <- as.character(indexlist[match(indices,indexlist[,1]),2])
Encoding(units) <- "UTF-8"
longnames <- as.character(indexlist[match(indices,indexlist[,1]),3])
Encoding(longnames) <- "UTF-8"
subtitle <- as.character(indexlist[,3])

# ------------------------------------------------ #
# ClimPACT2 GUI functions
# ------------------------------------------------ #

# extraQC code, taken from the "rclimdex_extraqc.r" package, 
# email from Lisa on 2013-07-19.
# NOTE: this part outputs some results, but does not change the data.
# Quality Control procedures programed by Enric Aguilar (C3, URV, Tarragona, Spain) and 
# and Marc Prohom, (Servei Meteorologic de Catalunya). Edited by nherold to output to .csv (Jan 2016).
allqc <- function (master, output, outrange = 4)
{
	output <- paste(output, "/", ofilename, sep = "")
	# fourboxes will produce boxplots for non-zero precip, tx, tn, dtr using the IQR entered previously
	# the plot will go to series.name_boxes.pdf
	# outliers will be also listed on a file (series.name_outliers.txt)
	fourboxes(master, output, save = 1, outrange)
	
	# Will plot a histogram of the decimal point to see rounding problems, for prec, tx, tn
	# The plot will go to series.name_rounding.pdf. Needs some formal arrangements (title, nice axis, etc)
	roundcheck(master, output, save = 1)
	
	# will list when tmax <= tmin. Output goes to series.name_tmaxmin.txt
	tmaxmin(master, output)
	
	# will list values exceeding 200 mm or temperatures with absolute values over 50. Output goes to 
	# series.name_toolarge.txt
	humongous(master, output)
	
	# 'Annual Time series' constructed with boxplots. Helps to identify years with very bad values
	# Output goes to series.name_boxseries.pdf
	boxseries(master, output, save = 1)
	
	# Lists duplicate dates. Output goes to series.name_duplicates.txt	
	duplivals(master, output)
	
	# The next two functions (by Marc Prohom, Servei Meteorologic de Catalunya) identify consecutive tx and tn values with diferences larger than 20
	# Output goes to series.name_tx_jumps.txt and series.name_tn_jumps.txt. The first date is listed. 
	jumps_tx(master, output)
	jumps_tn(master, output)
	
	# The next two functions (by Marc Prohom, Servei Meteorologic de Catalunya)identify 
	# series of 3 or more consecutive identical values. The first date is listed. 
	# Output goes to series.name_tx_flatline.txt  and series.name_tx_flatline.txt
	flatline_tx(master, output)
	flatline_tn(master, output)
	# tkmessageBox(message = "Extra Quality Control Routines finished!!!")
}

# A function that should be called before any .csv file is written. It appends some basic information that should be stored in each file for 
# the user's record.
write_header <- function(filename,header="")
{
	if(is.null(filename)) { stop("Filename not passed to function 'write_header'") }
	
#	header = paste("Description: ",header,sep="")
#	write.table(header, file = filename, append = FALSE, row.names=FALSE,col.names = FALSE)

	first_lines = cbind(c("Station: ","Latitude: ","Longitude: ","ClimPACT2_version: ","Date_of_calculation: "),c(ofilename,latitude,longitude,version.climpact,toString(Sys.Date())))
	# Check if file is open
	tmp = try(write.table(first_lines, sep=",", file = filename, append = FALSE, row.names=FALSE,col.names = FALSE))

	if(class(tmp)=="try-error") { tkmessageBox(message=paste("Error encountered, please check that the file ",filename," is not currently open, then select OK to try again.",sep=""),icon='warning');  write_header(filename) }
}

check_open <- function(filename)
{
	tmp = try(write.table("test text", sep=",", file = filename, append = FALSE, row.names=FALSE,col.names = FALSE))
	if(class(tmp)=="try-error") { tkmessageBox(message=paste("Error encountered, please check that the file ",filename," is not currently open, then select OK to try again.",sep=""),icon='warning');  check_open(filename) }
}

# Plots boxplots. Needs only station and save
fourboxes <- function(station, output, save = 0, outrange)
{
	# add save option
	if (save == 1)
	{ 
		nombre <- paste(output, "_boxes.pdf", sep = "")
		check_open(nombre)
		pdf(file = nombre)
	}
	
	datos <- read.table(station, col.names = c("year", "month", "day", "pc", "tx", "tn"),na.strings = "-99.9")
	datos$tr <- datos$tx - datos$tn
	prec <- subset(datos, datos$pc > 0)
	par(mfrow = c(2, 2))
	
	# we open a file for writing outliers. First time is not append; rest is append
	filena <- paste(output, "_outliers.csv", sep = "")
	
	# for each of precip, tmax, tmin, dtr:
	#   produce boxplots: IQR for default is 3 for temp and 5 for precip
	#     can be entered as parameter when calling the function. Precip will always be 2 units more than temp
	#   write outliers out
	# if no data's available, 'no data available' is printed on a blank panel instead
	write_header(filena,"Outliers shown in *boxseries.pdf")
	write.table(cbind("Date","Prec","TX","TN","DTR"), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)

	if (any(!is.na(prec$pc)))
	{
		respc <- boxplot(prec$pc ~ prec$month, main = "NON ZERO PREC", col = "blue", range = outrange + 2)

		# write precip outliers
		write.table("Prec up",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
			prov <- subset(datos,datos$month == a & datos$pc > respc$stats[5, a])
			date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
			write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
		} else
		{
			plot.new()
			text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
		}
	
	if (any(!is.na(datos$tx)))
	{
		restx <- boxplot(datos$tx ~ datos$month, main = "TX", col = "red", range = outrange)
		
		# write tmax outliers
		write.table("TX up",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tx > restx$stats[5, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file= filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
		write.table("TX low",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tx < restx$stats[1, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr),sep=",", file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
	} else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	
	if (any(!is.na(datos$tn)))
	{
		restn <- boxplot(datos$tn ~ datos$month, main = "TN", col = "cyan", range = outrange)
		
		# write tmin outliers
		write.table("TN up",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tn > restn$stats[5, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
		write.table("TN low",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tn < restn$stats[1, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
	} else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	
	if (any(!is.na(datos$tr)))
	{
		restr <- boxplot(datos$tr ~ datos$month, col = "yellow", main = "DTR", range = outrange)
		
		# write dtr outliers
		write.table("DTR up",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tr > restr$stats[5, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
		write.table("DTR low",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tr < restr$stats[1, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
	} else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	
	if (save == 1) dev.off()
	
	rm(datos)
}

# Plots histograms showing rounding. Needs station and you can delimit the period with first and last year
roundcheck <- function(station,output,fyear = 1000,lyear = 3000,save = 0)
{
	if (save == 1)
	{ 
		nombre<-paste(output,'_rounding.pdf',sep="")
		check_open(nombre)
		pdf(file=nombre)
	}
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	par(mfrow=c(1,3))
	my<-subset(datos,datos$year >= fyear & datos$year <= lyear)
	ispc=subset(my$pc,my$pc > 0)
	hist(ispc %% 1,col='blue',main='NON ZERO PREC ROUNDING',breaks=c(seq(-0.1,0.9,0.1)),xlab="")
	hist(my$tx %% 1,col='red',main='TX ROUNDING',breaks=c(seq(-0.1,0.9,0.1)),xlab="")
	hist(my$tn %% 1,col='cyan',main='TN ROUNDING',breaks=c(seq(-0.1,0.9,0.1)),xlab="")
	
	if (save == 1) { dev.off() }
	rm(datos)
}

tmaxmin <- function(station,output)
{
	filena = paste(output,'_tmaxmin.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	maxmin = subset(datos,(datos$tx-datos$tn)<=0)
	date.tmp = paste(maxmin$year,maxmin$month,maxmin$day,sep="-")
	write_header(filena,"Dates where TN>TX")
	write.table(cbind("Date","Prec","TX","TN"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,maxmin$pc,maxmin$tx,maxmin$tn),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	
	# If no data (i.e. no TN>TX) in variable print message
	if(length(maxmin)==0) { write.table("NO TN > TX FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(datos)
}

humongous <- function(station,output)
{
	filena = paste(output,'_toolarge.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	grande<-subset(datos,(datos$tx > 50 | datos$tx < -50 | datos$tn > 50 | datos$tn < -50 | datos$pc > 200 | datos$pc < 0))
	date.tmp = paste(grande$year,grande$month,grande$day,sep="-")
	write_header(filena,"Dates where precipition > 200 mm or abs(temperature) > 50 degrees.")
	write.table(cbind("Date","Prec","TX","TN"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,grande$pc,grande$tx,grande$tn),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no data (i.e. no large values) in variable print message
	if(length(grande)==0) { write.table("NO EXCESSIVE VALUES FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(list=ls())
}

boxseries <- function(station, output, save = 0)
{
	if (save == 1)
	{
		nombre <- paste(output, "_boxseries.pdf", sep = "")
		check_open(nombre)
		pdf(file = nombre)
	}
	
	datos <- read.table(station, col.names = c("year", "month", "day", "pc", "tx", "tn"),na.strings = "-99.9")
	datos$tr <- datos$tx - datos$tn
	prec <- subset(datos, datos$pc > 0)
	par(mfrow = c(2, 2))
	
	if (any(!is.na(prec$pc))) respc <- boxplot(prec$pc ~ prec$year, main = "NON ZERO PREC", col = "blue", range = 4) else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	if (any(!is.na(datos$tx))) restx <- boxplot(datos$tx ~ datos$year, main = "TX", col = "red", range = 3) else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	if (any(!is.na(datos$tn))) restn <- boxplot(datos$tn ~ datos$year, main = "TN", col = "cyan", range = 3) else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	if (any(!is.na(datos$tr))) restr <- boxplot(datos$tr ~ datos$year, col = "yellow", main = "DTR", range = 3) else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	
	if (save == 1) dev.off()
	
	rm(datos)  # we don't want to delete everyting...
}

duplivals <- function(station,output)
{
	filena = paste(output,'_duplicates.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	isdupli<-cbind(datos$year,datos$month,datos$day)
	duplicate.dates = subset(isdupli, duplicated(isdupli)== TRUE)
	date.tmp = paste(duplicate.dates[,1],duplicate.dates[,2],duplicate.dates[,3],sep="-")
	write_header(filena,"Dates that have been used more than once in your input file.")
	write.table(cbind("Dates_duplicated"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(date.tmp,sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no data (i.e. no large values) in variable print message
	if(length(date.tmp)==0) { write.table("NO DUPLICATE DATES FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(datos)  # we don't want to delete everyting...
}

jumps_tx <- function(station, output)
{
	filena = paste(output, '_tx_jumps.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	diftx <-abs(round(diff(datos$tx, lag=1, differences=1),digits=1))
	x <-data.frame(c.ndx=cumsum(rle(diftx)$lengths),c.type=rle(diftx)$values)
	x <-na.omit(x)
	names(x) <-c("id","val")
	z <-data.frame(id=row(datos), year=datos$year, month=datos$month, day=datos$day, day=datos$tx)
	Z <- z[,6:10]
	names(z) <-c("id","year","month","day","tx")
	jumps <- merge(z, x, by="id", all.x=F, all.y=T)
	jumps <- subset(jumps, (jumps$val>=20))
	jumps <- jumps[,7:11]
	jumps <- jumps[,-4]
	names(jumps) = c("year","month","day","tx")
	date.tmp = paste(jumps$year,jumps$month,jumps$day,sep="-")
	write_header(filena,"Dates where the change in TX is > 20 degrees.")
	write.table(cbind("Date","TX"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,jumps$tx),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	
	# If no issues found in variable, print message
	if(length(jumps$tx)==0) { write.table("NO LARGE TX JUMPS FOUND",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }
	
	rm(datos)  # we don't want to delete everyting...
}

jumps_tn <- function(station,output)
{
	filena = paste(output, '_tn_jumps.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	diftn <-abs(round(diff(datos$tn, lag=1, differences=1), digits=1))
	x <-data.frame(c.ndx=cumsum(rle(diftn)$lengths),c.type=rle(diftn)$values)
	x <-na.omit(x)
	names(x) <-c("id","val")
	z <-data.frame(id=row(datos), year=datos$year, month=datos$month, day=datos$day, day=datos$tn)
	Z <- z[,6:10]
	names(z) <-c("id","year","month","day","tn")
	jumps <- merge(z, x, by="id", all.x=F, all.y=T)
	jumps <- subset(jumps, (jumps$val>=20))
	jumps <- jumps[,7:10]
	names(jumps) = c("year","month","day","tn")
	date.tmp = paste(jumps$year,jumps$month,jumps$day,sep="-")
	write_header(filena,"Dates where the change in TN is > 20 degrees.")
	write.table(cbind("Date","TN"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,jumps$tn),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no issues found in variable, print message
	if(length(jumps$tn)==0) { write.table("NO LARGE TN JUMPS FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(datos)  # we don't want to delete everyting...
}

flatline_tx <- function(station,output)
{
	filena = paste(output, '_tx_flatline.csv', sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"), na.strings='-99.9')
	diftx <-abs(round(diff(datos$tx, lag=1, differences=1), digits=1))
	x <-data.frame(c.ndx=cumsum(rle(diftx)$lengths),c.size=rle(diftx)$lengths,c.type=rle(diftx)$values)
	x <-x[x$c.type==0,]
	x <-na.omit(x)
	names(x) <-c("id","dup","val")
	z <-data.frame(id=row(datos), year=datos$year, month=datos$month, day=datos$day, day=datos$tx)
	z_1 <- z[,6:10]
	names(z_1) <-c("id","year","month","day","tx") 
	flat <- merge(z_1, x, by="id", all.x=F, all.y=T)
	flat <- subset(flat, (flat$dup>=3))
	flat <- flat[,2:6]

	date.tmp = paste(flat$year,flat$month,flat$day,sep="-")
	write_header(filena,"Dates where TX values have been repeated more than 4 times.")
	write.table(cbind("Date","TX","Number of duplicates"),sep=",",append=TRUE,file=filena,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,flat$tx,flat$dup),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no issues found in variable, print message
	if(length(flat$tx)==0) { write.table("NO REPEATED TX FOUND",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }
	
	rm(datos)  # we don't want to delete everyting...
}

flatline_tn <- function(station,output)
{
	filena = paste(output, '_tn_flatline.csv', sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"), na.strings='-99.9')
	diftx <-abs(round(diff(datos$tn, lag=1, differences=1), digits=1))
	x <-data.frame(c.ndx=cumsum(rle(diftx)$lengths),c.size=rle(diftx)$lengths,c.type=rle(diftx)$values)
	x <-x[x$c.type==0,]
	x <-na.omit(x)
	names(x) <-c("id","dup","val")
	z <-data.frame(id=row(datos), year=datos$year, month=datos$month, day=datos$day, day=datos$tn)
	z_1 <- z[,6:10]
	names(z_1) <-c("id","year","month","day","tn") 
	flat <- merge(z_1, x, by="id", all.x=F, all.y=T)
	flat <- subset(flat, (flat$dup>=3))
	flat <- flat[,2:6]
	date.tmp = paste(flat$year,flat$month,flat$day,sep="-")
	write_header(filena,"Dates where TX values have been repeated more than 4 times.")
	write.table(cbind("Date","TN","Number of duplicates"),sep=",",append=TRUE,file=filena,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,flat$tn,flat$dup),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no issues found in variable, print message
	if(length(flat$tn)==0) { write.table("NO REPEATED TN FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(datos)  # we don't want to delete everyting...
}
# End of Prohom and Aguilar code.

# pplotts
# plots QC'ed data (TX, TN, PR) into pdf files.
pplotts <- function(var = "prcp", type = "h", tit = NULL)
{
	# set bounds for the plot based on available data. dtr and prcp have
	# floors of 0 by definition (assuming tmax and tmin have been qc'd)
	
	if(var == "dtr")
	{
	#  ymax <- max(data[, "tmax"] - data[, "tmin"], na.rm = TRUE)
		ymax <- max(cio@data$dtr,na.rm = TRUE)
		ymin <- 0
	} else if (var == "prcp")
	{
		ymax <- max(cio@data$prec, na.rm = TRUE)
		ymin <- 0
	} else
	{
		ymax <- max(cio@data[[var]], na.rm = TRUE) + 1
		ymin <- min(cio@data[[var]], na.rm = TRUE) - 1
	}
	if(var == "prcp") { var1 = "prec" } else { var1 = var }

	# set default y scales if proper ones can't be calculated
	# but do we really want to try to plot if there's no data available at all?
	if (is.na(ymax) | is.na(ymin) | (ymax == -Inf) | (ymin == -Inf))
	{
		ymax <- 100
		ymin <- -100
		warning(paste("Warnings have been generated because there is no available data for","one or more of tmax, tmin or precip. Check the plots in /qc to confirm this."))
	}
	
	par(mfrow = c(4, 1))
	par(mar = c(3.1, 2.1, 2.1, 2.1))
	
	for(i in seq(years, yeare, 10))
	{
		at <- rep(1, 10)
		# if (i > yeare)
		for(j in (i + 1):min(i + 9, yeare + 1))
		{
			if(leapyear(j)) at[j - i + 1] <- at[j - i] + 366 else
			  at[j - i + 1] <- at[j - i] + 365
		}
		
		tmp.dates <- format(pcict.dates,format="%Y")
		ttmp <- cio@data[[var1]][tmp.dates>=i & tmp.dates <= min(i + 9, yeare)]
		plot(1:length(ttmp), ttmp, type = type, col = "blue",
		  xlab = "", ylab = "", xaxt = "n", xlim = c(1, 3660), ylim = c(ymin, ymax))
		abline(h = 0)
		tt <- seq(1, length(ttmp))
		if(!is.null(ttmp)) tt <- tt[is.na(ttmp) == TRUE] #else print(paste(var,"is null."))
		axis(side = 1, at = at, labels = c(i:(i + 9)))
		for(k in 1:10) abline(v = at[k], col = "yellow")
		lines(tt, rep(ymin, length(tt)), type = "p", col = "red")
		title(paste("Station: ", tit, ", ", i, "~", min(i + 9, yeare), ",  ", var1, sep = ""))
	}
}

# load.data.qc
# This function essentially refers to "Step 1." in the GUI. It reads in the users text file, allows them to enter station information (name, lat/lon), outlier criteria, it then QC's the data and creates
# a climdex input object. Users can also read in previously generated thresholds here to overwrite those created by the climdex input object. An additional file can be read in that contains one
# additional field which can be plotted with the climate indices.
load.data.qc <- function() {
	# Go back to ClimPACT2 home screen
	cancel1 <- function() {
		tkdestroy(infor1)
		cio <<- NULL
		quantiles <<- NULL
		loaded <<- FALSE
                tkconfigure(start.but,bg="lightgreen",text = "   LOAD AND  \n  CHECK DATA   ", command = load.data.qc, width = 15, font = fontHeading2)
                tkconfigure(cal.but,bg="white",text = "   CALCULATE \n   INDICES  ", command = index.calc1, width = 15, font = fontHeading2)
		return() }

	# Load data. This function does the meat of the work and creates the climdex input object.
	load.data <- function() {
		print("LOADING CLIMDEX INPUT OBJECT...",quote=FALSE)
		setTkProgressBar(process.pb,10,label="									")
		setTkProgressBar(process.pb,10,label="Creating climdex input object...")

	# create a PCICt object for dates
		yyymmdd <- paste(data[,1],data[,2],data[,3],sep="-")
		dates <- as.Date(yyymmdd,format="%Y-%m-%d")
		assign('dates',dates,envir=.GlobalEnv)

	# Check dates are all existing, in the correct order and none are NA values.
		date.seq <- seq(dates[1],dates[length(dates)],by="day")

#		if(data[1,2] != "1" || data[1,3] != "1" || data[length(data[,2]),2] != "12" || data[length(data[,3]),3] != "31") { 
#			test <- tkmessageBox(message = "Please ensure your file contains only whole years (i.e. starts on January 1st and ends December 31st).",icon = "warning", title = "ClimPACT2 - warning")
#			close(process.pb)
#			return() }

#		if(length(dates) != length(date.seq)) { 
#                        test <- tkmessageBox(message = "Based on the first and last dates in your input file some intermediate dates seem to be missing. Check that the dates in your input file are complete and based on the gregorian calendar.",icon = "warning", title = "ClimPACT2 - warning")
#			close(process.pb)
#                        return() }
#		if(any(is.na(dates),is.nan(dates))) {
#                        test <- tkmessageBox(message = "Some dates are registering as NA, NaN or infinite. Please check dates in your input file.",icon = "warning", title = "ClimPACT2 - warning")
#                        close(process.pb)
#                        return() }
 #               if(any(dates[1:length(dates)-1] > dates[2:length(dates)])) {
 #                       test <- tkmessageBox(message = "Some dates are out of order. Please check dates in your input file.",icon = "warning", title = "ClimPACT2 - warning")
 #                       close(process.pb)
 #                       return() }

		date.seq <- data.frame(list(time=seq(dates[1],dates[length(dates)],by="day")))
		data_raw = data.frame(list(time=as.Date(yyymmdd,format="%Y-%m-%d"),prec=data[,4],tmax=data[,5],tmin=data[,6]))
		merge_data = merge(data_raw,date.seq,all=TRUE)

		days <- as.Date(as.character(merge_data[,1],format="%Y-%m-%d"))-as.Date("1850-01-01")
		seconds <- as.numeric(days*24*60*60)
		pcict.dates <- as.PCICt(seconds,cal="gregorian",origin=as.character("1850-01-01"))
		assign('pcict.dates',pcict.dates,envir=.GlobalEnv)

		date.months <- unique(format(as.character((merge_data[,1]),format="%Y-%m")))
		date.years <- unique(format(as.character((merge_data[,1]),format="%Y")))
		assign('date.months',date.months,envir=.GlobalEnv)
                assign('date.years',date.years,envir=.GlobalEnv)

	# create a climdex input object
		cio <- climdexInput.raw(tmin=merge_data[,4],tmax=merge_data[,3],prec=merge_data[,2],tmin.dates=pcict.dates,tmax.dates=pcict.dates,prec.dates=pcict.dates,base.range=c(base.year.start,base.year.end),prec.qtiles=prec.quantiles,
			temp.qtiles=temp.quantiles,quantiles=quantiles)

		assign('cio',cio,envir=.GlobalEnv)
		cio@data$dtr <<- cio@data$tmax - cio@data$tmin
		cio@data$tmean <<- (cio@data$tmax + cio@data$tmin)/2
        setTkProgressBar(process.pb,20,label="									")
		setTkProgressBar(process.pb,20,label="Calculating percentiles...")

	# If quantiles have not been read in, then calculate them and write out to file
		nam1 <- paste(outthresdir, paste(ofilename, "_thres.csv", sep = ""),sep="/")

		if(!is.null(temp.quantiles)) {
	                # get tavg quantiles if not loaded by user, since these aren't automatically generated by climdex.pcic
			tavgqtiles <- get.outofbase.quantiles(cio@data$tavg,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=c(base.year.start,base.year.end),temp.qtiles=temp.quantiles,prec.qtiles=NULL)
			cio@quantiles$tavg$outbase <<- tavgqtiles$tmax$outbase	# while this says tmax it is actually tavg, refer to above line.

			# heat wave thresholds
			tavg <- (cio@data$tmax + cio@data$tmin)/2
			Tavg90p <- suppressWarnings(get.outofbase.quantiles(tavg,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=c(base.year.start,base.year.end),n=15,temp.qtiles=0.9,prec.qtiles=NULL,
	                                                                min.base.data.fraction.present=0.1))
			TxTn90p <- suppressWarnings(get.outofbase.quantiles(cio@data$tmax,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=c(base.year.start,base.year.end),n=15,temp.qtiles=0.9,prec.qtiles=NULL,
	                                                                min.base.data.fraction.present=0.1))
	                tn90p <<- TxTn90p$tmin$outbase
	                tx90p <<- TxTn90p$tmax$outbase
	                tavg90p <<- Tavg90p$tmax$outbase

			# write to file
	                thres <- c(cio@quantiles$tmax$outbase,cio@quantiles$tmin$outbase,cio@quantiles$tavg$outbase,cio@quantiles$prec,as.list(tn90p),as.list(tx90p),as.list(tavg90p))#,cio@dates,cio@data)#$tmin,cio@data$tmax,cio@data$prec)

			write.table(as.data.frame(thres), file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "NA", col.names = c(paste("tmax",names(cio@quantiles$tmax$outbase),sep="_"),paste("tmin",names(cio@quantiles$tmin$outbase),sep="_"),
				paste("tavg",names(cio@quantiles$tavg$outbase),sep="_"),paste("prec",names(cio@quantiles$prec),sep="_"),"HW_TN90","HW_TX90","HW_TAVG90"),row.names=FALSE) 
	
	        	# write raw tmin, tmax and prec data for future SPEI/SPI calcs
		        yeardate2 <<- format(dates,format="%Y")
			base.dates <- dates[which(yeardate2 >= base.year.start & yeardate2 <= base.year.end)]
			thres2 <- list(dates=base.dates,tmin=cio@data$tmin[which(yeardate2 >= base.year.start & yeardate2 <= base.year.end)],tmax=cio@data$tmax[which(yeardate2 >= base.year.start & yeardate2 <= base.year.end)],
				prec=cio@data$prec[which(yeardate2 >= base.year.start & yeardate2 <= base.year.end)])
			nam2 <- paste(outthresdir, paste(ofilename, "_thres_spei.csv", sep = ""),sep="/")
	                write.table(as.data.frame(thres2), file = nam2, append = FALSE, quote = FALSE, sep = ", ", na = "NA", col.names = c("Base_period_dates","Base_period_tmin","Base_period_tmax","Base_period_prec"),row.names=FALSE)
		} else { 
                        tn90p <<- hwlist[[2]]
                        tx90p <<- hwlist[[1]]
                        tavg90p <<- hwlist[[3]]
		}

	# thresholds are automatically calculated when creating a climdex input object, so set this flag to TRUE
                thres.calc <<- TRUE #tclvalue(thres.yes)=1
	}
	
	# Leave the QC screen to go back to main menu
	ok1 <- function() {
		# Users want to press "continue".
		if(!loaded)
		{
		  # You must do something first - QC.
		 tkmessageBox(message='You must process the data before continuing.',icon='warning')
		 return()
		}
		tkdestroy(infor1)
		tkfocus(start1) 
	}
	
	# this function gets user-defined base period, and check if they're valid input.
	get_base <- function() {
		base.year.start<-as.numeric(tclvalue(base.year.start.tcl));  assign("base.year.start",base.year.start,envir=.GlobalEnv)
		base.year.end<-as.numeric(tclvalue(base.year.end.tcl));    assign("base.year.end",base.year.end,envir=.GlobalEnv)
		tclvalue(fail_base)=F 
	} 

	# users want to open an existing *_thres.csv file.
	read.threshold <- function() {
        if(loaded) {
                tkmessageBox(message='You cannot load thresholds after processing your data. To load thresholds please select CANCEL at the bottom of the screen and start STEP 1 again.',icon='warning')
		return() }

		get_base()           # get base period.
		if(tclvalue(fail_base)=='1') return()
		thres.file.in <- tclvalue(tkgetOpenFile(filetypes="{{csv Files} {.csv}} {{All files} *}",initialdir=outthresdir)) # choose thres file.
		if(thres.file.in=="")return()    # users change mind, don't want to open existing file, so return to previous window, and choose another option.

		# read in previously written thresholds
		print("READING THRESHOLDS...",quote=FALSE)
		prev.qtiles <- (read.csv(thres.file.in,header=T,sep=','))   # read in thres data.
		quantiles <<- hwquantiles <<- list()

		tminlist=vector("list",5)
                tmaxlist=vector("list",5)
                tavglist=vector("list",5)
		names(tminlist) <- c("q95","q90","q50","q10","q5")
                names(tmaxlist) <- c("q95","q90","q50","q10","q5")
                names(tavglist) <- c("q95","q90","q50","q10","q5")
		for (l in 1:length(tminlist)) { tminlist[[l]]=(eval(parse(text=paste("(prev.qtiles$tmin_",names(tminlist)[l],")",sep="")))) }
                for (l in 1:length(tmaxlist)) { tmaxlist[[l]]=(eval(parse(text=paste("(prev.qtiles$tmax_",names(tmaxlist)[l],")",sep="")))) }
                for (l in 1:length(tavglist)) { tavglist[[l]]=(eval(parse(text=paste("(prev.qtiles$tavg_",names(tavglist)[l],")",sep="")))) }
		quantiles$tmin$outbase <<- (tminlist)
                quantiles$tmax$outbase <<- (tmaxlist)
                quantiles$tavg$outbase <<- (tavglist)
                quantiles$tmin$inbase <<- (tminlist)
                quantiles$tmax$inbase <<- (tmaxlist)
                quantiles$tavg$inbase <<- (tavglist)

                preclist=vector("list",6)
		names(preclist) <- c("q99","q95","q90","q50","q10","q5")
                for (l in 1:length(preclist)) { preclist[[l]]=(eval(parse(text=paste("(prev.qtiles$prec_",names(preclist)[l],"[1])",sep="")))) }	# Only read the first element since precip quantiles are the same for each day.
		quantiles$prec <<- (preclist)

		# read in heat wave related thresholds
		hwlist<<-vector("list",3)
		names(hwlist) <<- c("HW_TX90","HW_TN90","HW_TAVG90")
                for (l in 1:length(hwlist)) { hwlist[[l]]<<-eval(parse(text=paste("(prev.qtiles$",names(hwlist)[l],")",sep=""))) }

                # Auto-read SPEI .csv file without the user's knowledge. Clunky and risks breaking if SPEI file doesn't exist?
                spei.file.in <- paste(substr(thres.file.in,1,(nchar(thres.file.in)-4)),"_spei.csv",sep="")
                spei.qtiles <- read.csv(spei.file.in,header=T,sep=',',na.strings="NA",colClasses=c('character','numeric','numeric','numeric'))

				speitmax <<- spei.qtiles$Base_period_tmax ; speitmin <<- spei.qtiles$Base_period_tmin ; speiprec <<- spei.qtiles$Base_period_prec ; speidates <<- spei.qtiles$Base_period_dates

		tkfocus(infor1)
		tkconfigure(msg,text='',font=font_small)
		thres.calc <<- TRUE
		temp.quantiles <<- NULL
		prec.quantiles <<- NULL
		print("COMPLETED READING THRESHOLDS.",quote=FALSE)
	}

        # users want to open an existing *_thres.csv file.
        read.threshold.spei <- function() {
                thres.file.in <- tclvalue(tkgetOpenFile(filetypes="{{csv Files} {.csv}} {{All files} *}",initialdir=outthresdir)) # choose thres file.
                if (thres.file.in=="")return()    # users change mind, don't want to open existing file, so return to previous window, and choose another option.

                # read in previously written thresholds and overwrite/append cio object
                print("READING THRESHOLDS...",quote=FALSE)
                prev.qtiles <- read.csv(thres.file.in,header=T,sep=',')   # read in thres data.
		speidata = list()
                speidata$speitmax <<- prev.qtiles$Base.period.tmax ; speidata$speitmin <<- prev.qtiles$Base.period.tmin ; speidata$speiprec <<- prev.qtiles$Base.period.prec ; speidata$speidates <<- prev.qtiles$Base.period.dates
                tkfocus(infor1)
                tkconfigure(msg,text='',font=font_small)
                thres.calc <<- TRUE
                print("COMPLETED READING THRESHOLDS.",quote=FALSE)
        } # end of reada()
	
	# qcontrol
	# run QC checks, call load.data which creates the climdex input object.
	qcontrol <- function() {
                latitude  <- as.numeric(tclvalue(latentry))   # get user-input parameter, and check if they're valid.
                longitude <- as.numeric(tclvalue(lonentry))
                ofilename <- tclvalue(station.entry)
                stddev.crit <- as.numeric(tclvalue(nstddev))

                if (is.na(latitude)  == TRUE | is.na(longitude) == TRUE | latitude < -90 | latitude > 90 | longitude > 180 | longitude < -180) {
                  tkmessageBox(message = paste("Please enter a valid latitude (-90 to +90) and longitude (-180 to +180).",sep = ""))
                  return() }

                yyymmdd <- paste(data[,1],data[,2],data[,3],sep="-")
                dates <- as.Date(yyymmdd,format="%Y-%m-%d")
                base.year.start<<-as.numeric(tclvalue(base.year.start.tcl)) ;  assign("base.year.start",base.year.start,envir=.GlobalEnv)
                base.year.end<<-as.numeric(tclvalue(base.year.end.tcl)) ;  assign("base.year.end",base.year.end,envir=.GlobalEnv)

        # Check base period is valid when no thresholds loaded
                if(is.null(quantiles)) {
                        if(base.year.start < format(dates[1],format="%Y") | base.year.end > format(dates[length(dates)],format="%Y") | base.year.start > base.year.end) {
                                tkmessageBox(message = paste("Base period must be between ", format(dates[1],format="%Y")," and ",format(dates[length(dates)],format="%Y"),". Please correct.",sep="")) ; return() }
                }

	# Check base period is valid when thresholds ARE loaded. 
                if (!is.null(quantiles) && ((base.year.start >= format(dates[1],format="%Y") && base.year.start <= format(dates[length(dates)],format="%Y")) | (base.year.end <= format(dates[length(dates)],format="%Y") && base.year.end >= format(dates[1],format="%Y"))))
				{
                	tkmessageBox(message = paste("The base period of your loaded thresholds ","(",base.year.start," to ",base.year.end,") must lie outside of the current data's date range (",format(dates[1],format="%Y")," to ",
        	              format(dates[length(dates)],format="%Y"),").",sep = ""))
	                return() 
				}

                process.pb <<- tkProgressBar("%", "Checking latitude/longitude, base period...",0, 100, 10)

                # source climpact code and load data from ascii file into climdex object
 #               source("climpact2.r")

	# NICK: After this point all references to data should be made to the climdex input object 'cio'. One exception is the allqc function, 
	# which still references the INPUT to the climdex.input function.
		load.data() ; if(!thres.calc) return()
		setTkProgressBar(process.pb,30,label="									")
		setTkProgressBar(process.pb,30,label="Checking precipitation data...")

		assign("latitude",  latitude, envir = .GlobalEnv)
		assign("longitude", longitude, envir = .GlobalEnv)
		if(latitude<0) lat_text = "°S" else lat_text = "°N"
		if(longitude<0) lon_text = "°W" else lon_text = "°E"
		Encoding(lon_text) <- "UTF-8"	# to ensure proper plotting of degree symbol in Windows (which uses Latin encoding by default)
		Encoding(lat_text) <- "UTF-8"
		title.station <- paste(ofilename, " [", latitude,lat_text, ", ", longitude,lon_text, "]", sep = "")
		assign("title.station", title.station, envir = .GlobalEnv)
		assign("ofilename", ofilename, envir = .GlobalEnv)
		
		# QC 1.
		# search for precip < 0 and write to file. 
#		bad.prec.ind <- which(!is.na(cio@data$prec) & cio@data$prec < 0)
#		bad.prec = cbind.data.frame(as.character(cio@dates[bad.prec.ind]),cio@data$prec[bad.prec.ind])
#		nam1 <- paste(outlogdir, paste(ofilename, "_prcpQC.csv", sep = ""), sep = "/")
#		write.table(bad.prec, file = nam1, append = FALSE, quote = FALSE, sep = ", ", row.names = FALSE,col.names=c("Date","Prec"))
##                if (any(!is.na(cio@data$prec) & cio@data$prec < 0)) {
##                        tkmessageBox(message = paste("ERROR: Negative precipitation values were found and require correcting before indices can be calculated.\nView values in:\n ",nam1,sep = ""))
##			cio <<- NULL
##			tkdestroy(infor1)
##			tkfocus(start1)
##			close(process.pb)
##			return()
##		}
		setTkProgressBar(process.pb,40,label="									")
		setTkProgressBar(process.pb,40,label="Creating QC plots...")

		# QC 2.
		# output plots for tmin, tmax, prcp and dtr
		nam1 <- paste(outlogdir, paste(ofilename, "_prcpPLOT.pdf", sep = ""), sep = "/")
		check_open(nam1)
		pdf(file = nam1)
		
		prcp <- cio@data$prec[cio@data$prec >= 1 & !is.na(cio@data$prec)]

		if(length(prcp) > 30)
		{
			hist(prcp, main = paste("Histogram for Station:", ofilename, " of PRCP>=1mm", sep = ""),breaks = c(seq(0, 40, 2),max(prcp)), xlab = "", col = "green" , freq = FALSE)
			lines(density(prcp, bw = 0.2, from = 1), col = "red")
		}
		pplotts(var = "prcp", tit = ofilename)
		dev.off()
		nam1 <- paste(outlogdir, paste(ofilename, "_tmaxPLOT.pdf", sep = ""), sep = "/")
		check_open(nam1)
		pdf(file = nam1)
		pplotts(var = "tmax", type = "l", tit = ofilename)
		dev.off()
		nam1 <- paste(outlogdir, paste(ofilename, "_tminPLOT.pdf", sep = ""), sep = "/")
		check_open(nam1)
		pdf(file = nam1)
		pplotts(var = "tmin", type = "l", tit = ofilename)
		dev.off()
		nam1 <- paste(outlogdir, paste(ofilename, "_dtrPLOT.pdf", sep = ""), sep = "/")
		check_open(nam1)
		pdf(file = nam1)
		pplotts(var = "dtr", type = "l", tit = ofilename)
		dev.off()

		# QC 3.
		# Find where tmax < tmin or where either are >/< 70 degC, then write to file.
		setTkProgressBar(process.pb,60,label="									")
		setTkProgressBar(process.pb,60,label="Checking temperature values...")
#
#		temiss <- which(cio@data$dtr <= 0 |
#		                 cio@data$tmax <= -70 |
#		                 cio@data$tmax >= 70 |
#		                 cio@data$tmin <= -70 |
#		                 cio@data$tmin >= 70)
#		                 
#		nam1 <- paste(outlogdir, paste(ofilename, "_tempQC.csv", sep = ""), sep = "/")
#		dataout = cbind.data.frame(as.character(cio@dates[temiss]),cio@data$tmax[temiss],cio@data$tmin[temiss],cio@data$prec[temiss],cio@data$dtr[temiss])
#		write.table(dataout, file = nam1, append = FALSE, quote = FALSE, sep = ", ", row.names = FALSE,col.names=c("Date","Tmax","Tmin","Prec","DTR"))
#
#		tmin.rle <- rle(cio@data$tmin[!is.na(cio@data$tmin)])
#		tmax.rle <- rle(cio@data$tmax[!is.na(cio@data$tmax)])
#                tmin.rle.na <- rle(cio@data$tmin)
#                tmax.rle.na <- rle(cio@data$tmax)
#
#		length.arrays = any(tmin.rle$lengths[tmin.rle$values==0] > 0)
#
#		if(any(tmin.rle$lengths[tmin.rle$values==0] > running.zero.allowed.in.temperature)) {
#			# Get index of beginning of anomalous zero run
#			end.index <- sum(tmin.rle.na$lengths[1:which(tmin.rle.na$values==0 & tmin.rle.na$lengths>running.zero.allowed.in.temperature)[1]])
#			beg.index <- end.index-tmin.rle.na$lengths[tmin.rle.na$values==0 & tmin.rle.na$lengths>running.zero.allowed.in.temperature]+1
#
#	                tkmessageBox(message = paste("WARNING: A series of at least ",running.zero.allowed.in.temperature," zeros were found in your minimum temperature data between ",
#				cio@dates[beg.index[1]]," and ",cio@dates[end.index[1]],". Please check these, processing will continue.",sep=""),icon = "warning", title = "ClimPACT2 - warning")
#			rm(beg.index,end.index)
#		}
#                if(any(tmax.rle$lengths[tmax.rle$values==0] > running.zero.allowed.in.temperature)) {
#                        # Get index of beginning of anomalous zero run
#                        end.index <- sum(tmax.rle.na$lengths[1:which(tmax.rle.na$values==0 & tmax.rle.na$lengths>running.zero.allowed.in.temperature)[1]])
#                        beg.index <- end.index-tmax.rle.na$lengths[tmax.rle.na$values==0 & tmax.rle.na$lengths>running.zero.allowed.in.temperature]+1
#
#                        tkmessageBox(message = paste("WARNING: A series of at least ",running.zero.allowed.in.temperature," zeros were found in your maximum temperature data between ",
#                                cio@dates[beg.index[1]]," and ",cio@dates[end.index[1]],". Please check these, processing will continue.",sep=""),icon = "warning", title = "ClimPACT2 - warning")
#                        rm(beg.index,end.index)
#                }
#
#		# NA temperature data at dates where tmax, tmin or dtr is suspicious.
#		cio@data$tmax[temiss] = NA
#		cio@data$tmin[temiss] = NA
#		cio@data$dtr[temiss] = NA
#		cio@data$tavg[temiss] = NA

		# QC 4. Check for outliers based on standard deviations
		# Check for temperatures outside a user-specified number of standard deviations.
		print("CHECKING FOR TEMPERATURE OUTLIERS...",quote=FALSE)
		# find stddev
		all.day.factors <- factor(format(cio@dates, format="%Y-%m-%d", tz="GMT")) # 38351 levels
		day.factors <- factor(format(cio@dates, format="%m-%d", tz="GMT"))	  # 366 levels

		print("CALCULATING MEANS AND STANDARD DEVIATIONS...",quote=FALSE)
		tmax.mean <- tapply(cio@data$tmax,day.factors,mean,na.rm=TRUE)
		tmax.stddev <- sqrt(tapply(cio@data$tmax,day.factors,var,na.rm=TRUE))
		tmin.mean <- tapply(cio@data$tmin,day.factors,mean,na.rm=TRUE)
		tmin.stddev <- sqrt(tapply(cio@data$tmin,day.factors,var,na.rm=TRUE))
		dtr.mean <- tapply(cio@data$dtr,day.factors,mean,na.rm=TRUE)
		dtr.stddev <- sqrt(tapply(cio@data$dtr,day.factors,var,na.rm=TRUE))

		print("TESTING DATA, PLEASE WAIT...",quote=FALSE)
		setTkProgressBar(process.pb,80,label="									")
		setTkProgressBar(process.pb,80,label="Checking for temperature outliers...")
		tmax.outliers <- tapply(1:length(cio@data$tmax),all.day.factors,function(idx) {
			month.day <- format(as.Date(all.day.factors[idx]),format="%m-%d")
			if(!is.na(cio@data$tmax[idx]) && !is.na(tmax.mean[month.day]) && !is.na(tmax.stddev[month.day])) {
				if(abs(cio@data$tmax[idx] - tmax.mean[month.day]) > (stddev.crit*tmax.stddev[month.day])) { return(TRUE) } else { return(FALSE) } }
			else { return(FALSE) } } )
		tmin.outliers <- tapply(1:length(cio@data$tmin),all.day.factors,function(idx) {
				month.day <- format(as.Date(all.day.factors[idx]),format="%m-%d")
		        if(!is.na(cio@data$tmin[idx]) && !is.na(tmin.mean[month.day]) && !is.na(tmin.stddev[month.day])) { 
		                if(abs(cio@data$tmin[idx] - tmin.mean[month.day]) > (stddev.crit*tmin.stddev[month.day])) { return(TRUE) } else { return(FALSE) } }
		        else { return(FALSE) } } )
		dtr.outliers <- tapply(1:length(cio@data$dtr),all.day.factors,function(idx) {
				month.day <- format(as.Date(all.day.factors[idx]),format="%m-%d")
		        if(!is.na(cio@data$dtr[idx]) && !is.na(dtr.mean[month.day]) && !is.na(dtr.stddev[month.day])) { 
		                if(abs(cio@data$dtr[idx] - dtr.mean[month.day]) > (stddev.crit*dtr.stddev[month.day])) { return(TRUE) } else { return(FALSE) } }
		        else { return(FALSE) } } )
		        setTkProgressBar(process.pb,90,label="									")
                setTkProgressBar(process.pb,90,label="Extra QC checks...")

		data <- data[, c("year", "month", "day", "prcp", "tmax", "tmin")]
		assign("data", data, envir = .GlobalEnv)

		# Remnant code from old climpact: writes out data to *_indcal.csv. This isn't read in in new climpact.
		#namcal <- paste(nama, "_indcal.csv", sep = "")  # User should change this file if error was reported because it will be used for all calculation.
		#assign("namcal", namcal, envir = .GlobalEnv)
		#write.table(data, file = namcal, append = FALSE, quote = FALSE, sep = ",", row.names = FALSE, na = "-99.9")

		# QC 5. Call the ExtraQC functions.
		allqc(master = orig.name, output = outqcdir, outrange = 3) #stddev.crit)   # extraQC is called here. NOTE the default outrange=3 in original verson.
		tclvalue(qc.yes) <<- TRUE  # the QC step is done, so you can continue...
		
		print("COMPLETED CHECKING FOR TEMPERATURE OUTLIERS.",quote=FALSE)

		# QC 6. Write out NA statistics.
		naprec = array(NA,dim=c(length(unique(cio@date.factors$annual))))
		naprec = tapply.fast(cio@data$prec,cio@date.factors$annual,function(x) { return(sum(is.na(x))) })
		natx = tapply.fast(cio@data$tmax,cio@date.factors$annual,function(x) { return(sum(is.na(x))) })
		natn = tapply.fast(cio@data$tmin,cio@date.factors$annual,function(x) { return(sum(is.na(x))) })
		
		nam1 <- paste(outqcdir, paste(ofilename, "_nastatistics.csv", sep = ""), sep = "/")
		write_header(nam1)
		# Suppress warning about column names in files
		suppressWarnings(write.table(cbind.data.frame(unique(cio@date.factors$annual),naprec,natx,natn), file = nam1, sep=",",append = TRUE, quote = FALSE, row.names = FALSE, col.names = c("Year","Prec","TX","TN")))

		# If outliers are found above, write out corresponding dates that have the suspect data.
		# This windowing code is admittedly verbose due to poor documentation of tcltk functions and time constraints.

                        close(process.pb)
                        nam1 <- paste(outqcdir, paste(ofilename, "_temp_stddev_QC.csv", sep = ""), sep = "/")
                        write_header(nam1)
                        idx <- which(tmax.outliers==TRUE | tmin.outliers==TRUE | dtr.outliers==TRUE)
                        ofile <- cbind.data.frame(as.character(cio@dates[idx]),cio@data$tmax[idx],cio@data$tmin[idx],cio@data$dtr[idx])
						# Suppress warning about column names in files
                        suppressWarnings(write.table(ofile, file = nam1, append = TRUE, quote = FALSE, sep = ",", row.names = FALSE,col.names=c("Date","Tmax","Tmin","DTR")))
						# If no data (i.e. no large values) in variable print message
						if(dim(ofile)[1]==0) { write.table("NO OUTLIERS FOUND",sep=",", file = nam1, append = TRUE, row.names = FALSE, col.names = FALSE) }

                        tkconfigure(start.but,bg="white",text = "   LOAD AND  \n  CHECK DATA   ", command = load.data.qc, width = 15, font = fontHeading2)
                        tkconfigure(cal.but,bg="lightgreen",text = "   CALCULATE \n   INDICES  ", command = index.calc1, width = 15, font = fontHeading2)

		        proc.complete.done <- function(){
						tkfocus(infor1)
		                tkdestroy(proc.complete) }

                        proc.complete <<- tktoplevel(bg = "white")
                        tkfocus(proc.complete)
                        tkwm.title(proc.complete, "\tClimPACT2\t")
                        tt2 <- tkframe(proc.complete,bg="white")
                        frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
                        tkgrid(frame.space); tkgrid(tklabel(tt2, text = "QUALITY CONTROL COMPLETE", bg = "white", font = fontHeading2),columnspan=1);tkgrid(frame.space); tkgrid(tt2)

                        tt2 <- tkframe(proc.complete,bg="white")
                        frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
                        tkgrid(frame.space)
                        tkgrid(tklabel(tt2,text=

						paste("Carefully evaluate output in the following directory \nfor potential issues before continuing.\n\n",outlogdir,
								"\n\nRefer to Appendix G in the ClimPACT2 manual for help.",sep="")
		                ,bg='white',font=font_small,width=75),sticky="nsew")
                        tkgrid(frame.space)
                        tkgrid(tt2)

                        tt2 <- tkframe(proc.complete,bg="white"); frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white");tkgrid(frame.space);tkgrid(tt2)
                        tt2 <- tkframe(proc.complete,bg="white"); ok1.but<-tkbutton(tt2,text="    Done    ",command=proc.complete.done,bg='white',font=font_small);tkgrid(ok1.but);tkgrid(tt2)
                        tt3 <- tkframe(proc.complete,bg="white");frame.space <- tklabel(tt3, text = " ", font = font_small, bg = "white");tkgrid(frame.space);tkgrid(tt3)
                        loaded <<- TRUE
	} # end of qcontrol()

	# additional.data
	# This function reads in a text file from the user that holds an additional column of data (in addition to the precip,tmin,tmax already provided).
	# This additional column of data represents a variable of interest to the user (e.g. wheat yield) and once read in is attached to the climdex input
	# object 'cio'. Thus, the user can repeat this process multiple times to add multiple new columns of data.
	additional.data <- function() {
                if (is.null(cio)) {
                  tkmessageBox(message = paste("Please process your climate data first.",sep = ""))
                  return() }

		ok.add.data<-function(){
	                add.data.name <<- as.character(tclvalue(add.data.name.entry))
	                if (add.data.name=="") {
        	          tkmessageBox(message = paste("Please provide a name for this data first.",sep = ""))
	                  return() }

			tkdestroy(add.data)
		# get a file from user
		        add.file.name <- tclvalue(tkgetOpenFile(filetypes="{{TEXT Files} {.txt}} {{All files} *}"))
		        if (add.file.name=="") { no_file=T; assign('no_file',no_file,envir=.GlobalEnv); return(); tkfocus(start1) }
		        assign('add.file.name',add.file.name,envir=.GlobalEnv)

		# read in data from file
		        add.data <- read.table(add.file.name,header=F,col.names=c("year","month","day",add.data.name),colClasses=rep("real",4))
		        outdirtmp<-strsplit(add.file.name,"/")[[1]]
		        assign("add.data",add.data,envir=.GlobalEnv)

		# check that dates are identical to cio dates
                        add.yyymmdd <- paste(add.data[,1],add.data[,2],add.data[,3],sep="-")
                        add.dates <- as.Date(add.yyymmdd,format="%Y-%m-%d")

                        orig.yyymmdd <- paste(data[,1],data[,2],data[,3],sep="-")
                        orig.dates <- as.Date(orig.yyymmdd,format="%Y-%m-%d")

			if(any(orig.dates!=add.dates)) {
                          tkmessageBox(message = paste("The dates in this file must be identical to the dates in the climate data file provided:\n",orig.name.user,sep = ""))
                          return() } else {

			  if(add.operation=="sum") {
				  add.data.monthly <- tapply(add.data[,add.data.name],cio@date.factors$monthly,sum,na.rm=TRUE)
				  add.data.annual <- tapply(add.data[,add.data.name],cio@date.factors$annual,sum,na.rm=TRUE)
			  } else {
                                  add.data.monthly <- tapply(add.data[,add.data.name],cio@date.factors$monthly,mean,na.rm=TRUE)
                                  add.data.annual <- tapply(add.data[,add.data.name],cio@date.factors$annual,mean,na.rm=TRUE)
			  }
                          cio@data$add.data.monthly <<- add.data.monthly
                          cio@data$add.data.annual <<- add.data.annual

			  tkdestroy(infor1)
                          tkmessageBox(message = paste("File read successfully.",sep = ""))
                          return() }
		}
		cancel.add.data <- function(){
			add.data.name.entry <-tclVar('')
			tkdestroy(add.data)
			tkfocus(infor1)
		}

                add.data <<- tktoplevel(bg = "white")
	        tkwm.title(add.data, "ClimPACT2 - Additional data")
                tt2 <- tkframe(add.data,bg="white")
                frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
                tkgrid(frame.space)
                add.data.name.widget <- tkentry(tt2, width = 20, textvariable = add.data.name.entry, bg = "white")
                tkgrid(tklabel(tt2, text = "Enter name of additional variable", bg = "white", font = font_small))
                tkgrid(frame.space)
                tkgrid(add.data.name.widget)
                tkgrid(tt2)
                tkpack(tt2)

                tt2 <- tkframe(add.data,bg="white")
                frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
                tkgrid(frame.space)
	        rb1 <- tkradiobutton(tt2)
        	rb2 <- tkradiobutton(tt2)
	        rbValue <- tclVar("sum")
	        tkconfigure(rb1,variable=rbValue,value="sum",bg='white')
	        tkconfigure(rb2,variable=rbValue,value="mean",bg='white')
	        tkgrid(tklabel(tt2,text="Sum or mean the data over months and years:",bg='white',font=font_small))
	        tkgrid(tklabel(tt2,text="sum ",bg='white',font=font_small),rb1)
	        tkgrid(tklabel(tt2,text="mean ",bg='white',font=font_small),rb2)
                add.operation <- as.character(tclvalue(rbValue)) ; assign("add.operation",add.operation,envir=.GlobalEnv)
                tkgrid(tt2)
                tkpack(tt2)

                tt2 <- tkframe(add.data,bg="white")
                ok1.but<-    tkbutton(tt2,text="    OK    ",command=ok.add.data,bg='white',font=font_small)
                cancel1.but<-tkbutton(tt2,text="  CANCEL  ",command=cancel.add.data,bg='white',font=font_small)
                tkgrid(ok1.but,cancel1.but)
		tkgrid(tt2)
		tkpack(tt2)
	}

	create.dir <- function() {
		file.name=outdirtmp[length(outdirtmp)]
		e=strsplit(file.name,"\\.")[[1]]
		ofilename=substr(file.name,start=1,stop=nchar(file.name)-nchar(e[length(e)])-1)
		assign('ofilename',ofilename,envir=.GlobalEnv)

		# create directory names
		if(length(outdirtmp)<=2) {
			dirsplit<-strsplit(dir.file.name,":")[[1]][1]
			outinddir<-paste(dirsplit,"indices",sep=":/")
			outlogdir<-paste(dirsplit,"qc",sep=":/")
			outjpgdir<-paste(dirsplit,"plots",sep=":/")
			outtrddir<-paste(dirsplit,"trend",sep=":/")
			outthresdir<-paste(dirsplit,"thres",sep=":/")  # to save *_thres.csv files   
			outqcdir<-paste(dirsplit,"qc",sep=":/")   # save results from extraqc
		} else{
			outdir<-outdirtmp[1]
			for(i in 2:(length(outdirtmp)-1))
			outdir<-paste(outdir,outdirtmp[i],sep="/")
			outinddir<-paste(outdir,"indices",sep="/")
			outlogdir<-paste(outdir,"qc",sep="/")
			outjpgdir<-paste(outdir,"plots",sep="/")
			outtrddir<-paste(outdir,"trend",sep="/")
			outqcdir<-paste(outdir,"qc",sep="/")    # save results from extraqc
			outthresdir<-paste(outdir,"thres",sep="/")   # to save *_thres.csv files 
		}

		# Create subdirectories if non-existent
		if(!file.exists(paste(outinddir,ofilename,sep="/"))) { dir.create(outinddir,showWarnings=FALSE) ; dir.create(paste(outinddir,ofilename,sep="/")) }
		if(!file.exists(paste(outlogdir,ofilename,sep="/"))) { dir.create(outlogdir,showWarnings=FALSE) ; dir.create(paste(outlogdir,ofilename,sep="/")) }
		if(!file.exists(paste(outjpgdir,ofilename,sep="/"))) { dir.create(outjpgdir,showWarnings=FALSE) ; dir.create(paste(outjpgdir,ofilename,sep="/")) }
		if(!file.exists(paste(outtrddir,ofilename,sep="/"))) { dir.create(outtrddir,showWarnings=FALSE) ; dir.create(paste(outtrddir,ofilename,sep="/")) }
		if(!file.exists(paste(outqcdir,ofilename,sep="/")))  { dir.create(outqcdir,showWarnings=FALSE) ; dir.create(paste(outqcdir,ofilename,sep="/")) }
		if(!file.exists(paste(outthresdir,ofilename,sep="/"))) { dir.create(outthresdir,showWarnings=FALSE) ; dir.create(paste(outthresdir,ofilename,sep="/")) }

		# modify subdirectory names
		outinddir <- paste(outinddir,ofilename,sep="/")
		outlogdir <- paste(outlogdir,ofilename,sep="/")
		outjpgdir <- paste(outjpgdir,ofilename,sep="/")
		outtrddir <- paste(outtrddir,ofilename,sep="/")
		outqcdir <- paste(outqcdir,ofilename,sep="/")
		outthresdir <- paste(outthresdir,ofilename,sep="/")

		# save the directory as global variable for use somewhere else.
		assign("nama",nama,envir=.GlobalEnv)
		assign("outinddir",outinddir,envir=.GlobalEnv)
		assign("outlogdir",outlogdir,envir=.GlobalEnv)
		assign("outjpgdir",outjpgdir,envir=.GlobalEnv)
		assign("outtrddir",outtrddir,envir=.GlobalEnv)
		assign("outqcdir", outqcdir, envir=.GlobalEnv)
		assign("outthresdir",outthresdir,envir=.GlobalEnv)
	}

######################################################################################################
# BEGIN load.data.qc FUNCTION SEQUENTIAL CODING
######################################################################################################

# get a file from user
	dir.file.name <- tclvalue(tkgetOpenFile(filetypes="{{TEXT Files} {.txt}}"))
	if (dir.file.name=="") { no_file=T; assign('no_file',no_file,envir=.GlobalEnv); return(); tkfocus(start1) } 
        reading.pb <<- tkProgressBar("%", "Reading file...",0, 100, 10)
	nama<-substr(dir.file.name,start=1,stop=(nchar(dir.file.name)-4))
	assign('orig.name.user',dir.file.name,envir=.GlobalEnv)

# If progressing set or reset some variables/objects. Here, any previously created climdex object is deleted.
        loaded <<- FALSE
        cio <<- NULL
        quantiles <<- NULL
	temp.quantiles <<- temp.quantiles.default
	prec.quantiles <<- prec.quantiles.default

        tkconfigure(start.but,bg="lightgreen",text = "   LOAD AND  \n  CHECK DATA   ", command = load.data.qc, width = 15, font = fontHeading2)
        tkconfigure(cal.but,bg="white",text = "   CALCULATE \n   INDICES  ", command = index.calc1, width = 15, font = fontHeading2)
        qc.yes <<- tclVar(FALSE)   # QC has not been done yet.
        thres.calc <<- FALSE #thres.yes <- tclVar(FALSE) 

# read in data from file
# Scan the file and replace any commas with a tab \t. Since read.table can only handle one separator type at a time.
        setTkProgressBar(reading.pb,30,label="Scanning for comma delimiters...")
	temp.filename = "temporary.data.txt"
	raw.table = readLines(dir.file.name)
	newtext = gsub(",","\t",raw.table)
	cat(newtext,file=temp.filename,sep="\n")
        assign('orig.name',temp.filename,envir=.GlobalEnv)

# Try to catch errors in the formatting of the user's text file gracefully.
	data <- tryCatch(read.table(temp.filename,header=F,col.names=c("year","month","day","prcp","tmax","tmin"),colClasses=rep("real",6)),
			error= function(c) {
				tkmessageBox(message = paste("Your input file doesn't appear to be formatted correctly. \n\nError returned was: ",c$message,
				"\n\nPlease correct your file, see the manual for correct formatting.", sep=""),icon = "warning", title = "ClimPACT2 - warning")
				close(reading.pb)
				tkfocus(start1)
				} )
        setTkProgressBar(reading.pb,60,label="Checking date order...")
	outdirtmp<-strsplit(dir.file.name,"/")[[1]]
	assign("data",data,envir=.GlobalEnv)

# Check that years are in ascending order
	if(!all(data$year == cummax(data$year))) {
                tkmessageBox(message = "Years are not in ascending order, please check your input file.",icon = "warning", title = "ClimPACT2 - warning")
		close(reading.pb)
                tkfocus(start1)
                return()
	}

# create directories
	create.dir()

# replace missing values (-99.9) with NA
        setTkProgressBar(reading.pb,80,label="Formatting missing values...")
	data$prcp[data$prcp==-99.9]=NA ; data[data$tmax==(-99.9),"tmax"]=NA ; data[data$tmin==(-99.9),"tmin"]=NA

	years<-data[1,1] ; yeare<-data[dim(data)[1],1]
	assign("years",years,envir=.GlobalEnv)
	assign("yeare",yeare,envir=.GlobalEnv)

# define some error messages
	err10 <- "no errors in PRCP"  # default error message for QC.
	err20 <- "no errors in temp"
	err40 <- "no errors in outlier"
	msg0  <- "waiting for your thres choice"
	thres_err <- ""
	fail_base <- tclVar(T)

	close(reading.pb)

# DRAW DATA LOADING INTERFACE
# enter station name and the times of standard deviation
	infor1 <<- tktoplevel(bg = "white")
	tkfocus(infor1)
	tkwm.geometry(infor1, "+300+200") # position in upper left corner of screen
	tkwm.title(infor1, "ClimPACT2 - Data preperation")	

        load.help<-function(){    # tip for the title in all plots.
                tkmessageBox(message=paste("Station name: name of the recording station that data originated from.",
					"Latitude/Longitude: geographical coordinates of the station in decimal form. (-90 to +90 and -180 to +180)",
					"Base period: a beginning and end year (four digits) to use as a reference period to calculate certain indices.",
					"Standard deviations for temperature outliers: the distance from the mean of the data that is considered suspicious.",
					"[OPTIONAL] load previous thresholds: if thresholds previously calculated are to be used for percentile indices.",
#					"[OPTIONAL] Load additional field: an additional time-series of data (which can represent anything) to load and plot with the indices. The file must be formatted similarly to your climate data file and must contain identical dates. e.g. [year,month,day,data].",
					sep="\n\n"),icon='question')
        }

	station.entry <- tclVar(ofilename)
	tt1 <- tkframe(infor1, bg = "white")
        frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")
	tkgrid(tklabel(tt1, text = paste("FILE:", orig.name.user, ""), bg = "white", font = font_small))
        tkgrid(tt1)

        tt1 <- tkframe(infor1, bg = "white")
        help1<-tkbutton(tt1,text='?',command=load.help,bg='white')
	tkgrid(tklabel(tt1, text = "ENTER RECORD INFORMATION", bg = "white", font = fontHeading2),help1)
        tkgrid(frame.space)
        tkgrid(tt1)

        tt1 <- tkframe(infor1, bg = "white")
	tkgrid(tklabel(tt1, text = "STATION NAME", bg = "white", font = font_small))
	textEntryWidget1 <- tkentry(tt1, width = 20, textvariable = station.entry, bg = "white")
	tkgrid(textEntryWidget1)#, side = "left")
        frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
	tkgrid(tt1)

	tt1<-tkframe(infor1,bg="white")
	LatEntry <- tkentry(tt1, width = 6, textvariable = latentry, bg = "white")
	LonEntry <- tkentry(tt1, width = 6, textvariable = lonentry, bg = "white")
	tkgrid(tklabel(tt1, text = "LATITUDE:", bg = "white", font = font_small),LatEntry,tklabel(tt1, text="LONGITUDE:", bg = "white", font = font_small),LonEntry)
	tkgrid(tt1)

        tt1<-tkframe(infor1,bg="white")
        frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")
        lab1<-tklabel(tt1,text=' BASE PERIOD',bg='white',font=font_small)
        tkgrid(frame.space)
        tkgrid(frame.space)
	tkgrid(lab1)
        tkgrid(tt1)

        tt1<-tkframe(infor1,bg="white")
        lab2<-tklabel(tt1,text=' to ',bg='white',font=font_small)
        enter1<-tkentry(tt1,width=6,textvariable=base.year.start.tcl,bg='white')
        enter2<-tkentry(tt1,width=6,textvariable=base.year.end.tcl,bg='white')
        tkgrid(enter1,lab2,enter2)
        base.year.start<-as.numeric(tclvalue(base.year.start.tcl));  assign("base.year.start",base.year.start,envir=.GlobalEnv)
        base.year.end<-as.numeric(tclvalue(base.year.end.tcl));    assign("base.year.end",base.year.end,envir=.GlobalEnv)
        tkgrid(tt1)

        tt1<-tkframe(infor1,bg="white")
        frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
	tkgrid(tklabel(tt1, text = "STANDARD DEVIATIONS FOR\nTEMPERATURE OUTLIERS", bg = "white", font = font_small))#, side = "left")
        tkgrid(tt1)
        tt1<-tkframe(infor1,bg="white")
        textEntryWidget2 <- tkentry(tt1, width = 5, textvariable = nstddev, bg = "white")
	tkgrid(textEntryWidget2)
	tkgrid(tt1)
	
	err2 <- tklabel(tt1, text = err20, font = font_small, bg = "white")

        tt1<-tkframe(infor1,bg="white")
        tkgrid(tklabel(tt1, text = "    ", bg = "white"));
        t1=tkbutton(tt1,text='load previous thresholds',command=read.threshold,bg='white',font=fontTextLabel)
        t2=tkbutton(tt1,text='load SPEI/SPI thresholds',command=read.threshold.spei,bg='white',font=fontTextLabel)
        tkgrid(t1)
        msg=tklabel(tt1,text=thres_err,bg='white')
        tkgrid(msg)
        tkgrid(tt1)

        tt1<-tkframe(infor1,bg="white")
        frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")
	tkgrid(tkbutton(tt1, text = "PROCESS AND\nQUALITY CONTROL",command = qcontrol, font = fontHeading2, bg = "white"))
        tkgrid(frame.space)
        tkgrid(frame.space)
        tkgrid(tt1)

#        tt1<-tkframe(infor1,bg="white")
#        tkgrid(tkbutton(tt1, text = "  Load an additional climate field ",command = additional.data, font = fontTextLabel, bg = "white"))
#        frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")
#        tkgrid(frame.space)
#        tkgrid(tt1)
#        tkpack(tt1)

	tt1<-tkframe(infor1,bg="white")
	ok1.but<-    tkbutton(tt1,text="    OK    ",command=ok1,bg='white',font=font_small)
	cancel1.but<-tkbutton(tt1,text="  CANCEL  ",command=cancel1,bg='white',font=font_small)
	tkgrid(ok1.but,cancel1.but)
        frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
        tkgrid(tt1)
} # END OF load.data.qc

# function leapyear
# return True (T) if leapyear, esle F
leapyear <- function(year)
{
  remainder400 <- trunc(year - 400 * trunc(year / 400));
  remainder100 <- trunc(year - 100 * trunc(year / 100));
  remainder4   <- trunc(year - 4 * trunc(year / 4));
  if (remainder400 == 0) leapyear <- TRUE else
  {
    if (remainder100 == 0) leapyear <- FALSE else
    {
      if(remainder4 == 0) leapyear <- TRUE else leapyear <- FALSE;
    }
  }
}

# index.calc1
# This function houses the beginning screen for "Step 2" in the GUI (i.e. calculating the indices). It reads in user preferences for the indices and calls the index functions for calculation and plotting.
index.calc1 <- function() {
	if(is.null(cio)) {
		tkmessageBox(message = "Please load and \ncheck data first.",
		icon = "warning", title = "ClimPACT2 - warning")
		tkfocus(start1)
		return()
	}

	tkdestroy(infor1)
	infor <- tktoplevel(bg = "white")
	tkfocus(infor)
	tkgrab.set(infor)
	tkwm.geometry(infor, "+300+200")
	tkwm.title(infor,"Set Parameter Values")
	
	textEntry6  <- Entry6
	textEntry7  <- Entry7  # show default values
	textEntry8  <- Entry8
	textEntry9  <- Entry9
	textEntry12 <- Entry12
	textEntry13 <- Entry13
	textEntry14 <- Entry14
	textEntry15 <- Entry15
	textEntry16 <- Entry16
	textEntry17 <- Entry17
	textEntry20 <- Entry20  #for user-defined location-specific base temperature Tb
	textEntry21 <- Entry21
	textEntry22 <- Entry22
	textEntry23 <- Entry23
	textEntry24 <- Entry24

	
	tkpack(tklabel(infor, text = "User defined parameters for Indices Calculation", font = fontHeading1, bg = "white"), side = "top")
	
	help.title<-function(){
		tkmessageBox(message=paste('# = station name, \n* = index name.\n e.g. If you input "station #, index *"\nYou can get \nstation ',title.station,', index TXx',sep=''),icon='question')
	}
	
	tt1=tkframe(infor,bg='white')   # add a "?" to the current window.
	textEntry3<-tclVar('Station: #')
	textEntryWidget3<-tkentry(tt1,width=30,textvariable=textEntry3,bg='white')
	help1=tkbutton(tt1,text=' ? ',command=help.title,bg='white')
	
	# User defined title for plotting
	tkpack(tklabel(tt1,text='User defined title for plotting:',bg='white',font=font_small),side='left')
	tkpack(textEntryWidget3,side='left')
	tkpack(tklabel(tt1,text='    ',bg='white'),side='left')
	tkpack(help1,side='right')
	tkpack(tt1)
	
	tt1<-tkframe(infor,bg='white')

	textEntryWidget6<-tkentry(tt1,width=10,textvariable=textEntry6,bg='white')
	textEntryWidget7<-tkentry(tt1,width=10,textvariable=textEntry7,bg='white')
	textEntryWidget8<-tkentry(tt1,width=10,textvariable=textEntry8,bg='white')
	textEntryWidget9<-tkentry(tt1,width=10,textvariable=textEntry9,bg='white')
	textEntryWidget12<-tkentry(tt1,width=10,textvariable=textEntry12,bg='white')	
	textEntryWidget13<-tkentry(tt1,width=10,textvariable=textEntry13,bg='white') # WSDI
	textEntryWidget14<-tkentry(tt1,width=10,textvariable=textEntry14,bg='white') # CSDI
	textEntryWidget15<-tkentry(tt1,width=10,textvariable=textEntry15,bg='white') # RX
	textEntryWidget16<-tkentry(tt1,width=10,textvariable=textEntry16,bg='white') # TXTN
	textEntryWidget20<-tkentry(tt1,width=10,textvariable=textEntry20,bg='white') # Tb for HDDheat
	textEntryWidget21<-tkentry(tt1,width=10,textvariable=textEntry21,bg='white') # Tb for CDDcold
	textEntryWidget22<-tkentry(tt1,width=10,textvariable=textEntry22,bg='white') # Tb for GDDgrow
    textEntryWidget17<-tkentry(tt1,width=10,textvariable=textEntry17,bg='white') # Rnnmm
	textEntryWidget23<-tkentry(tt1,width=10,textvariable=textEntry23,bg='white')
	textEntryWidget24<-tkentry(tt1,width=10,textvariable=textEntry24,bg='white')

	rb1 <- tkradiobutton(tt1)
	rb2 <- tkradiobutton(tt1)
	rbValue <- tclVar("annual")
	tkconfigure(rb1,variable=rbValue,value="monthly",bg='white')
	tkconfigure(rb2,variable=rbValue,value="annual",bg='white')
	tkgrid(tklabel(tt1,text="Select frequency of output for relevant indices:",bg='white',font=font_small))
	tkgrid(tklabel(tt1,text="month ",bg='white',font=font_small),rb1)
	tkgrid(tklabel(tt1,text="annual ",bg='white',font=font_small),rb2)

	tkgrid(tklabel(tt1,text="",bg='white',font=font_small))
	tkgrid(tklabel(tt1,text="Refer to section 3.6 of ClimPACT2 user guide for help",bg='white',font=font_small_bold))
#	tkgrid(tklabel(tt1,text="  Count the number of days where maximum temperature > ",bg='white',font=font_small),textEntryWidget6)
#	tkgrid(tklabel(tt1,text="  Count the number of days where maximum temperature < ",bg='white',font=font_small),textEntryWidget7)
#	tkgrid(tklabel(tt1,text="  Count the number of days where minimum temperature > ",bg='white',font=font_small),textEntryWidget8)
#	tkgrid(tklabel(tt1,text="  Count the number of days where minimum temperature < ",bg='white',font=font_small),textEntryWidget9)
##	tkgrid(tklabel(tt1,text="User defined daily precipitation threshold",bg='white',font=font_small),textEntryWidget12)
	tkgrid(tklabel(tt1,text="User defined WSDIn Days",bg='white',font=font_small),textEntryWidget13) # 13 wsdi
	tkgrid(tklabel(tt1,text="User defined CSDIn Days",bg='white',font=font_small),textEntryWidget14) # 14 csdi
	tkgrid(tklabel(tt1,text="User defined RxnDay Days",bg='white',font=font_small),textEntryWidget15) # 15 rxday
	tkgrid(tklabel(tt1,text="User defined n for nTXnTN and nTXbnTNb",bg='white',font=font_small),textEntryWidget16) # txtn
	tkgrid(tklabel(tt1,text="User defined base temperature for HDDheat",bg='white',font=font_small),textEntryWidget20) # Tb for HDDheat
	tkgrid(tklabel(tt1,text="User defined base temperature for CDDcold",bg='white',font=font_small),textEntryWidget21) # Tb for CDDcold
	tkgrid(tklabel(tt1,text="User defined base temperature for GDDgrow",bg='white',font=font_small),textEntryWidget22) # Tb for GDDgrow
	tkgrid(tklabel(tt1,text="Count the number of days where precipitation >= nn (Rnnmm)",bg='white',font=font_small),textEntryWidget17)
	tkgrid(tklabel(tt1,text="Calculate SPEI/SPI over custom months (3,6,12 done automatically)",bg='white',font=font_small),textEntryWidget23)

	tkgrid(tklabel(tt1,text="",bg='white',font=font_small))
	tkgrid(tklabel(tt1,text="Custom day count index",bg='white',font=font_small))
	tkgrid(tklabel(tt1,text="(e.g. number of days where TX > 40, named TXgt40)",bg='white',font=font_small))

	user.var <- c("TN","TX","TM","PR","DTR")
	user.op <- c(">",">=","<","<=")
	comboBox1 <- tkwidget(tt1,"ComboBox",editable=FALSE,values=user.var,width=8)
	comboBox2 <- tkwidget(tt1,"ComboBox",editable=FALSE,values=user.op,width=8)

	tkgrid(tklabel(tt1,text="Variable",bg='white',font=font_small),comboBox1)
	tkgrid(tklabel(tt1,text="Operation",bg='white',font=font_small),comboBox2)
	tkgrid(tklabel(tt1,text="Threshold",bg='white',font=font_small),textEntryWidget24)

	tkpack(tt1)

	check.then.continue<-function(){   # get user-definded parameters, check if they're valid, and set as global variable.
		frequency <- as.character(tclvalue(rbValue)) ; assign("frequency",frequency,envir=.GlobalEnv)

		uuu<-as.numeric(tclvalue(textEntry6)); assign("uuu",uuu,envir=.GlobalEnv)
		ulu<-as.numeric(tclvalue(textEntry7)); assign("uul",ulu,envir=.GlobalEnv)
		uul<-as.numeric(tclvalue(textEntry8)); assign("ulu",uul,envir=.GlobalEnv)
		ull<-as.numeric(tclvalue(textEntry9)); assign("ull",ull,envir=.GlobalEnv)
		
		ctmp<-as.character(tclvalue(textEntry3))
		plot.title<-gsub('\\#',title.station,ctmp); assign('plot.title',plot.title,envir=.GlobalEnv)
		
		Entry13<-as.numeric(tclvalue(textEntry13)); assign("wsdi_ud",as.double(Entry13),envir=.GlobalEnv) # 13 wsdi wsdi_ud
		if(Entry13<2 | Entry13>10 ){tkmessageBox(message='WSDI days is incorrect\n\nvalid range is [2, 10]',icon='warning');  return()}
		Entry14<-as.numeric(tclvalue(textEntry14)); assign("csdi_ud",as.double(Entry14),envir=.GlobalEnv)    # 14 csdi_ud
		if(Entry14<2 | Entry14>10 ){tkmessageBox(message='CSDI days is incorrect\n\nvalid range is [2, 10]',icon='warning');  return()}
		
		Entry15<-as.numeric(tclvalue(textEntry15)); assign("rx_ud",as.double(Entry15),envir=.GlobalEnv)# 14 rx_ud
		if(Entry15<2 | Entry15>10 ){tkmessageBox(message='RxDay days is incorrect\n\nvalid range is [2, 10]',icon='warning');  return()}
		Entry16<-as.numeric(tclvalue(textEntry16)); assign("txtn_ud",as.double(Entry16),envir=.GlobalEnv)# txtn_ud
		if(Entry16<2 | Entry16>10 ){tkmessageBox(message='n in nTXnTN and nTXbnTNb is incorrect\n\nvalid range is [2, 10]',icon='warning');  return()}
        Entry17<-as.numeric(tclvalue(textEntry17)); assign("rnnmm_ud",as.double(Entry17),envir=.GlobalEnv)# txtn_ud
        if(Entry17<0 ){tkmessageBox(message='User defined amount of precipitation (mm) for Rnnmm is incorrect\n\nvalid range is [0,Inf)',icon='warning');  return()}

		Entry20<-as.numeric(tclvalue(textEntry20)); assign("Tb_HDD",as.double(Entry20),envir=.GlobalEnv) # Tb for HDDheat
		Entry21<-as.numeric(tclvalue(textEntry21)); assign("Tb_CDD",as.double(Entry21),envir=.GlobalEnv) # Tb for HDDcold
		Entry22<-as.numeric(tclvalue(textEntry22)); assign("Tb_GDD",as.double(Entry22),envir=.GlobalEnv) # Tb for HDDgrow
		Entry23<-as.numeric(tclvalue(textEntry23)); assign("custom_SPEI",as.double(Entry23),envir=.GlobalEnv) # custom SPEI/SPI time period

		var.choice <- user.var[as.numeric(tclvalue(tcl(comboBox1,"getvalue")))+1]; assign("var.choice",var.choice,envir=.GlobalEnv)
		op.choice <- user.op[as.numeric(tclvalue(tcl(comboBox2,"getvalue")))+1]; assign("op.choice",op.choice,envir=.GlobalEnv)
		constant.choice <- as.numeric(tclvalue(textEntry24)); assign("constant.choice",constant.choice,envir=.GlobalEnv)

		tkgrab.release(infor);    tkdestroy(infor)
		index.calc2()
	}  # end of function check.then.continue
  
	cancel1<-function() {  # Users don't want to continue, so close this window and return to main window.
		tkdestroy(infor)
		return()
	}
  
	tt1<-tkframe(infor)
	ok1.but<-    tkbutton(tt1,text=" CALCULATE INDICES ",command=check.then.continue,bg='white',font=font_small)
	cancel1.but<-tkbutton(tt1,text="  CANCEL  ",command=cancel1,bg='white',font=font_small)
	tkgrid(ok1.but,cancel1.but)
	tkpack(tt1)
} # end of index.calc1

# done
done<-function(){tkdestroy(start1)}

# index.calc2
# Final index calculation window and is called from index.calc1. User selects which indices to calculate. Index function calls come from this function.
index.calc2<-function(){
	# fucntion index.calc3 is triggered by the OK button
	# Does all the calculations.
	index.calc3 <- function(){
		# pdf file for all plots
		cbv=rep(0,length.indices)
		for(i in 1:length.indices) cbv[i]=tclvalue(cbvalue[i])
		if(all(cbv==0)) {tkmessageBox(message="Please select at least one index to calculate.",title="ClimPACT2", icon='warning') ; return() }

		# Check 'all' PDF isn't open, then open.
		if(frequency == "monthly") { pdfname = paste(ofilename,"_MON_all_plots.pdf",sep="") }
		else if(frequency == "annual") { pdfname = paste(ofilename,"_ANN_all_plots.pdf",sep="") }
		
#		check_open(pdfname)
		tmp = try(pdf(file=paste(outjpgdir,pdfname,sep="/"),height=8,width=11.5))
		if(class(tmp)=="try-error") { tkmessageBox(message=paste("Error encountered, please check that the file ",pdfname," is not currently open, then select OK to try again.",sep=""),icon='warning'); return() }
		pdf.dev=dev.cur()
		assign('pdf.dev',pdf.dev,envir=.GlobalEnv)

		pb <- tkProgressBar("Index calculation progress", "Calculation complete %",0, 100, 10)
		
		# trend file
		trend_file<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/") ; assign('trend_file',trend_file,envir=.GlobalEnv)
		write_header(trend_file)
		cat(file=trend_file,paste("Lat","Lon","Indices","SYear","EYear","Slope","STD_of_Slope","P_Value",sep=","),fill=180,append=T)

		index_not_calculated=''   # contains index names that could not be calculated.
		assign('index_not_calculated',index_not_calculated,envir=.GlobalEnv)

		#=============================================================
		# Calculate selected indices
		#============================================================

		if (cbv[1]==1) { print(paste("calculating",indices[1]),quote=FALSE) ; index.store <- climdex.fd(cio) ; write.index.csv(index.store,index.name=indices[1]) ; plot.call(index.store,index.name=indices[1],index.units=units[1],x.label="Years",sub=subtitle[1]) ;
		cat(file=trend_file,paste(latitude,longitude,indices[1],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[2]==1) { print(paste("calculating",indices[2]),quote=FALSE) ; index.store <- climdex.fd2(cio) ; write.index.csv(index.store,index.name=indices[2]) ; plot.call(index.store,index.name=indices[2],index.units=units[2],x.label="Years",sub=subtitle[2]) 
                cat(file=trend_file,paste(latitude,longitude,indices[2],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[3]==1) { print(paste("calculating",indices[3]),quote=FALSE) ; index.store <- climdex.fdm2(cio) ; write.index.csv(index.store,index.name=indices[3]) ; plot.call(index.store,index.name=indices[3],index.units=units[3],x.label="Years",sub=subtitle[3]) 
                cat(file=trend_file,paste(latitude,longitude,indices[3],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[4]==1) { print(paste("calculating",indices[4]),quote=FALSE) ; index.store <- climdex.fdm20(cio) ; write.index.csv(index.store,index.name=indices[4]) ; plot.call(index.store,index.name=indices[4],index.units=units[4],x.label="Years",sub=subtitle[4]) 
                cat(file=trend_file,paste(latitude,longitude,indices[4],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[5]==1) { print(paste("calculating",indices[5]),quote=FALSE) ; index.store <- climdex.id(cio) ; write.index.csv(index.store,index.name=indices[5]) ; plot.call(index.store,index.name=indices[5],index.units=units[5],x.label="Years",sub=subtitle[5]) 
                cat(file=trend_file,paste(latitude,longitude,indices[5],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[6]==1) { print(paste("calculating",indices[6]),quote=FALSE) ; index.store <- climdex.su(cio) ; write.index.csv(index.store,index.name=indices[6]) ; plot.call(index.store,index.name=indices[6],index.units=units[6],x.label="Years",sub=subtitle[6]) 
                cat(file=trend_file,paste(latitude,longitude,indices[6],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[7]==1) { print(paste("calculating",indices[7]),quote=FALSE) ; index.store <- climdex.tr(cio) ; write.index.csv(index.store,index.name=indices[7]) ; plot.call(index.store,index.name=indices[7],index.units=units[7],x.label="Years",sub=subtitle[7]) 
                cat(file=trend_file,paste(latitude,longitude,indices[7],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[8]==1) { print(paste("calculating",indices[8]),quote=FALSE) ; if(latitude < 0) cio@northern.hemisphere <<- FALSE ; 
			index.store <- climdex.gsl(cio) ; write.index.csv(index.store,index.name=indices[8]) ; plot.call(index.store,index.name=indices[8],index.units=units[8],x.label="Years",sub=subtitle[8]) 
                cat(file=trend_file,paste(latitude,longitude,indices[8],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[9]==1) { print(paste("calculating",indices[9]),quote=FALSE) ; index.store <- climdex.txx(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[9],freq=frequency) ; plot.call(index.store,index.name=indices[9],index.units=units[9],x.label="Years",sub=subtitle[9],freq=frequency) 
                cat(file=trend_file,paste(latitude,longitude,indices[9],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		setTkProgressBar(pb,20,title="20%")
		if (cbv[10]==1) { print(paste("calculating",indices[10]),quote=FALSE) ; index.store <- climdex.tnn(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[10],freq=frequency) ; plot.call(index.store,index.name=indices[10],index.units=units[10],x.label="Years",sub=subtitle[10],freq=frequency) 
                cat(file=trend_file,paste(latitude,longitude,indices[10],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[11]==1) { print(paste("calculating",indices[11]),quote=FALSE) ; index.store <- climdex.tnx(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[11],freq=frequency) ; plot.call(index.store,index.name=indices[11],index.units=units[11],x.label="Years",sub=subtitle[11],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[11],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[12]==1) { print(paste("calculating",indices[12]),quote=FALSE) ; index.store <- climdex.txn(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[12],freq=frequency) ; plot.call(index.store,index.name=indices[12],index.units=units[12],x.label="Years",sub=subtitle[12],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[12],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[13]==1) { print(paste("calculating",indices[13]),quote=FALSE) ; index.store <- climdex.dtr(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[13],freq=frequency) ; plot.call(index.store,index.name=indices[13],index.units=units[13],x.label="Years",sub=subtitle[13],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[13],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[14]==1) { print(paste("calculating",indices[14]),quote=FALSE) ; index.store <- climdex.wsdi(cio) ; write.index.csv(index.store,index.name=indices[14]) ; plot.call(index.store,index.name=indices[14],index.units=units[14],x.label="Years",sub=subtitle[14])
                cat(file=trend_file,paste(latitude,longitude,indices[14],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[15]==1) { print(paste("calculating",indices[15]),quote=FALSE) ; index.store <- climdex.wsdin(cio,n=wsdi_ud) ; write.index.csv(index.store,index.name=indices[15]) ; plot.call(index.store,index.name=indices[15],index.units=units[15],x.label="Years",sub=subtitle[15])
                cat(file=trend_file,paste(latitude,longitude,indices[15],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[16]==1) { print(paste("calculating",indices[16]),quote=FALSE) ; index.store <- climdex.csdi(cio) ; write.index.csv(index.store,index.name=indices[16]) ; plot.call(index.store,index.name=indices[16],index.units=units[16],x.label="Years",sub=subtitle[16])
                cat(file=trend_file,paste(latitude,longitude,indices[16],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[17]==1) { print(paste("calculating",indices[17]),quote=FALSE) ; index.store <- climdex.csdin(cio,n=csdi_ud) ; write.index.csv(index.store,index.name=indices[17]) ; plot.call(index.store,index.name=indices[17],index.units=units[17],x.label="Years",sub=subtitle[17])
                cat(file=trend_file,paste(latitude,longitude,indices[17],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[18]==1) { print(paste("calculating",indices[18]),quote=FALSE) ; index.store <- climdex.tx50p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[18],freq=frequency) ; plot.call(index.store,index.name=indices[18],index.units=units[18],x.label="Years",sub=subtitle[18],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[18],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[19]==1) { print(paste("calculating",indices[19]),quote=FALSE) ; index.store <- climdex.tx95t(cio) ; write.index.csv(index.store,index.name=indices[19]) ; plot.call(index.store,index.name=indices[19],index.units=units[19],x.label="Years",sub=subtitle[19]) 
                cat(file=trend_file,paste(latitude,longitude,indices[19],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[20]==1) { print(paste("calculating",indices[20]),quote=FALSE) ; index.store <- climdex.tx10p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[20],freq=frequency) ; plot.call(index.store,index.name=indices[20],index.units=units[20],x.label="Years",sub=subtitle[20],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[20],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[21]==1) { print(paste("calculating",indices[21]),quote=FALSE) ; index.store <- climdex.tx90p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[21],freq=frequency) ; plot.call(index.store,index.name=indices[21],index.units=units[21],x.label="Years",sub=subtitle[21],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[21],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[22]==1) { print(paste("calculating",indices[22]),quote=FALSE) ; index.store <- climdex.tn10p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[22],freq=frequency) ; plot.call(index.store,index.name=indices[22],index.units=units[22],x.label="Years",sub=subtitle[22],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[22],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[23]==1) { print(paste("calculating",indices[23]),quote=FALSE) ; index.store <- climdex.tn90p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[23],freq=frequency) ; plot.call(index.store,index.name=indices[23],index.units=units[23],x.label="Years",sub=subtitle[23],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[23],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[24]==1) { print(paste("calculating",indices[24]),quote=FALSE) ; index.store <- climdex.tm5a(cio) ; write.index.csv(index.store,index.name=indices[24]) ; plot.call(index.store,index.name=indices[24],index.units=units[24],x.label="Years",sub=subtitle[24])
                cat(file=trend_file,paste(latitude,longitude,indices[24],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[25]==1) { print(paste("calculating",indices[25]),quote=FALSE) ; index.store <- climdex.tm5b(cio) ; write.index.csv(index.store,index.name=indices[25]) ; plot.call(index.store,index.name=indices[25],index.units=units[25],x.label="Years",sub=subtitle[25])
                cat(file=trend_file,paste(latitude,longitude,indices[25],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[26]==1) { print(paste("calculating",indices[26]),quote=FALSE) ; index.store <- climdex.tm10a(cio) ; write.index.csv(index.store,index.name=indices[26]) ; plot.call(index.store,index.name=indices[26],index.units=units[26],x.label="Years",sub=subtitle[26])
                cat(file=trend_file,paste(latitude,longitude,indices[26],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[27]==1) { print(paste("calculating",indices[27]),quote=FALSE) ; index.store <- climdex.tm10b(cio) ; write.index.csv(index.store,index.name=indices[27]) ; plot.call(index.store,index.name=indices[27],index.units=units[27],x.label="Years",sub=subtitle[27])
                cat(file=trend_file,paste(latitude,longitude,indices[27],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[28]==1) { print(paste("calculating",indices[28]),quote=FALSE) ; index.store <- climdex.su30(cio) ; write.index.csv(index.store,index.name=indices[28]) ; plot.call(index.store,index.name=indices[28],index.units=units[28],x.label="Years",sub=subtitle[28])
                cat(file=trend_file,paste(latitude,longitude,indices[28],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[29]==1) { print(paste("calculating",indices[29]),quote=FALSE) ; index.store <- climdex.su35(cio) ; write.index.csv(index.store,index.name=indices[29]) ; plot.call(index.store,index.name=indices[29],index.units=units[29],x.label="Years",sub=subtitle[29])
                cat(file=trend_file,paste(latitude,longitude,indices[29],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		if (cbv[30]==1) { print(paste("calculating",indices[30]),quote=FALSE) ; index.store <- climdex.hddheat(cio,Tb=Tb_HDD) ; write.index.csv(index.store,index.name=indices[30]) ; plot.call(index.store,index.name=indices[30],index.units=units[30],x.label="Years",sub=subtitle[30])
                cat(file=trend_file,paste(latitude,longitude,indices[30],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
		setTkProgressBar(pb,60,title="60%")
		if (cbv[31]==1) { print(paste("calculating",indices[31]),quote=FALSE) ; index.store <- climdex.cddcold(cio,Tb=Tb_CDD) ; write.index.csv(index.store,index.name=indices[31]) ; plot.call(index.store,index.name=indices[31],index.units=units[31],x.label="Years",sub=subtitle[31])
                cat(file=trend_file,paste(latitude,longitude,indices[31],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[32]==1) { print(paste("calculating",indices[32]),quote=FALSE) ; index.store <- climdex.gddgrow(cio,Tb=Tb_GDD) ; write.index.csv(index.store,index.name=indices[32]) ; plot.call(index.store,index.name=indices[32],index.units=units[32],x.label="Years",sub=subtitle[32])
                cat(file=trend_file,paste(latitude,longitude,indices[32],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[33]==1) { print(paste("calculating",indices[33]),quote=FALSE) ; index.store <- climdex.cdd(cio) ; write.index.csv(index.store,index.name=indices[33]) ; plot.call(index.store,index.name=indices[33],index.units=units[33],x.label="Years",sub=subtitle[33])
                cat(file=trend_file,paste(latitude,longitude,indices[33],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[34]==1) { print(paste("calculating",indices[34]),quote=FALSE) ; index.store <- climdex.cwd(cio) ; write.index.csv(index.store,index.name=indices[34]) ; plot.call(index.store,index.name=indices[34],index.units=units[34],x.label="Years",sub=subtitle[34])
                cat(file=trend_file,paste(latitude,longitude,indices[34],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[35]==1) { print(paste("calculating",indices[35]),quote=FALSE) ; index.store <- climdex.r10mm(cio) ; write.index.csv(index.store,index.name=indices[35]) ; plot.call(index.store,index.name=indices[35],index.units=units[35],x.label="Years",sub=subtitle[35])
                cat(file=trend_file,paste(latitude,longitude,indices[35],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[36]==1) { print(paste("calculating",indices[36]),quote=FALSE) ; index.store <- climdex.r20mm(cio) ; write.index.csv(index.store,index.name=indices[36]) ; plot.call(index.store,index.name=indices[36],index.units=units[36],x.label="Years",sub=subtitle[36])
                cat(file=trend_file,paste(latitude,longitude,indices[36],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[37]==1) { print(paste("calculating",indices[37]),quote=FALSE) ; index.store <- climdex.rx1day(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[37],freq=frequency) ; plot.call(index.store,index.name=indices[37],index.units=units[37],x.label="Years",sub=subtitle[37],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[37],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[38]==1) { print(paste("calculating",indices[38]),quote=FALSE) ; index.store <- climdex.rx5day(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[38],freq=frequency) ; plot.call(index.store,index.name=indices[38],index.units=units[38],x.label="Years",sub=subtitle[38],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[38],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[39]==1) { print(paste("calculating",indices[39]),quote=FALSE) ; index.store <- climdex.prcptot(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[39],freq=frequency) ; plot.call(index.store,index.name=indices[39],index.units=units[39],x.label="Years",sub=subtitle[39],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[39],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[40]==1) { print(paste("calculating",indices[40]),quote=FALSE) ; index.store <- climdex.sdii(cio) ; write.index.csv(index.store,index.name=indices[40]) ; plot.call(index.store,index.name=indices[40],index.units=units[40],x.label="Years",sub=subtitle[40])
                cat(file=trend_file,paste(latitude,longitude,indices[40],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[41]==1) { print(paste("calculating",indices[41]),quote=FALSE) ; index.store <- climdex.r95p(cio) ; write.index.csv(index.store,index.name=indices[41]) ; plot.call(index.store,index.name=indices[41],index.units=units[41],x.label="Years",sub=subtitle[41])
                cat(file=trend_file,paste(latitude,longitude,indices[41],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[42]==1) { print(paste("calculating",indices[42]),quote=FALSE) ; index.store <- climdex.r99p(cio) ; write.index.csv(index.store,index.name=indices[42]) ; plot.call(index.store,index.name=indices[42],index.units=units[42],x.label="Years",sub=subtitle[42])
                cat(file=trend_file,paste(latitude,longitude,indices[42],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[43]==1) { print(paste("calculating",indices[43]),quote=FALSE) ; index.store <- climdex.r95ptot(cio) ; write.index.csv(index.store,index.name=indices[43]) ; plot.call(index.store,index.name=indices[43],index.units=units[43],x.label="Years",sub=subtitle[43])
                cat(file=trend_file,paste(latitude,longitude,indices[43],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[44]==1) { print(paste("calculating",indices[44]),quote=FALSE) ; index.store <- climdex.r99ptot(cio) ; write.index.csv(index.store,index.name=indices[44]) ; plot.call(index.store,index.name=indices[44],index.units=units[44],x.label="Years",sub=subtitle[44])
                cat(file=trend_file,paste(latitude,longitude,indices[44],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[45]==1) { print(paste("calculating",indices[45]),quote=FALSE) ; index.store <- climdex.rxnday(cio,n=rx_ud,freq=frequency) ; write.index.csv(index.store,index.name=indices[45],freq=frequency) ; plot.call(index.store,index.name=indices[45],index.units=units[45],x.label="Years",sub=subtitle[45],freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,indices[45],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[46]==1) { print(paste("calculating",indices[46]),quote=FALSE) ; user.ind46 = paste("r",rnnmm_ud,"mm",sep="") ; 
                index.store <- climdex.rnnmm(cio,rnnmm_ud) ; write.index.csv(index.store,index.name=user.ind46) ; plot.call(index.store,index.name=user.ind46,index.units=units[46],x.label="Years",sub=paste("Annual number of days when precipitation >= ",rnnmm_ud,sep=""))
                cat(file=trend_file,paste(latitude,longitude,indices[46],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[47]==1) { print(paste("calculating",indices[47]),quote=FALSE) ; index.store <- climdex.ntxntn(cio,n=txtn_ud) ; write.index.csv(index.store,index.name=indices[47]) ; plot.call(index.store,index.name=indices[47],index.units=units[47],x.label="Years",sub=subtitle[47])
                cat(file=trend_file,paste(latitude,longitude,indices[47],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[48]==1) { print(paste("calculating",indices[48]),quote=FALSE) ; index.store <- climdex.ntxbntnb(cio,n=txtn_ud) ; write.index.csv(index.store,index.name=indices[48]) ; plot.call(index.store,index.name=indices[48],index.units=units[48],x.label="Years",sub=subtitle[48])
                cat(file=trend_file,paste(latitude,longitude,indices[48],years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
                if (cbv[49]==1) { print(paste("calculating",indices[49]),quote=FALSE) ; if(all(is.na(cio@data$tmin)) | all(is.na(cio@data$tmax))) { warning("NOT PLOTTING HEATWAVE INDICES: Requires tmin and tmax.") } else {
			setTkProgressBar(pb,90,title="90%")
			# If heatwave previous percentiles have been read in by user then use these in heatwave calculations, otherwise let climdex.hw calculate percentiles using currently loaded data.
                        if(!is.null(temp.quantiles)) { #{ tx90p <- hwlist$HW.TX90 ; tn90p <- hwlist$HW.TN90 ; tavg90p <- hwlist$HW.TAVG90 } else {
				tx90p <<- tn90p <<- tavg90p <<- tavg05p <<- tavg95p <<- NULL }
				index.store <- climdex.hw(cio,lat=latitude,base.range=c(base.year.start,base.year.end),tavg90p=tavg90p,tn90p=tn90p,tx90p=tx90p)

				write.hw.csv(index.store,index.name=indices[49])
				plot.hw(index.store,index.name=indices[49],index.units=units[49],x.label="Years")
			} }
                if (cbv[50]==1) { print(paste("calculating",indices[50]),quote=FALSE) ; if(all(is.na(cio@data$tmin)) | all(is.na(cio@data$tmax)) | all(is.na(cio@data$prec))) { warning("NOT PLOTTING SPEI: climdex.spei REQUIRES TMIN, TMAX AND PRECIP DATA.") } else {
			setTkProgressBar(pb,95,title="95%")
			# If SPEI/SPI thresholds have been read in by user then use these in SPEI/SPI calculations.
			if(exists("speiprec")) { tnraw <- speitmin ; txraw <- speitmax ; praw <- speiprec ; btime <- speidates } else {
				tnraw <- txraw <- praw <- btime <- NULL }

		        if(!is.null(btime)) computefuture = TRUE else computefuture = FALSE
			ts.start <- c(as.numeric(date.years[1]),1)
			ts.end <- c(as.numeric(date.years[length(date.years)]),12)

			# Code related to creating spi* variables aren't needed when relying on climpact2.r. However, due to ostensible issues with CRAN SPEI, this code needs to be rolled into this file in order to call our own SPEI code.
		        if(computefuture){
			                # construct dates
                                beg = as.Date(btime[1])
                                end = dates[length(dates)]      #as.Date(paste(base.year.end,"12","31",sep="-"))
                                dat.seq = seq(beg,end,by = "1 day")
                                spidates = dat.seq
                                spitmin <- spitmax <- spiprec <- spifactor <- vector(mode="numeric",length=length(spidates))
                                spitmin[1:length(tnraw)] = tnraw
                                spitmax[1:length(txraw)] = txraw
                                spiprec[1:length(praw)] = praw

		                spitmin[(length(spitmin)-length(cio@data$tmin)+1):length(spitmin)] = cio@data$tmin
		                spitmax[(length(spitmax)-length(cio@data$tmax)+1):length(spitmax)] = cio@data$tmax
		                spiprec[(length(spiprec)-length(cio@data$prec)+1):length(spiprec)] = cio@data$prec
		                spifactor = factor(format(spidates,format="%Y-%m"))
				ts.start <- c(as.numeric(format(beg,format="%Y")),1)
		        } else {
		                spitmin = cio@data$tmin
		                spitmax = cio@data$tmax
		                spiprec = cio@data$prec
		                spifactor = cio@date.factors$monthly
		        }
		        
######################################
# Calculate SPEI via old climpact code

		        # get monthly means of tmin and tmax. And monthly total precip.
		        tmax_monthly <- as.numeric(tapply.fast(spitmax,spifactor,mean,na.rm=TRUE))
		        tmin_monthly <- as.numeric(tapply.fast(spitmin,spifactor,mean,na.rm=TRUE))
		        prec_sum <- as.numeric(tapply.fast(spiprec,spifactor,function(x) { if(all(is.na(x))) { return(NA) } else { return(sum(x,na.rm=TRUE)) } } )) # Needed this function since summing a series of NA with na.rm = TRUE results in zero instead of NA.
		        tmax_monthly[tmax_monthly=="NaN"] <- NA
		        tmin_monthly[tmin_monthly=="NaN"] <- NA

			# Caclulate evapotranspiration estimate and create time-series object.
			pet = as.numeric(hargreaves(tmin_monthly,tmax_monthly,lat=latitude,Pre=prec_sum,na.rm=TRUE))
			dat = ts(prec_sum-pet,freq=12,start=ts.start,end=ts.end)
			index.store <- array(c(cspei(dat,na.rm=T,scale=c(3),ref.start=c(base.year.start,1),ref.end=c(base.year.end,12),basetmin=tnraw,basetmax=txraw,baseprec=praw,basetime=btime)$fitted,
						cspei(dat,na.rm=T,scale=c(6),ref.start=c(base.year.start,1),ref.end=c(base.year.end,12))$fitted,
						cspei(dat,na.rm=T,scale=c(12),ref.start=c(base.year.start,1),ref.end=c(base.year.end,12))$fitted,
						cspei(dat,na.rm=T,scale=c(custom_SPEI),ref.start=c(base.year.start,1),ref.end=c(base.year.end,12))$fitted),
						c(length((cspei(dat,na.rm=T,scale=c(3))$fitted)),4))
                        index.store <- aperm(index.store,c(2,1))

# End calculating SPEI via old climpact code
######################################

######################################
# Calculate SPEI via CRAN SPEI package housed in climpact2.r
#			index.store <- climdex.spei(cio,ref.start=c(base.year.start,1),ref.end=c(base.year.end,12),lat=latitude,basetmin=tnraw,basetmax=txraw,baseprec=praw,basetime=btime)

# Temporary SPEI to mask out values that should be NA
#			spiprec = cio@data$prec
#        	        spitmin = cio@data$tmin
#	                spitmax = cio@data$tmax
#                        prec_sum <- as.numeric(tapply.fast(spiprec,cio@date.factors$monthly,function(x) { if(all(is.na(x))) { return(NA) } else { return(sum(x,na.rm=TRUE)) } } ))
#        		tmax_monthly <- as.numeric(tapply.fast(spitmax,cio@date.factors$monthly,mean,na.rm=TRUE))
#		        tmin_monthly <- as.numeric(tapply.fast(spitmin,cio@date.factors$monthly,mean,na.rm=TRUE))
#			pet <- hargreaves(tmin_monthly,tmax_monthly,lat=latitude,Pre=prec_sum,na.rm=TRUE)
#			tmpspei = spei(ts(prec_sum-pet,freq=12,start=ts.start,end=ts.end),scale=1,ref.start=c(base.year.start,1),ref.end=c(base.year.end,12),na.rm=TRUE)$fitted
#			index.store[,which(is.na(tmpspei))] = NA

# End calculating SPEI via CRAN SPEI package housed in climpact2.r
######################################

			index.store <- ifelse(index.store=="Inf" | index.store=="-Inf" | index.store=="NaN",NA,index.store)

		# - Strip back off all data not part of the original time series.
		# - Another kludge here relates to an ostensible bug in the SPEI function. When SPEI is fed a series of NA values followed by valid data, it returns values of SPEI/SPI for those NA values, when it shouldn't.
		#    The author has been alerted to this problem. But this means that when a synthetic time series has been made for scenarios using reference data from a different dataset, the initial SPEI/SPI values need
		#    to be manually removed. The first 2, 5 and 11 values for each final time series needs NA'ing, corresponding to 3, 6 and 12 month calculation periods.
		        if(computefuture) {
		                index.store <- index.store[,(length(index.store[1,])-length(unique(cio@date.factors$monthly))+1):length(index.store[1,])]
						# remove spurious values that shouldn't exist (but exist anyway due to the synthetic time series we've fed the spei/spi function).
						index.store[1,1:2] <- NA
                        index.store[2,1:5] <- NA
                        index.store[3,1:11] <- NA
                        index.store[4,1:(custom_SPEI-1)] <- NA
                        spifactor <- spifactor[(length(spifactor)-length((cio@date.factors$monthly))+1):length(spifactor)]
		        }
			write.precindex.csv(index.store,index.name=indices[50],spifactor)
			plot.precindex(index.store,index.name=indices[50],index.units=units[50],x.label="Years",spifactor,sub=subtitle[50],times=c(3,6,12,custom_SPEI)) } }

                if (cbv[51]==1) { print(paste("calculating",indices[51]),quote=FALSE) ; if(all(is.na(cio@data$prec))) warning("NOT PLOTTING SPI: climdex.spi REQUIRES PRECIP DATA.") else {
			setTkProgressBar(pb,99,title="99%")
                        if(exists("speiprec")) { tnraw <- speitmin ; txraw <- speitmax ; praw <- speiprec ; btime <- speidates } else {
                                tnraw <- txraw <- praw <- btime <- NULL }

                        if(!is.null(btime)) computefuture = TRUE else computefuture = FALSE
                        ts.start <- c(as.numeric(date.years[1]),1)
                        ts.end <- c(as.numeric(date.years[length(date.years)]),12)

                        # Code related to creating spi* variables aren't needed when relying on climpact2.r. However, due to ostensible issues with CRAN SPEI, this code needs to be rolled into this file in order to call our own SPEI code.
                        if(computefuture){
                                # construct dates
                                beg = as.Date(btime[1])
                                end = dates[length(dates)]
                                dat.seq = seq(beg,end,by = "1 day")
                                spidates = dat.seq

                                spiprec <- spifactor <- array(NA,length(spidates))
                                spiprec[1:length(praw)] = praw

                                spiprec[(length(spiprec)-length(cio@data$prec)+1):length(spiprec)] = cio@data$prec
                                spifactor = factor(format(spidates,format="%Y-%m"))

                                ts.start <- c(as.numeric(format(beg,format="%Y")),1)
                        } else {
                                spiprec = cio@data$prec
                                spifactor = cio@date.factors$monthly
                        }

######################################
# Calculate SPI via old climpact code

			# get monthly total precip.
			prec_sum <- as.numeric(tapply.fast(spiprec,spifactor,function(x) { if(all(is.na(x))) { return(NA) } else { return(sum(x,na.rm=TRUE)) } } )) # Needed this function since summing a series of NA with na.rm = TRUE results in zero instead of NA.

			# Create time-series object.
			dat <- ts(prec_sum,freq=12,start=ts.start,end=ts.end)
                        index.store <- array(c(cspi(dat,na.rm=T,scale=3,ref.start=c(base.year.start,1),ref.end=c(base.year.end,12))$fitted,
						cspi(dat,na.rm=T,scale=6,ref.start=c(base.year.start,1),ref.end=c(base.year.end,12))$fitted,
						cspi(dat,na.rm=T,scale=12,ref.start=c(base.year.start,1),ref.end=c(base.year.end,12))$fitted,
						cspi(dat,na.rm=T,scale=custom_SPEI,ref.start=c(base.year.start,1),ref.end=c(base.year.end,12))$fitted),
						c(length((cspi(prec_sum,na.rm=T,scale=c(3))$fitted)),4))
                        index.store <- aperm(index.store,c(2,1))

# End calculating SPI via old climpact code
######################################

                        index.store <- ifelse(index.store=="Inf" | index.store=="-Inf" | index.store=="NaN",NA,index.store)

		# - Strip back off all data not part of the original time series.
		# - Another kludge here relates to an ostensible bug in the SPEI function. When SPEI is fed a series of NA values followed by valid data, it returns values of SPEI/SPI for those NA values, when it shouldn't.
		#    The author has been alerted to this problem. But this means that when a synthetic time series has been made for scenarios using reference data from a different dataset, the initial SPEI/SPI values need
		#    to be manually removed. The first 2, 5 and 11 values for each final time series needs NA'ing, corresponding to 3, 6 and 12 months calculation periods.
                        if(computefuture) {
                                index.store <- index.store[,(length(index.store[1,])-length(unique(cio@date.factors$monthly))+1):length(index.store[1,])]
                                # remove spurious values that shouldn't exist (but exist anyway due to the synthetic time series we've fed the spei/spi function).
                                index.store[1,1:2] <- NA
                                index.store[2,1:5] <- NA
                                index.store[3,1:11] <- NA
								index.store[4,1:(custom_SPEI-1)] <- NA
                                spifactor <- spifactor[(length(spifactor)-length((cio@date.factors$monthly))+1):length(spifactor)]
                        }
			write.precindex.csv(index.store,index.name=indices[51],spifactor)
			plot.precindex(index.store,index.name=indices[51],index.units=units[51],x.label="Years",spifactor,sub=subtitle[51],times=c(3,6,12,custom_SPEI)) } }
			
			if(cbv[52]==1) { user.ind52 = paste("su",uuu,sep="") ; print(paste("calculating",user.ind52),quote=FALSE) ; index.store <- (number.days.op.threshold(cio@data$tmax,cio@date.factors$annual,uuu,">")* cio@namasks$annual$tmax) ; write.index.csv(index.store,index.name=user.ind52) ; 
			plot.call(index.store,index.name=user.ind52,index.units="days",x.label="Years",sub=paste("Number of days where TX is greater than ",uuu,"°C",sep=""))
                cat(file=trend_file,paste(latitude,longitude,user.ind52,years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
			if(cbv[53]==1) { user.ind53 = paste("id",uul,sep="") ; print(paste("calculating",user.ind53),quote=FALSE) ; index.store <- (number.days.op.threshold(cio@data$tmax,cio@date.factors$annual,uul,"<")* cio@namasks$annual$tmax) ; write.index.csv(index.store,index.name=user.ind53) ; 
			plot.call(index.store,index.name=user.ind53,index.units="days",x.label="Years",sub=paste("Number of days where TX is less than ",uul,"°C",sep=""))
                cat(file=trend_file,paste(latitude,longitude,user.ind53,years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
			if(cbv[54]==1) { user.ind54 = paste("tr",ulu,sep="") ; print(paste("calculating",user.ind54),quote=FALSE) ; index.store <- (number.days.op.threshold(cio@data$tmin,cio@date.factors$annual,ulu,">")* cio@namasks$annual$tmin) ; write.index.csv(index.store,index.name=user.ind54) ; 
			plot.call(index.store,index.name=user.ind54,index.units="days",x.label="Years",sub=paste("Number of days where TN is greater than ",ulu,"°C",sep=""))
                cat(file=trend_file,paste(latitude,longitude,user.ind54,years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
			if(cbv[55]==1) { user.ind55 = paste("fd",ull,sep="") ; print(paste("calculating",user.ind55),quote=FALSE) ; index.store <- (number.days.op.threshold(cio@data$tmin,cio@date.factors$annual,ull,"<")* cio@namasks$annual$tmin) ; write.index.csv(index.store,index.name=user.ind55) ; 
			plot.call(index.store,index.name=user.ind55,index.units="days",x.label="Years",sub=paste("Number of days where TN is less than ",ull,"°C",sep=""))
                cat(file=trend_file,paste(latitude,longitude,user.ind55,years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
 			if(cbv[56]==1) { print("calculating mean TM",quote=FALSE) ; index.store <- climdex.mean.temp(cio,freq=frequency) ; write.index.csv(index.store,index.name="TMm",freq=frequency) ; 
			plot.call(index.store,index.name="TMm",index.units="°C",x.label="Years",sub="Mean daily mean temperature",freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,"TMm",years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
 			if(cbv[57]==1) { print("calculating mean TN",quote=FALSE) ; index.store <- climdex.mean.min.temp(cio,freq=frequency) ; write.index.csv(index.store,index.name="TNm",freq=frequency) ; 
			plot.call(index.store,index.name="TNm",index.units="°C",x.label="Years",sub="Mean daily minimum temperature",freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,"TNm",years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) }
 			if(cbv[58]==1) { print("calculating mean TX",quote=FALSE) ; index.store <- climdex.mean.max.temp(cio,freq=frequency) ; write.index.csv(index.store,index.name="TXm",freq=frequency) ; 
			plot.call(index.store,index.name="TXm",index.units="°C",x.label="Years",sub="Mean daily maximum temperature",freq=frequency)
                cat(file=trend_file,paste(latitude,longitude,"TXm",years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) } 
 			if(cbv[59]==1) { print("calculating custom index",quote=FALSE) ; 
				if(length(op.choice)==0 || length(var.choice)==0) { print("no custom index to calculate",quote=FALSE) } else {

					freq=frequency
					if(var.choice=="DTR") { var.choice2=cio@data$dtr ; mask.choice = cio@namasks[[match.arg(freq,choices=c("annual","monthly"))]]$tmin * cio@namasks[[match.arg(freq,choices=c("annual","monthly"))]]$tmax } 
					else if (var.choice=="TX") { var.choice2=cio@data$tmax ; mask.choice = cio@namasks[[match.arg(freq,choices=c("annual","monthly"))]]$tmax } 
					else if (var.choice=="TN") { var.choice2=cio@data$tmin ; mask.choice = cio@namasks[[match.arg(freq,choices=c("annual","monthly"))]]$tmin }
					else if (var.choice=="TM") { var.choice2=cio@data$tmean ; mask.choice = cio@namasks[[match.arg(freq,choices=c("annual","monthly"))]]$tmin }
					else if (var.choice=="PR") { var.choice2=cio@data$prec ; mask.choice = cio@namasks[[match.arg(freq,choices=c("annual","monthly"))]]$prec }

					if(op.choice==">") { op.choice2="gt" }
					else if(op.choice==">=") { op.choice2="ge" }
					else if(op.choice=="<") { op.choice2="lt" }
					else if(op.choice=="<=") { op.choice2="le" }

					if(is.null(var.choice2)) return()
					index.store <- number.days.op.threshold(var.choice2, cio@date.factors[[match.arg(freq,choices=c("annual","monthly"))]], constant.choice, op.choice) * mask.choice
					write.index.csv(index.store,index.name=paste(var.choice,op.choice2,constant.choice,sep=""),freq=frequency) ; 
					plot.call(index.store,index.name=paste(var.choice,op.choice2,constant.choice,sep=""),index.units="days",x.label="Years",sub=paste("Number of days where ",var.choice," ",op.choice," ",constant.choice,sep=""),freq=frequency)
					cat(file=trend_file,paste(latitude,longitude,paste(var.choice,op.choice2,constant.choice,sep=""),years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T) } }

		dev.off(pdf.dev)
		graphics.off()  # close the pdf file, so you can open to view it now.

		# new window - calculation done
		nstation<-tktoplevel(bg='white')
		tkwm.geometry(nstation, "+300+200") # position in upper left corner of screen
		tkwm.title(nstation,"ClimPACT2 - Done")
                tkfocus(nstation)
                close(pb)
		
		okk<-function(){tkdestroy(nstation);tkfocus(start1)}  # all are done, return to main window.
		textlabel0<-tklabel(nstation,text="     ",bg='white')  # message showing all are done, showing directory.
		textlabel1<-tklabel(nstation,text="Indices calculation completed",font=fontHeading1,bg='white')
		textlabel2<-tklabel(nstation,text=paste("   Plots are in ",outjpgdir,"   ",sep=" "),font=fontHeading2,bg='white')
                textlabel3<-tklabel(nstation,text=paste("   Data are in ",outinddir,"   ",sep=" "),font=fontHeading2,bg='white')
	
		## Will add one more part here, to show which indices were not calculated...
		okk.but<-tkbutton(nstation,text="   OK   ",command=okk,width=20,bg='white')
		tkpack(textlabel1)
		tkpack(textlabel2)
		tkpack(textlabel3)
		tkpack(textlabel0)
		tkpack(okk.but)
	}

        selectAll()
        index.calc3()
}
# end of index.calc2  

# write.index.csv
# takes a time series of a given index and writes to file
write.index.csv <- function(index=NULL,index.name=NULL,freq="annual") {
	if(is.null(index.name) | is.null(index)) stop("Need index data and index.name in order to write CSV file.")

	if(freq=="monthly") { freq="MON" }
	else if(freq=="annual") { freq="ANN" }
	else {}
	if(index.name=="tx95t") { freq="DAY" }
	
	if(index.name=="wsdin") { tmp.name=paste("wsdi",wsdi_ud,sep="") } 
	else if (index.name=="csdin") { tmp.name=paste("csdi",csdi_ud,sep="") }
	else if (index.name=="rxnday") { tmp.name=paste("rx",rx_ud,"day",sep="") }
	else if (index.name=="rnnmm") { tmp.name=paste("r",rnnmm_ud,"mm",sep="") }
	else if (index.name=="ntxntn") { tmp.name=paste(txtn_ud,"tx",txtn_ud,"tn",sep="") }
	else if (index.name=="ntxbntnb") { tmp.name=paste(txtn_ud,"txb",txtn_ud,"tnb",sep="") }
	else { tmp.name = index.name }
	nam1 <- paste(outinddir, paste(ofilename, "_", tmp.name,"_",freq, ".csv", sep = ""), sep = "/")
	write_header(nam1)
	index=c(tmp.name,index)
	names(index)[1]="time"
	
	# calculate normalised values
	norm = array(NA,(length(index)-1))
	avg = mean(as.numeric(index[2:length(index)]),na.rm=TRUE)
	stddev = sd(as.numeric(index[2:length(index)]),na.rm=TRUE)
	for (i in 2:length(index)) { norm[i-1] = (as.numeric(index[i])-avg)/stddev }
	norm = c("normalised (all years)",norm)
	new.index = cbind(index,norm)

	write.table(new.index,file = nam1, append = TRUE, sep = ", ", na = "-99.9", col.names = FALSE,quote=FALSE)
}

# write.hw.csv
# takes a time series of hw and writes to file
write.hw.csv <- function(index=NULL,index.name=NULL) {
        if(is.null(index)) stop("Need heatwave data to write CSV file.")

		# print each definition in a separate .csv. Thus each .csv will have columns of time, HWA, HWM, HWF, HWD, HWN.
		aspect.names <- list("time","HWM","HWA","HWN","HWD","HWF")
		
		# write Tx90 heatwave data
        nam1 <- paste(outinddir, paste(ofilename, "_Tx90_heatwave_ANN.csv", sep = ""), sep = "/")
		write_header(nam1)
		write.table(aspect.names, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),aperm(index[1,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write Tn90 heatwave data
        nam1 <- paste(outinddir, paste(ofilename, "_Tn90_heatwave_ANN.csv", sep = ""), sep = "/")
        write_header(nam1)
        write.table(aspect.names, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),aperm(index[2,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write EHF heatwave data
        nam1 <- paste(outinddir, paste(ofilename, "_EHF_heatwave_ANN.csv", sep = ""), sep = "/")
        write_header(nam1)
        write.table(aspect.names, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),aperm(index[3,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write ECF coldwave data
        nam1 <- paste(outinddir, paste(ofilename, "_ECF_heatwave_ANN.csv", sep = ""), sep = "/")
        write_header(nam1)
        write.table(aspect.names, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),aperm(index[4,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
}

# plot.hw
plot.hw <- function(index=NULL,index.name=NULL,index.units=NULL,x.label=NULL) {
        if(is.null(index)) stop("Need heatwave data to plot.")

	definitions <- c("Tx90","Tn90","EHF","ECF")
	aspects <- c("HWM","HWA","HWN","HWD","HWF")
	units <- c("°C","°C","heat waves","days","days")
	Encoding(units) <- "UTF-8"

	for (def in 1:length(definitions)) {
		for (asp in 1:length(aspects)) {
			if(all(is.na(index[def,asp,]))) { warning(paste("All NA values detected, not plotting ",definitions[def],", ",aspects[asp],".",sep="")) ; next }
			plot.title <- paste("Station: ",title.station,sep="")
			namp <- paste(outjpgdir, paste(ofilename, "_", definitions[def],"_",aspects[asp], "_ANN.jpg", sep = ""), sep = "/")
			jpeg(file = namp, width = 1024, height = 768)
			dev0 = dev.cur()

			if(aspects[asp]=="HWM") { sub=paste("Index: ",definitions[def],"-",aspects[asp],". Heat wave magnitude (mean temperature of all heat wave events)",sep="") } 
			else if(aspects[asp]=="HWA"){ sub=paste("Index: ",definitions[def],"-",aspects[asp],". Heat wave amplitude (peak temperature of the hottest heat wave event)",sep="") }
			else if(aspects[asp]=="HWD"){ sub=paste("Index: ",definitions[def],"-",aspects[asp],". Heat wave duration (length of longest heat wave event)",sep="") }
			else if(aspects[asp]=="HWF"){ sub=paste("Index: ",definitions[def],"-",aspects[asp],". Heat wave frequency (number of days contributing to heat wave event)",sep="") }
			else if(aspects[asp]=="HWN"){ sub=paste("Index: ",definitions[def],"-",aspects[asp],". Heat wave number (number of discreet heat wave events)",sep="") }

			if((definitions[def]=="EHF" || definitions[def]=="ECF") && any(aspects[asp]=="HWM",aspects[asp]=="HWA")) { unit = "°C^2" ; Encoding(unit) <- "UTF-8" } else unit = units[asp]
			plotx((date.years), index[def,asp,], main = gsub('\\*', unit, plot.title),ylab = unit,xlab = x.label,index.name=index.name,sub=sub)

			dev.set(which = pdf.dev)
			plotx((date.years), index[def,asp,], main = gsub('\\*', unit, plot.title),ylab = unit,xlab = x.label,index.name=index.name,sub=sub)
			dev.copy()
			dev.off(dev0)

			fit1<-suppressWarnings(lsfit((date.years),index[def,asp,]))
			out1<<-ls.print(fit1,print.it=F)
			cat(file=trend_file,paste(latitude,longitude,paste(definitions[def],aspects[asp],sep="."),years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T)
		}
	}
}

# write.precindex.csv
write.precindex.csv <- function(index=NULL,index.name=NULL,spifactor=NULL) {
        if(is.null(index)) stop("Need SPEI data to write CSV file.")
		colnames <- list("time",index.name)

        # write 3 month data
        nam1 <- paste(outinddir, paste(ofilename, "_3month_",index.name,"_MON.csv", sep = ""), sep = "/")
        write_header(nam1)
        write.table(colnames, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(unique(as.character(spifactor)),index[1,]), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write 6 month data
        nam1 <- paste(outinddir, paste(ofilename, "_6month_",index.name,"_MON.csv", sep = ""), sep = "/")
        write_header(nam1)
        write.table(colnames, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(unique(as.character(spifactor)),index[2,]), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write 12 month data
        nam1 <- paste(outinddir, paste(ofilename, "_12month_",index.name,"_MON.csv", sep = ""), sep = "/")
        write_header(nam1)
        write.table(colnames, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(unique(as.character(spifactor)),index[3,]), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        
        # write custom-period data
        nam1 <- paste(outinddir, paste(ofilename,"_",custom_SPEI, "month_",index.name,"_MON.csv", sep = ""), sep = "/")
        write_header(nam1)
        write.table(colnames, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(unique(as.character(spifactor)),index[4,]), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
}

# plot.precindex
# not sure how generic this process can be
plot.precindex <- function(index=NULL,index.name=NULL,index.units=NULL,x.label=NULL,spifactor=NULL,sub="",times="") {
        if(is.null(index)) stop("Need precip data to plot.")
		Encoding(sub) <- "UTF-8"

        for (time in 1:4) {
			if(all(is.na(index[time,]))) { warning(paste("All NA values detected, not plotting ",times[time]," month ",index.name,".",sep="")) ; next }

			subtmp=paste("Index: ",index.name," ",times[time]," month. ",sub,sep="")
			namp <- paste(outjpgdir, paste(ofilename, "_",times[time],"month_",index.name,"_MON.jpg", sep = ""), sep = "/")
			jpeg(file = namp, width = 1024, height = 768)

			dev0 = dev.cur()
	        plotx(unique(as.character(spifactor)), index[time,], main = paste(gsub('\\*', index.name, plot.title),sep=""),ylab = index.units,xlab = x.label,index.name=index.name,sub=subtmp)

            dev.set(which = pdf.dev)
            plotx(unique(as.character(spifactor)), index[time,], main = paste(gsub('\\*', index.name, plot.title),sep=""),ylab = index.units,xlab = x.label,index.name=index.name,sub=subtmp)
            dev.copy()
            dev.off(dev0)

            fit1<-suppressWarnings(lsfit(1:length(unique(spifactor)),index[time,]))
            out1<<-ls.print(fit1,print.it=F)
            cat(file=trend_file,paste(latitude,longitude,paste(index.name,times[time],"month",sep="."),years,yeare,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T)
        }
}

# plot.index
plot.call <- function(index=NULL,index.name=NULL,index.units=NULL,x.label=NULL,sub="",freq="annual") {
        if(is.null(index.name) | is.null(index) | is.null(index.units)) stop("Need index data, index.name, index units and an x label in order to plot data.")

		Encoding(sub) <- "UTF-8"
		Encoding(index.units) <- "UTF-8"
#	plot.title <- paste(title.station,index.name,sep=", ")
	if(index.name=="wsdin") { tmp.name=paste("wsdi",wsdi_ud,sep="") ; sub=paste("Index: ",tmp.name,". Annual number of days with at least ",wsdi_ud," consecutive days when TX > 90th percentile",sep="") } 
	else if (index.name=="csdin") { tmp.name=paste("csdi",csdi_ud,sep="") ; sub=paste("Index: ",tmp.name,". Annual number of days with at least ",csdi_ud," consecutive days when TN < 10th percentile",sep="")  }
	else if (index.name=="rxnday") { tmp.name=paste("rx",rx_ud,"day",sep="") ; sub=paste("Index: ",tmp.name,". Maximum ",rx_ud,"-day precipitation total",sep="")  }
	else if (index.name=="rnnmm") { tmp.name=paste("r",rnnmm_ud,"mm",sep="") ; sub=paste("Index: ",tmp.name,". Annual number of days when precipitation >= ",rnnmm_ud,"mm",sep="")  }
	else if (index.name=="ntxntn") { tmp.name=paste(txtn_ud,"tx",txtn_ud,"tn",sep="") ; sub=paste("Index: ",tmp.name,". Annual number of ",txtn_ud," consecutive days where both TX > 95th percentile and TN > 95th percentile",sep="")  }
	else if (index.name=="ntxbntnb") { tmp.name=paste(txtn_ud,"txb",txtn_ud,"tnb",sep="") ; sub=paste("Index: ",tmp.name,". Annual number of ",txtn_ud," consecutive days where both TX < 5th percentile and TN < 5th percentile",sep="")  }
	else if (index.name=="cddcold") { tmp.name=index.name ; sub=paste("Index: ",tmp.name,". Annual sum of TM - ",Tb_CDD,"°C (where ",Tb_CDD,"°C is a user-defined base temperature and should be smaller than TM)",sep="")  }
	else if (index.name=="hddheat") { tmp.name=index.name ; sub=paste("Index: ",tmp.name,". Annual sum of ",Tb_HDD,"°C - TM (where ",Tb_HDD,"°C is a user-defined base temperature and should be larger than TM)",sep="")  }
	else if (index.name=="gddgrow") { tmp.name=index.name ; sub=paste("Index: ",tmp.name,". Annual sum of TM - ",Tb_GDD,"°C (where ",Tb_GDD,"°C is a user-defined base temperature and should be smaller than TM)",sep="")  }
	else { tmp.name = index.name ; sub=paste("Index: ",tmp.name,". ",sub,sep="") }

	if(freq=="monthly") { freq="MON" }
	else if(freq=="annual") { freq="ANN" }
	else {}
	
	if(index.name=="tx95t") { freq="DAY" }
	
	namp <- paste(outjpgdir, paste(ofilename, "_", tmp.name, "_", freq,".jpg", sep = ""), sep = "/")
	jpeg(file = namp, width = 1024, height = 768)

	dev0 = dev.cur()
	if(index.name=="tx95t") { xdata <- 1:length(index) }
	else xdata <- names(index)

	plotx(xdata, index, main = gsub('\\*', tmp.name, plot.title),
	  ylab = index.units,xlab = x.label,index.name=index.name,sub=sub)

	dev.set(which = pdf.dev)
	plotx(xdata, index, main = gsub('\\*', tmp.name, plot.title),
	  ylab = index.units, xlab = x.label,index.name=index.name,sub=sub)
	dev.copy()
	dev.off(dev0)
}

# plotx
# make plots, this is called twice to make jpg and pdf files. 
plotx <- function (x0, y0, main = "", xlab = "", ylab = "", opt = 0,index.name=NULL,sub="")
{
	if(all(is.na(y0))) { print("NO DATA TO PLOT",quote=FALSE) ; return() }
	Encoding(main) <- "UTF-8"
	Encoding(sub) <- "UTF-8"
# take a copy of input, so we will not modify the input by mistake.
# And only take index values from the first non-NA value onwards, to avoid plotting long series of NA values.
	nay <- which(!is.na(y0))
	x <- x0[nay[1]:nay[length(nay)]]
	y <- y0[nay[1]:nay[length(nay)]]

	# james: i'm turning xpd off for barplots, so that i can clip the range w/o the bars
	# running off the page. is this required?
	par(oma = c(2, 1, 1, 1), xpd = FALSE,new=FALSE) #to enable things to be drawn outside the plot region
	#names(y) <- c(strtrim(x,4))

	# calculate range to limit the plots to (otherwise barplots are useless... they're in
	# any non-barplots for consistency). also to allow for overlays like marking na points
	# y.range <- range(y, na.rm = TRUE) #- 0.1 * (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
	# x.range <- min(x, na.rm = TRUE)      # should be no missing data in the x series

	if(barplot_flag)  # if true, we're doing a barplot
	{
		if(index.name=="spei" | index.name=="spi") {
			bp <- barplot(y, main = main, cex.main = 2,ylim = range(y, na.rm = TRUE),xlab = NULL, ylab = ylab,cex.lab = 1.5, cex.axis = 1.5,xpd = FALSE,col=ifelse(y>0,"blue","red"),border=NA,space=c(0,0))
			mtext(sub,cex=1)
            # NA points
            na.x <- bp
            na.y <- rep(NA, length(na.x))
            na.y[is.na(y)] <- par("usr")[3]
            points(na.x, na.y, pch = 17, col = "blue", cex = 1.5)
	                
			subx = as.numeric(substr(x,1,4))
			xind = which(subx%%5==0)
			xtmp.int = (subx[xind])
			axis(1,at=xind,labels=c(xtmp.int))

            box()
			xy <- cbind(bp,y)
		} else {
			plot(1:length(x), unname(y), main = main, cex.main = 2,ylim = range(unname(y), na.rm = TRUE),xaxt="n", xlab = "", ylab = ylab,type = "b", cex.lab = 1.5, cex.axis = 1.5,col="black")

			subx = as.numeric(substr(x,1,4))
			xind = which(subx%%5==0)
			xtmp.int = (subx[xind])
			axis(1,at=xind,labels=c(xtmp.int))

			mtext(sub,cex=1)

	        # NA points
	        na.x <- x
			na.y <- rep(NA, length(na.x))
			na.y[is.na(y)] <- min(y, na.rm = TRUE)

			points(1:length(na.x), na.y, pch = 17, col = "blue", cex = 1.5)
			xy <- cbind(x, y)
		}
	} else            # if false, we're doing a regular (line) plot
	{

	}

	if (opt == 1) return()  # no need to plot trend/fitting curve.
	if (opt == 2)
	{
		abline(h = 0.)
		return()
	}  # for spei & spi only!

	fit <- suppressWarnings(lsfit(1:length(x), y))		# assumes time intervals are always evenly spaced
	out <<- ls.print(fit, print.it = FALSE)
	r2 <- round(100 * as.numeric(out$summary[1, 2]), 1)
	pval <- round(as.numeric(out$summary[1, 6]), 3)
	beta <- round(as.numeric(out$coef.table[[1]][2, 1]), 3)
	betaerr <- round(as.numeric(out$coef.table[[1]][2, 2]), 3)
	if(index.name != "tx95t") {	abline(fit,lwd=2.0) }
	xy <- na.omit(xy)
	tmp_seq = 1:length(x)

	tmp_lowess=lowess(tmp_seq[!is.na(y)], y[!is.na(y)])   #xy[,2])
	lines(tmp_lowess, lwd = 3, lty = 2, col = "red")  # add fitting curve
	if (sum(is.na(y) == FALSE) >= min_trend)
	{
		subtit <- paste("Linear trend slope=", beta, "   Slope error=", betaerr, ",   p-value=", pval)             # least squares regression
	} else
	{
		subtit <- "No linear trend due to insufficient valid data points (10)"
	}
	title(sub = subtit, cex.sub = 1.5)

	old.par = par()	# store par settings to plot legend outside figure margins
	par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
	plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
	legend("bottomleft","locally weighted scatterplot smoothing",col = "red", lty = 2, lwd = 3, bty = "n")
	legend("bottomright",paste("ClimPACT2 v ",version.climpact,sep=""),col = "white", lty = 2, lwd = 0, bty = "n")
	suppressWarnings(par(old.par)) # restore previous par settings. Suppress warnings regarding parameters that cannot be set.
}
# end of plotx

# Computation of the Standardized Precipitation-Evapotranspiration Index (SPEI).
# Generic function
cspei <- function(x, y,...) UseMethod('cspei')

# Fit SPEI.
cspei <- function(data, scale, kernel=list(type='rectangular',shift=0),
	distribution='log-Logistic', fit='ub-pwm', na.rm=FALSE, 
	ref.start=NULL, ref.end=NULL, x=FALSE, ...) {
	scale <- as.numeric(scale)
	na.rm <- as.logical(na.rm)
	x <- as.logical(x)
	#if (!exists("data",inherits=F) | !exists("scale",inherits=F)) {
	#	stop('Both data and scale must be provided')
	#}
	if (sum(is.na(data))>0 & na.rm==FALSE) {
		stop('Error: Data must not contain NAs')
	}
	if (distribution!='log-Logistic' & distribution!='Gamma' & distribution!='PearsonIII') {
		stop('Distrib must be one of "log-Logistic", "Gamma" or "PearsonIII"')
	}
	if (fit!='max-lik' & fit!='ub-pwm' & fit!='pp-pwm') {
		stop('Method must be one of "ub-pwm" (default), "pp-pwm" or "max-lik"')
	}
	if ({!is.null(ref.start) & length(ref.start)!=2} | {!is.null(ref.end) & length(ref.end)!=2}) {
		stop('Start and end of the reference period must be a numeric vector of length two.')
	}
	if (!is.ts(data)) {
		data <- ts(as.matrix(data), frequency = 12)
	} else {
		data <- ts(as.matrix(data), frequency=frequency(data), start=start(data))
	}
	m <- ncol(data)
	fr <- frequency(data)

	if (distribution=='Gamma') {
		coef <- array(NA,c(2,m,fr),list(par=c('alpha','beta'),colnames(data),NULL))
	}
	if (distribution=='log-Logistic') {
		coef <- array(NA,c(3,m,fr),list(par=c('xi','alpha','kappa'),colnames(data),NULL))
	}
	if (distribution=='PearsonIII') {
		coef <- array(NA,c(3,m,fr),list(par=c('mu','sigma','gamma'),colnames(data),NULL))
	}
	
	# Loop through series (columns in data)
	if (!is.null(ref.start) & !is.null(ref.end)) {
		data.fit <- window(data,ref.start,ref.end)	
	} else {
		data.fit <- data
	}
	std <- data*NA
	for (s in 1:m) {
		# Cumulative series (acu)
		acu <- data.fit[,s]*NA
		acu.pred <- std[,s]
		if (scale>1) {
			wgt <- kern(scale,kernel$type,kernel$shift)
			for (t in scale:length(acu)) {
				acu[t] <- sum(data.fit[t:{t-scale+1},s]*wgt)
			} # next t
			for (t in scale:length(acu.pred)) {
				acu.pred[t] <- sum(data[t:{t-scale+1},s]*wgt)
			} # next t
		} else {
			acu <- data.fit[,s]
			acu.pred <- data[,s]
		}

		# Loop through the months
		for (c in (1:fr)) {
			# Filter month m, excluding NAs
			f <- seq(c,length(acu),fr)
			f <- f[!is.na(acu[f])]
			ff <- seq(c,length(acu.pred),fr)
			ff <- ff[!is.na(acu.pred[ff])]
			

			# Monthly series, sorted
			month <- sort(acu[f])

			if (length(month)==0 | is.na(sd(month,na.rm=TRUE))) {
				std[f] <- NA
				next()
			}
		
			if (fit=='pp-pwm') {
				pwm <- pwm.pp(month,-0.35,0)
			} else {
				pwm <- pwm.ub(month)
			}
			lmom <- pwm2lmom(pwm)
			if (!are.lmom.valid(lmom) | is.nan(sum(lmom[[1]]))) {
				next()
			}
	
			if (distribution=='log-Logistic') {
				# Fit a generalized log-Logistic distribution
				llpar <- parglo(lmom)
				if (fit=='max-lik') {
					llpar <- parglo.maxlik(month,llpar$para)
				}
				# Compute standardized values
				std[ff,s] <- qnorm(pglo(acu.pred[ff],llpar))
				coef[,s,c] <- llpar$para
			} else {
				# Probability of monthly precipitation = 0 (pze)
				zeros <- sum(month==0)
				pze <- sum(month==0)/length(month)
# 				month <- sort(month)
				if (distribution =='Gamma') {
					# Fit a Gamma distribution
					gampar <- pargam(lmom.ub(month))
					# Compute standardized values
					std[ff,s] <- qnorm(cdfgam(acu.pred[ff],gampar))
					std[ff,s] <- qnorm(pze + (1-pze)*pnorm(std[ff,s]))
					coef[,s,c] <- gampar$para
				} else if (distribution =='PearsonIII') {
					# Fit a PearsonIII distribution
					p3par <- parpe3(lmom.ub(month))
					# Compute standardized values
					std[ff,s] <- qnorm(cdfpe3(acu.pred[ff],p3par))
					std[ff,s] <- qnorm(pze + (1-pze)*pnorm(std[ff,s]))
					coef[,s,c] <- parpe3$para
				} # end if
			} # end if
		} # next c (month)

		#std[is.nan(std[,s]) | is.nan(std[,s]-std[,s]),s] <- NA
		#std[,s] <- std[,s]-mean(std[,s],na.rm=TRUE)
		#std[,s] <- std[,s]/sd(std[,s],na.rm=TRUE)
	} # next s (series)
	#colnames(std) <- rep('SPEI',m)
	colnames(std) <- colnames(data)

	z <- list(call=match.call(expand.dots=FALSE),
		fitted=std,coefficients=coef,scale=scale,kernel=list(type=kernel$type,
		shift=kernel$shift,values=kern(scale,kernel$type,kernel$shift)),
		distribution=distribution,fit=fit,na.action=na.rm)
	if (x) z$data <- data
	if (!is.null(ref.start)) z$ref.period <- rbind(ref.start,ref.end)

	class(z) <- 'spei'
	return(z)
}

# Generic function
cspi <- function(x, y,...) UseMethod('cspi')

# Fit SPI (previously spi() function). Default method.
cspi <-
function(data, scale, kernel=list(type='rectangular',shift=0),
	distribution='Gamma', fit='ub-pwm', na.rm=TRUE,
	ref.start=NULL, ref.end=NULL, x=FALSE, ...) {
	return(cspei(data, scale, kernel, distribution, fit, na.rm,
	ref.start, ref.end, x))
}

## <<<<<<<<<<<<< end of SPEI package  <<<<<<<<<<<<<<

# brief introduction of the background.
about <- function(){
        ab.done <- function(){
                tkdestroy(ab)
                tkfocus(start1)
        }

        ab <<- tktoplevel(bg = "white")
        tkfocus(ab)
        tkwm.title(ab, "\tClimPACT2 - About\t")
        tt2 <- tkframe(ab,bg="white")
        frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
        tkgrid(tklabel(tt2, text = "About ClimPACT2", bg = "white", font = fontHeading2),columnspan=1)
        tkgrid(frame.space)
        tkgrid(tt2)

        tt2 <- tkframe(ab,bg="white")
        frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
        tkgrid(tklabel(tt2,text=
"Developed by lots of people including Nicholas Herold, Lisa Alexander, Hongang Yang, 
James Goldie, Sarah Perkins. Based on the RClimDEX software developed by the joint WMO 
CCl/CLIVAR/JCOMM Expert Team on Climate Change Detection and Indies (ETCCDI). 
ClimPACT2 uses the R package climdex.pcic, developed by the Pacific Climate Impacts 
Consortium (PCIC), to calculate the majority of indices. For any comments, questions
or suggestions, e-mail nicholas.herold@unsw.edu.au"
,bg='white',font=font_small,width=90),sticky="nsew")
        tkgrid(frame.space)
        tkgrid(tt2)

        tt2 <- tkframe(ab,bg="white")
        frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
        tkgrid(tt2)

        tt2 <- tkframe(ab,bg="white")
        ok1.but<-tkbutton(tt2,text="    Done    ",command=ab.done,bg='white',font=font_small)
        tkgrid(ok1.but)
        tkgrid(tt2)

        tt3 <- tkframe(ab,bg="white")
        frame.space <- tklabel(tt3, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
        tkgrid(tt3)
}

license <- function(){
        lic.done <- function(){
                tkdestroy(lic)
                tkfocus(start1)
        }

	lic <<- tktoplevel(bg = "white")
	tkfocus(lic)
	tkwm.title(lic, "\tClimPACT2 - License\t")
	tt2 <- tkframe(lic,bg="white")
	frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
	tkgrid(frame.space)
	tkgrid(tklabel(tt2, text = "ClimPACT2 license agreement", bg = "white", font = fontHeading2),columnspan=1)
	tkgrid(frame.space)
	tkgrid(tt2)
	
	tt2 <- tkframe(lic,bg="white")
	frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
	tkgrid(frame.space)
	tkgrid(tklabel(tt2,text=
"This software is distributed as open source, anyone is free to modify and redistribute this software under 
the following conditions; that acknowledgement is given the original authors (see \"About\") and the 
World Meteorological Organisation, and that it is acknowledged that any changes to the ETCCDI or ETSCI 
indices as they are calculated here will no longer be considered official. The indices as they are 
calculated in ClimPACT2 are considered the official implementation (and are subject to change only by 
members of the ETSCI or ETCCDI). Lastly, this software is provided as-is and the authors nor their 
host institutions take any responsibility for the accuracy of the data produced by it.",bg='white',font=font_small,width=90),sticky="nsew")
        tkgrid(frame.space)
	tkgrid(tt2)

        tt2 <- tkframe(lic,bg="white")
        frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
        tkgrid(tt2)

		tt2 <- tkframe(lic,bg="white")
		ok1.but<-tkbutton(tt2,text="    Done    ",command=lic.done,bg='white',font=font_small)
		tkgrid(ok1.but)
		tkgrid(tt2)

        tt3 <- tkframe(lic,bg="white")
        frame.space <- tklabel(tt3, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
        tkgrid(tt3)
}

# package.check
# Check for required packages and installs if necessary
package.check <- function() {
	gui.packages <- c("bitops","Rcpp","caTools","PCICt","SPEI","climdex.pcic")
	new.packages <- gui.packages[!(gui.packages %in% installed.packages()[,"Package"])]

	# Install/update packages needed for ClimPACT2 GUI.
	if(length(new.packages)) {
	        print("******************************")
	        print(paste("Installing the following required packages...",new.packages,sep=""))
	        install.packages(new.packages) 
	}

	# Install Linux-specific packages
	if(.Platform$OS.type == "unix") {
                linux.packages <- c("ncdf4","foreach","doParallel","abind")
                new.linux.packages <- linux.packages[!(linux.packages %in% installed.packages()[,"Package"])]

		if(length(new.linux.packages)) {
	                print("******************************")
        	        print(paste("Installing the following required packages...",new.packages,sep=""))
			install.packages(new.linux.packages) } 
	}

	print(paste("R version ",as.character(getRversion())," detected.",sep=""),quote=FALSE)
}

# startss
# Main function. Initiates GUI.
startss <- function(){
	logo_require <- FALSE
	
	# search logo files in current working directory.
	no.logo <- FALSE;
	dir0 <- getwd();
	logo1 <- "WMOLogo.gif";
	logo2 <- "UNSW.gif";
	logo3 <- "coess_unsw.gif";
	
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
	
	tkwm.geometry(start1, "+400+200"); # position in upper left corner of screen
	tkwm.title(start1, paste("ClimPACT2",sep=" "));
	
	# Show logos on upper half of the main window, or "no logos available".  
	if (no.logo == FALSE)
	{  # with logos
	  logo1 <- paste(dir0, "/", logo1, sep = "");
	  logo2 <- paste(dir0, "/", logo2, sep = "");
	  logo3 <- paste(dir0, "/", logo3, sep = "");
	
	  img  <- tkimage.create("photo", file = logo1);
	  img2 <- tkimage.create("photo", file = logo2, width = 0);
	  img3 <- tkimage.create("photo", file = logo3, width = 0);
	
	  right <- tklabel(start1, image = img3);
	  tkgrid(right,columnspan=3)
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
	}
	# Everything above here relates to logos. A tad verbose...

	tkgrid(tklabel(start1, text = "    ", bg = "white",width=40));
	tkgrid(tklabel(start1, text = " ClimPACT2 ", font = fontHeading, width = 15, bg = "white"), columnspan = 3)
	tkgrid(tklabel(start1, text = paste(" v",version.climpact," ",sep=""), font = fontTextLabel, width = 5, bg = "white"), columnspan = 3)
	tkgrid(tklabel(start1, text = "    ", bg = "white"), columnspan = 3);
	tkgrid(tklabel(start1, text = "    ", bg = "white"));
	tkgrid(tklabel(start1, text = "    ", bg = "white"));

	start.but   <<- tkbutton(start1, text = "   LOAD AND  \n  CHECK DATA   ", command = load.data.qc, width = 15, font = fontHeading2, bg = "lightgreen") 
	cal.but     <<- tkbutton(start1, text = "   CALCULATE \n   INDICES  ", command = index.calc1, width = 15, font = fontHeading2, bg = "white")
	
	tkgrid(tklabel(start1, text = " STEP. 1", bg = "white",font=fontHeading1,width=8), columnspan =3)
	tkgrid(start.but, columnspan =3)
	tkgrid(tklabel(start1, text = "    ", bg = "white"));
	tkgrid(tklabel(start1, text = " STEP. 2  ", bg = "white",font=fontHeading1,width=8), columnspan =3)
	tkgrid(cal.but, columnspan =3)


	cancel.but  <- tkbutton(start1, text = " Exit ", command = done, width = 7, font = fontHeading2, bg = "white");
	help.but    <- tkbutton(start1, text = " About ", command = about, width = 7, font = fontHeading2, bg = "white");
	license.but <- tkbutton(start1, text = " License ", command = license, width = 7, font = fontHeading2, bg = "white");

	gap = tklabel(start1,width=5,text="",bg="white")

	tkgrid(tklabel(start1, text = "    ", bg = "white"));
	tkgrid(tklabel(start1, text = "    ", bg = "white"));

	tkgrid(help.but, columnspan = 3)
	tkgrid(license.but, columnspan = 3)
	tkgrid(cancel.but, columnspan = 3)
	tkgrid(tklabel(start1, text = "", bg = "white"))
	tkgrid(tklabel(start1, text = "", bg = "white"))

	tkfocus(start1)
}

#================================================
#  This checks for required packages and runs the program
#================================================
package.check()
source("climpact2.r") # source climpact code and load data from ascii file into climdex object
version.climpact <- software_id
startss()
