# ------------------------------------------------
# ClimPACT2 GUI
# University of New South Wales
# nherold, November 2016.
# This package is available on github https://github.com/ARCCSS-extremes/climpact2.
# ------------------------------------------------
#    
# This file constitutes the graphical user interface for ClimPACT2. It also contains code that plots graphs, writes out data
# and calculates SPEI/SPI indices.
#
# BUGS
#   - Currently SPEI/SPI are calculated via the old ClimPACT code. This is because the CRAN package for SPEI/SPI does not
#     ostenisbly support large runs of NA values. When this occurs real numbers are included in the output where NA values
#     should occur.
#
# TECHNICAL NOTES
#   - order of main functions: startss -> package.check,global.vars -> load.data.qc -> draw.step1.interface -> QC.wrapper
#                                                                   -> draw.step2.interface -> index.calc
#   - warnings are suppressed on lsfit to prevent numerous messages on the removal of missing values that may exist in the
#     users data.
#
# HISTORY
#   This file is a very heavily overhauled version of climpact.r from the original ClimPACT. The most significant change
#   that has taken place in ClimPACT2 is that the calculation of the indices is almost entirely taken care of by the R package
#   climdex.pcic, with several exceptions such as the heatwave indices and SPEI/SPI. 
#
#   Several people contributed significantly to the development of the original ClimPACT software (which was originally derived
#   from RClimdex). For posterity and credit, below is a list of the key names and dates.
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

# Remove previously opened devices and variables from memory.
graphics.off()
#rm(list = ls(all = TRUE))

# ------------------------------------------------ #
# ClimPACT2 GUI functions
# ------------------------------------------------ #

# extraQC code, taken from the "rclimdex_extraqc.r" package, 
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
}

# A function that should be called before any .csv file is written. It appends some basic information that should be stored in each file for 
# the user's record.
write_header <- function(filename,header="")
{
	if(is.null(filename)) { stop("Filename not passed to function 'write_header'") }
	
	header = cbind("Description: ",header)
	tmp = try(write.table(header, sep=",", file = filename, append = FALSE, row.names=FALSE,col.names = FALSE))
	# Check if file is open
	if(class(tmp)=="try-error") { tkmessageBox(message=paste("Error encountered, please check that the file ",filename," is not currently open, then select OK to try again.",sep=""),icon='warning');  write_header(filename) }

	first_lines = cbind(c("Station: ","Latitude: ","Longitude: ","ClimPACT2_version: ","Date_of_calculation: "),c(ofilename,latitude,longitude,version.climpact,toString(Sys.Date())))
	write.table(first_lines, sep=",", file = filename, append = TRUE, row.names=FALSE,col.names = FALSE)

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
		for (a in as.numeric(respc$names)) #1:12)
		{
			ind.of.month = match(respc$names[a],respc$names)
			prov <- subset(datos,datos$month == as.numeric(respc$names[a]) & datos$pc > respc$stats[5, ind.of.month])#a])
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
	hist(ispc %% 1,col='blue',main='NON ZERO PREC ROUNDING',breaks=c(seq(0,1.0,0.0999999)),xlab="")
	hist(my$tx %% 1,col='red',main='TX ROUNDING',breaks=c(seq(0,1.0,0.0999999)),xlab="")
	hist(my$tn %% 1,col='cyan',main='TN ROUNDING',breaks=c(seq(0,1.0,0.0999999)),xlab="")
	
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
	write_header(filena,"Dates where precipitation > 200 mm or abs(temperature) > 50 degrees.")
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
	write.table(cbind(date.tmp,flat$tx,flat$dup+1),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

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
	write.table(cbind(date.tmp,flat$tn,flat$dup+1),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no issues found in variable, print message
	if(length(flat$tn)==0) { write.table("NO REPEATED TN FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(datos)  # we don't want to delete everyting...
}
# End of Prohom and Aguilar code.

# pplotts
# plots QC'ed data (TX, TN, PR) into pdf files.
pplotts <- function(var = "prcp", type = "h", tit = NULL,cio,metadata)
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
		warning(paste("Warnings have been generated because there is no available data for one or more of tmax, tmin or precip. Check the plots in the /qc folder to confirm this."))
	}
	
	par(mfrow = c(4, 1))
	par(mar = c(3.1, 2.1, 2.1, 2.1))

	year.start = as.numeric(format(metadata$dates[1],format="%Y"))
	year.end = as.numeric(format(metadata$dates[length(metadata$dates)],format="%Y"))
	for(i in seq(year.start, year.end, 10))
	{
		at <- rep(1, 10)
		# if (i > yeare)
		for(j in (i + 1):min(i + 9, year.end + 1))
		{
			if(leapyear(j)) at[j - i + 1] <- at[j - i] + 366 else
			  at[j - i + 1] <- at[j - i] + 365
		}
		
		tmp.dates <- format(cio@dates,format="%Y")
		ttmp <- cio@data[[var1]][tmp.dates>=i & tmp.dates <= min(i + 9, year.end)]
		plot(1:length(ttmp), ttmp, type = type, col = "blue",
		  xlab = "", ylab = "", xaxt = "n", xlim = c(1, 3660), ylim = c(ymin, ymax))
		abline(h = 0)
		tt <- seq(1, length(ttmp))
		if(!is.null(ttmp)) tt <- tt[is.na(ttmp) == TRUE] #else print(paste(var,"is null."))
		axis(side = 1, at = at, labels = c(i:(i + 9)))
		for(k in 1:10) abline(v = at[k], col = "yellow")
		lines(tt, rep(ymin, length(tt)), type = "p", col = "red")
		title(paste("Station: ", tit, ", ", i, "~", min(i + 9, year.end), ",  ", var1, sep = ""))
	}
}

# Creates an array of strings, each string containing a folder in the path to the user's file.
# Globally assigns two variables: the array of strings and the final string (i.e. the file name)
# This should be improved in the future (global variables should not be relied on)
# The 'graphics' parameter indicates whether a progress bar is drawn to the screen via tcltk
get.file.path <- function(user.file,graphics=FALSE) {
	if(graphics) { process.pb <<- tkProgressBar("%", "Reading directory path...",0, 100, 10) }

	outdirtmp<-strsplit(user.file,"/")[[1]]
	file.name=outdirtmp[length(outdirtmp)]
	e=strsplit(file.name,"\\.")[[1]]
	if(graphics) { 
		setTkProgressBar(process.pb,40,label="								")
		setTkProgressBar(process.pb,40,label="Splitting directory path...") 
	}
	ofilename=substr(file.name,start=1,stop=nchar(file.name)-nchar(e[length(e)])-1)
	assign('outdirtmp',outdirtmp,envir=.GlobalEnv)
	assign('ofilename',ofilename,envir=.GlobalEnv)
	if(graphics) { close(process.pb) }
}

# This function calls the major routines involved in reading the user's file, creating the climdex object and running quality control
load.data.qc <- function() {
	user.file <- get.user.file()
	if(is.null(user.file)) { tkfocus(start1) ; return() } 
	get.file.path(user.file,graphics=TRUE)
	
		# put convert.user.file into a trycatch?
	user.data <- read.user.file(user.file,graphics=TRUE)
	if(!is.null(user.data)) draw.step1.interface(user.data,user.file)
}

# Preps data and creates the climdex.input object based on the R package climdex.pcic
create.climdex.input <- function(user.data,metadata) {
	date.seq <- data.frame(list(time=seq(metadata$dates[1],metadata$dates[length(metadata$dates)],by="day")))
	data_raw = data.frame(list(time=as.Date(metadata$dates,format="%Y-%m-%d"),prec=user.data[,4],tmax=user.data[,5],tmin=user.data[,6]))
	merge_data = merge(data_raw,date.seq,all=TRUE)
	
	days <- as.Date(as.character(merge_data[,1],format="%Y-%m-%d"))-as.Date("1850-01-01")
	seconds <- as.numeric(days*24*60*60)
	ts.origin = "1850-01-01"                # arbitarily chosen origin to create time-series object with. This needs to be made global 
	pcict.dates <- as.PCICt(seconds,cal="gregorian",origin=as.character(ts.origin))
	
	date.months <- unique(format(as.character((merge_data[,1]),format="%Y-%m")))
	date.years <- unique(format(as.character((merge_data[,1]),format="%Y")))
	assign('date.months',date.months,envir=.GlobalEnv)
	assign('date.years',date.years,envir=.GlobalEnv)

        # create a climdex input object
	cio <- climdexInput.raw(tmin=merge_data[,4],tmax=merge_data[,3],prec=merge_data[,2],tmin.dates=pcict.dates,tmax.dates=pcict.dates,prec.dates=pcict.dates,base.range=c(metadata$base.start,metadata$base.end),prec.qtiles=prec.quantiles,
				temp.qtiles=temp.quantiles,quantiles=quantiles)

	# add diurnal temperature range
	cio@data$dtr = cio@data$tmax - cio@data$tmin

	return(cio)
}

# This function runs QC functionality on the user specified input data. It requres as input;
#    - metadata: output of create.metadata()
#    - data: output of convert.user.file
#    - graphics: boolean for whether running with graphics or not (determines whether progress bars, message windows etc. are shown).
QC.wrapper <- function(metadata, user.data, user.file, graphics) {
	print("TESTING DATA, PLEASE WAIT...",quote=FALSE)
	if(graphics) { process.pb <<- tkProgressBar("%", "Checking latitude/longitude, base period...",0, 100, 10) }

	#############################################
	# BASIC CHECKS BEFORE climdex object creation
	
	# 1. Check for valid lat/lons
	if (is.na(metadata$lat) == TRUE | is.na(metadata$lon) == TRUE | metadata$lat < -90 | metadata$lat > 90 | metadata$lon > 180 | metadata$lon < -180) {
	  error.msg = paste("Please enter a valid latitude (-90 to +90) and longitude (-180 to +180).",sep = "")
	  if(graphics) { close(process.pb) ; tkmessageBox(message = error.msg)  } else {	skip <<- TRUE ; stop(error.msg) }
	  return()
	}

	# 2. Check base period is valid when no thresholds loaded
	if(is.null(quantiles)) {
	        if(metadata$base.start < format(metadata$dates[1],format="%Y") | metadata$base.end > format(metadata$dates[length(metadata$dates)],format="%Y") | metadata$base.start > metadata$base.end) {
					error.msg = paste("Base period must be between ", format(metadata$dates[1],format="%Y")," and ",format(metadata$dates[length(metadata$dates)],format="%Y"),". Please correct.",sep="")
					if(graphics) { close(process.pb) ; tkmessageBox(message = error.msg)  } else {	skip <<- TRUE ; stop(error.msg) }
					return()
			}
	}
	
	# 3. Check there are no missing dates by constructing a time series based on the first and last date provided by user and see if its length
	# is longer than the length of the user's data.
	length.of.user.data=length(user.data$year)
	first.date=as.Date(paste(user.data$year[1],user.data$month[1],user.data$day[1],sep="-"),"%Y-%m-%d")
	last.date=as.Date(paste(user.data$year[length.of.user.data],user.data$month[length.of.user.data],user.data$day[length.of.user.data],sep="-"),"%Y-%m-%d")
	date.series=seq(first.date,last.date,"day")
	user.date.series=as.Date(paste(user.data$year,user.data$month,user.data$day,sep="-"))
	missing.dates = date.series[!date.series %in% user.date.series]
	# Write out the missing.dates to a text file. Report the filename to the user.
	missing.dates.file = paste0(user.file,".missing_dates")
	if(file_test("-f",missing.dates.file)) { file.remove(missing.dates.file) }
	if(length(date.series[!date.series %in% user.date.series]) > 0) {
		write.table(date.series[!date.series %in% user.date.series], sep=",", file = missing.dates.file, append = FALSE, row.names=FALSE,col.names = FALSE)
		error.msg = paste0("You seem to have missing dates. See ",missing.dates.file," for a list of missing dates. Fill these with observations or missing values (-99.9) before continuing with quality control.")
		if(graphics) { close(process.pb) ; tkmessageBox(message = error.msg)  } else {	skip <<- TRUE ; stop(error.msg) }
		return()
	}

	# Check base period is valid when thresholds ARE loaded. 
#	if (!is.null(quantiles) && ((base.year.start >= format(metadata$dates[1],format="%Y") && base.year.start <= format(metadata$dates[length(metadata$dates)],format="%Y")) | 
#		(base.year.end <= format(metadata$dates[length(metadata$dates)],format="%Y") && base.year.end >= format(metadata$dates[1],format="%Y"))))
#	{
#	if(graphics) { 
#		tkmessageBox(message = paste("The base period of your loaded thresholds ","(",base.year.start," to ",base.year.end,") must lie outside of the current data's date range (",format(metadata$dates[1],format="%Y")," to ",
#                      format(metadata$dates[length(metadata$dates)],format="%Y"),").",sep = "")) }
#		return()
#	}

	# 4. Check for ascending order of years
	if(!all(user.data$year == cummax(user.data$year))) {
				error.msg = "Years are not in ascending order, please check your input file."
				if(graphics) { close(process.pb) ; tkmessageBox(message = error.msg)  } else {	skip <<- TRUE ; stop(error.msg) }
				return()
	}

    ##############################
	# Create climdex object
	# NICK: After this point all references to data should be made to the climdex input object 'cio'. One exception is the allqc function, 
	# which still references the INPUT to the climdex.input function.
	assign("latitude",  metadata$lat, envir = .GlobalEnv)
	assign("longitude", metadata$lon, envir = .GlobalEnv)

	cio <<- create.climdex.input(user.data,metadata)
	print("climdex input object created.",quote=FALSE)

    ##############################
	# Calculate and write out thresholds
	tavgqtiles <- get.outofbase.quantiles(cio@data$tavg,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=c(metadata$base.start,metadata$base.end),temp.qtiles=temp.quantiles,prec.qtiles=NULL)
	cio@quantiles$tavg$outbase <<- tavgqtiles$tmax$outbase	# while this says tmax it is actually tavg, refer to above line.

	# heat wave thresholds
	tavg <- (cio@data$tmax + cio@data$tmin)/2
	Tavg90p <- suppressWarnings(get.outofbase.quantiles(tavg,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=c(metadata$base.start,metadata$base.end),n=15,temp.qtiles=0.9,prec.qtiles=NULL,
	                                                                min.base.data.fraction.present=0.1))
	TxTn90p <- suppressWarnings(get.outofbase.quantiles(cio@data$tmax,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=c(metadata$base.start,metadata$base.end),n=15,temp.qtiles=0.9,prec.qtiles=NULL,
	                                                                min.base.data.fraction.present=0.1))
    tn90p <- TxTn90p$tmin$outbase
    tx90p <- TxTn90p$tmax$outbase
    tavg90p <- Tavg90p$tmax$outbase

	# write to file
    thres <- c(cio@quantiles$tmax$outbase,cio@quantiles$tmin$outbase,cio@quantiles$tavg$outbase,cio@quantiles$prec,as.list(tn90p),as.list(tx90p),as.list(tavg90p))#,cio@dates,cio@data)#$tmin,cio@data$tmax,cio@data$prec)
	nam1 <- paste(outthresdir, paste(ofilename, "_thres.csv", sep = ""),sep="/")
	write.table(as.data.frame(thres), file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "NA", col.names = c(paste("tmax",names(cio@quantiles$tmax$outbase),sep="_"),paste("tmin",names(cio@quantiles$tmin$outbase),sep="_"),
	paste("tavg",names(cio@quantiles$tavg$outbase),sep="_"),paste("prec",names(cio@quantiles$prec),sep="_"),"HW_TN90","HW_TX90","HW_TAVG90"),row.names=FALSE) 
	
    # write raw tmin, tmax and prec data for future SPEI/SPI calcs
	yeardate2 <- format(cio@dates,format="%Y")
	dates <-format(cio@dates,format="%Y-%m-%d")
	base.dates <- dates[which(yeardate2 >= metadata$base.start & yeardate2 <= metadata$base.end)]
	thres2 <- list(dates=base.dates,tmin=cio@data$tmin[which(yeardate2 >= metadata$base.start & yeardate2 <= metadata$base.end)],tmax=cio@data$tmax[which(yeardate2 >= metadata$base.start & yeardate2 <= metadata$base.end)],
	prec=cio@data$prec[which(yeardate2 >= metadata$base.start & yeardate2 <= metadata$base.end)])
	nam2 <- paste(outthresdir, paste(ofilename, "_thres_spei.csv", sep = ""),sep="/")
    write.table(as.data.frame(thres2), file = nam2, append = FALSE, quote = FALSE, sep = ", ", na = "NA", col.names = c("Base_period_dates","Base_period_tmin","Base_period_tmax","Base_period_prec"),row.names=FALSE)

    ##############################
	# Set some text options
	if(metadata$lat<0) lat_text = "°S" else lat_text = "°N"
	if(metadata$lon<0) lon_text = "°W" else lon_text = "°E"
	Encoding(lon_text) <- "UTF-8"	# to ensure proper plotting of degree symbol in Windows (which uses Latin encoding by default)
	Encoding(lat_text) <- "UTF-8"
	title.station <- paste(ofilename, " [", metadata$lat,lat_text, ", ", metadata$lon,lon_text, "]", sep = "")
	assign("title.station", title.station, envir = .GlobalEnv)

	##############################
	# output plots for tmin, tmax, prcp and dtr
	if(graphics) { 
		setTkProgressBar(process.pb,40,label="									")
		setTkProgressBar(process.pb,40,label="Creating data plots...") 
	}

	nam1 <- paste(outlogdir, paste(ofilename, "_prcpPLOT.pdf", sep = ""), sep = "/")
	check_open(nam1)
	pdf(file = nam1)
	
	prcp <- cio@data$prec[cio@data$prec >= 1 & !is.na(cio@data$prec)]

	if(length(prcp) > 30)
	{
		hist(prcp, main = paste("Histogram for Station:", ofilename, " of PRCP>=1mm", sep = ""),breaks = c(seq(0, 40, 2),max(prcp)), xlab = "", col = "green" , freq = FALSE)
		lines(density(prcp, bw = 0.2, from = 1), col = "red")
	}
	pplotts(var = "prcp", tit = ofilename,cio=cio,metadata=metadata)
	dev.off()
	nam1 <- paste(outlogdir, paste(ofilename, "_tmaxPLOT.pdf", sep = ""), sep = "/")
	check_open(nam1)
	pdf(file = nam1)
	pplotts(var = "tmax", type = "l", tit = ofilename,cio=cio,metadata=metadata)
	dev.off()
	nam1 <- paste(outlogdir, paste(ofilename, "_tminPLOT.pdf", sep = ""), sep = "/")
	check_open(nam1)
	pdf(file = nam1)
	pplotts(var = "tmin", type = "l", tit = ofilename,cio=cio,metadata=metadata)
	dev.off()
	nam1 <- paste(outlogdir, paste(ofilename, "_dtrPLOT.pdf", sep = ""), sep = "/")
	check_open(nam1)
	pdf(file = nam1)
	pplotts(var = "dtr", type = "l", tit = ofilename,cio=cio,metadata=metadata)
	dev.off()

	##############################
	# Call the ExtraQC functions.
	if(graphics) { 
		setTkProgressBar(process.pb,80,label="									")
		setTkProgressBar(process.pb,80,label="Calling quality control functions...") 
	}

	allqc(master = paste(user.file,".temporary",sep=""), output = outqcdir, outrange = 3) #stddev.crit)   # extraQC is called here. NOTE the default outrange=3 in original verson.
	if(graphics) tclvalue(qc.yes) <<- TRUE  # the QC step is done, so you can continue...

	##############################	
	# Write out NA statistics.
	write.NA.statistics(cio)

	##############################	
	# Remove temporary file
	if(file_test("-f",paste(user.file,".temporary",sep=""))) { file.remove(paste(user.file,".temporary",sep="")) }

	##############################
	# Draw 'QC complete' window
	# This windowing code is admittedly verbose due to poor documentation of tcltk functions and time constraints.
	if(graphics) {
		close(process.pb)
		tkdestroy(infor1)
		white1.green2()

		proc.complete.done <- function() { tkdestroy(proc.complete) }
	
		proc.complete <<- tktoplevel(bg = "white")
		tkfocus(proc.complete)
		tkwm.title(proc.complete, "\tClimPACT2\t")
		tt2 <- tkframe(proc.complete,bg="white")
		frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
		tkgrid(frame.space); tkgrid(tklabel(tt2, text = "QUALITY CONTROL COMPLETE", bg = "white", font = fontHeading2),columnspan=1);tkgrid(frame.space); tkgrid(tt2)
	
		tt2 <- tkframe(proc.complete,bg="white")
		frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
		tkgrid(frame.space)

		tkgrid(tklabel(tt2,text=paste("Carefully evaluate output in the following directory \nfor potential issues before continuing.\n\n",outlogdir,"\n\nRefer to Appendix C in the ClimPACT2 user guide for help.","\n\nOnce you are satisfied with the quality of your data, proceed to Step 2.",sep="")
				,bg='white',font=font_small),sticky="nsew")

		tkgrid(frame.space)
		tkgrid(tt2)
	
		tt2 <- tkframe(proc.complete,bg="white"); frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white");tkgrid(frame.space);tkgrid(tt2)
		tt2 <- tkframe(proc.complete,bg="white"); ok1.but<-tkbutton(tt2,text="    Done    ",command=proc.complete.done,bg='white',font=font_small);tkgrid(ok1.but);tkgrid(tt2)
		tt3 <- tkframe(proc.complete,bg="white");frame.space <- tklabel(tt3, text = " ", font = font_small, bg = "white");tkgrid(frame.space);tkgrid(tt3)
		loaded <<- TRUE
	}
	
#	##############################	
#	# Final check
#	# -- This code performs a check that should not be included in climpact. It's kept here because the functionality might be useful for something else.
#	# After QC work, final check is to see if sufficient amount of data exists for PR, TN and TX. If not, NA them. If all three variables have insufficient data, 
#	# stop with an appropriate message.
#	min.data.required = 0.7
#	length.of.pr = length(cio@data$prec)
#	missing.pr = sum(is.na(cio@data$prec))
#	length.of.tn = length(cio@data$tmin)
#	missing.tn = sum(is.na(cio@data$tmin))
#	length.of.tx = length(cio@data$tmax)
#	missing.tx = sum(is.na(cio@data$tmax))
#	
#	missing.str = ""
#	if(missing.pr/length.of.pr > (1-min.data.required)) { cio@data$prec[] <<- NA ; cio@namasks$annual$prec[] <<- NA ; cio@namasks$monthly$prec[] <<- NA ; missing.str = paste0(missing.str,"PR, ") }
#	if(missing.tn/length.of.tn > (1-min.data.required)) { cio@data$tmin[] <<- NA ; cio@data$tavg[] <<- NA ; cio@data$dtr[] <<- NA ; cio@namasks$annual$tmin[] <<- NA ; cio@namasks$monthly$tmin[] <<- NA ; cio@namasks$annual$tavg[] <<- NA ; cio@namasks$monthly$tavg[] <<- NA ; missing.str = paste0(missing.str,"TN, ") }
#	if(missing.tx/length.of.tx > (1-min.data.required)) { cio@data$tmax[] <<- NA ; cio@data$tavg[] <<- NA ; cio@data$dtr[] <<- NA ; cio@namasks$annual$tmax[] <<- NA ; cio@namasks$monthly$tmax[] <<- NA ; cio@namasks$annual$tavg[] <<- NA ; cio@namasks$monthly$tavg[] <<- NA ; missing.str = paste0(missing.str,"TX, ") }
#	print(paste0("Minimum fraction of data required: ",min.data.required))
#	print(paste0("Fraction of missing PR data: ",missing.pr/length.of.pr))
#	print(paste0("Fraction of missing TN data: ",missing.tn/length.of.tn))
#	print(paste0("Fraction of missing TX data: ",missing.tx/length.of.tx))
#
#	if(all(all(is.na(cio@data$prec)),all(is.na(cio@data$tmin)),all(is.na(cio@data$tmax)))) { 
#		error.msg=paste0("There is insufficient data to run ClimPACT2. At least ",min.data.required*100,"% of days in your input file must have valid data for one or more of PR, TX or TN.")
#		if(graphics) { close(process.pb) ; tkmessageBox(message = error.msg)  } else {	skip <<- TRUE ; stop(error.msg) }
#		return()
#	} else if (any(all(is.na(cio@data$prec)),all(is.na(cio@data$tmin)),all(is.na(cio@data$tmax)))) {
#		error.msg=paste0("The following variables have less than ",min.data.required*100,"% valid data and thus related indices will not be calculated: ",substr(missing.str,1,nchar(missing.str)-2),".")
#		if(graphics) { close(process.pb) ; tkmessageBox(message = error.msg)  } else {	skip <<- FALSE ; stop(error.msg) }
#		return()
#	}
} # end of QC.wrapper()

write.NA.statistics <- function(cio) { 
	naprec = array(NA,dim=c(length(unique(cio@date.factors$annual))))
	naprec = tapply.fast(cio@data$prec,cio@date.factors$annual,function(x) { return(sum(is.na(x))) })
	natx = tapply.fast(cio@data$tmax,cio@date.factors$annual,function(x) { return(sum(is.na(x))) })
	natn = tapply.fast(cio@data$tmin,cio@date.factors$annual,function(x) { return(sum(is.na(x))) })
	
	nam1 <- paste(outqcdir, paste(ofilename, "_nastatistics.csv", sep = ""), sep = "/")
	write_header(nam1)
	# Suppress warning about column names in files
	suppressWarnings(write.table(cbind.data.frame(unique(cio@date.factors$annual),naprec,natx,natn), file = nam1, sep=",",append = TRUE, quote = FALSE, row.names = FALSE, col.names = c("Year","Prec","TX","TN")))
}

# creates a list of metadata
create.metadata <- function(latitude,longitude,base.year.start,base.year.end,dates,ofilename) {
	return(list(lat=latitude,lon=longitude,base.start=base.year.start,base.end=base.year.end,year.start=as.numeric(format(dates[1],format="%Y")),year.end=as.numeric(format(dates[length(dates)],format="%Y")),dates=dates,ofile=ofilename)) 
}

# returns a date time-series from user data, removes any non-gregorian dates and corresponding data in the process
check.and.create.dates <- function(user.data) {
	yyymmdd <- paste(user.data[,1],user.data[,2],user.data[,3],sep="-")
	user.dates <- as.Date(yyymmdd,format="%Y-%m-%d")

	year <- user.data$year[!is.na(user.dates)]
	month <- user.data$month[!is.na(user.dates)]
	day <- user.data$day[!is.na(user.dates)]
	prcp <- user.data$prcp[!is.na(user.dates)]
	tmax <- user.data$tmax[!is.na(user.dates)]
	tmin <- user.data$tmin[!is.na(user.dates)]
	user.data2 <- data.frame(year=year,month=month,day=day,precp=prcp,tmax=tmax,tmin=tmin)

	user.data2$dates <- user.dates[!is.na(user.dates)]

	return(user.data2)
}

# This function draws the "Step 1" interface that lets user enter station metadata and run QC on their file.
draw.step1.interface <- function(user.data,user.file) {
	cheat.wrapper <- function () { 
		latitude  <- as.numeric(tclvalue(latentry))   # get user-input parameter, and check if they're valid.
        longitude <- as.numeric(tclvalue(lonentry))
        ofilename <<- tclvalue(station.entry)
        outdirtmp[length(outdirtmp)] <<- ofilename
        base.year.start<-as.numeric(tclvalue(base.year.start.tcl));  assign("base.year.start",base.year.start,envir=.GlobalEnv)
		base.year.end<-as.numeric(tclvalue(base.year.end.tcl));    assign("base.year.end",base.year.end,envir=.GlobalEnv)
		
        user.data <- check.and.create.dates(user.data)
		create.dir(user.file)
		metadata <- create.metadata(latitude,longitude,base.year.start,base.year.end,user.data$dates,ofilename)
		assign("metadata",metadata,envir=.GlobalEnv)
		QC.wrapper(metadata,user.data,user.file,graphics=TRUE)
	}
	
	cancel1 <- function() {
		cio <<- NULL
		quantiles <<- NULL
		loaded <<- FALSE
		tkdestroy(infor1)
		green1.white2()
		return() 
	}

	infor1 <<- tktoplevel(bg = "white")
	tkfocus(infor1)
	tkwm.geometry(infor1, "+300+200") # position in upper left corner of screen
	tkwm.title(infor1, "ClimPACT2 - Data preperation")	

        load.help<-function(){    # tip for the title in all plots.
                tkmessageBox(message=paste("Station name: name of the recording station that data originated from. This will be used to name output files and directories.",
					"Latitude/Longitude: geographical coordinates of the station in decimal form (-90 to +90 and -180 to +180).",
					"Base period: a beginning and end year (four digits) to use as a reference period to calculate percentile thresholds.",
#					"[OPTIONAL] Load additional field: an additional time-series of data (which can represent anything) to load and plot with the indices. The file must be formatted similarly to your climate data file and must contain identical dates. e.g. [year,month,day,data].",
					sep="\n\n"),icon='question')
        }

	station.entry <- tclVar(ofilename)
	tt1 <- tkframe(infor1, bg = "white")
	frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")
	tkgrid(tklabel(tt1, text = paste("FILE:", user.file, ""), bg = "white", font = font_small))
	tkgrid(tt1)

	tt1 <- tkframe(infor1, bg = "white")
	help1<-tkbutton(tt1,text='?',command=load.help,bg='white')
	tkgrid(tklabel(tt1, text = "ENTER RECORD INFORMATION", bg = "white", font = fontHeading2),help1)
	tkgrid(frame.space)
	tkgrid(tt1)

	# enter station name
	tt1 <- tkframe(infor1, bg = "white")
	tkgrid(tklabel(tt1, text = "STATION NAME", bg = "white", font = font_small))
	textEntryWidget1 <- tkentry(tt1, width = 20, textvariable = station.entry, bg = "white")
	tkgrid(textEntryWidget1)
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
	tkgrid(tt1)

	tt1<-tkframe(infor1,bg="white")
	frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")
	tkgrid(frame.space)
	tkgrid(tkbutton(tt1, text = "PROCESS AND\nQUALITY CONTROL",command=cheat.wrapper, font = fontHeading2, bg = "white"))
	tkgrid(frame.space)
	tkgrid(frame.space)
	tkgrid(tt1)

	tt1<-tkframe(infor1,bg="white")
	cancel1.but<-tkbutton(tt1,text="  CANCEL  ",command=cancel1,bg='white',font=font_small)
	frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")

	tkgrid(frame.space)
	tkgrid(frame.space)
	tkgrid(cancel1.but)
	frame.space <- tklabel(tt1, text = " ", font = font_small, bg = "white")

	tkgrid(frame.space)
	tkgrid(frame.space)
	tkgrid(tt1)
} # end of draw.step1.interface

# Paint button 1 green and button 2 white.
green1.white2 <- function() {
	tkconfigure(start.but,bg="lightgreen",text = "   LOAD AND  \n  CHECK DATA   ", command = load.data.qc, width = 15, font = fontHeading2)
	tkconfigure(cal.but,bg="white",text = "   CALCULATE \n   INDICES  ", command = draw.step2.interface, width = 15, font = fontHeading2)
}

# Paint button 1 white and button 2 green.
white1.green2 <- function() {
	tkconfigure(start.but,bg="white",text = "   LOAD AND  \n  CHECK DATA   ", command = load.data.qc, width = 15, font = fontHeading2)
	tkconfigure(cal.but,bg="lightgreen",text = "   CALCULATE \n   INDICES  ", command = draw.step2.interface, width = 15, font = fontHeading2)
}

# Let the user select a text file through a graphical menu. Return the file name and path.
get.user.file <- function() {
	dir.file.name <- tclvalue(tkgetOpenFile(filetypes="{{TEXT Files} {.txt}}"))
	if (dir.file.name=="") { return(); tkfocus(start1) }
	return(dir.file.name)
}

# Given a user's RClimdex text file path, read in, convert -99.9 to NA and return contents as array of 6 columns.
# The 'graphics' parameter indicates whether a progress bar is drawn to the screen via tcltk
read.user.file <- function(user.file,graphics=FALSE) {
# 	if(graphics) { process.pb <<- tkProgressBar("%", "Creating temporary file...",0, 100, 10) }
	temp.filename = paste(user.file,".temporary",sep="")
	raw.table = readLines(user.file)
	newtext = gsub(",","\t",raw.table)
	cat(newtext,file=temp.filename,sep="\n")

#	if(graphics) { 
#		setTkProgressBar(process.pb,80,label="								")
#		setTkProgressBar(process.pb,80,label="Reading data...") 
#	}
	
	data <- tryCatch(read.table(temp.filename,header=F,col.names=c("year","month","day","prcp","tmax","tmin"),colClasses=rep("real",6)),
			error= function(c) {
				if(graphics) { tkmessageBox(message = paste("Your input file doesn't appear to be formatted correctly. \n\nError returned was: ",c$message,
								"\n\nPlease correct your file, see the manual for correct formatting.", sep=""),icon = "warning", title = "ClimPACT2 - warning")
								close(process.pb)
								#tkfocus(start1)
								#load.data.qc()
				} else { stop(paste("INPUT FILE NOT FORMATTED CORRECTLY.\n\n",c$message,sep="")) }
				} )

	# Replace -99.9 data with NA
	if(!is.null(data)) { data$prcp[data$prcp==-99.9]=NA ; data$tmax[data$tmax==-99.9]=NA ; data$tmin[data$tmin==-99.9]=NA }

#	if(graphics) { close(process.pb) }
	return(data)
}

# Create directories for output. Requires get.file.path to be called beforehand. That functionality was moved to a separate function
# so that the directory could be modified by the user in the ClimPACT2 GUI.
# Undesirably these are currently kept as global variables.
create.dir <- function(user.file) {
	# create directory names
	if(length(outdirtmp)<=2) {
		dirsplit<-strsplit(user.file,":")[[1]][1]
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
	assign("outinddir",outinddir,envir=.GlobalEnv)
	assign("outlogdir",outlogdir,envir=.GlobalEnv)
	assign("outjpgdir",outjpgdir,envir=.GlobalEnv)
	assign("outtrddir",outtrddir,envir=.GlobalEnv)
	assign("outqcdir", outqcdir, envir=.GlobalEnv)
	assign("outthresdir",outthresdir,envir=.GlobalEnv)
}

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

# This function houses the beginning screen for "Step 2" in the GUI (i.e. calculating the indices). It reads in user preferences for the indices 
# and calls the index functions for calculation and plotting.
draw.step2.interface <- function() {
	if(is.null(cio)) {
		tkmessageBox(message = "Please load and \ncheck data first.",
		icon = "warning", title = "ClimPACT2 - warning")
		tkfocus(start1)
		return()
	}
   
	if(exists("proc.complete")) tkdestroy(proc.complete)
	tkdestroy(infor1)
	infor <- tktoplevel(bg = "white")
	tkfocus(infor)
	tkgrab.set(infor)
	tkwm.geometry(infor, "+300+200")
	tkwm.title(infor,"Set Parameter Values")
	
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
		tkmessageBox(message=paste('# = station name.\n e.g. If you input "station #"\nYou can get \nstation ',title.station,sep=''),icon='question')
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

	tkgrid(tklabel(tt1,text="",bg='white',font=font_small))
	tkgrid(tklabel(tt1,text="Refer to Section 3.5 of ClimPACT2 user guide for help",bg='white',font=font_small_bold))
	tkgrid(tklabel(tt1,text="User defined WSDId Days",bg='white',font=font_small),textEntryWidget13) # 13 wsdi
	tkgrid(tklabel(tt1,text="User defined CSDId Days",bg='white',font=font_small),textEntryWidget14) # 14 csdi
	tkgrid(tklabel(tt1,text="User defined Rxdday Days",bg='white',font=font_small),textEntryWidget15) # 15 rxday
	tkgrid(tklabel(tt1,text="User defined n for TXdTNd and TXbdTNbd",bg='white',font=font_small),textEntryWidget16) # txtn
	tkgrid(tklabel(tt1,text="User defined base temperature for HDDheatn",bg='white',font=font_small),textEntryWidget20) # Tb for HDDheat
	tkgrid(tklabel(tt1,text="User defined base temperature for CDDcoldn",bg='white',font=font_small),textEntryWidget21) # Tb for CDDcold
	tkgrid(tklabel(tt1,text="User defined base temperature for GDDgrown",bg='white',font=font_small),textEntryWidget22) # Tb for GDDgrow
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

	check.then.continue<-function()   # get user-definded parameters, check if they're valid, and set as global variable.
	{
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
		index.calc(metadata,TRUE)
	}  # end of function check.then.continue
  
	cancel1<-function() # Users don't want to continue, so close this window and return to main window.
	{
		tkdestroy(infor)
		return()
	}
  
	tt1<-tkframe(infor)
	ok1.but<-    tkbutton(tt1,text=" CALCULATE INDICES ",command=check.then.continue,bg='white',font=font_small)
	cancel1.but<-tkbutton(tt1,text="  CANCEL  ",command=cancel1,bg='white',font=font_small)
	tkgrid(ok1.but,cancel1.but)
	tkpack(tt1)
} # end of draw.step2.interface

# done
done<-function(){tkdestroy(start1)}

# This function loops through all indices and calls the appropriate functions to calculate them.
# It contains functions for some indices that are not kept in climpact2.etsci-functions.r. This is because they are specific to the GUI.
# The 'graphics' parameter indicates whether a progress bar is drawn to the screen via tcltk
index.calc<-function(metadata,graphics=TRUE){
	calculate.custom.index <- function() {
		print("calculating custom index",quote=FALSE)
		for (frequency in c("monthly","annual")) {
			if(var.choice=="DTR") { var.choice2=cio@data$dtr ; mask.choice = cio@namasks[[match.arg(frequency,choices=c("annual","monthly"))]]$tmin * cio@namasks[[match.arg(frequency,choices=c("annual","monthly"))]]$tmax } 
			else if (var.choice=="TX") { var.choice2=cio@data$tmax ; mask.choice = cio@namasks[[match.arg(frequency,choices=c("annual","monthly"))]]$tmax } 
			else if (var.choice=="TN") { var.choice2=cio@data$tmin ; mask.choice = cio@namasks[[match.arg(frequency,choices=c("annual","monthly"))]]$tmin }
			else if (var.choice=="TM") { var.choice2=cio@data$tavg ; mask.choice = cio@namasks[[match.arg(frequency,choices=c("annual","monthly"))]]$tmin }
			else if (var.choice=="PR") { var.choice2=cio@data$prec ; mask.choice = cio@namasks[[match.arg(frequency,choices=c("annual","monthly"))]]$prec }

			if(op.choice==">") { op.choice2="gt" }
			else if(op.choice==">=") { op.choice2="ge" }
			else if(op.choice=="<") { op.choice2="lt" }
			else if(op.choice=="<=") { op.choice2="le" }

			if(is.null(var.choice2)) return()
			index.stored <- number.days.op.threshold(var.choice2, cio@date.factors[[match.arg(frequency,choices=c("annual","monthly"))]], constant.choice, op.choice) * mask.choice
			write.index.csv(index.stored,index.name=paste(var.choice,op.choice2,constant.choice,sep=""),freq=frequency) ; 
			plot.call(index.stored,index.name=paste(var.choice,op.choice2,constant.choice,sep=""),index.units="days",x.label="Years",sub=paste("Number of days where ",var.choice," ",op.choice," ",constant.choice,sep=""),freq=frequency)
		}
	}

	calculate.hw <- function() {
		# If heatwave previous percentiles have been read in by user then use these in heatwave calculations, otherwise let climdex.hw calculate percentiles using currently loaded data.
        # #{ tx90p <- hwlist$HW.TX90 ; tn90p <- hwlist$HW.TN90 ; tavg90p <- hwlist$HW.TAVG90 } else {
			tx90p <<- tn90p <<- tavg90p <<- tavg05p <<- tavg95p <<- NULL #}

			index.stored <- climdex.hw(cio) #,tavg90p=tavg90p,tn90p=tn90p,tx90p=tx90p)

			write.hw.csv(index.stored,index.name=as.character(index.list$Short.name[i]),header="Heatwave definitions and aspects")
			plot.hw(index.stored,index.name=as.character(index.list$Short.name[i]),index.units=as.character(index.list$Units[i]),x.label="Years",metadata=metadata)
	}

	calculate.spei <- function() {
		if(all(is.na(cio@data$tmin)) | all(is.na(cio@data$tmax)) | all(is.na(cio@data$prec))) { print("NO DATA FOR SPEI.",quote=FALSE) ; return() } else {
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
		index.store <- array(c(cspei(dat,na.rm=T,scale=c(3),ref.start=c(metadata$base.start,1),ref.end=c(metadata$base.end,12),basetmin=tnraw,basetmax=txraw,baseprec=praw,basetime=btime)$fitted,
					cspei(dat,na.rm=T,scale=c(6),ref.start=c(metadata$base.start,1),ref.end=c(metadata$base.end,12))$fitted,
					cspei(dat,na.rm=T,scale=c(12),ref.start=c(metadata$base.start,1),ref.end=c(metadata$base.end,12))$fitted,
					cspei(dat,na.rm=T,scale=c(custom_SPEI),ref.start=c(metadata$base.start,1),ref.end=c(metadata$base.end,12))$fitted),
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
		write.precindex.csv(index.store,index.name=index.list$Short.name[82],spifactor,header="Standardised Precipitation-Evapotranspiration Index")
		plot.precindex(index.store,index.name=index.list$Short.name[82],index.units=index.list$Units[81],x.label="Years",spifactor,sub=as.character(index.list$Definition[82]),times=c(3,6,12,custom_SPEI),metadata=metadata) } 
	}
		
	calculate.spi <- function() {
			if(all(is.na(cio@data$prec))) { print("NO DATA FOR SPI.",quote=FALSE) ; return() } else {
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
                index.store <- array(c(cspi(dat,na.rm=T,scale=3,ref.start=c(metadata$base.start,1),ref.end=c(metadata$base.end,12))$fitted,
					cspi(dat,na.rm=T,scale=6,ref.start=c(metadata$base.start,1),ref.end=c(metadata$base.end,12))$fitted,
					cspi(dat,na.rm=T,scale=12,ref.start=c(metadata$base.start,1),ref.end=c(metadata$base.end,12))$fitted,
					cspi(dat,na.rm=T,scale=custom_SPEI,ref.start=c(metadata$base.start,1),ref.end=c(metadata$base.end,12))$fitted),
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
		write.precindex.csv(index.store,index.name=index.list$Short.name[83],spifactor,header="Standardised Precipitation Index")
		plot.precindex(index.store,index.name=index.list$Short.name[83],index.units=index.list$Units[82],x.label="Years",spifactor,sub=as.character(index.list$Definition[83]),times=c(3,6,12,custom_SPEI),metadata=metadata) 
		} 
	}

	# pdf file for all plots
	# Check 'all' PDF isn't open, then open.
	pdfname = paste(ofilename,"_all_plots.pdf",sep="")

	tmp = try(pdf(file=paste(outjpgdir,pdfname,sep="/"),height=8,width=11.5))
	if(class(tmp)=="try-error") { tkmessageBox(message=paste("Error encountered, please check that the file ",pdfname," is not currently open, then select OK to try again.",sep=""),icon='warning'); return() }
	pdf.dev=dev.cur()
	assign('pdf.dev',pdf.dev,envir=.GlobalEnv)

	# Read in index .csv file
	index.list <- read.csv("ancillary/climate.indices.csv",header=T,sep='\t')

	# trend file
	trend_file<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/") ; assign('trend_file',trend_file,envir=.GlobalEnv)
	write_header(trend_file,"Linear trend statistics")
	cat(file=trend_file,paste("Index","Frequency","StartYear","EndYear","Slope","STD_of_Slope","P_Value",sep=","),fill=180,append=T)

	index_not_calculated=''   # contains index names that could not be calculated.
	assign('index_not_calculated',index_not_calculated,envir=.GlobalEnv)

	# create a list of indices that do not require a 'frequency' parameter
	no.freq.list = c("r95ptot","r99ptot","sdii","hddheat","cddcold","gddgrow","r95p","r99p","gsl","spi","spei","hw","wsdi","wsdin","csdi","csdin","ntxntn","ntxbntnb")

	#####################################
	# MEAT DONE HERE
	# Loop through and calculate and plot each index
	if(graphics) pb <- tkProgressBar("Index calculation progress", "Calculation complete %",0, 100, 10)

	for (i in 1:length(index.list$Short.name)) {
		print(paste("calculating",index.list$Short.name[i]),quote=FALSE)
		tmp.index.name = as.character(index.list$Short.name[i])
		tmp.index.def = as.character(index.list$Definition[i])
		# Set frequency if relevant to current index
		if(is.na(index.list$Annual.flag[i])) frequency = NA
		else {
			if(index.list$Annual.flag[i]==TRUE) frequency = "annual"
			else frequency = "monthly"
		}
		
		if(!as.character(index.list$Short.name[i]) %in% no.freq.list) index.parameter = paste("cio,freq=\"",frequency,"\"",sep="")
		else index.parameter = paste("cio",sep="")
		
		if(index.list$Short.name[i]=="hw") { calculate.hw() ; next }
		else if (index.list$Short.name[i]=="spei") { calculate.spei() ; next }
		else if (index.list$Short.name[i]=="spi") { calculate.spi() ; next }
		else if (index.list$Short.name[i]=="rnnmm") {
			tmp.index.name = paste("r",rnnmm_ud,"mm",sep="")
			index.parameter = paste(index.parameter,rnnmm_ud,sep=",")
			tmp.index.def = paste("Number of days when precipitation >= ",rnnmm_ud,sep="") }
		else if (index.list$Short.name[i]=="wsdid") {
			tmp.index.name = paste("wsdi",wsdi_ud,sep="")
			index.parameter = paste("cio,n=",wsdi_ud,sep="")
			tmp.index.def = paste("Annual number of days with at least ",rnnmm_ud," consecutive days when TX > 90th percentile",sep="") }
		else if (index.list$Short.name[i]=="csdid") {
			tmp.index.name = paste("csdi",csdi_ud,sep="")
			index.parameter = paste("cio,n=",csdi_ud,sep="")
			tmp.index.def = paste("Annual number of days with at least ",csdi_ud," consecutive days when TN < 10th percentile",sep="") }
		else if (index.list$Short.name[i]=="txdtnd") {
			tmp.index.name = paste("tx",txtn_ud,"tn",txtn_ud,sep="")
			index.parameter = paste("cio,n=",txtn_ud,sep="")
			tmp.index.def = paste("Number of ",txtn_ud," consecutive days where both TX > 95th percentile and TN > 95th percentile",sep="") }
		else if (index.list$Short.name[i]=="txbdtnbd") {
			tmp.index.name = paste("txb",txtn_ud,"tnb",txtn_ud,sep="")
			index.parameter = paste("cio,n=",txtn_ud,sep="")
			tmp.index.def = paste("Number of ",txtn_ud," consecutive days where both TX < 5th percentile and TN < 5th percentile",sep="") }
		else if (index.list$Short.name[i]=="rxdday") {
			tmp.index.name = paste("rx",rx_ud,"day",sep="")
			index.parameter = paste(index.parameter,",n=",rx_ud,sep="")
			tmp.index.def = paste("Maximum ",rx_ud,"-day precipitation total",sep="") }
		else if (index.list$Short.name[i]=="hddheatn") {
			tmp.index.name = paste("hddheat",Tb_HDD,sep="")
			index.parameter = paste("cio,Tb=",Tb_HDD,sep="")
			tmp.index.def = paste("Annual sum of ",Tb_HDD," - TM",sep="") }
		else if (index.list$Short.name[i]=="cddcoldn") {
			tmp.index.name = paste("cddcold",Tb_CDD,sep="")
			index.parameter = paste("cio,Tb=",Tb_CDD,sep="")
			tmp.index.def = paste("Annual sum of TM - ",Tb_CDD,sep="") }
		else if (index.list$Short.name[i]=="gddgrown") {
			tmp.index.name = paste("gddgrow",Tb_GDD,sep="")
			index.parameter = paste("cio,Tb=",Tb_GDD,sep="")
			tmp.index.def = paste("Annual sum of TM - ",Tb_GDD,sep="") }

		index.stored <- eval(parse(text=paste("climdex.",as.character(index.list$Short.name[i]),"(",index.parameter,")",sep=""))) #index.function(cio)
		index.stored[index.stored==-Inf] = NA	# Because climdex functions (called in above line) will still calculate even if all data are NA, resulting in -Inf values being inserted into index.stored. Climdex functions only check if cio data are NULL.
		write.index.csv(index.stored,index.name=tmp.index.name,freq=frequency,header=tmp.index.def)
		plot.call(index.stored,index.name=tmp.index.name,index.units=as.character(index.list$Units[i]),x.label="Years",sub=tmp.index.def,freq=frequency)
		remove(index.parameter)

		if(graphics) {
			progress = round(as.numeric(i/length(index.list$Short.name))*100,0)
			setTkProgressBar(pb,progress,title=paste(progress,"%",sep=""))
		}
	}
	if(length(op.choice)==0 || length(var.choice)==0) { print("no custom index to calculate",quote=FALSE) } else { calculate.custom.index() }
	dev.off(pdf.dev)

	# Show message complete window and locations to output
	if(graphics) 
	{
		close(pb)
		graphics.off()  # close the pdf file, so you can open to view it now.

		nstation<-tktoplevel(bg='white')
		tkwm.geometry(nstation, "+300+200") # position in upper left corner of screen
		tkwm.title(nstation,"ClimPACT2 - Done")
        tkfocus(nstation)
	
		okk<-function(){tkdestroy(nstation);tkfocus(start1)}  # all are done, return to main window.
		textlabel0<-tklabel(nstation,text="     ",bg='white')  # message showing all are done, showing directory.
		textlabel1<-tklabel(nstation,text="Indices calculation completed",font=fontHeading1,bg='white')
		textlabel2<-tklabel(nstation,text=paste("   Plots are in ",outjpgdir,"   ",sep=" "),font=fontHeading2,bg='white')
		textlabel3<-tklabel(nstation,text=paste("   Data are in ",outinddir,"   ",sep=" "),font=fontHeading2,bg='white')

		okk.but<-tkbutton(nstation,text="   OK   ",command=okk,width=20,bg='white')
		tkpack(textlabel1)
		tkpack(textlabel2)
		tkpack(textlabel3)
		tkpack(textlabel0)
		tkpack(okk.but)
	}
}
# end of index.calc 

# write.index.csv
# takes a time series of a given index and writes to file
write.index.csv <- function(index=NULL,index.name=NULL,freq="annual",header="") {
	if(is.null(index) | all(is.na(index))) { print(paste0("NO DATA FOR ",index.name,". NOT WRITING .csv FILE."),quote=FALSE) ; return() }

	if(index.name=="tx95t") { freq="DAY" } 
	else {
		if(freq=="monthly") { freq="MON" }
		else if(freq=="annual") { freq="ANN" }
	}

	if(index.name=="wsdin") { tmp.name=paste("wsdi",wsdi_ud,sep="") } 
	else if (index.name=="csdid") { tmp.name=paste("csdi",csdi_ud,sep="") }
	else if (index.name=="rxdday") { tmp.name=paste("rx",rx_ud,"day",sep="") }
	else if (index.name=="rnnmm") { tmp.name=paste("r",rnnmm_ud,"mm",sep="") }
	else if (index.name=="txdtnd") { tmp.name=paste("tx",txtn_ud,"tn",txtn_ud,sep="") }
	else if (index.name=="txbdtnbd") { tmp.name=paste("txb",txtn_ud,"tnb",txtn_ud,sep="") }
	else { tmp.name = index.name }
	nam1 <- paste(outinddir, paste(ofilename, "_", tmp.name,"_",freq, ".csv", sep = ""), sep = "/")
	write_header(nam1,header)
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
write.hw.csv <- function(index=NULL,index.name=NULL,header="") {
#		if(is.null(index) | all(is.na(index))) { print("No index data, not writing CSV file.") ; return() }

		# print each definition in a separate .csv. Thus each .csv will have columns of time, HWA, HWM, HWF, HWD, HWN.
		aspect.names <- list("time","HWM","HWA","HWN","HWD","HWF")
		aspect.names.ECF <- list("time","CWM","CWA","CWN","CWD","CWF")

		# write Tx90 heatwave data
		if(!all(is.na(cio@data$tmax))) {
        nam1 <- paste(outinddir, paste(ofilename, "_tx90_heatwave_ANN.csv", sep = ""), sep = "/")
		write_header(nam1,header)
		write.table(aspect.names, file = nam1, append = TRUE, quote = FALSE, sep = ", ", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),aperm(index[1,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", row.names=FALSE,col.names = FALSE)
		}
		
        # write Tn90 heatwave data
        if(!all(is.na(cio@data$tmin))) {
        nam1 <- paste(outinddir, paste(ofilename, "_tn90_heatwave_ANN.csv", sep = ""), sep = "/")
        write_header(nam1,header)
        write.table(aspect.names, file = nam1, append = TRUE, quote = FALSE, sep = ", ", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),aperm(index[2,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", row.names=FALSE,col.names = FALSE)
		}

        # write EHF heatwave data
        if((!all(is.na(cio@data$tmax))) && (!all(is.na(cio@data$tmin)))) {
        nam1 <- paste(outinddir, paste(ofilename, "_ehf_heatwave_ANN.csv", sep = ""), sep = "/")
        write_header(nam1,header)
        write.table(aspect.names, file = nam1, append = TRUE, quote = FALSE, sep = ", ", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),aperm(index[3,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", row.names=FALSE,col.names = FALSE)
		}
		
        # write ECF coldwave data
        if((!all(is.na(cio@data$tmax))) && (!all(is.na(cio@data$tmin)))) {
        nam1 <- paste(outinddir, paste(ofilename, "_ecf_heatwave_ANN.csv", sep = ""), sep = "/")
        write_header(nam1,header)
        write.table(aspect.names.ECF, file = nam1, append = TRUE, quote = FALSE, sep = ", ", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),aperm(index[4,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", row.names=FALSE,col.names = FALSE)
		}
}

# plot.hw
plot.hw <- function(index=NULL,index.name=NULL,index.units=NULL,x.label=NULL,metadata) {
#        if(is.null(index)) stop("Need heatwave data to plot.")

	definitions <- c("Tx90","Tn90","EHF","ECF")
	aspects <- c("HWM","HWA","HWN","HWD","HWF")
	units <- c("°C","°C","heatwaves","days","days")
	Encoding(units) <- "UTF-8"

	# mask out indices that should not have values due to no or insufficient input data from user
	if(all(is.na(cio@data$tmax))) { index[c(1,3,4),,] = NA }
	if(all(is.na(cio@data$tmin))) { index[c(2,3,4),,] = NA }

	for (def in 1:length(definitions)) {
		for (asp in 1:length(aspects)) {
			if(all(is.na(index[def,asp,]))) { print(paste("NO DATA FOR HEATWAVE [",aspects[asp],", ",definitions[def],"].",sep="")) ; next }

			plot.title <- paste("Station: ",title.station,sep="")
			if(definitions[def]=="ECF") { namp <- paste(outjpgdir, paste(ofilename, "_", tolower(gsub("H","C",aspects[asp])),"_",tolower(definitions[def]), "_ANN.jpg", sep = ""), sep = "/") }
			else { namp <- paste(outjpgdir, paste(ofilename, "_",tolower(aspects[asp]),"_",tolower(definitions[def]), "_ANN.jpg", sep = ""), sep = "/") }
			jpeg(file = namp, width = 1024, height = 768)
			dev0 = dev.cur()

			if(aspects[asp]=="HWM" && !definitions[def] == "ECF") { sub=paste("Index: ",aspects[asp],"-",definitions[def],". Heatwave Magnitude (mean temperature of all heatwave events)",sep="") } 
			else if(aspects[asp]=="HWA" && !definitions[def] == "ECF"){ sub=paste("Index: ",aspects[asp],"-",definitions[def],". Heatwave Amplitude (peak temperature of the hottest heatwave event)",sep="") }
			else if(aspects[asp]=="HWD" && !definitions[def] == "ECF"){ sub=paste("Index: ",aspects[asp],"-",definitions[def],". Heatwave Duration (length of longest heatwave event)",sep="") }
			else if(aspects[asp]=="HWF" && !definitions[def] == "ECF"){ sub=paste("Index: ",aspects[asp],"-",definitions[def],". Heatwave Frequency (number of days contributing to heatwave events)",sep="") }
			else if(aspects[asp]=="HWN" && !definitions[def] == "ECF"){ sub=paste("Index: ",aspects[asp],"-",definitions[def],". Heatwave Number (number of discreet heatwave events)",sep="") }

			if(aspects[asp]=="HWM" && definitions[def] == "ECF") { sub=paste("Index: ",gsub("H","C",aspects[asp]),"-",definitions[def],". Coldwave Magnitude (mean temperature of all coldwave events)",sep="") } 
			else if(aspects[asp]=="HWA" && definitions[def] == "ECF"){ sub=paste("Index: ",gsub("H","C",aspects[asp]),"-",definitions[def],". Coldwave Amplitude (minimum temperature of the coldest coldwave event)",sep="") }
			else if(aspects[asp]=="HWD" && definitions[def] == "ECF"){ sub=paste("Index: ",gsub("H","C",aspects[asp]),"-",definitions[def],". Coldwave Duration (length of longest coldwave event)",sep="") }
			else if(aspects[asp]=="HWF" && definitions[def] == "ECF"){ sub=paste("Index: ",gsub("H","C",aspects[asp]),"-",definitions[def],". Coldwave Frequency (number of days contributing to coldwave events)",sep="") }
			else if(aspects[asp]=="HWN" && definitions[def] == "ECF"){ sub=paste("Index: ",gsub("H","C",aspects[asp]),"-",definitions[def],". Coldwave Number (number of discreet coldwave events)",sep="") }

			if((definitions[def]=="EHF" || definitions[def]=="ECF") && any(aspects[asp]=="HWM",aspects[asp]=="HWA")) { unit = "°C^2" ; Encoding(unit) <- "UTF-8" } else unit = units[asp]
			plotx((date.years), index[def,asp,], main = gsub('\\*', unit, plot.title),ylab = unit,xlab = x.label,index.name=index.name,sub=sub)

			dev.set(which = pdf.dev)
			plotx((date.years), index[def,asp,], main = gsub('\\*', unit, plot.title),ylab = unit,xlab = x.label,index.name=paste(paste(definitions[def],aspects[asp],sep=".")),sub=sub)
#			dev.copy()
			dev.off(dev0)

			cat(file=trend_file,paste(paste(definitions[def],aspects[asp],sep="."),"ANN",metadata$year.start,metadata$year.end,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T)
		}
	}
   # close all jpeg graphics devices  - RJHD 2017-12-19
   graphics.off()
}

# write.precindex.csv
write.precindex.csv <- function(index=NULL,index.name=NULL,spifactor=NULL,header="") {
#		if(is.null(index) | all(is.na(index))) { print("No index data, not writing CSV file.") ; return() }
		colnames <- list("time",index.name)

        # write 3 month data
        nam1 <- paste(outinddir, paste(ofilename, "_3month_",index.name,"_MON.csv", sep = ""), sep = "/")
        write_header(nam1,header)
        write.table(colnames, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(unique(as.character(spifactor)),index[1,]), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write 6 month data
        nam1 <- paste(outinddir, paste(ofilename, "_6month_",index.name,"_MON.csv", sep = ""), sep = "/")
        write_header(nam1,header)
        write.table(colnames, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(unique(as.character(spifactor)),index[2,]), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write 12 month data
        nam1 <- paste(outinddir, paste(ofilename, "_12month_",index.name,"_MON.csv", sep = ""), sep = "/")
        write_header(nam1,header)
        write.table(colnames, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(unique(as.character(spifactor)),index[3,]), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        
        # write custom-period data
        nam1 <- paste(outinddir, paste(ofilename,"_",custom_SPEI, "month_",index.name,"_MON.csv", sep = ""), sep = "/")
        write_header(nam1,header)
        write.table(colnames, file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(unique(as.character(spifactor)),index[4,]), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
}

# plot.precindex
# not sure how generic this process can be
plot.precindex <- function(index=NULL,index.name=NULL,index.units=NULL,x.label=NULL,spifactor=NULL,sub="",times="",metadata) {
#        if(is.null(index)) stop("Need precip data to plot.")
		Encoding(sub) <- "UTF-8"

        for (time in 1:4) {
			if(all(is.na(index[time,]))) { print(paste("All NA values detected, not plotting ",times[time]," month ",index.name,".",sep="")) ; next }

			subtmp=paste("Index: ",index.name," ",times[time]," month. ",sub,sep="")
			namp <- paste(outjpgdir, paste(ofilename, "_",times[time],"month_",index.name,"_MON.jpg", sep = ""), sep = "/")
			jpeg(file = namp, width = 1024, height = 768)

			dev0 = dev.cur()
	        plotx(unique(as.character(spifactor)), index[time,], main = paste(gsub('\\*', index.name, plot.title),sep=""),ylab = index.units,xlab = x.label,index.name=index.name,sub=subtmp)

            dev.set(which = pdf.dev)
            plotx(unique(as.character(spifactor)), index[time,], main = paste(gsub('\\*', index.name, plot.title),sep=""),ylab = index.units,xlab = x.label,index.name=index.name,sub=subtmp)
#            dev.copy()
            dev.off(dev0)

            cat(file=trend_file,paste(paste(index.name,times[time],"month",sep="."),"MON",metadata$year.start,metadata$year.end,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T)
        }
        # close all jpeg graphics devices  - RJHD 2017-12-19
        graphics.off()
}

# plot.index
plot.call <- function(index=NULL,index.name=NULL,index.units=NULL,x.label=NULL,sub="",freq="annual") {
	if(all(is.na(index))) { print(paste0("NO DATA FOR ",index.name,". NOT PLOTTING."),quote=FALSE) ; return() }
	
		Encoding(sub) <- "UTF-8"
		Encoding(index.units) <- "UTF-8"
#	plot.title <- paste(title.station,index.name,sep=", ")
	if(index.name=="wsdin") { tmp.name=paste("wsdi",wsdi_ud,sep="") ; sub=paste("Index: ",tmp.name,". Annual number of days with at least ",wsdi_ud," consecutive days when TX > 90th percentile",sep="") } 
	else if (index.name=="csdin") { tmp.name=paste("csdi",csdi_ud,sep="") ; sub=paste("Index: ",tmp.name,". Annual number of days with at least ",csdi_ud," consecutive days when TN < 10th percentile",sep="")  }
	else if (index.name=="rxnday") { tmp.name=paste("rx",rx_ud,"day",sep="") ; sub=paste("Index: ",tmp.name,". Maximum ",freq," ",rx_ud,"-day precipitation total",sep="")  }
	else if (index.name=="rnnmm") { tmp.name=paste("r",rnnmm_ud,"mm",sep="") ; sub=paste("Index: ",tmp.name,". ",freq," number of days when precipitation >= ",rnnmm_ud,"mm",sep="")  }
	else if (index.name=="ntxntn") { tmp.name=paste(txtn_ud,"tx",txtn_ud,"tn",sep="") ; sub=paste("Index: ",tmp.name,". Annual number of ",txtn_ud," consecutive days where both TX > 95th percentile and TN > 95th percentile",sep="")  }
	else if (index.name=="ntxbntnb") { tmp.name=paste(txtn_ud,"txb",txtn_ud,"tnb",sep="") ; sub=paste("Index: ",tmp.name,". Annual number of ",txtn_ud," consecutive days where both TX < 5th percentile and TN < 5th percentile",sep="")  }
	else if (index.name=="cddcold") { tmp.name=index.name ; sub=paste("Index: ",tmp.name,". Annual sum of TM - ",Tb_CDD,"°C (where ",Tb_CDD,"°C is a user-defined base temperature and should be smaller than TM)",sep="")  }
	else if (index.name=="hddheat") { tmp.name=index.name ; sub=paste("Index: ",tmp.name,". Annual sum of ",Tb_HDD,"°C - TM (where ",Tb_HDD,"°C is a user-defined base temperature and should be larger than TM)",sep="")  }
	else if (index.name=="gddgrow") { tmp.name=index.name ; sub=paste("Index: ",tmp.name,". Annual sum of TM - ",Tb_GDD,"°C (where ",Tb_GDD,"°C is a user-defined base temperature and should be smaller than TM)",sep="")  }
	else { tmp.name = index.name ; sub=paste("Index: ",tmp.name,". ",sub,sep="") }

	if(index.name=="tx95t") { freq="DAY" } 
	else {
		if(freq=="monthly") { freq="MON" }
		else if(freq=="annual") { freq="ANN" }
	}
	
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
	  
	cat(file=trend_file,paste(index.name,freq,metadata$year.start,metadata$year.end,round(as.numeric(out$coef.table[[1]][2, 1]), 3),round(as.numeric(out$coef.table[[1]][2, 2]), 3),round(as.numeric(out$summary[1, 6]),3),sep=","),fill=180,append=T)

#	dev.copy()
	dev.off(dev0)
}
graphics.off()

# plotx
# make plots, this is called twice to make jpg and pdf files. 
plotx <- function (x0, y0, main = "", xlab = "", ylab = "", opt = 0,index.name=NULL,sub="")
{
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
			xind = xind[seq(1,length(xind),12)]
			xtmp.int = unique(subx[xind]) 
			axis(1,at=xind,labels=c(xtmp.int))

            box()
			xy <- cbind(bp,y)
		} else {
			op <- par(mar=c(5, 4, 5, 2) + 0.1)
			plot(1:length(x), unname(y), cex.main = 2,ylim = range(unname(y), na.rm = TRUE),xaxt="n", xlab = "", ylab = ylab,type = "b", cex.lab = 1.5, cex.axis = 1.5,col="black")
			par(op)
			title(main,line=2.5,cex.main = 2)

			subx = as.numeric(substr(x,1,4))
			xind = which(subx%%5==0)
			if(nchar(x[1])==7) { 
				xind = xind[seq(1,length(xind),12)]
				xtmp.int = unique(subx[xind]) 
			} else { 
				xtmp.int = subx[xind] 
			}
			axis(1,at=xind,labels=c(xtmp.int))

			mtext(paste(strwrap(sub,width=100),collapse="\n"),cex=1)

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
cspi <- function(data, scale, kernel=list(type='rectangular',shift=0),
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
        tkwm.title(ab, "\tClimPACT2 - Credits\t")
        tt2 <- tkframe(ab,bg="white")
        frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
        tkgrid(tklabel(tt2, text = "Credits", bg = "white", font = fontHeading2),columnspan=1)
        tkgrid(frame.space)
        tkgrid(tt2)

        tt2 <- tkframe(ab,bg="white")
        frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
        tkgrid(frame.space)
        tkgrid(tklabel(tt2,text=
" Oversight: World Meteorological Organisation (WMO), the Expert Team 
 on Sector Climate Indices (ET-SCI).
  
 Design and documentation: Lisa Alexander and Nicholas Herold.
  
 GUI: Nicholas Herold, James Goldie, Hongang Yang, Yang Feng and Yujun Ouyang.
  
 NetCDF calculation: Pacific Climate Impacts Consortium (David Bronaugh, 
 James Hiebert), Nicholas Herold.
  
 Batch processing: Nicholas Herold.

 For any comments, questions or suggestions, e-mail nicholas.herold@unsw.edu.au"
,bg='white',font=font_small,width=90,justify="left"),sticky="nsew")
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
	tkgrid(frame.space)
	tkgrid(tt2)
	
	tt2 <- tkframe(lic,bg="white")
	frame.space <- tklabel(tt2, text = " ", font = font_small, bg = "white")
	tkgrid(frame.space)
	tkgrid(tklabel(tt2,text=
" Copyright (C) 2016 University of New South Wales

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, version 3 of the License.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see https://www.gnu.org/licenses/gpl-3.0.txt.",bg='white',font=font_small,width=90),sticky="nsew")
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

# Check for required packages and install if necessary
package.check <- function() {
	gui.packages <- c("bitops","Rcpp","caTools","PCICt","SPEI","climdex.pcic")
	new.packages <- gui.packages[!(gui.packages %in% installed.packages()[,"Package"])]

	# Install/update packages needed for ClimPACT2 GUI.
	if(length(new.packages)) {
	        print("******************************")
	        print(paste("Installing the following required packages...",new.packages,sep=""))
	        install.packages(new.packages) 
	}

	print(paste("R version ",as.character(getRversion())," detected.",sep=""),quote=FALSE)
}

# Define global variables. The use of global variables is undesirable from a programming point of view
# but for historical purposes is still used in this code.
global.vars <- function() {
	# Nullify objects globally to avoid warning messages.
	reading.pb <<- process.pb <<- pb <<- orig.name.user <<- qc.yes <<- outthresdir <<- quantiles <<- cio <<- ofilename <<- infor1 <<- orig.name <<- title.station <<- outlogdir <<- thres.calc <<- 
	add.data <<- add.data.name <<- out <<- ref.start <<- ts.end <<- basetmin <<- basetmax <<- baseprec <<- start.but <<- cal.but <<- ttmp <<- outqcdir <<- NULL
	
	start1<<-tktoplevel(bg='white')
	version.climpact <<- software_id
	
	# Fonts 
	fontHeading     <<- tkfont.create(family = "times", size = 40, weight = "bold", slant = "italic")
	fontHeading1    <<- tkfont.create(family = "times", size = 18)
	fontHeading2    <<- tkfont.create(family = "times", size = 14, weight = "bold")
	fontTextLabel   <<- tkfont.create(family = "arial", size = 12)
	font_small  <<- "arial 12"
	font_small_bold  <<- "arial 12 bold"
	font_err    <<- "times 13 bold"
	grey_font <<- tkfont.create(family = "arial", size = 30, weight = "bold", slant = "italic") #'times 20 grey bold'
	
	# Global variables
	running.zero.allowed.in.temperature <<- 4
	temp.quantiles <<- c(0.05,0.1,0.5,0.9,0.95)
	prec.quantiles <<- c(0.05,0.1,0.5,0.9,0.95,0.99)
	barplot_flag    <<- TRUE
	loaded <<- FALSE
	min_trend     <<- 10	# minimum number of data points for plotting a linear trend
	
	# Initial index parameter values
	stations<<-tclVar(paste(" "))
	base.year.start.tcl<<-tclVar(paste("1971"));base.year.end.tcl<<-tclVar(paste("2000"))
	latentry<<-tclVar(''); lonentry<<-tclVar('') ; add.data.name.entry <<-tclVar('')
	Entry4<<-tclVar(paste("0"))
	Entry5<<-tclVar(paste("0"))
	Entry13<<-tclVar(paste("2"))
	Entry14<<-tclVar(paste("2"))
	Entry15<<-tclVar(paste("3"))
	Entry16<<-tclVar(paste("2"))
	Entry17<<-tclVar(paste("30"))
	Entry20<<-tclVar(paste("18"))
	Entry21<<-tclVar(paste("18"))
	Entry22<<-tclVar(paste("10"))
	Entry23<<-tclVar(paste("24"))
	Entry24<<-tclVar(paste("0"))
}

# Main GUI function. Initiates GUI.
startss <- function() {
	package.check()
	library(tcltk)
	tclRequire("BWidget")
	source("ancillary/climpact2.etsci-functions.r")
	global.vars()
	logo_require <- FALSE
	
	# search logo files in current working directory.
	no.logo <- FALSE;
	dir0 <- getwd();
	logo1 <- "user_guide/images/coess_unsw.gif";
	
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
	if (file.exists(paste(dir0, "/", logo1, sep = "")) == FALSE) no.logo <- TRUE; # find logos?
	
	tkwm.geometry(start1, "+400+200"); # position in upper left corner of screen
	tkwm.title(start1, paste("ClimPACT2",sep=" "));
	
	# Show logos on upper half of the main window, or "no logos available".  
	if (no.logo == FALSE)
	{  # with logos
	  logo1 <- paste(dir0, "/", logo1, sep = "");
	  img  <- tkimage.create("photo", file = logo1);
	  right <- tklabel(start1, image = img);
	  tkgrid(right,columnspan=3)
	} else
	{    # no logos, show a help button.
	  help.logo <- function()
	  {
	    tkmessageBox(message=paste('You can see this help because the logo files are not in the working directory of R!',sep = ''), icon = 'question');
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
	cal.but     <<- tkbutton(start1, text = "   CALCULATE \n   INDICES  ", command = draw.step2.interface, width = 15, font = fontHeading2, bg = "white")
	
	tkgrid(tklabel(start1, text = " STEP. 1", bg = "white",font=fontHeading1,width=8), columnspan =3)
	tkgrid(start.but, columnspan =3)
	tkgrid(tklabel(start1, text = "    ", bg = "white"));
	tkgrid(tklabel(start1, text = " STEP. 2  ", bg = "white",font=fontHeading1,width=8), columnspan =3)
	tkgrid(cal.but, columnspan =3)


	cancel.but  <- tkbutton(start1, text = " Exit ", command = done, width = 7, font = fontHeading2, bg = "white");
#	help.but    <- tkbutton(start1, text = " About ", command = about, width = 7, font = fontHeading2, bg = "white");
	license.but <- tkbutton(start1, text = " License ", command = license, width = 7, font = fontHeading2, bg = "white");

	gap = tklabel(start1,width=5,text="",bg="white")

	tkgrid(tklabel(start1, text = "    ", bg = "white"));
	tkgrid(tklabel(start1, text = "    ", bg = "white"));

#	tkgrid(help.but, columnspan = 3)
	tkgrid(license.but, columnspan = 3)
	tkgrid(cancel.but, columnspan = 3)
	tkgrid(tklabel(start1, text = "", bg = "white"))
	tkgrid(tklabel(start1, text = "", bg = "white"))

	tkfocus(start1)
}

# Call function to start the program.
# This IF function is a hack to allow functionality in this file to be sourced without having to run the GUI itself.
# A more elegant solution likely exists. This IF statement requires that for startss() NOT to be called, more than 2 command-line
# parameters must have been specified (which is what happens when calling the batch processing code). "<=2" was chosen because
# while Linux systems only pass 1 command line argument when invoking R, Windows systems seem to pass 2 command line arguments.
#print(commandArgs())
if(length(commandArgs())<=2) startss()
