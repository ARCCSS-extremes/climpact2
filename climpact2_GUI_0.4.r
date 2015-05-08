# Climpact2 GUI
# This file constitutes graphical functionality for the Climpact2 package "climpact2".

climfile <- "climpact2_1.3.par.r"
source("climpact2_1.3.par.r")
library(tcltk)

# Global setup
# close all previously opened devices and remove all variables from memory ..
rm(list = ls(all = TRUE))
graphics.off()

nordaytem1 <<- outthresdir <<- quantiles <<- cio <<- ofilename <<- orig.name <<- title.station <<- outlogdir <<- thres.calc <<- ttmp <<- outqcdir <<- nordaytem1 <<- NULL

#logo_require    <- FALSE
start1<-tktoplevel(bg='white')

# fonts for different texts in GUI.
fontHeading     <- tkfont.create(family = "times", size = 40, weight = "bold", slant = "italic")
fontHeading1    <- tkfont.create(family = "times", size = 20, weight = "bold")
fontHeading2    <- tkfont.create(family = "times", size = 14, weight = "bold")
fontTextLabel   <- tkfont.create(family = "times", size = 12)
fontFixedWidth  <- tkfont.create(family = "courier", size = 12)
font_small  <- "times 12"
font_big    <- "times 15 bold"
font_err    <- "times 13 bold"
grey_font <- tkfont.create(family = "times", size = 30, weight = "bold", slant = "italic") #'times 20 grey bold'


# Some stuff that apparently needs global definition
stdt=4
nstddev<-tclVar(stdt)
latentry=tclVar(''); lonentry<-tclVar('')
dayim <- as.integer(c(31,28,31,30,31,30,31,31,30,31,30,31))  # day # in a month
dayim2 <- as.integer(c(31,29,31,30,31,30,31,31,30,31,30,31))
first <- TRUE   # whether it's the first time this code is run.
flag <- F
temp.quantiles = c(0.05,0.1,0.5,0.9,0.95)
prec.quantiles = c(0.05,0.1,0.5,0.9,0.95)
barplot_flag    <- TRUE

##  initial value for parameters
stations<-tclVar(paste(" ")); stdt<-tclVar(paste("4"))
base.year.start.tcl<-tclVar(paste("1971"));base.year.end.tcl<-tclVar(paste("2000"))  # base period
latentry=tclVar(''); lonentry<-tclVar('')
#Entry3<-tclVar(paste("5"))
Entry4<-tclVar(paste("0"))
Entry5<-tclVar(paste("0"))
Entry6<-tclVar(paste("25"));Entry7<-tclVar(paste("0"))
Entry8<-tclVar(paste("20"));Entry9<-tclVar(paste("0"))
#Entry10<-tclVar(paste("10"));Entry11<-tclVar(paste("5"))
Entry12<-tclVar(paste("25"))
Entry13<-tclVar(paste("2")) # 
Entry14<-tclVar(paste("2"))# 
Entry15<-tclVar(paste("5"))# 
Entry16<-tclVar(paste("2"))# 
Entry17<-tclVar(paste("10"))# Rnnmm default
Entry20<-tclVar(paste("18"))#HY added for user-defined location-specific base temperature Tb HDDheat
Entry21<-tclVar(paste("18"))#HY added  for CDDcold
Entry22<-tclVar(paste("10"))#HY added  for GDDgrow

# Read in climate index data
        indexfile = "index.master.list"
        indexlist <- (read.table(indexfile,sep="\t"))
        indices = as.character(indexlist[,1])
	units <- as.character(indexlist[match(indices,indexlist[,1]),2])

# initial value for check box button
#cbvalue <- rep(as.character(tclVar(init=1)),length(indices)) ; tclvalue(cbvalue[]) <- 1
cbvalue<-c()
for(i in 1:100){
  aux<-tclVar(init=1)
  cbvalue<-c(cbvalue,as.character(aux))
  tclvalue(cbvalue[i])<-TRUE
}

# If user chooses "select ALL" option...
#selectAll<-function() cbvalue[] = TRUE
selectAll<-function() { for(i in 1:length(indices)) tclvalue(cbvalue[i])=TRUE }

# If user chooses "select NONE" option...
#selectNone<-function() cbvalue[] = FALSE
selectNone<-function() { for(i in 1:length(indices)) tclvalue(cbvalue[i])=FALSE }



  ########################################################################################
  #COMMENT FROM LISA
  #THE REVISED QC IS IN RCLIMDEX_EXTRAQC - I WILL SEND YOU THIS AND ENRIC CAN SUPPLY 
  #TECHNICAL ADVICE
  ########################################################################################
  # james: called as allqc(master = orig.name, output = outqcdir, outrange = crt) above
  ###########################################################################################
  ##    extraQC code, taken from the "rclimdex_extraqc.r" package, 
  ##     email from Lisa on 2013-07-19.
  ##  NOTE: this part outputs some results, but does not change the data.
  # Quality Control procedures programed by Enric Aguilar (C3, URV, Tarragona, Spain) and 
  # and Marc Prohom, (Servei Meteorologic de Catalunya)
  allqc <- function (master, output, outrange = 4)
  {       # outlier, default was 3.
    #tkmessageBox(message = "You are running the Extra QC Functions")
    #output<-paste(output,'/',filename,sep="")
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

  # =======================================================================
  # this one plots boxplots. Needs only station and save
  fourboxes <- function(station, output, save = 0, outrange)
  {
    # add save option
    if (save == 1)
    { 
      nombre <- paste(output, "_boxes.pdf", sep = "")
      pdf(file = nombre)
    }

    datos <- read.table(station, col.names = c("year", "month", "day", "pc", "tx", "tn"),
      na.strings = "-99.9")
    datos$tr <- datos$tx - datos$tn
    prec <- subset(datos, datos$pc > 0)
    par(mfrow = c(2, 2))

    # we open a file for writing outliers. First time is not append; rest is append
    filena <- paste(output, "_outliers.txt", sep = "")

    # for each of precip, tmax, tmin, dtr:
    #   produce boxplots: IQR for default is 3 for temp and 5 for precip
    #     can be entered as parameter when calling the function. Precip will always be 2 units more than temp
    #   write outliers out
    # if no data's available, 'no data available' is printed on a blank panel instead
    if (any(!is.na(prec$pc)))
    {
      respc <- boxplot(prec$pc ~ prec$month, main = "NON ZERO PREC", col = "blue", range = outrange + 2)

      # write precip outliers
      write.table("pc up", file = filena, append = FALSE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      for (a in 1:12)
      {
        prov <- subset(datos,datos$month == a & datos$pc > respc$stats[5, a])
        write.table(prov, file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
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
      write.table("tx up", file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      for (a in 1:12)
      {
        prov <- subset(datos, datos$month == a & datos$tx > restx$stats[5, a])
        write.table(prov, file= filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      }
      write.table("tx low", file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      for (a in 1:12)
      {
        prov <- subset(datos, datos$month == a & datos$tx < restx$stats[1, a])
        write.table(prov, file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
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
      write.table("tn up", file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      for (a in 1:12)
      {
        prov <- subset(datos, datos$month == a & datos$tn > restn$stats[5, a])
        write.table(prov, file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      }
      write.table("tn low", file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      for (a in 1:12)
      {
        prov <- subset(datos, datos$month == a & datos$tn < restn$stats[1, a])
        write.table(prov, file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
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
      write.table("tr up", file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      for (a in 1:12)
      {
        prov <- subset(datos, datos$month == a & datos$tr > restr$stats[5, a])
        write.table(prov, file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      }
      write.table("tr low", file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      for (a in 1:12)
      {
        prov <- subset(datos, datos$month == a & datos$tr < restr$stats[1, a])
        write.table(prov, file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
     }
    } else
    {
      plot.new()
      text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
    }

    if (save == 1) dev.off()

    rm(datos)  # we don't want to delete everything...
  }

  # =======================================================================
  # this one plots histograms showing rounding. Needs station and you can delimit the period with first and last year
  roundcheck <- function(station,output,fyear = 1000,lyear = 3000,save = 0)
  {
    if (save == 1)
    { 
      nombre<-paste(output,'_rounding.pdf',sep="")
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
    rm(datos) # we don't want to delete everyting...
  }

  # =======================================================================
  tmaxmin <- function(station,output)
  {
    filena = paste(output,'_tmaxmin.txt',sep='')
    datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
    write.table(subset(datos,(datos$tx-datos$tn)<=0),file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
    rm(datos)  # we don't want to delete everyting...
  }

  # =======================================================================
  humongous <- function(station,output)
  {
    filena = paste(output,'_toolarge.txt',sep='')
    datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
    grande<-subset(datos,(datos$tx > 50 | datos$tx < -50 | datos$tn > 50 | datos$tn < -50 | datos$pc > 200 | datos$pc < 0))
    write.table(grande,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
    rm(list=ls())
  }

  # =======================================================================
  boxseries <- function(station, output, save = 0)
  {
   if (save == 1)
    {
      nombre <- paste(output, "_boxseries.pdf", sep = "")
      pdf(file = nombre)
    }

    datos <- read.table(station, col.names = c("year", "month", "day", "pc", "tx", "tn"),
      na.strings = "-99.9")
    datos$tr <- datos$tx - datos$tn
    prec <- subset(datos, datos$pc > 0)
    par(mfrow = c(2, 2))

    if (any(!is.na(prec$pc)))
      respc <- boxplot(prec$pc ~ prec$year, main = "NON ZERO PREC", col = "blue", range = 4) else
    {
      plot.new()
      text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
    }
    if (any(!is.na(datos$tx)))
      restx <- boxplot(datos$tx ~ datos$year, main = "TX", col = "red", range = 3) else
    {
      plot.new()
      text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
    }
    if (any(!is.na(datos$tn)))
      restn <- boxplot(datos$tn ~ datos$year, main = "TN", col = "cyan", range = 3) else
    {
      plot.new()
      text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
    }
    if (any(!is.na(datos$tr)))
      restr <- boxplot(datos$tr ~ datos$year, col = "yellow", main = "DTR", range = 3) else
    {
      plot.new()
      text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
    }

    if (save == 1) dev.off()

    rm(datos)  # we don't want to delete everyting...
  }

  # =======================================================================
  duplivals <- function(station,output)
  {
    filena = paste(output,'_duplicates.txt',sep='')
    datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
    isdupli<-cbind(datos$year,datos$month,datos$day)
    write.table(subset(isdupli, duplicated(isdupli)== TRUE),file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
    rm(datos)  # we don't want to delete everyting...
  }

  # =======================================================================
  jumps_tx <- function(station, output)
  {
    filena = paste(output, '_tx_jumps.txt',sep='')
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
    write.table(jumps,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
    rm(datos)  # we don't want to delete everyting...
  }

  # =======================================================================
  jumps_tn <- function(station,output)
  {
    filena = paste(output, '_tn_jumps.txt',sep='')
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
    write.table(jumps,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
    rm(datos)  # we don't want to delete everyting...
  }

  # =======================================================================
  flatline_tx <- function(station,output)
  {
    filena = paste(output, '_tx_flatline.txt', sep='')
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
    write.table(flat,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
    rm(datos)  # we don't want to delete everyting...
  }

  # =======================================================================
  flatline_tn <- function(station,output)
  {
    filena = paste(output, '_tn_flatline.txt', sep='')
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
    write.table(flat,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
    rm(datos)  # we don't want to delete everyting...
  }

  # End of Prohom and Aguilar code.
  ##################################################################################################

#==============================================
# function pplotts
# plot QC'ed data (TX, TN, PR) into pdf files.
#==============================================
pplotts <- function(var = "prcp", type = "h", tit = NULL)
{
	# set bounds for the plot based on available data. dtr and prcp have
	# floors of 0 by definition (assuming tmax and tmin have been qc'd)
	
	#NICK: change this to use climdex input object data.
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
	
	# set default y scales if proper ones can't be calculated
	# but do we really want to try to plot if there's no data available at all?
	if (is.na(ymax) | is.na(ymin) | (ymax == -Inf) | (ymin == -Inf))
	{
	  ymax <- 100
	  ymin <- -100
	  warning(paste("Warnings have been generated because there is no available data for",
	  "one or more of tmax, tmin or non-zero precip. Check the plots in /log to confirm this."))
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
	      ttmp <- cio@data[[var]][tmp.dates>=i & tmp.dates <= min(i + 9, yeare)]
	  plot(1:length(ttmp), ttmp, type = type, col = "blue",
	    xlab = "", ylab = "", xaxt = "n", xlim = c(1, 3660), ylim = c(ymin, ymax))
	  abline(h = 0)
	  tt <- seq(1, length(ttmp))
	  if(!is.null(ttmp)) tt <- tt[is.na(ttmp) == TRUE] else print(paste(var,"is null"))
	  axis(side = 1, at = at, labels = c(i:(i + 9)))
	  for(k in 1:10) abline(v = at[k], col = "yellow")
	  lines(tt, rep(0, length(tt)), type = "p", col = "red")
	  title(paste("Station: ", tit, ", ", i, "~", min(i + 9, yeare), ",  ", var, sep = ""))
	}
}
# end of pplotts.

# getfile0
load.data.qc <- function() {
	# set up functions inside load.data.qc
	cancel1 <- function() {
		ok.button<-tkbutton(base,text='       Ok        ',command=ok1,bg='white')
		tkconfigure(ok.button,text='               ',bg='white')  # Users choose to cancel the operation.
		tkdestroy(infor1)
		tkfocus(start1) }

	# GET RID OF THIS FUNCTION> DONE AUTOMATICALLY WITH CLIMDEX.PCIC	
	# users want to calculate new thres data.
	calc.threshold <- function() {
		get_base()            # get base period
		if(tclvalue(fail_base)=='1') { print("RETURN") ; return() }
		nordaytem1(opt=0)   # get threshold and do bootstrap
		if(flag==T) tkconfigure(msg,text='More than 25% temperature data (base period) missing, related indices will NOT be calculated !!',font=font_err)
		tkfocus(infor1)    # done (whether successful or not), so return to previous window, users can press "continue" now.
		thres.calc <<- TRUE #tclvalue(thres.yes)=T
		tkconfigure(ok.button,text='   Continue    ',bg='white',font=font_small) } # end of computa()
	
	# users want to use the threshold data in memory from previous run.
#	prev.threshold <- function() {
#		if(flag==T) {          # no thres data in memory from previous run, so you can not choose this option.
#		  tkmessageBox(message='you can NOT choose this option - NO previous threshold!',ico='warning')
#		  tkfocus(infor1)
#		  return() }
#		
#		get_base()           # get base period
#		if(tclvalue(fail_base)=='1') return()
#		nordaytem1(opt=1)    # do bootstrap, not calculate threshold.
#		tkfocus(infor1)      # return to previous window. Users can press "continue" now.
#		tclvalue(thres.yes)=T
#		tkconfigure(ok.button,text='   Continue    ',bg='white',font=font_small) } # end of previous
	
	help.thres <- function() {
		tkmessageBox(title='ClimPACT - help with threshold',message='You can choose to read threshold data "*_thres.csv" file from computer; 
		\nor to calculate a new threshold data and save it.
		\nor use threshold data in memory from most recent run (If it"s the first run or previous threshold data does not exit, you can not choose this option).
		\n\nYou should be careful when choosing it - make sure the base periods are the same !'
		,icon='question') }
	
	# Load data
	load.data <- function() {
		print("Loading climdex input object...")
	# create a PCICt object for dates
		yyymmdd <- paste(data[,1],data[,2],data[,3],sep="-")
		dates <- as.Date(yyymmdd,format="%Y-%m-%d")
		days <- dates-as.Date("1850-01-01") #dates[1] # days since first date, needed for PCICt
		seconds <- as.numeric(days*24*60*60)
		pcict.dates <- as.PCICt(seconds,cal="gregorian",origin=as.character("1850-01-01"))      # pass each date as number of seconds since the first date (which is used as the origin)
		assign('pcict.dates',pcict.dates,envir=.GlobalEnv)
		date.months <- unique(format(as.Date(yyymmdd),format="%Y-%m"))
		date.years <- unique(format(as.Date(yyymmdd),format="%Y"))
		assign('date.months',date.months,envir=.GlobalEnv)
                assign('date.years',date.years,envir=.GlobalEnv)

	# create a climdex input object
		cio <- climdexInput.raw(tmin=data[,6],tmax=data[,5],prec=data[,4],tmin.dates=pcict.dates,tmax.dates=pcict.dates,prec.dates=pcict.dates,base.range=c(base.year.start,base.year.end),prec.qtiles=prec.quantiles,
			temp.qtiles=temp.quantiles)
		assign('cio',cio,envir=.GlobalEnv)
                cio@data$dtr <<- cio@data$tmax - cio@data$tmin
print(str(cio))
	# write quantiles to file (we're not asking the users permission...)
		nam1 <- paste(outthresdir, paste(ofilename, "_thres.csv", sep = ""), sep = "/")
		thres <- c(cio@quantiles$tmax$outbase,cio@quantiles$tmin$outbase,cio@quantiles$prec)
		write.table(as.data.frame(thres), file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "-99.9", col.names = c(paste("tmax",names(cio@quantiles$tmax$outbase)),paste("tmin",names(cio@quantiles$tmin$outbase)),
			paste("prec",names(cio@quantiles$prec))))

	# thresholds are automatically calculated when creating a climdex input object, so set this flag to TRUE
                thres.calc <<- TRUE #tclvalue(thres.yes)=1
	}
	
	# Leave the QC screen to go back to main menu
	ok1 <- function() {
		# Users want to press "continue".
		if(!thres.calc) #tclvalue(thres.yes)=='0')
		{
		  # You must do something first - QC.
		 tkmessageBox(message='You must load and QC the data before continuing.',icon='warning')
		 return()
		}
		first=F; assign('first',first,envir=.GlobalEnv)
#		tkconfigure(ok.button,text='               ',bg='white')   # There's no "continue" button. 
#		if(!exists("cio")) { load.data() }
		tkdestroy(infor1)
		tkfocus(start1) 
	}
	
	# this function gets user-defined base period, and check if they're valid input.
	get_base <- function() {
		if(tclvalue(qc.yes)=='0') {
			tkmessageBox(message='You must load some data before loading thresholds.',icon='warning')
			tkfocus(infor1)
			tclvalue(fail_base)=T
			return()
		}
		base.year.start<-as.numeric(tclvalue(base.year.start.tcl));  assign("base.year.start",base.year.start,envir=.GlobalEnv)
		base.year.end<-as.numeric(tclvalue(base.year.end.tcl));    assign("base.year.end",base.year.end,envir=.GlobalEnv)
		
		if(base.year.start<=years | base.year.end>=yeare | base.year.start>=base.year.end) {
			tkmessageBox(title='ClimPACT - warning for base period',message=paste('base period is outside of valid years in data [',years,', ',yeare,'],\nor from_year is bigger than to_year!',sep=''),icon='warning')
			tkfocus(infor1)
			return()
		}
		tclvalue(fail_base)=F 
	} # end of get_base()
	
	# users want to open an existing *_thres.csv file.
	read.threshold <- function() {
		get_base()           # get base period.
		if(tclvalue(fail_base)=='1') return()
		name <- tclvalue(tkgetOpenFile(filetypes="{{csv Files} {.csv}} {{All files} *}",initialdir=outthresdir)) # choose thres file.
		if (name=="")return()    # users change mind, don't want to open existing file, so return to previous window, and choose another option.

		# read in previously written thresholds and overwrite/append cio object
                print("READING THRESHOLDS...")
		prev.qtiles <- read.csv(name,header=T,sep=',')   # read in thres data.
		cio@quantiles$tmax$outbase$q95 <<- prev.qtiles$tmax.q95 ; cio@quantiles$tmax$outbase$q90 <<- prev.qtiles$tmax.q90 ; cio@quantiles$tmax$outbase$q50 <<- prev.qtiles$tmax.q50 ; 
								cio@quantiles$tmax$outbase$q10 <<- prev.qtiles$tmax.q10 ; cio@quantiles$tmax$outbase$q10 <<- prev.qtiles$tmax.q10
                cio@quantiles$tmin$outbase$q95 <<- prev.qtiles$tmin.q95 ; cio@quantiles$tmin$outbase$q90 <<- prev.qtiles$tmin.q90 ; cio@quantiles$tmin$outbase$q50 <<- prev.qtiles$tmin.q50 ; 
                                                                cio@quantiles$tmin$outbase$q10 <<- prev.qtiles$tmin.q10 ; cio@quantiles$tmin$outbase$q10 <<- prev.qtiles$tmin.q10
                cio@quantiles$tmax$q95 <<- prev.qtiles$tmax.q95 ; cio@quantiles$tmax$q90 <<- prev.qtiles$tmax.q90 ; cio@quantiles$tmax$q50 <<- prev.qtiles$tmax.q50 ; 
                                                                cio@quantiles$tmax$q10 <<- prev.qtiles$tmax.q10 ; cio@quantiles$tmax$q10 <<- prev.qtiles$tmax.q10

		tkfocus(infor1)       # return to current window, and users can press "continue" now.
		tkconfigure(msg,text='',font=font_small)
		thres.calc <<- TRUE #tclvalue(thres.yes)=T
		tkconfigure(ok.button,text='   Continue    ',bg='white',font=font_small)
		print("COMPLETED READING THRESHOLDS.")
	} # end of reada()
	
	#==============================================
	# function qcontrol
	# QC the data with simple methods, then save the tmax, tmin, prcp plots.
	# the QC'ed data were saved in a _indcal.csv file for later use
	#     - users should modify it if there's error, then code will open this file and read in the data assmuing it's corrected.
	#==============================================
	qcontrol <- function() {
		# source climpact code and load data from ascii file into climdex object
		source("climpact2_1.3.par.r")
                latitude  <- as.numeric(tclvalue(latentry))   # get user-input parameter, and check if they're valid.
                longitude <- as.numeric(tclvalue(lonentry))
                ofilename <- tclvalue(station.entry)
                stddev.crit <- as.numeric(tclvalue(nstddev))

                if (is.na(latitude)  == TRUE | is.na(longitude) == TRUE | latitude < -90 | latitude > 90 | longitude > 180 | longitude < -180) {
                  tkmessageBox(message = paste("Please enter a valid latitude and longitude.",sep = ""))
                  return() }

	# NICK: After this point all references to data should be made to the climdex input object 'cio'
		load.data()
		
		flag <- FALSE    # indicator whether threshold is successful.
		assign("flag", flag, envir = .GlobalEnv)
		
		# output records of problematic like prcp <0 and NA
#		tkconfigure(ok.button, text = "               ")
		
		assign("latitude",  latitude, envir = .GlobalEnv)
		assign("longitude", longitude, envir = .GlobalEnv)
		title.station <- paste(ofilename, " [", latitude, ", ", longitude, "]", sep = "")
		assign("title.station", title.station, envir = .GlobalEnv)
		assign("ofilename", ofilename, envir = .GlobalEnv)
		
		# STEP 1.
		# determine which dates are duplicates and write them to file.
		ddu <- duplicated(data[, c("year", "month", "day")])
		if(sum(ddu) > 0) {
		  nam1 <- paste(outlogdir, paste(ofilename, "dupliQC.csv", sep = ""), sep = "/")
		  msg = paste("Date duplicated found in original data file, please check:", nam1, sep = " ")
		  tkmessageBox(message = msg)
		  ddu2 <- data[duplicated(data[, c("year", "month", "day")]) == TRUE, c("year", "month", "day")]
		  nam1 <- paste(outlogdir, paste(ofilename, "dupliQC.csv", sep = ""), sep = "/")
		  write.table(ddu2, file = nam1, append = FALSE, quote = FALSE, sep = ", ", row.names = FALSE)
		  tkdestroy(start1)
		  stop(paste("QC stopped due to duplicated date, please check ", nam1, sep = ""))
		}
		
		# STEP 2.
		# search for precip < 0 and write to file. 
		mid <- cio@data$prec[!is.na(cio@data$prec)]
		mid <- mid[mid < 0]
		nam1 <- paste(outlogdir, paste(ofilename, "_prcpQC.csv", sep = ""), sep = "/")
		write.table(mid, file = nam1, append = FALSE, quote = FALSE, sep = ", ", row.names = FALSE)
print(length(mid))
		if (length(mid) > 0) tkconfigure(err1, text = "!!!! error in PRCP found !!!!", font = font_err)

		# STEP 3.
		# output plots for tmin, tmax, prcp and dtr
		nam1 <- paste(outlogdir, paste(ofilename, "_prcpPLOT.pdf", sep = ""), sep = "/")
		pdf(file = nam1)
		
		prcp <- cio@data$prcp[cio@data$prcp >= 1]
#		prcp <- prcp[!is.na(prcp)]	# combine these two lines
		if(length(prcp) > 30)
		{
		  hist(prcp, main = paste("Histogram for Station:", ofilename, " of PRCP>=1mm", sep = ""),
		    breaks = c(seq(0, 20, 2), max(30, ttmp)), xlab = "", col = "green" , freq = FALSE)
		  lines(density(prcp, bw = 0.2, from = 1), col = "red")
		}
		pplotts(var = "prcp", tit = ofilename)
		dev.off()
		nam1 <- paste(outlogdir, paste(ofilename, "_tmaxPLOT.pdf", sep = ""), sep = "/")
		pdf(file = nam1)
		pplotts(var = "tmax", type = "l", tit = ofilename)
		dev.off()
		nam1 <- paste(outlogdir, paste(ofilename, "_tminPLOT.pdf", sep = ""), sep = "/")
		pdf(file = nam1)
		pplotts(var = "tmin", type = "l", tit = ofilename)
		dev.off()
		nam1 <- paste(outlogdir, paste(ofilename, "_dtrPLOT.pdf", sep = ""), sep = "/")
		pdf(file = nam1)
		pplotts(var = "dtr", type = "l", tit = ofilename)
		dev.off()
		
		# STEP 4.
		# Find where tmax < tmin or where either are >/< 70 degC, then write to file and set to NA.
		dtr <- data[, "tmax"] - data[, "tmin"]
		data <- cbind(data, dtr)
		dimnames(data)[[2]][7] <- "dtr"
		temiss <- data
		temiss <- temiss[is.na(temiss[, "tmax"]) == FALSE & is.na(temiss[, "tmin"]) == FALSE, ]
		temiss <- temiss[temiss[, 7] <= 0 |
		                 temiss[, 5] <= -70 |
		                 temiss[, 5] >= 70 |
		                 temiss[, 6] <= -70 |
		                 temiss[, 6] >= 70, ]
		dimnames(temiss)[[2]][7] <- "tmax-tmin"
		nam1 <- paste(outlogdir, paste(ofilename, "_tempQC.csv", sep = ""), sep = "/")
		write.table(temiss, file = nam1, append = FALSE, quote = FALSE, sep = ", ", row.names = FALSE)
		if (dim(temiss)[1] > 0)
		{
		  tkconfigure(err2, text ='!!!! error in temperature found !!!!' , font = font_err)
		  # records with abs(tmax)>=70, abs(tmin)>=70 set to NA
		  data[is.na(data[, 5]) == FALSE & abs(data[, 5]) >= 70, 5] <- NA
		  data[is.na(data[, 6]) == FALSE & abs(data[, 6]) >= 70, 6] <- NA
		  # records with tmax < tmin are set to NA
		  data[is.na(data[, 5]) == FALSE & is.na(data[, 6]) == FALSE & data[, "dtr"] < 0, c("tmax", "tmin")] <- NA
		}
		
		# NICK: re-write everything below in this function.

		# CHECK FOR TEMPERATURES OUTSIDE OF A SET NUMBER OF STDDEV'S.
		### We don't need to worry about leap years as climdex simply makes any values on the 29th feb = NA
		print("CHECKING FOR TEMPERATURE OUTLIERS.")
	
		# find stddev
		all.day.factors <- factor(format(cio@dates, format="%Y-%m-%d", tz="GMT")) # 38351 levels
		day.factors <- factor(format(cio@dates, format="%m-%d", tz="GMT"))	  # 366 levels
		
		# get day factors excluding leap years
		noleap.dates <- cio@dates[format(cio@dates, format="%m-%d", tz="GMT")!="02-29"]
		noleap.day.factors <- factor(format(noleap.dates, format="%m-%d", tz="GMT"))

		print("calculating means and standard deviations...")
		tmax.mean <- tapply(cio@data$tmax,day.factors,mean,na.rm=TRUE)
		tmax.stddev <- sqrt(tapply(cio@data$tmax,day.factors,var,na.rm=TRUE))
		tmin.mean <- tapply(cio@data$tmin,day.factors,mean,na.rm=TRUE)
		tmin.stddev <- sqrt(tapply(cio@data$tmin,day.factors,var,na.rm=TRUE))
		rm(dtr)		# need to redefine dtr using climdex object tmin and tmax since they have additional NA values at the end to make up the remainder of the last year.
#		cio@data$dtr <<- cio@data$tmax - cio@data$tmin
		dtr.mean <- tapply(cio@data$dtr,day.factors,mean,na.rm=TRUE)
		dtr.stddev <- sqrt(tapply(cio@data$dtr,day.factors,var,na.rm=TRUE))
		#ys <- yeare - years + 1

		print("testing data... please wait")
		tmax.outliers <- tapply(1:length(cio@data$tmax),all.day.factors,function(idx) {
			if(!is.na(cio@data$tmax[idx])) { month.day <- format(as.Date(all.day.factors[idx]),format="%m-%d")
				if(abs(cio@data$tmax[idx] - tmax.mean[month.day]) > (stddev.crit*tmax.stddev[month.day])) { return(TRUE) } else { return(FALSE) } }
			else { return(FALSE) } } )
		
		tmin.outliers <- tapply(1:length(cio@data$tmin),all.day.factors,function(idx) {
		        if(!is.na(cio@data$tmin[idx])) { month.day <- format(as.Date(all.day.factors[idx]),format="%m-%d")
		                if(abs(cio@data$tmin[idx] - tmin.mean[month.day]) > (stddev.crit*tmin.stddev[month.day])) { return(TRUE) } else { return(FALSE) } }
		        else { return(FALSE) } } )
		dtr.outliers <- tapply(1:length(cio@data$dtr),all.day.factors,function(idx) {
		        if(!is.na(cio@data$dtr[idx])) { month.day <- format(as.Date(all.day.factors[idx]),format="%m-%d")
		                if(abs(cio@data$dtr[idx] - dtr.mean[month.day]) > (stddev.crit*dtr.stddev[month.day])) { return(TRUE) } else { return(FALSE) } }
		        else { return(FALSE) } } )

		print(length(tmax.outliers))
		print(length(tmax.outliers[tmax.outliers=="FALSE"]))
		print(length(tmax.outliers[tmax.outliers=="TRUE"]))
		
		# If outliers are found above, write out corresponding dates that have the suspect data.
		if (any(tmax.outliers==TRUE) | any(tmin.outliers==TRUE) | any(dtr.outliers==TRUE))
		{	# this segment needs rewriting. Need to output a .csv...
			nam1 <- paste(outlogdir, paste(ofilename, "_tepstdQC.csv", sep = ""), sep = "/")
			tkconfigure(err4, text = paste("!!!! Outliers were found, please check\n", nam1, sep = ""), font = font_err)
			idx <- which(tmax.outliers==TRUE | tmin.outliers==TRUE | dtr.outliers==TRUE)
			ofile <- cbind(as.character(cio@dates[idx]),cio@data$tmax[idx],cio@data$tmin[idx],cio@data$dtr[idx])	# TODO: write out stddev's as well?
			write.table(ofile, file = nam1, append = FALSE, quote = FALSE, sep = ",", row.names = FALSE,col.names=c(" DATE"," TMAX"," TMIN"," DTR"))
		}
		
		data <- data[, c("year", "month", "day", "prcp", "tmax", "tmin")]
		assign("data", data, envir = .GlobalEnv)
		
		namcal <- paste(nama, "_indcal.csv", sep = "")  # User should change this file if error was reported because it will be used for all calculation.
		assign("namcal", namcal, envir = .GlobalEnv)
		write.table(data, file = namcal, append = FALSE, quote = FALSE, sep = ",", row.names = FALSE, na = "-99.9")
		allqc(master = orig.name, output = outqcdir, outrange = stddev.crit)   # extraQC is called here. NOTE the default outrange=3 in original verson.
		
		tclvalue(qc.yes) <- TRUE  # the QC step is done, so you can continue...
		print("COMPLETED CHECKING FOR TEMPERATURE OUTLIERS.")
	} # end of qcontrol()
	
	create.dir <- function() {
		# create directory names
		if(length(outdirtmp)<=2) {
			outinddir<-paste(strsplit(dir.file.name,":")[[1]][1],"indices",sep=":/")
			outlogdir<-paste(strsplit(dir.file.name,":")[[1]][1],"log",sep=":/")
			outjpgdir<-paste(strsplit(dir.file.name,":")[[1]][1],"plots",sep=":/")
			outtrddir<-paste(strsplit(dir.file.name,":")[[1]][1],"trend",sep=":/")
			outthresdir<-paste(strsplit(dir.file.name,":")[[1]][1],"thres",sep=":/")  # to save *_thres.csv files   
			outqcdir<-paste(strsplit(dir.file.name,":")[[1]][1],"extraqc",sep=":/")   # save results from extraqc
		} else{
			outdir<-outdirtmp[1]
			for(i in 2:(length(outdirtmp)-1))
			outdir<-paste(outdir,outdirtmp[i],sep="/")
			outinddir<-paste(outdir,"indices",sep="/")
			outlogdir<-paste(outdir,"log",sep="/")
			outjpgdir<-paste(outdir,"plots",sep="/")
			outtrddir<-paste(outdir,"trend",sep="/")
			outqcdir<-paste(outdir,"extraqc",sep="/")    # save results from extraqc
			outthresdir<-paste(outdir,"thres",sep="/")   # to save *_thres.csv files 
		}
	
		file.name=outdirtmp[length(outdirtmp)]
		e=strsplit(file.name,"\\.")[[1]]
		ofilename=substr(file.name,start=1,stop=nchar(file.name)-nchar(e[length(e)])-1)
		assign('ofilename',ofilename,envir=.GlobalEnv)
		
		# Create subdirectories if non-existent
		if(!file.exists(outinddir)) dir.create(outinddir)
		if(!file.exists(outlogdir)) dir.create(outlogdir)
		if(!file.exists(outjpgdir)) dir.create(outjpgdir)
		if(!file.exists(outtrddir)) dir.create(outtrddir)
		if(!file.exists(outqcdir))  dir.create(outqcdir)
		if(!file.exists(outthresdir)) dir.create(outthresdir)
		
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

	qc.yes <- tclVar(FALSE)   # QC has not been done yet.
	thres.calc <<- FALSE #thres.yes <- tclVar(FALSE) 
	thres.calc <<- FALSE #tclvalue(thres.yes) <- FALSE

# get a file from user
	dir.file.name <- tclvalue(tkgetOpenFile(filetypes="{{TEXT Files} {.txt}} {{All files} *}"))
	if (dir.file.name=="") { no_file=T; assign('no_file',no_file,envir=.GlobalEnv); return(); tkfocus(start1) }
	nama<-substr(dir.file.name,start=1,stop=(nchar(dir.file.name)-4))
	assign('orig.name',dir.file.name,envir=.GlobalEnv)

# read in data from file
	data <- read.table(dir.file.name,header=F,col.names=c("year","month","day","prcp","tmax","tmin"),colClasses=rep("real",6))
	outdirtmp<-strsplit(dir.file.name,"/")[[1]]
	assign("data",data,envir=.GlobalEnv)

# create directories
	create.dir()

# replace missing values (-99.9) with NA
	data[data$prcp==(-99.9),"prcp"]<-NA ; data[data$tmax==(-99.9),"tmax"]<-NA ; data[data$tmin==(-99.9),"tmin"]<-NA

# STUFF NOT INCLUDED HERE FROM CLIMPACT
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
	station.entry <- tclVar(ofilename)
	
	# Draw interface
	# enter station name and the times of standard deviation
	infor1 <- tktoplevel(bg = "white")
	tkfocus(infor1)
	tkgrab.set(infor1)
	tkwm.geometry(infor1, "+0+0") # position in upper left corner of screen
	tkwm.title(infor1, "ClimPACT - Data QC and threshold")
	
	station.entry <- tclVar(ofilename)
	base <- tkframe(infor1, bg = "white")
	 
	tt1 <- tkframe(base, bg = "white")
	tkpack(tklabel(tt1, text = paste("the data file is:", orig.name, "\n"), bg = "white", font = font_small))
	tkpack(tklabel(tt1, text = "Please enter station name:", bg = "white", font = font_small), side = "left")
	textEntryWidget1 <- tkentry(tt1, width = 30, textvariable = station.entry, bg = "white")
	tkpack(textEntryWidget1, side = "left")
	
	tt2 <- tkframe(base)
	LatEntry <- tkentry(tt2, width = 10, textvariable = latentry, bg = "white")
	LonEntry <- tkentry(tt2, width = 10, textvariable = lonentry, bg = "white")
	tkpack(tklabel(tt2, text = "    Latitude:", bg = "white", font = font_small), side = "left")
	tkpack(LatEntry, side = "left")
	tkpack(tklabel(tt2, text = "   Longitude:", bg = "white", font = font_small), side = "left")
	tkpack(LonEntry, side = "left")
	
	tt3 <- tkframe(base)
	textEntryWidget2 <- tkentry(tt3, width = 10, textvariable = nstddev, bg = "white")
	tkpack(tklabel(tt3, text = "Criteria(number of Standard Deviation):", bg = "white", font = font_small), side = "left")
	tkpack(textEntryWidget2, side = "left")
	tkpack(tt1, tt2, tt3)
	
	    tt1 <- tkframe(base, bg = "white")
	
	err1 <- tklabel(tt1, text = err10, font = font_small, bg = "white")
	err2 <- tklabel(tt1, text = err20, font = font_small, bg = "white")
	err4 <- tklabel(tt1, text = err40, font = font_small, bg = "white")
	
	tkpack(tkbutton(tt1, text = "  Load and QC the data >> ",command = qcontrol, font = fontHeading2, bg = "white"))  # original verson
	tkpack(err1,side='top')
	tkpack(err2,side='top')
	tkpack(err4,side='top')
	tkpack(tt1)
	
	    tt1=tkframe(base,bg='white')
	tkpack(tklabel(tt1,text='',bg='white'),side='top')
	lab1<-tklabel(tt1,text='Enter base period: from',bg='white',font=font_small); lab2<-tklabel(tt1,text=' to ',bg='white',font=font_small)
	enter1<-tkentry(tt1,width=15,textvariable=base.year.start.tcl,bg='white')
	enter2<-tkentry(tt1,width=15,textvariable=base.year.end.tcl,bg='white')
	tkpack(lab1,side='left'); tkpack(enter1,side='left'); tkpack(lab2,side='left'); tkpack(enter2,side='right')
	tkpack(tt1)
	tkpack(base,side='top')
	base.year.start<-as.numeric(tclvalue(base.year.start.tcl));  assign("base.year.start",base.year.start,envir=.GlobalEnv)
	base.year.end<-as.numeric(tclvalue(base.year.end.tcl));    assign("base.year.end",base.year.end,envir=.GlobalEnv)
	
	base<-tkframe(infor1,bg='white')
	tt1=tkframe(infor1,bg='white')
	
#	tkpack(tklabel(tt1,text='Choose one option for threshold:',font=fontHeading2,bg='white'),side='top') # totally 3 options.
	tkpack(tklabel(tt1,text='',font=fontHeading2,bg='white'),side='top')
	t1=tkbutton(tt1,text='load threshold file (*_thres.csv)',command=read.threshold,bg='white',font=fontTextLabel)
#	t2=tkbutton(tt1,text='calculate new threshold',command=calc.threshold,bg='white',font=font_small)
#	t3=tkbutton(tt1,text='use threshold from previous run',command=prev.threshold,bg='white',font=font_small)
	
#	if(first | flag==T)
#	{
#	  # If the code is run for first time, or previous run does NOT have valid threshold, only 2 options available.
	  tkpack(t1,side='bottom');#  tkpack(t2,side='left')
#	} else
#	{
#	  # Now you have three options available.
#	  tkpack(t1,side='left');  tkpack(t2,side='left');#  tkpack(t3,side='right')
#	}
	
	tkpack(tt1,side='top')
	tkpack(base,side='top')
	base<-tkframe(infor1,bg='white')
	msg=tklabel(base,text=thres_err,bg='white')
	tkpack(msg,side='top')
	tkpack(base,side='top')
	
	base=tkframe(infor1,bg='white')
	
	ok.button<-tkbutton(base,text='       Ok        ',command=ok1,bg='white',font=font_small)
#	tkpack(tklabel(base,text='',bg='white'))
	tkpack(ok.button,side='left')
	tkpack(tkbutton(base,text='     Cancel   ',command=cancel1,bg='white',font=font_small),side='left')
	tkpack(base)
} # END OF load.data.qc



#==============================================
# function leapyear
# return True (T) if leapyear, esle F
#==============================================
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

# nastat
index.calc1 <- function() {
#	if(exists("namcal") == FALSE) {
#		tkmessageBox(message = "You must run < Load Data and Run QC > first !",
#		icon = "warning", title = "ClimPACT - warning")
#		tkfocus(start1)
#		return()
#	}

	infor <- tktoplevel(bg = "white")
	tkfocus(infor)
	tkgrab.set(infor)
	tkwm.geometry(infor, "+0+0") # position in upper left corner of screen
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
	
	textEntryWidget6  <- tkentry(infor, width = 20, textvariable = textEntry6)
	textEntryWidget7  <- tkentry(infor, width = 20, textvariable = textEntry7)
	textEntryWidget8  <- tkentry(infor, width = 20, textvariable = textEntry8)
	textEntryWidget9  <- tkentry(infor, width = 20, textvariable = textEntry9)
	textEntryWidget12 <- tkentry(infor, width = 20, textvariable = textEntry12)
	
	textEntryWidget13 <- tkentry(infor, width = 20, textvariable = textEntry13) # WSDI
	textEntryWidget14 <- tkentry(infor, width = 20, textvariable = textEntry14) # CSDI
	textEntryWidget15 <- tkentry(infor, width = 20, textvariable = textEntry15) # RX
	
	textEntryWidget16 <- tkentry(infor, width = 20, textvariable = textEntry16) # TXTN
	textEntryWidget20 <-tkentry(infor, width = 20, textvariable = textEntry20) #Tb for HDDheat
	textEntryWidget21 <-tkentry(infor, width = 20, textvariable = textEntry21) #Tb for CDDcold
	textEntryWidget22 <-tkentry(infor, width = 20, textvariable = textEntry22) #Tb for GDDgrow
	
	tkpack(tklabel(infor, text = "User defined parameters for Indices Calculation", font = fontHeading1, bg = "white"), side = "top")
	
	help.title<-function(){    # tip for the title in all plots.
		tkmessageBox(message=paste('# = station name, \n* = index name.\n e.g. If you input "station #, index *"\nYou can get \nstation ',title.station,', index TXx',sep=''),icon='question')
	}
	
	tt1=tkframe(infor,bg='white')   # add a "?" to the current window.
	textEntry3<-tclVar('station: #, index: *')
	textEntryWidget3<-tkentry(tt1,width=30,textvariable=textEntry3,bg='white')
	help1=tkbutton(tt1,text=' ? ',command=help.title,bg='white')
	
	# User defined title for plotting
	tkpack(tklabel(tt1,text='User defined title for plotting:',bg='white',font=font_small),side='left')
	tkpack(textEntryWidget3,side='left')
	tkpack(tklabel(tt1,text='    ',bg='white'),side='left')
	tkpack(help1,side='right')
	tkpack(tt1)
	
	tt1<-tkframe(infor,bg='white')

	textEntryWidget6<-tkentry(tt1,width=20,textvariable=textEntry6,bg='white')
	textEntryWidget7<-tkentry(tt1,width=20,textvariable=textEntry7,bg='white')
	textEntryWidget8<-tkentry(tt1,width=20,textvariable=textEntry8,bg='white')
	textEntryWidget9<-tkentry(tt1,width=20,textvariable=textEntry9,bg='white')
	textEntryWidget12<-tkentry(tt1,width=20,textvariable=textEntry12,bg='white')
	
	textEntryWidget13<-tkentry(tt1,width=20,textvariable=textEntry13,bg='white') # WSDI
	textEntryWidget14<-tkentry(tt1,width=20,textvariable=textEntry14,bg='white') # CSDI
	textEntryWidget15<-tkentry(tt1,width=20,textvariable=textEntry15,bg='white') # RX
	textEntryWidget16<-tkentry(tt1,width=20,textvariable=textEntry16,bg='white') # TXTN
	textEntryWidget20<-tkentry(tt1,width=20,textvariable=textEntry20,bg='white') # Tb for HDDheat
	textEntryWidget21<-tkentry(tt1,width=20,textvariable=textEntry21,bg='white') # Tb for CDDcold
	textEntryWidget22<-tkentry(tt1,width=20,textvariable=textEntry22,bg='white') # Tb for GDDgrow
        textEntryWidget17<-tkentry(tt1,width=20,textvariable=textEntry17,bg='white') # Rnnmm

	#tt <- tktoplevel()
	rb1 <- tkradiobutton(tt1)
	rb2 <- tkradiobutton(tt1)
	rbValue <- tclVar("annual")
	tkconfigure(rb1,variable=rbValue,value="monthly",bg='white')
	tkconfigure(rb2,variable=rbValue,value="annual",bg='white')
	tkgrid(tklabel(tt1,text="Select frequency of output for relevant indices:",bg='white',font=font_small))
	tkgrid(tklabel(tt1,text="month ",bg='white',font=font_small),rb1)
	tkgrid(tklabel(tt1,text="annual ",bg='white',font=font_small),rb2)

	tkgrid(tklabel(tt1,text="User defined upper threshold of daily maximum temperature",bg='white',font=font_small),textEntryWidget6)
	tkgrid(tklabel(tt1,text="User defined lower threshold of daily maximum temperature",bg='white',font=font_small),textEntryWidget7)
	tkgrid(tklabel(tt1,text="User defined upper threshold of daily minimum temperature",bg='white',font=font_small),textEntryWidget8)
	tkgrid(tklabel(tt1,text="User defined lower threshold of daily minimum temperature",bg='white',font=font_small),textEntryWidget9)
	tkgrid(tklabel(tt1,text="User defined daily precipitation threshold",bg='white',font=font_small),textEntryWidget12)
	tkgrid(tklabel(tt1,text="User defined WSDI Flex Days",bg='white',font=font_small),textEntryWidget13) # 13 wsdi
	tkgrid(tklabel(tt1,text="User defined CSDI Flex Days",bg='white',font=font_small),textEntryWidget14) # 14 csdi
	tkgrid(tklabel(tt1,text="User defined RxnDay Flex Days",bg='white',font=font_small),textEntryWidget15) # 15 rxday
	tkgrid(tklabel(tt1,text="User defined n for nTXnTN and nTXbnTNb",bg='white',font=font_small),textEntryWidget16) # txtn
	tkgrid(tklabel(tt1,text="User defined base temperature for HDDheat",bg='white',font=font_small),textEntryWidget20) # Tb for HDDheat
	tkgrid(tklabel(tt1,text="User defined base temperature for CDDcold",bg='white',font=font_small),textEntryWidget21) # Tb for CDDcold
	tkgrid(tklabel(tt1,text="User defined base temperature for GDDgrow",bg='white',font=font_small),textEntryWidget22) # Tb for GDDgrow
        tkgrid(tklabel(tt1,text="User defined amount of precipitation (mm) for Rnnmm",bg='white',font=font_small),textEntryWidget17)
	
	tkpack(tt1)

	check.then.continue<-function(){   # get user-definded parameters, check if they're valid, and set as global variable.
		frequency <- as.character(tclvalue(rbValue)) ; assign("frequency",frequency,envir=.GlobalEnv)
		print(frequency)

		uuu<-as.numeric(tclvalue(textEntry6)); assign("uuu",uuu,envir=.GlobalEnv)
		ulu<-as.numeric(tclvalue(textEntry7)); assign("uul",ulu,envir=.GlobalEnv)
		uul<-as.numeric(tclvalue(textEntry8)); assign("ulu",uul,envir=.GlobalEnv)
		ull<-as.numeric(tclvalue(textEntry9)); assign("ull",ull,envir=.GlobalEnv)
		nn<-as.numeric(tclvalue(textEntry12)); 
		if(nn<0.){tkmessageBox(message='daily precipitation threshold WRONG!\n\nvalid range is [0, inf)',icon='warning');  return()}
		assign("nn",nn,envir=.GlobalEnv)
		
		ctmp<-as.character(tclvalue(textEntry3))
		plot.title<-gsub('\\#',title.station,ctmp); assign('plot.title',plot.title,envir=.GlobalEnv)
		
# Nick: Don't see the point of the following 5 lines, already assigned above...
#		Entry6<-textEntry6; assign("Entry6",Entry6,envir=.GlobalEnv)
#		Entry7<-textEntry7; assign("Entry7",Entry7,envir=.GlobalEnv)
#		Entry8<-textEntry8; assign("Entry8",Entry8,envir=.GlobalEnv)
#		Entry9<-textEntry9; assign("Entry9",Entry9,envir=.GlobalEnv)
#		Entry12<-textEntry12; assign("Entry12",Entry12,envir=.GlobalEnv)
		
		Entry13<-as.numeric(tclvalue(textEntry13)); assign("wsdi_ud",as.double(Entry13),envir=.GlobalEnv) # 13 wsdi wsdi_ud
		if(Entry13<2 | Entry13>10 ){tkmessageBox(message='WSDI Flex days WRONG!\n\nvalid range is [2, 10]',icon='warning');  return()}
		Entry14<-as.numeric(tclvalue(textEntry14)); assign("csdi_ud",as.double(Entry14),envir=.GlobalEnv)    # 14 csdi_ud
		if(Entry14<2 | Entry14>10 ){tkmessageBox(message='CSDI Flex days WRONG!\n\nvalid range is [2, 10]',icon='warning');  return()}
		
		Entry15<-as.numeric(tclvalue(textEntry15)); assign("rx_ud",as.double(Entry15),envir=.GlobalEnv)# 14 rx_ud
		if(Entry15<2 | Entry15>10 ){tkmessageBox(message='RxDay Flex days WRONG!\n\nvalid range is [2, 10]',icon='warning');  return()}
		Entry16<-as.numeric(tclvalue(textEntry16)); assign("txtn_ud",as.double(Entry16),envir=.GlobalEnv)# txtn_ud
		if(Entry16<2 | Entry16>10 ){tkmessageBox(message='n in nTXnTN and nTXbnTNb WRONG!\n\nvalid range is [2, 10]',icon='warning');  return()}
                Entry17<-as.numeric(tclvalue(textEntry17)); assign("rnnmm_ud",as.double(Entry17),envir=.GlobalEnv)# txtn_ud
                if(Entry17<0 ){tkmessageBox(message='n in Rnnmm WRONG!\n\nvalid range is [0,Inf)',icon='warning');  return()}

		Entry20<-as.numeric(tclvalue(textEntry20)); assign("Tb_HDD",as.double(Entry20),envir=.GlobalEnv) # Tb for HDDheat
		Entry21<-as.numeric(tclvalue(textEntry21)); assign("Tb_CDD",as.double(Entry21),envir=.GlobalEnv) # Tb for HDDcold
		Entry22<-as.numeric(tclvalue(textEntry22)); assign("Tb_GDD",as.double(Entry22),envir=.GlobalEnv) # Tb for HDDgrow

		tkgrab.release(infor);    tkdestroy(infor)
		index.calc2()
	}  # end of function check.then.continue
  
	cancel1<-function() {  # Users don't want to continue, so close this window and return to main window.
		tkdestroy(infor)
		return()
	}
  
	tt1<-tkframe(infor)
	ok1.but<-    tkbutton(tt1,text="    OK    ",command=check.then.continue,bg='white',font=font_small)
	cancel1.but<-tkbutton(tt1,text="  CANCEL  ",command=cancel1,bg='white',font=font_small)
	tkgrid(ok1.but,cancel1.but)
	tkpack(tt1)
} # end of index.calc1

# done
done<-function(){tkdestroy(start1)}

# index.calc2
# Final index calculation window. User selects which indices to calculate. Index function calls come from this function.
index.calc2<-function(){
	main<-tktoplevel(bg='white')
	tkfocus(main)
	tkwm.geometry(main, "+0+0") # position in upper left corner of screen
	tkwm.title(main,"ClimPACT - Calculating indices")
	
	mainall<-tkframe(main,bg='white')
	main0<-tkframe(mainall,bg='white')
	tkgrid(tklabel(main0,text="Check desired indices",font=fontHeading1,bg='white'))
	tkgrid(main0)
	
	main0<-tkframe(mainall,bg='white')
	  
	check_all <- tkbutton(main0 ,  text = " select ALL ",  command = selectAll, width=15,bg='white',font=font_small)
	check_none <- tkbutton(main0 , text = " select NONE ", command = selectNone,width=15,bg='white',font=font_small)
	
	tkgrid(check_all,check_none)
	tkgrid(main0)
	
	tt=tkframe(mainall,bg='white')   # add scrollbar
	scr <- tkscrollbar(tt, command=function(...)tkyview(txt,...),bg='white')
	txt <- tktext(tt,height=20,bg='white') 
	tkconfigure(txt, yscrollcommand=function(...)tkset(scr,...))
	tkpack(scr,side='right',fill='y')
	tkpack(txt,expand='yes',fill='both')
	
	header= tclVar(TRUE)
	color1=tclVar(TRUE)
	tktag.configure(txt,header,font='times 16 bold')
	tktag.configure(txt,color1,foreground="red")
	font0='times 14'
	
	tkinsert(txt,'end','ET-CRSCI core set\n',header)  # group 1 indives

	# List index check boxes
	for (i in 1:length(indices)) {
	        check_button <- tkcheckbutton(txt, variable = cbvalue[i], text = paste(indices[i]," "),font=font0,bg='white')
        	tkinsert(txt,'end','\n    '); tkwindow.create(txt, 'end',window=check_button)
	}
	
	tkinsert(txt,'end','\n')
	
	tkconfigure(txt, state="disabled")
	tkpack(txt)
	tkgrid(tt)
	
	main0=tkframe(mainall,bg='white')
	
	#========================================
	# fucntion index.calc3 is triggered by OK button
	# Do all the calculations.
	#========================================
	index.calc3 <- function(){
		cbv=rep(0,length(indices))
		for(i in 1:length(indices)) cbv[i]=tclvalue(cbvalue[i])
		
		tkdestroy(main)
		
		# trend file
		trend_file<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
		assign('trend_file',trend_file,envir=.GlobalEnv)
		cat(file=trend_file,paste("Lat","Lon","Indices","SYear","EYear","Slope","STD_of_Slope","P_Value",sep=","),fill=180,append=F)
		
		# pdf file for all plots
		pdf(file=paste(outjpgdir,paste(ofilename,"_all_plots.pdf",sep=""),sep="/"),height=8,width=11.5)
		pdf.dev=dev.cur()
		assign('pdf.dev',pdf.dev,envir=.GlobalEnv)
		
		index_not_calculated=''   # contains index names that could not be calculated.
		assign('index_not_calculated',index_not_calculated,envir=.GlobalEnv)
		
		#=============================================================
		# Depending on checkbox selected call appropriate function
		#============================================================
#		for(i in 1:length(indices)) {
#			if(cbv[i] == 1) {
#				indexparam <- paste("climdex.",indices[i],sep="")
#				index.store <- eval(indexparam) ; write.index.csv(index.store,index.name=indices[1]) ; plot.call(index.store,index.name=indices[1],index.units=units[1],x.label="Time")
#			}
#		}

		if (cbv[1]==1) { print(paste("calculating",indices[1])) ; index.store <- climdex.fd(cio) ; write.index.csv(index.store,index.name=indices[1]) ; plot.call(index.store,index.name=indices[1],index.units=units[1],x.label="Years") }
		if (cbv[2]==1) { print(paste("calculating",indices[2])) ; index.store <- climdex.fd2(cio) ; write.index.csv(index.store,index.name=indices[2]) ; plot.call(index.store,index.name=indices[2],index.units=units[2],x.label="Years") }
		if (cbv[3]==1) { print(paste("calculating",indices[3])) ; index.store <- climdex.fdm2(cio) ; write.index.csv(index.store,index.name=indices[3]) ; plot.call(index.store,index.name=indices[3],index.units=units[3],x.label="Years") }
		if (cbv[4]==1) { print(paste("calculating",indices[4])) ; index.store <- climdex.fdm20(cio) ; write.index.csv(index.store,index.name=indices[4]) ; plot.call(index.store,index.name=indices[4],index.units=units[4],x.label="Years") }
		if (cbv[5]==1) { print(paste("calculating",indices[5])) ; index.store <- climdex.id(cio) ; write.index.csv(index.store,index.name=indices[5]) ; plot.call(index.store,index.name=indices[5],index.units=units[5],x.label="Years") }
		if (cbv[6]==1) { print(paste("calculating",indices[6])) ; index.store <- climdex.su(cio) ; write.index.csv(index.store,index.name=indices[6]) ; plot.call(index.store,index.name=indices[6],index.units=units[6],x.label="Years") }
		if (cbv[7]==1) { print(paste("calculating",indices[7])) ; index.store <- climdex.tr(cio) ; write.index.csv(index.store,index.name=indices[7]) ; plot.call(index.store,index.name=indices[7],index.units=units[7],x.label="Years") }
		if (cbv[8]==1) { print(paste("calculating",indices[8])) ; index.store <- climdex.gsl(cio) ; write.index.csv(index.store,index.name=indices[8]) ; plot.call(index.store,index.name=indices[8],index.units=units[8],x.label="Years") }
		if (cbv[9]==1) { print(paste("calculating",indices[9])) ; index.store <- climdex.txx(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[9]) ; plot.call(index.store,index.name=indices[9],index.units=units[9],x.label="Years") }
		if (cbv[10]==1) { print(paste("calculating",indices[10])) ; index.store <- climdex.tnn(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[10]) ; plot.call(index.store,index.name=indices[10],index.units=units[10],x.label="Years") }
		if (cbv[11]==1) { print(paste("calculating",indices[11])) ; index.store <- climdex.tnx(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[11]) ; plot.call(index.store,index.name=indices[11],index.units=units[11],x.label="Years") }
		if (cbv[12]==1) { print(paste("calculating",indices[12])) ; index.store <- climdex.txn(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[12]) ; plot.call(index.store,index.name=indices[12],index.units=units[12],x.label="Years") }
		if (cbv[13]==1) { print(paste("calculating",indices[13])) ; index.store <- climdex.dtr(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[13]) ; plot.call(index.store,index.name=indices[13],index.units=units[13],x.label="Years") }
		if (cbv[14]==1) { print(paste("calculating",indices[14])) ; index.store <- climdex.wsdi(cio) ; write.index.csv(index.store,index.name=indices[14]) ; plot.call(index.store,index.name=indices[14],index.units=units[14],x.label="Years") }
		if (cbv[15]==1) { print(paste("calculating",indices[15])) ; index.store <- climdex.wsdin(cio,n=wsdi_ud) ; write.index.csv(index.store,index.name=indices[15]) ; plot.call(index.store,index.name=indices[15],index.units=units[15],x.label="Years") }
		if (cbv[16]==1) { print(paste("calculating",indices[16])) ; index.store <- climdex.csdi(cio) ; write.index.csv(index.store,index.name=indices[16]) ; plot.call(index.store,index.name=indices[16],index.units=units[16],x.label="Years") }
		if (cbv[17]==1) { print(paste("calculating",indices[17])) ; index.store <- climdex.csdin(cio,n=csdi_ud) ; write.index.csv(index.store,index.name=indices[17]) ; plot.call(index.store,index.name=indices[17],index.units=units[17],x.label="Years") }
		if (cbv[18]==1) { print(paste("calculating",indices[18])) ; index.store <- climdex.tx50p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[18]) ; plot.call(index.store,index.name=indices[18],index.units=units[18],x.label="Years") }
		if (cbv[19]==1) { print(paste("calculating",indices[19])) ; index.store <- climdex.tx95t(cio) ; write.index.csv(index.store,index.name=indices[19]) ; plot.call(index.store,index.name=indices[19],index.units=units[19],x.label="Years") }
		if (cbv[20]==1) { print(paste("calculating",indices[20])) ; index.store <- climdex.tx10p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[20]) ; plot.call(index.store,index.name=indices[20],index.units=units[20],x.label="Years") }
		if (cbv[21]==1) { print(paste("calculating",indices[21])) ; index.store <- climdex.tx90p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[21]) ; plot.call(index.store,index.name=indices[21],index.units=units[21],x.label="Years") }
		if (cbv[22]==1) { print(paste("calculating",indices[22])) ; index.store <- climdex.tn10p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[22]) ; plot.call(index.store,index.name=indices[22],index.units=units[22],x.label="Years") }
		if (cbv[23]==1) { print(paste("calculating",indices[23])) ; index.store <- climdex.tn90p(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[23]) ; plot.call(index.store,index.name=indices[23],index.units=units[23],x.label="Years") }
		if (cbv[24]==1) { print(paste("calculating",indices[24])) ; index.store <- climdex.tm5a(cio) ; write.index.csv(index.store,index.name=indices[24]) ; plot.call(index.store,index.name=indices[24],index.units=units[24],x.label="Years") }
		if (cbv[25]==1) { print(paste("calculating",indices[25])) ; index.store <- climdex.tm5b(cio) ; write.index.csv(index.store,index.name=indices[25]) ; plot.call(index.store,index.name=indices[25],index.units=units[25],x.label="Years") }
		if (cbv[26]==1) { print(paste("calculating",indices[26])) ; index.store <- climdex.tm10a(cio) ; write.index.csv(index.store,index.name=indices[26]) ; plot.call(index.store,index.name=indices[26],index.units=units[26],x.label="Years") }
		if (cbv[27]==1) { print(paste("calculating",indices[27])) ; index.store <- climdex.tm10b(cio) ; write.index.csv(index.store,index.name=indices[27]) ; plot.call(index.store,index.name=indices[27],index.units=units[27],x.label="Years") }
		if (cbv[28]==1) { print(paste("calculating",indices[28])) ; index.store <- climdex.su30(cio) ; write.index.csv(index.store,index.name=indices[28]) ; plot.call(index.store,index.name=indices[28],index.units=units[28],x.label="Years") }
		if (cbv[29]==1) { print(paste("calculating",indices[29])) ; index.store <- climdex.su35(cio) ; write.index.csv(index.store,index.name=indices[29]) ; plot.call(index.store,index.name=indices[29],index.units=units[29],x.label="Years") } 
		if (cbv[30]==1) { print(paste("calculating",indices[30])) ; index.store <- climdex.hddheat(cio,Tb=Tb_HDD) ; write.index.csv(index.store,index.name=indices[30]) ; plot.call(index.store,index.name=indices[30],index.units=units[30],x.label="Years") }
		if (cbv[31]==1) { print(paste("calculating",indices[31])) ; index.store <- climdex.cddcold(cio,Tb=Tb_CDD) ; write.index.csv(index.store,index.name=indices[31]) ; plot.call(index.store,index.name=indices[31],index.units=units[31],x.label="Years") }
                if (cbv[32]==1) { print(paste("calculating",indices[32])) ; index.store <- climdex.gddgrow(cio,Tb=Tb_GDD) ; write.index.csv(index.store,index.name=indices[32]) ; plot.call(index.store,index.name=indices[32],index.units=units[32],x.label="Years") }
                if (cbv[33]==1) { print(paste("calculating",indices[33])) ; index.store <- climdex.r20mm(cio) ; write.index.csv(index.store,index.name=indices[33]) ; plot.call(index.store,index.name=indices[33],index.units=units[33],x.label="Years") }
                if (cbv[34]==1) { print(paste("calculating",indices[34])) ; index.store <- climdex.cdd(cio) ; write.index.csv(index.store,index.name=indices[34]) ; plot.call(index.store,index.name=indices[34],index.units=units[34],x.label="Years") }
                if (cbv[35]==1) { print(paste("calculating",indices[35])) ; index.store <- climdex.cwd(cio) ; write.index.csv(index.store,index.name=indices[35]) ; plot.call(index.store,index.name=indices[35],index.units=units[35],x.label="Years") }
                if (cbv[36]==1) { print(paste("calculating",indices[36])) ; index.store <- climdex.r10mm(cio) ; write.index.csv(index.store,index.name=indices[36]) ; plot.call(index.store,index.name=indices[36],index.units=units[36],x.label="Years") }
                if (cbv[37]==1) { print(paste("calculating",indices[37])) ; index.store <- climdex.r20mm(cio) ; write.index.csv(index.store,index.name=indices[37]) ; plot.call(index.store,index.name=indices[37],index.units=units[37],x.label="Years") }
                if (cbv[38]==1) { print(paste("calculating",indices[38])) ; index.store <- climdex.rx1day(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[38]) ; plot.call(index.store,index.name=indices[38],index.units=units[38],x.label="Years") }
                if (cbv[39]==1) { print(paste("calculating",indices[39])) ; index.store <- climdex.rx5day(cio,freq=frequency) ; write.index.csv(index.store,index.name=indices[39]) ; plot.call(index.store,index.name=indices[39],index.units=units[39],x.label="Years") }
                if (cbv[40]==1) { print(paste("calculating",indices[40])) ; index.store <- climdex.prcptot(cio) ; write.index.csv(index.store,index.name=indices[40]) ; plot.call(index.store,index.name=indices[40],index.units=units[40],x.label="Years") }
                if (cbv[41]==1) { print(paste("calculating",indices[41])) ; index.store <- climdex.sdii(cio) ; write.index.csv(index.store,index.name=indices[41]) ; plot.call(index.store,index.name=indices[41],index.units=units[41],x.label="Years") }
                if (cbv[42]==1) { print(paste("calculating",indices[42])) ; index.store <- climdex.r95p(cio) ; write.index.csv(index.store,index.name=indices[42]) ; plot.call(index.store,index.name=indices[42],index.units=units[42],x.label="Years") }
                if (cbv[43]==1) { print(paste("calculating",indices[43])) ; index.store <- climdex.r99p(cio) ; write.index.csv(index.store,index.name=indices[43]) ; plot.call(index.store,index.name=indices[43],index.units=units[43],x.label="Years") }
                if (cbv[44]==1) { print(paste("calculating",indices[44])) ; index.store <- climdex.r95ptot(cio) ; write.index.csv(index.store,index.name=indices[44]) ; plot.call(index.store,index.name=indices[44],index.units=units[44],x.label="Years") }
                if (cbv[45]==1) { print(paste("calculating",indices[45])) ; index.store <- climdex.r99ptot(cio) ; write.index.csv(index.store,index.name=indices[45]) ; plot.call(index.store,index.name=indices[45],index.units=units[45],x.label="Years") }
                if (cbv[46]==1) { print(paste("calculating",indices[46])) ; index.store <- climdex.rxnday(cio,n=rx_ud) ; write.index.csv(index.store,index.name=indices[46]) ; plot.call(index.store,index.name=indices[46],index.units=units[46],x.label="Years") }
                if (cbv[47]==1) { print(paste("calculating",indices[47])) ; index.store <- climdex.rnnmm(cio,rnnmm_ud) ; write.index.csv(index.store,index.name=indices[47]) ; plot.call(index.store,index.name=indices[47],index.units=units[47],x.label="Years") }
                if (cbv[48]==1) { print(paste("calculating",indices[48])) ; index.store <- climdex.ntxntn(cio,n=txtn_ud) ; write.index.csv(index.store,index.name=indices[48]) ; plot.call(index.store,index.name=indices[48],index.units=units[48],x.label="Years") }
                if (cbv[49]==1) { print(paste("calculating",indices[49])) ; index.store <- climdex.ntxbntnb(cio,n=txtn_ud) ; write.index.csv(index.store,index.name=indices[49]) ; plot.call(index.store,index.name=indices[49],index.units=units[49],x.label="Years") }
                if (cbv[50]==1) { print(paste("calculating",indices[50])) ; index.store <- climdex.hw(cio,lat=latitude) ; write.hw.csv(index.store,index.name=indices[50]) ; plot.hw(index.store,index.name=indices[50],index.units=units[50],x.label="Years") }
                if (cbv[51]==1) { print(paste("calculating",indices[51])) ; rm(index.store); index.store <- climdex.spei(cio,ref.start=c(as.numeric(base.year.start),1),ref.end=c(as.numeric(base.year.end),1)
				,ini.date=min(as.numeric(date.years),na.rm=TRUE),lat=latitude) ; print(str(index.store)); write.precindex.csv(index.store,index.name=indices[51]) ; plot.precindex(index.store,index.name=indices[51],index.units=units[51],x.label="Years") }
                if (cbv[52]==1) { print(paste("calculating",indices[52])) ; index.store <- climdex.spi(cio) ; write.precindex.csv(index.store,index.name=indices[52]) ; plot.precindex(index.store,index.name=indices[52],index.units=units[52],x.label="Years") }

		dev.off(pdf.dev)
		graphics.off()  # close the pdf file, so you can open to view it now.
		
		# new window - calculation done
		nstation<-tktoplevel(bg='white')
		tkwm.geometry(nstation, "+0+0") # position in upper left corner of screen
		tkwm.title(nstation,"ClimPACT - Done")
		tkfocus(nstation)
		
		okk<-function(){tkdestroy(nstation);tkfocus(start1)}  # all are done, return to main window.
		
		textlabel0<-tklabel(nstation,text="     ",bg='white')  # message showing all are done, showing directory.
		textlabel1<-tklabel(nstation,text="Indices calculation completed",font=fontHeading1,bg='white')
		textlabel2<-tklabel(nstation,text=paste("Plots are in: ",outjpgdir,sep=" "),font=fontHeading2,bg='white')
		textlabel3<-tklabel(nstation,text=paste(ofilename,"_all_plots.pdf  contains all plots.",sep=""),font=fontHeading2,bg='white')
	
		## Will add one more part here, to show which indices were not calculated...
		okk.but<-tkbutton(nstation,text="   OK   ",command=okk,width=20,bg='white')
		tkpack(textlabel1)
		tkpack(textlabel2)
		tkpack(textlabel3)
		tkpack(textlabel0)
		tkpack(okk.but)
	}

	# If you choose "cancel", close current window, and return to main window (start1).
	done2<-function(){
		tkdestroy(main)
		tkfocus(start1)
	}
	
	tkgrid(tklabel(main0,text='',bg='white'))
	ok.but <-tkbutton(main0,text="  CONTINUE  ",command=index.calc3,width=20,bg='white',font=font_small)
	cancel.but<-tkbutton(main0,text="CANCEL",command=done2,width=20,bg='white',font=font_small)
	tkgrid(ok.but, cancel.but) 
	tkgrid(tklabel(main0,text='',bg='white'))
	tkgrid(tklabel(main0,text="It may take more than 5 minutes to compute all the indices.",font=fontHeading2,bg='white'),columnspan=2)
	tkgrid(tklabel(main0,text="Please be patient, you will be informed once computations are done.",font=fontHeading2,bg='white'),columnspan=2)
	
	tkgrid(main0)
	tkgrid(tklabel(mainall,text='',bg='white'))
	
	tkgrid(mainall)
}
# end of index.calc2  

# write.index.csv
# takes a time series of a given index and writes to file
write.index.csv <- function(index=NULL,index.name=NULL) {
        if(is.null(index.name) | is.null(index)) stop("Need index data and index.name in order to write CSV file.")

	nam1 <- paste(outinddir, paste(ofilename, "_", index.name, ".csv", sep = ""), sep = "/")
	write.table(index,    file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "-99.9", col.names = paste("time",index.name,sep=", "))
}

# write.hw.csv
# takes a time series of hw and writes to file
write.hw.csv <- function(index=NULL,index.name=NULL) {
        if(is.null(index)) stop("Need heatwave data to write CSV file.")

	# print each definition in a separate .csv. Thus each .csv will have columns of time, HWA, HWM, HWF, HWD, HWN.
	aspect.names <- list("time","HWM","HWA","HWN","HWD","HWF")

	# write Tx90 heatwave data
        nam1 <- paste(outinddir, paste(ofilename, "_Tx90_heatwave.csv", sep = ""), sep = "/")
	write.table(aspect.names, file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(as.numeric(date.years),aperm(index[1,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write Tn90 heatwave data
        nam1 <- paste(outinddir, paste(ofilename, "_Tn90_heatwave.csv", sep = ""), sep = "/")
        write.table(aspect.names, file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(as.numeric(date.years),aperm(index[2,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write EHF heatwave data
        nam1 <- paste(outinddir, paste(ofilename, "_EHF_heatwave.csv", sep = ""), sep = "/")
        write.table(aspect.names, file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind(as.numeric(date.years),aperm(index[3,,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
}

# plot.hw
# not sure how generic this process can be
plot.hw <- function(index=NULL,index.name=NULL,index.units=NULL,x.label=NULL) {
        if(is.null(index)) stop("Need heatwave data to plot.")

	definitions <- c("Tx90","Tn90","EHF")
	aspects <- c("HWM","HWA","HWN","HWD","HWF")
	units <- c("degC","degC","heat waves","days","days")

	for (def in 1:length(definitions)) {
		for (asp in 1:length(aspects)) {
			plot.title <- paste(title.station,definitions[def],aspects[asp],sep=", ")
	        	namp <- paste(outjpgdir, paste(ofilename, "_", definitions[def],"_",aspects[asp], ".jpg", sep = ""), sep = "/")
	        	jpeg(file = namp, width = 1024, height = 768)
		        dev0 = dev.cur()

			if(definitions[def]=="EHF" && any(aspects[asp]=="HWM",aspects[asp]=="HWA")) unit = "degC^2" else unit = units[asp]
		        plotx(as.numeric(date.years), index[def,asp,], main = gsub('\\*', unit, plot.title),ylab = unit,xlab = x.label)

		        dev.set(which = pdf.dev)
        		dev.copy()
		        dev.off(dev0)
		}
	}
}

# write.precindex.csv
# takes a time series of hw and writes to file
write.precindex.csv <- function(index=NULL,index.name=NULL) {
        if(is.null(index)) stop("Need SPEI data to write CSV file.")
print(str(index))
        # write 3 month data
        nam1 <- paste(outinddir, paste(ofilename, "_3month_SPEI.csv", sep = ""), sep = "/")
#        write.table(aspect.names, file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),index[1,]), file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = "time, SPEI")

        # write 6 month ata
        nam1 <- paste(outinddir, paste(ofilename, "_6month_SPEI.csv", sep = ""), sep = "/")
        write.table(aspect.names, file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.years),aperm(index[2,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)

        # write 12 month data
        nam1 <- paste(outinddir, paste(ofilename, "_12month_SPEI.csv", sep = ""), sep = "/")
        write.table(aspect.names, file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
        write.table(cbind((date.months),aperm(index[3,],c(2,1))), file = nam1, append = TRUE, quote = FALSE, sep = ", ", na = "-99.9", row.names=FALSE,col.names = FALSE)
}

# plot.precindex
# not sure how generic this process can be
plot.precindex <- function(index=NULL,index.name=NULL,index.units=NULL,x.label=NULL) {
        if(is.null(index)) stop("Need heatwave data to plot.")

        definitions <- c("Tx90","Tn90","EHF")
        aspects <- c("HWM","HWA","HWN","HWD","HWF")
        units <- c("degC","degC","heat waves","days","days")

        for (def in 1:length(definitions)) {
                for (asp in 1:length(aspects)) {
                        plot.title <- paste(title.station,definitions[def],aspects[asp],sep=", ")
                        namp <- paste(outjpgdir, paste(ofilename, "_", definitions[def],"_",aspects[asp], ".jpg", sep = ""), sep = "/")
                        jpeg(file = namp, width = 1024, height = 768)
                        dev0 = dev.cur()

                        if(definitions[def]=="EHF" && any(aspects[asp]=="HWM",aspects[asp]=="HWA")) unit = "degC^2" else unit = units[asp]
                        plotx(as.numeric(date.years), index[def,asp,], main = gsub('\\*', unit, plot.title),ylab = unit,xlab = x.label)

                        dev.set(which = pdf.dev)
                        dev.copy()
                        dev.off(dev0)
                }
        }
}

# plot.index
# not sure how generic this process can be
plot.call <- function(index=NULL,index.name=NULL,index.units=NULL,x.label=NULL) {
        if(is.null(index.name) | is.null(index) | is.null(index.units)) stop("Need index data, index.name, index units and an x label in order to plot data.")

#	if (sum(is.na(tclext[, i + 1]) == F) >= min_data) {
		plot.title <- paste(title.station,index.name,sep=", ")
		namp <- paste(outjpgdir, paste(ofilename, "_", index.name, ".jpg", sep = ""), sep = "/")
		jpeg(file = namp, width = 1024, height = 768)
		dev0 = dev.cur()
		if(index.name=="tx95t") xdata <- 1:length(index)
		else xdata <- names(index)
		plotx(xdata, index, main = gsub('\\*', index.units, plot.title),
		  ylab = index.units,xlab = x.label)

		dev.set(which = pdf.dev)
		plotx(xdata, index, main = gsub('\\*', index.units, plot.title),
		  ylab = index.units, xlab = x.label)
		dev.copy()
		dev.off(dev0)
#	}
}

# plotx
# make plots, this is called twice to make jpg and pdf files. 
plotx <- function (x0, y0, main = "", xlab = "", ylab = "", opt = 0)
{
	if(all(is.na(y0))) { print("No data to plot") ; return() }

	x <- x0  # take a copy of input, so we will not modify the input by mistake.
	y <- y0
	
	# james: i'm turning xpd off for barplots, so that i can clip the range w/o the bars
	# running off the page. is this required?
	par(oma = c(1, 1, 1, 1), xpd = FALSE) #to enable things to be drawn outside the plot region
	names(y) <- c(x)
	
	# calculate range to limit the plots to (otherwise barplots are useless... they're in
	# any non-barplots for consistency). also to allow for overlays like marking na points
	# y.range <- range(y, na.rm = TRUE) #- 0.1 * (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
	# x.range <- min(x, na.rm = TRUE)      # should be no missing data in the x series
	
	if(barplot_flag)  # if true, we're doing a barplot
	{
	  bp <- barplot(y, main = main, cex.main = 2,
	    ylim = range(y, na.rm = TRUE),
	    xlab = xlab, ylab = ylab,
	    cex.lab = 1.5, cex.axis = 1.5,
	    xpd = FALSE)
	  
	  # NA points
	  na.x <- bp
	  na.y <- rep(NA, length(na.x))
	  na.y[is.na(y)] <- par("usr")[3]
	  points(na.x, na.y, pch = 17, col = "blue", cex = 1.5)
	  box()
	} else            # if false, we're doing a regular (line) plot
	{
	  plot(x, y, main = main, cex.main = 2,
	    ylim = range(y, na.rm = TRUE),
	    xlab = xlab, ylab = ylab,
	    type = "b", cex.lab = 1.5, cex.axis = 1.5)
	
	  # NA points
	  na.x <- x
	  na.y <- rep(NA, length(na.x))
	  na.y[is.na(y)] <- min(y, na.rm = TRUE)
	  points(na.x, na.y, pch = 17, col = "blue", cex = 1.5)
	}
	
	if (opt == 1) return()  # no need to plot trend/fitting curve.
	if (opt == 2)
	{
	  abline(h = 0.)
	  return()
	}  # for spei & spi only!

	fit <- lsfit(1:length(x), y)		# assumes time intervals are always evenly spaced
	out <- ls.print(fit, print.it = FALSE)
	r2 <- round(100 * as.numeric(out$summary[1, 2]), 1)
	pval <- round(as.numeric(out$summary[1, 6]), 3)
	beta <- round(as.numeric(out$coef.table[[1]][2, 1]), 3)
	betaerr <- round(as.numeric(out$coef.table[[1]][2, 2]), 3)
	
	xy <- cbind(x, y)                      # for regular plots
	if (barplot_flag) xy <- cbind(bp,y)    # for barplots
	xy <- na.omit(xy)
	lines(lowess(xy[, 1], xy[, 2]), lwd = 3, lty = 2, col = "red")  # add fitting curve
	
#	if (sum(is.na(y) == FALSE) >= min_trend)
#	{
	   subtit <- paste("Linear trend slope=", beta, "   Slope error=", betaerr, ",   p-value=", pval)             # least squares regression
#	} else
#	{
#	  subtit <- "No linear trend due to insufficient valid data points (10)"
#	}
	title(sub = subtit, cex.sub = 1.5)
	if (abs(max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) < 1.e-3)
	{
	  legend("bottomleft", "locally weighted scatterplot smoothing", col = "red", lty = 2, lwd = 3, bty = "n")
	} else
	{  
#	  legend(x[1],
#	    min(y, na.rm = TRUE) - (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) / 10,
		legend("bottomleft","locally weighted scatterplot smoothing",col = "red", lty = 2, lwd = 3, bty = "n")
	}
}
# end of plotx

# brief introduction of the background.
about <- function(){
	tkmessageBox(message="This code was developed by Hongang Yang, Lisa Alexander and Sarah Perkins based on the RClimDEX software developed by the joint WMO CCl/CLIVAR/JCOMM Expert Team on Climate Change Detection and Indies (ETCCDI), with modifications by several researchers and students. Please refer to the software manual for technical support. \n\nLast updated on 2013-06-04. \n",
	title="ClimPACT - About", icon='info')
}

#######################################################################################
# startss
# Main function. Initiates GUI.
# NOTES: - Don't need to be so pedantic about logos -> If files don't exist, show a warning at most, then move on.
startss <- function(){
	logo_require <- FALSE
	
	# search logo files in current working directory.
	no.logo <- FALSE;
	dir0 <- getwd();
	logo1 <- "WMOLogo.gif";
	logo2 <- "UNSW.gif";
	logo3 <- "coess.gif";
	
	# NICK: Do we need this?
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
	# Everything above here relates to logos!!
	
	
	
	# lower half of the window.
	tkgrid(tklabel(start1, text = "    ", bg = "white"));
	tkgrid(tklabel(start1, text = "             ClimPACT             ", font = fontHeading, width = 30, bg = "white"), columnspan = 2);
	tkgrid(tklabel(start1, text = "    ", bg = "white"));
	
	start.but   <- tkbutton(start1, text = "Load Data and Run QC", command = load.data.qc, width = 30, font = fontHeading2, bg = "white");
	cal.but     <- tkbutton(start1, text = "Indices Calculation", command = index.calc1, width = 30, font = fontHeading2, bg = "white");
	cancel.but  <- tkbutton(start1, text = "Exit", command = done, width = 30, font = fontHeading2, bg = "white");
	help.but    <- tkbutton(start1, text = "About", command = about, width = 30, font = fontHeading2, bg = "white");
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

#================================================
#  This runs the program
#================================================
startss()
