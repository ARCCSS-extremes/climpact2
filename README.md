

# <p align="center">ClimPACT2</p>


  What is it?
  -----------
  
  ClimPACT2 is an R software package that calculates the ET-SCI indices as well 
  as additional climate extremes indices from data stored in text or netCDF files. It 
  directly incorporates the R packages climdex.pcic and climdex.pcic.ncdf developed 
  by the Pacific Climate Impacts Consortium (PCIC). Three methods of using the 
  software allow the user to calculate indices on a station text file
  via a Graphical User Interface, to batch process multiple station text 
  files in parallel and to calculate the indices on netCDF data in parallel.
  
  
  Where can I get it?
  -------------------
  
  ClimPACT2 is available on github @ https://github.com/ARCCSS-extremes/climpact2


  How do I use the GUI?
  ---------------------

    Software you will need before proceeding:
        -R (version 3.3 or later) 
         You will need administrator privileges on your computer
         or the ability to install R libraries.

	1) Download and extract the following file to your computer:
       https://github.com/ARCCSS-extremes/climpact2/archive/master.zip
       This will create a directory named "climpact2-master".

    2) In Windows: open R and select "File->Change dir..." and select the 
       climpact2-master directory created in step 1. Then type 
       "source('climpact2.GUI.r')"
       
       NOTE: If nothing happens, try run the additional command "startss()".

       In Linux/MacOS: cd to the climpact2-master directory created in
       step 1, then open R in a terminal window and type 
       "source('climpact2.GUI.r')".

       The first time ClimPACT2 is run it will install required R packages.
       This will likely require you to select a mirror to download from.

    Video tutorial on how to install R in Windows
    -> https://www.youtube.com/watch?v=Nb-yt3gSnQw


  How do I calculate the indices on netCDF datasets? (Linux/MacOS only)
  ---------------------------------------------------------------------

    Warning: Due to an error in the netCDF SPEI and SPI package these indices will not be
    correct IF your data contain missing values (e.g. they are based on observations).
    
    Software you will need before proceeding:
        -R (version 3.3 or later). You will need administrator privileges 
		 on your computer or the ability to install R libraries.
        -netCDF
        -PROJ4 development files (libproj-dev package on Ubuntu)
        -udunits development files (libudunits2-dev package on Ubuntu)

	1) Download and extract the following file to your computer:
       https://github.com/ARCCSS-extremes/climpact2/archive/master.zip
       This will create a directory named "climpact2-master".

    2) Cd to the climpact2-master directory created in step 1, open R and run 
       "source('installers/climpact2.ncdf.installer.r')" to install the required R packages.
       You may be asked whether you would like to make a personal library, in 
       most cases the answer should be 'yes'. Once complete, quit R by typing
       "q()". This step only needs to be done once.

    3) Modify the climpact2.ncdf.wrapper.r file to suit your needs (see manual
       for optional parameters to specify). Then execute by running 
       "Rscript climpact2.ncdf.wrapper.r" from the Linux command line. Depending
       on the size of your data and the number of cores selected, this process
       can take anywhere from one to twelve hours.

              Notes on netCDF data format:
              - Files must be CF compliant.
              - There must be no 'bounds' attributes in your latitude or 
                longitude variables.
              - Your precipitation variable must have units of "kg m-2 d-1",
                not "mm/day". These are numerically equivalent.
              - Your minimum and maximum temperature variables must be 
                uniquely named.
              - ncrename, ncatted and ncks from the NCO toolset can help 
                you modify your netCDF files.
                http://nco.sourceforge.net/


  How do I batch process multiple station (.txt) files?
  -----------------------------------------------------
  
    Software you will need before proceeding:
        -R (version 3.3 or later). You will need administrator privileges 
		 on your computer or the ability to install R libraries.
	
	1) Download and extract the following file to your computer:
       https://github.com/ARCCSS-extremes/climpact2/archive/master.zip
       This will create a directory named "climpact2-master".

    2) Cd to the climpact2-master directory created in step 1, open R and run 
       "source('installers/climpact2.batch.installer.r')" to install the required R packages.
       You may be asked whether you would like to make a personal library, in 
       most cases the answer should be 'yes'. Once complete, quit R by typing
       "q()". This step only needs to be done once.
       
    3) From the terminal run the following command, replacing the flags
       with the folder where your station text files are kept, a metadata file
       containing the file name of each station text file along with relevant 
       station information, the beginning and end years of the base period, and
       the number of cores to use in processing, respectively. See the user guide
       for more information.
	   Rscript climpact2.batch.stations.r ./sample_data/ ./sample_data/climpact2.sample.batch.metadata.txt 1971 2000 4
  
  
  Common problems
  ---------------

* Running the GUI on MacOS. Users may need to install XQuartz, ensure
to restart your computer after installing. https://www.xquartz.org/

* Running the GUI on Linux. If an error occurs citing "BWidget" then it is likely
that your operating system needs the "BWidget" package installed (this comes
pre-installed on some operating systems but not on all). This can be done
through your operating system's package manager.

* Running the GUI on Linux. When trying to install packages R fails with 
"/bin/bash: g++: command not found"
You need to install g++ onto your operating system via your package manager.
In most Linux operating systems a command such as "sudo apt-get install g++" 
at the command line (i.e. not inside R) will suffice. Once installed, run 
ClimPACT2 again.

* If you experience trouble installing R packages in Windows, try to disable
your antivirus software temporarily.

* For interpolated data remove coordinates attribute from the variables via ncatted -O -a coordinates,,d,,<your_file>.nc

* Guidance on running the batch script in Windows see [this post](https://github.com/ARCCSS-extremes/climpact2/issues/56)
<a/>


  Known issues
  ------------

* When using the GUI or batch script, plots of the SPEI and SPI indices do not get printed into the aggregated PDF. Individual plot files are still made.

* Due to an error in the [CRAN SPEI package](https://cran.r-project.org/web/packages/SPEI/index.html) SPEI and SPI will not be
    correct IF your data contain missing values (e.g. they are based on observations). This only applies to netCDF data, if using
    the GUI then this is not an issue.
<a/>


  Documentation
  -------------
  
  Documentation exists in the form of this README file, the official ClimPACT2
  user guide (available with this software) as well as the source code itself.


  Contact
  -------
  
  Software issues contact Nicholas Herold : nicholas.herold@unsw.edu.au
  
  All other issues contact Lisa Alexander : l.alexander@unsw.edu.au
  
  
  Credits
  -------
  
  Oversight: World Meteorological Organisation (WMO), the Expert Team on
  Sector-specific Climate Indices (ET-SCI).
  
  Design and documentation: Lisa Alexander and Nicholas Herold.
  
  GUI: Nicholas Herold, James Goldie, Lisa Alexander, Enric Aguilar, Marc Prohom, 
	  the Pacific Climate Impacts Consortium (David Bronaugh, James Hiebert),
	  Hongang Yang, Yang Feng and Yujun Ouyang.
  
  NetCDF calculation: Pacific Climate Impacts Consortium (David Bronaugh, 
  James Hiebert) and Nicholas Herold.
  
  Batch processing: Nicholas Herold.
  
  
