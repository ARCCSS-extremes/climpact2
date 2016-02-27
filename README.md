

				ClimPACT2
			Last updated: February 2016


  What is it?
  -----------
  
  ClimPACT2 is an R software package that calculates the ET-SCI indices, as well 
  as additional climate extremes indices. It directly incorporates the R 
  climdex.pcic package (available on CRAN) to perform most of the calculations, 
  which is available thanks to the efforts of the Pacific Climate Impacts 
  Consortium (PCIC). ClimPACT2 was developed at the University of New South Wales 
  and has several dependencies on other R packages. Two separate files allow the 
  indices to be calculated in different ways. The climpact.loader function in 
  climpact2.r allows power users to process gridded netCDF data, while 
  climpact2.GUI.r provides a basic graphical user interface for processing ASCII 
  time-series data.
  
  
  Where can I get it?
  -------------------
  
  ClimPACT2 is available on github @ https://github.com/ARCCSS-extremes/climpact2
  

  How do I use the GUI?
  ---------------------

    Software you will need installed:
        -R

    1) Download and extract the following file:
       https://github.com/ARCCSS-extremes/climpact2/archive/master.zip

    2) In Windows, open R and select "File->Change dir..." and select the 
       climpact2-master directory created in step 1. Then type 
       "source('climpact2.GUI.r')"

       In Linux or MacOS, cd to the climpact2-master directory created in
       step 1, then open R and type "source('climpact2.GUI.r')".

       The first time ClimPACT2 is run it will install required packages.
       This will likely require you to select a mirror to download from.
 

  VIDEO TUTORIAL HOW TO INSTALL R IN WINDOWS 
  -> https://www.youtube.com/watch?v=Nb-yt3gSnQw

  VIDEO TUTORIAL HOW TO RUN CLIMPACT2 IN WINDOWS
  -> https://www.youtube.com/watch?v=eD8FqXdGNCs

  VIDEO TUTORIAL HOW TO RUN CLIMPACT2 IN LINUX
  -> https://www.youtube.com/watch?v=1sSCqsRMCZI


  What if I want to calculate the indices on a netCDF dataset?**
  --------------------------------------------------------------

    Software you will need installed:
        -R
        -netCDF

    1) Create a new directory

    2) cd to new directory, and download and extract the following file:
       https://github.com/ARCCSS-extremes/climpact2/archive/master.zip

    3) Run climpact2.checker.r file to install required packages.

    4) Modify the climpact2.wrapper.r file to suit your needs (see manual for
       optional parameters to specify). Then execute by running 
       'Rscript climpact2.wrapper.r' from the Linux command line.***

  ** Note that if you are running a Windows operating system you will only
     be able to use the GUI. In a Unix-based operating system you can use
     the GUI as well as operate on netCDF files.
  *** Calculating all 51 indices for a 20 year record with horizontal resolution 
      of 144 x 215 takes approximately 12 hours on 2 cores. 


  Common problems
  ---------------

    1) Running the GUI in Linux; when trying to install packages R fails with 
       "/bin/bash: g++: command not found"
       You need to install g++ onto your system via the package manager on your 
       particular operating system. In most Linux operating system a command such 
       as "sudo apt-get install g++" at the command line (i.e. not inside R) will
       suffice in installing the software. Once done, run ClimPACT2 again.

    2) If you experience trouble installing R packages in Windows, try to disable
       your antivirus temporarily.


  Documentation
  -------------
  
  Documentation exists in the form of this README file, the official ClimPACT2
  manual (available with this software on github) as well as the source code
  itself.

  
  Licensing
  ---------

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
  

  Contacts
  --------
  
  Software issues contact Nicholas Herold : nicholas.herold@unsw.edu.au
  All other issues contact Lisa Alexander : lisa.alexander@unsw.edu.au

