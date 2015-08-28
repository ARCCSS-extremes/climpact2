

				ClimPACT2
			Last updated: 26/8/2015


  What is it?
  -----------
  
  ClimPACT2 is an R software package that calculates the ETSCI indices, ETCCDI 
  indices, and the heatwave indices from Perkins and Alexander (2013). It directly 
  incorporates the R climdex.pcic package (available on CRAN) to perform most 
  of the calculations, which is available thanks to the efforts of the Pacific 
  Climate Impacts Consortium (PCIC). ClimPACT2 was developed at the University of 
  New South Wales and has several dependencies on other R packages. Two separate 
  files allow the indices to be calculated in different ways. The climpact.loader 
  function in climpact2.r allows power users to process gridded netCDF data, while
  climpact2.GUI.r provides a basic graphical user interface for processing ASCII 
  time-series data.
  
  
  Where can I get it?
  -------------------
  
  ClimPACT2 is available on github @ https://github.com/ARCCSS-extremes/climpact2
  

  How do I install it?
  --------------------

    Software you will need:
        -netCDF (only required if working with three dimensional datasets)
        -R. You will also need several R packages, which are installed
         for you by the checker script, see below.

    1) Create a new directory

    2) cd to above directory and download and extract the following file:
       https://github.com/ARCCSS-extremes/climpact2/archive/master.zip

    3) In R, type "source('climpact2.checker.r')" to install the appropriate
       R packages needed by ClimPACT2. 
 
    4) Installation is complete.

  ** Note that if you are running a Windows operating system you will only 
     be able to use the GUI. In a Unix-based operating system you can use 
     the GUI as well as operate on gridded data.

  How do I use it?
  ----------------

  To run the GUI, cd to the directory where the ClimPACT2 files have been
  extracted, then open R and run 'source("climpact2.GUI.r")'.

  To calculate indices from gridded netCDF datasets, source the climpact2.r 
  file and call the climpact.loader function. See climpact2.wrapper.r as an
  example of doing this, to execute this script on the sample data, run 
  'Rscript climpact2.wrapper.r' from the terminal command line.

  Calculating all 51 indices for a 20 year record with horizontal resolution 
  of 144 x 215 takes approximately 12 hours on 2 cores. 


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

