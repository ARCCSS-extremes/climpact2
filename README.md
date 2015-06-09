

				ClimPACT2
			Last updated: 31/5/2015


  What is it?
  -----------
  
  ClimPACT2 is an R software package that calculates the ETSCI indices, ETCCDI 
  indices, and the heatwave indices from Perkins and Alexander (2013). It directly 
  incorporates the R climdex.pcic package (available on CRAN) to perform most 
  of the calculations. It was developed at the University of New South Wales and
  has several dependencies on other R packages. Two separate files allow the
  indices to be calculated in different ways. The climpact.loader function in 
  climpact2.r allows power users to process gridded netCDF data, while climpact2.GUI.r 
  provides a basic graphical user interface for processing ASCII time-series data.
  
  
  Where can I get it?
  -------------------
  
  ClimPACT2 is available on github @ https://github.com/heroldn/climpact2
  
  
  How do I use it?
  ----------------

  To calculate indices for from gridded netCDF files, use climpact2.wrapper.r to call 
  the climpact.loader function in climpact2.r. Calculating all 51 indices for a 20 year
  record with horizontal resolution of 144 x 215 takes approximately 12 hours on 2 
  cores. To calculate indices from a time-series of station data stored in ASCII 
  format, source climpact2.GUI.r.


  Documentation
  -------------
  
  Documentation exists in the form of this README file, the official ClimPACT2
  manual (available with this software on github) as well as the source code
  itself.
  
  
  Installation
  ------------
  
    Software you will need:
    	-R
    
    The following R packages need to be installed. For the GUI only the first
    4 packages are required.
    	-climdex.pcic (there is a custom version of this package to be installed which
           	contains minor fixes and is downloaded automatically with the rest of the
     	        ClimPACT2 software)
        -tcltk
        -PCICt
    	-SPEI
    	-foreach
    	-doParallel
    	-abind
        -ncdf4

    1) Create a new directory

    2) cd to above directory and run "git clone https://github.com/heroldn/climpact2.git"
    
    3) Install the required R packages listed above. This can be done with 
       climpact2.checker.r
  
    4) To calculate indices from gridded netCDF datasets source the climpact2.r file and 
       call the climpact.loader function (see climpact2.wrapper.r for an example). 
       Alternatively, to load time-series from ASCII files source climpact2.GUI.r to use
       the graphical user interface. Refer to the user manual for more detailed instructions.
  
  
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

