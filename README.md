# climpact2
Combining climdex.pcic and climpact @ UNSW

NOTE
- There are two bugs in climdex.pcic that need fixing before using climpact2. If you have climdex.pcic already installed you will need to remove it (see install.txt). The climdex.pcic maintainer is aware of these bugs but their fixes are not incorporated in the current CRAN release.
- The  functions should work. To specify a parameter specific to a particular index (e.g. "cdd") simply prefix the parameter with the index name when calling climpact.loader (e.g. you could specify "cdd_spells.can.span.years = TRUE").

TIPS FOR TESTERS
- Since the indices take a while to calculate (~50 minutes per index), put 'nohup' in front of your Rscript command to ensure the process keeps running even if you have to disconnect. If you're running screen this won't matter.

- For functional tests (i.e. those that don't require you checking that actual output of the index calculations), you should use small grids instead of the example NarClim data I include. The "cdo -selyear" and "cdo -sellonlatbox" commands let you chop up an existing dataset.

RECORDING PROBLEMS/ISSUES
- When you find an error or want to make a comment or suggestion use the "Issues" tab on the right side of this screen.
