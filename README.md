# climpact2
Combining climdex.pcic and climpact @ UNSW

TIPS FOR TESTERS
- Since the indices take a while to calculate (~50 minutes per index), put 'nohup' in front of your Rscript command to ensure the process keeps running even if you have to disconnect. If you're running screen this won't matter.

- For functional tests (i.e. those that don't require you checking that actual output of the index calculations), you should use small grids instead of the example NarClim data I include. The "cdo -selyear" command lets you chop up an existing dataset.
