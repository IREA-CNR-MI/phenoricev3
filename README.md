# phenoricev3
New version of phenorice

## Main changes

* Streamlined code
* Use of "new" ENVI object-oriented processing routines for managing raster data processing
* Use of metaspectralraster input files - allows avoiding to create intermediate inputs "
* Introduced parallelization on smoothing algorithm and processing algorithm - huge speed increase !
* Introduced additional checks on shape of Spectral Indexes time series
* Introduced a "subsetting" capability - if the mask is smaller than the full extent of the inputs, processing is done only on the subset
* Improved the GUI and streamlined the criteria to facilitate the user
* Modified outputs structure - both "single files" for each phenometric and a "full_out" file containing all bands are produced
* Introduced a test-dataset, allowing to quickly run the algorithm on a small area (currently in PHL) and verify bugs and/or results of algorithm modifications

## Current Status

* Algorithm normally runs in parallelized mode. Debug mode available (uses standard "serial" processing - slower but easier to debug)

* Work still to be done: 

  1. Introduce a IDL-based GUI
  2. Introduce a "checking" routine to allow user to quickly visualize/extract both inputs and results on a selected pixel (area)
   .....
