# phenoricev3
New version of phenorice

## Main changes

* Streamlined code
* Use of "new" ENVI object-oriented processing routines for managing raster data processing
* Introduced parallelization on smoothing algorithm and processing algorithm
* Modified outputs structure - both "single files" for each phenometric and a "full_out" file containing all bands are produced
* Introduced a test-dataset, allowing to quickly run the algorithm on a small area (currently in Italy) and verify bugs and/or results of algorithm modifications

## Current Status

* Input files creation and smoothing should be working

* Algorithm normally runs in parallelized mode. Debug mode available (uses standard "serial" processing - slower but easier to debug)

* Work still to be done: 

  1. Introduce a IDL-based GUI
  2. Introduce subsetting capabilities on the basis of a user provided shapefile/extent
  2. Improve documentation
  2. Implement "VI-shape" check to reduce misclassification with forest in tropics
  3. Introduce a "checking" routine to allow user to quickly visualize/extract both inputs and results on a selected pixel (area)
   .....
