# phenoricev3
New version of phenorice

## Main changes

* Streamlined code
* Use of "new" ENVI object-oriented processing routines for managing raster data processing
* Introduced parallelization on smoothing algorithm
* Introduced a test-dataset, allowing to quickly run the algorithm on a small area (currently in Italy) and verify bugs and/or results of algorithm modifications

## Current Status

* Input files creation and smoothing should be working

* Three smoothing methods are available and can be selected in the first lines of "pr__main_gui_v30.pro: 

  1) "normal"  --> Slow single core computation
  2) "parallel-pixel"  --> Intermediate solution. Breaks lines among processors
  3) "parallel-line"   --> Fastest solution. Breaks input images in "chunks". Could be memory-hungry !

* Work still to be done: 

  1) Implement parallelization also on the phenology extraction part
  2) Implement "VI-shape" check to reduce misclassification with forest in tropics
  3) Re-think outputs formatting and allow user to select only the "variables he is interested in 
  4) Introduce a "checking" routine to allow user to quickly visualize/extract both inputs and results on a selected pixel (area)
  5) .....