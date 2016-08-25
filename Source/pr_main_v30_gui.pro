  ; Title pr_main
  ;:Description
  ; Main launching script for the PHENORICE v3.0 application
  ;   --> Calls the GUI for selecting processing parameters
  ;   --> Set up the structure containing processing options (opts)
  ;   --> Launches the script for creating input time series (pr_build_inputs_v33.pro)
  ;   --> Launch processing (pr_init_processing_v30_parline)

  ;Details
  ;
  ;:RETURNS:
  ;
  ;:AUTHOR: Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it(2016)
  ;
  ;License GPL(>3.0)
  ;-


  COMPILE_OPT idl2
  COMPILE_OPT hidden

  t1 = systime(1)

  ;- --------------------------------------------------------- ;
  ; Set some options for test processing
  ;- --------------------------------------------------------- ;
  test_data      = 0             ; Leads to using default input data and parameters (for testing purposes)
  test_folder    = '/home/lb/Desktop/PHL_Clipped/'  ; testing data folder
  mapscape       = 1             ; Specify to use "mapscape-like" input files --> Leads to changes in NODATA values and (possibly)
  ; generate smoothed file from MAPSCAPE data!!!
  sel_seasons    = [1,1,1,1]
  doy_1q         = [0,90]        ; -> Start and end DOYs of each "season"
  doy_2q         = [91,180]
  doy_3q         = [181,270]
  doy_4q         = [271,365]

  start_year     = 2012          ; Start and end year for the test run
  end_year       = 2013

  shp_check      = 0
  check_corr     = 0.9

  ;- --------------------------------------------------------- ;
  ; Set General processing options: number of cpus, overwriting, reporocessing ,etc.
  ;- --------------------------------------------------------- ;

  ncpus          = !CPU.hw_ncpu - 1 ; Find number of available cores - KEEP ONE FREE TO AVOID OVERLOAD !!!!!
  method         = 'parallel-line'; Processing method *"parallel-line" (faster - difficult to debug ! ))
  chunksize      = 50             ; Number of lines to assign to each core: the higher, the fastest, but the
  ; highest also the memory load !

  META           = 1             ; Specify if saving input multitemporal files or just use "virtual" in-memory files
  ; referring to the input single-data - avoids creating huge "physical" input files !

  force_rebuild  = 0             ; Flag. if set to 1 the input files are rebuilt (overwritten) even if already existing
  force_resmooth = 1             ; Flag. if set to 1 the smoothed file is rebuilt (overwritten) even if already existing
  overwrite_out  = 1             ; If = 0, then trying to overwrite existing outputs is NOT POSSIBLE
  fullout        = 1             ; Specify if also building an output file containing all bands - obsolete !

  debug          = 0             ; Specify if using "standard" processing for debug purposes.
  ; If set to 1, parallel processing is not used so that the debug is easier


  ; TODO: substitute with automatic resize of imagery on the basis of an input shape/raster file.
  resize         = 0
  resize_bbox    = [538,2028,1882,2692]

  ;- --------------------------------------------------- ;
  ; Set general variables ------------------------------ ;
  ;- --------------------------------------------------- ;

  ; Codenames of the input bands (inherited from MODIStsp R package (https://github.com/lbusett/MODIStsp)
  ; Used in the phase of building the "short" time series using pr_build_inputs
  ; The first item indicate whether to use EVI or NDVI -> Change: Removed NDII7 since it's useless !

  in_bands_or = ['EVI','NDFI','b3_Blue','Rely','UI','DOY','LST']

  ; Folders where MODIStsp stores the different parameters - do not touch !

  folder_suffixes_or = ['VI_16Days_250m','VI_16Days_250m','VI_16Days_250m','VI_16Days_250m', $
    'VI_16Days_250m','VI_16Days_250m','Surf_Temp_8Days_1Km']

  ; Nodata values used in MODIStsp for the different parameters

  nodatas_or = [32767,32767,32767,32767,255,255,32767,32767]

  ; If mapscape inputs are used, NODATA values for Surf_Temp are different ! To be changed in mapscape !!
  IF mapscape EQ 1 THEN  nodatas_or = [-3000,32767,-1000,-1,255,255,-1,0]

  ;- -------------------------------------------------- ;
  ; Create opts structure containing processing options ;
  ;- -------------------------------------------------- ;

  ; If working on test data, set options to default values and set paths to test datasets
  IF test_data EQ 1 THEN BEGIN

    ; Input folder where times series (Single date files) created with MODIStso are stored -
    ; (NOT required if input time series of the considered year are already available !)
    or_ts_folder = path_create([test_folder,'Original_MODIS'])

    ; Folder where time series to be used as input for phenorice will be stored -->  Intermediate multiband files
    in_ts_folder = path_create([test_folder, 'inputs'])

    ; Name of the input "land cover masking" file. Only pixels at "1" in this file are processed
    in_lc_file = path_create([test_folder,'Ancillary', $
      'Land_Cover', 'Mask.dat'])

    start_year = start_year     &         end_year = end_year   ; Start and end year for the analysis

    opts = { $
      ;---- General Options
      in_vi          : "EVI", $      ; Name of the VI index used
      in_flood_ind   : "NDFI", $     ; Name of flooding Index
      sel_seasons    : sel_seasons,$ ; Quarters to be analyzed
      doy_1q         : doy_1q, $     ; -> Start and end DOYs of each "season"
      doy_2q         : doy_2q, $
      doy_3q         : doy_3q, $
      doy_4q         : doy_4q, $
      win_dim_l      : 3, $          ; -> Smooth window dimentions (left & right) :
      win_dim_r      : 3, $
      cloud_clear    : 500 ,$        ; -> Blue band thresholds for cloud level identification :
      cloud_full     : 1800, $
      method         : method ,$     ; processing method - to be removed if parline is ok
      proc_year      : 0000, $       ; processing years
      ovr            : overwrite_out, $  ; flag: if 1, outputs are overwritten if existing
      mapscape       : mapscape, $    ; flag: if 1, try using mapscape smoothed array
      resize         : resize, $      ; dummy for now - to be used to allow processing of subsets
      resize_bbox    : resize_bbox, $
      force_resmooth : force_resmooth, $ ; flag - if 1, smoothing is redone even if file existing
      debug          : debug, $       ; flag - if 1, don't use parallel processing to allow debug
      fullout        : fullout, $     ; flag - if 1, build also an output file with all bands
      interleave     : "bip", $
      chunksize      : chunksize , $  ; Number of lines to be passed to each cpu - to be optimized to balance memory load and I/O time !

      ;---- Criteria on Average
      avg_check      : 1 ,$      ; Check if average VI Above threshold ? (1 = Yes)
      avg_thresh     : 5000, $   ; Threshold to be used on average

      ;-----  Criteria for maximum detection
      derivs         : 1, $      ; Check derivatives on left/right side of max ? ( 1 = Yes)
      derivs_opt     : [5,3],$   ; Check for at least 3 derivs on 5 on both sides
      max_value      : 1 ,$      ; Check if max above threshold ? ( 1 = Yes)
      vi_tr_max      : 4000 ,$   ; Threshold for max - if max below this value, it is discarded
      decrease       : 1      ,$ ; Check if max decreases below a threshold on a window on th right side ? ( 1 = Yes)
      decrease_win   : 16  ,$     ; Dimension of the decrease window (as number of 8 days periods)
      decrease_perc  : 0.50, $   ; Percentage decrease to be checked
      ;---- Criteria for minimum detection
      min_value      : 1, $      ; Check if min below threshold ? ( 1 = Yes)
      vi_tr_min      : 2500 ,$   ; max threshold for legal min (If min above this threshold it is discarded)
      growth         : 1      ,$ ; Check for positive derivatives after min ? ( 1 = Yes)
      growth_opt     : [5,3] ,$  ; First index: fimension of the window; second index: how many in the window must be postive for the min to be legit
      flood          : 1      ,$ ; Check for ocurrence of a flooding in a window of dim flood_win centered on min ? ( 1 = Yes)
      flood_win      : 16   ,$   ; dimension of win to be checked for flood (in DAYS)
      check_ndfi     : 1 , $     ; Do the check on NDFI ? (default to 1)
      max_after      : 1   ,$     ; Check if min is followed by a max in a win of dimension specified below ? ( 1 = Yes)
      max_aft_win    : [50/8,114/8],$;  First index: min number of compositing periods between min and max;
      lst            : 1   ,$           ; Check if min occurs in a period with LST above a given threshold ? ( 1 = Yes)
      lst_thresh     : 15,     $ ; Threshold for LST (in °C)

      ; criteria for vi shape checks
      shp_check      : shp_check, $
      check_shape_meth: "linear", $
      check_corr     : check_corr, $

      ;---- Selected outputs
      n_rice         : 1, $   ; Number of rice seasons
      max            : 0, $   ; DOY of Max EVI
      sow            : 1, $   ; DOY of sowing (min)
      hh             : 1, $   ; DOY of flowering (midpoint EVI Z 90th perc.)
      eos            : 1, $   ; EOS (decrease 50 %)
      int            : 1, $   ; Cumulated EVI between min and flowering
      maxvi          : 0, $   ; EVI at maximum
      minvi          : 0, $   ; EVI at minimum
      maxmin         : 1, $   ; Length of vegetative season
      eosmin         : 1,  $   ; Length of season (EOS to SOS)
      meta           : META, $
      force_rebuild  : force_rebuild, $
      ncpus          : ncpus $

    }

  ENDIF ELSE BEGIN

    ;- --------------------------------------------------------------- ;
    ;-  IF not run on test_data, then open the GUI to select parameters
    ;- --------------------------------------------------------------- ;

    ; TODO: Create a IDL Widget GUI
    R_GUI_function_path = programrootdir()+'Accessoires'+path_sep()+'Phenorice_GUI.R'
    launch_string = 'Rscript'+' '+ '"'+R_GUI_function_path +'"'+' "'+file_dirname(R_GUI_function_path)+ '"'
    spawn,launch_string, out_folder
    out_folder = strmid(out_folder, 5,(strlen(out_folder)-6))    ; Get the out_folder from the GUI

    ;- --------------------------------------------------------------- ;
    ; Read the input options from the txt file saved by Phenorice_GUI (TO be removed after switching to IDL GUI
    ;- --------------------------------------------------------------- ;

    IF out_folder EQ '' OR out_folder EQ 'ALS'THEN BEGIN
      print, 'User selected to quit'
      stop
    ENDIF
    options_file = out_folder+'/phenorice_options.txt'
    in_file = options_file
    in_opts_arr = ''
    line = ''

    openr, lun, in_file, /GET_LUN
    WHILE NOT eof(lun) DO BEGIN
      readf, lun, line
      in_opts_arr = [in_opts_arr, line]
    ENDWHILE

    free_lun, lun
    in_opts_arr[where(in_opts_arr EQ 'On')] = 1     ; Convert "Yes/No" to 0/1
    in_opts_arr[where(in_opts_arr EQ 'Off')] = 0

    or_ts_folder = in_opts_arr[1] ; Input folder where times series (Single date files) created with MODIStsp are stored - (NOT required if input time series of the considered year are already available !)

    in_ts_folder = in_opts_arr[4]   ; Folder where time series to be used as input for phenorice will be stored -->  Intermediate multiband files

    in_lc_file = in_opts_arr[3]         ; Name of the input "land cover masking" file. Only pixels at "1" in this file are processed

    out_filename = in_opts_arr[2]   ; Out file name (Actually, a prefixc to which indication about processing year is appended

    start_year = fix(in_opts_arr[33])     &         end_year = fix(in_opts_arr[34])   ; Start and end year for the analysis

    ; Options for selected seasons and strt/end doys from GUI

    sel_seasons = [fix(in_opts_arr[5]),fix(in_opts_arr[12]),fix(in_opts_arr[19]),fix(in_opts_arr[26])]    ;

    ; The R GUI converts the selected day/months into DOYS. Here I retrieve the doys and use them to
    ; define doy ranges where the maximums will be allowed to be in each quarter

    stard_doy_seas1 = in_opts_arr[10]   &   end_doy_seas1 = in_opts_arr[11]
    stard_doy_seas2 = in_opts_arr[17]   &   end_doy_seas2 = in_opts_arr[18]
    stard_doy_seas3 = in_opts_arr[24]   &   end_doy_seas3 = in_opts_arr[25]
    stard_doy_seas4 = in_opts_arr[31]   &   end_doy_seas4 = in_opts_arr[32]

    range_seas_1 = indgen(abs(fix(stard_doy_seas1)-fix(end_doy_seas1)-1))+stard_doy_seas1
    range_seas_2 = indgen(abs(fix(stard_doy_seas2)-fix(end_doy_seas2)-1))+stard_doy_seas2
    range_seas_3 = indgen(abs(fix(stard_doy_seas3)-fix(end_doy_seas3)-1))+stard_doy_seas3
    range_seas_4 = indgen(abs(fix(stard_doy_seas4)-fix(end_doy_seas4)-1))+stard_doy_seas4

    ; Check if the specified ranges "make sense". - NOTE: Ranges must NOT overlap and be "ordered" even if some of them are deselected ! (TO BE FIXED ! )
    ; Also, if season 1 is selected and with a negative starting month, then it must not overlap with the other seasons (that is, if I go back to novemeber last year
    ; to account for areas in which the Maximum can be between November and January, then the other ranges must go at maximum to novemeber,
    ; otherwise I will count twice the same "season", one for "previous" and one for "current " year

    order = sort([stard_doy_seas1,stard_doy_seas2,stard_doy_seas3,stard_doy_seas4])
    IF abs(max (order - [0,1,2,3])) NE 0  THEN BEGIN
      mes = dialog_message('Error in ordering of seasons doys - Please Check')
      stop
    ENDIF

    ; Check for overlapping between ranges

    Inters_1_2 = setintersection(range_seas_1, range_seas_2)
    Inters_2_3 = setintersection(range_seas_2, range_seas_3)
    Inters_3_4 = setintersection(range_seas_3, range_seas_4)

    IF (min([Inters_1_2,Inters_2_3,Inters_3_4]) NE -999) OR (max([Inters_1_2,Inters_2_3,Inters_3_4]) NE -999) THEN BEGIN
      mes =dialog_message('Selected periods are overlapping - please correct')
      stop
    ENDIF

    ; Check for possible overlapping between consecutive years

    IF (stard_doy_seas1 LT 0) THEN BEGIN
      range_seas1_cor = 365+range_seas_1
      IF (total(sel_seasons[0:1]) EQ 2)           THEN Inters_1_2_cor = setintersection(range_seas1_cor, range_seas_2)  ELSE inters_1_2_cor = -999
      IF ((sel_seasons [0] + sel_seasons[2] )EQ 2) THEN Inters_1_3_cor = setintersection(range_seas1_cor, range_seas_3) ELSE inters_1_3_cor = -999
      IF ((sel_seasons [0] + sel_seasons[3] )EQ 2) THEN Inters_1_4_cor = setintersection(range_seas1_cor, range_seas_4) ELSE inters_1_4_cor = -999

      IF  (min([Inters_1_2_cor,Inters_1_3_cor,Inters_1_4_cor]) NE -999) OR (max([Inters_1_2_cor,Inters_1_3_cor,Inters_1_4_cor]) NE -999) THEN BEGIN
        mes =dialog_message('Selected periods are overlapping - Quarter One includes maxima of previous year in a period potentially included in other quarters for current year !!! Please correct !')
        stop
      ENDIF

    ENDIF

    ; set-up processing options using data retrieved from GUI

    opts = { $

      ;---- General Options
      in_vi           : in_bands_or[0], $     ; Name of the index
      in_flood_ind    : in_bands_or[1], $      ; Name of NDVI Index
      sel_seasons     : sel_seasons,$
      doy_1q          : [stard_doy_seas1,end_doy_seas1],$ ; -> Start and end bands of each "season"
      doy_2q          : [stard_doy_seas2,end_doy_seas2],$
      doy_3q          : [stard_doy_seas3,end_doy_seas3],$
      doy_4q          : [stard_doy_seas4,end_doy_seas4],$
      win_dim_l       : 3 ,$         ; -> Smooth's window dimentions (left & right) :
      win_dim_r       : 3 ,$
      cloud_clear     : 500 ,$      ; -> Blu band thresholds for cloud level identification :
      cloud_full      : 1800, $
      method          : method, $
      proc_year       : 0000, $
      ovr             : overwrite_out, $
      mapscape        : mapscape, $
      resize          : resize, $
      resize_bbox     : resize_bbox, $
      force_resmooth  : force_resmooth, $
      debug           : debug, $
      fullout         : fullout, $
      interleave      : "bip", $
      chunksize       : chunksize , $

      ;---- Criteria on Average
      avg_check       : fix(in_opts_arr[35]) ,$ ; Check if average VI Above threshold ? (1 = Yes)
      avg_thresh      : fix(in_opts_arr[36]) ,$ ; Threshold to be used on average

      ;---- Criteria on Maximums
      derivs          : 1, $       ; Check derivatives on left/right side of max ? ( 1 = Yes)
      derivs_opt      : [5,3],$    ; Check for at least 3 derivs on 5 on both sides
      max_value       : fix(in_opts_arr[37])  ,$ ; Check if max above threshold ? ( 1 = Yes)
      vi_tr_max       : fix(in_opts_arr[38])  ,$ ; Threshold for max - if max below this value, it is discarded
      decrease        : fix(in_opts_arr[41])  ,$ ; Check if max decreases below a threshold on a window on th right side ? ( 1 = Yes)
      decrease_win    : fix(in_opts_arr[40])/8,$ ; Dimension of the decrease window
      decrease_perc   : float(in_opts_arr[39]),$ ; Percentage decrease to be checked

      ;---- Criteria on Minimums
      min_value       : fix(in_opts_arr[42]), $  ; Check if min below threshold ? ( 1 = Yes)
      vi_tr_min       : fix(in_opts_arr[43]) ,$  ; max threshold for legal min (If min above this threshold it is discarded)
      growth          : 1     ,$  ; Check for positive derivatives after min ? ( 1 = Yes)
      growth_opt      : [5,3] ,$  ; First index: fimension of the window; second index: how many in the window must be postive for the min to be legit
      flood           : fix(in_opts_arr[44]) ,$ ; Check for ocurrence of a flooding in a window of dim flood_win centered on min ? ( 1 = Yes)
      flood_win       : fix(in_opts_arr[45]) ,$ ; dimension of win to be checked for flood (in DAYS)
      check_ndfi      : 1 , $     ; Do the check on NDFI ? (default to 1)
      max_after       : fix(in_opts_arr[46]) ,$     ; Check if min is followed by a max in a win of dimension specified below ? ( 1 = Yes)
      max_aft_win     : [fix(in_opts_arr[47])/8,fix(in_opts_arr[48])/8],$;  First index: min number of compositing periods between min and max;
      ;  Second index: max number of compositing periods between min and max;
      lst             : fix(in_opts_arr[49]) ,$           ; Check if min occurs in a period with LST above a given threshold ? ( 1 = Yes)
      lst_thresh      : fix(in_opts_arr[50] ),$ ; Threshold for LST (in °C)
      
      ; criteria for vi shape checks
      shp_check      : shp_check, $
      check_shape_meth: "linear", $
      check_corr     : check_corr, $

      ;---- Selected outputs

      n_rice         : 1, $   ; Number of rice seasons
      max            : 0, $   ; DOY of Max EVI
      sow            : 1, $   ; DOY of sowing (min)
      hh             : 1, $   ; DOY of flowering (midpoint EVI Z 90th perc.)
      eos            : 1, $   ; EOS (decrease 50 %)
      int            : 1, $   ; Cumulated EVI between min and flowering
      maxvi          : 0, $   ; EVI at maximum
      minvi          : 0, $   ; EVI at minimum
      maxmin         : 1, $   ; Length of vegetative season
      eosmin         : 1,  $   ; Length of season (EOS to SOS)
      meta           : META, $
      force_rebuild  : force_rebuild, $
      ncpus          : ncpus $

    }


  ENDELSE ; END else on use of test data

  ;-----------------------------------------------------------------------------------------------------
  ; Start the processing --> Call the pr_build_inputs and then the pr_init_processing
  ; routines with a cycle on years
  ;-----------------------------------------------------------------------------------------------------

  ;- --------------------------------------------------------- ;
  ;-  Start Cycling on years
  ;- --------------------------------------------------------- ;
  ind_year = 0

  FOR proc_year = end_year, start_year, -1 DO BEGIN

    opts.proc_year = proc_year
    t1 = systime(2)   ; Get starting time
    print, "# ############################################ #"
    print, "# Working on Year: "+ string(proc_year)
    print, "# ############################################ #"

    ;- --------------------------------------------------------- ;
    ;-  Create input files starting from MODIStsp time series)
    ;- --------------------------------------------------------- ;
    print, "# BUILDING INPUT MULTITEMPORAL FILES"
    print, "# ############################################ #"
    
    ; Out file name (Actually, a prefixc to which indication about processing year is appended
    out_filename = path_create([file_dirname(programrootdir()), 'test_data', test_folder,'Outputs',(string(proc_year)).trim(),'Phenorice_out_'])
    file_mkdir,file_dirname(out_filename)

    ; Initialize structure of file names and of metaraster files (used if "META")
    out_files_list = {evi_file: "", ndfi_file:"", blue_file :"", $
      rely_file:"", ui_file: "", doy_file: "", lst_file:"", $
      quality_file: "", smooth_file : "" , lc_file : in_lc_file, $
      out_filename : out_filename}

    out_rast_list = {evi_file: obj_new(), ndfi_file:obj_new(), blue_file :obj_new(), $
      rely_file:obj_new(), ui_file: obj_new(), doy_file: obj_new(), lst_file:obj_new(), $
      quality_file: obj_new(), smooth_file : "" , lc_file : in_lc_file, $
      out_filename : out_filename}

    in_files = pr_build_inputs_v30(or_ts_folder, in_ts_folder, in_bands_or, in_bands_derived, out_filename, $
      folder_suffixes_or, opts, nodatas_or, META, out_files_list, out_rast_list, force_rebuild)

    T2=systime(1)
    print,"Time to build files: ", (string(t2-t1)).trim(), " seconds"

    ;- ---------------------------------------------------------- ;
    ;- Launch processsing: smoothing + pheno (if smoothing file exists,
    ;- the existing one is used unless opts.force_resmooth = 1)
    ;- -----------------------------------------------------------;
    print, "# ############################################ #"
    print, "# Smoothing and processing DATA "
    print, "# ############################################ #"

    IF (opts.method EQ "parallel-line") THEN results = pr_init_processing_v30(in_files, opts,out_rast_list, ind_year)

    print, "# ############################################ #"
    print, "#                   DONE !                     #"
    print, "# ############################################ #"

    print, "# Main PhenoRice processing"

    T2=systime(1)
    print,"Total processing time: ", (string(t2-t1)).trim(), " seconds"
    ind_year = ind_year +1
    heap_gc
  ENDFOR  ; End Cycle on years

END


