; Title pr_main
;:Description
; Main launching script for the PHENORICE v3.0 application
;   --> Calls the GUI for selecting processing parameters
;   --> Launches the script for creating input time series (pr_build_inputs_v3_filler.pro)
;   --> Launch processing (pr_process_v3)

;Details
;
;:RETURNS:
;
;:AUTHOR: Lorenzo Busetto, phD (2014)
;  #' email: busetto.l@irea.cnr.it
;
;License GPL(>2)
;-


compile_opt idl2
t1 = systime(1) & ddmm = strcompress(strmid(t1,4,6),/REMOVE_ALL) ; ddmm = date of processing run

; Flags for particular processing

mapscape = 0      ; Specify to use "mapscape" input files --> Leads to changes in NODATA values and to not trying to generate
                  ; the smoothed file !!!
                  

test_data = 1     ; Leads to using default input data and parameters (for testing purposes)
test_folder = 'IT_Full'
old_approach = 1  ; Leads to using the "old" (e.g., July 2016) computation algorithms so to check differences with v3.0

;ENVI

; TODO: substitute with automatic resize of imagery on the basis of an input shape/raster file.
resize = 0
resize_bbox = [538,2028,1882,2692]

;---------- ----

;- --------------------------------------------------------- ;
;-  Set general variables -----------------------------------
;- --------------------------------------------------------- ;

in_bands_or = ['EVI','NDFI','b3_Blue','Rely','UI','DOY','LST']  ; Codenames of the input bands (inherited from the MODIStsp R package (https://github.com/lbusett/MODIStsp)
; Used in the phase of building the "short" time series using pr_build_inputs
; The first item indicate whether to use EVI or NDVI -> Change: Removed NDII7 since it's useless !

; in_bands_derived =['QUALITY','SNOW','LC','SEASON']                ; Leftover of previous scheme -  check if removal is possible.

folder_suffixes_or = ['VI_16Days_250m','VI_16Days_250m','VI_16Days_250m','VI_16Days_250m', $      ; Folders where MODIStsp stores the different parameters
  'VI_16Days_250m','VI_16Days_250m','Surf_Temp_8Days_1Km']

nodatas_or = [32767,32767,32767,32767,255,255,32767,32767]

if mapscape EQ 1 then  nodatas_or = [32767,32767,32767,32767,255,255,32767,-1]      ; If mapscape inputs are used, NODATA values for Surf_Temp are different ! To be changed in mapscape

;- --------------------------------------------------------- ;
;-  Set Input / output files and folders (retrieve from GUI selection)
;- --------------------------------------------------------- ;

if test_data EQ 0 then begin  ; IF not run on test_data, then open the GUI to select parameters

  ; TODO: Create a IDL Widget GUI
  R_GUI_function_path = ProgramRootDir()+'Source'+path_sep()+'phenorice'+path_sep()+'source'+path_sep()+'Accessoires'+path_sep()+'Phenorice_GUI.R'
  launch_string = 'Rscript'+' '+ '"'+R_GUI_function_path +'"'+' "'+file_dirname(R_GUI_function_path)+ '"'
  spawn,launch_string, out_folder
  out_folder = strmid(out_folder, 5,(strlen(out_folder)-6))    ; Get the out_folder from the GUI

  ;---------- ----
  ; Read the input options from the txt file saved by Phenorice_GUI (TO be removed after switching to IDL GUI
  ;---------- ----
  if out_folder EQ '' OR out_folder EQ 'ALS'then begin
    print, 'User selected to quit'
    stop
  endif
  options_file = out_folder+'/phenorice_options.txt'
  in_file = options_file
  in_opts_arr = ''
  line = ''

  openr, lun, in_file, /GET_LUN
  while not eof(lun) do begin
    readf, lun, line
    in_opts_arr = [in_opts_arr, line]
  endwhile

  free_lun, lun
  in_opts_arr[where(in_opts_arr eq 'On')] = 1     ; Convert "Yes/No" to 0/1
  in_opts_arr[where(in_opts_arr eq 'Off')] = 0

  or_ts_folder = in_opts_arr[1] ; Input folder where times series (Single date files) created with MODIStso are stored - (NOT required if input time series of the considered year are already available !)

  in_ts_folder = in_opts_arr[4]   ; Folder where time series to be used as input for phenorice will be stored -->  Intermediate multiband files

  in_lc_file = in_opts_arr[3]         ; Name of the input "land cover masking" file. Only pixels at "1" in this file are processed

  out_filename = in_opts_arr[2]   ; Out file name (Actually, a prefixc to which indication about processing year is appended

  start_year = fix(in_opts_arr[33])     &         end_year = fix(in_opts_arr[34])   ; Start and end year for the analysis

  ; Options for selected seasons and strt/end doys from GUI

  sel_seasons = [fix(in_opts_arr[5]),fix(in_opts_arr[12]),fix(in_opts_arr[19]),fix(in_opts_arr[26])]    ;

  ; The R GUI converts the selected day/months into DOYS. Here I retrieve the doys and convert the start and end doysto start and end
  ; "indexes" of MODIS images within the image stack

  stard_doy_seas1=  in_opts_arr[10]   &   end_doy_seas1=  in_opts_arr[11]
  stard_doy_seas2=  in_opts_arr[17]   &   end_doy_seas2=  in_opts_arr[18]
  stard_doy_seas3=  in_opts_arr[24]   &   end_doy_seas3=  in_opts_arr[25]
  stard_doy_seas4=  in_opts_arr[31]   &   end_doy_seas4=  in_opts_arr[32]

  range_seas_1 = indgen(abs(fix(stard_doy_seas1)-fix(end_doy_seas1)-1))+stard_doy_seas1
  range_seas_2 = indgen(abs(fix(stard_doy_seas2)-fix(end_doy_seas2)-1))+stard_doy_seas2
  range_seas_3 = indgen(abs(fix(stard_doy_seas3)-fix(end_doy_seas3)-1))+stard_doy_seas3
  range_seas_4 = indgen(abs(fix(stard_doy_seas4)-fix(end_doy_seas4)-1))+stard_doy_seas4

  ; Check if the specified ranges "make sense". - NOTE: Ranges must NOT overlap and be "ordered" even if some of them are deselected ! (TO BE FIXED ! )
  ; Also, if season 1 is selected and with a negative starting month, then it must not overlap with the other seasons (taht is, if I go back to novemeber last year
  ; to account for areas in which the Maximum can be between November and January, then the other ranges must go at mkaximum to novemeber,
  ; otherwise I will count twice the same "season", one for "previous" and one for "current " year

  order = sort([stard_doy_seas1,stard_doy_seas2,stard_doy_seas3,stard_doy_seas4])
  if abs(max (order - [0,1,2,3])) NE 0  then begin
    mes = DIALOG_MESSAGE('Error in ordering of seasons doys - Please Check')
    stop
  endif

  ; Check for overlapping between ranges

  Inters_1_2 = SetIntersection(range_seas_1, range_seas_2)
  Inters_2_3 = SetIntersection(range_seas_2, range_seas_3)
  Inters_3_4 = SetIntersection(range_seas_3, range_seas_4)

  if (min([Inters_1_2,Inters_2_3,Inters_3_4]) NE -999) OR (max([Inters_1_2,Inters_2_3,Inters_3_4]) NE -999) then begin
    mes =DIALOG_MESSAGE('Selected periods are overlapping - please correct')
    stop
  endif

  if (stard_doy_seas1 LT 0) then begin
    range_seas1_cor = 365+range_seas_1
    if (total(sel_seasons[0:1]) EQ 2)  then Inters_1_2_cor= SetIntersection(range_seas1_cor, range_seas_2) else inters_1_2_cor = -999
    if ((sel_seasons [0] + sel_seasons[2] )EQ 2) then Inters_1_3_cor = SetIntersection(range_seas1_cor, range_seas_3) else inters_1_3_cor = -999
    if ((sel_seasons [0] + sel_seasons[3] )EQ 2) then Inters_1_4_cor = SetIntersection(range_seas1_cor, range_seas_4) else inters_1_4_cor = -999

    if  (min([Inters_1_2_cor,Inters_1_3_cor,Inters_1_4_cor]) NE -999) OR (max([Inters_1_2_cor,Inters_1_3_cor,Inters_1_4_cor]) NE -999) then begin
      mes =DIALOG_MESSAGE('Selected periods are overlapping - Quarter One includes maxima of previous year in a period potentially included in other quarters for current year !!! Please correct !')
      stop
    endif

  endif

  proc_opts = { $
    in_VI: in_bands_or[0], $     ; Name of the index
    in_flood_ind : in_bands_or[1], $      ; Name of NDVI Index
    sel_seasons : sel_seasons,$
    doy_1q : [stard_doy_seas1,end_doy_seas1],$                          ; -> Start and end bands of each "season"
    doy_2q : [stard_doy_seas2,end_doy_seas2],$
    doy_3q : [stard_doy_seas3,end_doy_seas3],$
    doy_4q : [stard_doy_seas4,end_doy_seas4],$
    win_dim_l : 3 ,$         ; -> Smooth's window dimentions (left & right) :
    win_dim_r : 3 ,$
    cloud_clear : 500 ,$      ; -> Blu band thresholds for cloud level identification :
    cloud_full  : 1800, $
    method : "parallel_line" $
  }

  ; Criteria for average VI check
  avg_criteria = { $
    avg_check : fix(in_opts_arr[35]) ,$   ; Check if average VI Above threshold ? (1 = Yes)
    avg_thresh : fix(in_opts_arr[36]) $; Threshold to be used on average
  }

  ; Criteri for maximum detection
  max_criteria = { $
    derivs: 1, $           ; Check derivatives on left/right side of max ? ( 1 = Yes)
    derivs_opt: [5,3],$    ; Check for at least 3 derivs on 5 on both sides
    max_value: fix(in_opts_arr[37]) ,$        ; Check if max above threshold ? ( 1 = Yes)
    vi_tr_max : fix(in_opts_arr[38]) ,$    ; Threshold for max - if max below this value, it is discarded
    decrease: fix(in_opts_arr[41])      ,$    ; Check if max decreases below a threshold on a window on th right side ? ( 1 = Yes)
    decrease_win: fix(in_opts_arr[40])/8  ,$    ; Dimension of the decrease window
    decrease_perc: float(in_opts_arr[39]) $; Percentage decrease to be checked
  }

  ; Set the criteria for minimum detection
  min_criteria = { $
    min_value: fix(in_opts_arr[42]), $     ; Check if min below threshold ? ( 1 = Yes)
    vi_tr_min : fix(in_opts_arr[43]) ,$   ; max threshold for legal min (If min above this threshold it is discarded)
    growth : 1      ,$    ; Check for positive derivatives after min ? ( 1 = Yes)
    growth_opt: [5,3] ,$  ; First index: fimension of the window; second index: how many in the window must be postive for the min to be legit
    flood: fix(in_opts_arr[44])      ,$      ; Check for ocurrence of a flooding in a window of dim flood_win centered on min ? ( 1 = Yes)
    flood_win: fix(in_opts_arr[45])   ,$    ; dimension of win to be checked for flood (in DAYS)
    check_NDFI: 1 , $     ; Do the check on NDFI ? (default to 1)
    check_NDII: 0 , $     ; Do the check on NDII (Xiao) ? Default to 0
    max_after: fix(in_opts_arr[46])   ,$     ; Check if min is followed by a max in a win of dimension specified below ? ( 1 = Yes)
    max_aft_win : [fix(in_opts_arr[47])/8,fix(in_opts_arr[48])/8],$;  First index: min number of compositing periods between min and max;
    ;  Second index: max number of compositing periods between min and max;
    LST: fix(in_opts_arr[49])   ,$           ; Check if min occurs in a period with LST above a given threshold ? ( 1 = Yes)
    LST_thresh: fix(in_opts_arr[50] )     $; Threshold for LST (in °C)
  }

endif else begin   ; On test run, set parameters to default values

  or_ts_folder = ProgramRootDir() + 'data' + path_sep() + test_folder + path_sep() + 'Original_MODIS'; Input folder where times series (Single date files) created with MODIStso are stored - (NOT required if input time series of the considered year are already available !)

  in_ts_folder = ProgramRootDir() + 'data' + path_sep() + test_folder + path_sep() + 'inputs'   ; Folder where time series to be used as input for phenorice will be stored -->  Intermediate multiband files

  in_lc_file = ProgramRootDir() + 'data' + path_sep() + test_folder + path_sep() + 'Ancillary' + path_sep() + 'Land_Cover' + path_sep() + 'Mask.dat'         ; Name of the input "land cover masking" file. Only pixels at "1" in this file are processed

  out_filename = ProgramRootDir() + 'data/Outputs'; Out file name (Actually, a prefixc to which indication about processing year is appended

  start_year = 2015     &         end_year = 2015   ; Start and end year for the analysis

  proc_opts = { $
    in_VI: "EVI", $     ; Name of the index
    in_flood_ind : "NDFI", $      ; Name of NDVI Index
    sel_seasons : [1,1,1,0],$
    doy_1q : [0,90],$                          ; -> Start and end bands of each "season"
    doy_2q : [91,180],$
    doy_3q : [181,270],$
    doy_4q : [270,290],$
    win_dim_l : 3 ,$         ; -> Smooth's window dimentions (left & right) :
    win_dim_r : 3 ,$
    cloud_clear : 500 ,$      ; -> Blu band thresholds for cloud level identification :
    cloud_full  : 1800, $
    method : "parallel_line" $
  }

  ; Criteria for average VI check
  avg_criteria = { $
    avg_check : 1 ,$   ; Check if average VI Above threshold ? (1 = Yes)
    avg_thresh : 5000 $; Threshold to be used on average
  }

  ; Criteri for maximum detection
  max_criteria = { $
    derivs: 1, $           ; Check derivatives on left/right side of max ? ( 1 = Yes)
    derivs_opt: [5,3],$    ; Check for at least 3 derivs on 5 on both sides
    max_value: 1 ,$        ; Check if max above threshold ? ( 1 = Yes)
    vi_tr_max : 4000 ,$    ; Threshold for max - if max below this value, it is discarded
    decrease: 1      ,$    ; Check if max decreases below a threshold on a window on th right side ? ( 1 = Yes)
    decrease_win: 8  ,$    ; Dimension of the decrease window
    decrease_perc: 0.50 $; Percentage decrease to be checked
  }

  ; Set the criteria for minimum detection
  min_criteria = { $
    min_value: 1, $     ; Check if min below threshold ? ( 1 = Yes)
    vi_tr_min : 2500 ,$   ; max threshold for legal min (If min above this threshold it is discarded)
    growth : 1      ,$    ; Check for positive derivatives after min ? ( 1 = Yes)
    growth_opt: [5,3] ,$  ; First index: fimension of the window; second index: how many in the window must be postive for the min to be legit
    flood: 1      ,$      ; Check for ocurrence of a flooding in a window of dim flood_win centered on min ? ( 1 = Yes)
    flood_win: 16   ,$    ; dimension of win to be checked for flood (in DAYS)
    check_NDFI: 1 , $     ; Do the check on NDFI ? (default to 1)
    max_after: 1   ,$     ; Check if min is followed by a max in a win of dimension specified below ? ( 1 = Yes)
    max_aft_win : [50/8,114/8],$;  First index: min number of compositing periods between min and max;
    ;  Second index: max number of compositing periods between min and max;
    LST: 1   ,$           ; Check if min occurs in a period with LST above a given threshold ? ( 1 = Yes)
    LST_thresh: 15     $; Threshold for LST (in °C)
  }
endelse

;-----------------------------------------------------------------------------------------------------
; Start the processing --> Call the pr_process routine with a cycle on years and selected parameters
;-----------------------------------------------------------------------------------------------------

;- --------------------------------------------------------- ;
;-  Start Cycling on years
;- --------------------------------------------------------- ;
for proc_year = end_year, start_year, -1 do begin

  t1 = systime(2)   ; Get starting time
  print, "# ############################################ #"
  print, "# Working on Year: "+ string(proc_year)
  print, "# ############################################ #"

  ;- --------------------------------------------------------- ;
  ;-  Creates input files starting from "long" time series)
  ;- --------------------------------------------------------- ;
  print, "# BUILDING INPUT MULTITEMPORAL FILES"
  smooth_dirname = in_ts_folder+path_sep()+strtrim(proc_year,2)+path_sep()+'VI_Smoothed'
  smooth_file = smooth_dirname+path_sep()+'VI_smooth_'+strtrim(string(proc_year), 2)+'.dat'
  FILE_MKDIR, smooth_dirname
  
  out_files_list = {EVI_file: "", NDFI_file:"", Blue_file :"", $
    Rely_file:"", UI_file: "", DOY_file: "", LST_file:"", $
    Quality_file: "", Smooth_file : smooth_file , LC_File : in_lc_file}    ; Initialize array of output file names
    

  in_files = pr_build_inputs_v30(or_ts_folder, in_ts_folder, in_bands_or, in_bands_derived , proc_year, out_filename,folder_suffixes_or, proc_opts, nodatas_or,$
    resize, resize_bbox, min_criteria, max_criteria, mapscape, META, out_files_list)


  ;- ---------------------------------------------------------- ;
  ;-  Smooth Time series (If smoothed file doesn't exist already)
  ;- -----------------------------------------------------------;
  ; if (NOT EXISTS Smoothed file --> Launch pr_smoothv30
  ;- ---------------------------------------------------------- ;

  print, "# ############################################ #"
  print, "# SMOOTHING VI DATA
  print, "# ############################################ #"

  ;if (FILE_TEST(in_files.Smooth_file) EQ 0) then begin

  if (proc_opts.method EQ "normal") then smooth_file = pr_smooth_v30(in_files, proc_opts, proc_year, mapscape)
  
  if (proc_opts.method EQ "parallel_pixel") then smooth_file = pr_smooth_v30_parpix(in_files, proc_opts, proc_year, mapscape)
  
  if (proc_opts.method EQ "parallel_line") then smooth_file = pr_smooth_v30_parline(in_files, proc_opts, proc_year, mapscape)
      
    T2=systime(1)
;    print,"start processing:", t1
;    print,"End processing:",  t2
    print, T2-t1, ' Seconds'
    print, "done!"
    heap_gc

  ;endif else print ,"Smoothed VI file already esisting"

  ;- ---------------------------------------------------------- ;
  ;-  Launch processing - pr_process v30 function
  ;- ---------------------------------------------------------- ;
  
  print, "# ############################################ #"
  print, "# PERFORMING PHENOLOGICAL ANALYSIS
  print, "# ############################################ #"

  print, "# Main PhenoRice processing"
;  outrast_folder = path_create([out_folder, strtrim(proc_year,2),"raster"])  ; output folder for images
;  outlog_folder = path_create([out_folder, "log"])                            ; output folder for log
;  out_logfile = path_create([outlog_folder,(out_file_prefix + "_Log_file.txt")])  ; output log file
;
;  out_proc = pr_process_v30(in_files , out_filename , pr_opts , $
;    avg_criteria,max_criteria, min_criteria, out_logfile , proc_year , note, resize, $
;    start_x, end_x,start_y, end_y, mapscape)
;
;  T2=systime(2)
;  print,"start processing:", T1
;  print,"start processing:", T2
;  print, t2-t1
;  print, "done!"
  heap_gc
endfor  ; End Cycle on years

;- --------------------------------------------------------- ;
;-  Launch postprocessing (TBD)
;- --------------------------------------------------------- ;

end
