;
; Title pr_process_v21
;:Description
;
;Details:

; :Params:
;    in_files
;    outrast_folder
;    proc_opts
;    max_criteria
;    min_criteria
;    out_logfile
;    proc_year
;    note
;
;
;
;:RETURNS:
;
;:AUTHOR: Lorenzo Busetto, phD (2014)
;  #' email: busetto.l@@irea.cnr.it
;
;License GPL(>2)
;-
FUNCTION pr_init_process_v30_parline, in_files , out_filename , proc_opts , $
  avg_criteria,max_criteria, min_criteria, out_logfile, proc_year, resize, resize_bbox, mapscape, overwrite_out

  COMPILE_OPT idl2

  add_outfiles = 0
  t1 = systime(1)

  e = ENVI(/HEADLESS)

  pr_build_log_v30, out_filename, proc_opts, in_files, max_criteria, min_criteria, proc_year  ; Create log file

  selquarts = where(proc_opts.SEL_SEASONS EQ 1)

  ;- --------------------------------------------------------- ;
  ; Removed resize ! Will have to be implemented while
  ; building the  inputs !!!!!
  ;- --------------------------------------------------------- ;


  ;  IF (resize EQ 1) THEN BEGIN
  ;    dims_res = [-1, start_x, end_x, start_y, end_y]
  ;    ; ENVI_DOIT, 'CONVERT_INPLACE_DOIT',dims = dims_res, fid = r_fid, o_interleave = 2, pos = indgen(70)
  ;    in_file_lc_temp= file_dirname(in_files.lc)+path_sep()+'Resize_'+file_basename(in_files.lc)
  ;
  ;    envi_open_file , in_files.lc  , r_fid=lc_fid, /no_realize
  ;    ENVI_DOIT, 'RESIZE_DOIT', DIMS=dims_res, FID=lc_fid, OUT_NAME= in_file_lc_temp, POS=0,rfact = [1,1], r_fid = r_fid
  ;    ENVI_FILE_MNG, ID=lc_fid, /REMOVE
  ;    in_files.lc = in_file_lc_temp
  ;    lc_fid = r_fid
  ;
  ;  ENDIF

  ;- --------------------------------------------------------- ;
  ;-  Open Input files and get necessary infos
  ;- --------------------------------------------------------- ;

  in_vi      = e.OpenRaster(in_files.EVI_File)
  in_lst     = e.OpenRaster(in_files.LST_FILE)
  in_lc      = e.OpenRaster(in_files.LC_FILE)
  in_smooth  = e.OpenRaster(in_files.Smooth_File)
  in_doy  = e.OpenRaster(in_files.Smooth_File)

  nb = in_vi.NBANDS
  ncols = in_vi.NCOLUMNS
  nrows = in_vi.NROWS
  proj = in_vi.SPATIALREF

  getheader = ENVITask('RasterMetadataItem')
  getheader.INPUT_RASTER = in_vi
  ; Get the acquisition times from the header & retrieve acquisition years
  getheader.KEY = 'time' &  getheader.execute
  times = getheader.value
  years = times.substring(0,3)
  prev_y =  where(years EQ proc_year -1)

  ; Get the acquisition DOYS from the header
  getheader.KEY = 'Wavelength' &  getheader.execute
  doys_reg = getheader.value

  ; If we want to create also the "arrays" to check pixels -
  ; I'll probably remove this and introduce a simple "checking" routine !

  IF add_outfiles EQ 1 THEN BEGIN
    outfile_maxarray = out_filename.Remove(-4) + '_maxarray.dat'
    outfile_minarray = out_filename.Remove(-4) + '_minarray.dat'
  ENDIF

  ;- --------------------------------------------------------- ;
  ; Initialize "reused" variables before the FOR loop !!!! avoid doing same thing on each cycle !!!
  ;- --------------------------------------------------------- ;

  ; Criteria on maximums - set indexes for the windows of the different checks and the threshold
  der_ind = indgen (max_criteria.derivs_opt [0])+1  ; Positions where derivatives should be checked for MAX identification
  maxdec_wid = max_criteria.decrease_win ; Positions where derivatives should be checked for VI decrease
  maxdec_ind = indgen (max_criteria.decrease_win)+1 ; Positions where derivatives should be checked for VI decrease
  maxthresh = max_criteria.vi_tr_max    ; Threshold for maximum
  maxdec_perc = max_criteria.decrease_perc    ; % of max decrease

  check_arr_max = [max_criteria.derivs,max_criteria.max_value, max_criteria.decrease]  ; Array specisying which max  criteria should be considered

  ; Criteria on minimums - set indexes for the windows of the different checks and the threshold
  ;- ---------------
  minthresh = min_criteria.vi_tr_min
  growth_ind = indgen (min_criteria.growth_opt [0])+1 ; positions to be checked to identify consistent growth after minimum
  growth_wid = min_criteria.growth_opt [0]  ; width of window  to be checked to identify consistent growth after minimum
  growth_thresh = min_criteria.growth_opt [1] ; number of positive der. after minimum needed for min. to be legit
  vi_flood_wid = min_criteria.flood_win           ; dimension of win to be checked for flood
  vi_flood_ind = indgen (min_criteria.flood_win )-min_criteria.flood_win/2     ; positions to be checked to identify flooding around minimum
  check_NDFI = min_criteria.check_NDFI
  vi_max_ind = indgen(min_criteria.max_aft_win[1] - min_criteria.max_aft_win[0]+1)+min_criteria.max_aft_win[0] ; positions to be checked to identify if max is present in suitable period after min
  LST_thresh = min_criteria.lst_thresh

  check_arr_min = [min_criteria.min_value,min_criteria.growth, min_criteria.flood, $  ; Array specisying which min criteria should be considered
    min_criteria.max_after, min_criteria.lst]

  ; band positions for the differnt "quarters" for max positioning within quarters
  temp_pos_quarters_max = intarr(nb)-1

  pos_quart_max = intarr(nb)-1  ; Initialize array  with -1
  n_sel_season = total(proc_opts.sel_seasons)
  pos_legit_maxs = intarr(nb)
  IF (proc_opts.sel_seasons[0] EQ 1) THEN pos_quart_max [where (doys_reg GE proc_opts.doy_1q[0]-7 AND  doys_reg LE proc_opts.doy_1q[1])]  = 0  ; set the "season" according to doy
  IF (proc_opts.sel_seasons[1] EQ 1) THEN pos_quart_max [where (doys_reg GE proc_opts.doy_2q[0]-7 AND  doys_reg LE proc_opts.doy_2q[1])]  = 1
  IF (proc_opts.sel_seasons[2] EQ 1) THEN pos_quart_max [where (doys_reg GE proc_opts.doy_3q[0]-7 AND  doys_reg LE proc_opts.doy_3q[1])]  = 2
  IF (proc_opts.sel_seasons[3] EQ 1) THEN pos_quart_max [where (doys_reg GE proc_opts.doy_4q[0]-7 AND  doys_reg LE proc_opts.doy_4q[1])]  = 3

  pos_legit_maxs = pos_quart_max NE -1
  ;doys_reg = float(doys_reg +8)                        ; Move the "reference" doy for each composition period from day 1 to day 8


  ;- --------------------------------------------------------- ;
  ;-  Start of processing - divide load among cores !
  ;- --------------------------------------------------------- ;

  IF ((FILE_TEST(out_filename) EQ 0) OR (overwrite_out EQ 1 )) THEN BEGIN

    IF (FILE_TEST(out_filename) EQ 1) THEN BEGIN
      FILE_DELETE, out_filename
      print, " Deleted old output ! "
    ENDIF

  ENDIF

  lines = indgen(in_vi.nrows)
  print , '--- PhenoRice VI-smoothing - pleas wait ! ---'

  ;- ----------------------------------------------------------------------------
  ;-  Start cycling on line blocks to do the smoothing - parallel implementation -
  ;-  http://daedalus.as.arizona.edu/wiki/ParallelIDL
  ;- ----------------------------------------------------------------------------
  nCPUs = !CPU.HW_NCPU-1      ; Find number of available cores - set CPUs to N-1
  nCPUs = 2
  oBridge = objarr(nCPUs-1)

  FOR i=0, nCPUs-1 DO BEGIN   ; cycle on CPUs

    ; Divide work among CPUs: find lines to be assigned to CPU N
    range = [n_elements(lines)/nCPUs*i,n_elements(lines)/nCPUs*(i+1)-1]

    IF i EQ n_elements(oBridge) THEN range[1] = n_elements(lines)-1  ; On last CPU assign the remaining lines

    IF i EQ nCPUs-1 THEN BEGIN  ; On second-to-last CPU, use the main thread
      sel_lines = [range[0],range[1]]
      lines = indgen(sel_lines[1] - sel_lines[0] + 1)
      data_lc = in_lc.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_VI = in_vi.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_Smooth = in_smooth.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_DOY = in_doy.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])

      results = pr_process_pixel_v30_parline(lines, data_lc, data_VI, data_QA,data_DOY, nb, ncols, doys_reg, years, prev_y)

    ENDIF ELSE BEGIN  ; For each CPU, get the necessary data
      sel_lines = [range[0],range[1]]
      lines_ind = indgen(sel_lines[1] - sel_lines[0] + 1)
      data_lc = in_lc.GetData(SUB_RECT=[0,sel_lines[0],NCOLS-1,sel_lines[1]])
      data_VI = in_vi.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_smooth = in_smooth.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_DOY = in_doy.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])

      oBridge[i] = obj_new('IDL_IDLBridge')



      ; provide necessary variables to the CPUs
      oBridge[i]->SetVar, 'inlines', lines
      struct_pass, proc_opts, oBridge[i]
      struct_pass, max_criteria, oBridge[i]
      struct_pass, min_criteria, oBridge[i]
      oBridge[i]->SetVar, 'data_lc', data_lc
      oBridge[i]->SetVar, 'data_VI', data_VI
      oBridge[i]->SetVar, 'data_smooth', data_smooth
      oBridge[i]->SetVar, 'data_DOY', data_DOY
      oBridge[i]->SetVar, 'years', years
;      oBridge[i]->SetVar, 'proc_year', proc_year
      oBridge[i]->SetVar, 'nb', nb
      oBridge[i]->SetVar, 'ncols', ncols
      oBridge[i]->SetVar, 'doys_reg', doys_reg
;      oBridge[i]->SetVar, 'proc_year', proc_year
      oBridge[i]->SetVar, 'prev_y', prev_y
;      oBridge[i]->SetVar, 'mapscape', mapscape

      ; define the location of the smoothing function and execute it on the different bridges
      oBridge[i]->Execute, ".r " + FILE_DIRNAME(ProgramRootDir())+PATH_SEP()+"Accessoires"+PATH_SEP()+"pr_smooth_pixel_v30_parline.pro"
      oBridge[i]->Execute, ".r " + "!PATH = EXPAND_PATH('<IDL_DEFAULT>:+" + FILE_DIRNAME(ProgramRootDir())+PATH_SEP()
      oBridge[i]->Execute, "results = pr_process_v30_parline(ilines, data_lc, data_VI, data_QA,data_DOY, nb, ncols, doys_reg, years, prev_y)", /nowait

    ENDELSE
  ENDFOR

  ; Don't know what this means....

  notdone = 1
  WHILE notdone DO BEGIN
    done=0
    FOR i=0, n_elements(oBridge)-1 DO $
      done = done+oBridge[i]->Status()
    IF done EQ 0 THEN notdone=done
  ENDWHILE

  ;- ------------------------------------------------------------------
  ;- Reconcile results of the different cores
  ;- ------------------------------------------------------------------

  FOR i=0, n_elements(oBridge)-1 DO BEGIN
    results = [[[OBRIDGE[N_ELEMENTS(OBRIDGE)-1-i]->GETVAR('results')]],[[results]]]
    obj_destroy, oBridge[n_elements(oBridge)-1-i]
  ENDFOR

  return, 'DONE'

END
