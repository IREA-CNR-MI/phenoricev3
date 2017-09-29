PRO pr_build_log_v30,  opts , out_filename, in_files
  COMPILE_OPT hidden
  COMPILE_OPT IDL2
  ;    - --------------------------------------------------------- ;
  ;  -  Write log information
  ;  - --------------------------------------------------------- ;
  file_mkdir,  file_dirname(out_filename)
  outlog_folder = path_create([file_dirname(out_filename), "log"])                       ; output folder for log
  file_mkdir,  file_dirname(outlog_folder)
  out_logfile = path_create([outlog_folder,"PhenoRice_Log_file"+strtrim(string(opts.proc_year),2)+".txt"])  ; output log file

  file_mkdir, file_dirname(out_logfile)

  openw , u_log , out_logfile , /get_lun
  printf, u_log, "-> Date of processing: " + systime() & printf, u_log
  printf, u_log, "------------------------------"
  printf, u_log, 'INPUT FILES:'
  printf, u_log, "-> VI file: "+in_files.EVI_file
  printf, u_log, "-> NDFI file: "+in_files.NDFI_file
  printf, u_log, "-> Pixel QA file: "+in_files.Quality_file
  printf, u_log, "-> LST file: "+in_files.LST_file
  printf, u_log, "-> Land Cover file: "+in_files.LC_File
  printf, u_log, ""
  printf, u_log, "------------------------------"
  printf, u_log, 'OUTPUT FILES:'
  printf, u_log, "-> Output File: "+file_dirname(out_filename)
  printf, u_log,''
  printf, u_log, "------------------------------"
  printf, u_log, 'PROCESSING YEAR:' + strtrim(string(opts.proc_year),2)
  printf, u_log, "------------------------------"
  printf, u_log,''
  printf, u_log, "------------------------------"
  printf, u_log, "INPUT PARAMETERS              "
  printf, u_log, "------------------------------"
  printf, u_log, "-> Processed Quarters: ", strtrim(string(opts.SEL_SEASONS[0]),2) + "-" + strtrim(string(opts.SEL_SEASONS[1]),2) $
    + "-" + strtrim(string(opts.SEL_SEASONS[2]),2) + "-" + strtrim(string(opts.SEL_SEASONS[3]),2)
  printf, u_log, "-> Start/End of 1st quarter: ", strtrim(opts.doy_1q[0],2) + " / " + strtrim(opts.doy_1q[1],2)
  printf, u_log, "-> Start/End of 2nd quarter: ", strtrim(opts.doy_2q[0],2) + " / " + strtrim(opts.doy_2q[1],2)
  printf, u_log, "-> Start/End of 3rd quarter: ", strtrim(opts.doy_3q[0],2) + " / " + strtrim(opts.doy_3q[1],2)
  printf, u_log, "-> Start/End of 4th quarter: ", strtrim(opts.doy_4q[0],2) + " / " + strtrim(opts.doy_4q[1],2)
  printf, u_log, ""
  IF (opts.derivs EQ 1) THEN BEGIN
    printf, u_log, "-> Derivatives checks on maxs", $
      strtrim(opts.derivs_opt[0]),' - ',strtrim(opts.derivs_opt[1])
  ENDIF ELSE BEGIN
    printf, u_log, "-> NO Derivatives checks on maxs"
  ENDELSE

  IF (opts.max_value EQ 1) THEN BEGIN
    printf, u_log, "-> Minimum EVI/NDVI value for a real rice MAX identification", $
      strtrim(opts.vi_tr_max)
  ENDIF ELSE BEGIN
    printf, u_log, "-> NO check on Maximum absolute Value"
  ENDELSE

  IF (opts.MAT_CHECK EQ 1) THEN BEGIN
    printf, u_log, "-> 'max to eos' length  and % for decrease check on eos", $
      strtrim(opts.MAT_WIN[0])*8 - strtrim(opts.MAT_WIN[1])*8, ' - ',strtrim(opts.decrease_perc)
  ENDIF ELSE BEGIN
    printf, u_log, "-> NO check on decrease in senescence"
  ENDELSE

  IF (opts.min_value EQ 1) THEN BEGIN
    printf, u_log, "-> Maximum EVI/NDVI value for a real rice MIN identification", $
      strtrim(opts.vi_tr_min)
  ENDIF ELSE BEGIN
    printf, u_log, "-> NO check on Minimum absolute Value"
  ENDELSE

  IF (opts.growth EQ 1) THEN BEGIN
    printf, u_log, "-> Check for derivatives after min", $
      strtrim(opts.growth_opt [0]), ' - ',strtrim(opts.growth_opt [1])
  ENDIF ELSE BEGIN
    printf, u_log, "->  Check for derivatives after min"
  ENDELSE

  IF (opts.flood EQ 1) THEN BEGIN
    printf, u_log, "-> Check for flooding - width of window", $
      strtrim(opts.flood_win )
  ENDIF ELSE BEGIN
    printf, u_log, "-> NO Check for flooding"
  ENDELSE

  IF (opts.max_after EQ 1) THEN BEGIN
    printf, u_log, "-> Range for MAX position after min", $
      strtrim(opts.max_aft_win[0]),' - ',strtrim(opts.max_aft_win[1])
  ENDIF ELSE BEGIN
    printf, u_log, "-> NO Check for MAX position after min"
  ENDELSE

  IF (opts.lst EQ 1) THEN BEGIN
    printf, u_log, "-> Minimum LST value for legal min", $
      strtrim(opts.lst_thresh)
  ENDIF ELSE BEGIN
    printf, u_log, "-> NO Check for Minimum LST value for legal min"
  ENDELSE

  close, u_log & free_lun,u_log

END
