pro pr_build_log_v22, out_filename,out_logfile, pr_opts,out_file_prefix,note,in_files,max_criteria,min_criteria

  ;    - --------------------------------------------------------- ;
  ;  -  Write log information
  ;  - --------------------------------------------------------- ;
  file_mkdir,  file_dirname(out_filename)
  file_mkdir, file_dirname(out_logfile)

  openw, u_log , out_logfile , /get_lun
  printf, u_log, "Date: " + out_filename & printf, u_log, "NOTE: " + note
  ; printf, u_log, "Start processing: " + T1 & printf, u_log, 'INPUT FILES:'
  printf, u_log, "Input VI file: "+in_files.ndvi    & printf, u_log, "Input NDII file: "+in_files.ndii
  printf, u_log, "Input NDFI file: "+in_files.ndfi    & printf, u_log, "Input Pixel QA file: "+in_files.qa
  ; printf, u_log, "Input SNOW file: "+in_files.snow    &
  printf, u_log, "Input Blue file: "+in_files.blue
  ; printf, u_log, "Input Land Cover file: "+in_files.lc      & printf,"Input SEASON file: "+ u_log, in_files.season
  printf, u_log, "Input LST file: "+in_files.lst
  printf, u_log
  printf, u_log, "Output fodler: "+file_dirname(out_filename)

  printf, u_log,''
  printf, u_log, "PhenoriceV2.0 Parametrization input: "
  printf, u_log, "-> Number of bands on previous year ", strtrim(pr_opts.nb_prev)
  printf, u_log, "-> Number of bands on each year quarter ",                     $
    strtrim(pr_opts.nb_1q), strtrim(pr_opts.nb_2q), strtrim(pr_opts.nb_3q), strtrim(pr_opts.nb_4q)
  printf, u_log, "-> Smooth's window dim. ", strtrim(pr_opts.win_dim_l), strtrim(pr_opts.win_dim_r)
  printf, u_log, "-> Blu band thresholds for cloud level identification ",       $
    strtrim(pr_opts.cloud_clear),  strtrim(pr_opts.cloud_full)

  if (max_criteria.derivs eq 1) then begin
    printf, u_log, "-> Derivatives checks on maxs", $
      strtrim(max_criteria.derivs_opt[0]),' - ',strtrim(max_criteria.derivs_opt[1])
  endif else begin
    printf, u_log, "-> NO Derivatives checks on maxs"
  endelse

  if (max_criteria.max_value eq 1) then begin
    printf, u_log, "-> Minimum EVI/NDVI value for a real rice MAX identification", $
      strtrim(max_criteria.vi_tr_max)
  endif else begin
    printf, u_log, "-> NO check on Maximum absolute Value"
  endelse

  if (max_criteria.decrease eq 1) then begin
    printf, u_log, "-> Width and % for decrease check on senescence", $
      strtrim(max_criteria.decrease_win), ' - ',strtrim(max_criteria.decrease_perc)
  endif else begin
    printf, u_log, "-> NO check on decrease in senescence"
  endelse

  if (min_criteria.min_value eq 1) then begin
    printf, u_log, "-> Maximum EVI/NDVI value for a real rice MIN identification", $
      strtrim(min_criteria.vi_tr_min)
  endif else begin
    printf, u_log, "-> NO check on Minimum absolute Value"
  endelse

  if (min_criteria.growth eq 1) then begin
    printf, u_log, "-> Check for derivatives after min", $
      strtrim(min_criteria.growth_opt [0]), ' - ',strtrim(min_criteria.growth_opt [1])
  endif else begin
    printf, u_log, "->  Check for derivatives after min"
  endelse

  if (min_criteria.flood eq 1) then begin
    printf, u_log, "-> Check for flooding - width of window", $
      strtrim(min_criteria.flood_win )
  endif else begin
    printf, u_log, "-> NO Check for flooding"
  endelse

  if (min_criteria.max_after eq 1) then begin
    printf, u_log, "-> Range for MAX position after min", $
      strtrim(min_criteria.max_aft_win[0]),' - ',strtrim(min_criteria.max_aft_win[1])
  endif else begin
    printf, u_log, "-> NO Check for MAX position after min"
  endelse

  if (min_criteria.lst eq 1) then begin
    printf, u_log, "-> Minimum LST value for legal min", $
      strtrim(min_criteria.lst_thresh)
  endif else begin
    printf, u_log, "-> NO Check for Minimum LST value for legal min"
  endelse

  ;TODO: Add log info of outputs locations

close, u_log & free_lun,u_log

end
 