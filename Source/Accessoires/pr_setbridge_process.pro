;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    lines
;    proc_opts
;    max_criteria
;    min_criteria
;    data_lc
;    data_VI
;    data_smooth
;    data_DOY
;    nb
;    ncols
;    win_dim_l
;    win_dim_r
;    doys_reg
;    years
;    proc_year
;    prev_y
;    oBridge
;
;
;
; :Author: lb
;-
pro pr_setbridge_process, lines, proc_opts, max_criteria, min_criteria, data_lc, data_VI, data_smooth, $
  data_DOY, nb, ncols, win_dim_l, win_dim_r, doys_reg, years, proc_year, prev_y , oBridge
  COMPILE_OPT idl2
  oBridge->SetVar, 'inlines', lines
  oBridge->SetVar, 'der_ind', der_ind
  oBridge->SetVar, 'maxdec_wid', maxdec_wid
  oBridge->SetVar, 'maxdec_ind', maxdec_ind
  oBridge->SetVar, 'maxthresh', maxthresh
  oBridge->SetVar, 'maxdec_perc', maxdec_perc
  oBridge->SetVar, 'check_arr_max', check_arr_max
  oBridge->SetVar, 'minthresh', minthresh
  oBridge->SetVar, 'growth_ind', growth_ind
  oBridge->SetVar, 'growth_wid', growth_wid
  oBridge->SetVar, 'growth_thresh', growth_thresh
  oBridge->SetVar, 'vi_flood_wid', vi_flood_wid
  oBridge->SetVar, 'vi_flood_ind', vi_flood_ind
  oBridge->SetVar, 'check_NDFI', check_NDFI
  oBridge->SetVar, 'vi_max_ind', vi_max_ind
  oBridge->SetVar, 'LST_thresh', LST_thresh
  oBridge->SetVar, 'check_arr_min', check_arr_min
  oBridge->SetVar, 'pos_legit_maxs', pos_legit_maxs
  oBridge->SetVar, 'pos_quart_max', pos_quart_max
  
END
