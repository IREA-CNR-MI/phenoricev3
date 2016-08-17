;+pr_crop_stage_min_v30
;
; :Description:
;
;
; :Params:
;    opts
;    smooth_pix
;    NDFI_pix
;    LST_pix
;    doy_pix
;    min_array
;    max_array
;    der_neg_pix
;    der_pos_pix
;    nb
;    doys_reg
;
;
; :Author:
; 	Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it (2016)
;
; :License: GPL>3.0
;-
FUNCTION pr_crop_stage_min_v30, opts, smooth_pix, NDFI_pix, LST_pix, doy_pix, min_array, max_array, $
  der_neg_pix, der_pos_pix, nb, doys_reg
  COMPILE_OPT hidden
  COMPILE_OPT IDL2
  
  min_array[nb-6:nb-1] = 0 ; Set to 0 all mins at the end of the serie ! no time to find a max ! + smooth to 0
  min_array[0:5]       = 0       ; Set to 0 all mins at the end of the serie ! smooth to 0 - probably useless
  ;------------------------------------------- Apply VI-treshold on MIN-VI-value :
  pos_mins             = where(min_array EQ 1, count_mins)

  IF count_mins NE 0 THEN BEGIN  ; Is there at least one min ? If so, then process
    pos_mins = transpose(pos_mins)

    ; first check : Min above a threshold Where results one, min didn't pass the check
    IF (opts.check_arr_min[0] EQ 1) THEN vi_min_checks = 10*(smooth_pix [pos_mins] GT opts.VI_TR_MIN) ELSE vi_min_checks =bytarr(count_mins)              

    ;second check: MIN IS FOLLOWED BY A FAST GROW: 3 POSITIVE DERIV. VALUES, IN A 5 WINDOW
    ; = 1 if less than 3 positive ders in 5 window --> 1 = didn't pass
    IF (opts.check_arr_min[1] EQ 1) THEN BEGIN
      growth_indexes = rebin(opts.growth_ind,opts.growth_opt[0],count_mins)+rebin(pos_mins,opts.growth_opt[0],count_mins)
      vi_grow_checks = 50*(total(der_pos_pix[growth_indexes],1) LT opts.growth_opt[1])
    ENDIF ELSE vi_grow_checks =bytarr(count_mins)              ; Where results one, min didn't pass the check

    ;third check: MIN OCCURS IN CORRESPONDENCE OF A FLOOD:
    IF (opts.check_arr_min[2] EQ 1) THEN BEGIN
      flood_checks = bytarr(count_mins)
      FOR min = 0 ,  count_mins-1 DO BEGIN
        ; vi_flood_wid_count = n° of obs found in the window
        flood_vi_indexes = where (abs(doys_reg[pos_mins[min]]-doy_pix) LE opts.flood_win, vi_flood_wid_count)  
        ; = 0 if at least one NDFI below 0 in all observations nearby the minimum
        IF opts.check_NDFI EQ 1 THEN checks_NDFI = total(NDFI_pix[flood_vi_indexes] GT 0) EQ 0 $   
        ELSE checks_NDFI = 1
        flood_checks [min] = checks_NDFI
      ENDFOR

    ENDIF ELSE flood_checks = bytarr(count_mins) ; array of length n_mins - 0 for maxs that passed at least one "flooding" criteria

    ;fourth check: MIN OCCURS IN CORRESPONDENCE OF WINDOW OF xx to YY COMPOSITE BEFORE MAX;

    IF (opts.check_arr_min[3] EQ 1) THEN BEGIN
      max_check_vi_indexes = rebin(opts.vi_max_ind,n_elements(opts.vi_max_ind),count_mins)+rebin(pos_mins,n_elements(opts.vi_max_ind),count_mins)
      max_check_vi_indexes = max_check_vi_indexes < nb    ; Substitute indexes > nb with nb to prevent array subscripting error
      maxs_checks          = 200*(total((max_array [max_check_vi_indexes] EQ 1 ),1) EQ 0)

    ENDIF ELSE maxs_checks = bytarr(count_mins) ; array of length n_mins

    ;fifth check: Minimum in "good " season according to LST
    IF (opts.check_arr_min[4] EQ 1) THEN LST_checks = 400*(LST_pix [pos_mins] LT opts.lst_thresh) ELSE LST_checks =bytarr(count_mins) 

    checks_mins_tot = vi_min_checks + vi_grow_checks + flood_checks + maxs_checks + LST_checks
    min_array [pos_mins] = checks_mins_tot  > 1

    ; Return the minimum of the check array ( = 1 only if some min survived !!!!)
    return, min(checks_mins_tot)

  ENDIF ELSE BEGIN

    return, 600                    ; Return 600 if no mins identified in the original TS
    
  ENDELSE

END
