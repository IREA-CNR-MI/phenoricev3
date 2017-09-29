;License GPL(>2)
;-
;+pr_crop_stage_max_v30
;
; :Description:
;
;
; :Params:
;
;    opts        :
;    smooth_pix  :
;    max_array   :
;    der_neg_pix :
;    der_pos_pix :
;    nb          :
;
; :Returns:
;
; :Author:
; 	Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it (2016)
;
; :License: GPL>3.0
;-

FUNCTION pr_crop_stage_max_v30, opts, smooth_pix, max_array, der_neg_pix, der_pos_pix, nb
  COMPILE_OPT hidden
  COMPILE_OPT IDL2

  ; ; CRITERIA :
  ;
  ; 1 MAX is above a given threshold
  ; 2 MAX OCCURS AFTER POSTIVIVE DER (3 ON 5 DATES WINDOW) AND
  ;   IS FOLLOWED BY NEG DER (3 ON 5 DATES WINDOW)
  ; 3 MAX OCCURS IN CORRESPONDENCE OF SENESCENCE (FAST DECREASING) WITHIN 5
  ;   COMPOSITE AFTER MAX (EVI DROPS BELOW 1/3 EVIMAX)
  ;-------------------------------------------------------------- First criteria :

  ; NEW VERSION --> Removed useless if cycles; added outputs on "reasons of max removal"

  max_array[0:opts.win_dim_l+3] = 0   ; Remove maxs too early in the series: I don't have data to seek the min !
  max_array[nb-3:nb-1]          = 0       ; Remove maxs too late in the series: I don't have data to seek the decrease,

  pos_maxs   = where(max_array GT 0, count_maxs)  ; Find and get positions of remaining maxs

  IF count_maxs GT 0 THEN BEGIN

    pos_maxs        = transpose(pos_maxs)

    max_indexes     = rebin(pos_maxs,5,count_maxs)
    der_indexes_reb = rebin(opts.der_ind,5,count_maxs)
    der_pos_indexes = max_indexes - der_indexes_reb     ; Check backwards on positive derivatives
    der_neg_indexes = max_indexes + der_indexes_reb     ; Check forard on negative derivatives

    ; For each nax, this gives out 0 if at least 3 out of 5 points before the max have positive der. AND 3 out of 5
    ; points after max have negative der. Otherwise, it gives 1 (condition not satisfied)

    IF (opts.check_arr_max[0] EQ 1) THEN BEGIN
      check_derivs = (total(der_pos_pix[der_pos_indexes],1) LT 3) OR (total(der_neg_pix[der_neg_indexes],1) LT 3)
      if (min(check_derivs)) EQ 1 then return, 200    ; if no maxs satisfy criteria, return to caller with code 200 (tells caller to skip pixel)
    ENDIF ELSE check_derivs=bytarr(count_maxs)         

    ; For each nax, this gives out 1 if max below minimum threshold  (condition not satisfied)
    
    IF (opts.check_arr_max[1] EQ 1) THEN BEGIN
      vi_maxs = smooth_pix[pos_maxs]
      vi_max_checks = (vi_maxs LT opts.VI_TR_MAX)
      if (min(check_derivs + vi_max_checks)) EQ 1 then return, 200  ; if no maxs satisfy both criteria, return to caller
    ENDIF ELSE vi_max_checks=bytarr(count_maxs)

    ; compute a "code" from results of the check. If all checks passed it stays at 0
    check_tot_maxs = check_derivs + vi_max_checks            

    ; build the array containing results on checks on maximums.
    ; this is an array of dims (n° of bands).
    ; 0 = NO MAX
    ; 1 = Good max
    ; >1 = bad max (not satisfying one or more criteria

    max_array [pos_maxs] = (1 + check_tot_maxs) > 1 

    return, min(check_tot_maxs)        ; Return the minimum of the check array ( = 0 only if some max survived !!!!)

  ENDIF ELSE BEGIN

    ; Return 200 if no maxs identified in the original TS
    return, 200
    
  ENDELSE
  
END