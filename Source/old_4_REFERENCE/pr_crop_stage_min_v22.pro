
;
; Title pr_crop_stage_min_v2
;:Description
;
;Details
;
; :Params:
;    smooth_pix
;    rice_min_arr_pix
;    rice_max_arr_pix
;    NDII_pix
;    NDFI_pix
;    LST_pix
;    der_neg_pix
;    der_pos_pix
;    nb
;    minthresh
;    growth_ind
;    growth_wid
;    growth_thresh
;    vi_flood_ind
;    vi_flood_wid
;    check_NDFI
;    check_NDII
;    vi_max_ind
;    LST_thresh
;    check_arr_min
;    win_dim_l
;    doys_reg
;    doy_pix
;
;
;
;:RETURNS:
;
;:AUTHOR: Lorenzo Busetto, phD (2015)
;  #' email: busetto.l@@irea.cnr.it
;
;License GPL(>2)
;-
function pr_crop_stage_min_v22, smooth_pix , rice_min_arr_pix, rice_max_arr_pix, NDII_pix, NDFI_pix, LST_pix,     $
  der_neg_pix, der_pos_pix, nb,minthresh,growth_ind, growth_wid, growth_thresh, $
  vi_flood_ind,vi_flood_wid,  check_NDFI,check_NDII,vi_max_ind, LST_thresh, check_arr_min, win_dim_l, doys_reg,doy_pix


  compile_opt IDL2
  ;---------------------------------------- Absolute seasonal MIN identification :
  ; CRITERIA :
  ; 1) MIN ARE CUTTED WITH A TRESHOLD ON VI-INDEX (minthresh)
  ; 2) MIN IS FOLLOWED BY A FAST GROW: 3 POSITIVE DERIV. VALUES, IN A 5 WINDOW)
  ; 3) MIN OCCURS IN CORRESPONDENCE OF A FLOOD:
  ;    (CHECK VALUE LSWD GT EVI or NDWI GT 0 IN A 5 DATES WINDOW -2 + 2)
  ; 4) MIN OCCURS IN CORRESPONDENCE OF WINDOW OF-17) to - 6 COMPOSITE BEFORE MAX;
  ;    *-11 ADDED THE 28/03/2012 and -17 ADDED THE 11/Oct/2012
  ;if lc_ok eq 176 then begin
  ;stop
  ;endif

  ; New version

  rice_min_arr_pix[nb-6:nb-1]  = 0 ; Set to 0 all mins at the end of the serie ! no time to find a max ! + smooth to 0
  rice_min_arr_pix[0:5]  = 0       ; Set to 0 all mins at the end of the serie ! smooth to 0 - probably useless
  ;------------------------------------------- Apply VI-treshold on MIN-VI-value :
  pos_mins   = where(rice_min_arr_pix eq 1, count_mins)

  if count_mins ne 0 then begin  ; Is there at least one min ? If so, then process
    pos_mins = transpose(pos_mins)

    ; first check : Min above a threshold
    if (check_arr_min[0] eq 1) then vi_min_checks = 10*(smooth_pix [pos_mins] gt minthresh) else vi_min_checks =bytarr(count_mins)              ; Where results one, min didn't pass the check

    ;second check: MIN IS FOLLOWED BY A FAST GROW: 3 POSITIVE DERIV. VALUES, IN A 5 WINDOW
    if (check_arr_min[1] eq 1) then begin
      grow_indexes = rebin(growth_ind,growth_wid,count_mins)+rebin(pos_mins,growth_wid,count_mins)
      vi_grow_checks = 50*(total(der_pos_pix[grow_indexes],1) lt growth_thresh)
    endif else vi_grow_checks =bytarr(count_mins)              ; Where results one, min didn't pass the check
    ; = 1 if less than 3 positive ders in 5 window --> 1 = didn't pass

    ;third check: MIN OCCURS IN CORRESPONDENCE OF A FLOOD:
    if (check_arr_min[2] eq 1) then begin
      flood_checks = bytarr(count_mins)
      for min = 0 ,  count_mins-1 do begin
        flood_vi_indexes = where (abs(doys_reg[pos_mins[min]]-doy_pix) le vi_flood_wid,  vi_flood_wid_count)  ; vi_flood_wid_count = n° of obs found in the window
        if check_NDII eq 1 then checks_NDII  = total((NDII_pix[flood_vi_indexes]+500) gt (smooth_pix [flood_vi_indexes])) eq 0 $ ; = 0 if at least one NDFI below 0 in all observations nearby the minimum
        else checks_NDII = 1; array of length n_mins - 0 for maxs that passed the criteria
        if check_NDFI eq 1 then checks_NDFI = total(NDFI_pix[flood_vi_indexes] gt 0) eq 0 $   ; = 0 if at least one NDFI below 0 in all observations nearby the minimum
        else checks_NDFI = 1
        flood_checks [min] = 100* (checks_NDII and checks_NDFI)
      endfor
    endif else flood_checks =bytarr(count_mins) ; array of length n_mins - 0 for maxs that passed at least one "flooding" criteria

    ;fourth check: MIN OCCURS IN CORRESPONDENCE OF WINDOW OF-17) to - 6 COMPOSITE BEFORE MAX;
    if (check_arr_min[3] eq 1) then begin
      max_check_vi_indexes = rebin(vi_max_ind,n_elements(vi_max_ind),count_mins)+rebin(pos_mins,n_elements(vi_max_ind),count_mins)
      max_check_vi_indexes = max_check_vi_indexes < 69    ; Substitute indexes > nb with nb to prevent array subscripting error
      maxs_checks = 200*(total((rice_max_arr_pix [max_check_vi_indexes] eq 1 ),1) eq 0)
    endif else maxs_checks =bytarr(count_mins) ; array of length n_mins - 0 for maxs tha

    ;fifth check: Minimum in "good " season according to LST
    if (check_arr_min[4] eq 1) then LST_checks = 400*(LST_pix [pos_mins] lt 15) else LST_checks =bytarr(count_mins) ; array of length n_mins - 0 for maxs tha

    checks_mins_tot = vi_min_checks + vi_grow_checks + flood_checks + maxs_checks + LST_checks

    rice_min_arr_pix [pos_mins] = checks_mins_tot  > 1
    return, min(checks_mins_tot)        ; Return the minimum of the check array ( = 1 only if some max survived !!!!)
  endif else begin
    return, 600                    ; Return 600 if no mins identified in the original TS
  endelse
end
