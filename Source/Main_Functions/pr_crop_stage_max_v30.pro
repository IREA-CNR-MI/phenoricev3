; Title pr_crop_stage_max_v2
;:Description
;
;Details
;
; :Params:
;    smooth_pix
;    rice_max_arr_pix
;    der_neg_pix
;    der_pos_pix
;    nb
;    pr_opts
;    der_ind
;    maxdec_wid
;    maxdec_win
;    maxthresh
;    maxdec_perc
;    check_arr_max
;    win_dim_l
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
function pr_crop_stage_max_v22, smooth_pix, rice_max_arr_pix, der_neg_pix, der_pos_pix,nb, pr_opts, $
           der_ind,maxdec_wid, maxdec_win, maxthresh, maxdec_perc, check_arr_max, win_dim_l

  ;--------------------------------------- Absolute seasonal MAX identification :
  ; CRITERIA :
  ; 
  ; 1 MAX is above a given threshold
  ; 2 MAX OCCURS AFTER POSTIVIVE DER (3 ON 5 DATES WINDOW) AND
  ;   IS FOLLOWED BY NEG DER (3 ON 5 DATES WINDOW)
  ; 3 MAX OCCURS IN CORRESPONDENCE OF SENESCENCE (FAST DECREASING) WITHIN 5
  ;   COMPOSITE AFTER MAX (EVI DROPS BELOW 1/3 EVIMAX)
    ;-------------------------------------------------------------- First criteria :

  ; NEW VERSION --> Removed useless if cycles; added outputs on "reasons of max removal"

  rice_max_arr_pix[0:win_dim_l+3]=0   ; Remove maxs too early in the series: I don't have data to seek the min !
  rice_max_arr_pix[nb-3:nb-1]=0       ; Remove maxs too late in the series: I don't have data to seek the decrease,
                                      ; and (also) harvest would be in next year, so should be seen in the analysis of
                                      ; next rice season !

  pos_maxs   = where(rice_max_arr_pix gt 0, count_maxs)  ; Find and get positions of remaining maxs

  if count_maxs gt 0 then begin

    pos_maxs = transpose(pos_maxs)

    max_indexes = rebin(pos_maxs,5,count_maxs)
    der_indexes_reb = rebin(der_ind,5,count_maxs)
    der_pos_indexes = max_indexes - der_indexes_reb     ; Check backwards on positive derivatives
    der_neg_indexes = max_indexes + der_indexes_reb     ; Check forard on negative derivatives

    if (check_arr_max[0] eq 1) then check_derivs= (total(der_pos_pix[der_pos_indexes],1) lt 3) $   ; For each nax, this gives out 0 if at least 3 out of 5 
      or (total(der_neg_pix[der_neg_indexes],1) lt 3) else check_derivs=bytarr(count_maxs)         ; points before the max have positive der. AND 3 out of 5 
                                                                                                   ; points after max have negative der. Otherwise, it gives 1 (condition not satisfied)

    vi_maxs = smooth_pix[pos_maxs]
    if (check_arr_max[1] eq 1) then vi_max_checks = (vi_maxs lt maxthresh) else vi_max_checks=bytarr(count_maxs)  ; For each nax, this gives out 1 if max below minimum threshold  (condition not satisfied)
    
    ; CHECK FOR DECREASE MOVE AFTERWARDS !!!!!!!!
;    max_vis_indexes = rebin(maxdec_win,maxdec_wid,count_maxs)+rebin(pos_maxs,maxdec_wid,count_maxs)
;    if (check_arr_max[2] eq 1) then vi_dec_checks = min(smooth_pix[max_vis_indexes],dimension = 1) gt  $; For each nax, this gives out 0 if VI value
;                      (maxdec_perc*vi_maxs) else vi_dec_checks=bytarr(count_maxs)                     ; decreases below a given % with respect to the max in a  
;                                                                                                       ; specified period. 
    
    check_tot_maxs = 10*check_derivs + 50*vi_max_checks ;+ 100*vi_dec_checks             ; compute a "code" from results of the check. If all checks passed it stays at 0 

    rice_max_arr_pix [pos_maxs] = check_tot_maxs  > 1         ; build the array containing results on checks on maximums. 
                                                              ; this is an array of dims (n° of bands). 0 = NO MAX
                                                              ; 1 = Good max
                                                              ; >1 = bad max (not satisfying one or more criteria . 
                                                              
    return, min(check_tot_maxs)        ; Return the minimum of the check array ( = 0 only if some max survived !!!!)
  endif else begin
    return, 200
    ; Return 200 if no maxs identified in the original TS
  endelse
end