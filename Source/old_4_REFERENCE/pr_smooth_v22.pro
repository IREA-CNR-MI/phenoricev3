; Title pr_smooth_v2
;:Description  
;
; Function used to smooth the VI time series for application of the PHENORICE algorithm 
;
;Details
; The smoothing algorithm follows the following steps: 
; 
;1) SPIKE DETECTION AND GAP FILLING --> Spikes are identified by analyzing the deviation of a 
;   point in the TS with respect to its neighbours (5 composites window). If deviation from the 
;   median is greater than 2 std deviations, the point is flagged as a spike, and substitude with
;   a polynomial fit on a 3x3 window centered on the point
;   
;
;2-3) Iterative SAVGOL SMOOTHING

;       - implementation of savgol (2^ order WEIGTHED polynomial fitting)
;         that exploit MODIS quality flag (quality and snow) and blue band
;         info for signal contamination
;       - Weights assigned on the basis of the "quality" incicators set in the "pr_build_inputs" function 
;         weights assigned are: 
;             QA = 0 --> Measure_Errors = 0.02
;             QA = 1 --> Measure_Errors = 0.11
;             QA = 2 --> Measure_Errors = 0.30
;       
;   
; :Params:
;    vi_pix: Input non-smoothed VI array for the pixel
;    qa_pix: Quality data for the pixel ( 0 = clear; 1 = mixed; 2 = Bad)
;    nb: Number of bands in input TS
;    win_dim_l: Width of the smoothing window for savgol - left
;    win_dim_r: Width of the smoothing window for savgol - right
;    doys_reg: regular array of doys on to which restitute smoothed values
;    check: FLAG: if one, a visual representation of results is shown for each pixel while iterating
;
;:RETURNS:
;
;:AUTHOR: Lorenzo Busetto, phD (2014)
;  #' email: busetto.l@@irea.cnr.it
;
;License GPL(>2)
;-
function pr_smooth_v22, vi_pix , qa_pix,doy_pix, nb, win_dim_l, win_dim_r, doys_reg, check = check

  compile_opt IDL2
  check = 0

  ;------- SET WORKING ARRAY. FLOAT/DOUBLE IS NEEDED BY SAVGOL INTERPOLATION:
  

  
  ;-------------------------------------- 1)Spike detection and substitution:
;  TIME_SERIE_measure_errors =replicate(1.0/6,6)
  X_VECT=[1,2,3,5,6,7]
  doys_ord = sort(doy_pix) 
  vi_pix_ord = vi_pix[doys_ord]
  doy_pix_ord = doy_pix[doys_ord]
  
  qa_pix_ord = qa_pix[doys_ord]
  
  vi_pix_fl= vi_pix_ord
  dove_na = where(vi_pix_fl eq 32767, count_na)
  X_VECT=[1,2,3,5,6,7]
  
  
  ; 1) SPIKE DETECTION AND GAP FILLING
  TIME_SERIE_measure_errors = (200 * (qa_pix_ord eq 0) + 1100 * (qa_pix_ord eq 1) + 3000* (qa_pix_ord eq 2))
  if count_na GT 0 then begin
    vi_pix_fl[dove_na] = 0
    TIME_SERIE_measure_errors [dove_na]= 7000
  endif
  
  for K=win_dim_l, NB-(win_dim_r+1) do begin

    check_arr4 = vi_pix_ord[[k-2, k-1, k+1,k+2]]
    check_arr6 = vi_pix_ord[[k-3,k-2, k-1, k+1,k+2,k+3]]
    measure_errors= TIME_SERIE_measure_errors[[k-3,k-2, k-1, k+1,k+2,k+3]]
    MED_VAL=mean(CHECK_ARR4)
    STDEV_val=stdev(CHECK_ARR4)
    if (vi_pix_ord[k] lt MED_VAL-2*STDEV_val) or  (vi_pix_ord[k] gt MED_VAL+2*STDEV_val) then begin

      ; OPTION 1 GAP FILLING:  SECOND ORDER POLINOMIAL
      result = poly_fit(X_VECT, CHECK_ARR6, 2, MEASURE_ERRORS=measure_errors, Yfit = Yfit,/DOUBLE,STATUS=status)
      TIME_SERIE_measure_errors[k] = 3000
      vi_pix_fl[k] = result[0]+RESULT[1]*4+RESULT[2]*(4^2)
      
    endif
  endfor
  
   ; 1) SAVGOL - first iteration - use weights derived from quality flags
  TIME_SERIE_Y_NEW1=fltarr(nb)
  sum_index = indgen(win_dim_r*2+1)
  for k=0, nb-(win_dim_r*2+1) do begin

    smooth_indexes = k+sum_index
    Y=vi_pix_fl[smooth_indexes]
    X_VECT = doy_pix_ord[smooth_indexes]
    x_vect_reg = doys_reg[smooth_indexes]
    measure_errors= TIME_SERIE_measure_errors[smooth_indexes]
    result = poly_fit(X_VECT, Y, 2, MEASURE_ERRORS=measure_errors, Yfit = Yfit,STATUS=status)
    TIME_SERIE_Y_NEW1[K+win_dim_r]= Yfit[win_dim_r]
    
;    result[0]+RESULT[1]*X_VECT[win_dim_r]+RESULT[2]*(X_VECT[win_dim_r]^2)

  endfor
  
  ; put values "out" of the filtering (borders of TS) again at the original values
  TIME_SERIE_Y_NEW1[0:(win_dim_r-1)] = vi_pix_ord[0:(win_dim_r-1)]
  TIME_SERIE_Y_NEW1[(nb-win_dim_r):nb-1] = vi_pix_ord[(nb-win_dim_r):nb-1]
    ; 2) SAVGOL - second iteration - upper envelope adaptation
    ; 
  old_vi = vi_pix_fl
  diff  = (TIME_SERIE_Y_NEW1 - vi_pix_fl)
  diff[where(TIME_SERIE_measure_errors GT 10000*0.2)] = +0.01
  upper = where(diff LT 0, complement = pos_lower) 
  TIME_SERIE_measure_errors [ upper ] = 200          ; set higher weights to where first run > orig to force towards upper envelope
  TIME_SERIE_measure_errors [ pos_lower ] = 500
 
  vi_pix_fl[pos_lower] = TIME_SERIE_Y_NEW1[pos_lower] ; Set time series values of where first run < orig to first run, otherwise keep orig
  vi_pix_fl[0:(win_dim_r-1)] = vi_pix_ord[0:(win_dim_r-1)]    ; Reset the first and last values to the original VI values (so I don't have to set them to 0...)
  vi_pix_fl[(nb-win_dim_r):nb-1] = vi_pix_ord[(nb-win_dim_r):nb-1] 
  TIME_SERIE_Y_NEW2=fltarr(nb)
 
  for k=0, nb-(win_dim_r*2+1) do begin
    smooth_indexes = k+sum_index
    Y=vi_pix_fl[smooth_indexes]
    X_VECT = doy_pix_ord[smooth_indexes]
    x_vect_reg = doys_reg[smooth_indexes]
    measure_errors= TIME_SERIE_measure_errors[smooth_indexes]
    result = poly_fit(X_VECT, Y, 2, MEASURE_ERRORS=measure_errors, Yfit = Yfit,STATUS=status)
     TIME_SERIE_Y_NEW2[K+win_dim_r]= result[0]+RESULT[1]*x_vect_reg[3]+RESULT[2]*(x_vect_reg[3]^2)
     ;result[0]+RESULT[1]*x_vect_reg[3]+RESULT[2]*(x_vect_reg[3]^2)

  endfor
 
  TIME_SERIE_Y_NEW2[0:(win_dim_r-1)] = vi_pix_ord[0:(win_dim_r-1)]     ; Reset the first and last values to the original VI values (so I don't have to set them to 0...)
  TIME_SERIE_Y_NEW2[(nb-win_dim_r):nb-1] = vi_pix_ord[(nb-win_dim_r):nb-1]
  
  if check EQ 1 then begin
  ; plot results - optional
  cgplot,doy_pix_ord, vi_pix_ord, color = 'black' ,psym = 2
  cgplot,doy_pix_ord, old_vi, color = 'blue' ,/overplot,psym = 4
  cgplot,doy_pix_ord, TIME_SERIE_Y_NEW1, color = 'darkgreen' ,/overplot
  cgplot,doys_reg, TIME_SERIE_Y_NEW2, color = 'red' ,/overplot

  C = GET_KBRD()
  endif

  return, fix(TIME_SERIE_Y_NEW2)     ; Return smoothed array to caller !
  
end