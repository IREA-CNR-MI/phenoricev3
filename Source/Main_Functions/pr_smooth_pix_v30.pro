;+pr_smooth_pix_v30
;
; :Description:
;
;
; :Params:
;    opts
;    vi_pix
;    qa_pix
;    doy_pix
;    nb
;    doys_reg
;    x_vect
;    smooth_1
;    sum_index
;    smooth_2
;    err_pix
;
;
;
; :Author:
; 	Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it (2016)
;
; :License: GPL>3.0
;-
FUNCTION pr_smooth_pix_v30, opts, vi_pix, qa_pix, doy_pix, nb, doys_reg,$
   smooth_1, sum_index, smooth_2, err_pix, x_vect
  
  COMPILE_OPT idl2
  COMPILE_OPT hidden

  smoother    = ""    ; Temporary - in case we implement other smoothing approaches

  ; This first part could be moved outside the pixel loop for a ? small ?
  ; speed improvement ?

  doys_ord    = sort(doy_pix)
  vi_pix_ord  = float(vi_pix[doys_ord])
  vi_pix_ord[where(vi_pix_ord EQ 32767)] = !values.F_NaN
  vi_pix_ord[where(vi_pix_ord EQ -3000)] = !values.F_NaN
  doy_pix_ord = long(doy_pix[doys_ord])
  qa_pix_ord  = qa_pix[doys_ord]
  vi_pix_fl   = vi_pix_ord

  ;  ; 1) SPIKE DETECTION AND GAP FILLING
  
  FOR K = opts.win_dim_l, nb-(opts.win_dim_r+1) DO BEGIN

    check_arr4     = vi_pix_ord[[k-2, k-1, k+1,k+2]]
    check_arr6     = vi_pix_ord[[k-3,k-2, k-1, k+1,k+2,k+3]]
    wgt            = err_pix[[k-3,k-2, k-1, k+1,k+2,k+3]]
    a = MOMENT(CHECK_ARR4, mean = MED_VAL , sdev = STDEV_val, maxmoment = 1, /NAN)
    IF (vi_pix_ord[k] LT MED_VAL-2*STDEV_val) OR (vi_pix_ord[k] GT MED_VAL+2*STDEV_val) OR (finite(vi_pix_ord[k]) EQ 0) THEN BEGIN
      ; OPTION 1 GAP FILLING:  SECOND ORDER POLINOMIAL
      ;result       = poly_fit(x_vect, CHECK_ARR6, 2, MEASURE_ERRORS=measure_errors, Yfit = Yfit,/DOUBLE,status=status)
      result = polyfitfast(x_vect[where(finite(CHECK_ARR6) EQ 1)], CHECK_ARR6[where(finite(CHECK_ARR6) EQ 1)], 2, w = wgt)
      err_pix[k]   = 3000
      vi_pix_fl[k] = result[0] + result[1]*4 + result[2]*16
    ENDIF
  ENDFOR

  ; 1) SAVGOL - first iteration - use weights derived from quality flags
  IF (smoother EQ "lowess") THEN BEGIN
    lowess,doy_pix_ord, vi_pix_fl, 8*(opts.win_dim_r+1), smooth_1, order = 2, WEIGHT = err_pix
  ENDIF ELSE BEGIN

    FOR k=opts.win_dim_l, nb-(opts.win_dim_r+1) DO BEGIN

      smooth_indexes = k+sum_index
      Y              = vi_pix_fl[smooth_indexes]
      X              = doy_pix_ord[smooth_indexes]
      result = polyfitfast(X[where(finite(Y) EQ 1)], Y[where(finite(Y) EQ 1)], 2, w = 1.0/err_pix[smooth_indexes], yfit)
      smooth_1[K]    = result[0] + result[1]*X[3] + result[2]*(X[3]^2)

    ENDFOR

  ENDELSE

  ; put values "out" of the filtering (borders of TS) again at the original values
  smooth_1[0:(opts.win_dim_r-1)] = vi_pix_ord[0:(opts.win_dim_r-1)]
  smooth_1[(nb-opts.win_dim_r):nb-1] = vi_pix_ord[(nb-opts.win_dim_r):nb-1]

  ; 2) SAVGOL - second iteration - upper envelope adaptation

  ; reset weights: assign low weights to values lower than first smoothing
  
  diff               = (smooth_1 - vi_pix_fl)
  diff[where(err_pix GT 10000*0.2)] = +0.01
  upper              = where(diff LT 0, complement = pos_lower)
  err_pix[upper]     = 200          ; set higher weights to where first run > orig to force towards upper envelope
  err_pix[pos_lower] = 500

  vi_pix_fl[pos_lower]                = smooth_1[pos_lower] ; Set time series values of where first run < orig to first run, otherwise keep orig
  vi_pix_fl[0:(opts.win_dim_r-1)]     = vi_pix_ord[0:(opts.win_dim_r-1)]    ; Reset the first and last values to the original VI values (so I don't have to set them to 0...)
  vi_pix_fl[(nb-opts.win_dim_r):nb-1] = vi_pix_ord[(nb-opts.win_dim_r):nb-1]

  ; redo the smoothing

  IF (smoother EQ "lowess") THEN BEGIN
    lowess,doy_pix_ord, vi_pix_fl, 8*(opts.win_dim_r+1), smooth_2, order = 2, WEIGHT = err_pix, NEWX = doys_reg
  ENDIF ELSE BEGIN

    FOR k = opts.win_dim_l, nb-(opts.win_dim_r+1) DO BEGIN

      smooth_indexes = k+sum_index
      Y           = vi_pix_fl[smooth_indexes]
      X           = doy_pix_ord[smooth_indexes]
      X_reg       = doys_reg[smooth_indexes[opts.win_dim_r]]
      ;result         = poly_fit(x_vect, Y, 2, MEASURE_ERRORS=err_pix[smooth_indexes], Yfit = Yfit,status=status)
      result      = polyfitfast(X[where(finite(Y) EQ 1)], Y[where(finite(Y) EQ 1)], 2, Yfit, w = err_pix[smooth_indexes])
      smooth_2[K] = result[0] + result[1]*X_reg + result[2]*(X_reg^2)

    ENDFOR

  ENDELSE

; Reset the first and last values to the original VI values (so I don't have to set them to 0...)
  smooth_2[0:(opts.win_dim_r-1)]     = vi_pix_ord[0:(opts.win_dim_r-1)]     
  smooth_2[(nb-opts.win_dim_r):nb-1] = vi_pix_ord[(nb-opts.win_dim_r):nb-1]
  
; return smoothed pixel
  return, smooth_2

END