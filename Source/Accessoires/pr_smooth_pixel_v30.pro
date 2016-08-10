;-
;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    vi_pix
;    qa_pix
;    doy_pix
;    nb
;    win_dim_l
;    win_dim_r
;    doys_reg
;    mapscape
;
; :Keywords:
;    check
;
; :Author: lb
;-
function pr_smooth_pixel_v30, vi_pix, qa_pix,doy_pix, nb, win_dim_l,win_dim_r, doys_reg, check,  mapscape

  compile_opt IDL2
  check = 0
;  CATCH, Error_status
;
;  IF Error_status NE 0 THEN BEGIN
;    PRINT, 'Error in Smoothing ! - line: '+ string(tile_index)+' pixel:  ', string(pixel)
;    PRINT, 'The pixel will be set to NODATA and skipped'
;    return, intarr(nb) + 32767
;    ; Handle the error by keeping the pixel to NODATA
;  ENDIF

  ; -------------------------------------------------
  ; ------ Initial checks on consistency of DOYs ----
  ; -------------------------------------------------

  ;  Substitute doys of where doy is nodata with doys_reg
  if MAPSCAPE EQ 0 then begin
    BAD_DOY = where(abs(DOY_PIX ) gt 1000, count_bad_doy)    ; Check on doys: doys > 400 = NODATA in DOY image --> set them to the doy of the composite
  endif else begin
    BAD_DOY = where(DOY_PIX EQ -1, count_bad_doy)    ; Check on doys: doys > 400 = NODATA in DOY image --> set them to the doy of the composite !
  endelse

  if (count_bad_doy ne 0 ) then DOY_PIX[bad_doy] = doys_reg[bad_doy]

  ; Check for problems on DOYS: if difference between doy_pix and doys_reg LT -16 --> last doy_pixs wrong beacuase acquired in first days of "next year"
  ; --> Add 365 to the DOY
  diff = doy_pix-doys_reg
  bad_diff = where(diff LT -16, count_baddiff)
  if (count_baddiff GT 0 ) then begin
    doy_pix[bad_diff] = doy_pix[bad_diff]+365
  endif

  ; Final check on doys. If still problems stop (Should never happen now !!!!)
  diff =max(abs(doy_pix-doys_reg))
  if (diff gt 16) then begin
    print, "Something wrong was found on  the DOYS.... code to be recheked. contact cdevelopers @ lbusett@gmail.com"
    stop
  endif

  if (mapscape EQ 0) then nodata_pos = where(vi_pix eq 32767, count_NA) else nodata_pos = where(vi_pix eq -3000, count_NA)  ; Check on vi values: vi = -3000 = NODATA .

  ; -------------------------------------------------
  ; ----------- Do the smoothing  -------------------
  ; -------------------------------------------------

  ; If more than 10 NODATA, skip the pixel and put it at 32767
  if count_NA lt 10 then begin          ; otherwise, perform smoothing

    ;------- SET WORKING ARRAYS. FLOAT/DOUBLE IS NEEDED BY SAVGOL INTERPOLATION:

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

  endif else begin ; End if on less than 10 NA on VI series

    return, intarr(nb) + 32767

  endelse

end