
;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    opts
;    smooth_pix
;    out_mindoy
;    out_maxdoy
;    out_eosdoy
;    doys_reg
;    ok_seasons
;
;
;
; :Author: lb
;-
FUNCTION pr_checkshape, opts, smooth_pix, out_mindoy, out_maxdoy, out_eosdoy, doys_reg, ok_seasons

  COMPILE_OPT idl2
  
  ; Initialize an "empty" check array
  check = 0
  checks_shape = fltarr(n_elements(out_mindoy)) - 999

  ; For each "valid" season (i.e., mindoy not at -999), extract the time series from sow to max
  ; and from max to eos. preform a linear regression analysis and compute R2 on both the
  ; series min to max and max to eos. Then check the R2 against the opts.checkR2 parameter.
  ; --> If the check "passes" for both growing and senescence, then keep the "season" as
  ; good. Otherwise, set checks_shape[season] to 0. This tells the caller that the "shpae" of
  ; EVI for that season is not "good", so that it should be removed

  FOREACH okseason, ok_seasons DO BEGIN

    IF opts.check_shape_meth EQ "linear" THEN BEGIN

      pos_max = where(doys_reg EQ out_maxdoy[okseason])
      pos_min = where(doys_reg EQ out_mindoy[okseason])
      where_eos = min(abs(doys_reg - out_eosdoy[okseason]), pos_eos)

      smooth_sub_grow = smooth_pix [pos_min:pos_max]
      doy_sub_grow    = doys_reg   [pos_min:pos_max]
      fit_grow = regress(doy_sub_grow, smooth_sub_grow, CHISQ = chi_grow, CORRELATION = corr_grow, FTEST = ftest_grow)

      smooth_sub_sen = smooth_pix [pos_max:pos_eos]
      doy_sub_sen   = doys_reg   [pos_max:pos_eos]
      fit_sen = regress(doy_sub_sen, smooth_sub_sen, CHISQ = chi_sen, CORRELATION = corr_sen, FTEST = ftest_sen)

      IF (corr_grow^2 GT opts.check_R2) AND (corr_sen^2 GT opts.check_R2) THEN checks_shape[okseason] = 1 $
      ELSE checks_shape[okseason] = 0

    ENDIF



    IF opts.check_shape_meth EQ "hypertan" THEN BEGIN

      pos_max = where(doys_reg EQ out_maxdoy[okseason])
      pos_min = where(doys_reg EQ out_mindoy[okseason])

      IF out_eosdoy[okseason] NE -999 THEN BEGIN
        where_eos = min(abs(doys_reg - out_eosdoy[okseason]), pos_eos)


        doy_sub    = doys_reg [pos_min:pos_eos]
        smooth_sub = smooth_pix [pos_min:pos_eos]
        min_EVI    = smooth_pix[pos_min]
        range_EVI  = smooth_pix[pos_max]-min_EVI

        ; P[0] = Minimum
        ; P[1] = Range
        ; P[2] = Speed of increase - consider constraining
        ; P[3] = Speed of decrease - consider constraining
        ; P[4] = doys_reg[pos_min] + 30 - position of flex on increase
        ; P[5] = doys_reg[pos_max] + 30 - position of flex on decrease

        expr   = '(P[0] + 0.5*P[1]*(tanh(P[2]*(X-P[4]))-tanh(P[3]*(X-P[5]))))'

        ; Set the first guess and errors
        u = 0.02
        v = 0.02
        start =  [min_EVI,range_EVI, u, v , doys_reg[pos_min] + 30, doys_reg[pos_max] + 30]  ; Initialize Firt Guesss
        rerr = fltarr(n_elements(doy_sub))+1000

        ; Set the constraints
        parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
          limits:[0.D,0]}, 6)
        parinfo[0].fixed = 1
        parinfo[1].fixed = 0
        parinfo[2:3].limited = [1,0]
        parinfo[2:3].limits [0]= 0.01
        parinfo[*].value = start

        ; Try to fit the function

        ;result  = MPFITEXPR(expr, doy_sub, smooth_sub, rerr, start, parinfo = parinfo, STATUS = STATUS, YFIT=YFIT, /Quiet)
        result = mpfitexpr(expr, doy_sub, smooth_sub, rerr, start, parinfo = parinfo, STATUS = STATUS, YFIT=YFIT, ftol = 0.1, maxiter = 20, /Quiet)

        IF check EQ 1 THEN BEGIN
          plot, doy_sub, smooth_sub                                      ; Plot data
          oplot, doy_sub, mpevalexpr(expr, doy_sub, result), color = 220                ; Plot model
          oplot, doy_sub, smooth_pix [pos_min:pos_eos], color = 150                ; Plot model
          R = get_kbrd()
        ENDIF

        IF status GT 0 THEN BEGIN
          ; Check correlation between original and fitted
          corr_hyper = correlate(smooth_sub, yfit)
          IF (corr_hyper^2 GT opts.check_R2) AND result[1] GT 2500 THEN BEGIN
            checks_shape[okseason] = 1 
            ENDIF ELSE BEGIN
            checks_shape[okseason] = 0
          ENDELSE

        ENDIF ELSE BEGIN
          checks_shape[okseason] = 0
        ENDELSE

      ENDIF ELSE BEGIN
        checks_shape[okseason] = 0
      ENDELSE

    ENDIF ; end on check using hyperbolic tangent function

  ENDFOREACH  ; End cycle on valid seasons

  return, checks_shape

END
