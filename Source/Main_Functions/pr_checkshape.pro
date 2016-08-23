;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    opts
;    smooth_pix
;    temp_mindoy
;    temp_maxdoy
;    doys_reg
;
;
;
; :Author: lb
;-
function pr_checkshape, opts, smooth_pix, temp_mindoy, temp_maxdoy, doys_reg, ok_seasons

  COMPILE_OPT idl2

  ; Identify the seasons with valid min/max doys

  checks_shape = fltarr(n_elements(temp_mindoy)) - 999
  ; For each valid season, extract the time serie up to flowering

  FOREACH okseason, ok_seasons DO BEGIN

    IF opts.check_shape_meth EQ "linear" THEN BEGIN

      smooth_sub = smooth_pix [temp_mindoy[okseason]:temp_maxdoy[okseason]]
      doy_sub    = doys_reg   [temp_mindoy[okseason]:temp_maxdoy[okseason]]
      fit        = regress(doy_sub, smooth_sub, CHISQ = chi, CORRELATION = corr, FTEST = ftest)

    ENDIF

    IF (corr GT opts.check_corr) THEN checks_shape[okseason] = 1 $
    ELSE checks_shape[okseason] = 0

  ENDFOREACH

  return, checks_shape

END
