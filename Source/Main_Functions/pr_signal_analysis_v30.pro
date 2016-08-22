;+pr_signal_analysis_v30
;
; :Description:
;
;
; :Params:
;    opts        : PhenoRice options
;    smooth_pix  : array with smoothed VI values (dims = nb)
;    nb          : Number of bands
;    max_array   : array where first guesses for maximums should be stored
;    min_array   : array where first guesses for minimums should be stored
;    der_neg_pix : array where negative derivatives for the pixel should be stored (1 = negative der; 0 = otherwise)
;    der_pos_pix : array where positive derivatives for the pixel should be stored (1 = negative der; 0 = otherwise)
;
;:RETURNS:
;
;   Sets first guesses for minimums and maximums; sets derivatives of smoothed VI
;
;
; :Author:
; 	Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it (2016)
;
; :License: GPL>3.0
;-
PRO pr_signal_analysis_v30, opts, smooth_pix,  nb, $
  max_array, min_array, der_neg_pix, der_pos_pix

  COMPILE_OPT hidden
  COMPILE_OPT IDL2

  ;_____________________________________________________________________________
  ; B1) MIN & MAX RELATIVE DETECTION
  ;- DERIVATIVE analysisi of smoothed function (with IDL function)
  ;- MIN = detected in points on the EVI-Smoothed index,
  ; where first derivate is > 0 & is preceded from a point in which it is < 0
  ; Min(t)=1 if der(t) LT 0 AND der(t+1) GT 0
  ;- MAX = detected in points on the EVI-Smoothed index,
  ; where first derivate is < 0 & is followed by a point in which it is > 0
  ; Max(t)=1 if der(t-1) GT 0 AND der(t) LT 0
  ;__________________________________________________________________________


  smooth_pix_der = smooth_pix - shift(smooth_pix,+1)  ; Compute derivative of the curve using differentiation

  der_neg_pix = 1B*(smooth_pix_der LT 0)  ; Find negative derivatives
  der_pos_pix = 1B*(smooth_pix_der GE 0)  ; Find positive derivatives

  shifted     = der_pos_pix-shift(der_pos_pix, -1)  ; Find maximums - do derpos-shifted and find the ones
  max_array   = shifted EQ 1

  shifted     = der_neg_pix-shift(der_neg_pix, -1)  ; Find minimums - do derneg-shifted and find the ones
  min_array   = shifted EQ 1

  ; Check if this can be removed !!!
  max_array [opts.win_dim_l]    = 0  ; Adjust at Win_dm_l position, otherwise always EQ 1
  min_array [nb-opts.win_dim_r] = 0  ; Adjust at wIn_dm_r position, otherwise always EQ 1

  return

END
