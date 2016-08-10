
;
; Title pr_signal_analysis_v2
;:Description
;
;Details
;
; :Params:
;    smooth_pix: array with smoothed VI values (dims = nb)
;    qa_pix: array with quality values (dims = nb)
;    NDII_pix: array with NDII values
;    NDFI_pix: array with NDFI values
;    nb: Number of bands (70)
;    lc_ok: Position of the analyzed pixel within the line
;    out_SMOOTH_DER: array where derivatives of the smoothed array should be stored
;    out_RICE_MAX_ARRAY: array where first guesses for maximums should be stored
;    out_RICE_MIN_ARRAY: array where first guesses for minimums should be stored
;    der_neg_pix: array where negative derivatives for the pixel should be stored (1 = negative der; 0 = otherwise)
;    der_pos_pix: array where positive derivatives for the pixel should be stored (1 = negative der; 0 = otherwise)
;    win_dim_l: width of the savgol smoothing window - right
;    win_dim_r: width of the savgol smoothing window - left
;
;:RETURNS:
;
;   Sets first guesses for minimums and maximums; sets derivatives of smoothed VI 
;
;:AUTHOR: Lorenzo Busetto, phD (2014)
;  #' email: busetto.l@@irea.cnr.it
;
;License GPL(>2)
;-
pro pr_signal_analysis_v22, smooth_pix, NDII_pix, NDFI_pix, nb, lc_ok, $
   out_RICE_MAX_ARRAY, out_RICE_MIN_ARRAY, der_neg_pix, der_pos_pix, win_dim_l, win_dim_r
  
   
  compile_opt IDL2
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
 
  ;- --------------------------------------------------------- ;
  ;-  Put to -9999 the values of NDII and NDFI where quality == removed !!!!!! 
  ;- --------------------------------------------------------- ;
;  
;  Pos2=where((qa_pix eq 2), count2)
;  if (count2 gt 0) then begin
;    NDII_pix[Pos2]=-9999
;    NDFI_pix[Pos2]=-9999
;  endif
  
  smooth_pix_der = smooth_pix - shift(smooth_pix,+1)  ; Compute derivative of the curve using differentiation
  
  der_neg_pix  = 1B*(smooth_pix_der lt 0)         ; Find negative derivatives
  der_pos_pix = 1B*(smooth_pix_der ge 0)          ; Find positive derivatives
  
  shifted = der_pos_pix-shift(der_pos_pix, -1)  ; Find maximums - do derpos-shifted and find the ones
  maxs = shifted eq 1  & maxs[win_dim_l] = 0    ; Adjust at Win_dm_l position, otherwise always EQ 1
  
  shifted = der_neg_pix-shift(der_neg_pix, -1)  ; Find minimums - do derneg-shifted and find the ones
  mins = shifted eq 1  & mins[nb-win_dim_r] = 0  ; Adjust at wIn_dm_r position, otherwise always EQ 1
  
  ;- --------------------------------------------------------- ;
  ;- put outputs in correct positions of arrays
  ;- --------------------------------------------------------- ;
  
;  out_SMOOTH_DER[*,lc_ok]= smooth_pix_der
  out_RICE_MAX_ARRAY[*,lc_ok] = maxs        ; Set Initial guesses for arrays of maxs and mins: = 1 at positions of local minima
  out_RICE_MIN_ARRAY[*,lc_ok] = mins        ; Set Initial guesses for arrays of maxs and mins: = 1 at positions of local maxima

  return
end
  