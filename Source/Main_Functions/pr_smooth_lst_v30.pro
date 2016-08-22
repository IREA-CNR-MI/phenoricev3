;+pr_smooth_lst_v30
;
;:Description
;
;   Function used to remove sudden drops from LST time series due to NODATA
;   values
;
; :Params:
;    lst_pix: Array of LST values for thepixel
;    nb     : nUMBER OF BANDS
;
;:RETURNS:
;
;   Updates the LST_pix array with gap-filled values
;
; :Author:
; 	Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it (2016)
;
; :License: GPL>3.0
;-
FUNCTION pr_smooth_lst_v30, lst_pix, nb
  COMPILE_OPT hidden
  COMPILE_OPT IDL2

  zero_positions = where ((lst_pix LT -50) OR (lst_pix GT 50))    ; Find gaps

  FOREACH element, zero_positions DO BEGIN

    valid_before = where((lst_pix [0:(element-1)] GT -50) AND (lst_pix [0:(element-1)] LT 50), count_before)
    valid_after  = where((lst_pix [(element+1)<(nb-1):(nb-1)] GT -50) AND (lst_pix [(element+1)<(nb-1):(nb-1)] LT 50), count_after)+element+1

    IF (count_before NE 0 AND count_after NE 0 ) THEN BEGIN    ;sUBSTITUE WITH AVERAGE OF VALID VALUES IMMEDIATELY BEFOR EAND AFTER THE "GAP"
      lst_pix[element] = lst_pix[max(valid_before)]+(element-max(valid_before))*(lst_pix[min(valid_after)]- $
                         lst_pix[max(valid_before)])/(min(valid_after)- max(valid_before))  ; Substitues with average
    ENDIF

  ENDFOREACH

  return, lst_pix    ; Return to caller
  
END