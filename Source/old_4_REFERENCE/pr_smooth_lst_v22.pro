;
; Title pr_smooth_lst
;:Description 
;   
;   Function used to remove sudden drops from LST time series due to NODATA 
;   values
;
;Details
;
;   Checks for NODAT (Temp = -273.15) and substitues with an average of before/after values
;   
; :Params:
;    lst_pix: Array of LST values for thepixel
;
;:RETURNS:
;
;   Updates the LST_pix array with gap-filled values
;
;:AUTHOR: Lorenzo Busetto, phD (2014)
;  #' email: busetto.l@@irea.cnr.it
;
;License GPL(>2)
;-
function pr_smooth_lst_v22, lst_pix,nb
  compile_opt idl2
    
  zero_positions = where ((lst_pix LT -50) OR (lst_pix GT 50))    ; Find gaps
  
  foreach element, zero_positions do begin
    
    valid_before = where((lst_pix [0:(element-1)] GT -50) AND (lst_pix [0:(element-1)] LT 50), count_before)
    valid_after = where((lst_pix [(element+1)<(nb-1):(nb-1)] GT -50) AND (lst_pix [(element+1)<(nb-1):(nb-1)] LT 50), count_after)+element+1
   
    if (count_before NE 0 and count_after ne 0 ) then begin    ;sUBSTITUE WITH AVERAGE OF VALID VALUES IMMEDIATELY BEFOR EAND AFTER THE "GAP" 
      lst_pix[element] =lst_pix[max(valid_before)]+(element-max(valid_before))*(lst_pix[min(valid_after)]-lst_pix[max(valid_before)])/(min(valid_after)- max(valid_before))  ; Substitues with average
    endif

  endforeach
   return, lst_pix    ; Return to caller
end