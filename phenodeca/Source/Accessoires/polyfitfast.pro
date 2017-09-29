;+
; NAME: 
;       POLYFIT
;
; PURPOSE:
; Fit a polynomial to a function using linear least-squares
; NOTE:
;       Twice as fast as POLY_FIT.PRO as tested by Robishaw's 
;       BENCHMARK.PRO due to the miracle of loopless IDL code.
;+
; NAME:
;           FAN
;
; PURPOSE:
;           Take the outer product of the input ARRAY and a
;           UNIT_VECTOR to "fan out" a 1D vector into an array
;           comprised of the vector repeated row-wise NFAN times.
;           Useful for array-wise mathematics (Look Ma, no FOR loops!)
;
; CALLING SEQUENCE:
;           result = fan(array [,nfan, /transpose])
;
; INPUTS:
;           ARRAY - 1D array, input vector
;           NFAN  - number of times to repeat the input vector,
;                   default is N_ELEMENTS(ARRAY)
;
; KEYWORD PARAMETERS:
;
;           TRANSPOSE - Repeat the input vector column-wise
;
; OUTPUTS:
;           A 2D array with N_ELEMENTS(ARRAY) columns and NFAN
;           rows.
;
; EXAMPLE:
;           Fan a FINDGEN of 3 elements, twice.
;
;           IDL> a = findgen(3)
;           IDL> print,fan(a,2)
;                 0.00000      1.00000      2.00000
;                 0.00000      1.00000      2.00000
;
; MODIFICATION HISTORY:
;           Created sometime in ought-2 by JohnJohn
; 06 Dec 2002 JohnJohn- Added some error handling at the beginning
;-
FUNCTION fan,array,nfan,transpose=transpose
  on_error,2  ;if broke then return to sender
  IF n_params() LT 1 THEN BEGIN
    message,'Syntax: f = fan(array [,nfan, /transpose])',/info
    return,-1
  ENDIF

  IF n_elements(nfan) EQ 0 THEN nfan = n_elements(array)
  unit_vector = replicate(1d,nfan)   ;dblarr(nfan)+1.
  IF keyword_set(transpose) THEN new = array##unit_vector $
  ELSE new = unit_vector##array
  return,new
END


;
;       Uses JohnJohn's FAN.PRO 
;       http://astron.berkeley.edu/~johnjohn/idlprocs/fan.pro
;
; CALLING SEQUENCE:
;       coeffs = polyfit(t, y_of_t, degree [, yfit, dum, covariance=])
; 
; KEYWORDS
; MODIFICATION HISTORY:
;       Written by JohnJohn long ago in ought 1 after attending Carl
;       Heiles' lecture on line fitting.
;-
function polyfitfast,t,y,deg,yfit,yfit1,covariance=cov,weight=w
compile_opt idl2
;on_error,2
n = n_elements(t)
pow = indgen(deg+1)
powarr = fan(pow,n,/trans)
x =  fan(double(t),deg+1)
xarr = x^powarr
xarrt = transpose(xarr)
if keyword_set(w) then xarr = fan(double(w),deg+1)*x^powarr 

alpha = xarr##xarrt
beta = xarr##(double(y))
cov = invert(alpha)
a = cov##beta
if n_params() eq 4 then yfit = poly(t,a)
if n_params() eq 5 then yfit1 = poly(t,a) ;;Legacy code, kept for compatibility
return,a
end