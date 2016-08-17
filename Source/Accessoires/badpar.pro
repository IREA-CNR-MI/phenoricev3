;+
; NAME:
;     badpar
; PURPOSE: (one line)
;     Validate an input parameter against valid entries.
; DESCRIPTION:
;
;     This is a general parameter checking function for validating input
;     quantities in other procedures and functions.  This routine will
;     generate an error message indicating what is wrong with the item.
;
;     Example of use:
;
;     pro foo,array
;     if badpar(array,[4,5],2,CALLER='foo') then return
;        .
;        . code for foo .
;        .
;     end
;
;
;     This would cause an immediate return to the routine that called foo
;     with an error message if the input was not either floating or double
;     and 2 dimensional.
;
;     As of IDL v3.0, these are the recognized type codes (see 1-218 in
;        reference guide).
;
;        Type
;        Code     Data Type
;        ----    -----------------------------
;          0      Undefined
;          1      Byte
;          2      Integer
;          3      Longword integer
;          4      Floating point
;          5      Double-precision floating
;          6      Complex floating
;          7      String
;          8      Structure
;          9      Double-precision complex
;         10      Pointer
;         11      Object reference
;         12      Unsigned integer
;         13      Unsigned Longword integer
;         14      64-bit integer
;         15      Unsigned 64-bit integer
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;     val = badpar(param,goodtype,goodrank)
; INPUTS:
;     param    - IDL variable to validate.
;     goodtype - Scalar or vector of type codes that are valid.
;     goodrank - Scalar or vector of valid ranks.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;     CALLER   - String identifying the calling routine.
;     DEFAULT  - Value to return in param if undefined and undefined allowed.
;     DIMEN    - Dimensions of variable.
;     NPTS     - Total number of elements in variable.
;     RANK     - Rank of variable.
;     TYPE     - Type of variable.
; OUTPUTS:
;     Return value is true if the parameter is bad.  False if good.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     3/24/93 - Written by Marc W. Buie, Lowell Observatory.
;     4/27/93 - MWB, added TYPE and DEFAULT keywords.
;     2000/11/1, MWB, added new types for IDL v5.4
;-
FUNCTION badpar,param,goodtype,goodrank, $
  CALLER=caller, DEFAULT=default, DIMEN=dimen, $
  NPTS=npts, RANK=rank, TYPE=type
  COMPILE_OPT IDL2
  COMPILE_OPT hidden
  

  errval = 0

  sz = size(param)

  rank = sz[0]
  type = sz[rank+1]
  npts = sz[rank+2]

  err1=''
  err2=''

  IF rank EQ 0 THEN dimen=0 ELSE dimen = sz[1:rank]

  z=where(goodtype EQ type, count)
  IF count EQ 0 THEN BEGIN
    CASE type OF
      0 :    err1 = 'Undefined variable is not allowed.'
      1 :    err1 = 'Byte variable type is not allowed.'
      2 :    err1 = 'Integer variable type is not allowed.'
      3 :    err1 = 'Longword integer variable type is not allowed.'
      4 :    err1 = 'Floating point variable type is not allowed.'
      5 :    err1 = 'Double-precision floating point variable type is not allowed.'
      6 :    err1 = 'Complex floating point variable type is not allowed.'
      7 :    err1 = 'String variable type is not allowed.'
      8 :    err1 = 'Structure variable type is not allowed.'
      9 :    err1 = 'Double-precision complex type is not allowed.'
      10:    err1 = 'Pointer type is not allowed.'
      11:    err1 = 'Object reference type is not allowed.'
      12:    err1 = 'Unsigned integer type is not allowed.'
      13:    err1 = 'Unsigned longword integer type is not allowed.'
      14:    err1 = '64-bit integer type is not allowed.'
      15:    err1 = 'Unsigned 64-bit integer type is not allowed.'
      ELSE : err1 = 'Unrecognized variable type code.  Impossible!'
    ENDCASE
    errval=1
  ENDIF

  IF type NE 0 THEN BEGIN
    z=where(goodrank EQ rank, count)
    IF count EQ 0 THEN BEGIN
      CASE rank OF
        0 :    err2 = 'Scalar variables are not allowed.'
        1 :    err2 = 'Vector variables are not allowed.'
        2 :    err2 = '2-D variables are not allowed.'
        3 :    err2 = '3-D variables are not allowed.'
        4 :    err2 = '4-D variables are not allowed.'
        5 :    err2 = '5-D variables are not allowed.'
        6 :    err2 = '6-D variables are not allowed.'
        7 :    err2 = '7-D variables are not allowed.'
        8 :    err2 = '8-D variables are not allowed.'
        ELSE : err2 = 'Unrecognized variable rank.  Impossible!'
      ENDCASE
      errval=1
    ENDIF
  ENDIF

  IF errval THEN BEGIN
    IF NOT keyword_set(caller) THEN caller = ''
    print,caller,'Illegal variable encountered.'
    IF err1 NE '' THEN print,err1
    IF err2 NE '' THEN print,err2
    return,errval
  ENDIF

  IF type EQ 0 THEN BEGIN
    szd = size(default)
    IF szd[szd[0]+1] NE 0 THEN BEGIN
      param = default
      sz    = size(param)
      rank  = sz[0]
      type  = sz[rank+1]
      npts  = sz[rank+2]
    ENDIF
  ENDIF

  return,errval

END