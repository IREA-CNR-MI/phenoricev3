;+
; NAME:
;   struct_addtags
;
; PURPOSE:
;   Add tags from one structure (array) to another
;
; CALLING SEQUENCE:
;   outstruct = struct_addtags(astruct, bstruct)
;
; INPUTS:
;   astruct    - First structure, which can be an array
;   bstruct    - Second structure, which can be an array
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;   outstruct  - Ouput structure array
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;   The dimensions of the output array match that of ASTRUCT.  For example,
;   if ASTRUCT has dimensions [5,10], and BSTRUCT has dimensions [2,25],
;   the output structure has dimensions [5,10].
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;   copy_struct
;   copy_struct_inx
;
; REVISION HISTORY:
;   28-Jun-2000  Written by D. Schlegel, Princeton
;------------------------------------------------------------------------------
FUNCTION struct_addtags, astruct, bstruct

  IF (N_elements(astruct) EQ 0) THEN $
    return, bstruct

  num1 = N_elements(astruct)
  num2 = N_elements(bstruct)
  IF (num1 NE num2) THEN $
    message, 'Both structures must have the same number of elements'

  ;----------
  ; Create an empty structure with all the tags from both structures

  obj1 = create_struct(astruct[0], bstruct[0])
  dims = size(astruct,/dimens)
  outstruct = make_array(dimension=dims, value=obj1)

  ;----------
  ; Assign elements from ASTRUCT into the new output structure

  copy_struct, astruct, outstruct

  ;----------
  ; Assign elements from BSTRUCT into the new output structure

  copy_struct_inx, bstruct, outstruct

  return, outstruct
END
;------------------------------------------------------------------------------
;

;+
; NAME:
; COPY_STRUCT
; PURPOSE:
;   Copy all fields with matching tag names from one structure to another
; EXPLANATION:
;       COPY_STRUCT is similar to the intrinsic STRUCT_ASSIGN procedure but
;       has optional keywords to exclude or specify specific tags.
;
; Fields with matching tag names are copied from one structure array to
; another structure array of different type.
; This allows copying of tag values when equating the structures of
; different types is not allowed, or when not all tags are to be copied.
; Can also recursively copy from/to structures nested within structures.
; Note that the number of elements in the output structure array
; is automatically adjusted to equal the length of input structure array.
; If this not desired then use pro copy_struct_inx which allows
; specifying via subscripts which elements are copied where in the arrays.
;
; CALLING SEQUENCE:
;
; copy_struct, struct_From, struct_To, NT_copied
; copy_struct, struct_From, struct_To, EXCEPT=["image","misc"]
; copy_struct, struct_From, struct_To, /RECUR_TANDEM
;
; INPUTS:
; struct_From = structure array to copy from.
; struct_To = structure array to copy values to.
;
; KEYWORDS:
;
; EXCEPT_TAGS = string array of tag names to ignore (to NOT copy).
;   Used at all levels of recursion.
;
; SELECT_TAGS = tag names to copy (takes priority over EXCEPT).
;   This keyword is not passed to recursive calls in order
;   to avoid the confusion of not copying tags in sub-structures.
;
; /RECUR_FROM = search for sub-structures in struct_From, and then
;   call copy_struct recursively for those nested structures.
;
; /RECUR_TO = search for sub-structures of struct_To, and then
;   call copy_struct recursively for those nested structures.
;
; /RECUR_TANDEM = call copy_struct recursively for the sub-structures
;   with matching Tag names in struct_From and struct_To
;   (for use when Tag names match but sub-structure types differ).
;
; OUTPUTS:
; struct_To = structure array to which new tag values are copied.
; NT_copied = incremented by total # of tags copied (optional)
;
; INTERNAL:
; Recur_Level = # of times copy_struct calls itself.
;   This argument is for internal recursive execution only.
;   The user call is 1, subsequent recursive calls increment it,
;   and the counter is decremented before returning.
;   The counter is used just to find out if argument checking
;   should be performed, and to set NT_copied = 0 first call.
; EXTERNAL CALLS:
; pro match (when keyword SELECT_TAGS is specified)
; PROCEDURE:
; Match Tag names and then use corresponding Tag numbers.
; HISTORY:
; written 1989 Frank Varosi STX @ NASA/GSFC
;   mod Jul.90 by F.V. added option to copy sub-structures RECURSIVELY.
; mod Aug.90 by F.V. adjust # elements in TO (output) to equal
;     # elements in FROM (input) & count # of fields copied.
; mod Jan.91 by F.V. added Recur_Level as internal argument so that
;     argument checking done just once, to avoid confusion.
;     Checked against Except_Tags in RECUR_FROM option.
; mod Oct.91 by F.V. added option SELECT_TAGS= selected field names.
; mod Aug.95 by W. Landsman to fix match of a single selected tag.
; mod Mar.97 by F.V. do not pass the SELECT_TAGS keyword in recursion.
; Converted to IDL V5.0   W. Landsman   September 1997
;       mod May 01 by D. Schlegel use long integers
;-

PRO copy_struct, struct_From, struct_To, NT_copied, Recur_Level,            $
  EXCEPT_TAGS  = except_Tags, $
  SELECT_TAGS  = select_Tags, $
  RECUR_From   = recur_From,  $
  RECUR_TO     = recur_To,    $
  RECUR_TANDEM = recur_tandem

  IF N_elements( Recur_Level ) NE 1 THEN Recur_Level = 0L

  Ntag_from = N_tags( struct_From )
  Ntag_to = N_tags( struct_To )

  IF (Recur_Level EQ 0) THEN BEGIN  ;check only at first user call.

    NT_copied = 0L

    IF (Ntag_from LE 0) OR (Ntag_to LE 0) THEN BEGIN
      message,"two arguments must be structures",/INFO
      print," "
      print,"syntax:    copy_struct, struct_From, struct_To"
      print," "
      print,"keywords:  EXCEPT_TAGS= , SELECT_TAGS=,  "
      print,"   /RECUR_From,  /RECUR_TO,  /RECUR_TANDEM"
      return
    ENDIF

    N_from = N_elements( struct_From )
    N_to = N_elements( struct_To )

    IF (N_from GT N_to) THEN BEGIN

      message," # elements (" + strtrim( N_to, 2 ) + $
        ") in output TO structure",/INFO
      message," increased to (" + strtrim( N_from, 2 ) + $
        ") as in FROM structure",/INFO
      struct_To = [ struct_To, $
        replicate( struct_To[0], N_from-N_to ) ]

    ENDIF ELSE IF (N_from LT N_to) THEN BEGIN

      message," # elements (" + strtrim( N_to, 2 ) + $
        ") in output TO structure",/INFO
      message," decreased to (" + strtrim( N_from, 2 ) + $
        ") as in FROM structure",/INFO
      struct_To = struct_To[0:N_from-1]
    ENDIF
  ENDIF

  Recur_Level = Recur_Level + 1   ;go for it...

  Tags_from = Tag_names( struct_From )
  Tags_to = Tag_names( struct_To )
  wto = lindgen( Ntag_to )

  ;Determine which Tags are selected or excluded from copying:

  Nseltag = N_elements( select_Tags )
  Nextag = N_elements( except_Tags )

  IF (Nseltag GT 0) THEN BEGIN

    match, Tags_to, [strupcase( select_Tags )], mt, ms,COUNT=Ntag_to

    IF (Ntag_to LE 0) THEN BEGIN
      message," selected tags not found",/INFO
      return
    ENDIF

    Tags_to = Tags_to[mt]
    wto = wto[mt]

  ENDIF ELSE IF (Nextag GT 0) THEN BEGIN

    except_Tags = [strupcase( except_Tags )]

    FOR t=0L,Nextag-1 DO BEGIN
      w = where( Tags_to NE except_Tags[t], Ntag_to )
      Tags_to = Tags_to[w]
      wto = wto[w]
    ENDFOR
  ENDIF

  ;Now find the matching Tags and copy them...

  FOR t = 0L, Ntag_to-1 DO BEGIN

    wf = where( Tags_from EQ Tags_to[t] , nf )

    IF (nf GT 0) THEN BEGIN

      from = wf[0]
      to = wto[t]

      IF keyword_set( recur_tandem ) AND    $
        ( N_tags( struct_To.(to) ) GT 0 ) AND  $
        ( N_tags( struct_From.(from) ) GT 0 ) THEN BEGIN

        struct_tmp = struct_To.(to)

        copy_struct, struct_From.(from), struct_tmp,  $
          NT_copied, Recur_Level,       $
          EXCEPT=except_Tags,           $
          /RECUR_TANDEM,                $
          RECUR_FROM = recur_From,      $
          RECUR_TO   = recur_To

        struct_To.(to) = struct_tmp

      ENDIF ELSE BEGIN

        struct_To.(to) = struct_From.(from)
        NT_copied = NT_copied + 1
      ENDELSE
    ENDIF
  ENDFOR

  ;Handle request for recursion on FROM structure:

  IF keyword_set( recur_From ) THEN BEGIN

    wfrom = lindgen( Ntag_from )

    IF (Nextag GT 0) THEN BEGIN

      FOR t=0L,Nextag-1 DO BEGIN
        w = where( Tags_from NE except_Tags[t], Ntag_from )
        Tags_from = Tags_from[w]
        wfrom = wfrom[w]
      ENDFOR
    ENDIF

    FOR t = 0L, Ntag_from-1 DO BEGIN

      from = wfrom[t]

      IF N_tags( struct_From.(from) ) GT 0 THEN BEGIN

        copy_struct, struct_From.(from), struct_To,        $
          NT_copied, Recur_Level,    $
          EXCEPT=except_Tags,        $
          /RECUR_FROM,               $
          RECUR_TO     = recur_To,   $
          RECUR_TANDEM = recur_tandem
      ENDIF
    ENDFOR
  ENDIF

  ;Handle request for recursion on TO structure:

  IF keyword_set( recur_To ) THEN BEGIN

    FOR t = 0L, Ntag_to-1 DO BEGIN

      to = wto[t]

      IF N_tags( struct_To.(to) ) GT 0 THEN BEGIN

        struct_tmp = struct_To.(to)

        copy_struct, struct_From, struct_tmp,              $
          NT_copied, Recur_Level,    $
          EXCEPT=except_Tags,        $
          /RECUR_TO,                 $
          RECUR_FROM = recur_From,   $
          RECUR_TANDEM = recur_tandem
        struct_To.(to) = struct_tmp
      ENDIF
    ENDFOR
  ENDIF

  Recur_Level = Recur_Level - 1
END

;+
; NAME:
; COPY_STRUCT_INX
; PURPOSE:
; Copy matching tags & specified indices from one structure to another
; EXPLANATION:
;   Copy all fields with matching tag names (except for "except_Tags")
; from one structure array to another structure array of different type.
; This allows copying of tag values when equating the structures of
; different types is not allowed, or when not all tags are to be copied.
; Can also recursively copy from/to structures nested within structures.
; This procedure is same as copy_struct with option to
; specify indices (subscripts) of which array elements to copy from/to.
; CALLING SEQUENCE:
;
; copy_struct_inx, struct_From, struct_To, NT_copied, INDEX_FROM=subf
;
; copy_struct_inx, struct_From, struct_To, INDEX_FROM=subf, INDEX_TO=subto
;
; INPUTS:
; struct_From = structure array to copy from.
; struct_To = structure array to copy values to.
;
; KEYWORDS:
;
; INDEX_FROM = indices (subscripts) of which elements of array to copy.
;   (default is all elements of input structure array)
;
; INDEX_TO = indices (subscripts) of which elements to copy to.
;   (default is all elements of output structure array)
;
; EXCEPT_TAGS = string array of Tag names to ignore (to NOT copy).
;   Used at all levels of recursion.
;
; SELECT_TAGS = Tag names to copy (takes priority over EXCEPT).
;   This keyword is not passed to recursive calls in order
;   to avoid the confusion of not copying tags in sub-structures.
;
; /RECUR_FROM = search for sub-structures in struct_From, and then
;   call copy_struct recursively for those nested structures.
;
; /RECUR_TO = search for sub-structures of struct_To, and then
;   call copy_struct recursively for those nested structures.
;
; /RECUR_TANDEM = call copy_struct recursively for the sub-structures
;   with matching Tag names in struct_From and struct_To
;   (for use when Tag names match but sub-structure types differ).
;
; OUTPUTS:
; struct_To = structure array to which new tag values are copied.
; NT_copied = incremented by total # of tags copied (optional)
;
; INTERNAL:
; Recur_Level = # of times copy_struct_inx calls itself.
;   This argument is for internal recursive execution only.
;   The user call is 1, subsequent recursive calls increment it,
;   and the counter is decremented before returning.
;   The counter is used just to find out if argument checking
;   should be performed, and to set NT_copied = 0 first call.
; EXTERNAL CALLS:
; pro match (when keyword SELECT_TAGS is specified)
; PROCEDURE:
; Match Tag names and then use corresponding Tag numbers,
; apply the sub-indices during = and recursion.
; HISTORY:
; adapted from copy_struct: 1991 Frank Varosi STX @ NASA/GSFC
; mod Aug.95 by F.V. to fix match of a single selected tag.
; mod Mar.97 by F.V. do not pass the SELECT_TAGS keyword in recursion,
;   and check validity of INDEX_FROM and INDEX_TO in more detail.
; Converted to IDL V5.0   W. Landsman   September 1997
;       Use long integers W. Landsman May 2001
;-

PRO copy_struct_inx, struct_From, struct_To, NT_copied, Recur_Level,        $
  EXCEPT_TAGS  = except_Tags, $
  SELECT_TAGS  = select_Tags, $
  INDEX_From   = index_From,  $
  INDEX_To     = index_To,    $
  RECUR_From   = recur_From,  $
  RECUR_To     = recur_To,    $
  RECUR_TANDEM = recur_tandem

  IF N_elements( Recur_Level ) NE 1 THEN Recur_Level = 0L

  Ntag_from = N_tags( struct_From )
  Ntag_to = N_tags( struct_To )

  IF (Recur_Level EQ 0) THEN BEGIN  ;check only at first user call.

    NT_copied = 0L

    IF (Ntag_from LE 0) OR (Ntag_to LE 0) THEN BEGIN
      message,"two arguments must be structures",/INFO
      print," "
      print,"syntax:  copy_struct_inx, struct_From, struct_To"
      print," "
      print,"keywords:  INDEX_From= , INDEX_To="
      print,"   EXCEPT_TAGS= , SELECT_TAGS=,  "
      print,"   /RECUR_From,  /RECUR_To,  /RECUR_TANDEM"
      return
    ENDIF

    N_from = N_elements( struct_From )
    N_to = N_elements( struct_To )

    IF N_elements( index_From ) LE 0 THEN index_From = $
      lindgen( N_from )
    Ni_from = N_elements( index_From )
    IF N_elements( index_To ) LE 0 THEN index_To = lindgen( Ni_from )
    Ni_to = N_elements( index_To )

    IF (Ni_from LT Ni_to) THEN BEGIN

      message," # elements (" + strtrim( Ni_to, 2 ) + $
        ") in output TO indices",/INFO
      message," decreased to (" + strtrim( Ni_from, 2 ) + $
        ") as in FROM indices",/INFO
      index_To = index_To[0:Ni_from-1]

    ENDIF ELSE IF (Ni_from GT Ni_to) THEN BEGIN

      message," # elements (" + strtrim( Ni_from, 2 ) + $
        ") of input FROM indices",/INFO
      message," decreased to (" + strtrim( Ni_to, 2 ) + $
        ") as in TO indices",/INFO
      index_From = index_From[0:Ni_to-1]
    ENDIF

    Mi_to = max( [index_To] )
    Mi_from = max( [index_From] )

    IF (Mi_to GE N_to) THEN BEGIN

      message," # elements (" + strtrim( N_to, 2 ) + $
        ") in output TO structure",/INFO
      message," increased to (" + strtrim( Mi_to, 2 ) + $
        ") as max value of INDEX_To",/INFO
      struct_To = [ struct_To, $
        replicate( struct_To[0], Mi_to-N_to ) ]
    ENDIF

    IF (Mi_from GE N_from) THEN BEGIN

      w = where( index_From LT N_from, nw )

      IF (nw GT 0) THEN BEGIN
        index_From = index_From[w]
        message,"max value (" + strtrim( Mi_from, 2 ) +$
          ") in FROM indices",/INFO
        print,"decreased to " + strtrim( N_from,2 ) + $
          ") as in FROM structure",/INFO
      ENDIF ELSE BEGIN
        message,"all FROM indices are out of bounds",/IN
        return
      ENDELSE
    ENDIF
  ENDIF

  Recur_Level = Recur_Level + 1   ;go for it...

  Tags_from = Tag_names( struct_From )
  Tags_to = Tag_names( struct_To )
  wto = lindgen( Ntag_to )

  ;Determine which Tags are selected or excluded from copying:

  Nseltag = N_elements( select_Tags )
  Nextag = N_elements( except_Tags )

  IF (Nseltag GT 0) THEN BEGIN

    match, Tags_to, [strupcase( select_Tags )], mt, ms,COUNT=Ntag_to

    IF (Ntag_to LE 0) THEN BEGIN
      message," selected tags not found",/INFO
      return
    ENDIF

    Tags_to = Tags_to[mt]
    wto = wto[mt]

  ENDIF ELSE IF (Nextag GT 0) THEN BEGIN

    except_Tags = [strupcase( except_Tags )]

    FOR t=0L,Nextag-1 DO BEGIN
      w = where( Tags_to NE except_Tags[t], Ntag_to )
      Tags_to = Tags_to[w]
      wto = wto[w]
    ENDFOR
  ENDIF

  ;Now find the matching Tags and copy them...

  FOR t = 0L, Ntag_to-1 DO BEGIN

    wf = where( Tags_from EQ Tags_to[t] , nf )

    IF (nf GT 0) THEN BEGIN

      from = wf[0]
      to = wto[t]

      IF keyword_set( recur_tandem ) AND    $
        ( N_tags( struct_To.(to) ) GT 0 ) AND  $
        ( N_tags( struct_From.(from) ) GT 0 ) THEN BEGIN

        struct_tmp = struct_To[index_To].(to)

        copy_struct, struct_From[index_From].(from),  $
          struct_tmp,                   $
          NT_copied, Recur_Level,       $
          EXCEPT=except_Tags,           $
          /RECUR_TANDEM,                $
          RECUR_FROM = recur_From,      $
          RECUR_To   = recur_To

        struct_To[index_To].(to) = struct_tmp

      ENDIF ELSE BEGIN

        struct_To[index_To].(to) = $
          struct_From[index_From].(from)
        NT_copied = NT_copied + 1
      ENDELSE
    ENDIF
  ENDFOR

  ;Handle request for recursion on FROM structure:

  IF keyword_set( recur_From ) THEN BEGIN

    wfrom = lindgen( Ntag_from )

    IF (Nextag GT 0) THEN BEGIN

      FOR t=0L,Nextag-1 DO BEGIN
        w = where( Tags_from NE except_Tags[t], Ntag_from )
        Tags_from = Tags_from[w]
        wfrom = wfrom[w]
      ENDFOR
    ENDIF

    FOR t = 0L, Ntag_from-1 DO BEGIN

      from = wfrom[t]

      IF N_tags( struct_From.(from) ) GT 0 THEN BEGIN

        copy_struct_inx, struct_From.(from), struct_To,        $
          NT_copied, Recur_Level,    $
          EXCEPT=except_Tags,        $
          /RECUR_FROM,               $
          INDEX_From   = index_From, $
          INDEX_To     = index_To,   $
          RECUR_To     = recur_To,   $
          RECUR_TANDEM = recur_tandem
      ENDIF
    ENDFOR
  ENDIF

  ;Handle request for recursion on TO structure:

  IF keyword_set( recur_To ) THEN BEGIN

    FOR t = 0L, Ntag_to-1 DO BEGIN

      to = wto[t]

      IF N_tags( struct_To.(to) ) GT 0 THEN BEGIN

        struct_tmp = struct_To[index_To].(to)

        copy_struct_inx, struct_From, struct_tmp,          $
          NT_copied, Recur_Level,    $
          EXCEPT=except_Tags,        $
          /RECUR_To,                 $
          INDEX_From   = index_From, $
          RECUR_FROM = recur_From,   $
          RECUR_TANDEM = recur_tandem
        struct_To[index_To].(to) = struct_tmp
      ENDIF
    ENDFOR
  ENDIF

  Recur_Level = Recur_Level - 1
END


