;+pr_process_v30_parline
;
; :Description:
;    Function used to launch temporal smoothing on EVI data and to launch pheno processing
;
;     --> Cycles on lines and prepares the inputs for the pr_smooth_pixel function
;     --> Launches processing on  EVI array of each smoothed pixel
;     --> Retrieves output and creates the output matrix to pass back
;
; :Params:
;    opts             : PhenoRice options structure. contains info about width of smooth window
;    lines            : intarray contains line numbers of input data to be proecessed ("chunks")
;    data_lc          : intarr (nc, nl) containing land cover mask data
;    data_VI          : intarr (nb, nc, nl) containing EVI data
;    data_QA          : intarr (nb, nc, nl) containing Quality data
;    data_DOY         : intarr (nb, nc, nl) containing DOY data
;    data_lst         : intarr (nb, nc, nl) containing LST data
;    data_NDFI        : intarr (nb, nc, nl) containing NDFI data
;    nb               : int number of bands in input/smooothed rasters
;    ncols            : int number of columns in input data
;    doys_reg         : intarr (nb) regular dates array
;    years            : intarr (nb) years of acquisition of the different bands
;    prev_y           : intarr vector indicationg which bands fall in previous year wrt proc_year
;    smooth_flag      : flag Indicates if smoothing has to be performed even if data already present
;    nb_out           : int number of bands that will be in output matrix (1 + n_sel_season*N° of sel. outputs)
;    smooth_matrix    : intarr (nb, nc, nl) Input/Output matrix of smoothed EVI
;    empty_out_matrix : intarr (nb_out, nc, nl) Output matrix
;
; :Returns:
;
;    out_matrix       : intarr  Output matrix (nb_out, nc, nl) (nb + nb_out, nc, nl if smoothing performed)
;
; :Author:
;   Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it (2016)
;
; :License: GPL>3.0
;-
FUNCTION pr_process_v30_parline, opts, lines, data_lc, data_VI, data_QA, data_DOY, data_lst, data_NDFI, nb, $
  ncols, doys_reg, years, prev_y, smooth_flag, nb_out, smooth_matrix, empty_out_matrix

  COMPILE_OPT IDL2
  COMPILE_OPT hidden

  ; Initilize recycled variables

  out_matrix = empty_out_matrix
  x_vect    = [1,2,3,5,6,7]
  sum_index = indgen(opts.win_dim_r*2+1)-opts.win_dim_r
  smooth_1  = (smooth_2 = fltarr(nb))

  IF opts.mapscape EQ 1 THEN data_DOY[where(data_DOY EQ -1)] = 32767

  ;- ------------------------------------------------------------------
  ; Reshuffle DOYS in case more than one year is on the required time serie
  ; + check and replace NODATA on DOYS
  ;- ------------------------------------------------------------------

  IF (min(years) LT opts.proc_year) THEN BEGIN    ; If some bands of previous year required, compute their doy by subtracting 365

    ; Check with GT 12 needed because last doys of a year are set to small values if acquisition in day of composite in next year (e.g.,
    ; on the last image of the year (DOY = 361 or 356, if composition day is on the last "week", the real doy is in the next year and the
    ; value can be 3, or 4)

    IF opts.meta THEN prev_y_data = data_DOY[*, *, prev_y] ELSE prev_y_data = data_DOY[prev_y, *, *]
    prev_y_data[where(prev_y_data GT 12)] = prev_y_data[where(prev_y_data GT 12)] - 365
    IF opts.meta THEN data_DOY [*, *, prev_y] = prev_y_data ELSE data_DOY [prev_y, *, *] = prev_y_data
  ENDIF

  IF (max(years) GT opts.proc_year) THEN BEGIN    ; If some bands of next year required, compute their doy by adding 365

    IF opts.meta THEN data_doy [*,*,where(years EQ opts.proc_year +1)] = data_DOY [*,*,where(years EQ opts.proc_year +1)] + 365 $
    ELSE data_doy [where(years EQ opts.proc_year +1),*, *] = data_DOY [where(years EQ opts.proc_year +1),*, *] + 365

  ENDIF

  IF smooth_flag EQ 0 THEN BEGIN
    IF opts.mapscape EQ 0 THEN dove_vi_na  = where(data_vi EQ 32767, count_na) $
    ELSE dove_vi_na  = where(data_vi EQ -3000, count_na)
    data_errors = 200 * (data_QA EQ 0) + 1100 * (data_QA EQ 1) + 3000* (data_QA EQ 2)
    IF (count_na NE 0 ) THEN data_errors [dove_vi_na] = 7000
    data_errors = 1.0/data_errors
  ENDIF


  ;- ------------------------------------------------------------------
  ; Cycle on the lines of the "data chunk" and run the smoothing
  ; on each of its pixels
  ;- ------------------------------------------------------------------

  FOR line = 0, n_elements(lines)-1 DO BEGIN

    IF opts.META EQ 1 THEN lc_line = data_lc [*,line] ELSE lc_line = data_lc [*,line]
    lc_ok   = where(lc_line NE 0, count_lc_ok, complement = lc_bad)   ; Find Pixels in the line with "good" lc values

    IF count_lc_ok GT 0 THEN BEGIN ; check on "land cover" mask. If no pixel in the line are "good", skip the line

      ; Start cycling on "good pixels"

      FOR ind_pixel = 0, n_elements(lc_ok)-1 DO BEGIN

        pixel  = lc_ok[ind_pixel]
        IF opts.META THEN vi_pix = reform(data_vi [pixel,line,*]) ELSE vi_pix = data_vi [*,pixel,line]       ; Retrieve vi for the pixel/line tuple

        ; Check on vi values: vi = -3000/32767 = NODATA. Will have to remove this in the future !!!!
        IF (opts.mapscape EQ 0) THEN nodata_pos = where(vi_pix EQ 32767, count_NA) ELSE nodata_pos = where(vi_pix EQ -3000, count_NA)

        ; If more than 10 NODATA, skip the pixel and leave it at NODATA

        IF count_NA LT 10 THEN BEGIN

          ; get doy, NDFI, lst data

          IF opts.META THEN doy_pix  = reform(data_DOY [pixel,line,*])  ELSE doy_pix  = data_DOY  [*,pixel,line]
          IF opts.META THEN lst_pix  = reform(data_lst [pixel,line,*])  ELSE lst_pix  = data_lst  [*,pixel,line]
          IF opts.META THEN NDFI_pix = reform(data_NDFI [pixel,line,*]) ELSE NDFI_pix = data_NDFI [*,pixel,line]

          ; -------------------------------------------------
          ; ------ Initial checks on consistency of DOYs ----
          ; -------------------------------------------------

          ; Check on doys: doys > 400 = NODATA in DOY image --> set them to the doy of the composite
          IF opts.mapscape NE 1000 THEN BEGIN
            BAD_DOY = where(abs(DOY_PIX ) GT 1000, count_bad_doy)
          ENDIF ELSE BEGIN
            BAD_DOY = where(DOY_PIX EQ -1, count_bad_doy)
          ENDELSE

          IF (count_bad_doy NE 0 ) THEN DOY_PIX[bad_doy] = doys_reg[bad_doy]

          ; Check for problems on DOYS: if difference between doy_pix and doys_reg LT -16 --> last
          ; doy_pixs wrong because acquired in first days of "next year" --> Add 365 to the DOY

          diff     = doy_pix-doys_reg
          bad_diff = where(diff LT -16, count_baddiff)
          IF (count_baddiff GT 0 ) THEN BEGIN
            doy_pix[bad_diff] = doy_pix[bad_diff]+365
          ENDIF

          ; Final check on doys. If still problems stop (Should never happen now !!!!)
          diff =max(abs(doy_pix-doys_reg))
          IF (diff GT 16) THEN BEGIN
            print, "Something wrong was found on the DOYS.... code to be recheked. contact developers @ lbusett@gmail.com"
            stop
          ENDIF

          ; -----------------------------------------------------------------------
          ; ------ launch the smoothing (or just get data from available file) ----
          ; -----------------------------------------------------------------------

          IF (smooth_flag EQ 1) THEN BEGIN

            IF opts.META THEN smooth_pix = smooth_matrix[pixel, line, *] ELSE smooth_pix = smooth_matrix[*, pixel, line]   ; get data from smoothed file

          ENDIF ELSE BEGIN ; otherwise, perform smoothing

            ; prepare data
            IF opts.META THEN qa_pix  = data_QA [pixel, line, *]     ELSE qa_pix  = data_QA [*,pixel, line]
            IF opts.META THEN err_pix = data_errors [pixel, line, *] ELSE err_pix = data_errors [*,pixel,line]

            ;launch smoothing
            smooth_pix = fix(pr_smooth_pix_v30(opts, vi_pix, qa_pix, doy_pix, nb, doys_reg,$
              smooth_1, sum_index, smooth_2, err_pix, x_vect))
            IF opts.META THEN smooth_matrix[pixel, line, *] = smooth_pix ELSE smooth_matrix[*, pixel, line] = smooth_pix

          ENDELSE

          ; -----------------------------------------------------------------------
          ; -------  Now launch the pheno processing on smooth_pix            -----
          ; -----------------------------------------------------------------------
          ;if (pixel EQ 870) and (Line EQ 12) then stop
          out_data = pr_process_pix_v30(opts, smooth_pix, NDFI_pix, lst_pix, doy_pix, nb, doys_reg)

          ; -----------------------------------------------------------------------
          ; --- build the output matrix according to user choices aboout outputs --
          ; --- of interest                                                   -----
          ; -----------------------------------------------------------------------

          IF opts.META THEN out_matrix[pixel, line, 0] = out_data.(0) ELSE out_matrix[0, pixel, line] = out_data.(0)   ; get n_rice_seasons data

          ; get data of the other selected output bands

          out_ind  = 0    ; counter used to get the correct bands and put them in correct bands of out_matrix

          FOR outs = 0, (n_tags(out_data)-2) DO BEGIN

            IF opts.(44+outs) EQ 1 THEN BEGIN
              band_ind = 1 + [(opts.n_sel_Season*out_ind):(opts.n_sel_Season*(out_ind+1)-1)]
              IF opts.META THEN out_matrix[pixel, line, band_ind] = out_data.(outs+1) ELSE out_matrix[band_ind, pixel, line] = out_data.(outs+1)
              out_ind  = out_ind + 1
            ENDIF

          ENDFOR ; end for on output bands

        ENDIF ; End if on less than 10 NA on VI series

      ENDFOR ; end cycle on "good" pixels

    ENDIF ; Endif on check if all line is at 0 in the LC mask

  ENDFOR ; end for on lines

  ; If smoothing was performed, join smooth_matrix and out_matrix in a single output
  ; so that smoothed matrix is available in output for saving !

  IF smooth_flag EQ 0 THEN BEGIN
    IF opts.meta THEN out_matrix = [[[smooth_matrix]],[[out_matrix]]] ELSE out_matrix = [smooth_matrix,out_matrix]
  ENDIF

  ; send back result to the calling routine

  return, out_matrix

END

