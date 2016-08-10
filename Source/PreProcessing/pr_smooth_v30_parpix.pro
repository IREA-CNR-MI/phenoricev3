
;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    in_files
;    proc_opts
;    proc_year
;    mapscape
;
;
;
; :Author: lb
;-
FUNCTION pr_smooth_v30_parpix, in_files, proc_opts, proc_year, mapscape

  e = ENVI(/HEADLESS)

  ;- ------------------------------------------------------------------
  ; Open required rasters and get information from header files -------
  ;- ------------------------------------------------------------------

  in_vi = e.OpenRaster(in_files.EVI_file)
  in_quality = e.OpenRaster(in_files.quality_file)
  in_doy = e.OpenRaster(in_files.doy_file)
  in_lc = e.OpenRaster(in_files.lc_file)

  getheader = ENVITask('RasterMetadataItem')
  getheader.INPUT_RASTER = in_vi

  ; Get the acquisition times from the header & retrieve acquisition years
  getheader.KEY = 'time' &  getheader.execute
  times = getheader.value
  years = strmid(times,0,4)
  prev_y =  where(years EQ proc_year -1)

  ; Get the acquisition DOYS from the header
  getheader.KEY = 'Wavelength' &  getheader.execute
  doys_reg = getheader.value

  ; Get number of bands and number of columns
  nb = in_vi.NBANDS
  ncols = in_vi.NCOLUMNS

  ;- ------------------------------------------------------------------
  ;- Initialize output file  ------------------------------------------
  ;- ------------------------------------------------------------------

  IF (FILE_TEST(in_files.Smooth_file) EQ 1) THEN FILE_DELETE, in_files.Smooth_file
  out_smooth_file = ENVIRaster(URI= in_files.smooth_file, NROWS=in_vi.nrows, NCOLUMNS=in_vi.ncolumns, $
    NBANDS=in_vi.nbands, DATA_TYPE = in_vi.data_type)

  ;- ------------------------------------------------------------------
  ;- Start Processing        ------------------------------------------
  ;- ------------------------------------------------------------------

  ; Initialize tiling ---

  tileIterator = in_lc.CreateTileIterator(MODE = 'spectral')

  smooth_dummy = intarr(nb, ncols) + 32767

  FOREACH tile, tileIterator, tile_index DO BEGIN

    ;if (tile_index/100.0 - round(tile_index/100.0) eq 0)  then print , 'PhenoRice VI-smoothing, line', tile_index + 1, '  of:', in_vi.nrows
    print , 'PhenoRice VI-smoothing, line', tile_index + 1, '  of:', in_vi.nrows
    ;   out_SMOOTH = intarr(nb,ns)      ; Initialize smoothed line array

    data_LC = tile         ;--- Extract LC data of each tile:
    lc_ok = where(data_LC EQ 1, count_lc_ok, complement = lc_bad)   ; Find Pixels in the line with "good" lc values

    smooth_line = smooth_dummy

    IF count_lc_ok GT 0 THEN BEGIN ; check on "land cover" mask. If no pixel in the line are "good", skip the line, otherwise, perform smooth
      ; on good pixels

      ; Get data of the necessary multitemporal files
      data_VI = in_vi.GetData(SUB_RECT=tileIterator.current_subrect)
      data_QA = in_quality.GetData(SUB_RECT=tileIterator.current_subrect)
      data_DOY = in_doy.GetData(SUB_RECT=tileIterator.current_subrect)

      IF MAPSCAPE EQ 1 THEN data_DOY[data_DOY EQ -1] = 32767

      ;- ------------------------------------------------------------------
      ; Reshuffle DOYS in case more than one year is on the required time serie + check and replace NODATA on DOYS
      ;- ------------------------------------------------------------------

      IF (min(years) LT proc_year) THEN BEGIN    ; If some bands of previous year required, compute their doy by subtracting 365

        ; Check with GT 12 needed because last doys of a year are set to small values if acquisition in day of composite in next year (e.g.,
        ; on the last image of the year (DOY = 361 or 356, if composition day is on the last "week", the real doy is in the next year and the
        ; value can be 3, or 4)

        prev_y_data = data_DOY[prev_y, *]
        prev_y_data[where(prev_y_data GT 12)] = prev_y_data[where(prev_y_data GT 12)] - 365
        data_DOY [prev_y, * ] = prev_y_data
      ENDIF

      IF (max(years) GT proc_year) THEN BEGIN    ; If some bands of next year required, compute their doy by adding 365
        data_doy [where(years EQ proc_year +1),*] = data_DOY [where(years EQ proc_year +1),*] + 365
      ENDIF

      ;- ----------------------------------------------------------------------------
      ;-  Start cycling on pixels to do the smoothing - parallel implementation -
      ;-  http://daedalus.as.arizona.edu/wiki/ParallelIDL
      ;- ----------------------------------------------------------------------------

      ;FOREACH pixel, lc_ok, ind_pixel DO BEGIN

      nCPUs = !CPU.HW_NCPU-2      ; Find number of available cores
      oBridge = objarr(nCPUs-1)
;      output = intarr(nb, n_elements(lc_ok))
      FOR i=0, nCPUs-1 DO BEGIN   ; cycle on CPUs

        range = [n_elements(lc_ok)/nCPUs*i,n_elements(lc_ok)/nCPUs*(i+1)-1]   ; Divide work among CPUs
        IF i EQ n_elements(oBridge) THEN range[1] = n_elements(lc_ok)-1

        IF i EQ nCPUs-1 THEN BEGIN
          pixs = lc_ok[range[0]:range[1]]
          pix_ind = indgen(n_elements(pixs))
          VI = data_VI [*,pixs]
          QA = data_QA [*,pixs]
          DOY = data_DOY [*,pixs]
          
          results = pr_smooth_pixel_v30_parpix(pix_ind, VI, QA,DOY, nb, proc_opts.win_dim_l, proc_opts.win_dim_r, $
           doys_reg, mapscape)
          ENDIF ELSE BEGIN
            pixs = lc_ok[range[0]:range[1]]
            pix_ind = indgen(n_elements(pixs))
            VI = data_VI [*,pixs]
            QA = data_QA [*,pixs]
            DOY = data_DOY [*,pixs]
          
          oBridge[i] = obj_new('IDL_IDLBridge')
          
          ; provide necessary variables to the CPUs 
          oBridge[i]->SetVar, 'lc_ok', pix_ind
          oBridge[i]->SetVar, 'VI', VI
          oBridge[i]->SetVar, 'QA', QA
          oBridge[i]->SetVar, 'DOY', DOY
          oBridge[i]->SetVar, 'nb', nb
          oBridge[i]->SetVar, 'win_dim_l', proc_opts.win_dim_l
          oBridge[i]->SetVar, 'win_dim_r', proc_opts.win_dim_r
          oBridge[i]->SetVar, 'doys_reg', doys_reg
          oBridge[i]->SetVar, 'mapscape', mapscape
          
          ; define the location of the smoothing function 
          oBridge[i]->Execute, ".r " + FILE_DIRNAME(ProgramRootDir())+PATH_SEP()+"Accessoires"+PATH_SEP()+"pr_smooth_pixel_v30_parpix.pro"
          oBridge[i]->Execute, "results = pr_smooth_pixel_v30_parpix(lc_ok, VI, QA,DOY, nb, win_dim_l, win_dim_r, doys_reg, mapscape)", /nowait
        ENDELSE
      ENDFOR

      notdone = 1
      WHILE notdone DO BEGIN
        done=0
        FOR i=0, n_elements(oBridge)-1 DO $
          done = done+oBridge[i]->Status()
        IF done EQ 0 THEN notdone=done
      ENDWHILE

      FOR i=0, n_elements(oBridge)-1 DO BEGIN
        results = [[OBRIDGE[N_ELEMENTS(OBRIDGE)-1-I]->GETVAR('RESULTS')],[results]]
        obj_destroy, oBridge[n_elements(oBridge)-1-i]
      ENDFOR

      ;return, results
    
      smooth_line[*,lc_ok] = results
      out_smooth_file.SetData, smooth_line, SUB_RECT = tileIterator.current_subrect ; Put the output in the smoothed file line
;      smooth_line [*,pixel] = pr_smooth_pixel_v30_parpix(data_VI, data_QA,data_DOY, nb, proc_opts.win_dim_l, proc_opts.win_dim_r, doys_reg, check,  mapscape)  ; Launch smoothing

      ;ENDFOREACH  ; end cycle on pixels not masked out by lc

    ENDIF ; endif on at least one pixel not masked out

    out_smooth_file.SetData, transpose(smooth_line), SUB_RECT=tileIterator.current_subrect ; Put the output in the smoothed file line

  ENDFOREACH ; End of tiling

  out_smooth_file.metadata.AddItem, 'Wavelength', doys_reg
  out_smooth_file.metadata.AddItem, 'Band Names', "VI_Smoothed" + "_" + strtrim(string(doys_reg),2)
  out_smooth_file.metadata.AddItem, 'time', times
  out_smooth_file.Save

  return, "DONE ! "

END

