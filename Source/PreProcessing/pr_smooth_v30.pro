
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
function pr_smooth_v30, in_files, proc_opts, proc_year, mapscape

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

  if (FILE_TEST(in_files.Smooth_file) EQ 1) then FILE_DELETE, in_files.Smooth_file
  out_smooth_file = ENVIRaster(URI= in_files.smooth_file, NROWS=in_vi.nrows, NCOLUMNS=in_vi.ncolumns, $
    NBANDS=in_vi.nbands, DATA_TYPE = in_vi.data_type)

  ;- ------------------------------------------------------------------
  ;- Start Processing        ------------------------------------------
  ;- ------------------------------------------------------------------

  ; Initialize tiling ---
  
  tileIterator = in_lc.CreateTileIterator(MODE = 'spectral')

  if (FILE_TEST(in_files.Smooth_file) EQ 1) then FILE_DELETE, in_files.Smooth_file

  out_smooth_file = ENVIRaster(URI= in_files.smooth_file, NROWS=in_vi.nrows, NCOLUMNS=in_vi.ncolumns, $
    NBANDS=in_vi.nbands, DATA_TYPE = in_vi.data_type)

  smooth_dummy = intarr(nb, ncols) + 32767

  FOREACH tile, tileIterator, tile_index DO BEGIN

    ;if (tile_index/100.0 - round(tile_index/100.0) eq 0)  then print , 'PhenoRice VI-smoothing, line', tile_index + 1, '  of:', in_vi.nrows
    print , 'PhenoRice VI-smoothing, line', tile_index + 1, '  of:', in_vi.nrows
    ;   out_SMOOTH = intarr(nb,ns)      ; Initialize smoothed line array

    data_LC = tile         ;--- Extract LC data of each tile:
    lc_ok = where(data_LC eq 1, count_lc_ok, complement = lc_bad)   ; Find Pixels in the line with "good" lc values

    smooth_line = smooth_dummy

    if count_lc_ok gt 0 then begin ; check on "land cover" mask. If no pixel in the line are "good", skip the line, otherwise, perform smooth
      ; on good pixels
      ;
      ; Get data of the necessary multitemporal files
      data_VI = in_vi.GetData(SUB_RECT=tileIterator.current_subrect)
      data_QA = in_quality.GetData(SUB_RECT=tileIterator.current_subrect)
      data_DOY = in_doy.GetData(SUB_RECT=tileIterator.current_subrect)

      if MAPSCAPE EQ 1 then data_DOY[data_DOY EQ -1] = 32767

      ; ---------------------------
      ; Reshuffle DOYS in case more than one year is on the required time serie + check and replace NODATA on DOYS
      ; ---------------------------
      if (min(years) LT proc_year) then begin    ; If some bands of previous year required, compute their doy by subtracting 365

        prev_y_data = data_DOY[prev_y, *]
        ; Check with GT 12 needed because last doys of a year are set to small values if acquisition in day of composite in next year (e.g.,
        ; on the last image of the year (DOY = 361 or 356, if composition day is on the last "week", the real doy is in the next year and the
        ; value can be 3, or 4)
        prev_y_data[where(prev_y_data gt 12)] = prev_y_data[where(prev_y_data gt 12)] - 365
        data_DOY [prev_y, * ] = prev_y_data
      endif

      if (max(years) GT proc_year) then begin    ; If some bands of next year required, compute their doy by adding 365
        data_doy [where(years EQ proc_year +1),*] = data_DOY [where(years EQ proc_year +1),*] + 365
      endif

      foreach pixel, lc_ok, ind_pixel do begin

        vi_pix  = data_VI [*,pixel]     ; Retrieve vi, qa and doy for the pixel
        qa_pix  = data_QA [*,pixel]
        doy_pix = data_DOY[*,pixel]

        smooth_line [*,pixel] = pr_smooth_pixel_v30(vi_pix, qa_pix,doy_pix, nb, proc_opts.win_dim_l, proc_opts.win_dim_r, doys_reg, check,  mapscape)  ; Launch smoothing
        ;  CATCH, Error_status

        ; Error handler on smoothing: Stop if error
        ;          IF Error_status NE 0 THEN BEGIN
        ;            PRINT, 'Error in Smoothing ! - line: '+ string(tile_index)+' pixel:  ', string(pixel)
        ;            PRINT, 'The pixel will be set to NODATA and skipped'
        ;            stop
        ;            ; Handle the error by keeping the pixel to NODATA
        ;          ENDIF



      endforeach  ; end cycle on pixels not masked out by lc

    endif ; endif on at least one pixel not masked out

    out_smooth_file.SetData, transpose(smooth_line), SUB_RECT=tileIterator.current_subrect ; Put the output in the smoothed file line

  ENDFOREACH ; End of tiling

  out_smooth_file.metadata.AddItem, 'Wavelength', doys_reg
  out_smooth_file.metadata.AddItem, 'Band Names', "VI_Smoothed" + "_" + strtrim(string(doys_reg),2)
  out_smooth_file.metadata.AddItem, 'time', times
  out_smooth_file.Save

  return, "DONE ! "

end

