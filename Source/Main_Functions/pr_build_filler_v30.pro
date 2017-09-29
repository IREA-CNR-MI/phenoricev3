PRO pr_build_filler_v30, avail_years_files, no_data, in_band, out_name_filler, e, compare_file
  COMPILE_OPT hidden
  COMPILE_OPT IDL2
  comprast = compare_file
  FOREACH yearfile, avail_years_files, yearfile_index   DO BEGIN
    raster = e.openraster(yearfile)
    IF (raster.ncolumns NE comprast.ncolumns  OR raster.nrows NE comprast.nrows ) THEN BEGIN
      ref_SpatialGridRaster = envispatialgridraster(envisubsetraster(comprast, BANDS=0), GRID_DEFINITION = Grid)
      obj_SpatialGridRaster = envispatialgridraster(raster, GRID_DEFINITION = Grid)
      raster = envimetaspectralraster([obj_SpatialGridRaster], spatialref = obj_SpatialGridRaster.spatialref)
    ENDIF

    IF yearfile_index EQ 0 THEN raster_list = raster ELSE raster_list = [raster_list, raster]

  ENDFOREACH

  ; Remove nodatas

  doy_reg = yearfile.Substring(-7,-5)
  ; TO BE DONE: Check for equal dimensions somehow ! If not equal, do a layer stacking !
  in_lta  = envimetaspectralraster(raster_list, spatialref = raster.spatialref)
  or_data = float(in_lta.getdata())
  dove_na = where(or_data EQ no_data)
  or_data[dove_na] = !values.f_nan
  ; Do a check on the difference between reported DOY and theoric doy - needed to
  ; avoid problems on averages computation for the last image of the year
  IF (in_band EQ "DOY") THEN BEGIN
    dove_bigdiff = where(abs(or_data - doy_reg) GT 18)
    or_data[dove_bigdiff] = or_data[dove_bigdiff] + 365  ; Substiute "wrong" doys !
  ENDIF

  avg_data = mean(or_data, dimension=3, /Nan)    ; Compute average

  dove_na_avg = where(finite(avg_data) EQ 0, na_avg_cnt)   ; Where infinite, set to NA
  IF (na_avg_cnt NE 0 ) THEN BEGIN
    avg_data [dove_na_avg] = no_data
  ENDIF
  ; Round the data to save space
  avg_data = round(temporary(avg_data))
  ncols = in_lta.ncolumns
  nrows = in_lta.nrows

  file_delete, out_name_filler, /ALLOW_NONEXISTENT
  file_delete,(out_name_filler.remove(-3)+'hdr'),/ALLOW_NONEXISTENT
  out_raster = enviraster(URI = out_name_filler, NROWS=nrows, NCOLUMNS=ncols, NBANDS = 1, $
    data_type = 2, SPATIALREF = in_lta.spatialref)
  out_raster.SetData, avg_data
  out_raster.save
  out_raster.close

END