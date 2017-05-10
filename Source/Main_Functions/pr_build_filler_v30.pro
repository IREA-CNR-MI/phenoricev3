PRO pr_build_filler_v30, avail_years_files, no_data, in_band, out_name_filler,e, compare_file
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
  
  doy_reg = yearfile.Substring(-7,-5)
  ; TO BE DONE: Check for equal dimensions somehow ! If not equal, do a layer stacking !
  in_lta  = envimetaspectralraster(raster_list, spatialref = raster.spatialref)
  or_data = float(in_lta.getdata())
  dove_na = where(or_data EQ no_data)
  or_data[dove_na] = !values.f_nan
  
  ; Do a check on the difference between reported DOY and theoric doy - needed to 
  ; avoid problems on averages computation for the last image of the year
  dove_bigdiff = where(abs(or_data - doy_reg) GT 18) 
  or_data[dove_bigdiff] = or_data[dove_bigdiff] + 365  ; Substiute "wrong" doys ! 
  
  
  avg_data = mean(or_data, dimension=3, /Nan)    ; Compute average
  dove_na_avg = where(finite(avg_data) EQ 0, na_avg_cnt)   ; Where infinite, set to NA
  IF (na_avg_cnt NE 0 ) THEN BEGIN
    avg_data [dove_na_avg] = no_data
  ENDIF
  ; Round the data to save space
  avg_data = round(temporary(avg_data))
  ncols = in_lta.ncolumns
  nrows = in_lta.nrows
  print, ncols
  print, nrows
  
  file_delete, out_name_filler, /ALLOW_NONEXISTENT
  file_delete,(out_name_filler.remove(-3)+'hdr'),/ALLOW_NONEXISTENT
  out_raster = enviraster(URI = out_name_filler, NROWS=nrows, NCOLUMNS=ncols, NBANDS = 1, $
    data_type = 2, SPATIALREF = in_lta.spatialref)
    out_raster.SetData, avg_data
  out_raster.save
  out_raster.close


;  ; Save output filler file
;  file_mkdir,file_dirname(out_name_filler)
;  envi_write_envi_file, avg_data, out_name = out_name_filler, r_fid = r_fid,  interleave = 0, $
;    nb =1, nl = nl, ns = ns, map_info = map_info, /no_open
;
;  heap_gc
;  fid = ENVIRasterToFID(in_lta)
;  data_tot_doy
;  ENVI_FILE_QUERY,fid, DIMS = DIMS, NB = nb
;  ENVI_DOIT,'ENVI_Sum_Data_Doit',$
;    DIMS = dims, $
;    FID = fid, $
;    POS = lindgen(nb), $
;    compute_flag = [0,0,1,0,0,0,0,0],$
;    out_dt = 4, $
;    out_bname = ['mean'], /in_memory, r_fid = r_fid
;    
;    stop
;  FOREACH yearfile, avail_years_files, yearfile_index DO BEGIN
;    envi_open_file, yearfile, r_fid = in_fid, /no_realize,/INVISIBLE
;    IF yearfile_index EQ 0 THEN BEGIN
;      envi_file_query, in_fid, dims=dims, nb=nb, ns=ns, nl=nl, ystart=ystart,   $
;        bnames=bnames, data_type=data_type, xstart=xstart, sensor_type=sensor_type, wl = wl ,map_info = map_info
;      data_tot_doy = fltarr(ns,nl,n_elements(avail_years_files))
;      map_info = ENVI_GET_MAP_INFO(FID=in_fid)
;    ENDIF
;
;    data_tot_doy[*,*,yearfile_index] = envi_get_data(fid = in_fid, dims = dims, pos = 0)
;    ; Workaround to prevent incorrect computation of average DOYS on last dates of the year if computed filler is of "DOY" type
;    IF (in_band EQ "DOY") THEN BEGIN
;
;      dove_wrong = where(abs(data_tot_doy[*,*,yearfile_index] - doy) GT 16, count_wrong)
;      IF (count_wrong NE 0 ) THEN BEGIN
;        temp = data_tot_doy[*,*,yearfile_index]
;        temp [dove_wrong] = temp [dove_wrong] + 365
;        data_tot_doy[*,*,yearfile_index] = temp
;      ENDIF
;
;    ENDIF
;
;
;    envi_file_mng, id = in_fid, /remove
;  ENDFOREACH

  ; Remove nodatas

 

END