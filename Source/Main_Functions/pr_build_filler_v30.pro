PRO pr_build_filler_v30, avail_years_files, no_data, in_band, out_name_filler
  COMPILE_OPT hidden
  COMPILE_OPT IDL2

  FOREACH yearfile, avail_years_files, yearfile_index DO BEGIN
    envi_open_file, yearfile, r_fid = in_fid, /no_realize,/INVISIBLE
    IF yearfile_index EQ 0 THEN BEGIN
      envi_file_query, in_fid, dims=dims, nb=nb, ns=ns, nl=nl, ystart=ystart,   $
        bnames=bnames, data_type=data_type, xstart=xstart, sensor_type=sensor_type, wl = wl ,map_info = map_info
      data_tot_doy = fltarr(ns,nl,n_elements(avail_years_files))
    ENDIF

    data_tot_doy[*,*,yearfile_index] = envi_get_data(fid = in_fid, dims = dims, pos = 0)
    ; Workaround to prevent incorrect computation of average DOYS on last dates of the year if computed filler is of "DOY" type
    IF (in_band EQ "DOY") THEN BEGIN

      dove_wrong = where(abs(data_tot_doy[*,*,yearfile_index] - doy) GT 16, count_wrong)
      IF (count_wrong NE 0 ) THEN BEGIN
        temp = data_tot_doy[*,*,yearfile_index]
        temp [dove_wrong] = temp [dove_wrong] + 365
        data_tot_doy[*,*,yearfile_index] = temp
      ENDIF

    ENDIF


    envi_file_mng, id = in_fid, /remove
  ENDFOREACH

  ; Remove nodatas

  dove_na = where(data_tot_doy EQ no_data)
  data_tot_doy[dove_na] = !values.f_nan
  avg_data = mean(data_tot_doy, dimension=3, /Nan)    ; Compute average
  dove_na_avg = where(finite(avg_data) EQ 0, na_avg_cnt)   ; Where infinite, set to NA
  IF (na_avg_cnt NE 0 ) THEN BEGIN
    avg_data [dove_na_avg] = no_data
  ENDIF
  ; Round the data to save space
  avg_data = round(temporary(avg_data))

  ; Save output filler file
  file_mkdir,file_dirname(out_name_filler)
  envi_write_envi_file, avg_data, out_name = out_name_filler, r_fid = r_fid,  interleave = 0, $
    nb =1, nl = nl, ns = ns, map_info = map_info,/no_open
  envi_file_mng, id = r_fid, /remove
  heap_gc

END