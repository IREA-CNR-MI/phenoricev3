;+pr_build_inputs_v30
;
; :Description:
;
; Function used to create the input files for PHENORICE application, starting from MODIS time series created
; by the MODIStsp "R" package (https://github.com/lbusett/MODIStsp)
;
; :Params:
;    or_ts_folder: Folder containing the original MODIS time series files derived from MODIStsp to be used
;                  to extract the input files for PHENORICE processing
;    in_ts_folder: Folder where input files for PHENORICE processing created by the routine will be stored
;    in_bands_or: Codenames of the input bands (see pr_main)
;    in_bands_derived: Codenames used to create filenames of derived bandnames useful for processing
;    out_filename: prefix used to build input file name
;    folder_suffixes_or: Suffixes specifying in which subfolder of or_ts_folders are the different "parameters (e.g., EVI is in  "VI_16Days_250m
;    opts: PhenoRice options structure - needed to determine the total number of bands required for the inputs
;    nodatas_or: NODATA values of original MODIStsp single date images
;    META: dummy - not used at the moment
;    out_files_list: array that will contain the names of the created input files
;    force_rebuild: flag - if set to 1, inpuit files are re-created even if already existing. Useful for testing purposes or to do a "clean"
;    reprocessing
;
; :RETURNS:
;
; out_files_list --> Structure containing full paths of files to be used for PHENORICE processing
;
; :Author:
; 	Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it (2016)
;
; :License: GPL>3.0
;-
FUNCTION pr_build_inputs_v30, or_ts_folder, in_ts_folder, in_bands_or, in_bands_derived, out_filename, $
  folder_suffixes_or, opts, nodatas_or,  META, out_files_list, out_rast_list,  force_rebuild, resizeonmask
  COMPILE_OPT idl2
  COMPILE_OPT hidden

  e = envi(/HEADLESS)         ; Start envi in batch mode
  proc_year = opts.proc_year  ; get processing year

  ;- --------------------------------------------------------- ;
  ;-  Set processing parameters and general variables
  ;- --------------------------------------------------------- ;
  cloud_clear = opts.cloud_clear
  cloud_full = opts.cloud_full

  ; Find the "minimum" doy to be considered for searching a Sowing Date
  vect_mins_pos = fix([opts.doy_1q[0],opts.doy_2q[0],opts.doy_3q[0],opts.doy_4q[0]])
  min_pos = min(vect_mins_pos[where(opts.sel_seasons EQ 1)])

  ; Find the "maximum" doy to be considered for searching a Sowing Date
  vect_maxs_pos = fix([opts.doy_1q[1],opts.doy_2q[1],opts.doy_3q[1],opts.doy_4q[1]])
  max_pos = max(vect_maxs_pos[where(opts.sel_seasons EQ 1)])

  ;- --------------------------------------------------------- ;
  ;   On the basis of above identified ranges, define the bands required for the time series to be used for running Phenorice. To do that:
  ;   For the minimum, compute min_pos - max difference allowed between min and max - half length of the smoothing window - 1
  ;   For the maximum, compute max_pos + number of periods on which decrease is checked (if check decrease = 1) +  half length of the
  ;   smoothing window + 1 period (8 days)
  ;- --------------------------------------------------------- ;

  min_doy = min_pos - 8*opts.max_aft_win[1] - 8*opts.win_dim_l
  max_doy = max_pos + 8*opts.MAT_WIN[1]*opts.MAT_CHECK + 8*opts.win_dim_r + 8 + 8

  ; if min_doy < 0 it means that min_doy is on previous year, so recompute it as the complement to 365 and reset min_year to min_year -1
  IF (min_doy LT 0 ) THEN BEGIN
    min_year = proc_year - 1
    min_doy = 365-abs(min_doy)
  ENDIF ELSE BEGIN
    min_year = proc_year
  ENDELSE

  ; if max_doy > 365 it means that max_doy is on next year, so recompute it adding 365 and reset max_year to max_year+1
  IF (max_doy GT  365 ) THEN BEGIN
    max_year = proc_year + 1
    max_doy = max_doy-365
  ENDIF ELSE BEGIN
    max_year = proc_year
  ENDELSE

  ; Create an array containing the "MODIS ACQUISITION DOYS" and find to which "band" of the series the identified
  ; min and max doys correspond. To be sure to include all required bands, extend of 1 period on the left
  doys_vect = string((indgen(46)*8 + 1),format = '(I3.3)')
  min_period = min(where(doys_vect GT min_doy))-1
  max_period = min(where(doys_vect GT max_doy))

  ;- -----------------------------------------------------------------------
  ; Find the filenames of the required files. These are lately used to check if required filenames exist
  ; or they need to be generated from averages (fillers)
  ;- -----------------------------------------------------------------------

  ; If some files of previous year are needed
  IF ((min_year NE proc_year) AND (max_year EQ proc_year)) THEN BEGIN
    yeardoys_required =  [strtrim(string(proc_year-1),2)+'_'+strtrim(string(doys_vect [min_period : 45]),2), $
      strtrim(string(proc_year),2)+'_'+strtrim(string(doys_vect [0 : max_period]),2)]
  ENDIF

  ; If some files of previous year AND some files from next year are needed
  IF ((min_year NE proc_year) AND (max_year NE proc_year)) THEN BEGIN
    yeardoys_required =  [strtrim(string(proc_year-1),2)+'_'+strtrim(string(doys_vect [min_period : 45]),2), $
      strtrim(string(proc_year),2)+'_'+strtrim(string(doys_vect [0 : 45]),2), $
      strtrim(string(proc_year+1),2)+'_'+strtrim(string(doys_vect [0 : max_period]),2)]
  ENDIF

  ; If some files from next year are needed
  IF ((min_year EQ proc_year) AND (max_year NE proc_year)) THEN BEGIN
    yeardoys_required =  [strtrim(string(proc_year),2)+'_'+strtrim(string(doys_vect [min_period : 45]),2), $
      strtrim(string(proc_year+1),2)+'_'+strtrim(string(doys_vect [0 : max_period]),2)]

  ENDIF

  ; If only files from current year are needed
  IF ((min_year EQ proc_year) AND (max_year EQ proc_year)) THEN BEGIN
    yeardoys_required =  [strtrim(string(proc_year),2)+'_'+strtrim(string(doys_vect [min_period : max_period]),2)]
  ENDIF

  ;- -----------------------------------------------------------------------
  ; Compute a monotonous sequence of required DOYS: Needed to be able to put bands in correct order
  ; while analyzing the time series
  ;- -----------------------------------------------------------------------

  doys_required  = strmid(yeardoys_required,5,3)
  years_required = strmid(yeardoys_required,0,4)

  ; If some bands of previous year required, compute their doy by subtracting 365
  IF (min_year LT proc_year) THEN BEGIN
    doys_required [where(years_required EQ proc_year -1)] = doys_required [where(years_required EQ proc_year -1)] - 365
  ENDIF

  ; If some bands of next year required, compute their doy by adding 365
  IF (max_year GT proc_year) THEN BEGIN
    doys_required [where(years_required EQ proc_year +1)] = doys_required [where(years_required EQ proc_year +1)] + 365
  ENDIF

  smooth_dirname = path_create([in_ts_folder,strtrim(proc_year,2),'VI_Smoothed'])
  out_files_list.smooth_file = path_create([smooth_dirname,file_basename(out_filename)+'VI_smooth_'+yeardoys_required[0]+'_'+yeardoys_required[-1]+'.dat'])
  file_mkdir,smooth_dirname

  out_files_list.out_filename = out_files_list.out_filename + yeardoys_required[0]+'_'+yeardoys_required[-1]+'.dat'

  ;- --------------------------------------------------------- ;
  ;-  Search file system to see which of the required input files
  ;-  are already present and which not,  and create the missing ones
  ;- --------------------------------------------------------- ;

  FOR band = 0L, n_elements(in_bands_or)-1 DO BEGIN   ; Start for cycle on original time series bands

    ; build name for input phenorice file of the parameter (e.g., EVI file)
    out_name = path_create([in_ts_folder+path_sep()+strtrim(proc_year, 2),in_bands_or[band]+ $
      '_ts_input_'+yeardoys_required[0]+'_'+yeardoys_required[-1]+'.json'])

    ;    IF META THEN out_name = out_name.remove(-3)+'json'   ; If META, save a virtual raster as a json file !
    out_files_list.(band) = out_name

    ; Check if already existing. If no, then create it using the MODIStsp inputs + creating fillers

    IF ((file_test(out_name)) EQ 0 OR (force_rebuild EQ 1)) THEN BEGIN

      file_delete, out_name, /ALLOW_NONEXISTENT
      file_delete,(out_name.remove(-3)+'hdr'),/ALLOW_NONEXISTENT
      no_data           = nodatas_or[band]
      or_ts_folder_band = or_ts_folder+path_sep()+folder_suffixes_or[band]+path_sep()+in_bands_or[band]   ; Folder of the band
      pattern           = '*'+in_bands_or[band]+'*.dat'     ; Set the pattern to search for one of the single-date inputs (e.g., NDVI )
      in_files          = file_search(or_ts_folder_band+path_sep()+pattern)   ; Find the files contained in the input folder

      ; From the list of available files, get the doys and years of acquisition of each image
      doys              = strmid(in_files, 6, 3, /REVERSE_OFFSET)   ; get the doys
      years             = strmid(in_files, 11, 4, /REVERSE_OFFSET)  ; get the years
      in_files_required = strarr(n_elements(yeardoys_required))     ; Create empty array of strings - will contain the required input files

      FOREACH yeardoy, yeardoys_required, index_files DO BEGIN

        ; Check if required file exists
        result = where(strmatch(in_files, '*'+yeardoy+'*\.dat'), count_files)

        IF count_files EQ 1 THEN BEGIN ; if exists, get its filename and put it in the list of required files

          in_files_required [index_files] = in_files[result]

        ENDIF ELSE BEGIN   ; If file doesn't exist, build an "average" filler file to be used to fill the series !

          out_name_filler = in_ts_folder+path_sep()+'ph_Fillers'+path_sep()+'AvgFiller'+'_'+ $
            in_bands_or[band]+'_'+yeardoy+'.dat'
          IF ((file_test(out_name_filler) EQ 0) OR (force_rebuild EQ 1)) THEN BEGIN   ; If required filler doesn't exist, then create it
            doy = strmid(yeardoy, 2, /REVERSE_OFFSET)
            print, 'Computing Filler file for DOY'+string(doy)
            doy_positions = where(doys EQ doy, count_doys)
            IF (count_doys LE 3) THEN BEGIN    ; If less than 3 years available, computing the average is unstable  - exit
              print, "Impossible to compute Filler files - Time series available in " + or_ts_folder + "are not sufficiently long !"
              print, "Either reduce the  doy of the last requested analysis period, or extend your input time series !"
              print, "Exiting Program"
              stop
            ENDIF ELSE BEGIN
              avail_years_files = in_files [doy_positions]   ; Get the files of previous years acquired in the same doy
              ; Launch build_filler to create an average file for the missing DOY
              pr_build_filler_v30, avail_years_files, no_data, in_bands_or[band], out_name_filler
            ENDELSE
          ENDIF
          in_files_required [index_files]  =  out_name_filler  ; Put tout_rast_list.doy_filehe filler filename in the correct position of the input files list

        ENDELSE ; end else on existance of required file

      ENDFOREACH ; end foreach on searching required files

      ;--------------------------------------------------------
      ; Create input multitemporal file
      ;--------------------------------------------------------

      print, '# Building Input File - ' +  in_bands_or[band]
      times = strarr(n_elements(in_files_required))   ; Array that will contain images acquisition times

      ; open required single date rasters and assign them the acquisition time
      ; metadata
      FOREACH file, in_files_required, file_ind  DO BEGIN

        date = yeardoys_required[file_ind]
        time = envitime(ACQUISITION = doy2date(fix(strmid(strtrim(date, 2), 5, 3)), fix(strmid(strtrim(date, 2), 0, 4)))+"T00:00:00Z")
        times[file_ind] = time.acquisition

        IF file_ind EQ 0 THEN BEGIN
          raster_list = e.openraster(file)
          IF (raster_list.metadata.hastag ('time')) THEN raster_list.metadata.updateitem, 'time', time.acquisition $
          ELSE raster_list.metadata.additem,'time', time.acquisition
        ENDIF ELSE BEGIN
          raster = e.openraster(file)
          IF (raster.metadata.hastag ('time')) THEN raster.metadata.updateitem, 'time', time.acquisition ELSE raster.metadata.additem,'time', time.acquisition
          raster_list = [raster_list, raster]
        ENDELSE

      ENDFOREACH

      ; Create multiband file using the single date rasters
      result = envimetaspectralraster(raster_list, spatialref = raster.spatialref)
     
      ; on LST input, check the dimensions. If not equal to other files do a quick layer stack
      ;(needed because MODIStsp LST output may have slightly different dimensions from the other
      ; bands !

      IF (band EQ 6) THEN BEGIN

        IF META THEN  compare_file = out_rast_list.(0) ELSE compare_file = out_rast_list.(0)
        ncols = compare_file.ncolumns
        nrows = compare_file.nrows
        IF (result.ncolumns NE ncols OR result.nrows NE nrows) THEN BEGIN
          ref_SpatialGridRaster = envispatialgridraster(envisubsetraster(compare_file, BANDS=1), GRID_DEFINITION = Grid)
          obj_SpatialGridRaster = envispatialgridraster(result, GRID_DEFINITION = Grid)
          result = envimetaspectralraster([obj_SpatialGridRaster], spatialref = obj_SpatialGridRaster.spatialref)
          
        ENDIF

      ENDIF
      
      IF (resizeonmask EQ 1) THEN BEGIN

        ; get the spatial extent from the mask
        mask = e.openraster(out_files_list.LC_FILE)
        UL = mask.spatialref.TIE_POINT_MAP
        LR = [UL[0] + (mask.ncolumns-1)*mask.spatialref.PIXEL_SIZE[0],UL[1] - (mask.nrows-1)*mask.spatialref.PIXEL_SIZE[1]]
        subset = envisubsetraster(result, SUB_RECT=[UL[0],LR[1],LR[0],UL[1]], SPATIALREF = mask.spatialref)
        result = subset
      ENDIF

      
      
      ; add 8 to the reported acquisition DOYs --> done because usually the real DOYs are
      ; in the last part of the period, so that when "substituting" real doy with theoretical
      ; we get less "real" difference

      doys_required = fix(doys_required) + 8

      result.metadata.additem,    'Wavelength', doys_required
      result.metadata.additem,    'time', times
      result.metadata.updateitem, 'Band Names', in_bands_or[band] + "_" + yeardoys_required

;      resultHash     = result.dehydrate()
;      resultHashJSON = json_serialize(resultHash)
;      jsonFile       = out_files_list.(band)
;      openw, LUN, jsonFile, /GET_LUN
;      printf, LUN, resultHashJSON
;      free_lun, LUN

       out_rast_list.(band) = result   ; Put the open virtual raster in the list of required rasters

      ; If not using META, then save the multitemporal file to disk

      IF (META EQ 0) THEN BEGIN
        file_mkdir, file_dirname(OUT_NAME)
        result.export, out_name, 'envi', interleave = opts.interleave, data_ignore_value = no_data
        ; Cleanup - close open files
        result.close

      ENDIF ELSE BEGIN

       ; TO BE DONE WHEN PASSING TO Service pack 1 !!!!
        ; if META, store the multitemporal file in JSON virtual format
        ; Don't close the fiules so that they are still there for Quality computation !
        ;        resultHash = result.dehydrate()
        ;        resultJSON = json_serialize(resultHash)
        ;        jsonFile = out_name.remove(-3)+'json'
        ;        openw, LUN, jsonFile, /GET_LUN
        ;        printf, LUN, ndviJSON
        ;        free_lun, LUN

      ENDELSE

      ; clean up
      FOREACH item, raster_list DO item.close

    ENDIF ELSE print, '# --- ' + in_bands_or[band] + ' File already existing ---- '; End of check on file existence
  ENDFOR       ; End for cycle on band

  ;- --------------------------------------------------------- ;
  ;  Create additional required files from the TS files created above (Quality file)
  ; TODO: Parallelize this execution
  ;- --------------------------------------------------------- ;

;  out_files_list.quality_file = path_create([in_ts_folder+path_sep()+strtrim(proc_year,2), $
;    "Quality"+'_ts_input_'+yeardoys_required[0]+'_'+yeardoys_required[-1]+'.dat'])
;  print, '# --- Building Quality File ---- '
;
;  ; Check if quality file already existing
;  IF ((file_test(out_files_list.quality_file) EQ 0) OR (force_rebuild EQ 1)) THEN BEGIN
;
;    file_delete,out_files_list.quality_file,/ALLOW_NONEXISTENT
;    file_delete,out_files_list.quality_file.remove(-3)+'.hdr',/ALLOW_NONEXISTENT
;
;    IF META EQ 0 THEN BEGIN
;      ; open files needed to compute quality
;      in_rely = e.openraster(out_files_list.rely_file)
;      in_UI   = e.openraster(out_files_list.ui_file)
;      in_blue = e.openraster(out_files_list.blue_file)
;    ENDIF ELSE BEGIN ; OR use the already available METAs
;      in_rely = out_rast_list.rely_file
;      in_UI   = out_rast_list.ui_file
;      in_blue = out_rast_list.blue_file
;    ENDELSE
;
;    ; start computing quality
;    tileIterator = in_rely.createtileiterator(MODE = 'spectral')
;
;    ; Initialize output quality raster
;    IF META THEN interleave = 'bsq' ELSE interleave = 'bip'
;    out_quality_file = enviraster(URI = out_files_list.quality_file, NROWS = in_rely.nrows, NCOLUMNS = in_rely.ncolumns, $
;      NBANDS = in_rely.nbands, DATA_TYPE = in_rely.data_type, interleave = interleave)
;
;    FOREACH tile, tileIterator, line DO BEGIN ; cycle on lines ( = tiles)
;
;      IF line MOD 100 EQ 0 THEN print , line
;      data_rely   = transpose(tile)
;      data_ui     = (in_UI.getdata(SUB_RECT = tileIterator.current_subrect,BANDS = tileiterator.current_band))
;      data_blue   = (in_blue.getdata(BANDS = tileiterator.current_band, SUB_RECT = tileIterator.current_subrect))
;      out_quality = 1B*(data_rely EQ 1)+2B*(data_rely GT 1)+2B*(data_rely LT 0)
;
;      ;--- Update reliability on "intermediate" condition using usefulness index
;      ones   = where(data_Rely EQ 1  , count_1)
;      IF count_1 NE 0 THEN BEGIN
;        ui_one      = data_ui[ones]
;        quality_one = 0B*(ui_one LE 2) +1B*((ui_one GT 2) AND (ui_one LE 10)) + 2B*(ui_one GT 10)
;        out_quality [ones] = quality_one
;      ENDIF
;
;      ;--- Update quality on the basis of blue band (see the excel file in the "docs" folders for the coding)
;      out_quality = 0B*((data_blue LE cloud_clear) AND (out_quality EQ 0)) + $
;        1B*((out_quality EQ 1) AND (data_blue LE cloud_full))  + $
;        1B*((out_quality EQ 0) AND (data_blue GT cloud_clear) AND (data_blue LE cloud_full)) + $
;        2B*((out_quality EQ 2) OR  (data_blue GT cloud_full))
;
;      ; Save line on output file
;      out_quality_file.setdata, out_quality, SUB_RECT=tileIterator.current_subrect, BANDS = tileiterator.current_band
;
;    ENDFOREACH
;
;    ; Add metadata to quality file and save it
;    out_quality_file.metadata.additem, 'Wavelength', doys_required
;    out_quality_file.metadata.additem, 'Band Names', "Quality" + "_" + yeardoys_required
;
;    out_quality_file.save
;    IF META THEN out_rast_list.quality_file = envimetaspectralraster(out_quality_file, spatialref = result.spatialref) $
;    ELSE out_rast_list.quality_file = envimetaspectralraster(out_quality_file, spatialref = in_rely.spatialref)
;    out_quality_file.close
;
;    ; Cleanup
;    in_rely.close
;    in_ui.close
;    in_blue.close
;
;
;  ENDIF ELSE print, '# --- Quality File already existing ---- '
;
;  ; If opts.mapscape is 1 then look for existance of smoothed single bands and try building  the smoothed file
;  ; Will probably remove this completely !
;
;  IF opts.mapscape EQ 100 THEN BEGIN
;
;    smooth_dirname = or_ts_folder+path_sep()+'VI_Smoothed'
;    outname_smooth = smooth_dirname+path_sep()+strtrim(proc_year,2) + path_sep()+file_basename(out_filename) + $
;      '_VI_smooth_'+yeardoys_required[0]+'_'+yeardoys_required[-1]+'.dat'
;
;    ; If multitemporal smoothed file doesn't exist, create it from single date files
;    IF ((file_test(outname_smooth) EQ 0) OR (force_rebuild EQ 1)) THEN BEGIN
;
;      file_mkdir,smooth_dirname
;      in_smoothfiles_required = strarr(n_elements(yeardoys_required))
;      pattern = '*EVIsmooth*.dat'     ; Set the patern to search for one of theinputs (e.g., NDVI )
;      in_files = file_search(smooth_dirname+path_sep()+pattern)   ; Find the file. Issue error if 0 or more than one file
;      ; Create list of required smoothed single band files
;      FOREACH yeardoy, yeardoys_required, index_file DO BEGIN
;        result = where(strmatch(in_files, '*'+yeardoy+'*.dat'), count_files)
;        IF count_files EQ 1 AND  strmid(in_file[result], 3, /REVERSE_OFFSET) EQ '.dat' THEN BEGIN
;          in_smoothfiles_required [index_file] = in_file[result]
;        ENDIF ELSE BEGIN
;          print, 'One or more of the required EVI smoothed files is missing. Please check. Exiting ! '
;          stop
;        ENDELSE
;      ENDFOREACH
;
;      ; Create multiband file
;      FOREACH smoothfile, in_smoothfiles_required, file_ind  DO BEGIN
;        IF file_ind EQ 0 THEN BEGIN
;          raster_list = e.openraster(file)
;        ENDIF ELSE BEGIN
;          raster_list = [raster_list, e.openraster(file)]
;        ENDELSE
;        result = envimetaspectralraster(raster_list, spatialref = raster.spatialref)
;        result.metadata.additem, 'Wavelength', doys_required
;        result.metadata.updateitem, 'Band Names', "VI_Smooothed" + yeardoys_required
;        IF file_test(outname_smooth) EQ 1 THEN BEGIN
;          file_delete, outname_smooth
;          file_delete, (outname_smooth.remove(-3)+'hdr')
;        ENDIF
;        file_mkdir, file_dirname(OUT_NAME)
;        result.export, outname_smooth, 'envi', interleave = opts.interleave, data_ignore_value = no_data
;
;        ; Cleanup
;        FOREACH item, raster_list DO item.close
;        result.close
;      ENDFOREACH
;
;    ENDIF ELSE print, '# ---' + in_bands_or[band] + ' Smoothed File already existing ---- '; End of check on file existence
;
;  ENDIF

  ; Cleanup to close all open files

  DataColl = e.data
  DataItems = DataColl.get()
  FOREACH item, DataItems DO item.close
  e.close

  return, out_files_list

END
