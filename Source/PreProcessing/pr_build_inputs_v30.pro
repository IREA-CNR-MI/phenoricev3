
; -
; :Description:
;
; Function used to create the filenames for PHENORICE application, starting from MODIS time series created
; by the MODDWL application
;
;Details
;
; The function takes as input the name of a folder containing the original MODIS single date files created by MODIStsp to be used for
; extracting/creating the input files to be used for PHENORICE. It then
; :
;   1) Extracts the files to be used for processing by extracting the "dates" useful to process the selected year according to the selected parameters
;;   2) Creats the "accessory" derived input on the same dates (Quality)
;
; :Params:
;    or_ts_folder: Folder containing the original MODIS time series files derived from MODDWL to be used
;                  to extract the input files for PHENORICE processing
;    in_ts_folder: Folder where input files for PHENORICE processing created by the routine will be stored
;    in_bands_or: Codenames of the input bands (see pr_main)
;    in_bands_derived: Codenames used to create filenames of derived bandnames useful for processing
;    proc_year: Processing year
;    out_file_prefix: pre
;    pr_opts: Structure of general procesing options defined in pr_main
;
; :Params:
;    or_ts_folder Main folder containing the single-band files created by MODIStsp (Includes both the VI_250m and the LST subfolders)
;    in_ts_folder Folders in which the multitemporal (or META) files used as input by phenorice are saved
;    in_bands_or Band prefixes of the required input multitemporal images (e.g., EVI, b3_blue, etc)
;    in_bands_derived Band prefixes of the additional multitemporal files to be created (e.g., Quality - check for removal !)
;    proc_year Processing year
;    out_filename Name of output file
;    folder_suffixes_or Suffixes specifying in which subfolder of or_ts_folders are the different "parameters (e.g., EVI is in  "VI_16Days_250m
;    pr_opts processing options (structure) - needed to determine the total number of bands required for the inputs
;    nodatas_or NODATA values of original MODIStsp single date images
;    resize flag - if = 1 a resize is performed according to "resize_bbox) ( YET TO BE IMPLEMENTED)
;    resize_bbox - contains xmin,xmax, ymin, ymax coordinates to be used for the resize (if required according to resize flag
;    min_criteria - structure containing the criteria to be used on mins and their values (see pr_main_v30_gui.pro)
;    max_criteria  - structure containing the criteria to be used on maxs and their values (see pr_main_v30_gui.pro)
;    MAPSCAPE - flag. If = 1, then the MAPSCAPE smoothed data is used for processing instead than trying to create a smoothed file using pr_smooth_v30
;    META - flag. If = 1, then ENVI META files are created instead than "physical" files
;
; RETURNS:
;
; in_files --> Structure containing full paths of files to be used for PHENORICE processing
;
;
;:AUTHOR: Lorenzo Busetto, phD (2014)
;  #' email: busetto.l@@irea.cnr.it
;
; :Author: lb
; ;License GPL(>2)
; -

function pr_build_inputs_v30,or_ts_folder, in_ts_folder, in_bands_or, in_bands_derived, proc_year, out_filename, folder_suffixes_or, pr_opts, nodatas_or, resize, $
  resize_bbox, min_criteria, max_criteria, MAPSCAPE ,META, out_files_list
  compile_opt idl2

  e = ENVI(/HEADLESS)

  ;- --------------------------------------------------------- ;
  ;-  Set processing parameters and general variables
  ;- --------------------------------------------------------- ;
  cloud_clear = pr_opts.cloud_clear
  cloud_full = pr_opts.cloud_full

  vect_mins_pos = fix([pr_opts.doy_1Q[0],pr_opts.doy_2Q[0],pr_opts.doy_3Q[0],pr_opts.doy_4Q[0]])   ; Find the "minimum" doy to be considered for searching a Sowing Date
  min_pos = min(vect_mins_pos[where(pr_opts.SEL_SEASONS EQ 1)])

  vect_maxs_pos = fix([pr_opts.doy_1Q[1],pr_opts.doy_2Q[1],pr_opts.doy_3Q[1],pr_opts.doy_4Q[1]])  ; Find the "maximum" doy to be considered for searching a Sowing Date
  max_pos = max(vect_maxs_pos[where(pr_opts.SEL_SEASONS EQ 1)])

  ;- --------------------------------------------------------- ;
  ; On the basis of above identified ranges, define the bands required for the time series to be used for running Phenorice. To do that:
  ;   For the minimum, compute min_pos - max difference allowed between min and max - half length of the smoothing window - 1
  ;   For the maximum, compute max_pos + number of periods on which decrease is checked (if check decrease = 1) +  half length of the smoothing window + 1
  ;- --------------------------------------------------------- ;

  min_doy = min_pos - 8*min_criteria.max_aft_win[1] - 8*pr_opts.WIN_DIM_L
  max_doy = max_pos + 8*max_criteria.DECREASE_WIN*max_criteria.decrease + 8*pr_opts.WIN_DIM_R + 1

  ; if min_doy < 0 it means that min_doy is on previous year, so recompute it as the complement to 365 and reset min_year to min_year -1
  if (min_doy LT 0 ) then begin
    min_year = proc_year - 1
    min_doy = 365-abs(min_doy)
  endif else begin
    min_year = proc_year
  endelse

  ; if min_doy < 0 it means that max_doy is on next year, so recompute it adding 365 and reset max_year to max_year+1
  if (max_doy GT  365 ) then begin
    max_year = proc_year + 1
    max_doy = max_doy-365
  endif else begin
    max_year = proc_year
  endelse

  ; Create an array containing the "MODIS ACQUISITION DOYS" and find to which "band" of the serie the identified
  ; min and max doys correspond
  doys_vect = string((indgen(46)*8 + 1),format = '(I3.3)')
  min_period = min(where(doys_vect GT min_doy))-1
  max_period = max(where(doys_vect LT max_doy))

  ; Find the filenames of the required files. These are lately used to check if required filenames exist or they need to be generated from averages (fillers)

  if ((min_year NE proc_year) AND (max_year EQ proc_year)) then begin   ; If some files of previous year are needed

    yeardoys_required =  [strtrim(string(proc_year-1),2)+'_'+strtrim(string(doys_vect [min_period : 45]),2), $
      strtrim(string(proc_year),2)+'_'+strtrim(string(doys_vect [0 : max_period]),2)]

  endif

  if ((min_year NE proc_year) AND (max_year NE proc_year)) then begin  ; If some files of previous year AND some files from next year are needed

    yeardoys_required =  [strtrim(string(proc_year-1),2)+'_'+strtrim(string(doys_vect [min_period : 45]),2), $
      strtrim(string(proc_year),2)+'_'+strtrim(string(doys_vect [0 : 45]),2), $
      strtrim(string(proc_year+1),2)+'_'+strtrim(string(doys_vect [0 : max_period]),2)]

  endif

  if ((min_year EQ proc_year) AND (max_year NE proc_year)) then begin   ; If some files from next year are needed

    yeardoys_required =  [strtrim(string(proc_year),2)+'_'+strtrim(string(doys_vect [min_period : 45]),2), $
      strtrim(string(proc_year+1),2)+'_'+strtrim(string(doys_vect [0 : max_period]),2)]

  endif

  if ((min_year EQ proc_year) AND (max_year EQ proc_year)) then begin   ; If only files from current year are needed

    yeardoys_required =  [strtrim(string(proc_year),2)+'_'+strtrim(string(doys_vect [min_period : max_period]),2)]

  endif

  ; Compute a monotonous sequence of required DOYS: Needed to be able to put bands in correct order while analyzing
  ; the time series
  ;
  doys_required = strmid(yeardoys_required,5,3)
  years_required = strmid(yeardoys_required,0,4)

  if (min_year LT proc_year) then begin    ; If some bands of previous year required, compute their doy by subtracting 365
    doys_required [where(years_required EQ proc_year -1)] = doys_required [where(years_required EQ proc_year -1)] - 365
  endif

  if (max_year GT proc_year) then begin    ; If some bands of next year required, compute their doy by adding 365
    doys_required [where(years_required EQ proc_year +1)] = doys_required [where(years_required EQ proc_year +1)] + 365
  endif

  doys_required = fix(doys_required) + 8

  ;- --------------------------------------------------------- ;
  ;-  Search file system to see which of the required input files are present and which not and create the missing ones
  ;- --------------------------------------------------------- ;

  for band = 0L, n_elements(in_bands_or)-1 do begin   ; Start for cycle on original time sries bands

    no_data = nodatas_or[band]

    ; build  name for input phenorice file
    out_name = path_create([in_ts_folder+path_sep()+strtrim(proc_year, 2),in_bands_or[band]+'_ts_input_'+yeardoys_required[0]+'_'+yeardoys_required[-1]+'.dat'])
    out_files_list.(band) = out_name

    if (FILE_TEST(out_name) EQ 0) then begin    ; Check if already existing. If no, then create it using the MODIStsp inputs + creating fillers

      or_ts_folder_band = or_ts_folder+path_sep()+folder_suffixes_or[band]+path_sep()+in_bands_or[band]   ; Folder of the band
      pattern = '*'+in_bands_or[band]+'*.dat'     ; Set the patern to search for one of the inputs (e.g., NDVI )

      in_files = file_search(or_ts_folder_band+path_sep()+pattern)   ; Find the files contained in the input folder

      ; From the list of available files, get the doys and years of acquisition of each image

      doys = strmid(in_files, 6, 3, /REVERSE_OFFSET)   ; get the doys
      years = strmid(in_files, 11, 4, /REVERSE_OFFSET); get the years
      in_files_required = STRARR(N_ELEMENTS(yeardoys_required))   ; Create empty array of strings - will contain the required input files

      foreach yeardoy, yeardoys_required, index_files do begin

        ; Check if required file exists
        result = where(STRMATCH(in_files, '*'+yeardoy+'*\.dat'), count_files)

        if count_files EQ 1 then begin
          in_files_required [index_files] = in_files[result]

        endif else begin  ; If file doesn't exist, build an "average" filler file to be used to fill in the series !
          out_name_filler = in_ts_folder+path_sep()+'ph_Fillers'+path_sep()+'AvgFiller'+'_'+ $
            in_bands_or[band]+'_'+yeardoy+'.dat'

          if (FILE_TEST(out_name_filler) EQ 0) then begin   ; If required filler doesn't exist, then create it

            doy = strmid(yeardoy, 2, /REVERSE_OFFSET)
            print, 'Computing Filler file for DOY'+string(doy)

            doy_positions = where(doys eq doy, count_doys)

            if (count_doys LE 3) then begin    ; If only 1 file available, computing the average is unstable  - exit

              print, "Impossible to compute Filler files - Time series available in " + or_ts_folder + "are not sufficiently long !"
              print, "Either reduce the  doy of the last requested analysis period, or extend your input time series !"
              print, "Exiting Program"
              stop

            endif else begin

              avail_years_files = in_files [doy_positions]   ; Get the files of previous years acquired in the same doy
              pr_build_filler_v30, avail_years_files, no_data, in_bands_or[band], out_name_filler

            endelse
          endif

          in_files_required [index_files]  =  out_name_filler  ; Put the filler filename in the correct position of the input files list

        endelse

      endforeach

      ;--------------------------------------------------------
      ; Create input multitemporal file ("physical" or META)
      ;--------------------------------------------------------

      print, '# Building Input File - ' +  in_bands_or[band]

      ;      if (META EQ 0 ) then begin
      times = strarr(N_ELEMENTS(in_files_required))

      foreach file, in_files_required, file_ind  do begin
        date = yeardoys_required[file_ind]
        time = ENVITime(ACQUISITION = doy2date(Fix(StrMid(StrTrim(date, 2), 5, 3)),Fix(StrMid(StrTrim(date, 2), 0, 4)))+"T00:00:00Z")
        times[file_ind] = time.acquisition
        if file_ind eq 0 then begin
          raster_list = e.OpenRaster(file)
          IF (raster_list.metadata.HasTag ('time')) THEN raster_list.metadata.UpdateItem, 'time', time.acquisition else raster_list.metadata.AddItem,'time', time.acquisition
          ; raster_list.WriteMetadata
        endif else begin
          raster = e.OpenRaster(file)
          IF (raster.metadata.HasTag ('time')) THEN raster.metadata.UpdateItem, 'time', time.acquisition else raster.metadata.AddItem,'time', time.acquisition
          ; raster.WriteMetadata
          raster_list = [raster_list, raster]
        endelse

      endforeach

      result = ENVIMetaspectralRaster(raster_list, spatialref = raster.spatialref)
      
      if FILE_TEST(out_name) EQ 1 then file_DELETE, out_name

      if (band EQ 6) then begin

        compare_file = e.OpenRaster(out_files_list.(0))
        ncols = compare_file.ncolumns
        nrows = compare_file.nrows

        if (result.ncolumns ne ncols or result.nrows  ne nrows) then begin

          ref_SpatialGridRaster = ENVISpatialGridRaster(ENVISubsetRaster(compare_file, BANDS=1), GRID_DEFINITION=Grid)
          obj_SpatialGridRaster = ENVISpatialGridRaster(result, GRID_DEFINITION=Grid)
          result = ENVIMetaspectralRaster([obj_SpatialGridRaster])

        endif
      ENDIF
      FILE_MKDIR, FILE_DIRNAME(OUT_NAME)
      result.Export, out_name, 'envi', interleave = "bip", data_ignore_value = no_data
      out_rast = e.OpenRaster(out_name)
      out_rast.metadata.AddItem, 'Wavelength', doys_required
      out_rast.metadata.AddItem, 'time', times
      out_rast.metadata.UpdateItem, 'Band Names', in_bands_or[band] + "_" + yeardoys_required
      out_rast.writeMetadata

      foreach item, raster_list do item.Close

      result.Close
      out_rast.Close
    endif else print, '# ---' + in_bands_or[band] + 'File already existing ---- '; End of check on file existence
  endfor       ; End for cycle on band

  ;- --------------------------------------------------------- ;
  ;  Create additional required files from the TS files created above
  ;- --------------------------------------------------------- ;

  out_files_list.Quality_file = path_create([in_ts_folder+path_sep()+strtrim(proc_year,2),"Quality"+'_ts_input_'+yeardoys_required[0]+'_'+yeardoys_required[-1]+'.dat'])

  print, '# Building Quality File - '

  if (file_test(out_files_list.Quality_file) eq 0) then begin    ; Check if already existin

    in_rely = e.OpenRaster(out_files_list.Rely_file)
    in_UI = e.OpenRaster(out_files_list.UI_file)
    in_blue = e.OpenRaster(out_files_list.Blue_file)

    tileIterator = in_rely.CreateTileIterator(MODE = 'spectral')
    count = 0
    out_quality_file = ENVIRaster(URI=out_files_list.Quality_file, NROWS=in_rely.nrows, NCOLUMNS=in_rely.ncolumns, $
      NBANDS=in_rely.nbands, DATA_TYPE=in_rely.data_type, interleave = in_rely.interleave)

    FOREACH tile, tileIterator DO BEGIN

      data_rely = transpose(tile)
      data_ui = (in_UI.GetData(SUB_RECT=tileIterator.current_subrect,BANDS=tileiterator.current_band))
      data_blue = (in_blue.GetData(BANDS=tileiterator.current_band, SUB_RECT=tileIterator.current_subrect))
      out_quality = 1B*(data_rely eq 1)+2B*(data_rely gt 1)+2B*(data_rely lt 0)

      ;------------------------- Update reliability on "intermediate" condition using usefulness index
      ones   = where(data_Rely eq 1  , count_1)
      if count_1 ne 0 then begin
        ui_one = data_ui[ones]
        quality_one = 0B*(ui_one le 2) +1B*((ui_one gt 2) and (ui_one le 10)) + 2B*(ui_one gt 10)
        out_quality [ones] = quality_one
      endif

      ;------------------------- Update quality on the basis of blue band (see the excel file in the "docs" folders for the coding)
      out_quality = 0B*((data_blue le cloud_clear) and (out_quality eq 0)) + $
        1B*((out_quality eq 1) and (data_blue le cloud_full))  + $
        1B*((out_quality eq 0) and (data_blue gt cloud_clear) and (data_blue le cloud_full)) + $
        2B*((out_quality eq 2) or  (data_blue gt cloud_full))

      out_quality_file.SetData, out_quality, SUB_RECT=tileIterator.current_subrect, BANDS = tileiterator.current_band


    ENDFOREACH

    ; out_quality_file.Export, out_files_list.Quality_file,'envi', interleave = "bip", data_ignore_value = 255
    out_quality_file.metadata.AddItem, 'Wavelength', doys_required
    out_quality_file.metadata.AddItem, 'Band Names', "Quality" + "_" + yeardoys_required
    out_quality_file.Save

    in_rely.Close
    in_ui.Close
    in_blue.Close
    out_quality_file.Close

  endif else print, '# --- Quality File already existing ---- '


  if MAPSCAPE EQ 1 then begin   ; If mapscape is 1 then look forexistance of smoothed single bands and build the smoothed file

    smooth_dirname = or_ts_folder+path_sep()+'VI_Smoothed'
    outname_smooth = smooth_dirname+path_sep()+strtrim(proc_year,2) + PATH_SEP()+file_basename(out_filename) + '_VI_smooth_'+strtrim(string(proc_year), 2)+'.dat'


    if file_test(outname_smooth) eq 0 then begin  ; If multitemporal smoothed file doesn't exist, create it from single date files

      FILE_MKDIR,smooth_dirname
      in_smoothfiles_required = STRARR(N_ELEMENTS(yeardoys_required))
      pattern = '*EVIsmooth*.dat'     ; Set the patern to search for one of theinputs (e.g., NDVI )
      in_files = file_search(smooth_dirname+path_sep()+pattern)   ; Find the file. Issue error if 0 or more than one file

      ; Create list of required smoothed single band files
      foreach yeardoy, yeardoys_required, index_file do begin

        result = where(STRMATCH(in_files, '*'+yeardoy+'*.dat'), count_files)

        if count_files EQ 1 AND  strmid(in_file[result], 3, /REVERSE_OFFSET) EQ '.dat' then begin
          in_smoothfiles_required [index_file] = in_file[result]
        endif else begin
          print, 'One or more of the required EVI smoothed files is missing. Please check. Exiting ! '
          stop
        endelse

      endforeach

      ; Create multiband file
      foreach smoothfile, in_smoothfiles_required, file_ind  do begin

        if file_ind eq 0 then begin
          raster_list = e.OpenRaster(file)
        endif else begin
          raster_list = [raster_list, e.OpenRaster(file)]
        endelse

        result = ENVIMetaspectralRaster(raster_list, spatialref = raster.spatialref)
        result.metadata.AddItem, 'Wavelength', doys_required
        result.metadata.UpdateItem, 'Band Names', "VI_Smooothed" + yeardoys_required
        if FILE_TEST(outname_smooth) EQ 1 then file_DELETE, outname_smooth

        FILE_MKDIR, FILE_DIRNAME(OUT_NAME)
        result.Export, outname_smooth, 'envi', interleave = "bip", data_ignore_valuno_data
        
        foreach item, raster_list do item.Close
        result.Close

      endforeach
    
    endif else print, '# ---' + in_bands_or[band] + 'Smoothe File already existing ---- '; End of check on file existence
  
  endif

  return, out_files_list

end
