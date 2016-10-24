;+pr_init_processing_v30
;
; :Description:
;
; The function is used to initialize PhenoRice processing and to retrieve results and save them.
; In particular:
;   --> Opens inputs and gets info on raster time series
;   --> Retrieves input options from the opts structure, computes some additional processing option variables
;   --> Sets up the processing, either initializing bridges to CPUS or running in debug mode
;   --> Sets up the output matrixes (smooth and out)
;   --> Launches processing (pr_process_v30_parline)
;   --> Collect outputs from pr_process_v30_parline and saves the outputs files
;
; :Params:
;    in_files : structure containing the names of the input files to be used and of the output file
;    opts     : structure containing all PhenoRice processing parameters (passed by pr_main_v30_gui)
;
; :Author:
; 	Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it (2016)
;
; :License: GPL>3.0
;-
FUNCTION pr_init_processing_v30, in_files, opts, out_rast_list, ind_year

  COMPILE_OPT idl2
  COMPILE_OPT hidden


  IF opts.meta EQ 0 THEN e = envi(/HEADLESS)   ; initialize envi
  e = envi(/HEADLESS)
;- ------------------------------------------------------------------
; Open required rasters and get information from header files -------
;- ------------------------------------------------------------------

  print , '# --- PhenoRice Processing - please wait ! --- #'
  IF opts.meta EQ 0 THEN BEGIN
    in_vi      = e.openraster(in_files.evi_file)
    in_quality = e.openraster(in_files.quality_file)
    in_doy     = e.openraster(in_files.doy_file)
    in_lc      = e.openraster(in_files.lc_file)
    in_NDFI    = e.openraster(in_files.ndfi_file)
    in_lst     = e.openraster(in_files.lst_file)
    in_blue     = e.openraster(in_files.blue_file)
    in_rely     = e.openraster(in_files.rely_file)
    in_ui     = e.openraster(in_files.ui_file)
  ENDIF ELSE BEGIN
    in_vi      = out_rast_list.evi_file
    IF opts.force_rebuild THEN in_quality = out_rast_list.quality_file ELSE in_quality = e.openraster(in_files.quality_file)
    in_doy     = out_rast_list.doy_file
    in_lc      = e.openraster(in_files.lc_file)
    in_NDFI    = out_rast_list.ndfi_file
    in_lst     = out_rast_list.lst_file
    in_blue    = out_rast_list.blue_file
    in_rely    = out_rast_list.rely_file
    in_ui      = out_rast_list.ui_file
    
  ENDELSE

; IF input taken from already existing smoothed file, set smooth_flag to 1
; this avoids re-doing the smoothing. 

  IF file_test(in_files.smooth_file) EQ 1 AND opts.force_resmooth EQ 0 THEN BEGIN
    in_smooth   = e.openraster(in_files.smooth_file)
    smooth_flag = 1     
    print, '# --- Smoothed file already existing - skipping smoothing ---'
  ENDIF ELSE BEGIN
    smooth_flag = 0
    file_delete,in_files.smooth_file,/ALLOW_NONEXISTENT
    file_delete,(in_files.smooth_file.remove(-3)+'hdr'),/ALLOW_NONEXISTENT
  ENDELSE

  ; Get the acquisition times from the header & retrieve acquisition years
  getheader = envitask('RasterMetadataItem')
  getheader.input_raster = in_vi
  getheader.key = 'time' &  getheader.execute
  times     = getheader.value
  years     = times.substring(0,3)
  prev_y    = where(years EQ opts.proc_year -1)

  ; Get the acquisition DOYS from the header
  getheader.key = 'Wavelength' &  getheader.execute
  doys_reg  = getheader.value
  
  ; add 8 to the reported acquisition DOYs --> done because usually the real DOYs are
  ; in the last part of the period, so that when "substituting" real doy with theoretical
  ; we get less "real" difference

  doys_reg = fix(doys_reg) + 8

  ; Get number of bands and number of columns
  nb    = in_vi.nbands
  ncols = in_vi.ncolumns
  nrows = in_vi.nrows

;- ------------------------------------------------------------------
; Set up the output rasters and interleave
; 
; Setting up the rasters beforehand allows to write the data in chunks
; as they are computed, thus allowing to NOT create a huge matrix to 
; contain results and save it at the end. This leads to much higher
; memory and I/O management !!!!
;- ------------------------------------------------------------------

  ; Compute number of required output bands: equal to number of
  ; selected outputs X n° of selected seasons + 1 for n_rice_seasons

  nb_out = opts.n_rice + n_elements(where(opts.sel_seasons EQ 1)) * $
    (opts.max + opts.sow + opts.hh + opts.eos + $
    opts.int + opts.maxvi + opts.minvi + opts.maxmin + opts.eosmin)

  out_filename = in_files.out_filename+'_fullout.dat'

  ; Delete or rename outfilename if already existing !
  IF opts.ovr EQ 1 AND file_test (out_filename) EQ 1 THEN BEGIN
    file_delete, out_filename, /ALLOW_NONEXISTENT, /VERBOSE
    file_delete, out_filename.replace('.dat','.hdr') , /ALLOW_NONEXISTENT
  ENDIF ELSE BEGIN
    IF (file_test (out_filename) EQ 1 ) THEN $
      out_filename = out_filename.replace('.dat','_new.dat')
  ENDELSE
    
  ; TODO: Since the "META" approach seems preferable, all this IFs on interleave 
  ; could be removed !

  IF opts.meta THEN out_interleave = 'bsq' ELSE out_interleave = 'bip'

  out_raster = enviraster(URI = out_filename, NROWS=nrows, NCOLUMNS=ncols, NBANDS = nb_out, $
    data_type = 2, interleave = out_interleave, SPATIALREF = in_vi.spatialref, DATA_IGNORE_VALUE = -999)

  IF smooth_flag EQ 0 THEN $
    smooth_raster = enviraster(URI = in_files.smooth_file, NROWS=nrows, NCOLUMNS=ncols, NBANDS = in_vi.nbands, $
    data_type = 2, interleave = out_interleave, SPATIALREF = in_vi.spatialref, DATA_IGNORE_VALUE = 32767)

;- ------------------------------------------------------------------
; Add to the "opts" structure some recyclable variables so not to have 
; to define / redefine them later several times
;- ------------------------------------------------------------------

  if (ind_year EQ 0) then begin
    addopts = {$; array indexes
      der_ind        : indgen (opts.derivs_opt [0]) + 1  ,$  ; Positions where derivatives should be checked for MAX identification
      maxdec_ind     : indgen (opts.MAT_WIN[1])   + 1  ,$ ; Positions where derivatives should be checked for VI decrease
      growth_ind     : indgen (opts.growth_opt [0]) + 1  ,$ ; positions to be checked to identify consistent growth after minimum
      vi_max_ind     : indgen (opts.max_aft_win[1]  - opts.max_aft_win[0]+1)+opts.max_aft_win[0], $ ; positions to be checked to identify if max is present in suitable period after min
      selquarts      : where  (opts.sel_seasons EQ 1) ,$
      n_sel_season   : total  (opts.sel_seasons) ,$
      pos_legit_maxs : intarr(nb) , $
      pos_quart_max  : intarr(nb) - 1, $
      check_arr_max  : [opts.derivs,opts.max_value, opts.MAT_CHECK], $
      check_arr_min  : [opts.min_value,opts.growth, opts.flood, $  ; Array specisying which min criteria should be considered
      opts.max_after, opts.lst] $
    }
    opts = struct_addtags(opts,addopts)
  endif else begin
      opts.der_ind        = indgen (opts.derivs_opt [0]) + 1    ; Positions where derivatives should be checked for MAX identification
      opts.maxdec_ind     = indgen (opts.MAT_WIN[1])   + 1   ; Positions where derivatives should be checked for VI decrease
      opts.growth_ind     = indgen (opts.growth_opt [0]) + 1   ; positions to be checked to identify consistent growth after minimum
      opts.vi_max_ind     = indgen (opts.max_aft_win[1] - opts.max_aft_win[0]+1)+opts.max_aft_win[0] ; positions to be checked to identify if max is present in suitable period after min
      opts.selquarts      = where  (opts.sel_seasons EQ 1)
      opts.n_sel_season   = total  (opts.sel_seasons)
      opts.pos_legit_maxs = intarr(nb) 
      opts.pos_quart_max  = intarr(nb) - 1
      opts.check_arr_max  = [opts.derivs,opts.max_value, opts.MAT_CHECK]
      opts.check_arr_min  = [opts.min_value,opts.growth, opts.flood, $ ; Array specisying which min criteria should be considered
                            opts.max_after, opts.lst] 
  endelse

  ; Identify positions of legitimate max (i.e., max in the periods defined on the basiss of
  ; user defined seasons )
  pos_quart_max = intarr(nb)-1  ; Initialize array  with -1
  IF (opts.sel_seasons[0] EQ 1) THEN opts.pos_quart_max [where (doys_reg GE opts.doy_1q[0]-7 AND  doys_reg LE opts.doy_1q[1])]  = 0
  IF (opts.sel_seasons[1] EQ 1) THEN opts.pos_quart_max [where (doys_reg GE opts.doy_2q[0]-7 AND  doys_reg LE opts.doy_2q[1])]  = 1
  IF (opts.sel_seasons[2] EQ 1) THEN opts.pos_quart_max [where (doys_reg GE opts.doy_3q[0]-7 AND  doys_reg LE opts.doy_3q[1])]  = 2
  IF (opts.sel_seasons[3] EQ 1) THEN opts.pos_quart_max [where (doys_reg GE opts.doy_4q[0]-7 AND  doys_reg LE opts.doy_4q[1])]  = 3
  opts.pos_legit_maxs = opts.pos_quart_max NE -1

;- ------------------------------------------------------------------
;- Start Processing        ------------------------------------------
;- ------------------------------------------------------------------

  ;- ----------------------------------------------------------------------------
  ;-  Start cycling on blocks of line to do the smoothing - parallel implementation following-
  ;-  http://daedalus.as.arizona.edu/wiki/ParallelIDL and
  ;-  http://www.iluvatar.org/~dwijn/parallelidl
  ;- ----------------------------------------------------------------------------

  IF (opts.method EQ "parallel-line") THEN BEGIN

    ; Build the "chunk ranges" - each chunk is formed by n_cpus*chunk_size lines
    IF (nrows GT opts.chunksize*opts.ncpus) THEN BEGIN

      chunk_ranges = intarr(nrows/(opts.chunksize*opts.ncpus),2)
      n_chunks = n_elements(chunk_ranges[*,1])
      FOR chunk = 0, n_chunks-1 DO chunk_ranges[chunk,*] = [nrows/n_chunks*chunk,nrows/n_chunks*(chunk+1)-1]   ; Divide work among line chunks
      chunk_ranges[n_chunks-1,1] = nrows-1  ; Last chunk gets the last lines
    
    ENDIF ELSE BEGIN

      n_chunks = 1
      chunk_ranges = [0,nrows-1]

    ENDELSE

    ;- ----------------------------------------------------------------------------
    ;-  Initialize bridges - one for each cpu !
    ;- ----------------------------------------------------------------------------

    IF opts.debug EQ 0 THEN BEGIN
      print, "# --- Initializing CPU bridges - please wait ! ---"
      bridges = build_bridges(opts.ncpus)   ; Create bridges
    ENDIF

    print, "# --- Starting the processing ---"
    t1 = systime(1)

    ; Start parallelized processing - for each "chunk", split the lines among cpus
    ; then run processing on the different cpus, wait for all to complete processing
    ; retrieve the outputs and pass to the next "chunk". Dividing by "line chunks" is
    ; necessary to avoid memory problems when working with big input images

    ; if smooth file not exists, output matrix passed back to this routine is composed
    ; by the smmothed VI matrix (nb bands) + the output matrix (nb_out bands). 
    ; otherwise, it contains only nb_out bands

    IF (smooth_flag EQ 1) THEN BEGIN
      IF opts.meta THEN out_matrix = intarr(ncols, nrows, nb_out, /NOZERO) ELSE out_matrix = intarr(nb, ncols, nrows, /NOZERO)
    ENDIF ELSE BEGIN
      IF opts.meta THEN out_matrix = intarr(ncols, nrows, nb + nb_out, /NOZERO) ELSE out_matrix = intarr(nb + nb_out, ncols, nrows, /NOZERO)
    ENDELSE

    ; Start cycling on line chunks
    FOR chunk = 0, n_chunks-1 DO BEGIN   

      print, '# --- Working on chunk of lines: ' + (string(chunk + 1)).trim() + ' of: ' + (string(n_chunks)).trim()
      
      ; find the input lines to be assigned to each CPU (or each block if working in debug mode)

      IF n_chunks GT 1 THEN BEGIN
        cpu_lines = [chunk_ranges[chunk,0]:chunk_ranges[chunk,1]]
      ENDIF ELSE BEGIN
        cpu_lines = [chunk_ranges[0]:chunk_ranges[1]]
      ENDELSE
      n_cpulines  = n_elements(cpu_lines)
      cpu_ranges  = intarr(opts.ncpus, 2)
      FOR cpu_n   = 0, opts.ncpus-1 DO cpu_ranges[cpu_n,*] = [n_cpulines/opts.ncpus*cpu_n, n_cpulines/opts.ncpus*(cpu_n+1)-1]   ; Divide work among CPUs
      cpu_ranges[opts.ncpus-1,1] = n_cpulines-1  ; Last cpu gets the last lines
      cpu_ranges  = cpu_ranges + chunk_ranges[chunk,0]

      ; Now start cycling on CPUs

      FOR cpu_n = 0, opts.ncpus-1 DO BEGIN  

        sel_lines = cpu_ranges[cpu_n, *]   ; Get lines to be assigned to the CPU
        lines     = indgen(sel_lines[1] - sel_lines[0] + 1)
        data_lc   = IN_LC.getdata(SUB_RECT=[0,sel_lines[0],NCOLS-1,sel_lines[1]])
        data_VI   = in_vi.getdata(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
        data_DOY  = in_doy.getdata(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
        data_lst  = 0.02*in_lst.getdata(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])-273.15
        data_NDFI = in_NDFI.getdata(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])

        ; Initialize output matrix to nodata (so that we don't have to build it in each bridge)
        IF opts.meta THEN empty_out_matrix = intarr(ncols,n_elements(lines),nb_out)-999 $
        ELSE empty_out_matrix = intarr(nb_out, ncols, n_elements(lines))-999  

        ; If smoothed file was already existing get the data from the file, otherwise set smooth_matrix
        ; to a dummy array set to NODATA

        IF (smooth_flag EQ 1) THEN BEGIN
          smooth_matrix = in_smooth.getdata(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
          data_QA = 0
        ENDIF ELSE BEGIN
          IF opts.meta THEN BEGIN
            smooth_matrix = 32767+(intarr(ncols, n_elements(lines), nb))
          ENDIF ELSE BEGIN
             smooth_matrix = (intarr(nb, ncols, n_elements(lines))+32767)
          ENDELSE
            ; If smoothed file doesn't exist, get the data from accessory files andcompute the quality layer 
            data_rely = in_rely.getdata(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
            data_blue = in_blue.getdata(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
            data_ui = in_ui.getdata(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]]) 
            data_QA = 0B*data_ui
            ones   = where(data_rely EQ 1, count_1)
            IF count_1 NE 0 THEN BEGIN
              ui_one      = data_ui[ones]
              quality_one = 0B*(ui_one LE 2) +1B*((ui_one GT 2) AND (ui_one LE 10)) + 2B*(ui_one GT 10)
              data_QA [ones] = quality_one
            ENDIF
          
            ;--- Update quality on the basis of blue band (see the excel file in the "docs" folders for the coding)
            data_QA = 0B*((data_blue LE opts.cloud_clear) AND (data_QA EQ 0)) + $
              1B*((data_QA EQ 1) AND (data_blue LE opts.cloud_full))  + $
              1B*((data_QA EQ 0) AND (data_blue GT opts.cloud_clear) AND (data_blue LE opts.cloud_full)) + $
              2B*((data_QA EQ 2) OR  (data_blue GT opts.cloud_full))
      
        ENDELSE

        ; If debug not choosen, pass variables to bridges and execute pr_process_v30_parline to do
        ; the per-pixel processing
        IF (opts.debug EQ 0 ) THEN BEGIN

          ;- ----------------------------------------------------------------------------
          ;-  Process data
          ;- ----------------------------------------------------------------------------
          bridge = bridges[cpu_n]
          print,cpu_n
          
          ; Provide necessary variables to the CPU
          struct_pass   , opts, bridge  ; Pass the opts structure to the bridge
          bridge->setvar, 'lines', lines
          bridge->setvar, 'data_lc', data_lc
          bridge->setvar, 'data_VI', data_VI
          bridge->setvar, 'data_DOY', data_DOY
          bridge->setvar, 'data_lst', data_lst
          bridge->setvar, 'data_NDFI', data_NDFI
          bridge->setvar, 'years', years
          bridge->setvar, 'nb', nb
          bridge->setvar, 'ncols', ncols
          bridge->setvar, 'doys_reg', doys_reg
          bridge->setvar, 'prev_y', prev_y
          bridge->setvar, 'nb_out', nb_out
          bridge->setvar, 'smooth_flag', smooth_flag
          bridge->setvar, 'data_QA', data_QA
          bridge->setvar, 'empty_out_matrix', empty_out_matrix
          bridge->setvar, 'smooth_matrix', smooth_matrix

          ; Execute the processing function. Use "/nowait" to allow concurrent processing on several cores

          bridge->execute, "temp_matrix = pr_process_v30_parline(opts, lines, data_lc, data_VI, data_QA,data_DOY, data_lst, data_NDFI, "$
            + "nb, ncols, doys_reg, years, prev_y, smooth_flag, nb_out, smooth_matrix, empty_out_matrix)", /nowait

        ENDIF ELSE BEGIN ; if debug, simply call processing function on each chunk - slower but "debuggable" !

          print, '# --- Debug mode - working on block: ' + (string(cpu_n+1)).trim() + ' of: ' + (string(opts.ncpus)).trim()
          temp_matrix = pr_process_v30_parline(opts, lines, data_lc, data_VI, data_QA,data_DOY, data_lst, data_NDFI, nb, $
            ncols, doys_reg, years, prev_y, smooth_flag, nb_out, smooth_matrix, empty_out_matrix)

          ; put results in output files.If smoothed file had to be built, extract the smoothed matrix and the
          ; output matrix from the total output matrix and write results to output rasters

          IF smooth_flag EQ 0 THEN BEGIN

            IF opts.meta THEN BEGIN
              temp_smooth = temp_matrix [*,*,0:nb-1]
              temp_out = temp_matrix [*,*,(nb):(nb+nb_out-1)]
            ENDIF ELSE BEGIN
              temp_smooth = temp_matrix [0:nb-1,*,*]
              temp_out = temp_matrix [(nb):(nb+nb_out-1),*,*]
            ENDELSE

            smooth_raster.setdata, temp_smooth, SUB_RECT = [0, sel_lines[0], ncols-1, sel_lines[1]]
            out_raster.setdata, temp_out, SUB_RECT= [0, sel_lines[0], ncols-1, sel_lines[1]]

          ; otherwise, just save the ourput matrix
          ENDIF ELSE BEGIN 
            out_raster.setdata, temp_matrix, SUB_RECT = [0, sel_lines[0], ncols-1, sel_lines[1]]
          ENDELSE

        ENDELSE ; end else on debug mode

        heap_gc

      ENDFOR ; End cycling on cpus

      ; If not in debug mode, set a barrier, so that processing continues only after
      ; all CPUs finish their work on the lines' chunk

      IF opts.debug EQ 0 THEN BEGIN

        barrier_bridges, bridges

        ;- ------------------------------------------------------------------
        ;- Save the results of the different cpus in the output files, in 
        ;- the correct line ranges
        ;- ------------------------------------------------------------------

        FOR cpu_n = 0, n_elements(bridges)-1 DO BEGIN

          temp_matrix = bridges[cpu_n] -> getvar('temp_matrix') ; retrieve result from idle cpu
          sel_lines = cpu_ranges[cpu_n, *]                      ; find lines that were assinged to that cpu

          ; put results in output files.If smoothed file had to be built, extract the smoothed matrix and the
          ; output matrix from the total output matrix and write results to output rasters

          IF smooth_flag EQ 0 THEN BEGIN
            IF opts.meta THEN BEGIN
              temp_smooth = temp_matrix [*,*,0:nb-1]
              temp_out = temp_matrix [*,*,(nb):(nb+nb_out-1)]
            ENDIF ELSE BEGIN
              temp_smooth = temp_matrix [0:nb-1,*,*]
              temp_out = temp_matrix [(nb):(nb+nb_out-1),*,*]
            ENDELSE
            smooth_raster.setdata, temp_smooth, SUB_RECT = [0, sel_lines[0], ncols-1, sel_lines[1]]
            out_raster.setdata, temp_out, SUB_RECT =  [0, sel_lines[0], ncols-1, sel_lines[1]]
          ENDIF ELSE BEGIN ; otherwise, just save the ourput matrix
            out_raster.setdata, temp_matrix, SUB_RECT =  [0, sel_lines[0], ncols-1, sel_lines[1]]
          ENDELSE

        ENDFOR

      ENDIF

    ENDFOR ; End for on chunks

    print, '# --- Finished processing - Saving Outputs --- #'

  ENDIF ; End if on use of parallel processing - see if remove

;- ------------------------------------------------------------------
;- Add metadata to the resulting smoothed file if needed
;- ------------------------------------------------------------------

  IF smooth_flag EQ 0 THEN BEGIN

    smooth_raster.metadata.additem, 'Wavelength', doys_reg
    smooth_raster.metadata.additem, 'Band Names', "VI_Smoothed" + "_" + (string(doys_reg)).trim()
    smooth_raster.metadata.additem, 'time', times
    smooth_raster.metadata.additem, 'DATA_IGNORE_VALUE', 32767
    smooth_raster.writemetadata
    smooth_raster.save
    smooth_raster.close
    
  ENDIF

;- ------------------------------------------------------------------
;- Save the Processed output file
;- ------------------------------------------------------------------

  ; Create the band names of the output raster
  
  ; make a hash table to pass from output codes to band names
  bandhash = hash('N_RICE', 'N_Rice_Seasons', $
    'MAX', 'Doy of Maximum EVI (POS)', $
    'SOW', 'Doy of Sowing (SOS)', $
    'HH', 'Doy of Heading (FOS)', $
    'EOS', 'Doy of Harvest (EOS)', $
    'INT', 'Cumulated EVI', $
    'MAXVI', 'Maximum EVI', $
    'MINVI', 'Minimum EVI', $
    'MAXMIN', 'Length of Vegetative Season (SOS to FOS)', $
    'EOSMIN', 'Length of Season (SOS to EOS)'$
    )

  ; get tagnames of output bands
  tagnames = tag_names(opts)
  tagnames_out = tagnames [45:54]
  bnames = strarr(nb_out)

  ; Cycle on possible output bands and get bnames only for those selected by the user
  out_ind = 0
  FOREACH tagout, tagnames_out, tagind DO BEGIN
    IF opts.(45+tagind) EQ 1 THEN BEGIN
      IF tagind EQ 0 THEN bnames[tagind] = bandhash[tagout] ELSE BEGIN
        nb_save    = opts.n_sel_season
        band_ind   = 1+[nb_save*(out_ind-1):(nb_save*(out_ind)-1)]
        bnames[band_ind] =  bandhash[tagout] + " - Season " + (string(opts.selquarts + 1)).trim()
      ENDELSE
      out_ind = out_ind +1   ; Increment counter ONLY if the band was selected !
    ENDIF
  ENDFOREACH

  ; Update the metadata: band names + creation date !
  out_raster.metadata.additem, 'Band Names', bnames
  time = string(bin_date(systime()))
  time = string(time[0], format = "(I4.4)")+'-'+string(time[1], format = "(I2.2)")+'-'+string(time[2], format = "(I2.2)")
  IF (out_raster.metadata.hastag ('time')) THEN out_raster.metadata.updateitem, 'time', time ELSE out_raster.metadata.additem,'time', time
  out_raster.save
  out_raster.close
  
  ;- ------------------------------------------------------------------
  ;- Final clean up
  ;- ------------------------------------------------------------------

  IF opts.debug EQ 0 THEN burn_bridges, bridges ; Kill bridges
  DataColl = e.data
  DataItems = DataColl.get()
  FOREACH item, DataItems DO item.close
  e.close
  heap_gc

  T2=systime(1)
  print, "# ############################################ #"
  print, "# --- Processing finished correctly ---        #"
  print, "# --- Processing time: " + (string(t2-t1)).trim() + ' --- #'
  print, "# ############################################ #"

  return, "DONE ! "

END

;
  ;
  ;
  ;  ; Cycle on output bands and save only those selected by the user
  ;  FOREACH tagout, tagnames_out, tagind DO BEGIN
  ;    IF opts.(42+tagind) EQ 1 THEN BEGIN
  ;      out_filename = in_files.out_filename + "_" + tagout + ".dat"
  ;      IF tagout EQ "N_RICE" THEN BEGIN  ; Save n_reice band
  ;        nb_save    = 1
  ;        IF opts.meta THEN out_data = reform(out_matrix[*,*,0], ncols, nrows) ELSE out_data   = reform(out_matrix[0,*,*])
  ;        data_type  = 1
  ;        files_list = out_filename
  ;      ENDIF ELSE BEGIN  ; save other bands
  ;        nb_save    = opts.n_sel_season
  ;        band_ind   = 1+[nb_save*(out_ind-1):(nb_save*(out_ind)-1)]
  ;        IF opts.meta THEN out_data = reform(out_matrix[*,*,band_ind], ncols, nrows, nb_save) ELSE out_data   = out_matrix[band_ind,*,*]
  ;        data_type  = 2
  ;        files_list = [files_list, out_filename]
  ;      ENDELSE
  ;
  ;      IF opts.ovr EQ 1 AND file_test (out_filename) EQ 1 THEN BEGIN
  ;        file_delete, out_filename, /ALLOW_NONEXISTENT
  ;        file_delete, out_filename.replace('.dat','.hdr') , /ALLOW_NONEXISTENT
  ;      ENDIF ELSE BEGIN
  ;        IF (file_test (out_filename) EQ 1 ) THEN $
  ;          out_filename = out_filename.replace('.dat','_new.dat')
  ;      ENDELSE
  ;
  ;      ; Save the output file, adding required metadata
  ;
  ;      out_raster   = enviraster(URI=out_filename, NROWS=nrows, NCOLUMNS=ncols, $
  ;        NBANDS = nb_save, data_type = data_type, interleave = interleave, $
  ;        SPATIALREF = in_vi.spatialref, DATA_IGNORE_VALUE = -999)
  ;      out_raster.setdata,out_data
  ;      bname = bandhash[tagout]
  ;      ; Set some additional metadatas: band names ancd creation time
  ;      IF tagout NE "N_RICE" THEN $
  ;        out_raster.metadata.additem, 'Band Names', bname + " - Season " + (string(opts.selquarts)).trim() $
  ;      ELSE $
  ;        out_raster.metadata.additem, 'Band Names', bname
  ;      time = string(bin_date(systime()))
  ;      time = string(time[0], format = "(I4.4)")+'-'+string(time[1], format = "(I2.2)")+'-'+string(time[2], format = "(I2.2)")
  ;      IF (out_raster.metadata.hastag ('time')) THEN out_raster.metadata.updateitem, 'time', time ELSE out_raster.metadata.additem,'time', time
  ;      out_raster.save
  ;      out_raster.close
  ;      ; Increase counter used to get correct data from out_matrix
  ;      out_ind = out_ind +1
  ;    ENDIF
  ;
  ;  ENDFOREACH ; endforeach on possible output bands

  ;- ------------------------------------------------------------------
  ; If requested, also save a multiband file containing all outputs
  ;- ------------------------------------------------------------------
  ; Could change this to creating a META file !!!!
  ;  IF (opts.fullout EQ 1) THEN BEGIN
  ;    fullout_name    = in_files.out_filename+'_fullout.dat'
  ;    FOREACH ff, files_list, indfile DO BEGIN
  ;      IF indfile EQ 0 THEN BEGIN
  ;        raster_list = e.openraster(ff)
  ;      ENDIF ELSE BEGIN
  ;        raster      = e.openraster(ff)
  ;        raster_list = [raster_list,raster]
  ;      ENDELSE
  ;    ENDFOREACH
  ;
  ;    IF opts.ovr EQ 1 AND file_test (fullout_name) EQ 1 THEN BEGIN
  ;      file_delete, fullout_name, /ALLOW_NONEXISTENT
  ;      file_delete, fullout_name.replace('.dat','.hdr') , /ALLOW_NONEXISTENT
  ;    ENDIF ELSE BEGIN
  ;      IF (file_test(fullout_name) EQ 1 ) THEN $
  ;        out_filename = fullout_name.replace('.dat','_new.dat')
  ;    ENDELSE
  ;
  ;    result                 = envimetaspectralraster(raster_list, spatialref = raster.spatialref)
  ;    result.metadata.additem, 'time', time
  ;    getheader              = envitask('RasterMetadataItem')
  ;    getheader.input_raster = result
  ;    getheader.key          = 'Band Names' &  getheader.execute
  ;    old_bnames             = getheader.value
  ;    bnames                 = old_bnames.remove(0,7)
  ;    result.metadata.updateitem, 'Band Names', bnames
  ;    result.export, fullout_name, 'envi', interleave = "bip", data_ignore_value = no_data
  ;
  ;    ; cleanup
  ;    result.close
  ;    FOREACH rr, raster_list DO rr.close

  ;ENDIF

