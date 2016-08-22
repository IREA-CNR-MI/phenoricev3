;+pr_init_processing_v30_parline
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
FUNCTION pr_init_processing_v30_parline, in_files, opts

  COMPILE_OPT idl2
  COMPILE_OPT hidden

  t1 = systime(1)
  if opt.META EQ 0 then e = ENVI(/HEADLESS)   ; initialize envi

;- ------------------------------------------------------------------
; Open required rasters and get information from header files -------
;- ------------------------------------------------------------------
  print , '# --- PhenoRice Processing - please wait ! --- #'
  
  if opts.META EQ 0 then begin
    in_vi      = e.OpenRaster(in_files.EVI_file)
    in_quality = e.OpenRaster(in_files.QUALITY_file)
    in_doy     = e.OpenRaster(in_files.DOY_file)
    in_lc      = e.OpenRaster(in_files.lc_file)
    in_NDFI    = e.OpenRaster(in_files.NDFI_file)
    in_lst     = e.OpenRaster(in_files.LST_file)
  endif else begin
    in_vi      = out_rast_list.EVI_file
    in_quality = out_rast_list.QUALITY_file
    in_doy     = out_rast_list.DOY_file
    in_lc      = e.OpenRaster(in_files.lc_file)
    in_NDFI    = out_rast_list.NDFI_file
    in_lst     = out_rast_list.LST_file
  endelse

  IF FILE_TEST(in_files.smooth_file) EQ 1 AND opts.force_resmooth EQ 0 THEN BEGIN
    in_smooth   = e.OpenRaster(in_files.smooth_file)
    smooth_flag = 1     ; IF input taken from already existing file, set this flag to 1
    print, '# --- Smoothed file already existing - skipping smoothing ---'
  ENDIF ELSE BEGIN
    smooth_flag = 0
    file_delete,in_files.smooth_file,/ALLOW_NONEXISTENT
    file_delete,(in_files.smooth_file.remove(-3)+'hdr'),/ALLOW_NONEXISTENT
  ENDELSE

  ; Get the acquisition times from the header & retrieve acquisioition years
  getheader = ENVITask('RasterMetadataItem')
  getheader.INPUT_RASTER = in_vi
  getheader.KEY = 'time' &  getheader.execute
  times     = getheader.value
  years     = times.substring(0,3)
  prev_y    = where(years EQ opts.proc_year -1)

  ; Get the acquisition DOYS from the header
  getheader.KEY = 'Wavelength' &  getheader.execute
  doys_reg  = getheader.value

  ; Get number of bands and number of columns
  nb    = in_vi.NBANDS
  ncols = in_vi.NCOLUMNS
  nrows = in_vi.nrows

;- ------------------------------------------------------------------
; Add to the "opts" structure some recyclable variables so not to have to define / redefine 
; them later several times
;- ------------------------------------------------------------------

  addopts = {$; array indexes
    der_ind        : indgen (opts.derivs_opt [0]) + 1  ,$  ; Positions where derivatives should be checked for MAX identification
    maxdec_ind     : indgen (opts.decrease_win)   + 1  ,$ ; Positions where derivatives should be checked for VI decrease
    growth_ind     : indgen (opts.growth_opt [0]) + 1  ,$ ; positions to be checked to identify consistent growth after minimum
    vi_max_ind     : indgen (opts.max_aft_win[1]  - opts.max_aft_win[0]+1)+opts.max_aft_win[0], $ ; positions to be checked to identify if max is present in suitable period after min
    selquarts      : where  (opts.sel_seasons EQ 1) ,$
    n_sel_season   : total  (opts.sel_seasons) ,$
    pos_legit_maxs : intarr(nb) , $
    pos_quart_max  : intarr(nb) - 1, $
    check_arr_max  : [opts.derivs,opts.max_value, opts.decrease], $
    check_arr_min  : [opts.min_value,opts.growth, opts.flood, $  ; Array specisying which min criteria should be considered
    opts.max_after, opts.lst] $
  }

  opts = struct_addtags(opts,addopts)

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

; Compute number of required output bands: equal to number of
; selected outputs X n° of selected seasons + 1 for n_rice_seasons

  nb_out = opts.n_rice + n_elements(where(opts.SEL_SEASONS EQ 1)) * $
    (opts.max + opts.sow + opts.hh + opts.eos + $
    opts.int + opts.maxvi + opts.minvi + opts.maxmin + opts.eosmin)

  tot_lines = nrows

;- ----------------------------------------------------------------------------
;-  Start cycling on blocks of line to do the smoothing - parallel implementation following-
;-  http://daedalus.as.arizona.edu/wiki/ParallelIDL and
;-  http://www.iluvatar.org/~dwijn/parallelidl
;- ----------------------------------------------------------------------------

  IF (opts.method EQ "parallel-line") THEN BEGIN

    nCPUs = !CPU.hw_ncpu     ; Find number of available cores
    ranges = intarr(ncpus, 2)
    
    
    
    ; find the input lines to be assigned to each CPU (or each block if working in debug mode)
    FOR cpu = 0, nCPUs-1 DO ranges[cpu,*] = [tot_lines/nCPUs*cpu,tot_lines/nCPUs*(cpu+1)-1]   ; Divide work among CPUs
    ranges[ncpus-1,1] = tot_lines-1  ; Lat cpu gets the last lines

    ;- ----------------------------------------------------------------------------
    ;-  Initialize bridges - one for each cpu !
    ;- ----------------------------------------------------------------------------

    IF opts.debug EQ 0 THEN BEGIN
      print, "# --- Initializing CPU bridges - please wait ! ---"
      bridges = build_bridges(nCPUs)   ; Create bridges
    ENDIF

    print, "# --- Starting the processing ---"

    FOR cpu=0, nCPUs-1 DO BEGIN   ; cycle on availlable CPUs

      sel_lines = ranges[cpu, *]   ; Get lines to be assigned to the CPU
      lines     = INDGEN(sel_lines[1] - sel_lines[0] + 1)
      data_lc   = IN_LC.GETDATA(SUB_RECT=[0,sel_lines[0],NCOLS-1,sel_lines[1]])
      data_VI   = in_vi.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_DOY  = in_doy.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_lst  = in_lst.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_NDFI = in_NDFI.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      
      ; Initialize output matrix (so that we don't have to build it in each bridge
      empty_out_matrix = intarr(nb_out, ncols, n_elements(lines))-999   ; Otherwise initialize to NODATA

      ; If smoothed file was already existing get the data from the caller, otherwise set smooth_matrix
      ; to a dummy array set to NODATA

      IF (smooth_flag EQ 1) THEN BEGIN
        smooth_matrix = in_smooth.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
        data_QA = 0
      ENDIF ELSE BEGIN
        smooth_matrix = intarr(nb, ncols, n_elements(lines))+32767
        data_QA = in_quality.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      ENDELSE

      ; If debug not choosen, pass variables to bridges and run them 
      IF (opts.debug EQ 0 ) THEN BEGIN

;- ----------------------------------------------------------------------------
;-  Process data
;- ----------------------------------------------------------------------------
        bridge = get_idle_bridge(bridges)  ; Find an IDLE CPU

        ; Provide necessary variables to the CPU
        struct_pass   , opts, bridge  ; Pass the opts structure to the bridge
        bridge->SetVar, 'lines', lines
        bridge->SetVar, 'data_lc', data_lc
        bridge->SetVar, 'data_VI', data_VI
        bridge->SetVar, 'data_DOY', data_DOY
        bridge->SetVar, 'data_lst', data_lst
        bridge->SetVar, 'data_NDFI', data_NDFI
        bridge->SetVar, 'years', years
        bridge->SetVar, 'nb', nb
        bridge->SetVar, 'ncols', ncols
        bridge->SetVar, 'doys_reg', doys_reg
        bridge->SetVar, 'prev_y', prev_y
        bridge->SetVar, 'nb_out', nb_out
        bridge->SetVar, 'smooth_flag', smooth_flag
        bridge->SetVar, 'data_QA', data_QA
        bridge->SetVar, 'empty_out_matrix', empty_out_matrix
        bridge->SetVar, 'smooth_matrix', smooth_matrix

        ; Execute the processing function

        bridge->Execute, "temp_matrix = pr_process_v30_parline(opts, lines, data_lc, data_VI, data_QA,data_DOY, data_lst, data_NDFI, "$
          + "nb, ncols, doys_reg, years, prev_y, smooth_flag, nb_out, smooth_matrix, empty_out_matrix)", /nowait

      ENDIF ELSE BEGIN ; if debug, simply call processing function
        print, '# --- Debug mode - working on block: ' + (string(cpu+1)).trim() + ' of: ' + (string(nCPUs)).trim()
        temp_matrix = pr_process_v30_parline(opts, lines, data_lc, data_VI, data_QA,data_DOY, data_lst, data_NDFI, nb, $
          ncols, doys_reg, years, prev_y, smooth_flag, nb_out, smooth_matrix, empty_out_matrix)

        ; reconcile results in a single matrix
        IF (cpu EQ 0 ) THEN BEGIN
          out_matrix = temp_matrix
        ENDIF ELSE BEGIN
          out_matrix = [[[out_matrix]],[[temp_matrix]]]
        ENDELSE
      ENDELSE ; end else on debug mode
    ENDFOR ; End for on CPUs

    ; If not in debug mode, set a barrier, so that processing continues only after
    ; all CPUs finish their 100 lines chunk

    IF opts.debug EQ 0 THEN BEGIN
      barrier_bridges, bridges
      ;- ------------------------------------------------------------------
      ;- Reconcile results of the different CPUs on a single matrix
      ;- ------------------------------------------------------------------
      FOR i=0, n_elements(bridges)-1 DO BEGIN
        IF (i EQ 0 and chunk EQ 0) THEN BEGIN
          out_matrix = bridges[i]->getvar('temp_matrix')
        ENDIF ELSE BEGIN
          pippo = bridges[i]->getvar('temp_matrix')
          out_matrix = [[[out_matrix]],[[pippo]]]
        ENDELSE
      ENDFOR
    ENDIF
    print, '# --- Finished processing - Saving Outputs --- #'

  ENDIF ; End if on use of parallel processing - see if remove

;- ------------------------------------------------------------------
;- Save the resulting smoothed file if needed
;- ------------------------------------------------------------------

  IF smooth_flag EQ 0 THEN BEGIN

    smooth_matrix   = out_matrix[0:(nb-1),*,*]
    out_smooth_file = ENVIRaster(smooth_matrix, URI=in_files.smooth_file, interleave = 'bip', spatialref = in_vi.spatialref)
    out_smooth_file.metadata.AddItem, 'Wavelength', doys_reg
    out_smooth_file.metadata.AddItem, 'Band Names', "VI_Smoothed" + "_" + (string(doys_reg)).trim()
    out_smooth_file.metadata.AddItem, 'time', times
    out_smooth_file.metadata.AddItem, 'DATA_IGNORE_VALUE', 32767
    out_smooth_file.Save
    out_smooth_file.Close
    out_matrix      = out_matrix[nb:(nb + nb_out-1),*,*]

  ENDIF
  
;- ------------------------------------------------------------------
;- Save the Processed output file
;- ------------------------------------------------------------------

  ; make a hash table to pass from output codes to band names
  bandhash = HASH('N_RICE', 'N_Rice_Seasons', $
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
  tagnames_out = tagnames [40:49]
  out_ind = 0 

  ; Cycle on output bands and save only those selected by the user
  FOREACH tagout, tagnames_out, tagind DO BEGIN
    IF opts.(40+tagind) EQ 1 THEN BEGIN
      out_filename = in_files.out_filename + "_" + tagout + ".dat"
      IF tagout EQ "N_RICE" THEN BEGIN  ; Save n_reice band
        nb_save    = 1
        out_data   = out_matrix[0,*,*]
        data_type  = 1
        files_list = out_filename
      ENDIF ELSE BEGIN  ; save other bands
        nb_save    = opts.N_SEL_SEASON
        band_ind   = 1+[nb_save*(out_ind-1):(nb_save*(out_ind)-1)]
        out_data   = out_matrix[band_ind,*,*]
        data_type  = 2
        files_list = [files_list, out_filename]
      ENDELSE

      IF opts.ovr EQ 1 AND file_test (out_filename) EQ 1 THEN BEGIN
        file_delete, out_filename, /ALLOW_NONEXISTENT
        file_delete, out_filename.replace('.dat','.hdr') , /ALLOW_NONEXISTENT
      ENDIF ELSE BEGIN
        IF (file_test (out_filename) EQ 1 ) THEN $
          out_filename = out_filename.replace('.dat','_new.dat')
      ENDELSE

      ; Save the output file, adding required metadata

      out_raster   = ENVIRaster(URI=out_filename, NROWS=nrows, NCOLUMNS=ncols, $
        NBANDS=nb_save, data_type = data_type, interleave = 'bip', $
        SPATIALREF = in_vi.spatialref, DATA_IGNORE_VALUE = -999)
      out_raster.SetData,out_data
      bname = bandhash[tagout]
      ; Set some additional metadatas: band names ancd creation time
      IF tagout NE "N_RICE" THEN $
        out_raster.metadata.AddItem, 'Band Names', bname + " - Season " + (string(opts.selquarts)).trim() $
      ELSE $
        out_raster.metadata.AddItem, 'Band Names', bname
      time = string(BIN_DATE(systime()))
      time = string(time[0], format = "(I4.4)")+'-'+string(time[1], format = "(I2.2)")+'-'+string(time[2], format = "(I2.2)")
      IF (out_raster.metadata.HasTag ('time')) THEN out_raster.metadata.UpdateItem, 'time', time ELSE out_raster.metadata.AddItem,'time', time
      out_raster.Save
      out_raster.Close
      ; Increase counter used to get correct data from out_matrix
      out_ind = out_ind +1    
    ENDIF

  ENDFOREACH ; endforeach on possible output bands

;- ------------------------------------------------------------------
; If requested, also save a multiband file containing all outputs
;- ------------------------------------------------------------------
  ; Could change this to creating a META file !!!!
  IF (opts.fullout EQ 1) THEN BEGIN
    fullout_name    = in_files.OUT_FILENAME+'_fullout.dat'
    FOREACH ff, files_list, indfile DO BEGIN
      IF indfile EQ 0 THEN BEGIN
        raster_list = e.OpenRaster(ff)
      ENDIF ELSE BEGIN
        raster      = e.OpenRaster(ff)
        raster_list = [raster_list,raster]
      ENDELSE
    ENDFOREACH
    
    IF opts.ovr EQ 1 AND file_test (fullout_name) EQ 1 THEN BEGIN
      file_delete, fullout_name, /ALLOW_NONEXISTENT
      file_delete, fullout_name.replace('.dat','.hdr') , /ALLOW_NONEXISTENT
    ENDIF ELSE BEGIN
      IF (file_test(fullout_name) EQ 1 ) THEN $
        out_filename = fullout_name.replace('.dat','_new.dat')
    ENDELSE

    result                 = ENVIMetaspectralRaster(raster_list, spatialref = raster.spatialref)
    result.metadata.AddItem, 'time', time
    getheader              = ENVITask('RasterMetadataItem')
    getheader.INPUT_RASTER = result
    getheader.KEY          = 'Band Names' &  getheader.execute
    old_bnames             = getheader.value
    bnames                 = old_bnames.remove(0,7)
    result.metadata.UpdateItem, 'Band Names', bnames
    result.Export, fullout_name, 'envi', interleave = "bip", data_ignore_value = no_data

    ; cleanup
    result.Close
    FOREACH rr, raster_list DO rr.close
    
  ENDIF

;- ------------------------------------------------------------------
;- Final clean up
;- ------------------------------------------------------------------

  if opts.debug EQ 0 then burn_bridges, bridges ; Kill bridges 
  DataColl = e.Data
  DataItems = DataColl.Get()
  FOREACH item, DataItems DO item.Close
  e.Close
  heap_gc

  T2=systime(1)
  print, "# ############################################ #"
  print, "# --- Processing finished correctly ---        #"
  print, "# --- Processing time: " + (string(t2-t1)).trim() + ' --- #'
  print, "# ############################################ #"

  return, "DONE ! "

END

