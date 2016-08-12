
;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    in_files
;    proc_opts
;    opts.proc_year
;    mapscape
;
;
;
; :Author: lb
;
;-




FUNCTION pr_init_processing_v30_parline, in_files, opts

COMPILE_OPT idl2

  t1 = systime(1)

  e = ENVI(/HEADLESS)   ; initialize envi

  ;- ------------------------------------------------------------------
  ; Open required rasters and get information from header files -------
  ;- ------------------------------------------------------------------

  in_vi      = e.OpenRaster(in_files.EVI_file)
  in_quality = e.OpenRaster(in_files.quality_file)
  in_doy     = e.OpenRaster(in_files.doy_file)
  in_lc      = e.OpenRaster(in_files.lc_file)
  IF FILE_TEST(in_files.smooth_file) EQ 1 AND opts.force_resmooth EQ 0 THEN BEGIN
    in_smooth = e.OpenRaster(in_files.smooth_file)
    smooth_flag = 1     ; IF input taken from already existing file, set this flag to 1

  ENDIF ELSE BEGIN
    smooth_flag = 0
  ENDELSE

  getheader = ENVITask('RasterMetadataItem')
  getheader.INPUT_RASTER = in_vi

  ; Get the acquisition times from the header & retrieve acquisioition years
  getheader.KEY = 'time' &  getheader.execute
  times  = getheader.value
  years  = strmid(times,0,4)
  prev_y = where(years EQ opts.proc_year -1)

  ; Get the acquisition DOYS from the header
  getheader.KEY = 'Wavelength' &  getheader.execute
  doys_reg = getheader.value

  ; Get number of bands and number of columns
  nb = in_vi.NBANDS
  ncols = in_vi.NCOLUMNS

  ; Compute number of required output bands: equal to number of
  ; selected outputs X n° of selected seasons
  nb_out = n_elements(where(opts.SEL_SEASONS EQ 1)) * $
        (opts.n_rice +  opts.sow +  opts.flow +  opts.eos +  opts.integral) 

  ;- ------------------------------------------------------------------
  ;- Start Processing        ------------------------------------------
  ;- ------------------------------------------------------------------

  tot_lines = indgen(in_vi.nrows)
  print , '--- PhenoRice VI-smoothing - please wait ! ---'

  ;- ----------------------------------------------------------------------------
  ;-  Start cycling on line blocks to do the smoothing - parallel implementation -
  ;-  http://daedalus.as.arizona.edu/wiki/ParallelIDL
  ;- ----------------------------------------------------------------------------


  nCPUs = !CPU.HW_NCPU-1      ; Find number of available cores - set CPUs to N-1
  oBridge = objarr(nCPUs-1)

  FOR i=0, nCPUs-1 DO BEGIN   ; cycle on CPUs

    range = [n_elements(tot_lines)/nCPUs*i,n_elements(tot_lines)/nCPUs*(i+1)-1]   ; Divide work among CPUs
    IF i EQ n_elements(oBridge) THEN range[1] = n_elements(tot_lines)-1

    IF i EQ nCPUs-1 THEN BEGIN
      sel_lines = [range[0],range[1]]
      lines = indgen(sel_lines[1] - sel_lines[0] + 1)
      data_lc = in_lc.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_VI = in_vi.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_DOY = in_doy.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      IF (smooth_flag EQ 1) THEN BEGIN
        data_smooth = in_smooth.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
        data_QA = 0
      ENDIF ELSE BEGIN
        data_smooth = 0
        data_QA = in_quality.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      ENDELSE
      smooth_matrix = pr_smooth_v30_parline(opts, lines, data_lc, data_VI, data_QA,data_DOY, data_smooth, nb, ncols, doys_reg, years, prev_y, smooth_flag, nb_out)
      ; out_matrix = pr_process_v30_parline(opts, lines, data_lc, data_VI, data_smooth, $
      ;  data_DOY, nb, ncols, doys_reg, years, proc_year, prev_y)

    ENDIF ELSE BEGIN
      oBridge[i] = obj_new('IDL_IDLBridge')
      sel_lines = [range[0],range[1]]
      lines = indgen(sel_lines[1] - sel_lines[0] + 1)
      data_lc = IN_LC.GETDATA(SUB_RECT=[0,sel_lines[0],NCOLS-1,sel_lines[1]])
      data_VI = in_vi.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_DOY = in_doy.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      struct_pass, opts, oBridge[i]
      oBridge[i]->SetVar, 'lines', lines
      oBridge[i]->SetVar, 'data_lc', data_lc
      oBridge[i]->SetVar, 'data_VI', data_VI
      oBridge[i]->SetVar, 'data_DOY', data_DOY
      oBridge[i]->SetVar, 'years', years
      oBridge[i]->SetVar, 'nb', nb
      oBridge[i]->SetVar, 'ncols', ncols
      oBridge[i]->SetVar, 'doys_reg', doys_reg
      oBridge[i]->SetVar, 'prev_y', prev_y

      IF (smooth_flag EQ 1) THEN BEGIN
        data_smooth = in_smooth.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
        data_QA = 0
      ENDIF ELSE BEGIN
        data_smooth = 0
        data_QA = in_quality.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      ENDELSE
      
      ; provide necessary variables to the CPUs
      oBridge[i]->SetVar, 'data_QA', data_QA
      oBridge[i]->SetVar, 'data_smooth', data_smooth
      oBridge[i]->SetVar, 'nb_out', nb_out
      ; define the location of the processing function and executes it on the different bridges
;      oBridge[i]->Execute, ".r " + "!PATH = EXPAND_PATH('<IDL_DEFAULT>:+" + FILE_DIRNAME(ProgramRootDir())+PATH_SEP()
      oBridge[i]->Execute, ".r " + FILE_DIRNAME(ProgramRootDir())+PATH_SEP()+"Accessoires"+PATH_SEP()+"pr_smooth_v30_parline.pro"
      oBridge[i]->Execute, "data_smooth = pr_smooth_v30_parline(opts, lines, data_lc, data_VI, data_QA,data_DOY, data_smooth, nb, ncols, doys_reg, years, prev_y, smooth_flag, nb_out)", /nowait

      ;      oBridge[i]->Execute, ".r " + FILE_DIRNAME(ProgramRootDir())+PATH_SEP()+"Accessoires"+PATH_SEP()+"pr_smooth_v30_parline.pro"
      ;      oBridge[i]->Execute, "out_matrix = pr_smooth_v30_parline(opts, lines, data_lc, data_VI, data_QA,data_DOY, nb, ncols, doys_reg, years, prev_y, in_files)", /nowait

    ENDELSE
  ENDFOR

  ; Don't know what this means....

  notdone = 1
  WHILE notdone DO BEGIN
    done=0
    FOR i=0, n_elements(oBridge)-1 DO $
      done = done+oBridge[i]->Status()
    IF done EQ 0 THEN notdone=done
  ENDWHILE

  ;- ------------------------------------------------------------------
  ;- Reconcile results of the different cores
  ;- ------------------------------------------------------------------

  FOR i=0, n_elements(oBridge)-1 DO BEGIN
    
;    out_bridge_data = OBRIDGE[N_ELEMENTS(OBRIDGE)-1-i]->GETVAR('smooth_matrix')
    smooth_matrix = [[[OBRIDGE[N_ELEMENTS(OBRIDGE)-1-i]->GETVAR('smooth_matrix')]],[[smooth_matrix]]]
    ;out_matrix  = [[[OBRIDGE[N_ELEMENTS(OBRIDGE)-1-i]->GETVAR('out_matrix')]],[[out_matrix]]]
    obj_destroy, oBridge[n_elements(oBridge)-1-i]
  ENDFOR

  ;- ------------------------------------------------------------------
  ;- Save the resulting smoothed file
  ;- ------------------------------------------------------------------

  out_smooth_file = ENVIRaster(data_smooth, URI=in_files.smooth_file, interleave = 'bip')
  out_smooth_file.metadata.AddItem, 'Wavelength', doys_reg
  out_smooth_file.metadata.AddItem, 'Band Names', "VI_Smoothed" + "_" + strtrim(string(doys_reg),2)
  out_smooth_file.metadata.AddItem, 'time', times
  out_smooth_file.metadata.AddItem, 'DATA_IGNORE_VALUE', 32767
  out_smooth_file.Save

  ;- ------------------------------------------------------------------
  ;- Save the Procesed file
  ;- ------------------------------------------------------------------

  return, "DONE ! "
  T2=systime(1)
  ;    print,"start processing:", t1
  ;    print,"End processing:",  t2
  print, "Smoothing time: " + strtrim(string(t2-t1))
  print, "done!"

  ;- ------------------------------------------------------------------
  ;- Clean up
  ;- ------------------------------------------------------------------

  in_vi.Close
  in_quality.Close
  in_doy.Close
  in_lc.Close

  heap_gc

END

