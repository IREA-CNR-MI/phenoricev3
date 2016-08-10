
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
FUNCTION pr_smooth_v30_parline, in_files, proc_opts, proc_year, mapscape

  t1 = systime(1) & ddmm = strcompress(strmid(t1,4,6),/REMOVE_ALL) ; ddmm = date of processing run

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

  smooth_dummy = intarr(nb, ncols) + 32767

  lines = indgen(in_vi.nrows)
  print , 'PhenoRice VI-smoothing - pleas wait'

  ;- ----------------------------------------------------------------------------
  ;-  Start cycling on pixels to do the smoothing - parallel implementation -
  ;-  http://daedalus.as.arizona.edu/wiki/ParallelIDL
  ;- ----------------------------------------------------------------------------

  ;FOREACH pixel, lc_ok, ind_pixel DO BEGIN

  nCPUs = !CPU.HW_NCPU-1      ; Find number of available cores
  oBridge = objarr(nCPUs-1)
  ;      output = intarr(nb, n_elements(lc_ok))
  FOR i=0, nCPUs-1 DO BEGIN   ; cycle on CPUs

    range = [n_elements(lines)/nCPUs*i,n_elements(lines)/nCPUs*(i+1)-1]   ; Divide work among CPUs
    IF i EQ n_elements(oBridge) THEN range[1] = n_elements(lines)-1

    IF i EQ nCPUs-1 THEN BEGIN
      sel_lines = [range[0],range[1]]
      lines_ind = indgen(sel_lines[1] - sel_lines[0] + 1)
      data_lc = in_lc.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_VI = in_vi.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_QA = in_quality.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_DOY = in_doy.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])

      results = pr_smooth_pixel_v30_parline(lines_ind, data_lc, data_VI, data_QA, data_DOY, nb, ncols, proc_opts.win_dim_l, proc_opts.win_dim_r, $
        doys_reg, years, proc_year, prev_y, mapscape)

    ENDIF ELSE BEGIN
      sel_lines = [range[0],range[1]]
      lines_ind = indgen(sel_lines[1] - sel_lines[0] + 1)
      data_lc = IN_LC.GETDATA(SUB_RECT=[0,sel_lines[0],NCOLS-1,sel_lines[1]])
      data_VI = in_vi.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_QA = in_quality.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])
      data_DOY = in_doy.GetData(SUB_RECT=[0,sel_lines[0],ncols-1,sel_lines[1]])

      oBridge[i] = obj_new('IDL_IDLBridge')

      ; provide necessary variables to the CPUs
      oBridge[i]->SetVar, 'inlines', lines_ind
      oBridge[i]->SetVar, 'data_lc', data_lc
      oBridge[i]->SetVar, 'data_VI', data_VI
      oBridge[i]->SetVar, 'data_QA', data_QA
      oBridge[i]->SetVar, 'data_DOY', data_DOY
      oBridge[i]->SetVar, 'years', years
      oBridge[i]->SetVar, 'proc_year', proc_year
      oBridge[i]->SetVar, 'nb', nb
      oBridge[i]->SetVar, 'ncols', ncols
      oBridge[i]->SetVar, 'win_dim_l', proc_opts.win_dim_l
      oBridge[i]->SetVar, 'win_dim_r', proc_opts.win_dim_r
      oBridge[i]->SetVar, 'doys_reg', doys_reg
      oBridge[i]->SetVar, 'proc_year', proc_year
      oBridge[i]->SetVar, 'prev_y', prev_y
      oBridge[i]->SetVar, 'mapscape', mapscape

      ; define the location of the smoothing function
      oBridge[i]->Execute, ".r " + FILE_DIRNAME(ProgramRootDir())+PATH_SEP()+"Accessoires"+PATH_SEP()+"pr_smooth_pixel_v30_parline.pro"
      oBridge[i]->Execute, "results = pr_smooth_pixel_v30_parline(inlines, data_lc, data_VI, data_QA," $
        + "data_DOY, nb, ncols, win_dim_l, win_dim_r, doys_reg, years, proc_year, prev_y, mapscape)", /nowait
        
;        lines_ind, data_lc, data_VI, data_QA, data_DOY, nb, ncols, proc_opts.win_dim_l, proc_opts.win_dim_r, $
;        doys_reg, years, proc_year, prev_y, mapscape
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
    results = [[[OBRIDGE[N_ELEMENTS(OBRIDGE)-1-I]->GETVAR('results')]],[[results]]]
    obj_destroy, oBridge[n_elements(oBridge)-1-i]
  ENDFOR

  ;return, results

;  smooth_line[*,lc_ok] = results
;  out_smooth_file.SetData, results ; Put the output in the smoothed file line
;  ;      smooth_line [*,pixel] = pr_smooth_pixel_v30_parpix(data_VI, data_QA,data_DOY, nb, proc_opts.win_dim_l, proc_opts.win_dim_r, doys_reg, check,  mapscape)  ; Launch smoothing
;
;  ;ENDFOREACH  ; end cycle on pixels not masked out by lc
;
;  ;ENDIF ; endif on at least one pixel not masked out
;
;  out_smooth_file.SetData, transpose(smooth_line), SUB_RECT=tileIterator.current_subrect ; Put the output in the smoothed file line

  ;ENDFOREACH ; End of tiling

  out_smooth_file.metadata.AddItem, 'Wavelength', doys_reg
  out_smooth_file.metadata.AddItem, 'Band Names', "VI_Smoothed" + "_" + strtrim(string(doys_reg),2)
  out_smooth_file.metadata.AddItem, 'time', times
  out_smooth_file.Save

  return, "DONE ! "
  T2=systime(1)
  ;    print,"start processing:", t1
  ;    print,"End processing:",  t2
  print, "Smoothing time: " + strtrim(string(t2-t1))
  print, "done!"
  heap_gc

END

