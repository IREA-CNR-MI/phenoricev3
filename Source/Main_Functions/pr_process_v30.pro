;
; Title pr_process_v21
;:Description
;
;Details:

; :Params:
;    in_files
;    outrast_folder
;    pr_opts
;    max_criteria
;    min_criteria
;    out_logfile
;    proc_year
;    note
;
;
;
;:RETURNS:
;
;:AUTHOR: Lorenzo Busetto, phD (2014)
;  #' email: busetto.l@@irea.cnr.it
;
;License GPL(>2)
;-
function pr_process_v30, in_files , out_filename , pr_opts , $
  avg_criteria,max_criteria, min_criteria, out_logfile, proc_year,  note, resize, $
  start_x, end_x,start_y, end_y, MAPSCAPE

  add_outfiles = 0
  t1 = systime(2)

  pr_build_log_v22,out_filename,out_logfile, pr_opts,out_file_prefix,note,in_files,max_criteria,min_criteria  ; Create log file

  ; Retrieve options from pr_opts: n° of images in previous year and 4 quarters, width of smoothing
  ;-----
  nb_prev = pr_opts.nb_prev  &  nb_1q = pr_opts.nb_1q &  nb_2q = pr_opts.nb_2q
  nb_3q = pr_opts.nb_3q      &  nb_4q = pr_opts.nb_4q &  win_dim_l=pr_opts.win_dim_l
  win_dim_r=pr_opts.win_dim_r

  selquarts = where(pr_opts.SEL_SEASONS EQ 1)

  if (resize EQ 1) then begin
    dims_res = [-1, start_x, end_x, start_y, end_y]
    ; ENVI_DOIT, 'CONVERT_INPLACE_DOIT',dims = dims_res, fid = r_fid, o_interleave = 2, pos = indgen(70)
    in_file_lc_temp= file_dirname(in_files.lc)+path_sep()+'Resize_'+file_basename(in_files.lc)

    envi_open_file , in_files.lc  , r_fid=lc_fid, /no_realize
    ENVI_DOIT, 'RESIZE_DOIT', DIMS=dims_res, FID=lc_fid, OUT_NAME= in_file_lc_temp, POS=0,rfact = [1,1], r_fid = r_fid
    ENVI_FILE_MNG, ID=lc_fid, /REMOVE
    in_files.lc = in_file_lc_temp
    lc_fid = r_fid

  endif

  ;- --------------------------------------------------------- ;
  ;-  Open Input files
  ;- --------------------------------------------------------- ;

  envi_open_file , in_files.ndvi, r_fid=ndvi_fid, /no_realize
  envi_open_file , in_files.ndii, r_fid=ndii_fid, /no_realize
  envi_open_file , in_files.ndfi, r_fid=ndfi_fid, /no_realize
  envi_open_file , in_files.qa, r_fid=QA_fid, /no_realize
  envi_open_file , in_files.lc  , r_fid=lc_fid, /no_realize
  envi_open_file , in_files.lst, r_fid=lst_fid, /no_realize
  envi_open_file , in_files.doy, r_fid=doy_fid, /no_realize
  envi_file_query, ndvi_fid, dims=dims, nb=nb, ns=ns, nl=nl, ystart=ystart,   $
    bnames=bnames, data_type=data_type, xstart=xstart, wl = wl
  map_info = envi_get_map_info(fid =  ndvi_fid)

  ;- --------------------------------------------------------- ;
  ;-  Create and Open in writing the output files
  ;- --------------------------------------------------------- ;
  ;  outfiles_codes = ['VI_smooth','map_rice','map_min','map_SOS','map_max','map_EoS', $
  ;    'smooth_der','ricemax_array','ricemin_array', $
  ;    'max_VI','min_VI','SoS_min_delta','max_min_delta','EoS_min_delta','Full_Out']

  out_filesmooth = FILE_DIRNAME(in_files.ndvi) + PATH_SEP()+file_basename(out_filename)+'_VI_smooth_'+strtrim(string(proc_year),2)+'.dat'
  out_folder = FILE_DIRNAME(out_filename)+path_sep()+strtrim(string(proc_year),2)
  FILE_MKDIR,out_folder
  
  ; At each year, modify the "template" filename to add the "year" suffix
  out_file = out_folder + path_sep() + FILE_BASENAME(out_filename) +'_'+strtrim(string(proc_year),2)+'.dat'
  openw, Output_id , out_file, /GET_LUN

  if add_outfiles EQ 1 then begin
    outfile_maxarray = out_file +strtrim(string(proc_year),2)+ 'maxarray.dat'
    openw, ricemax_array_id , outfile_maxarray , /GET_LUN

    outfile_minarray = out_file +strtrim(string(proc_year),2)+ 'minarray.dat'
    openw, ricemin_array_id, outfile_minarray, /GET_LUN
  endif

  

;  outfiles_dtypes = [1,1,2]       ; Data Types for outputs

  ;  out_files = outrast_folder + path_sep() + out_file_prefix + '_'+outfiles_codes+'_'+strtrim(string(proc_year),2)+'.dat'  ; Build file name

  ;  out_files[3] = outrast_folder + path_sep() + out_file_prefix + '_'+outfiles_codes[3]+'_'+strtrim(string(proc_year),2)+'new.dat'  ; Build file name

  ; Used to automatically create and opn all output files, without the need of cycling on names .
  ;  temp_hash = hash(outfiles_codes,out_files)    &   out_files_str = temp_hash.tostruct()
  ;  tags = tag_names(out_files_str)
  ;  outfile_ids = tags+'_id'
  ;
  ;  for ff = 0, n_elements(tags)-1 do begin   ; Start for cycle on out_files (except smooth) and open them in writing
  ;    file_mkdir, file_dirname(outfile_ids[ff])
  ;    openw,  scope_varfetch(outfile_ids[ff],/ENTER) , out_files_str.(ff)  , /get_lun
  ;  endfor       ; End for cycle on out_f

  ;- --------------------------------------------------------- ;
  ;-  Initialize tiling on input files
  ;- --------------------------------------------------------- ;
  pos = indgen(nb)
  tile_ndvi = envi_init_tile(ndvi_fid, pos , interleave=2, num_tiles=num_tiles,   $
    xs=dims[1], xe=dims[2], ys=dims[3], ye=dims[4])                               ;vi
  tile_ndii = envi_init_tile(ndii_fid, pos, interleave=2, match_id=tile_ndvi)     ;NDII
  tile_ndfi = envi_init_tile(ndfi_fid, pos, interleave=2, match_id=tile_ndvi)     ;NDFI
  tile_QA = envi_init_tile(QA_fid, pos, interleave=2, match_id=tile_ndvi)         ;QA
  tile_lc = envi_init_tile(lc_fid, 0, interleave=2, match_id=tile_ndvi)           ;lc-band
  ;  tile_season = envi_init_tile(season_fid, 0, interleave=2, match_id=tile_ndvi);season
  tile_lst = envi_init_tile(lst_fid, pos, interleave=2, match_id=tile_ndvi)       ;LST
  tile_doy = envi_init_tile(doy_fid, pos, interleave=2, match_id=tile_ndvi)       ;LST

  ;- --------------------------------------------------------- ;
  ; Initialize "reused" variables before the FOR loop !!!! avoid doing same thing on each cycle !!!
  ;- --------------------------------------------------------- ;
  ;
  ; Criteria on maximums - set indexes for the windows of the different checks and the threshold
  der_ind = indgen (max_criteria.derivs_opt [0])+1  ; Positions where derivatives should be checked for MAX identification
  maxdec_wid = max_criteria.decrease_win ; Positions where derivatives should be checked for VI decrease
  maxdec_ind = indgen (max_criteria.decrease_win)+1 ; Positions where derivatives should be checked for VI decrease
  maxthresh = max_criteria.vi_tr_max    ; Threshold for maximum
  maxdec_perc = max_criteria.decrease_perc    ; % of max decrease

  check_arr_max = [max_criteria.derivs,max_criteria.max_value, max_criteria.decrease]  ; Array specisying which max  criteria should be considered

  ; Criteria on minimums - set indexes for the windows of the different checks and the threshold
  ;- ---------------
  minthresh = min_criteria.vi_tr_min
  growth_ind = indgen (min_criteria.growth_opt [0])+1 ; positions to be checked to identify consistent growth after minimum
  growth_wid = min_criteria.growth_opt [0]  ; width of window  to be checked to identify consistent growth after minimum
  growth_thresh = min_criteria.growth_opt [1] ; number of positive der. after minimum needed for min. to be legit
  vi_flood_wid = min_criteria.flood_win           ; dimension of win to be checked for flood
  vi_flood_ind = indgen (min_criteria.flood_win )-min_criteria.flood_win/2     ; positions to be checked to identify flooding around minimum
  check_NDFI = min_criteria.check_NDFI
  check_NDII = min_criteria.check_NDII
  vi_max_ind = indgen(min_criteria.max_aft_win[1] - min_criteria.max_aft_win[0]+1)+min_criteria.max_aft_win[0] ; positions to be checked to identify if max is present in suitable period after min
  LST_thresh = min_criteria.lst_thresh

  check_arr_min = [min_criteria.min_value,min_criteria.growth, min_criteria.flood, $  ; Array specisying which min criteria should be considered
    min_criteria.max_after, min_criteria.lst]

  ; band positions for the differnt "quarters" for max positioning within quarters
  temp_pos_quarters_max = intarr(nb)-1

  ; Retrieve the acquisition DOYS from the input band names
  split_bnames = strsplit(bnames, '_', /extract)    ; split the bnames and convert to array
  split_bnames = split_bnames.toarray()
  size_split = size(split_bnames)
  years = split_bnames[*,size_split[2]-2]                        ; get the years

  doys_reg = fix(strmid(split_bnames[*,size_split[2]-1],0,3))          ; get the doys

  if (min(years[uniq(years)]) LT proc_year) then begin    ; If some bands of previous year required, compute their doy by subtracting 365
    doys_reg [where(years EQ proc_year -1)] = doys_reg [where(years EQ proc_year -1)] - 365
  endif

  if (max(years[uniq(years)]) GT proc_year) then begin    ; If some bands of next year required, compute their doy by adding 365
    doys_reg [where(years EQ proc_year +1)] = doys_reg [where(years EQ proc_year +1)] + 365
  endif

  pos_quart_max = intarr(nb)-1  ; Initialize array  with -1
  n_sel_season = total(pr_opts.sel_seasons)
  pos_legit_maxs = intarr(nb)
  if (pr_opts.sel_seasons[0] EQ 1) then pos_quart_max [where (doys_reg ge pr_opts.doy_1q[0]-7 AND  doys_reg le pr_opts.doy_1q[1])]  = 0  ; set the "season" according to doy
  if (pr_opts.sel_seasons[1] EQ 1) then pos_quart_max [where (doys_reg ge pr_opts.doy_2q[0]-7 AND  doys_reg le pr_opts.doy_2q[1])]  = 1
  if (pr_opts.sel_seasons[2] EQ 1) then pos_quart_max [where (doys_reg ge pr_opts.doy_3q[0]-7 AND  doys_reg le pr_opts.doy_3q[1])]  = 2
  if (pr_opts.sel_seasons[3] EQ 1) then pos_quart_max [where (doys_reg ge pr_opts.doy_4q[0]-7 AND  doys_reg le pr_opts.doy_4q[1])]  = 3
  ;
  ;
  pos_legit_maxs = pos_quart_max NE -1
  doys_reg = float(doys_reg +8)                        ; Move the "reference" doy for each composition period from day 1 to day 8

  ;_______________________________________________________________________________
  ; --- here we check if the smoothed vi file already exists. If not, it is created
  ;----------------------------------------------------------------------

  ; Check if the "smoothed VI" file already exists. If Yes, do nothing. Otherwise, perform the smoothing

  smooth_chk  = file_search(out_filesmooth)                ; Chiave per il check

  ; ---------------------------------------------------------------------------
  ; --- Start of smoothing --  if smoothed file doesn't exist, it is created
  ;---------------------------------------------------------------------------
  if smooth_chk eq '' then begin
    openw, VI_smooth_id, out_filesmooth, /get_lun

    for line=0, num_tiles-1 do begin              ; Smooth VI data tile after tile

      print , 'PhenoRice VI-smoothing, line', line+1, '  on:', num_tiles
      out_SMOOTH = intarr(nb,ns)      ; Initialize smoothed line array
      ;--- Extract LC data of each tile:
      data_LC = envi_get_tile(tile_lc, line, band_index=nb)         ; Get data from LC mask
      lc_mask = where(data_LC eq 1, count_lc_ok, complement = lc_bad)   ; Find Pixels in the line with "good" lc values

      if count_lc_ok gt 0 then begin ; check on "land cover" mask. If no pixel in the line are "good", skip the line, otherwise, perform smooth

        ; Get data
        data_VI   = float(envi_get_tile(tile_ndvi, line, band_index=nb))   ; Get Vi line
        data_QA   = envi_get_tile(tile_QA  , line, band_index=nb)   ; Get QA line
        data_DOY  = float(envi_get_tile(tile_doy , line, band_index=nb))   ; Get DOY line
        
        if MAPSCAPE EQ 1 then data_DOY[data_DOY EQ -1] = 32767
        
        ; ---------------------------
        ; Reshuffle DOYS in case more than one year is on the required time serie + check and replace NODATA on DOYS
        ; ---------------------------
        if (min(years[uniq(years)]) LT proc_year) then begin    ; If some bands of previous year required, compute their doy by subtracting 365
          prev_y =  where(years EQ proc_year -1)
          prev_y_data = data_DOY [prev_y , * ]
          prev_y_data[prev_y_data gt 12] = prev_y_data[prev_y_data gt 12] - 365
          data_DOY [prev_y, * ] = prev_y_data
        endif

        if (max(years[uniq(years)]) GT proc_year) then begin    ; If some bands of next year required, compute their doy by adding 365
          data_DOY [where(years EQ proc_year +1),*] = data_DOY [where(years EQ proc_year +1),*] + 365
        endif

        for lc_ok_index=0, count_lc_ok-1 do begin ; Cycle on "good" pixels in the line (lc = 1)
          lc_ok   = lc_mask[lc_ok_index]  ; position of the pixel in the line
          vi_pix  = data_VI [*,lc_ok]     ; Retrieve vi, qa and doy for the pixel
          qa_pix  = data_QA [*,lc_ok]
          doy_pix = data_DOY[*,lc_ok]

          ;  Substitute doys of where doy is nodata with doys_reg
          BAD_DOY = where(abs(DOY_PIX ) gt 1000, count_bad_doy)    ; Check on doys: doys > 400 = NODATA in DOY image --> set them to the doy of the composite 
;          if MAPSCAPE EQ 1 then BAD_DOY = where(DOY_PIX EQ -1, count_bad_doy)    ; Check on doys: doys > 400 = NODATA in DOY image --> set them to the doy of the composite !
          
          if (count_bad_doy ne 0 ) then begin
            DOY_PIX[bad_doy] = doys_reg[bad_doy]
          endif

          ; Check for problems on DOYS: if difference between doy_pix and doys_reg LT -16 = problem.
          ; Add 365 to the DOY
          diff = doy_pix-doys_reg
          bad_diff = where(diff LT -16, count_baddiff)
          if (count_baddiff GT 0 ) then begin
            doy_pix[bad_diff] = doy_pix[bad_diff]+365
          endif

          ; Final check on doys. If still problems stop (Probably could be removed)
          diff =max(abs(doy_pix-doys_reg))
          if (diff gt 16) then begin
            stop
          endif

          nodata_pos = where(vi_pix eq 32767, count_NA)  ; Check on vi values: vi = 32767 = NODATA . If more than 10 NODATA, skip the pixel (it stays at 0 !)
          if MAPSCAPE EQ 1 then nodata_pos = where(vi_pix eq -3000, count_NA)  ; Check on vi values: vi = -3000 = NODATA . If more than 10 NODATA, skip the pixel (it stays at 0 !)
          if count_NA lt 10 then begin          ; otherwise, perform smoothing
            smooth_pix    = pr_smooth_v22(vi_pix, qa_pix,doy_pix, nb, win_dim_l, win_dim_r, doys_reg, check = 0)  ; Launch smoothing
            CATCH, Error_status

            ; Error handler on smoothing: Stop if error
            IF Error_status NE 0 THEN BEGIN
              PRINT, 'Error in Smoothing ! - line '+string(line)+'pix  ', string(lc_ok)
              ; Handle the error by extending A:
              stop
              smooth_pix  = intarr(nb)+32767
              

            ENDIF
            out_SMOOTH[*,lc_ok] = smooth_pix    ; put results of smoothing in the output file

          endif    ;endif count_NA LT 10
        endfor     ;endif lc_ok_index
      endif     ;endif count_lc_ok
      writeu, VI_smooth_id, out_SMOOTH   ; Write the new line of smoothed file (if line skipped, write an array of zeroes (should be changed to NODATA value)
    endfor        ;endfor on lines

    ; Write the header and open the smoothed file
    free_lun, VI_smooth_id
    envi_setup_head, fname=out_filesmooth , ns=ns, nl=nl, nb=nb,                        $
      data_type= 2, offset=0, interleave=2, MAP_INFO=map_info, bnames=bnames,      $
      wl = doys_reg, $
      xstart=xstart+dims[1], ystart=ystart+dims[3], descrip='Phenorice - Smoothed VI', /write

    envi_open_file, out_filesmooth, r_fid=smooth_fid, /no_realize
  endif else begin   ; Endif on non-existance of smoothed file
    
    ; If smoothed file already exists, just open the already existing smoothed VI file

    envi_open_file, smooth_chk[0], r_fid=smooth_fid, /no_realize            
  endelse
  
; initialize tiling on smoothed file
  tile_smooth = envi_init_tile(smooth_fid, pos, interleave=2, match_id=tile_ndvi)   

  ;- --------------------------------------------------------- ;
  ;-  Start PHENORICE processing
  ;- --------------------------------------------------------- ;

  ;- --------------------------------------------------------- ;
  ;-  Start cyclying on tiles of input files (== lines)
  ;- --------------------------------------------------------- ;

  for line=0, num_tiles-1 do begin                               ;Start loop on file lines

    print , 'Processing Line: '+ strtrim(line,2) + 'of : ' + strtrim(num_tiles,2)
    ;- --------------------------------------------------------- ;
    ;-  Initialize output arrays
    ;- --------------------------------------------------------- ;
    ; DOYS and VI values for quarters with identifiede rice seasons

    out_MAPRICE      = bytarr(1,ns)                 ; Number of identified seasons
    out_map_mindoy   = intarr(n_sel_season,ns)              ; MAPPA DEI MINIMI --> DOYs of identified minimums. Set to O if no min (should change to NODATA)
    out_MAP_MAXDOY   = intarr(n_sel_season,ns)              ; MAPPA DEI MASSIMI --> DOYs of identified minimums. Set to O if no min
    out_max_VI    = intarr(n_sel_season,ns)                  ; value EVI in rice MIN
    out_min_VI    = intarr(n_sel_season,ns)                  ; value val EVI in rice max
    out_MAP_VItot= intarr(n_sel_season,ns)
    ; SoS and Eos
    out_MAP_SOS      = intarr(n_sel_season,ns)                 ; sos dates
    out_MAP_EOS      = intarr(n_sel_season,ns)                 ; eos dates
    out_MAP_Half      = intarr(n_sel_season,ns)                ; 50 % value dates
    out_MAP_Flow      = intarr(n_sel_season,ns)                ; 90 % value dates
    out_MAP_Maxinc      = intarr(n_sel_season,ns)              ; Max slope dates
    out_MAP_HalfHead      = intarr(n_sel_season,ns)              ; Max slope dates
    out_MAP_LastFlood= intarr(n_sel_season,ns)              ; Max slope dates
    out_RICE_MAX_ARRAY      = bytarr(nb,ns)         ; vettore RICE-MAX (useful for checks - useless for users)
    out_RICE_MIN_ARRAY      = intarr(nb,ns)         ; vettore RICE-MIN (useful for checks - useless for users)

    ; Phenological metrics
    out_SOS_MIN_DELTA     = intarr(n_sel_season,ns)            ; metrica distanza tra rice MIN e rice SoS - in n° of days
    out_SOS_MAX_DELTA     = intarr(n_sel_season,ns)
    out_EOS_MIN_DELTA     = intarr(n_sel_season,ns)            ; metrica distanza tra rice MIN e rice EoS - in n° of days
    out_MAX_MIN_DELTA     = intarr(n_sel_season,ns)            ; metrica distanza tra rice MIN e rice MAX - in n° of days

      ;---------------------- Extract data for LC for each tile:
    data_LC   = envi_get_tile(tile_lc,  line, band_index=nb)
    lc_mask = where(data_LC eq 1, count_lc_ok, complement = lc_bad)
    
    

    if count_lc_ok gt 0 then begin    ;  Check If pixel has to be analyzed on the basis of LC mask 
      
      ;------------------------- Extract input data for the line:
      ;      envi_open_file, smoot_str, r_fid=smooth_fid, /no_realize
      data_smooth = envi_get_tile(tile_smooth, line, band_index=nb)
      data_NDII   = envi_get_tile(tile_ndii  , line, band_index=nb)
      data_NDFI   = envi_get_tile(tile_ndfi  , line, band_index=nb)
      data_LST    = 0.02*envi_get_tile(tile_LST,  line, band_index=nb)-273.15     ; Compute temperature from LST data
      data_DOY    = envi_get_tile(tile_doy,  line, band_index=nb)
      der_neg_pix = bytarr(nb)    ; Reset derivatives arrays to 0 for each line
      der_pos_pix = bytarr(nb)
      
      ; ---------------------------
      ; Reshuffle DOYS in case more than one year is on the required time serie + check and replace NODATA on DOYS
      ; ---------------------------
      if (min(years[uniq(years)]) LT proc_year) then begin    ; If some bands of previous year required, compute their doy by subtracting 365
        prev_y =  where(years EQ proc_year -1)
        prev_y_data = data_DOY [prev_y , * ]
        prev_y_data[prev_y_data gt 12] = prev_y_data[prev_y_data gt 12] - 365
        data_DOY [prev_y, * ] = prev_y_data
      endif

      if (max(years[uniq(years)]) GT proc_year) then begin    ; If some bands of next year required, compute their doy by adding 365
        data_DOY [where(years EQ proc_year +1),*] = data_DOY [where(years EQ proc_year +1),*] + 365
      endif

      for lc_ok_index=0, count_lc_ok-1 do begin      ; loop su land cover a riso
        ;------------------------- Get input arrays for analyzed pixel (lc_ok position:
        lc_ok      = lc_mask[lc_ok_index]
        smooth_pix = data_smooth[*,lc_ok]
        lst_pix    = data_LST[*,lc_ok]
        NDII_pix   = data_NDII[*,lc_ok]
        NDFI_pix   = data_NDFI[*,lc_ok]
        doy_pix    = data_DOY[*,lc_ok]

        ; Create vector of "regular" doys. Needed to replace "bad doys" with regular ones ! This is needed in the check for flooding !

;        ; TBD: Move the check OUTSIDE the cycle on pixels !!!!!
;        if (min(years[uniq(years)]) LT proc_year) then begin    ; If some bands of previous year required, compute their doy by subtracting 365
;          doy_pix [where(years EQ proc_year -1 AND doy_pix gt 12) ] = doy_pix [where(years EQ proc_year -1 AND doy_pix gt 12 )] - 365
;        endif
;
;        if (max(years[uniq(years)]) GT proc_year) then begin    ; If some bands of next year required, compute their doy by adding 365
;          doy_pix [where(years EQ proc_year +1)] = doy_pix [where(years EQ proc_year +1)] + 365
;        endif

        BAD_DOY = where(DOY_PIX gt 1000, count_bad_doy)    ; Check on doys: doys > 400 = NODATA in DOY image --> set them to the doy of the composite !
        if MAPSCAPE EQ 1 then BAD_DOY = where(DOY_PIX EQ -1, count_bad_doy)    ; Check on doys: doys > 400 = NODATA in DOY image --> set them to the doy of the composite !
        if (count_bad_doy ne 0 ) then begin
          DOY_PIX[bad_doy] = doys_reg[bad_doy]
        endif

        diff = doy_pix-doys_reg
        bad_diff = where (diff LT -16, count_baddiff)
        if (count_baddiff GT 0 ) then begin
          doy_pix[bad_diff] = doy_pix[bad_diff]+365
        endif

        nodata_pos = where(smooth_pix eq 32767, count_NA, complement=good_data_pos) ;-> probably removable !
        if MAPSCAPE EQ 1 then  nodata_pos = where(smooth_pix eq -3000, count_NA, complement=good_data_pos)
        if count_NA lt 10 then begin   ;-> probably removable !

          smooth_pix     = data_smooth[*,lc_ok]     ; get the smoothed vi time serie

          if (avg_criteria.avg_check EQ 1) then begin   ; If check on average was selected, then compute the average,
            avg = mean(smooth_pix [good_data_pos] )      ; otherwise set the avg to -999 so that check is always passed
          endif else begin
            avg = -999
          endelse

          if (avg lt avg_criteria.avg_thresh) then begin    ; Remove pixels if average of VI above selected threshold (~ "rough" forest mask)

           if min_criteria.LST EQ 1 then  lst_pix_smooth = pr_smooth_lst_v22(lst_pix,nb)   ; Perform a basic smoothiing of LST values (removes spikes and NODATA)

            ; Identify all local maxima and minima + compute array of VI derivatives

            pr_signal_analysis_v22, smooth_pix, NDII_pix, NDFI_pix,nb, lc_ok, $  ;
              out_RICE_MAX_ARRAY, out_RICE_MIN_ARRAY, $
              der_neg_pix, der_pos_pix, win_dim_l, win_dim_r

            rice_max_arr_pix = out_RICE_MAX_ARRAY[*,lc_ok]    ; put the retrieved max in max array
            rice_min_arr_pix = out_RICE_MIN_ARRAY[*,lc_ok]    ; put the retrieved mins in min array


            ; Remove maximums corresponding to non-selected seasons
            rice_max_arr_pix = rice_max_arr_pix * pos_legit_maxs

            ; Check if identified maxs are "legit" according to the selected maximums criteria
            ; maxs not satisfying the criteria in rice_max_arr_pix are set to a value representing which criteria failed

            check_maxs = pr_crop_stage_max_v22(smooth_pix, rice_max_arr_pix, der_neg_pix, der_pos_pix,nb, pr_opts, $
              der_ind,maxdec_wid, maxdec_ind, maxthresh, maxdec_perc, check_arr_max, win_dim_l)

            if check_maxs eq 0 then begin ; Start if cycle - skips all further processing if all maxs already removed

              ;- --------------------------------------------------------- ;
              ; Check if identified mins are "legit" according to the selected maximums criteria
              ; mins not satisfying the criteria in rice_min_arr_pix are set to a value representing which criteria failed
              ;- --------------------------------------------------------- ;
              check_mins = pr_crop_stage_min_v22(smooth_pix , rice_min_arr_pix, rice_max_arr_pix, NDII_pix, NDFI_pix, LST_pix,     $
                der_neg_pix, der_pos_pix, nb, minthresh,growth_ind, growth_wid, growth_thresh, $
                vi_flood_ind,vi_flood_wid, check_NDFI,check_NDII, vi_max_ind, LST_thresh, check_arr_min, win_dim_l, doys_reg,doy_pix )

              ;- --------------------------------------------------------- ;
              ;-  Write in four quarters the MAP of rice MAX DOYS
              ;- --------------------------------------------------------- ;
              temp_maxdoy = intarr(n_sel_season)
              val_maxs =  smooth_pix * (rice_max_arr_pix eq 1)  ; Array with values of recognized maxs - 0 elsewhere

              FOREACH season, selquarts, ind_season DO BEGIN    ; Cycle on selected quarters to set the doy of maxs recognized in the different quarters
                val_max_quart =  val_maxs * (pos_quart_max eq season)  ;   Create array with values of recognized maxs in selected quarter - 0 elsewhere
                val_max_ass = max(val_max_quart, pos_max_ass)  ;   Find absolute max in selected quarter and its position
                IF (val_max_ass NE 0 ) THEN BEGIN
                  temp_maxdoy[ind_season] = pos_max_ass   ; If a max availble, put its position in temp_max_map (if no max, temp_max_map of the quarter stays at 0)
                  rice_max_arr_pix[pos_max_ass] = 5  ; Change the values of "rice" maxs (i.e., absolute maxs in quarters) from 1 to 5 in the output rice max array
                ENDIF
              ENDFOREACH

              tempmax_ok = where (temp_maxdoy ne 0, count)
              if (count ne 0 ) then out_MAP_MAXDOY [tempmax_ok,lc_ok] = doys_reg[temp_maxdoy[tempmax_ok]]   ; Set the values for the output map of doys of rice maxs

              ;- --------------------------------------------------------- ;
              ;- Write in four quarters the MAP of rice minimums DOYS
              ;- --------------------------------------------------------- ;

              temp_mindoy=intarr(n_sel_season)

              FOREACH season, selquarts, ind_season DO BEGIN
                pos_minq_ok = intarr(nb)
                temp_maxdoy_quart = temp_maxdoy[ind_season]
                IF temp_maxdoy_quart  GT 0 THEN BEGIN
                  pos_minq_ok [temp_maxdoy_quart-min_criteria.max_aft_win[1]>0 : temp_maxdoy_quart-min_criteria.max_aft_win[0]] = 1 ;
                  check_arr = pos_minq_ok*rice_min_arr_pix
                  pos_min_quart=where((check_arr EQ 5) OR (check_arr EQ 1), count)  ; find positions of minimums between -16 and -6 composites of absolute max
                  IF count GT 0 THEN BEGIN
                    temp_mindoy[ind_season]= max(pos_min_quart)
                    rice_min_arr_pix[max(pos_min_quart)] = 5 ; Change the values of "rice" mins (i.e., mins nearest to a suitable max) from 1 to 5 in the output rice min array
                  ENDIF ELSE BEGIN    ; If no "legit" maxs are found in the window, DON'T put the min to 5, and reset the MAX to 400 (a.k.a. --> max legit, but removed because no min found)
                    rice_max_arr_pix[temp_maxdoy[ind_season]] = 255
                    temp_maxdoy[ind_season] = 400
                    out_MAP_MAXDOY[ind_season,lc_ok]=400  ; --> Set to 400 maxs removed because no good min.Warning ! Map of maxsdoy is updatef, map of maxs no !!!! discrepancy in out !
                    out_MAP_HalfHead[ind_season,lc_ok]=400
                  ENDELSE
                ENDIF
              ENDFOREACH

              ; Check for removing maxima if the same minimum is detected on two consecutive quarters
              ; Only the first minimum (a.k.a. the first "season" is kept in this case)  (TO BE CHECKE D FURTHER !)

              FOREACH season, selquarts, ind_season DO BEGIN
                IF (n_sel_season GT 1) THEN BEGIN
                  IF ((temp_mindoy[ind_season] EQ temp_mindoy[ind_season-1])AND (temp_mindoy[ind_season] NE 0))  THEN BEGIN
                    temp_mindoy[ind_season] = 0
                    IF (temp_maxdoy[ind_season] NE 400) THEN rice_max_arr_pix[temp_maxdoy[ind_season]] = 255
                    out_MAP_MAXDOY[ind_season,lc_ok]=500
                    out_MAP_HalfHead[ind_season,lc_ok]=500
                  ENDIF
                ENDIF
              ENDFOREACH


              tempmin_ok = where(temp_mindoy ne 0, count)
              if (count ne 0 ) then out_map_mindoy [tempmin_ok,lc_ok] = doys_reg[temp_mindoy[tempmin_ok]]   ; Set the values for the output map of doys of rice mins


              ; Final check on decrease of signal !!!!!

              if max_criteria.decrease EQ 1 then begin

                FOREACH season, selquarts, ind_season DO BEGIN

                  if (temp_mindoy[ind_season] NE 0 AND temp_maxdoy[ind_season] NE 0 ) then begin
                    min_vi = smooth_pix[temp_mindoy[ind_season]]
                    max_vi = smooth_pix[temp_maxdoy[ind_season]]
                    thresh = min_vi +(max_vi - min_vi)*maxdec_perc
                    check = min(smooth_pix[temp_maxdoy[ind_season]:(temp_maxdoy[ind_season]+ maxdec_wid)<(nb-1)]-thresh)

                    if (check  GT 0) then begin   ; If check not passed, then set maxdoy, mindoy to 0
                      out_map_mindoy [ind_season,lc_ok] = 450
                      out_MAP_LastFlood [ind_season,lc_ok] = 450
                      out_MAP_MAXDOY[ind_season,lc_ok]  = 550
                      out_MAP_HalfHead[ind_season,lc_ok]=550
                      rice_min_arr_pix[max(pos_min_quart)] = 123
                      rice_max_arr_pix[max(pos_min_quart)] = 123

                    endif

                  endif

                ENDFOREACH


              endif

              ;- --------------------------------------------------------- ;
              ;- Write in four quarters the MAP of number of detected seasons = n° of quarters in which we have a "legit" value of out_map_max_doy
              ;- --------------------------------------------------------- ;
              out_MAPRICE[*,lc_ok] = total(out_MAP_MAXDOY[*,lc_ok] lt 400 and out_MAP_MAXDOY[*,lc_ok] ne 0 )

              ;- --------------------------------------------------------- ;
              ;- Write in four quarters the MAP of SoS & EoS --> This part will be probably re-checked in the near future
              ;- --------------------------------------------------------- ;

              FOREACH season, selquarts, ind_season DO BEGIN
                Flooded = where(NDFI_pix GT 0 , countflood)
                ;              for quart=0 , 3 do begin
                if out_MAP_MAXDOY[ind_season,lc_ok] NE 0 and out_MAP_MAXDOY[ind_season,lc_ok] lt  400 then begin    ; If a valid doy repo0rted for max in the quarter, compute pheonometrics
                  pos_min = temp_mindoy[ind_season] & val_min = smooth_pix[pos_min]
                  pos_max = temp_maxdoy[ind_season] & val_max = smooth_pix[pos_max]
                  out_MAP_VItot [ind_season,lc_ok]= total(smooth_pix[pos_min:pos_max]/100)
                  val_min_doy = out_MAP_MINDOY[ind_season,lc_ok]
                  ;- SoS: First position where vi > min+10% of min-max range ?????
                  ; - Flow: First position where vi > min+90% of min-max range ?????
                  ; - Half: First position where vi > min+50% of min-max range ?????
                  ; - MaxInc: Position of maximum increment between min and max

                  SoS = where((smooth_pix[pos_min:pos_max]  ge (val_min + 0.1 * (val_max-val_min))) and (smooth_pix[pos_min:pos_max] ge 0), countSoS)
                  Flow = where((smooth_pix[pos_min:pos_max]  ge (val_min + 0.9 * (val_max-val_min))) and (smooth_pix[pos_min:pos_max] ge 0), countSoS)
                  Half = where((smooth_pix[pos_min:pos_max]  ge (val_min + 0.5 * (val_max-val_min))) and (smooth_pix[pos_min:pos_max] ge 0), countSoS)
                  Increments = shift(smooth_pix - shift(smooth_pix,1),-1)
                  MaxInc = max(Increments[pos_min:pos_max], pos_maxinc)
                  HalfHead = where((smooth_pix[pos_min:(pos_max+maxdec_wid)]  ge (val_min + 0.9 * (val_max-val_min))) and (smooth_pix[pos_min:(pos_max+maxdec_wid)] ge 0), countHalfHead)


                  out_MAP_SOS[ind_season,lc_ok] = 8*min(SoS) + doys_reg[pos_min]     ; Compute DOYS on the basis of positions
                  out_MAP_Flow[ind_season,lc_ok] = 8*min(Flow) + doys_reg[pos_min]
                  out_MAP_Half[ind_season,lc_ok] = 8*min(Half) + doys_reg[pos_min]
                  out_MAP_MaxInc[ind_season,lc_ok] = 8*pos_maxinc + doys_reg[pos_min]
                  out_MAP_HalfHead [ind_season,lc_ok] = mean(8*HalfHead)+ doys_reg[pos_min]
                  doyflood = doy_pix[Flooded]
                  delta_last_flood = val_min_doy - doyflood
                  sort_delta_lastflood  = sort(abs(val_min_doy - doyflood))
                  ; [where((doyflood LE (val_min_doy+vi_flood_wid)) AND (doyflood GE (val_min_doy-vi_flood_wid)))]),pos_minlastflood)
                  poslastflood = doyflood[sort_delta_lastflood[0]]

                  out_MAP_LastFlood [ind_season,lc_ok] = poslastflood
                  ;------------------------------------------ EoS: ---> EOS computations very rough - need to be revised !!!
                  if nb-pos_max gt 8 then begin
                    if val_min le 0 then begin
                      EoS = where(smooth_pix[pos_max:pos_max+8] le (val_max - 0.5*(val_max)), countEoS)
                    endif else begin
                      EoS = where(smooth_pix[pos_max:pos_max+8] le (val_max - 0.5*(val_max-val_min)), countEoS)
                    endelse
                    if countEoS gt 0 then begin
                      out_MAP_EOS[ind_season,lc_ok] = 8*min(EoS) + doys_reg[pos_max]
                    endif else begin
                      out_MAP_EOS[ind_season,lc_ok] = doys_reg[pos_max]+8*8; ---> Possible error !!!!!!!!!!! WHY NOT 99 ??????
                    endelse
                  endif else begin
                    if nb-pos_max gt 3 then begin
                      win = nb-pos_max-1
                      if val_min le 0 then begin
                        EoS = where(smooth_pix[pos_max:pos_max+win] le (val_max - 0.5*(val_max-0)), countEoS)
                      endif else begin
                        EoS = where(smooth_pix[pos_max:pos_max+win] le (val_max - 0.5*(val_max-val_min)), countEoS)
                      endelse
                      if countEoS gt 0 then begin
                        out_MAP_EOS[ind_season,lc_ok] = 8*min(EoS) + doys_reg[pos_max]
                      endif
                      if countEoS eq 0 then begin
                        out_MAP_EOS[ind_season,lc_ok] = 99
                      endif
                    endif else  out_MAP_EOS[ind_season,lc_ok] = 99
                  endelse

                  ;- --------------------------------------------------------- ;
                  ;- Write in four quarters the MAP of metrics
                  ;- --------------------------------------------------------- ;

                  out_max_VI[ind_season,lc_ok] = val_max     ; Vi value at MAX
                  out_min_VI[ind_season,lc_ok] = val_min     ; Vi value at Min
                  out_SOS_MIN_DELTA[ind_season,lc_ok]  = (out_MAP_SOS[ind_season,lc_ok] - out_map_mindoy[ind_season,lc_ok])    ; n° of days between min and SOS
                  out_SOS_MAX_DELTA[ind_season,lc_ok]  = (out_MAP_MAXDOY[ind_season,lc_ok]-out_MAP_SOS[ind_season,lc_ok])      ; n° of days between max and SOS
                  out_MAX_MIN_DELTA[ind_season,lc_ok]  = (out_MAP_MAXDOY[ind_season,lc_ok] - out_map_mindoy [ind_season,lc_ok])  ; n° of days between max and min
                  ;                  out_EOS_MIN_DELTA[quart,lc_ok]  = (out_MAP_EOS[quart,lc_ok] - out_map_mindoy[quart,lc_ok]) * 8
                endif                                               ; punta a banda gt 0
              ENDFOREACH                                           ;fine loop sui 4 quarters
              out_RICE_MAX_ARRAY[*,lc_ok] = rice_max_arr_pix
              out_RICE_MIN_ARRAY[*,lc_ok] = rice_min_arr_pix
            endif else begin ; end If on no maxs identified - decide to what values should be setted !
              ;          print, 'nomaxs detected !!!!1'
            endelse
          endif else begin ; end If on average VI above threshold - If mean VI above thresh then min and maxs set to 600
            ;            print, 'Average VI above threshold'
            out_MAP_MAXDOY[*,lc_ok]=600
            out_MAP_MINDOY[*,lc_ok]=600
            out_MAP_HalfHead[*,lc_ok]=600
            out_MAP_LastFlood[*,lc_ok]=600
          endelse
        endif ; End cycle on nodata
      endfor ; End for on good land cover pixels
    endif   ; End IF on existence of at least one good land cover pixel in selected line
    ;endif  ; TO BE REMOVED
    ;- --------------------------------------------------------- ;
    ;- Write the lines of the output matrixes to the output files
    ;- --------------------------------------------------------- ;
    if add_outfiles EQ 1 then begin
      writeu, ricemax_array_id, out_RICE_MAX_ARRAY
      writeu, ricemin_array_id, out_RICE_MIN_ARRAY
    endif

    ; Build the "full" output for the processed line using the arrays of the different "partial" outputs related to the
    ; different computed parameters
    out_total_line = [out_MAPRICE, out_map_mindoy,out_MAP_MAXDOY, out_MAX_MIN_DELTA, out_MAP_SOS,out_SOS_MIN_DELTA, out_SOS_MAX_DELTA, out_MAP_Half, out_MAP_maxinc, out_MAP_Flow,out_MAP_EOS, out_min_VI, out_max_VI,out_MAP_HalfHead,out_MAP_LastFlood,out_MAP_VItot]
    writeu, Output_id, out_total_line

  endfor                                        ; End loop on ENVI tiles (lines)

  bnames_mindoy = ['Min_DOY_1st_Quarter','Min_DOY_2nd_Quarter','Min_DOY_3rd_Quarter','Min_DOY_4th_Quarter']
  bnames_maxdoy = ['Max_DOY_1st_Quarter','Max_DOY_2nd_Quarter','Max_DOY_3rd_Quarter','Max_DOY_4th_Quarter']
  bnames_maxmin_lgt= ['Min_Max_Length_1st_Quarter','Min_Max_Length_2nd_Quarter','Min_Max_Length_3rd_Quarter','Min_Max_Length_4th_Quarter']
  bnames_sosdoy = ['SOS_DOY_1st_Quarter','SOS_DOY_2nd_Quarter','SOS_DOY_3rd_Quarter','SOS_DOY_4th_Quarter']
  bnames_sosminlgt = ['SOS_Min_Length_1st_Quarter','SOS_Min_Length_2nd_Quarter','SOS_Min_Length_3rd_Quarter','SOS_Min_Length_4th_Quarter']
  bnames_sosmaxlgt = ['SOS_Max_Length_1st_Quarter','SOS_Max_Length_2nd_Quarter','SOS_Max_Length_3rd_Quarter','SOS_Max_Length_4th_Quarter']
  bnames_halfdoy = ['Half_DOY_1st_Quarter','Half_DOY_2nd_Quarter','Half_DOY_3rd_Quarter','Half_DOY_4th_Quarter']
  bnames_maxincdoy = ['MaxInc_DOY_1st_Quarter','MaxInc_DOY_2nd_Quarter','MaxInc_DOY_3rd_Quarter','MaxInc_DOY_4th_Quarter']
  bnames_flowdoy = ['Flow_DOY_1st_Quarter','Flow_DOY_2nd_Quarter','Flow_DOY_3rd_Quarter','Flow_DOY_4th_Quarter']
  bnames_eosdoy = ['EOS_DOY_1st_Quarter','EOS_DOY_2nd_Quarter','EOS_DOY_3rd_Quarter','EOS_DOY_4th_Quarter']
  bnames_minvi = ['Min_VI_1st_Quarter','Min_VI_2nd_Quarter','Min_VI_3rd_Quarter','Min_VI_4th_Quarter']
  bnames_maxvi = ['Max_VI_1st_Quarter','Max_VI_2nd_Quarter','Max_VI_3rd_Quarter','Max_VI_4th_Quarter']
  bnames_halfhead = ['Halfhead_1st_Quarter','HalfHead_2nd_Quarter','HalfHead_3rd_Quarter','HalfHead_4th_Quarter']
  bnames_lastflood = ['LastFlood1st_Quarter','LastFlood2nd_Quarter','LastFlood3rd_Quarter','LastFlood4th_Quarter']
  bnames_cumVi = ['CumVI1st_Quarter','CumVI2nd_Quarter','CumVI3rd_Quarter','CumVI4th_Quarter']
  ; Band names for the output file
  bnames_out_tot = ['N_Seasons', bnames_mindoy[selquarts], bnames_maxdoy[selquarts],bnames_maxmin_lgt[selquarts],bnames_sosdoy[selquarts], $
    bnames_sosminlgt[selquarts],bnames_sosmaxlgt[selquarts], bnames_halfdoy[selquarts],bnames_maxincdoy[selquarts], $
    bnames_flowdoy[selquarts],bnames_eosdoy[selquarts],bnames_minvi[selquarts],bnames_maxvi[selquarts] ,bnames_halfhead [selquarts],bnames_lastflood[selquarts],bnames_cumVi[selquarts]]

  ;- --------------------------------------------------------- ;
  ;-  write header files for the ouputs
  ;- --------------------------------------------------------- ;

  free_lun, Output_id
  envi_setup_head, fname=out_file , ns=ns, nl=nl, nb=n_elements(bnames_out_tot), data_type=2,         $
    offset=0, interleave=2, xstart=xstart+dims[1], ystart=ystart+dims[3],$
    bnames = bnames_out_tot,  $
    descrip='Global Phenorice Output', map_info = map_info, /write

  if add_outfiles EQ 1 then begin
    free_lun, ricemax_array_id   ; Map of maxs, classified according to what described above
    envi_setup_head, fname=outfile_maxarray, ns=ns, nl=nl, nb=nb, data_type=1,         $
      offset=0, interleave=2, MAP_INFO=map_info,           $
      bnames=bnames, xstart=xstart+dims[1], ystart=ystart+dims[3],                 $
      descrip='Phenorice - Categorized VI maximums ', /write

    free_lun, ricemin_array_id  ; Map of mins, classified according to what described above
    envi_setup_head, fname=outfile_minarray, ns=ns, nl=nl, nb=nb, data_type=2,         $
      offset=0, interleave=2, MAP_INFO=map_info,           $
      bnames=bnames, xstart=xstart+dims[1], ystart=ystart+dims[3],                 $
      descrip='Phenorice - Categorized VI minimums', /write
  endif
  ; Close the open files
  envi_file_mng, ID=ndvi_fid   , /REMOVE
  envi_file_mng, ID=ndii_fid   , /REMOVE
  envi_file_mng, ID=ndfi_fid   , /REMOVE
  envi_file_mng, ID=QA_fid     , /REMOVE
  envi_file_mng, ID=lc_fid     , /REMOVE
  envi_file_mng, ID=smooth_fid , /REMOVE
  envi_file_mng, ID=lst_fid    , /REMOVE
  envi_file_mng, ID=doy_fid    , /REMOVE

  ;- --------------------------------------------------------- ;
  ;-  Write log information
  ;- --------------------------------------------------------- ;
  close, /ALL
  heap_gc
  print,'Processing completed !'

  T2=systime(2)
  print,'start processing:', T1
  print,'start processing:', T2
  print, t2-t1
  print, 'done!'
  ;  printf, u_log, "End processing: " + T2

  return, 'DONE'

end
