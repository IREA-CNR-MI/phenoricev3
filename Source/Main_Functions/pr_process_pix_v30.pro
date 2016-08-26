;+pr_process_pix_v30
;
; :Description:
;
;
; :Params:
;    opts       : structure of PhenoRice processing parameters
;    smooth_pix : Smoothed EVI of the pixel
;    NDFI_pix   : NDFI of the pixel
;    lst_pix    : LST of the pixel
;    doy_pix    : doy of the pixel
;    nb         : total number of bands in input images
;    doys_reg   : intarr with regular acqusition doys
;
; :Returns:
;
;    out_data   : structure containing results of phenological processing on the pixel
;
; :Author:
; 	Lorenzo Busetto, phD - email: busetto.l@irea.cnr.it (2016)
;
; :License: GPL>3.0
;-
FUNCTION pr_process_pix_v30, opts, smooth_pix, NDFI_pix, lst_pix, doy_pix, nb, doys_reg

  COMPILE_OPT hidden
  COMPILE_OPT IDL2
  COMPILE_OPT Strictarrsubs

  ; INitialize outputs arrays to -999

  out_nrice    = (out_maxdoy = (out_mindoy = (out_sosdoy = (out_eosdoy = (out_int = intarr(opts.n_sel_season) - 999)))))
  out_halfhead = (out_maxvi  = (out_minvi  = (out_max_min_delta = (out_eos_min_delta = intarr(opts.n_sel_season) - 999))))

  ;- ---------------------------------------------------------------- -
  ;- --- Start performing the checks to detect rice seasons -
  ;- ---------------------------------------------------------------- -

  ; Check on average EVI

  IF (opts.avg_check EQ 1) THEN BEGIN   ; If check on average was selected, then compute the average,
    avg = mean(smooth_pix)      ; otherwise set the avg to -999 so that check is always passed
  ENDIF ELSE BEGIN
    avg = -999
  ENDELSE

  ; Skip pixels if average of VI above selected threshold (~ "rough" forest mask)

  IF (avg LT opts.avg_thresh) THEN BEGIN

    ; Perform a basic smoothiing of LST values (removes spikes and NODATA)
    IF opts.LST EQ 1 THEN  lst_pix_smooth = pr_smooth_lst_v30(lst_pix,nb)

    ; Identify all local maxima and minima + compute array of VI derivatives

    pr_signal_analysis_v30, opts, smooth_pix,  nb, max_array, min_array, der_neg_pix, der_pos_pix

    ; Remove maximums corresponding to non-selected time periods using the pos_legit_max
    ; array

    max_array = max_array * opts.pos_legit_maxs

    ;- ---------------------------------------------------------------- -
    ; Check if identified maxs are "legit" according to the selected maximums criteria
    ; maxs not satisfying the criteria in max_array are set to a value representing which criteria failed
    ;- ---------------------------------------------------------------- -

    check_maxs = pr_crop_stage_max_v30(opts, smooth_pix, max_array, der_neg_pix, der_pos_pix,nb)

    IF check_maxs NE 200 THEN BEGIN ; Start if cycle - skips all further processing if all maxs already removed

      ;- --------------------------------------------------------- ;
      ; Check if identified mins are "legit" according to the selected maximums criteria
      ; mins not satisfying the criteria in min_array are set to a value representing which criteria failed
      ;- --------------------------------------------------------- ;

      check_mins = pr_crop_stage_min_v30(opts, smooth_pix, NDFI_pix, LST_pix, doy_pix, min_array, $
        max_array, der_neg_pix, der_pos_pix, nb, doys_reg)

      IF check_mins NE 200 THEN BEGIN  ; Start if cycle - skips all further processing if all mins already removed

        ;- --------------------------------------------------------- ;
        ;-  on the basis of previous results, identify maxs tuples
        ;- corresponding to rice seasons in the selected seasons and get their min/maxdates
        ;- --------------------------------------------------------- ;

        temp_maxdoy = (temp_mindoy = intarr(opts.n_sel_season) - 999)
        val_maxs    = smooth_pix * (max_array EQ 1)  ; Array with values of recognized maxs - 0 elsewhere

        FOREACH season, opts.selquarts, ind_season DO BEGIN          ; Cycle on selected quarters to set the doy of maxs recognized in the different quarters
          val_max_quart = val_maxs * (opts.pos_quart_max EQ season)  ;   Create array with values of recognized maxs in selected quarter - 0 elsewhere
          val_max_ass   = max(val_max_quart, pos_max_ass)            ;   Find absolute max in selected quarter and its position
          IF (val_max_ass NE 0 ) THEN BEGIN
            temp_maxdoy[ind_season] = pos_max_ass                    ; If a max availble, put its position in temp_maxdoy (if no max, temp_max_map of the quarter stays at -999)
            max_array[pos_max_ass]  = 5  ; Change the values of "rice" maxs (i.e., absolute maxs in quarters) from 1 to 5 in the output rice max array
          ENDIF

        ENDFOREACH

        tempmax_ok = where(temp_maxdoy NE -999, count)  ; Where did I find a good max ?
        IF (count NE 0 ) THEN out_maxdoy [tempmax_ok] = doys_reg[temp_maxdoy[tempmax_ok]]   ; Set the values for the output map of doys of rice maxs

        ;- --------------------------------------------------------- ;
        ;- on the basis of previous results, identify mins
        ;- corresponding to rice seasons in the selected seasons and get their dates
        ;- --------------------------------------------------------- ;

        FOREACH season, opts.selquarts, ind_season DO BEGIN

          pos_minq_ok       = intarr(nb)
          temp_maxdoy_quart = temp_maxdoy[ind_season]
          IF temp_maxdoy_quart  NE -999 THEN BEGIN

            ; find  minimums between XX and YY composites of absolute max
            pos_minq_ok [temp_maxdoy_quart-opts.max_aft_win[1] > 0 : temp_maxdoy_quart-opts.max_aft_win[0]] = 1 ;
            check_arr     = pos_minq_ok*min_array
            pos_min_quart = where((check_arr EQ 5) OR (check_arr EQ 1), count)
            IF count GT 0 THEN BEGIN
              temp_mindoy[ind_season]       = max(pos_min_quart)
              min_array[max(pos_min_quart)] = 5 ; Change the values of "rice" mins (i.e., mins nearest to a suitable max) from 1 to 5 in the output rice min array
            ENDIF ELSE BEGIN    ; If no "legit" maxs are found in the window, DON'T put the min to 5, and reset the MAX to -999 (a.k.a. --> max legit, but removed because no min found)
              max_array[temp_maxdoy[ind_season]] = -999
              temp_maxdoy[ind_season]            = -999
              out_maxdoy[ind_season]             = -999  ; --> Set to -999 maxs removed because no good min
              out_halfhead[ind_season]           = -999
            ENDELSE
          ENDIF

        ENDFOREACH

        ; Check for removing maxima if the same minimum is detected on two consecutive quarters
        ; Only the first minimum (a.k.a. the first "season" is kept in this case)  (TO BE CHECKE D FURTHER !)

        FOREACH season, opts.selquarts, ind_season DO BEGIN
          IF (opts.n_sel_season GT 1) THEN BEGIN
            IF ((temp_mindoy[ind_season] EQ temp_mindoy[ind_season-1]) AND (temp_mindoy[ind_season] NE -999))  THEN BEGIN
              temp_mindoy[ind_season]  = 0
              IF (temp_maxdoy[ind_season] NE -999) THEN max_array[temp_maxdoy[ind_season]] = -999
              out_maxdoy[ind_season]   = -999
              out_halfhead[ind_season] = -999
            ENDIF
          ENDIF
        ENDFOREACH

        ; Get the values of doys corresponding to the retained minima

        tempmin_ok = where(temp_mindoy NE -999, count)
        IF (count NE 0 ) THEN out_mindoy [tempmin_ok] = doys_reg[temp_mindoy[tempmin_ok]]

        ; Final check on decrease of signal !!!!!

        IF opts.decrease EQ 1 AND count NE 0 THEN BEGIN

          FOREACH season, opts.selquarts, ind_season DO BEGIN

            IF (temp_mindoy[ind_season] NE -999 AND temp_maxdoy[ind_season] NE -999 ) THEN BEGIN
              min_vi = smooth_pix[temp_mindoy[ind_season]]
              max_vi = smooth_pix[temp_maxdoy[ind_season]]
              thresh = min_vi +(max_vi - min_vi)*opts.decrease_perc
              check  = min(smooth_pix[temp_maxdoy[ind_season]:(temp_maxdoy[ind_season]+ opts.decrease_win)<(nb-1)]-thresh)

              IF (check  GT 0) THEN BEGIN   ; If check not passed, then set maxdoy, mindoy etc to -999
                out_mindoy [ind_season]       = -999
                out_maxdoy [ind_season]        = -999
                ;                out_halfhead[ind_season]      = -999
                ;                min_array[max(pos_min_quart)] = -999
                ;                max_array[max(pos_min_quart)] = -999

              ENDIF

            ENDIF

          ENDFOREACH

        ENDIF

        ok_seasons = where(temp_mindoy NE -999, count_okseasons)

        IF (count_okseasons NE 0) THEN BEGIN

          IF (opts.shp_check EQ 1) THEN BEGIN

            check_shp = pr_checkshape(opts, smooth_pix, temp_mindoy, temp_maxdoy, doys_reg, ok_seasons)
            check_failed = where(check_shp EQ 0, count_failed)
            
            IF (count_failed NE 0) THEN BEGIN

              out_mindoy [where(check_shp EQ 0)] = -999
              out_maxdoy [where(check_shp EQ 0)] = -999

            ENDIF

          ENDIF

        ENDIF



        ;- --------------------------------------------------------- ;
        ;- Compute number of identified rice seasons
        ;- --------------------------------------------------------- ;
        out_nrice = total(out_maxdoy NE -999)

        ;- --------------------------------------------------------- ;
        ;- Compute additional pheno paramaetrs on identified seasons
        ;- (execute ONLY if at least a good season was found !)
        ;- --------------------------------------------------------- ;

        IF (out_nrice GT 0) THEN BEGIN

          FOREACH season, opts.selquarts, ind_season DO BEGIN

            ;- --------------------------------------------------------- ;
            ; If a valid doy reported for max in the quarter, compute pheonometrics
            ;- --------------------------------------------------------- ;

            ;            flooded = where(NDFI_pix GT 0, countflood)

            IF out_maxdoy[ind_season] NE -999 THEN BEGIN
              pos_min     = temp_mindoy[ind_season] & val_min = smooth_pix[pos_min]
              pos_max     = temp_maxdoy[ind_season] & val_max = smooth_pix[pos_max]
              val_min_doy = out_mindoy[ind_season]

              ; "flowering" date: hlf position of where vi above 90th percentile !

              if opts.decrease EQ 1 then begin
                HalfHead = where((smooth_pix[pos_min:(pos_max+opts.decrease_win)] GE (val_min + 0.9 * (val_max-val_min))) AND (smooth_pix[pos_min:(pos_max+opts.decrease_win)] GE 0), countHalfHead)
                out_halfhead [ind_season] = mean(8*HalfHead)+ doys_reg[pos_min]
              endif else begin
                HalfHead = where((smooth_pix[pos_min:(pos_max+3)] GE (val_min + 0.9 * (val_max-val_min))) AND (smooth_pix[pos_min:(pos_max+3)] GE 0), countHalfHead)
                out_halfhead [ind_season] = mean(8*HalfHead)+ doys_reg[pos_min]
              endelse

              ;- SoS: First position where vi > min+10% of min-max range ?????
              ; - Flow: First position where vi > min+90% of min-max range ?????
              ; - Half: First position where vi > min+50% of min-max range ?????
              ; - MaxInc: Position of maximum increment between min and max

              ; Old stuff - kept here in case still neeeded in the future

              ;          SoS = where((smooth_pix[pos_min:pos_max]  GE (val_min + 0.1 * (val_max-val_min))) AND (smooth_pix[pos_min:pos_max] GE 0), countSoS)
              ;          Flow = where((smooth_pix[pos_min:pos_max]  GE (val_min + 0.9 * (val_max-val_min))) AND (smooth_pix[pos_min:pos_max] GE 0), countflw)
              ;          Half = where((smooth_pix[pos_min:pos_max]  GE (val_min + 0.5 * (val_max-val_min))) AND (smooth_pix[pos_min:pos_max] GE 0), counthalf)
              ;          Increments = shift(smooth_pix - shift(smooth_pix,1),-1)
              ;          MaxInc = max(Increments[pos_min:pos_max], pos_maxinc)
              ;          out_MAP_SOS[ind_season,lc_ok] = 8*min(SoS) + doys_reg[pos_min]     ; Compute DOYS on the basis of positions
              ;          out_MAP_Flow[ind_season,lc_ok] = 8*min(Flow) + doys_reg[pos_min]
              ;          out_MAP_Half[ind_season,lc_ok] = 8*min(Half) + doys_reg[pos_min]
              ;          out_MAP_MaxInc[ind_season,lc_ok] = 8*pos_maxinc + doys_reg[pos_min]

              ;          doyflood = doy_pix[Flooded]
              ;          delta_last_flood = val_min_doy - doyflood
              ;          sort_delta_lastflood  = sort(abs(val_min_doy - doyflood))
              ;          ; [where((doyflood LE (val_min_doy+vi_flood_wid)) AND (doyflood GE (val_min_doy-vi_flood_wid)))]),pos_minlastflood)
              ;          poslastflood = doyflood[sort_delta_lastflood[0]]
              ;
              ;          out_lastflood [ind_season,lc_ok] = poslastflood

              ;- --------------------------------------------------------- ;
              ; Compute EoS: ---> EOS computations very rough - need to be revised !!!
              ; removed ALL checks. Now if the time series "ends" before reaching EOS, the EOS
              ; is just kept to -999
              ;- --------------------------------------------------------- ;

              EoS = where(smooth_pix[pos_max:(nb-1)] LE (val_max - 0.5*(val_max-val_min)), countEoS)
              IF countEoS GT 0 THEN BEGIN
                out_eosdoy[ind_season] = 8*min(EoS) + doys_reg[pos_max]
              ENDIF

              ;- --------------------------------------------------------- ;
              ;- Additional phenometrics: max and min vi, integral, lenghts
              ;- --------------------------------------------------------- ;

              out_int [ind_season]          = total(smooth_pix[pos_min:pos_max])/10  ; integral of EVI between sow and max
              out_maxvi[ind_season]         = val_max     ; Vi value at MAX
              out_minvi[ind_season]         = val_min     ; Vi value at Min
              out_max_min_delta[ind_season] = (out_maxdoy[ind_season] - out_mindoy [ind_season])  ; n° of days between max and min
              out_eos_min_delta[ind_season] = (out_eosdoy[ind_season] - out_mindoy [ind_season])  ; n° of days between eos and min

            ENDIF  ; end if on identfied season in quarter

          ENDFOREACH ;fine loop sui 4 quarters

        ENDIF ; End loop on rice seasons > 0 x computing additiona parameters

      ENDIF ELSE BEGIN ; end If on no legit mins identified

        out_nrice = 0

      ENDELSE

    ENDIF ELSE BEGIN ; end If on no legit maxs identified

      out_nrice = 0

    ENDELSE

  ENDIF ELSE BEGIN ; end If on average VI above threshold

    out_nrice = 0

  ENDELSE

  ;- --------------------------------------------------------- ;
  ;- Set up output structure for the pixel (easier to use than )
  ;- array since I can interrogate by tag
  ;- --------------------------------------------------------- ;


  out_data = {$
    out_nrice         : out_nrice   ,$
    out_maxdoy        : out_maxdoy    , $
    out_mindoy        : out_mindoy    , $
    out_halfhead      : out_halfhead  , $
    out_eosdoy        : out_eosdoy    , $
    out_int           : out_int       , $
    out_maxvi         : out_maxvi     , $
    out_minvi         : out_minvi     , $
    out_max_min_delta : out_max_min_delta, $
    out_eos_min_delta : out_eos_min_delta  $
  }

  return, out_data

END