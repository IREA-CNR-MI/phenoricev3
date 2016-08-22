FUNCTION pr_process_pix ,vi_pix, smooth_pix, doy_pix, lst_pix, proc_opts, $
max_criteria, min_criteria, doy_pix, nb, ndfi_pix
  COMPILE_OPT idl2

   IF (avg_criteria.avg_check EQ 1) THEN BEGIN   ; If check on average was selected, then compute the average,
    avg = mean(smooth_pix [good_data_pos] )      ; otherwise set the avg to -999 so that check is always passed
  ENDIF ELSE BEGIN
    avg = -999
  ENDELSE

  IF (avg LT avg_criteria.avg_thresh) THEN BEGIN    ; Remove pixels if average of VI above selected threshold (~ "rough" forest mask)

    IF min_criteria.LST EQ 1 THEN  lst_pix_smooth = pr_smooth_lst_v30(lst_pix,nb)   ; Perform a basic smoothiing of LST values (removes spikes and NODATA)

    ; Identify all local maxima and minima + compute array of VI derivatives

    pr_signal_analysis_v30, smooth_pix, NDFI_pix,nb, lc_ok, $  ;
      out_RICE_MAX_ARRAY, out_RICE_MIN_ARRAY, $
      der_neg_pix, der_pos_pix, proc_opts.win_dim_l, proc_opts.win_dim_r

    rice_max_arr_pix = out_RICE_MAX_ARRAY[*,lc_ok]    ; put the retrieved max in max array
    rice_min_arr_pix = out_RICE_MIN_ARRAY[*,lc_ok]    ; put the retrieved mins in min array


    ; Remove maximums corresponding to non-selected seasons
    rice_max_arr_pix = rice_max_arr_pix * pos_legit_maxs

    ; Check if identified maxs are "legit" according to the selected maximums criteria
    ; maxs not satisfying the criteria in rice_max_arr_pix are set to a value representing which criteria failed

    check_maxs = pr_crop_stage_max_v22(smooth_pix, rice_max_arr_pix, der_neg_pix, der_pos_pix,nb, proc_opts, $
      der_ind,maxdec_wid, maxdec_ind, maxthresh, maxdec_perc, check_arr_max, proc_opts.win_dim_l)

    IF check_maxs EQ 0 THEN BEGIN ; Start if cycle - skips all further processing if all maxs already removed

      ;- --------------------------------------------------------- ;
      ; Check if identified mins are "legit" according to the selected maximums criteria
      ; mins not satisfying the criteria in rice_min_arr_pix are set to a value representing which criteria failed
      ;- --------------------------------------------------------- ;
      check_mins = pr_crop_stage_min_v22(smooth_pix , rice_min_arr_pix, rice_max_arr_pix, NDII_pix, NDFI_pix, LST_pix,     $
        der_neg_pix, der_pos_pix, nb, minthresh,growth_ind, growth_wid, growth_thresh, $
        vi_flood_ind,vi_flood_wid, check_NDFI,check_NDII, vi_max_ind, LST_thresh, check_arr_min, proc_opts.win_dim_l, doys_reg,doy_pix )

      ;- --------------------------------------------------------- ;
      ;-  Write in four quarters the MAP of rice MAX DOYS
      ;- --------------------------------------------------------- ;
      temp_maxdoy = intarr(n_sel_season)
      val_maxs =  smooth_pix * (rice_max_arr_pix EQ 1)  ; Array with values of recognized maxs - 0 elsewhere

      FOREACH season, selquarts, ind_season DO BEGIN    ; Cycle on selected quarters to set the doy of maxs recognized in the different quarters
        val_max_quart =  val_maxs * (pos_quart_max EQ season)  ;   Create array with values of recognized maxs in selected quarter - 0 elsewhere
        val_max_ass = max(val_max_quart, pos_max_ass)  ;   Find absolute max in selected quarter and its position
        IF (val_max_ass NE 0 ) THEN BEGIN
          temp_maxdoy[ind_season] = pos_max_ass   ; If a max availble, put its position in temp_max_map (if no max, temp_max_map of the quarter stays at 0)
          rice_max_arr_pix[pos_max_ass] = 5  ; Change the values of "rice" maxs (i.e., absolute maxs in quarters) from 1 to 5 in the output rice max array
        ENDIF
      ENDFOREACH

      tempmax_ok = where (temp_maxdoy NE 0, count)
      IF (count NE 0 ) THEN out_MAP_MAXDOY [tempmax_ok,lc_ok] = doys_reg[temp_maxdoy[tempmax_ok]]   ; Set the values for the output map of doys of rice maxs

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


      tempmin_ok = where(temp_mindoy NE 0, count)
      IF (count NE 0 ) THEN out_map_mindoy [tempmin_ok,lc_ok] = doys_reg[temp_mindoy[tempmin_ok]]   ; Set the values for the output map of doys of rice mins


      ; Final check on decrease of signal !!!!!

      IF max_criteria.decrease EQ 1 THEN BEGIN

        FOREACH season, selquarts, ind_season DO BEGIN

          IF (temp_mindoy[ind_season] NE 0 AND temp_maxdoy[ind_season] NE 0 ) THEN BEGIN
            min_vi = smooth_pix[temp_mindoy[ind_season]]
            max_vi = smooth_pix[temp_maxdoy[ind_season]]
            thresh = min_vi +(max_vi - min_vi)*maxdec_perc
            check = min(smooth_pix[temp_maxdoy[ind_season]:(temp_maxdoy[ind_season]+ maxdec_wid)<(nb-1)]-thresh)

            IF (check  GT 0) THEN BEGIN   ; If check not passed, then set maxdoy, mindoy to 0
              out_map_mindoy [ind_season,lc_ok] = 450
              out_MAP_LastFlood [ind_season,lc_ok] = 450
              out_MAP_MAXDOY[ind_season,lc_ok]  = 550
              out_MAP_HalfHead[ind_season,lc_ok]=550
              rice_min_arr_pix[max(pos_min_quart)] = 123
              rice_max_arr_pix[max(pos_min_quart)] = 123

            ENDIF

          ENDIF

        ENDFOREACH


      ENDIF

      ;- --------------------------------------------------------- ;
      ;- Write in four quarters the MAP of number of detected seasons = n° of quarters in which we have a "legit" value of out_map_max_doy
      ;- --------------------------------------------------------- ;
      out_MAPRICE[*,lc_ok] = total(out_MAP_MAXDOY[*,lc_ok] LT 400 AND out_MAP_MAXDOY[*,lc_ok] NE 0 )

      ;- --------------------------------------------------------- ;
      ;- Write in four quarters the MAP of SoS & EoS --> This part will be probably re-checked in the near future
      ;- --------------------------------------------------------- ;

      FOREACH season, selquarts, ind_season DO BEGIN
        Flooded = where(NDFI_pix GT 0 , countflood)
        ;              for quart=0 , 3 do begin
        IF out_MAP_MAXDOY[ind_season,lc_ok] NE 0 AND out_MAP_MAXDOY[ind_season,lc_ok] LT  400 THEN BEGIN    ; If a valid doy repo0rted for max in the quarter, compute pheonometrics
          pos_min = temp_mindoy[ind_season] & val_min = smooth_pix[pos_min]
          pos_max = temp_maxdoy[ind_season] & val_max = smooth_pix[pos_max]
          out_MAP_VItot [ind_season,lc_ok]= total(smooth_pix[pos_min:pos_max]/100)
          val_min_doy = out_MAP_MINDOY[ind_season,lc_ok]
          ;- SoS: First position where vi > min+10% of min-max range ?????
          ; - Flow: First position where vi > min+90% of min-max range ?????
          ; - Half: First position where vi > min+50% of min-max range ?????
          ; - MaxInc: Position of maximum increment between min and max

          SoS = where((smooth_pix[pos_min:pos_max]  GE (val_min + 0.1 * (val_max-val_min))) AND (smooth_pix[pos_min:pos_max] GE 0), countSoS)
          Flow = where((smooth_pix[pos_min:pos_max]  GE (val_min + 0.9 * (val_max-val_min))) AND (smooth_pix[pos_min:pos_max] GE 0), countSoS)
          Half = where((smooth_pix[pos_min:pos_max]  GE (val_min + 0.5 * (val_max-val_min))) AND (smooth_pix[pos_min:pos_max] GE 0), countSoS)
          Increments = shift(smooth_pix - shift(smooth_pix,1),-1)
          MaxInc = max(Increments[pos_min:pos_max], pos_maxinc)
          HalfHead = where((smooth_pix[pos_min:(pos_max+maxdec_wid)]  GE (val_min + 0.9 * (val_max-val_min))) AND (smooth_pix[pos_min:(pos_max+maxdec_wid)] GE 0), countHalfHead)


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
          IF nb-pos_max GT 8 THEN BEGIN
            IF val_min LE 0 THEN BEGIN
              EoS = where(smooth_pix[pos_max:pos_max+8] LE (val_max - 0.5*(val_max)), countEoS)
            ENDIF ELSE BEGIN
              EoS = where(smooth_pix[pos_max:pos_max+8] LE (val_max - 0.5*(val_max-val_min)), countEoS)
            ENDELSE
            IF countEoS GT 0 THEN BEGIN
              out_MAP_EOS[ind_season,lc_ok] = 8*min(EoS) + doys_reg[pos_max]
            ENDIF ELSE BEGIN
              out_MAP_EOS[ind_season,lc_ok] = doys_reg[pos_max]+8*8; ---> Possible error !!!!!!!!!!! WHY NOT 99 ??????
            ENDELSE
          ENDIF ELSE BEGIN
            IF nb-pos_max GT 3 THEN BEGIN
              win = nb-pos_max-1
              IF val_min LE 0 THEN BEGIN
                EoS = where(smooth_pix[pos_max:pos_max+win] LE (val_max - 0.5*(val_max-0)), countEoS)
              ENDIF ELSE BEGIN
                EoS = where(smooth_pix[pos_max:pos_max+win] LE (val_max - 0.5*(val_max-val_min)), countEoS)
              ENDELSE
              IF countEoS GT 0 THEN BEGIN
                out_MAP_EOS[ind_season,lc_ok] = 8*min(EoS) + doys_reg[pos_max]
              ENDIF
              IF countEoS EQ 0 THEN BEGIN
                out_MAP_EOS[ind_season,lc_ok] = 99
              ENDIF
            ENDIF ELSE  out_MAP_EOS[ind_season,lc_ok] = 99
          ENDELSE

          ;- --------------------------------------------------------- ;
          ;- Write in four quarters the MAP of metrics
          ;- --------------------------------------------------------- ;

          out_max_VI[ind_season,lc_ok] = val_max     ; Vi value at MAX
          out_min_VI[ind_season,lc_ok] = val_min     ; Vi value at Min
          out_SOS_MIN_DELTA[ind_season,lc_ok]  = (out_MAP_SOS[ind_season,lc_ok] - out_map_mindoy[ind_season,lc_ok])    ; n° of days between min and SOS
          out_SOS_MAX_DELTA[ind_season,lc_ok]  = (out_MAP_MAXDOY[ind_season,lc_ok]-out_MAP_SOS[ind_season,lc_ok])      ; n° of days between max and SOS
          out_MAX_MIN_DELTA[ind_season,lc_ok]  = (out_MAP_MAXDOY[ind_season,lc_ok] - out_map_mindoy [ind_season,lc_ok])  ; n° of days between max and min
          ;                  out_EOS_MIN_DELTA[quart,lc_ok]  = (out_MAP_EOS[quart,lc_ok] - out_map_mindoy[quart,lc_ok]) * 8
        ENDIF                                               ; punta a banda gt 0
      ENDFOREACH                                           ;fine loop sui 4 quarters
      out_RICE_MAX_ARRAY[*,lc_ok] = rice_max_arr_pix
      out_RICE_MIN_ARRAY[*,lc_ok] = rice_min_arr_pix
    ENDIF ELSE BEGIN ; end If on no maxs identified - decide to what values should be setted !
      ;          print, 'nomaxs detected !!!!1'
    ENDELSE
  ENDIF ELSE BEGIN ; end If on average VI above threshold - If mean VI above thresh then min and maxs set to 600
    ;            print, 'Average VI above threshold'
    out_MAP_MAXDOY[*,lc_ok]=600
    out_MAP_MINDOY[*,lc_ok]=600
    out_MAP_HalfHead[*,lc_ok]=600
    out_MAP_LastFlood[*,lc_ok]=600
  ENDELSE

  return, 1
END

