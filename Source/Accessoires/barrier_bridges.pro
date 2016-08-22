PRO barrier_bridges, bridges
  COMPILE_OPT IDL2
  COMPILE_OPT hidden

  ncpus = n_elements(bridges)
  idle = bytarr(ncpus)
  widle = where(idle EQ 0, nw)
  time = 0
  wait = 1
  REPEAT BEGIN
    time = time + wait
    FOR i=0,nw-1 DO BEGIN
      CASE (bridges[widle[i]])->status(error=errstr) OF
        0: idle[widle[i]] = 1b
        2: idle[widle[i]] = 1b
        3: BEGIN
          print, 'Error encountered: '+errstr
          stop
        END
        4: BEGIN
          print, 'Aborted execution: '+errstr
          stop
        END
        ELSE: ; do nothing
      ENDCASE
    ENDFOR
    widle = where(idle EQ 0, nw)
    IF nw GT 0 THEN BEGIN
      IF (time MOD 10) EQ 0 THEN BEGIN
        print, '# Still processing on: ' + strtrim(string(nw),2) + $
          ' of: '+ strtrim(string(ncpus),2) + ' cpus  - Processing Time: ' + strtrim(string(time),2) + ' seconds'
      ENDIF
    ENDIF
    wait, wait ; idle loop

  ENDREP UNTIL nw EQ 0
  
END
