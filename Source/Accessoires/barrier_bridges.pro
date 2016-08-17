pro barrier_bridges, bridges
  COMPILE_OPT IDL2
  COMPILE_OPT hidden
  
	ncpus = n_elements(bridges)
	idle = bytarr(ncpus)
	widle = where(idle eq 0, nw)
	time = 0 
	wait = 5
	repeat begin
		time = time + wait
		for i=0,nw-1 do begin
			case (bridges[widle[i]])->status(error=errstr) of
				0: idle[widle[i]] = 1b
				2: idle[widle[i]] = 1b
				3: begin
					print, 'Error encountered: '+errstr
					stop
				end
				4: begin
					print, 'Aborted execution: '+errstr
					stop
				end
				else: ; do nothing
			endcase
		endfor
		widle = where(idle eq 0, nw)
		if nw gt 0 then begin
 
		  print, '# Still processing on: ' + strtrim(string(nw),2) + $
		     ' of: '+ strtrim(string(ncpus),2) + ' cpus  - Processing Time: ' + strtrim(string(time),2) + ' seconds'   
		  wait, wait ; idle loop
		endif
	endrep until nw eq 0
end
