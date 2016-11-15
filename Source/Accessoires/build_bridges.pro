function build_bridges, ncpus, nthreads
  COMPILE_OPT IDL2
  COMPILE_OPT hidden
	if n_elements(ncpus) eq 0 then $
		ncpus = !cpu.hw_ncpu
	if n_elements(nthreads) eq 0 then $
		nthreads = 1
	bridges = objarr(ncpus)
	;Scd, current=pwd
	for cpu=0,ncpus-1 do begin
		; create bridge
		logfile = "/home/lb/log_bridge_"+strtrim(string(cpu),2)+'.log'
		bridges[cpu] = obj_new('IDL_IDLBridge', OUTPUT=logfile)
		; expand path so that routines in the path are found !
		(bridges[cpu])->SetVar,'!path',!path  
	endfor
	return, bridges
end
