function build_bridges, ncpus, nthreads
  COMPILE_OPT IDL2
  COMPILE_OPT hidden
	if n_elements(ncpus) eq 0 then $
		ncpus = !cpu.hw_ncpu
	if n_elements(nthreads) eq 0 then $
		nthreads = 1
	bridges = objarr(ncpus)
	wait, 0.01
	;Scd, current=pwd
	for cpu=0,ncpus-1 do begin
		; create bridge
		print, cpu
		logfile = "/home/lb/log_bridge_"+strtrim(string(cpu),2)+'.log'
		bridges[cpu] =  obj_new('IDL_IDLBridge', OUTPUT=logfile)
		
	;	Obj_New('IDL_IDLBridge_ENVI'); obj_new('IDL_IDLBridge', OUTPUT=logfile)
		; expand path so that routines in the path are found !
		bridges[cpu] ->SetVar,'!path',!path  
		;(bridges[cpu])->Execute, 'Envi, /RESTORE_BASE_SAVE_FILES & Message, /RESET' 
	endfor
	return, bridges
end
