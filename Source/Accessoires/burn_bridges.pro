pro burn_bridges, bridges
  COMPILE_OPT IDL2
  COMPILE_OPT hidden
	ncpus = n_elements(bridges)
	for cpu=0,ncpus-1 do $
		obj_destroy, bridges[cpu]
end
