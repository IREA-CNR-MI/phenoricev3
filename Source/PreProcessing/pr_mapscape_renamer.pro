input_mapscape_folder = '/home/lb/network/ftp/IREA/Temp_Sharing/Boghendra/2015-2016/input'
input_smoothed_folder = '/home/lb/network/ftp/IREA/Temp_Sharing/Boghendra/2015-2016/Filter_smooth'
orig_suffixes = ['EVI','NDII7','NDFI','BLUE','pixel_reliability','vi_usefulness','DOY_PIX','LST']
out_suffixes = ['EVI',$
  'NDII7',$
  'NDFI',$
  'b3_Blue'$
  ,'Rely'$
  ,'UI'$
  ,'DOY'$
  ,'LST']

out_folders = ['VI_16Days_250m/EVI',$
  'VI_16Days_250m/NDII7',$
  'VI_16Days_250m/NDFI',$
  'VI_16Days_250m/b3_Blue'$
  ,'VI_16Days_250m/Rely'$
  ,'VI_16Days_250m/UI'$
  ,'VI_16Days_250m/DOY'$
  ,'Surf_Temp_8Days_1Km/LST']

; Rename MAPSCAPE files to conform to Phenorice conventions

for band = 0L, n_elements(orig_suffixes)-1 do begin

  print, 'Renaming ' + orig_suffixes[band] + 'Files'
  pattern = '*'+orig_suffixes[band]     ; Set the patern to search for one of theinputs (e.g., NDVI )
  in_files = file_search(input_mapscape_folder+path_sep()+pattern, /TEST_REGULAR)
  out_dir = input_mapscape_folder + path_sep() + out_folders[band]
  FILE_MKDIR, out_dir
  foreach in_file, in_files do begin

    split_bnames = strsplit(file_basename(in_file), '_', /extract)
    if (orig_suffixes[band] EQ  'pixel_reliability') OR (orig_suffixes[band] EQ  'vi_usefulness') OR (orig_suffixes[band] EQ 'DOY_PIX') then begin
      yeardoy = split_bnames[n_elements(split_bnames)-3]
    endif else begin
      yeardoy = split_bnames[n_elements(split_bnames)-2]
    endelse

    out_file = out_dir + path_sep()+ 'MAPSCAPEIN_'+out_suffixes[band]+'_'+strmid(yeardoy, 0,4)+'_'+strmid(yeardoy, 4,3)
    ;      file_copy, in_file, out_file+'.dat', /OVERWRITE
    ;      file_copy, in_file+'.hdr', out_file + '.hdr', /OVERWRITE
    ;      file_copy, in_file+'.sml', out_file + '.sml', /OVERWRITE

    ; Activate this only after testing !!!!!
    file_move, in_file, out_file+'.dat'
    file_move, in_file+'.hdr', out_file + '.hdr'
    file_move, in_file+'.sml', out_file + '.sml'

  endforeach

endfor

; Rename MAPSCAPE smoothed files to conform to Phenorice conventions
print, 'Renaming Smoothed EVI Files'
pattern = '*EVI_smth'
in_files = file_search(input_smoothed_folder+path_sep()+pattern, /TEST_REGULAR)
out_dir = input_mapscape_folder + path_sep() + 'EVI_Smoothed'
FILE_MKDIR,out_dir
foreach in_file, in_files do begin
  split_bnames = strsplit(file_basename(in_file), '_', /extract)
  yeardoy = split_bnames[n_elements(split_bnames)-3]
  out_file = out_dir + path_sep()+ 'MAPSCAPEIN_EVIsmooth_'+strmid(yeardoy, 0,4)+'_'+strmid(yeardoy, 4,3)
  ;  file_copy, in_file, out_file+'.dat', /OVERWRITE
  ;  file_copy, in_file+'.hdr', out_file + '.hdr', /OVERWRITE
  ;  file_copy, in_file+'.sml', out_file + '.sml', /OVERWRITE

  ; Activate this only after testing !!!!!
  file_move, in_file, out_file+'.dat'
  file_move, in_file+'.hdr', out_file + '.hdr'
  file_move, in_file+'.sml', out_file + '.sml'
endforeach
print, 'Finished'
end