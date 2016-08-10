function path_create , string_arr

  path = string_arr[0]
  for index = 1L, N_ELEMENTS(string_arr)-1 do begin
    path = path + PATH_SEP()+ string_arr[index]
  endfor
  return,path
  
end


