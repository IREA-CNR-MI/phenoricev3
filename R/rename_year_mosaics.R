rename_year_mosaics <- function(mosaics_folder) {

  #   ____________________________________________________________________________
  #   change band names                         ####

  patterns    <- c("n*", "s*", "e*", "p*", "v*", "l*", "c*")
  newpatterns <- c("nseas", "sos_", "eos_", "pos_", "veglgt_", "totlgt_", "cumevi_")

  for (pat in seq_along(patterns)) {
    old_names <- list.files(mosaics_folder, pattern = glob2rx(patterns[pat]), full.names = F)
    new_names <- gsub(strsplit(patterns[pat],"*")[[1]][1], newpatterns[pat], old_names)
    file.rename(file.path(mosaics_folder, old_names), file.path(mosaics_folder, new_names))
  }
}

# mosaics_folder <- "/home/lb/my_data/prasia/mosaics/by_year"
# rename_year_mosaics(mosaics_folder)
# temp_vrt <- tempfile(fileext = ".vrt")
# pp = gdalUtils::gdalbuildvrt(ordered_files, temp_vrt, separate = T)
#
#
#  gdal_translate -ot Int16 -of Gtiff -co "COMPRESS=DEFLATE" /tmp/RtmpyrONRv/file7360614d92a6.vrt /tmp/RtmpyrONRv/prova.tif
