library(tidyverse)
library(gdalUtils)
library(foreach)
library(parallel)
library(doSNOW)
mosaics_folder <- "/home/lbusetto/nas-s4a/nr_working/shared/PhenoRice/Asia/Data/mosaics/by_year/"
patterns <- c("sos", "eos", "pos", "cumevi", "veglgt", "totlgt", "nseas")

for (pattern in patterns) {
  print(pattern)
  create_vrt_mosaics(mosaics_folder, pattern)
}

vrts    <- list.files(mosaics_folder, pattern = ".vrt", full.names = T)
outfold <- "/home/lbusetto/nas-s4a/nr_working/shared/PhenoRice/Asia/Data/mosaics/ordered"
dir.create(outfold)

ncores <- 8
clust <- parallel::makeCluster(ncores, outfile = " ")
doSNOW::registerDoSNOW(clust)

out <- foreach(vrt_n = seq_along(vrts),
               .combine      = "c",
               .packages     = c("gdalUtils"),
               .verbose      = TRUE) %dopar% {

                 message("create_vrt_tiff --> processing: ", basename(vrts[vrt_n]))
                 parameter <- basename(tools::file_path_sans_ext(vrts[vrt_n]))
                 outfile <- file.path(outfold, paste0(basename(tools::file_path_sans_ext(vrts[vrt_n])), ".tif"))
                 if (parameter %in% c("sos_ordered", "eos_ordered", "pos_ordered")) {
                   ot <- "Int16"
                 }
                 if (parameter %in% c("cumevi_ordered")) {
                   ot <- "Int32"
                 }
                 if (parameter %in% c("totlgt_ordered", "veglgt_ordered", "nseas_ordered")) {
                   ot <- "Byte"
                 }

                 gdalUtils::gdal_translate(vrts[vrt_n],
                                           outfile,
                                           ot        = "Int16",
                                           of        = "GTiff",
                                           co        = "COMPRESS=DEFLATE",
                                           separate  = TRUE,
                                           verbose   = TRUE,
                                           overwrite = TRUE)

                 return(paste0(outfile, " completed ! "))
               }
parallel::stopCluster(clust)
