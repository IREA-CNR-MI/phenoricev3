subset_phenorice <- function(in_tiff, mask, out_rast) {

  out <- sprawl::mask_rast(in_tiff, mask, to_file = T, out_rast = out_rast, crop = TRUE)

}

library(sprawl)
library(sf)

mosaic_folder <- "/home/lb/nr_working/shared/PhenoRice/Asia/Data/mosaics/ordered"
out_folder <- "/home/lb/nr_working/shared/PhenoRice/Asia/Data/mosaics/ordered/subsets"

in_tiffs <- list.files(mosaic_folder, pattern = ".tif", full.names = TRUE)

subset_name <- "Nueva_Ecijia"

in_country = "PHL"
boundaries <- get_boundaries(in_country, level = 1) %>%
  sf::st_as_sf() %>%
  dplyr::filter(NAME_1 == "Nueva Ecija")


for (file in seq_along(in_tiffs)) {
  in_rast   <- raster::stack(in_tiffs[file])
  out_tiff  <- file.path(out_folder, subset_name, basename(in_tiffs[file]))
  dir.create(dirname(out_tiff))
  out_tiff  <- sprawl::mask_rast(in_rast,
                                 boundaries,
                                 to_file = T,
                                 out_rast = out_tiff,
                                 crop = TRUE)
}


a = "/home/lb/my_data/prasia/mosaics/ordered/subsets/Nueva_Ecijia/sos_ordered.tif"
