create_vrt_mosaics <- function(mosaics_folder, pattern) {

  in_files <- list.files(mosaics_folder, pattern = pattern, full.names = T)
  names <- tibble::tibble(fullname = in_files,
                          shortname = basename(tools::file_path_sans_ext(in_files)))
  split_names <- stringr::str_split_fixed(names$shortname, "_", 3)
  names <- cbind(names, split_names, or_order = seq(1:length(names$fullname))) %>%
    `colnames<-`(c("fullname", "basename", "var", "season", "year", "order")) %>%
    dplyr::arrange(year, season )
  ordered_files <- names$fullname[1:5]

  vrt_file <- file.path(mosaics_folder, "vrts", paste0(pattern, "_ordered.vrt"))
  dir.create(dirname(vrt_file))
  gdalUtils::gdalbuildvrt(ordered_files, vrt_file, separate = T, overwrite = T)

}




