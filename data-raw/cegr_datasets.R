## code to prepare `cegr_datasets` dataset goes here

# Recursive function to make terminal elements of a hierarchical list into paths
# to themselves (e.g. foo$bar = "a" becomes foo$bar$a = foo:bar:a)
list_as_paths <- function(l, path = NULL) {
  if (is.null(names(l))) {
    paths <- as.list(paste(path, l, sep = ":"))
    magrittr::set_names(paths, l)
  } else {
    paths <- if(is.null(path)) {
      names(l)
    } else {
      paste(path, names(l), sep = ":")
    }
    mapply(list_as_paths, l, paths, SIMPLIFY = FALSE)
  }
}

cegr_datasets <- list(
  supercomputer = list(
    ROMS = c("bbv", "curl", "ild_05", "ssh", "sst", "su", "sustr", "sv", "svstr")
  ),
  annex = list(
    satellite = list(
      `Sea surface temperature` = list(
        nrt = c("analysed_sst", "sea_ice_fraction"),
        historical = c("analysed_sst", "sea_ice_fraction")
      ),
      Altimetry = list(
        nrt = c("adt", "sla", "ugos", "ugosa", "vgos", "vgosa"),
        historical = c("adt", "sla", "ugos", "ugosa", "vgos", "vgosa")
      ),
      `Ocean physics model` = list(
        nrt = c("mlotst", "so", "thetao", "sithick", "siconc", "zos", "uo", "vo"),
        historical = c("mlotst", "so", "thetao", "sithick", "siconc", "zos", "uo", "vo")
      ),
      `Biogeochemistry ocean model` = list(
        nrt = c("o2", "pp", "chl", "fe", "nitrate", "ph", "po4", "si", "co2"),
        historical = c("o2", "pp", "chl", "nitrate", "po4", "si")
      ),
      Chla = list(
        nrt = "chla",
        historical = "chla"
      )
    ),
    static = list(
      Bathymetry = "bathy"
    )
  )
) %>%
  list_as_paths()

cegr_paths <- readLines("data-raw/paths.txt") %>%
  strsplit("=") %>%
  sapply(\(x) {
    result <- list(x[2])
    names(result) = x[1]
    result
  })

usethis::use_data(cegr_datasets, overwrite = TRUE)
usethis::use_data(cegr_paths, overwrite = TRUE, internal = TRUE)
