## code to prepare `cegr_datasets` dataset goes here

# Recursive function to make terminal elements of a hierarchical list into paths
# to their values. Necessary because display names of variables may not match
# internal variable names.
# E.g.,
# cegr_datasets$annex$satellite$`Ocean physics model`$nrt$mld = "mlotst"
# becomes:
# cegr_datasets$annex$satellite$`Ocean physics model`$nrt$mld = "annex:satellite:Ocean physics model:nrt:mlotst"
list_as_paths <- function(l, path = NULL) {
  if (is.character(l[[1]])) {
    result <- as.list(paste(path, l, sep = ":"))
    names(result) <- names(l)
    result
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
    ROMS = list(
      bbv = "bbv",
      curl = "curl",
      ild_05 = "ild_05",
      ssh = "ssh",
      sst = "sst",
      su = "su",
      sustr = "sustr",
      sv = "sv",
      svstr = "svstr")
  ),
  annex = list(
    satellite = list(
      `Sea surface temperature` = list(
        nrt = list(
          sst = "analysed_sst",
          sea_ice = "sea_ice_fraction"
        ),
        historical = list(
          sst = "analysed_sst",
          sea_ice = "sea_ice_fraction"
        )
      ),
      Altimetry = list(
        nrt = list(
          adt = "adt",
          sla = "sla",
          ugos = "ugos",
          ugosa = "ugosa",
          vgos = "vgos",
          vgosa = "vgosa"
        ),
        historical = list(
          adt = "adt",
          sla = "sla",
          ugos = "ugos",
          ugosa = "ugosa",
          vgos = "vgos",
          vgosa = "vgosa"
        )
      ),
      `Ocean physics model` = list(
        nrt = list(
          mld = "mlotst",
          salinity = "so",
          temperature = "thetao",
          ice_thick = "sithick",
          ice_conc = "siconc",
          ssh = "zos",
          uo = "uo",
          vo = "vo"
        ),
        historical = list(
          mld = "mlotst",
          salinity = "so",
          temperature = "thetao",
          ice_thick = "sithick",
          ice_conc = "siconc",
          ssh = "zos",
          uo = "uo",
          vo = "vo"
        )
      ),
      `Biogeochemistry ocean model` = list(
        nrt = list(
          o2 = "o2",
          pp = "nppv",
          chl_a = "chl",
          nitrate = "no3",
          phosphate = "po4",
          si = "si"
        ),
        historical = list(
          o2 = "o2",
          pp = "nppv",
          chl_a = "chl",
          nitrate = "no3",
          phosphate = "po4",
          si = "si"
        )
      ),
      Chla = list(
        nrt = list(
          chl_a = "CHL",
          chl_a_err = "CHL_uncertainty"
        ),
        historical = list(
          chl_a = "CHL",
          chl_a_err = "CHL_error"
        )
      )
    ),
    static = list(
      Bathymetry = list(bathy = "altitude")
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
