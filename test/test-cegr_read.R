library(cegR)

# Annex tests
annex_datasets <- unlist(cegr_datasets$annex)
california_current_crs <- "+proj=laea +lat_0=36 +lon_0=-122 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
r <- cegr_create_template(
  lon = c(-127, -117),
  lat = c(32, 40),
  crs = california_current_crs,
  res_km = 10
)
for (d in annex_datasets) {
  message(sprintf("testing %s...", d))
  tryCatch({
    result <- cegr_read(d, -125, 37, as.POSIXct("2020-01-01", "UTC"), r)
    if (is.numeric(result) && length(result) == 1) message("pass")
    else message("FAIL: result not numeric(1)")
  },
   error = function(e) message(sprintf("FAIL: %s", e$message))
  )
}

foo <- cegr_read(cegr_datasets$annex$satellite$`Ocean physics model`$nrt$salinity,
                 -125, 37, as.POSIXct("2020-01-01", "UTC"), r)
