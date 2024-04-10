#' Convert temperatures in CMEMS data to celsius
#'
#' CMEMS data stores temperatures in units of "Kelvin times 100". This converts
#' those values to celsius.
#'
#' @param cmems_temperature `[dbl(n)]` A vector of CMEMS temperatures
#'
#' @return `[dbl(n)]` Temperatures in celsius
#' @export
#'
#' @examples
#' # Only works on Annex computer
#' california_current_crs <- "+proj=laea +lat_0=36 +lon_0=-122 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#' r <- cegr_create_template(
#'   lon = c(-127, -117),
#'   lat = c(32, 40),
#'   crs = california_current_crs,
#'   res_km = 10
#' )
#' # Get the sea surface temperature off the coast of California in January
#' cmems_temp <- cegr_read(cegr_datasets$annex$satellite$`Sea surface temperature`$nrt$analysed_sst,
#'                         -125, 37, as.POSIXct("2020-01-01", "UTC"), r)
#' cmems_temp
#' # 1389 degrees isn't useful for most applications
#' cmems_to_celsius(cmems_temp)
#' # 11.16 is a bit more useful
cmems_to_celsius <- function(cmems_temperature) {
  (cmems_temperature - 273.15) * 0.01
}
