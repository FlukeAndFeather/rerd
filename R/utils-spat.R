#' Create a raster template
#'
#' Create a common grid across variables for data extraction and prediction.
#'
#' @param lon `[dbl(2)]` Longitudinal extent, xmin and xmax.
#' @param lat `[dbl(2)]` Latitudinal extent, ymin and ymax.
#' @param crs `[chr(1)]` proj4 string describing projection of raster template.
#' @param res_km `[dbl(1)]` Resolution of raster template in km.
#'
#' @return `[terra::SpatRaster]`
#' @export
#'
#' @examples
#' # A CRS appropriate for the California Current
#' # Specifically, an oblique Lambert azimuthal equal area projection centered
#' # on 36N, 122W
#' crs <- "+proj=laea +lat_0=36 +lon_0=-122 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#' # A raster template roughly covering the California Current from San Diego to
#' # Arcata at a 10km horizontal resolution.
#' r <- cegr_create_template(
#'   lon = c(-127, -117),
#'   lat = c(32, 40),
#'   crs = crs,
#'   res_km = 10
#' )
#' r
cegr_create_template <- function(lon, lat, crs, res_km) {
  template_ext <- terra::rast(
    terra::ext(lon[1], lon[2], lat[1], lat[2]),
    crs = "EPSG:4326"
  ) %>%
    terra::project(crs) %>%
    terra::ext()
  result <- terra::rast(
    template_ext,
    crs = crs
  )
  res <- res_km * 1000 / terra::linearUnits(result)
  terra::res(result) <- res
  result
}

copernicus_to_rast <- function(copernicus_path, var_id) {
  coper_rast <- suppressWarnings(terra::rast(copernicus_path))
  terra::set.crs(coper_rast, "EPSG:4326")
  terra::set.ext(coper_rast, c(-180, 180, -90, 90))
  coper_rast <- terra::flip(coper_rast, "vertical")
  coper_rast[[var_id]]
}
