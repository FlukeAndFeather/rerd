#' Read CEG-approved data sources
#'
#' Note that you **must** provide a template raster for extraction. This may
#' seem like an extraneous step, but it's the only way to ensure data extraction
#' and model prediction happen on the same grid.
#'
#' @param cegr_var `[chr(1)]` Variable id, see `cegr_datasets`
#' @param lon `[dbl(n)]` Longitude
#' @param lat `[dbl(n)]` Latitude
#' @param t `[dbl(n)]` Time
#' @param rast_template `[terra::SpatRaster]` Template raster (see
#'   `\link{cegr_create_template}`)
#' @param depth `[dbl(n)]` Depth (defaults to NA)
#'
#' @return `[dbl(n)]`
#' @export
#'
#' @examples
#' # Raster template
#' california_current_crs <- "+proj=laea +lat_0=36 +lon_0=-122 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#' r <- cegr_create_template(
#'   lon = c(-127, -117),
#'   lat = c(32, 40),
#'   crs = california_current_crs,
#'   res_km = 10
#' )
#' # Read one ROMS value
#' cegr_read(cegr_datasets$supercomputer$ROMS$bbv,
#'           -125, 37, as.POSIXct("2020-01-01", "UTC"), r)
#' # Read multiple ROMS values
#' cegr_read(cegr_datasets$supercomputer$ROMS$bbv,
#'           seq(-125, -130, length.out = 10),
#'           seq(37, 40, length.out = 10),
#'           seq(as.POSIXct("2020-01-01", "UTC"),
#'               as.POSIXct("2020-12-01", "UTC"),
#'               length.out = 10),
#'               r)
#' # Read from a CMEMS dataset and convert temperature to celsius
#' cegr_read(cegr_datasets$annex$satellite$`Sea surface temperature`$nrt$analysed_sst,
#'           -125, 37, as.POSIXct("2020-01-01", "UTC"), r) %>%
#'   cmems_to_celsius()
cegr_read <- function(cegr_var, lon, lat, t, rast_template,
                      focal_stat = NULL, focal_win = NULL, depth = NA) {
  stopifnot(is_path_valid(cegr_var))

  read_fun <- if (grepl("ROMS", cegr_var)) {
    read_roms
  } else {
    read_satellite
  }

  read_fun(cegr_var, lon, lat, t, rast_template, focal_stat, focal_win, depth)
}

read_roms <- function(cegr_var, lon, lat, t, rast_template, focal_stat, focal_win, depth) {
  # Locate ROMS variable file
  var_split <- strsplit(cegr_var, ":")[[1]]
  roms_var <- var_split[3]
  roms_path <- cegr_paths[[paste(var_split[1:2], collapse = ":")]]
  roms_var_path <- dir(roms_path, pattern = roms_var, full.names = TRUE)

  # Read dimensions and data
  roms_nc <- ncdf4::nc_open(roms_var_path)
  roms_lon <- ncdf4::ncvar_get(roms_nc, "lon_rho")[, 1]
  roms_lat <- ncdf4::ncvar_get(roms_nc, "lat_rho")[1, ]
  roms_time <- ncdf4::ncvar_get(roms_nc, "time") %>%
    as.POSIXct(tz = "UTC", origin = "2011-1-2")
  roms_data <- ncdf4::ncvar_get(roms_nc, names(roms_nc$var)[3])
  ncdf4::nc_close(roms_nc)
  # ROMS data has the lon and lat dimension flipped from what terra::rast
  # expects and the lat are in the reverse order
  # aperm() is like t() but generalized to arrays
  roms_data <- aperm(roms_data, c(2, 1, 3))
  roms_rast <- terra::rast(
    ncols = length(roms_lon),
    nrows = length(roms_lat),
    nlyrs = length(roms_time),
    xmin = min(roms_lon),
    xmax = max(roms_lon),
    ymin = min(roms_lat),
    ymax = max(roms_lat),
    crs = "EPSG:4326",
    vals = roms_data
  ) %>%
    terra::flip(direction = "vertical")
  # Apply focal statistic, if specified
  if (!is.null(focal_stat)) {
    roms_rast <- terra::focal(roms_rast, w = focal_win, fun = focal_stat)
  }
  # Project and align to raster template
  roms_rast <- terra::project(roms_rast, rast_template,
                              method = "bilinear", align = TRUE)

  # Extract data
  time_idx <- find_nearest(t, roms_time)
  loc_vect <- terra::vect(cbind(lon, lat), crs = "EPSG:4326") %>%
    terra::project(roms_rast)
  terra::extract(roms_rast, loc_vect, layer = time_idx)$value
}

read_satellite <- function(cegr_var, lon, lat, t, rast_template, focal_stat, focal_win, depth) {
  # Locate satellite product directory (drop final component of cegr_var, which
  # is the variable within the product)
  product_id <- sub("(.*):[^:]+$", "\\1", cegr_var)
  product_dir <- cegr_paths[[product_id]]

  # Extract variable id (final component of cegr_var)
  var_id <- substr(cegr_var, nchar(product_id) + 2, nchar(cegr_var))

  # Split request by date (split-apply-combine approach)
  request_by_date <- dplyr::tibble(lon, lat, t, depth, i = seq(length(lon))) %>%
    dplyr::mutate(t_year = format(t, "%Y"),
                  t_month = format(t, "%m"),
                  t_ymd = format(t, "%Y%m%d")) %>%
    dplyr::group_by(t_year, t_month, t_ymd) %>%
    dplyr::group_split()

  # Apply single-day data extraction
  daily_results <- lapply(request_by_date, \(one_day) {
    # Find day's netCDF file
    ym_dir <- file.path(product_dir, one_day$t_year, one_day$t_month)
    if (!dir.exists(ym_dir)) {
      stop(stringr::str_glue("No directory found at {ym_dir}. Are you sure you're on the Annex computer?"))
    }
    ymd_path <- dir(ym_dir, one_day$t_ymd, full.name = TRUE)
    # Convert to raster
    ymd_rast <- copernicus_to_rast(ymd_path, var_id)
    # Apply focal statistic, if specified
    if (!is.null(focal_stat)) {
      ymd_rast <- terra::focal(ymd_rast, w = focal_win, fun = focal_stat)
    }
    # Project raster
    ymd_proj <- terra::project(ymd_rast, rast_template)
    # Extract data
    result <- cbind(one_day$lon, one_day$lat) %>%
      terra::vect(crs = "EPSG:4326") %>%
      terra::extract(ymd_proj, .)
    # Return result
    one_day$result <- result[[2]] # 1 is ID, 2 is result
    one_day
  })

  # Combine daily requests and return result
  purrr::list_rbind(daily_results) %>%
    dplyr::arrange(i) %>%
    dplyr::pull(result)
}

find_nearest <- function(x, y) {
  sapply(x, \(.x) which.min(abs(as.numeric(.x - y))))
}
