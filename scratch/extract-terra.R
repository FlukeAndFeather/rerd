devtools::load_all()

cmems_path <- "cmems_mod_glo_phy_my_0.083deg_P1D-m_multi-vars_180.00W-179.92E_80.00S-90.00N_0.49-2865.70m_2021-06-29-2021-06-30.nc"
v <- "thetao"
cmems_rast <- terra::sds(cmems_path)[v]
terra::crs(cmems_rast) <- "EPSG:4326"

r <- cegr_create_lonlat_template(
  lon = c(-127, -117),
  lat = c(32, 40),
  res_deg = 0.25
)

cmems_aoi <- terra::resample(cmems_rast, r)

x <- c(-125, -124)
y <- c(37, 36)
depth <- c(20, 200)
t <- as.POSIXct("2021-06-29 12:00:00", "UTC") + c(0, 24 * 3600)
path <- terra::vect(data.frame(x = x, y = y, z = depth, t = t),
                    geom = c("x", "y"),
                    crs = "EPSG:4326")

path_extracted <- terra::extract(cmems_rast, path) %>%
  cbind(path)

col_depth <- as.numeric(stringr::str_extract(colnames(path_extracted), "depth=(.*)_", 1))
col_time_idx <- as.numeric(stringr::str_extract(colnames(path_extracted), "_(\\d+)$", 1))
col_time <- sort(unique(terra::time(cmems_rast)))[col_time_idx]

nearest_depth <- lapply(path$z, \(.z) {
  zs <- na.omit(unique(col_depth))
  err <- abs(zs - .z)
  closest_z <- zs[err == min(err)][1]
  which(closest_z == col_depth)
})
nearest_time <- lapply(path$t, \(.t) {
  ts <- na.omit(unique(col_time))
  err <- abs(ts - .t)
  closest_t <- ts[err == min(err)][1]
  which(closest_t == col_time)
})
value_column <- sapply(seq(nrow(path_extracted)), \(i) {
  intersect(nearest_depth[[i]], nearest_time[[i]])
})
unlist(path_extracted[cbind(seq(nrow(path_extracted)), value_column)])
