is_path_valid <- function(p) {
  if (is.character(p) && length(p) == 1) {
    path_parts <- strsplit(p, ":")[[1]]
    result = Reduce(getElement, path_parts, init = cegr_datasets)
    result == p
  } else {
    FALSE
  }
}
