
#' @export
available_maps <- function(){
  names(geodata:::maps)
}

#' @export
validate_map_name <- function(map_name){
  if(!map_name %in% available_maps())
    stop("map_name not available, check `available_maps()`")
}

#' @export
gd_meta <- function(){
  geodata:::maps
}

#' @export
gd_codes <- function(map_name){
  l <- geodata:::maps[[map_name]]
  l$centroids %>% dplyr::select(id, name)
}

#' @export
gd_altnames <- function(map_name){
  l <- geodata:::maps[[map_name]]
  l$altnames
}

#' @export
gd_altids <- function(map_name){
  l <- geodata:::maps[[map_name]]
  l$altids
}





