
#' @export
available_maps <- function(){
  names(geodata:::maps)
}

#' @export
valid_map_name <- function(map_name){
  map_name %in% available_maps()
}

#' @export
gd_meta <- function(){
  geodata:::maps
}

#' @export
gd_codes <- function(map_name){
  l <- geodata:::maps[[map_name]]
  l$centroids %>% select(id, name)
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





