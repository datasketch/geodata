




#' @export
read_validate_meta <- function(path){
  l <- yaml::yaml.load_file(fs::path(path, "meta.yaml"))
  map_name <- fs::path_file(path)
  l$map_name <- map_name

  # Validate centroids
  l$centroids <- read_csv(fs::path(path, l$centroids),
                          col_types = cols(.default = "c"))
  geodata::validate_centroids(l$centroids, map_name)
  # Validate altnames
  if(!is.null(l$altnames)){
    l$altnames <- read_csv(fs::path(path, l$altnames),
                           col_types = cols(.default = "c"))
    geodata::validate_altnames(l$altnames, map_name)
  }
  # Validate altids
  if(!is.null(l$altids)){
    l$altids <- read_csv(fs::path(path, l$altids),
                           col_types = cols(.default = "c"))
    geodata::validate_altids(l$altids, map_name)
  }
  # Validate topojson
  tj <- sf::read_sf(file.path(path, l$topojson$path))
  geodata::validate_topojson(tj,
                             scope = l$topojson$scope,
                             centroids = l$centroids,
                             map_name)
  l$tj <- tj
  l
}


#' @export
validate_centroids <- function(centroids, map_name = NULL){
  if(!all(c("id", "name", "lon", "lat") %in% names(centroids)))
    stop("Centroids with no id, name, lon lat columns: ", map_name)
}

#' @export
validate_altnames <- function(altnames, map_name = NULL){
  if(!all(c("id", "altname") %in% names(altnames)))
    stop("No id and altname: ", map_name)
}

#' @export
validate_altids <- function(altidss, map_name = NULL){
  if(!all(c("id") %in% names(altids)))
    stop("No id and altids: ", map_name)
}

#' @export
validate_topojson <- function(tj, scope, centroids, map_name = NULL){
  if(!all(c("id", "name") %in% names(tj)))
    stop("Topojson mus have id and name for the features: ", map_name)
  if(!all(centroids$name %in% tj$name))
    stop(map_name,"\n   ",
         "Not all centroids name in topojson name: ",
         collapse_char(which_not_in(centroids$name, tj$name)), "\n   ",
         "Not all topojson name in centroids name: ",
         collapse_char(which_not_in(tj$name, centroids$name))
         )
  if(!all(centroids$id %in% tj$id))
    stop("Not all centroids id in topojson id: ", map_name)
}

#' @export
centroids_from_topojson <- function(tj){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  df <- suppressWarnings(st_centroid(tj, of_largest = TRUE))
  centroids <- tibble::as_tibble(sf::st_coordinates(df$geometry)) %>%
    set_names(c("lat", "lon"))
  centroids <- df %>%  st_drop_geometry() %>% bind_cols(centroids)
  centroids
}

#' @export
check_names_in_topojson <- function(tj, codes){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  if(!all(codes$name %in% tj$name) || !all(tj$name %in% codes$name))
    stop("Not all codes name in topojson name: \n",
         collapse_char(which_not_in(codes$name, tj$name)), "\n  ",
         "Not all topojson name in codes name: \n",
         collapse_char(which_not_in(tj$name, codes$name))
    )
}

#' @export
check_names_in_topojson2 <- function(tj, codes){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  tj_list <- tj %>% group_split(zone)
  purrr::walk(tj_list, function(tjj){
    #tjj <- tj_list[[1]]
    current_zone <- unique(tjj$zone)
    message(current_zone)
    zone_codes <- codes %>% dplyr::filter(zone == current_zone)
    check_names_in_topojson(tjj, zone_codes)
  })
}




#' @export
change_topojson_names <- function(tj, new_names, write_path = NULL){
  if(!"tj" %in% class(tj)){
    tj <- read_sf(tj)
  }
  nms <- st_drop_geometry(tj) %>% dplyr::select(name)
  nms <- left_join(nms, new_names, by = "name")
  tj$name <- coalesce(nms$new_name, nms$name)
  if(!is.null(write_path))
    write_topojson(tj, write_path)
  tj
}



#' @export
check_ids_in_topojson <- function(tj, codes){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  if(!all(codes$id %in% tj$id) || !all(tj$id %in% codes$id))
    stop("Not all codes id in topojson id: ",
         collapse_char(which_not_in(codes$id, tj$id)), "\n  ",
         "Not all topojson id in code id: ",
         collapse_char(which_not_in(tj$id, codes$id))
    )
}


#' @export
change_topojson_ids <- function(tj, codes, write_path = NULL){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  tj_codes <- st_drop_geometry(tj) %>% rename(id_old = id)
  tj_codes <- left_join(tj_codes, codes, by = "name")

  # verify all values are joined
  no_match <- tj_codes %>% filter(is.na(id))
  if(nrow(no_match) > 0)
    stop("No match in: ", collapse_char(no_match$name))

  tj2 <- tj
  tj2$id <- tj_codes$id

  if(!is.null(write_path))
    write_topojson(tj, write_path)
  tj2
}


#' @export
write_topojson <- function(tj, path){
  json <- geojsonio::topojson_json(tj)
  readr::write_lines(json, path)
}


#' @export
gd_simplify <- function(tj, output_size = 3e5){
  init_size <- pryr::object_size(tj)
  keep <- 1.2 *output_size/init_size
  tj_out <- rmapshaper::ms_simplify(tj, keep = keep,
                                 keep_shapes = TRUE)
  pryr::object_size(tj_out)
  tj_out
}

