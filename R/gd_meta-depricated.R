

#' @export
availableGeodata <- function(){
  lifecycle::deprecate_stop(when = "1.1", "availableGeodata()",
                            "available_maps()")
  names(geodataMeta())
}

#' @export
geodataMeta <- function(mapName = NULL, load_data = FALSE, debug = FALSE){
  lifecycle::deprecate_stop(when = "1.1", "geodataMeta()",
                            "geodata:::gd_meta()")
  if(!is.null(mapName)) load_data <- TRUE
  dir <- system.file("meta",package="geodata", mustWork=TRUE)
  files <- list.files(dir,pattern = ".*.yaml",full.names = TRUE)
  l <- purrr::map(files,function(x){
    #x <- files[[11]]
    if(debug) message("\n--- ",basename(x))
    ll <- yaml::yaml.load_file(x)
    purrr::map(ll, function(y){
      #y <- ll[[1]]
      y$geoname = basename(file_path_sans_ext(x))
      if(!"basename" %in% names(y))
        stop("No basename in yaml: ", y)
      codesFilename <- system.file(file.path("geodata",y$geoname,paste0(y$basename, ".csv")),package = "geodata")
      if(debug) message("codes: ",codesFilename)
      if(!file.exists(codesFilename)){
        #stop("File ",codesFilename, " does not exist")
        y$codes <- NULL
      }else{
        if(load_data){
          y$codes <- noWrnMsg(readr::read_csv(codesFilename,
                                              col_types = readr::cols(id = 'c', name='c')))
        }else{
          y$codes <- codesFilename
        }
      }

      regionFilename <- file.path("geodata",y$geoname,paste0(y$basename, "-regions.csv"))
      if(file.exists(system.file(regionFilename, package = "geodata"))){
        if(debug) message("regions: ",regionFilename)
        if(load_data){
          y$regions <- noWrnMsg(readr::read_csv(system.file(regionFilename, package = "geodata"),
                                                col_types = readr::cols(id = 'c')))
        }else{
          y$regions <- regionFilename
        }
      }else{
        y$regions <- NULL
      }
      altnamesFilename <- file.path("geodata",y$geoname,paste0(y$basename, "-altnames.csv"))
      if(file.exists(system.file(altnamesFilename, package = "geodata"))){
        if(debug) message("altnames: ", altnamesFilename)
        if(load_data){
          y$altnames <- noWrnMsg(readr::read_csv(system.file(altnamesFilename, package = "geodata"),
                                                 col_types = readr::cols(id = 'c')))
        }else{
          y$altnames <- altnamesFilename
        }
      }else{
        y$altnames <- NULL
      }
      y$codes_name_col <- y$codes_name_col %||% "name"
      y
    })
  }) %>% purrr::flatten()
  if(!is.null(mapName)){
    return(l[[mapName]])
  }
  l
}


#' @export
geodataCodes <- function(mapName = NULL, load_data = FALSE){
  lifecycle::deprecate_stop(when = "1.1", "geodataCodes()",
                            "geodata:::gd_codes()")
  dm <- geodataMeta(mapName, load_data = load_data)
  dm$codes
}



#' @export
geodataAltnames <- function(mapName = NULL, load_data = FALSE){
  lifecycle::deprecate_stop(when = "1.1", "geodataAltnames()",
                            "geodata:::gd_altnames()")
  dm <- geodataMeta(mapName, load_data = load_data)
  dm$altnames
}

#' @export
gd_tj <- function(map_name){
  l <- geodata:::maps[[map_name]]
  sf::st_read(l$topojson$path)
}


#' @export
geodataPolygon <- function(mapName = NULL){
  lifecycle::deprecate_stop(when = "1.1", "geodataAltnames()",
                            "geodata:::gd_tj()")
  dm <- geodataMeta(mapName, load_data = load_data)
  path <- file.path("geodata", dm$geoname, paste0(dm$basename,".topojson"))
  dm$centroids <- file.path("geodata", dm$geoname, paste0(dm$basename, ".csv"))
  tj <- topojson_read(system.file(path, package = "geodata"))
  data_map <- ggplot2::fortify(tj) %>% dplyr::mutate(.id = as.numeric(id)) %>%
    dplyr::select(-id)
  data_info <- tj@data %>% mutate(.id = 0:(nrow(.) - 1))
  left_join(data_map, data_info)
}

#' @export
geodataProjections <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodataProjections()",
                            "geodata:::gd_tj()")
  l <- geodataMeta(mapName)
  names(l$projections)
}

#' @export
geodataProjectionOptions <- function(mapName, projection, withDefaults = TRUE){
  lifecycle::deprecate_stop(when = "1.1", "geodataProjectionOptions()",
                            "NULL")
  l <- geodataMeta(mapName)
  if(!projection %in% names(l$projections))
    stop(mapName, "does not support this projection")
  projection <- l$projections[[projection]]
  if(!withDefaults) return(names(projection))
  projection
}

#' @export
geodataCsv <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodataCsv()",
                            "geodata:::gd_codes()")
  geodataMeta(mapName)$codes
}

#' @export
geodataTopojsonPath <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodataTopojsonPath()",
                            "geodata:::gd_tj()")
  y <- geodataMeta(mapName, load_data = FALSE)
  system.file(file.path("geodata",y$geoname,paste0(y$basename, ".topojson")),package = "geodata")
}

#' @export
geodataCsvPath <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodataCsvPath()",
                            "geodata:::gd_codes()")
  y <- geodataMeta(mapName, load_data = FALSE)
  system.file(file.path("geodata",y$geoname,paste0(y$basename, ".csv")),package = "geodata")
}


#' @export
geodataRdsPath <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodataRdsPath()",
                            "geodata:::gd_tj()")
  y <- geodataMeta(mapName, load_data = FALSE)
  system.file(file.path("geodata",y$geoname,paste0(y$basename, "-centroids.rds")),package = "geodata")
}


#' @export
geoinfo <- function(mapName) {
  lifecycle::deprecate_stop(when = "1.1", "geoinfo()",
                            "geodata:::gd_meta()")
  dir <- system.file("meta",package="geodata", mustWork=TRUE)
  files <- list.files(dir,pattern = ".*.yaml",full.names = TRUE)
  l <- purrr::map(files,function(x){
    ll <- yaml::yaml.load_file(x)
    purrr::map(ll, function(y){
      geoprep <- NULL
      geoprep$geoname <- basename(file_path_sans_ext(x))
      geoprep$basename <- y$basename
      geoprep
    })
  }) %>% purrr::flatten()

  geoprep <- l[[mapName]]
  centroids_rds <- readr::read_rds(file =
                                     system.file(
                                       file.path("geodata", geoprep$geoname,paste0(geoprep$basename, "-centroids.rds")),
                                       package = "geodata"
                                     )
  )
  topo_sf <- sf::read_sf(dsn =
                           system.file(
                             file.path("geodata", geoprep$geoname,paste0(geoprep$basename, ".topojson")),
                             package = "geodata"
                           ))
  topo_rds <- readr::read_rds(file =
                                system.file(
                                  file.path("geodata", geoprep$geoname,paste0(geoprep$basename, ".rds")),
                                  package = "geodata"
                                ))

  list (
    centroids = centroids_rds,
    geo_sf = topo_sf,
    geo_rds = topo_rds
  )
}

