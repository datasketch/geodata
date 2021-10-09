filter_json <- function(map_name = NULL, col_filter = "name", data_filter = NULL, object_name = NULL) {



  geo_available <- suppressWarnings(availableGeodata())
  if (is.null(map_name) | !(map_name %in% geo_available)) stop("You must type a map_name, view availableGeodata()")
  topo_file <-  suppressWarnings(geodata::geodataTopojsonPath(mapName = map_name))
  geodata <- st_read(topo_file)
  nms_geo <- names(geodata)
  if (!(col_filter %in% nms_geo)) stop(paste0("you are entering an unknown column, the column to filter can be: ",
                                              paste0(nms_geo, collapse = ", ")))

  info_filter <- unique(geodata[[col_filter]])
  if (sum(data_filter %in% info_filter) == 0) stop(paste0("there are no matches, make sure you enter one of the following values"),
                                                  paste0(info_filter, collapse = ", "))
  if (length(data_filter %in%  info_filter) < length(data_filter)) warning("Some categories were not joined")

  geofilter <- geodata[geodata[[col_filter]] != data_filter, ]
  geofilter <- geofilter %>% sf::st_as_sf()
  if (is.null(object_name)) object_name <- paste0(map_name, "_filter")
  st_write(geofilter, dsn = paste0(object_name, ".json"), driver = "GeoJSON")
}


#filter_json(map_name = "usa_counties", col_filter = "depto", data_filter = "California", object_name = "california_map")
