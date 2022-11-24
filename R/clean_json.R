
clean_json <- function(geoName, geoFile = NULL, geoId, geoProperties, newnamesProperties, jsonName, saveFile = TRUE, savePath = NULL) {
  geoPath <- geoName
  if (!is.null(geoFile)) geoPath <- paste0(geoFile, geoName)
  print(geoPath)
  json_file <- jsonlite::read_json(paste0(geoPath, '.json'))
  leng_json <- length(json_file$objects[[geoName]][["geometries"]])
  print(json_file$objects[[geoName]][["geometries"]][[1]][["properties"]])
  for(i in 1:leng_json){
    json_file$objects[[geoName]][["geometries"]][[i]]$id <- json_file$objects[[geoName]][["geometries"]][[i]][["properties"]][[geoId]]
    json_file$objects[[geoName]][["geometries"]][[i]][["properties"]] <- json_file$objects[[geoName]][["geometries"]][[i]][["properties"]][geoProperties]
    names(json_file$objects[[geoName]][["geometries"]][[i]][["properties"]]) <- newnamesProperties
    json_file
  }

  json_file <- rjson::toJSON(json_file)

  if (saveFile) {
    if (is.null(savePath)) {
      writeLines(json_file,(paste0(path.package("geodata"),"/inst/geodata/", geoName, "/", jsonName, ".topojson")))
    } else {
      writeLines(json_file, paste0(savePath, "/" , jsonName, ".topojson"))
    }
  } else {
    json_file
  }


}


centroids_json <- function(map_name, write = TRUE, ext = "csv") {

  topojsonPath <- geodataTopojsonPath(mapName = map_name)
  tj <- read_sf(topojsonPath)
  df <- suppressWarnings(st_centroid(tj, of_largest = TRUE))
  df$centroids <- stringr::str_extract(string = st_as_text(df$geometry),
                                       pattern = "(?<=\\().*(?=\\))")
  df <- df %>%
    tidyr::unnest(centroids) %>%
    tidyr::separate(centroids, c("lon", "lat"), sep = " ") %>%
    st_drop_geometry()
 df <- df %>% dplyr::distinct(id, .keep_all = T)
  if (!("name" %in% names(df))) message("geo information without name")
  if (!("name" %in% names(df))) return()
  if (length(unique(df$name)) != nrow(df)) warning("there are repeated geo names")
 print(topojsonPath)
  if (write) {
    if (ext == "csv") {
      readr::write_csv(df, gsub(".topojson", ".csv", topojsonPath))
    } else if (ext == "rds") {
      saveRDS(df, gsub(".topojson", "-centroids.rds", topojsonPath))
    } else {
      stop("format must be csv or rds")
    }
  } else {
    df
  }
}

#change from topojson to rds
# map(seq_along(list.files("inst/geodata/")), function(i) {
#   file_tj <- list.files(paste0("inst/geodata/", list.files("inst/geodata/")[i], "/"), pattern = "topojson")
#   map(file_tj, function(j) {
#   tj <- read_lines(paste0("inst/geodata/", list.files("inst/geodata/")[i], "/", j))
#   saveRDS(tj, paste0("inst/geodata/", list.files("inst/geodata/")[i], "/",  gsub(".topojson", "", j), ".rds"))
#   })
# })


