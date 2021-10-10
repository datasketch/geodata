


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
validate_altids <- function(altids, map_name = NULL){
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
