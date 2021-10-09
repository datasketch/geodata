#' @export
geocode <- function(d, col = NULL, mapName = NULL,
                    result = c("code", "latlon", "all")){
  lifecycle::deprecate_stop(when = "1.1", "geocode()",
                            "gd_match()")
  if(is.null(col)){
    geoname <- which_geoname_column(names(d))
    col <- geoname$column
  }
  if(is.null(mapName)){
    mapName <- geoname$mapName
  }
  # dgeo <- d[col] %>% setNames("_geoname")
  codes <- geodataCodes(mapName = mapName)
  alt_names <- geodataAltnames(mapName = mapName)
  meta <- geodataMeta(mapName = mapName, load_data = FALSE)

  ## Fill id
  codes_col <- meta$codes_name_col
  did <- complete_geoid(d, name_col = col,
                      codes = codes, codes_col = meta$codes_name_col,
                      alt_names = alt_names)
  dgeo <- dplyr::bind_cols(d,did) %>% dplyr::left_join(codes)
  dgeo
}

complete_geoid <- function(d, name_col, codes, codes_col, alt_names){
  lifecycle::deprecate_stop(when = "1.1", "fakeData()",
                            "gd_match()")
  dname <- d[name_col]
  names(alt_names) <- c("id", codes_col)
  cods <- codes %>% dplyr::select(one_of("id",codes_col)) %>%
    dplyr::bind_rows(alt_names) %>%
    purrr::set_names(c("id", "..name")) %>%
    dplyr::filter(!is.na(id)) %>%
    dplyr::mutate(..name = clean_string(..name)) %>%
    dplyr::select(2,1)
  data.frame(id = match_replace(clean_string(d[[name_col]]), cods), stringsAsFactors = FALSE)
}

#' Which geoname column
#'
#' @param colnames A vector of data.frame names.
#' @param colname_variations A vector of custom names to append to the vector
#'   of frequent colnames for geo names.
#' @param show_guess Show message with the guessed column.
#' @return A single colname with the match of common geo name columns.
#'
#' @examples
#' which_name_column(c("Name", "Age", "City"))
#'
#' @export
which_geoname_column <- function(colnames, colname_variations = NULL, show_guess = FALSE){
  lifecycle::deprecate_stop(when = "1.1", "which_geoname_column()",
                            "gd_match()")
  common_geonames <- system.file("geonames/common_geonames.csv", package = "geodata")
  common_geonames <- readr::read_csv(common_geonames)
  name_cols <- c(common_geonames$name, colname_variations)
  names(colnames) <- tolower(colnames)
  col <- which_in(name_cols, names(colnames))
  col <- unname(colnames[col])
  if(show_guess) message("Guessed names column: ", col)
  if(length(col) == 0) return()
  if(length(col) > 1) warning("Found multiple gender column candidates: ",paste(col, collapse = ", "),
                              ". Using column",col[1])
  list(column = col,
       mapName = common_geonames %>%
         dplyr::filter(name %in% col) %>%
         dplyr::pull(mapName)
  )
}


clean_string <- function(x){
  trim_punct(remove_accents(tolower(x)))
}





