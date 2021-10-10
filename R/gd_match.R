
#' @export
gd_match <- function(d){

}



#' @export
gd_match_codes <- function(d, map_name = NULL, col = NULL){
  if(is.null(map_name)){
    stop("Need a map_name to match")
  }
  validate_map_name(map_name)
  col <- parse_col(d, col)

  join_by <- "id"
  names(join_by) <- col

  codes <- gd_codes(map_name) %>% rename_dotdot()
  codes$id <- codes$..gd_id

  if(map_name == "col_departments"){
    d[[col]] <- leading_zeros(d[[col]], 2)
  }

  dplyr::left_join(d, codes, by = join_by)

}

#' @export
gd_match_names <- function(d, map_name = NULL, col = NULL){
  if(is.null(map_name)){
    stop("Need a map_name to match")
  }
  validate_map_name(map_name)
  col <- parse_col(d, col)

  join_by <- "..gd_clean_name"

  codes0 <- gd_codes(map_name) %>% rename_dotdot()
  # Add altnames if they exist
  altnames <- gd_altnames(map_name)
  if(!is.null(altnames)){
    altnames <- altnames %>% purrr::set_names(names(codes0))
    codes <- dplyr::bind_rows(codes0, altnames)
  } else{
    codes <- codes0
  }
  codes$..gd_clean_name <- str_clean(codes$..gd_name)
  codes <- codes %>% dplyr::distinct(..gd_id, ..gd_clean_name, .keep_all = TRUE)

  d$..gd_clean_name <- str_clean(d[[col]])

  match <- dplyr::left_join(d, codes, by = join_by) %>%
    dplyr::select(-..gd_clean_name, -..gd_name)
  # Match again with original codes and not altnames
  match %>% dplyr::left_join(codes0)

}


parse_col <- function(d, col){
  if(is.null(col)){
    col <- names(d)[1]
  }else{
    if(is.numeric(col)) col <- names(d)[col]
    if(!col %in% names(d))
      stop("Column not found in table")
  }
  col
}


