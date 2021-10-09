
# fake data
#' @export
fakeData <- function(map_name = NULL, by = "name", ...) {
  lifecycle::deprecate_stop(when = "1.1", "fakeData()",
                            "gd_sample()")
  if (is.null(map_name)) return()
  centroids <- suppressWarnings(geodataMeta(map_name)$codes)
  names_centroids <- names(centroids)
  diff_names <- setdiff(names_centroids, c("id", "name", "lat", "lon"))
  nsample <- nrow(centroids)
  if (nsample > 30) nsample <- 30
  centroids <- centroids[sample(1:nrow(centroids), nsample),]
  if (by == "name" & !(identical(diff_names, character()))) {
    d <- data.frame(name = centroids[[by]],
                    name_addition = centroids[[diff_names]], sample_value = abs(runif(nsample, 33, 333)),
                    stringsAsFactors = FALSE)
  } else {
    d <- data.frame(name = sample(centroids[[by]], nsample), sample_value = abs(runif(nsample, 33, 333)),
                    stringsAsFactors = FALSE)
  }
  d
}


#' standar dataset
#' @export
standar_values <- function(data) {
  lifecycle::deprecate_stop(when = "1.1", "fakeData()",
                            "gd_sample()")
  l <- purrr::map(colnames(data), function(i) as.character(iconv(tolower(data[[i]]), to = "ASCII//TRANSLIT")))
  names(l) <- names(data)
  l <- l %>% dplyr::bind_rows()
  l
}

# find geo code or geo name
#' @export
find_geoinfo <- function(data, centroids) {
  lifecycle::deprecate_stop(when = "1.1", "fakeData()",
                            "gd_match()")
  centroids <- centroids %>% dplyr::select(-lat, -lon)
  centroids <- standar_values(centroids)
  dic_info <- data.frame(names_centroids = c("id", "name", "name_addition", "code_addition"),
                         ftype = c("Gcd", "Gnm", "Gnm", "Gcd"), stringsAsFactors = FALSE)

  info_data <- paste0("^", purrr::map(colnames(centroids),
                                      function (i) {unique(centroids[[i]])
                                      }) %>% unlist(), "$", collapse = "|")

  data <- standar_values(data)


  l <- sapply(colnames(data), function(x) {
    search_info <- !identical(grep(info_data, as.matrix(data[,x])), integer(0)) == TRUE
  })
  r <- names(l)[l == TRUE]
  if (identical(r, character(0))) {
    return() # if is null is not finding consciousness
  } else{
    r # return names of date with geo code o geoname
  }

}


# guess ftypes changed cat by Gnm or Gcd
#' @importFrom dplyr %>%
#' @export
guess_ftypes <- function(data, map_name) {
  lifecycle::deprecate_stop(when = "1.1", "fakeData()",
                            "gd_sample()")
  #data <- sample_data("Glt-Gln-Num-Cat-Num-Num-Cat")
  if (is.null(map_name))
    stop("Please type a map name")
  if (is.null(data)) return()

  f <- homodatum::fringe(data)
  d <- homodatum::fringe_d(f)
  dic <- homodatum::fringe_dic(f)
  dic$id <- names(d)

  centroids <- suppressWarnings(geodataMeta(map_name)$codes)
  centroids$id <- iconv(tolower(centroids$id), to = "ASCII//TRANSLIT")
  dif_names <- setdiff(names(centroids), c("id", "name", "lat", "lon"))


  if (!identical(dif_names, character())) {
    col_names <- c("name", dif_names)
    centroids$name <- iconv(tolower(centroids$name), to = "ASCII//TRANSLIT")
    centroids[[dif_names]] <- iconv(tolower(centroids[[dif_names]]), to = "ASCII//TRANSLIT")
  } else {
    col_names <- c("name")
    centroids$name <- iconv(tolower(centroids$name), to = "ASCII//TRANSLIT")
  }
  var_geo <- find_geoinfo(data = as.data.frame(data), centroids)
  d <- data[var_geo]
  d <- standar_values(d)

  info_gcd <- paste0("^", centroids$id, "$", collapse = "|")
  l <- sapply(colnames(d), function(x) {
    search_info <- !identical(grep(info_gcd, as.matrix(d[,x])), integer(0)) == TRUE
  })
  r <- names(l)[l == TRUE]

  if (!identical(r, character(0))) {
    if(!all(is.na(suppressWarnings(as.numeric(centroids$id))))) {
      max_gcd <- max(centroids$id)
      d_gcd <- r %>%
        purrr::map(function(i){max(d[[i]], na.rm = TRUE) <= max(centroids$id)}) %>% unlist()
      r <- r[d_gcd]
    }
    ld<- map(r, function(i) {
      dic$hdType[dic$label == i] <<- "Gcd"
    })
  }


  info_gnm <- paste0("^", purrr::map(col_names,
                                     function (i) {unique(centroids[[i]])
                                     }) %>% unlist(), "$", collapse = "|")
  l <- sapply(colnames(d), function(x) {
    search_info <- !identical(grep(info_gnm, as.matrix(d[,x])), integer(0)) == TRUE
  })
  r <- names(l)[l == TRUE]

  if (!identical(r, character(0))) {
    ld<- purrr::map(r, function(i) {
      dic$hdType[dic$label == i] <<- "Gnm"
    })
  }

  dic

}
