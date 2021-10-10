test_that("Matching geocodes and geonames work",{

  map_name <- "col_departments"

  d <- data.frame(cod_dane = c("11", "05", "20","500"))
  gd_match_codes(d, map_name = map_name)


  d <- data.frame(depto = c("Bogotá", "Antioquia", "Chocó"))
  gd_match_names(d, map_name = map_name)


  map_name <- "world_countries"
  d <- data.frame(country = c("Cabo verde", "Congo (Kinshasa)",
                              "Congo (Brazzaville)", "Eswatini","Holy See"))
  gd_match_names(d, map_name = map_name, col = NULL)



})




test_that("Name and id match works", {

  # some_nms <- c("nombre","ciudad","edad")
  # col <- which_geoname_column(some_nms)
  #
  #
  # d <- dplyr::tribble(
  #   ~ciudad,~value,
  #   "Bogotá",1,
  #   "Pasto",2,
  #   "Buga",3
  # )
  #
  # geocode(d, mapName = "col_municipalities")


  # d <- data.frame(country = c("Cabo verde", "Congo (Kinshasa)","Congo (Brazzaville)", "Eswatini","Holy See"))
  # geocode(d, mapName = "world_countries")

})
