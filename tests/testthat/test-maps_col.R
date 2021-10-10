test_that("Maps Col", {

  library(tidyverse)

  map_name <- "col_departments"
  d <- data.frame(cod_dane = c(11, 5, 20,500))
  match1 <- gd_match_codes(d, map_name = map_name)

  d <- data.frame(depto =  c("Bogotá", "Antioquia", "Cesar", "Otro"))
  match2 <- gd_match_names(d, map_name = map_name)

  expect_equal(match1 %>% select(-1), match2 %>% select(-1))

})
