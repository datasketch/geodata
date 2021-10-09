
test_that("improvised geographic data", {

 # df <- fakeData("col_departments", "name")
 # expect_equal(names(df), c("name", "sample_value"))

})

test_that("guess types in geographic data", {

  # gt <- guess_ftypes(fakeData("col_departments", "name"), map_name = "col_departments")
  # expect_equal(as.vector(gt$hdType), c('Gnm', 'Num'))
  #
  # gt <- guess_ftypes(data = sample_data("Gcd-Cat-Num-Gnm"), map_name = "world_countries")
  # expect_equal(as.vector(gt$hdType), c('Gcd', 'Cat', "Num", "Gnm"))
  #
  # df <- data.frame(ciudad = c("Quindio", "Amazonas"), planta = c("Palma", "Ayahuasca"))
  # gt <- guess_ftypes(df, "col_departments")
  # expect_equal(as.vector(gt$hdType), c('Gnm', 'Cat'))

})
