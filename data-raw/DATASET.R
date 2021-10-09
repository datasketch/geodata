
library(fs)
library(tidyverse)
library(readr)

devtools::load_all()

dirs <- fs::dir_ls("data-raw/geodata", recurse = 1, type = "directory",
                   regexp = "/.*/.*/")
dirs

map_names <- basename(dirs)

path <- dirs[1]
read_validate_meta(path)

l <- map(dirs, function(x){
  ## Read and validate metadata
  l <- read_validate_meta(x)
  l
}) %>% purrr::set_names(map_names)

maps <- l
pryr::object_size(maps)
usethis::use_data(maps, internal = TRUE, overwrite = TRUE)
