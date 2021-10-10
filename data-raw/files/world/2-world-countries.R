

library(tidyverse)
devtools::load_all()

map_name <- "world_countries"

file_path <- "data-raw/files/world"
geodata_path <-  "data-raw/geodata/world"

### WORLD

# Make sure topojsons and codes have the same names

codes0 <- read_csv(file.path(file_path, map_name,"aux/world_countries-codes-0.csv"))

topojson_path <- file.path(file_path, map_name, "aux/world-countries.topojson")
tj0 <- read_sf(topojson_path)

tj0_codes <- st_drop_geometry(tj0)

codes <- left_join(tj0_codes, codes0)
check_names_in_topojson(tj0, codes)

write_csv(codes, file.path(file_path, map_name, "world_countries-codes.csv"))



pryr::object_size(tj0)
tj1 <- gd_simplify(tj0, output_size = 6e5)
pryr::object_size(tj1)

write_topojson(tj1, file.path(file_path, map_name,
                              paste0(map_name,".topojson")))
write_topojson(tj1,file.path(geodata_path, map_name,
                             paste0(map_name,".topojson")))

# Write centroids

centroids <- centroids_from_topojson(tj1)
write_csv(centroids, file.path(file_path, map_name,
                               "world_countries-centroids.csv"))
write_csv(centroids, file.path(geodata_path, map_name,
                               paste0(map_name,"-centroids.csv")))

