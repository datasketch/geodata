
library(tidyverse)
devtools::load_all()


file_path <- "data-raw/files/gtm"
geodata_path <-  "data-raw/geodata/gtm"

### MUNICIPIOS
# Prepater municipios geojson
## datos de https://guilles.website/2017/03/17/datos-gis-del-ign/
tj0 <- sf::read_sf(file.path(file_path, "aux/municipios.geojson"))


tj0 <- st_collection_extract(tj0,type = c("POLYGON", "POINT", "LINESTRING"))
write_topojson(tj0, file.path(file_path, "aux/tmp.topojson"))

tj <- tj0

tj$stroke.opacity <- NULL
tj$AREA_KM._ <-NULL
tj$fill <- NULL
tj$Style <- NULL
tj$stroke.width <- NULL
tj$PERIMETR_1 <- NULL
tj$fill.opacity <- NULL
tj$stroke <- NULL
tj$fill.opacity <- NULL

names(tj)
tj0 <- tj %>% dplyr::filter(!COD_MUNI_1 %in% c(0, 2000))
tj1 <- tj0 %>%
  dplyr::rename(zone = DEPTO_1, zone_id = COD_DEPT_1,
                id = COD_MUNI_1, name = NOMBRE_1) %>%
  select(id, name, everything()) %>%
  arrange(zone, name)

pryr::object_size(tj1)
tj2 <- gd_simplify(tj1)
pryr::object_size(tj2)

write_topojson(tj2, file.path(file_path, "aux/gtm_municipios-0.topojson"))

# Codes

codes <- st_drop_geometry(tj1)
codes <- codes %>% arrange(zone, name)


codes_deptos <- codes %>%
  select(id = zone_id, name = zone) %>% distinct()
write_csv(codes_deptos, file.path(file_path, "gtm_departments-codes.csv"))

codes_munis <- codes %>% distinct()
write_csv(codes_munis, file.path(file_path, "gtm_municipalities-codes.csv"))

