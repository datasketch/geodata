
library(rnaturalearth)

library(sf)


tj <- sf::read_sf("data-raw/files/gtm/municipios.geojson")

x <- st_drop_geometry(tj)


st_drivers()


x <- ne_download(scale = 110, type = 'admin_0_countries', category = 'cultural')
