library(geodata)
# prep colombia con su isla mas grande
geodata:::clean_json(geoName = "colombia",
                     geoFile = "inst/example/",
                     geoId = "DPTO",
                     geoProperties = "DPTO_NOMBRE",
                     newnamesProperties = "name",
                     jsonName = "col-departments", saveFile = TRUE, savePath = "inst/geodata/col/")


# save this file in rds format
tj <- read_lines("inst/geodata/col/col-departments.topojson")
saveRDS(tj, "inst/geodata/col/col-departments.rds")

# manually add the info of the topojson in the meta folder in the corresponding file
# for eg, for this case the info add in col.yaml
# then, restart session and use devtools to install changes
devtools::load_all()
devtools::document()
devtools::install()
"col_larg" %in% geodata::availableGeodata()

# add centroids file
geodata:::centroids_json(map_name = "col_larg", write = TRUE)
geodata:::centroids_json(map_name = "col_larg", write = TRUE, ext = "rds")
# Again restar and install pkg
devtools::load_all()
devtools::document()
devtools::install()

# now you can plot your map
lfltmagic::lflt_choropleth_Gnm(map_name = "col_larg")




# read and clean json to convert in topojson


geodata:::clean_json(geoName = "alcaldias",
                     geoFile = "inst/example/",
                     geoId = "id",
                     geoProperties = "nomgeo",
                     newnamesProperties = "name",
                     jsonName = "mex-mayors", saveFile = TRUE, savePath = "inst/geodata/mex/")

# save this file in rds format
tj <- read_lines("inst/geodata/mex/mex-mayors.topojson")
saveRDS(tj, "inst/geodata/mex/mex-mayors.rds")

# manually add the info of the topojson in the meta folder in the corresponding file
# for eg, for this case the info add in mex.yaml
# then, restart session and use devtools to install changes
devtools::load_all()
devtools::document()
devtools::install()
"mex_mayors" %in% geodata::availableGeodata()

# add centroids file
geodata:::centroids_json(map_name = "mex_mayors", write = TRUE)
geodata:::centroids_json(map_name = "mex_mayors", write = TRUE, ext = "rds")
# Again restar and install pkg
devtools::load_all()
devtools::document()
devtools::install()

# now you can plot your map
lfltmagic::lflt_choropleth_Gnm(map_name = "mex_mayors")


