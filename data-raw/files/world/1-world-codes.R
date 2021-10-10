
library(tidyverse)

#devtools::install_github('AndySouth/rworldmap')
library(rworldmap)

data(countrySynonyms)
x <- countrySynonyms

map_name <- "world_countries"

file_path <- "data-raw/files/world"
geodata_path <-  "data-raw/geodata/world"

# # https://github.com/mledoze/countries
# url <- "https://raw.githubusercontent.com/mledoze/countries/master/dist/countries.json"
# countries <- jsonlite::read_json(url)
# countries <- jsonlite::flatten(countries)

url <- "https://raw.githubusercontent.com/mledoze/countries/master/dist/countries.csv"
countries <- read_csv(url)

is_list_col <- function(col){
  sum(grepl(",*.,", col))/length(col) > 0.5
}

#list_cols <- countries %>% map(is_list_col) %>% keep(~.) %>% names()
names_cols <- c("cca3","name", "altSpellings", "translations", "demonyms")

c0 <- countries %>% select(all_of(names_cols))

c1 <- c0 %>%
  tidyr::unite("name", 2:5, sep = ",") %>%
  select(id = cca3, name)

c2 <- c1 %>%
  separate(name, "name", sep = ",")
write_csv(c2, file.path(file_path, map_name, "aux/world_countries-codes-0.csv"))

altnames1 <- c1 %>%
  mutate(name_list = strsplit(name, split = ",")) %>%
  unnest(cols = name_list) %>%
  select(-name, altname = name_list) %>% distinct()
write_csv(altnames1, file.path(file_path, map_name,
                        "aux/world_countries-altnames-1.csv"))


# ALTNAMES

# merge altnames
altnames0 <- read_csv(file.path(file_path, map_name,
                                "aux/world-countries-altnames-0.csv"))
altnames <- bind_rows(altnames0, altnames1) %>%
  distinct() %>%
  arrange(id)

write_csv(altnames, file.path(file_path, map_name,
                        "world_countries-altnames.csv"))
write_csv(altnames, file.path(geodata_path, map_name,
                              "world_countries-altnames.csv"))


