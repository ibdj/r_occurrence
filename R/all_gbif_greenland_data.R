
library(tidyverse)
library(googlesheets4)
library(janitor)
library(sf)
library(rgbif)

#### importing data ####
gbif_vascularplants_greenland20260226 <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/gbif_vascularplants_greenland20260226.csv")

taxons_test <- gbif_vascularplants_greenland20260226 |> 
  group_by(scientificName) |>
  mutate(scientificName = as.factor(scientificName)) |> 
  summarise(count = n())

tmp <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1-SLBF6wwNXc1z3znjUNZuZQQ8XXx_iplr4sumrnwOn4/edit?gid=1576015470",
  skip = 4,
  n_max = 1
)

n_cols <- ncol(tmp)

checklist <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1-SLBF6wwNXc1z3znjUNZuZQQ8XXx_iplr4sumrnwOn4/edit?gid=1576015470#gid=1576015470", skip = 4, col_types = "text") |> 
  clean_names()

checklist <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1-SLBF6wwNXc1z3znjUNZuZQQ8XXx_iplr4sumrnwOn4/edit?gid=1576015470",
  skip = 4,
  col_types = paste(rep("c", n_cols), collapse = "")) |> 
  janitor::clean_names()

str(checklist)

checklist <- checklist |> 
  mutate(across(where(is.list), ~ as.character(unlist(.))))

redlist <- checklist |> 
  filter(grl_red %in% c("NT","VU")) |> 
  rename(taxonKey = taxon_key) |> 
  mutate(taxonKey = as.double(taxonKey))

names(gbif_vascularplants_greenland20260226)
names(redlist)

gbif_match_redlist <- gbif_vascularplants_greenland20260226 |> 
  left_join(redlist, by = "taxonKey")

gbif_redlisted_in_greenland <- gbif_match_redlist |> 
  filter(grl_red %in% c("NT","VU"))

#### mapping ####

points_sf <- gbif_redlisted_in_greenland |>
  dplyr::filter(!is.na(decimalLongitude),
                !is.na(decimalLatitude)) |>
  sf::st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326,         # WGS84
    remove = FALSE
  )


plot(sf::st_geometry(sf::st_transform(points_sf, 32622)))

points_sf <- sf::st_transform(points_sf, 32622)
poly <- sf::st_transform(poly, 32622)

ggplot() +
  geom_sf(data = poly, fill = "lightblue", color = "darkblue", alpha = 0.3) +
  geom_sf(data = grl, fill = "brown", color = NA, alpha = 0.3) +
  geom_sf(data = points_sf, color = "red", size = 1) +
  theme_minimal()

poly <- sf::st_read("rba_disko_nuus.gpkg")
grl <- sf::st_read("grl_land_only.gpkg")

points_sf <- points_sf |>
  filter(sf::st_coordinates(points_sf)[,2] >= 55)

error <- points_sf |>
  filter(sf::st_coordinates(points_sf)[,2] <= 55)

# Compute which points are within the polygon
inside <- sf::st_within(points_sf, poly, sparse = FALSE)[,1]  # [,1] if single polygon

# Filter points
points_in_poly <- points_sf[inside, ]

sf::st_crs(points_sf)
sf::st_crs(poly)

#### stats #####

count <- gbif_redlisted_in_greenland |> 
  group_by(scientificName) |> 
  summarize(count = n())

#### using the gbif packages ####

trach <- name_backbone(name = "Tracheophyta")  # [web:6]
trach$usageKey
# Should be 7707728 (GBIF key for Tracheophyta = vascular plants) [web:6]

trach_key <- trach$usageKey

download_key <- occ_download(
  pred("taxonKey", 212),   # example
  user  = Sys.getenv("GBIF_USER"),
  pwd   = Sys.getenv("GBIF_PWD"),
  email = Sys.getenv("GBIF_EMAIL")
)

occ_download_meta(download_key)

tracheo_key <- rgbif::name_backbone(name = "Tracheophyta", rank = "PHYLUM")$usageKey
tracheo_key

download_key <- occ_download(
  pred("taxonKey", trach_key),
  pred("country", "GL"),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "DWCA",
  
  # user authentication
  user = Sys.getenv("GBIF_USER"),
  pwd  = Sys.getenv("GBIF_PWD"),
  email= Sys.getenv("GBIF_EMAIL")
)

occ_download_meta(download_key)

rgbif::occ_download_wait(download_key)

rgbif::occ_download_cancel(download_key)

zipfile <- occ_download_get(download_key, overwrite = TRUE)
dat <- occ_download_import(zipfile)
table(dat$basisOfRecord)
table(dat$year)
table(dat$taxonomicStatus)

#### filters of all gbif data ####
dat <- dat |> 
  filter(basisOfRecord != "FOSSIL_SPECIMEN")

taxa_all_greenland <- dat |> 
  group_by(scientificName) |> 
  reframe(count = n())

genus_all_greenland <- dat |> 
  group_by(genus) |> 
  reframe(count = n())

inspect <- dat |> 
  filter(genus == "Ginkgo")

