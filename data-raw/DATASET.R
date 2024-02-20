library(osmdata)

time <- format(
  as.POSIXct("2024-02-15 18:55:20 CET"),
  format = "%Y-%m-%dT%H:%M:%SZ"
)

pharma <- opq(
  "Rutland",
  osm_types = "node",
  out = "body",
  datetime = time
) %>%
  add_osm_feature(key = "amenity", value = "pharmacy") %>%
  osmdata_sf()

pharma <- pharma$osm_points
pharma <- pharma[c("name", "dispensing", "geometry")]
rownames(pharma) <- NULL

save(pharma, file = "data/pharma.rda")
