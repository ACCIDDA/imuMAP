# Script to generate package datasets: nc_schools, nc_cbgs, nc_demographics
# Matches simulated school locations in imuGAP for 3 North Carolina counties

library(sf)
library(data.table)
library(tigris)
options(tigris_use_cache = TRUE)

dir.create("data", showWarnings = FALSE)

# 1. School points
sch_shp <- st_read(
  "/home/holism/workspaces/ACCIDDA/nc_measles/data/school_geodata/geocoded_addresses_arcgis.shp",
  quiet = TRUE
)
gis_key <- as.data.table(readRDS(
  "/home/holism/workspaces/ACCIDDA/nc_measles/output/NC/gis_translate.rds"
))
unit_key <- as.data.table(readRDS(
  "/home/holism/workspaces/ACCIDDA/nc_measles/output/NC/unit_key.rds"
))
res_dt <- as.data.table(readRDS(
  "/home/holism/workspaces/ACCIDDA/nc_measles/output/NC/cleaned_data.rds"
))

counties <- c(44, 50, 88)
s2024 <- res_dt[enc_unit_id %in% counties & year == 2024]
s2024[unit_key, on = .(unit_id), orig_name := i.name]
s2024[unit_key, on = .(unit_id), subtype := i.subtype]
s2024[gis_key, on = .(unit_id), USER_id := i.USER_id]

county_map <- c("44" = "Scruggs", "50" = "Simone", "88" = "Watson")
school_names <- c(
  "Chickadee Elementary",
  "Nuthatch Academy",
  "Blue Heron School",
  "Flycatcher Elementary",
  "Bluebird Learning Center",
  "Catbird Academy",
  "Finch Elementary",
  "Sparrow School",
  "Towhee Children's Academy",
  "Warbler Elementary",
  "Egret Elementary",
  "Cardinal Academy",
  "Bunting School",
  "Tanager Academy",
  "Oriole Youth Academy",
  "Grosbeak Learning Center",
  "Junco Elementary",
  "Meadowlark School",
  "Goldfinch Elementary",
  "Mockingbird Academy",
  "Kinglet Learning Center",
  "Vireo School",
  "Kingfisher Academy",
  "Cormorant Elementary"
)

s2024[, county := county_map[as.character(enc_unit_id)]]
s2024[, id := school_names[.I]]

m_shp <- merge(
  sch_shp[, c("USER_id", "geometry")],
  s2024[, .(USER_id, id, county, school_type = subtype)],
  by = "USER_id"
)
m_shp <- m_shp[order(match(m_shp$id, school_names)), ]
nc_schools <- m_shp[, c("id", "county", "school_type", "geometry")]
st_geometry(nc_schools) <- "geometry"
nc_schools <- st_transform(nc_schools, 4326)

# 2. Census Block Group boundaries
bg <- block_groups(
  state = "NC",
  county = c("Haywood", "Jackson", "Transylvania"),
  year = 2020
)
bg_map <- c("087" = "Scruggs", "099" = "Simone", "175" = "Watson")
nc_cbgs <- bg[, c("GEOID", "COUNTYFP", "ALAND", "AWATER", "geometry")]
names(nc_cbgs)[names(nc_cbgs) == "GEOID"] <- "id"
nc_cbgs$county <- bg_map[nc_cbgs$COUNTYFP]
nc_cbgs <- nc_cbgs[, c("id", "county", "ALAND", "AWATER", "geometry")]
nc_cbgs <- st_transform(nc_cbgs, 4326)
nc_cbgs <- st_simplify(nc_cbgs, preserveTopology = TRUE, dTolerance = 0.0005)

# 3. Demographics per CBG
cbg_dt <- as.data.table(st_drop_geometry(nc_cbgs))
cbg_dt[, area_wt := ALAND / sum(ALAND), by = county]
county_pops <- c("Scruggs" = 1528, "Simone" = 747, "Watson" = 621)
cbg_dt[, population := round(county_pops[county] * area_wt)]
cbg_dt[population < 5, population := 5]
nc_demographics <- cbg_dt[, .(id, county, population)]

# Save to data/
save(nc_schools, file = "data/nc_schools.rda", compress = "xz")
save(nc_cbgs, file = "data/nc_cbgs.rda", compress = "xz")
save(nc_demographics, file = "data/nc_demographics.rda", compress = "xz")

message("Data generation complete!")
