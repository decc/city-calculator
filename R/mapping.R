library(maptools)
library(mapproj)
library(rgdal)
library(ggplot2)

gb <- readShapePoly("~/Work/DECC/City/raw-data/ordnance_survey/bdline_gb/Data/district_borough_unitary_region", proj4string = CRS("+init=epsg:27700"))

manchester <- c("E08000001","E08000002", "E08000003", "E08000004",
                "E08000005",
                "E08000006",
                "E08000007",
                "E08000008",
                "E08000009",
                "E08000010")

man <- gb[gb$CODE %in% manchester,]

man <- spTransform(man, CRS("+init=epsg:4326"))

manf <- fortify(man, region = CODE)

qplot(long, lat, data = manf, group = id, geom = "polygon", fill = id) +
  coord_map() + scale_colour_brewer()

man.osm <- get_map(location = c(bbox(man)["x", "min"], bbox(man)["y", "min"],
                     bbox(man)["x","max"], bbox(man)["y","max"]), source =
                   "osm")

ggmap(man.osm) + geom_path(aes(x = long, y = lat, group = id), data = manf)


