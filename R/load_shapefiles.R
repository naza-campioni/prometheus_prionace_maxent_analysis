load_shapefiles <- function() {
  shp_folder <- "data/shapefiles"
  med_poly <- vect(file.path(shp_folder, "Mediterranean_Sea/Mediterranean_Sea.shp"))
  med_poly$NAME <- "MEDITERRANEAN"
  
  ices <- "ICES_ecoregion\\ICES_ecoregions_20171207_erase_ESRI.shp"
  ices_poly <- vect(file.path(shp_folder, ices))
  ices_poly <- crop(ices_poly, med_poly)
  
  west <- ices_poly[ices_poly$OBJECTID == 2 | ices_poly$OBJECTID == 4]
  west$NAME <- "WEST"
  
  central_east <- ices_poly[ices_poly$OBJECTID == 5 | ices_poly$OBJECTID == 7 |
                              ices_poly$OBJECTID == 8]
  central_east$NAME <- "CENTRAL_EAST"
  
  regions <- list(west = west, central_east = central_east)
  
  return(list(med = med_poly, reg = regions))
}