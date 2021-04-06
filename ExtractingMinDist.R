##############################################################################################################
# This script allows to extract the minimum distance between studied sites and the lines/polygons of a 
# shapefile (e.g. roads, lakes).
# It returns a data table containing the point IDs and the minimum distance from lines/polygons in meters.
##############################################################################################################

### Arguments ###

# tableSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS82
# -> a column named "Longitude" with the site longitude in WGS82
# -> a column with the sites unique IDs

# Col_ID : column name of the point layer containing the site IDs (ex : "CODE_SITE")

# lines_shp_or_path : either the path to open the line file (ex : "C:/Users/Prenom Nom/Documents/Lines.shp")
# or the line file already opened in your script

##############################################################################################################

Calc_min_distance <- function (tableSites,Col_ID,lines_shp_or_path){

  #required package
  if (!("sf" %in% installed.packages()[, "Package"])){
    install.packages("sf", dependencies = TRUE)
  }
  library(sf)
  
  #creation of a site layer
  shp_points <- st_as_sf(tableSites, coords = c("Longitude","Latitude"), crs = 4326)
  shp_points <- st_transform(shp_points,crs=2154)
  
  #open lines data
  if (all(class(lines_shp_or_path)=="character")){
    shp_lines <- st_read(lines_shp_or_path) 
  } else if (all(class(lines_shp_or_path)==c("sf","data.frame"))){
    shp_lines <- lines_shp_or_path
  } else {
    cat("lines_shp_or_path has to be either the path to the polygon file (i.e. a character)\nor a shp file created with the sf package (i.e. an object whose class is c(sf,data.frame)")
  }
  shp_lines <- st_transform(shp_lines,crs=2154) #projection in Lambert 93 if not
  
  #table to fill
  DistBuf <- data.frame(data.frame(shp_points[,Col_ID])[,Col_ID])
  colnames(DistBuf) <- "Code_site"

  DistBuf$index_min_dist <- st_nearest_feature(shp_points,shp_lines)
  DistBuf$min_dist <- rep(NA,dim(DistBuf)[1])
  
  Done <- 0
  for (distance_index in unique(DistBuf$index_min_dist)){
    DistBuf$min_dist[which(DistBuf$index_min_dist==distance_index)] <- as.numeric(st_distance(shp_points[
                  which(DistBuf$index_min_dist==distance_index),],shp_lines[distance_index,]))
    Done <- Done+1
    cat(paste0(round(Done/(length(unique(DistBuf$index_min_dist)))*100,digits=1),"%\r"))
  }
  
  DistBuf <- DistBuf[,which(colnames(DistBuf)!="index_min_dist")]
  return(DistBuf)
  
}
