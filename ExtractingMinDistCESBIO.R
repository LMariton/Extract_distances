##############################################################################################################
# This script allows to extract the minimum distance between studied sites and a land use category from the
# CESBIO (ex. deciduous forest, pasture, etc.) in meters
# Those sites have to be in metropolitan France (and Corsica)
#
# It returns a dataframe with the site IDs and the minimum distance from the land use category chosen.
#
##############################################################################################################
#
# Land use data as to be download at this link : 
# http://osr-cesbio.ups-tlse.fr/echangeswww/TheiaOSO/vecteurs_2017/liste_vecteurs.html
# Choose all the departements (and eventually the adjacent ones) where your sites are located.
# Store all the unziped files in the same folder
#
##############################################################################################################
#
# Arguments : 
#
# path_to_folder : path to the folder containing the CESBIO polygon layers (no other file should be stored in
# this folder)
# 
# tableSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS82
# -> a column named "Longitude" with the site longitude in WGS82
# -> a column named "with the sites unique IDs
#
# Col_ID : column name of the table containing the site IDs (ex : "CODE_SITE")
#
# landuse_class <- class of land use for which a minimum distance from each sites should be processed
# Reminder: culture ete:11, culture hiver:12, foret feuillus:31, foret coniferes:32, pelouses:34
# landes ligneuses:36, urbain dense:41, urbain diffus:42, zones ind et com:43, surfaces routes:44
# surfaces minerales:45, plages et dunes:46, eau:51, glaciers ou neige: 53, prairies:211
# vergers:221, vignes:222
#
##############################################################################################################

min_distance_landuse <- function(path_to_folder,tableSites,Col_ID,landuse_class){
  
  ####Preliminary####
  
  #Install and open required packages
  
  load <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  } 
  packages <- c("sf","stringr")
  load(packages)
  
  #creation of a site layer
  shp_points <- st_as_sf(tableSites, coords = c("Longitude","Latitude"), crs = 4326)
  shp_points <- st_transform(shp_points,crs=2154)
  
  ####I. Open shapefile layers with only polygons of the chosen land use feature####
  
  #Names of the shp to open
  names_Files <- list.files(path_to_folder,full.names=T)
  names_Files_short <- list.files(path_to_folder,full.names=F)
  
  #Open the shapefiles but keep only the polygons of the choosen landuse
  list_couches_sf <- list()
  for (index in (1:length(names_Files))){
    list_couches_sf[[index]] <- st_read(names_Files[index],query = paste0("select * from ",names_Files_short[index]," where (Classe=",landuse_class,")"),quiet = T)
    cat(paste0("First step - open landuse shapefiles : ",round(index/(length(names_Files))*100,digits=1),"%\r"))
  }
  names(list_couches_sf)<-c(names_Files_short[c(1:length(names_Files))])
  
  ####II. Calculation of the minimum distance bewteen sites and land use feature####
  
  #for each shapefile (i.e. departement) calculation of the minimum distance between points and features
  #each department has to be considered as a point might be closer to a forest - for example - that is in
  #an other departement than the departement it is located in (because the point is near administrative borders)
  dist_by_dep <- data.frame(shp_points[,Col_ID][[1]])
  colnames(dist_by_dep) <- Col_ID
  for (index in (1:length(list_couches_sf))){
    dist_by_dep[,(dim(dist_by_dep)[2])+1] <- st_nearest_feature(shp_points,list_couches_sf[[index]])
    colnames(dist_by_dep)[dim(dist_by_dep)[2]] <- paste0(names(list_couches_sf)[index],"_index")
    dist_by_dep[,(dim(dist_by_dep)[2])+1] <- rep(NA,dim(dist_by_dep)[1])
    colnames(dist_by_dep)[dim(dist_by_dep)[2]] <- names(list_couches_sf)[index]
    
    for (distance_index in unique(dist_by_dep[,paste0(names(list_couches_sf)[index],"_index")])){
      dist_by_dep[which(dist_by_dep[,paste0(names(list_couches_sf)[index],"_index")]==distance_index),
                  names(list_couches_sf)[index]] <- as.numeric(st_distance(shp_points[
                    which(dist_by_dep[,paste0(names(list_couches_sf)[index],"_index")]==distance_index),],list_couches_sf[[index]][distance_index,]))
    }
    cat(paste0("Second step - process minimum distances : ",round(index/(length(list_couches_sf))*100,digits=1),"%\r"))
  }
  
  #Keep only columns with distance between points and land use features by departement
  dist_by_dep_only_dist <- dist_by_dep[,which(!(str_detect(colnames(dist_by_dep),"index")))]
  dist_by_dep_only_dist <- dist_by_dep_only_dist[,which(!(colnames(dist_by_dep_only_dist)==Col_ID))]
  
  #Remove empty columns (happen when no land use features from the chosen land use category in a departement)
  dist_by_dep_only_dist <- dist_by_dep_only_dist[,which(!(colnames(dist_by_dep_only_dist) %in% colnames(dist_by_dep_only_dist)[is.na(dist_by_dep_only_dist[1,])]))]
  
  #Keep only the minimum distance by row (i.e. the minimum distance when considering all departement)
  tab_min_dist <- data.frame(dist_by_dep[,Col_ID],apply(dist_by_dep_only_dist,1,min))
  colnames(tab_min_dist) <- c(Col_ID,paste0("min_dist_",landuse_class))  
  
  return(tab_min_dist)

}