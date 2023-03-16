# Extract_distances
Scripts to extract distance from points

### ExtractingMinDistCESBIO :
This script extracts the minimum distance between studied sites and a land use 
category from the CESBIO (ex. deciduous forest, pasture, etc.) in meters.
Those sites have to be in metropolitan France (and Corsica).
It returns a dataframe with the site IDs and the minimum distance from the 
land use category chosen.

### ExtractingMinDist :
This script extracts the minimum distance between studied sites and the lines/polygons
of a shapefile (e.g. roads, lakes).
It returns a data table containing the point IDs and the minimum distance from 
lines/polygons in meters.

### Warning:
**These scripts are « functions »**, to use them you have to download them and keep the R. files somewhere on your computer.  

In theory, you don’t even need to open them with R, however I strongly advise you to do so to read the first lines presenting what the function does and how to use it (in particular what the arguments of the function are and how your dataset should be structured). 

In your script, simply add the following command:  
`source(“pathToTheRFile”)`  
Then you can use the function!  

Here is an example of what your script should look like: 

`sitesSurveyed <- read.csv(“C:/Users/Name/Document/sitesStudied.csv”)`  
`pathToLayer <- “C:/Users/Name/Document/Waterways.shp”`  

`source("C:/Users/Name/Document/ExtractingMinDist.R")`  
`distWaterSites <- Calc_min_distance(sitesSurveyed,”Code_site”,pathToLayer)`
