library(data.table)
library(leaflet)
library(leaflegend)
library(maptools)
library(rgdal)
library(raster)
library(maps)
library(mapdata)
library(ggmap)
library(marmap)
library(lattice)
library(sp)

#split lat,long into two separate strings
commaSplitterCLIWOC = function(text) {
  y = unlist(strsplit(text, split = ",", fixed = TRUE))
  return(y)
}

#this makes a Leaflet map given a prepared .csv of names, coordinates, counts, and other features (see CLIWOCforLeaflet)
makeCLIWOCmap <- function(landfn, seafn) {
  dfLand = as.data.frame(read.csv(landfn))
  dfSea = as.data.frame(read.csv(seafn))
  #land points
  holder = lapply(as.character(unlist(dfLand$DistinctLatLong)), commaSplitter)
  lats = as.numeric(unlist(lapply(holder, function(x) x[1])))
  longs = as.numeric(unlist(lapply(holder, function(x) x[2])))
  coordsLand = coordinates(as.matrix(cbind(longs,lats)))
  maxCoords = coordinates(as.matrix(cbind(longs[which.max(dfLand$Count)],lats[which.max(dfLand$Count)])))
  maxSpoints = SpatialPoints(maxCoords)
  lpoints = SpatialPoints(coordsLand)
  #normalize with text maps
  weightFactor = 2000000 / max(dfLand$Count)
  #label vague ones
  vagueOrNot = ifelse(dfLand$TooVague == "Y",
                      FALSE,
                      TRUE)
  
  #sea points
  coordsSea = coordinates(as.matrix(cbind.data.frame(dfSea$longitude,dfSea$latitude, deparse.level = 1)))
  spoints = SpatialPoints(coordsSea)
  
  #make popups
  popups = character(length = length(dfLand$PlaceName))
  tempText = character()
  for (i in 1:length(dfLand$PlaceName)) {
    tempText = paste(c(as.character(dfLand$PlaceName[i]), " TOTAL COUNT = ", as.character(dfLand$Count[i])), collapse = "")
    popups[i] <- tempText
  }
  
  #prepare the legend
  html_legend <- "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/Small-dark-green-circle.svg/1200px-Small-dark-green-circle.svg.png'style='width:10px;height:10px;'> Vague Destination<br/>
 <img src='https://upload.wikimedia.org/wikipedia/commons/1/11/Pan_Green_Circle.png'style='width:10px;height:10px;'> Specific Destination<br/>
  <img src='https://upload.wikimedia.org/wikipedia/commons/8/8e/Pan_Blue_Circle.png'style='width:10px;height:10px;'> Recorded Ship Position"
  
  #main map making
  leafletMap <- leaflet() %>%
    addTiles() %>%
    addCircles(data = spoints,
               color = "blue",
               opacity = .1,
               fillOpacity = .1,
               radius = 1,
               weight = 1) %>%
    addCircles(data = lpoints,
               color = "green",
               fillOpacity = .3,
               opacity = .5,
               stroke = !vagueOrNot,
               fill = vagueOrNot, #only fill if vague
               radius = dfLand$Count * weightFactor,
               popup = popups) %>%
  addControl(html = html_legend, position = "bottomleft")
  return(leafletMap)
}
  