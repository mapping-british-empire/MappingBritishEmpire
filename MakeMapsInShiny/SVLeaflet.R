# makes maps from Slave Voyages data, as formatted in SVPost1750RawNames.csv
# works similarly to MakeMapsFromData, but uses the filenames

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

#dummy function used in intermediateDFmaker
countToponymsSV <- function(entry, range) {
  return(length(which(entry == range)))
}

#dummy function used in intermediateDFmaker
#ref should have be a df formatted like NamesAndCoordsRef(NoBlanks)
lookUpToponymsSV <- function(entry, ref) {
  matchingRow = which(entry == as.character(ref$PortNames))
  if (length(matchingRow) > 0) {
    #note need the 1 because sometimes there are multiple matches--just use the first
    return(ref[matchingRow[1],3:6])
  }
  else {
    return(c(NA,NA,NA,NA))
  }
}

#input a data frame of toponyms, with repeats, from SUNERToToponymFileInMakeMap and a reference csv of toponyms with lat, long, and other features (TooVague,Sea,IndigenousName)
#counts up the toponyms and looks them up in that reference csv
#outputs a df of those toponyms with their coords, counts, and other features
intermediateDFmakerSV <- function(toponymdf,refFilename) {
  refdf = read.csv(refFilename)
  finalDf = data.frame(unique(toponymdf))
  finalDf$Count = lapply(as.character(finalDf[,1]), FUN = countToponymsSV, range = as.character(toponymdf[,1]))
  #print(finalDf[1:20,])
  otherInfo = lapply(as.character(finalDf[,1]), FUN = lookUpToponymsSV, ref = refdf)
  otherInfodf = data.frame(do.call(rbind, otherInfo))
  finalDf = cbind(finalDf, otherInfodf)
  names(finalDf)[1] <- "Toponym"
  #print(finalDf[1:20,])
  #cbind(finalDf,otherInfo)
  #print(finalDf)
  #print(as.character(finalDf[,1]))
  #print(lapply(as.character(finalDf[,1]), FUN = countToponyms, range = as.character(toponymdf[,1])))
  return(finalDf)
}

#split lat,long at comma
commaSplitterSV = function(text) {
  y = unlist(strsplit(text, split = ",", fixed = TRUE))
  return(y)
}

#find the right rows
rowFinderSV = function(a,b) {
  return(which(a == b))
}

#turn the intermediate df into a list where each entry is a coordinate with the associated names and counts
makeMappableListSV <- function(intermediateDf) {
  df = intermediateDf
  coords = unique(df$DistinctLatLong)
  #print(coords)
  placeList = as.list(coords)
  #print(placeList[1:100])
  
  #get the lat and long split
  sepCoords = lapply(as.character(coords), commaSplitterSV)
  sepCoords <- lapply(sepCoords,c)
  
  #get the place names in a list
  nameRows = lapply(coords, rowFinderSV, df$DistinctLatLong)
  toponymList = lapply(nameRows, function(l) df$Toponym[l])
  #toponymList = lapply(as.character(toponymList1), c)
  #df$Toponym[nameRows]
  #placeNames = sapply(1:length(nameRows), function()
  
  #get the counts of each place name
  countList = sapply(nameRows, function(l) df$Count[l])
  countList <- sapply(1:length(countList), function(i)
    c(as.numeric(countList[[i]])))
  #print(toponymList[1:25])
  #print(countList[1:25])
  
  #get the sum of those counts
  sumCounts = lapply(countList, sum)
  #print(sumCounts[1:25])
  
  #get the TooVague labels
  tooVagues = rep("N",length(coords))
  #print(df$TooVague[1:25])
  for (i in 1:length(tooVagues)) {
    #print(i)
    #choose just the first entry
    if(!is.na(coords[i])){
      activeRow <- which(as.character(coords)[i] == as.character(df$DistinctLatLong))
      #print(activeRow)
      #print(df$TooVague[activeRow])
      if(as.character(df$TooVague)[activeRow] == "y" | as.character(df$TooVague)[activeRow] == "Y" ){
        tooVagues[i] <- "Y"
      }
      else {
        tooVagues[i] <- "N"
      }
    }
  }
  
  #get the Sea labels
  Seas = rep("N",length(coords))
  #print(df$Sea)
  for (i in 1:length(Seas)) {
    #print(i)
    #choose just the first entry
    if(!is.na(coords[i])){
      activeRow <- which(as.character(coords)[i] == as.character(df$DistinctLatLong))
      #print(activeRow)
      #print(df$TooVague[activeRow])
      #print(activeRow)
      #print(df$Sea[activeRow])
      if(as.character(df$Sea)[activeRow] == "y" | as.character(df$Sea)[activeRow] == "Y"){
        Seas[i] <- "Y"
      }
      else {
        Seas[i] <- "N"
      }
    }
  }
  
  #get the IndigNames labels
  indigNames = rep("N",length(coords))
  #print(df$TooVague[1:25])
  for (i in 1:length(indigNames)) {
    #print(i)
    #choose just the first entry
    if(!is.na(coords[i])){
      activeRow <- which(as.character(coords)[i] == as.character(df$DistinctLatLong))
      #print(activeRow)
      #print(df$TooVague[activeRow])
      if(as.character(df$IndigenousName)[activeRow] == "y" | as.character(df$IndigenousName)[activeRow] == "Y" ){
        indigNames[i] <- "Y"
      }
      else {
        indigNames[i] <- "N"
      }
    }
  }
  
  newList <- sapply(1:length(placeList),function(i) list(c(as.character(placeList[[i]]), 
                                                           c(sepCoords[[i]]), 
                                                           list(as.character(toponymList[[i]])),
                                                           list(as.numeric(countList[[i]]),
                                                                sumCounts[[i]],
                                                                tooVagues[i],
                                                                Seas[i],
                                                                indigNames[i]))))
  #winnow out incomplete entries
  badEntries = numeric()
  #print(newList[1:20])
  for (i in 1:length(newList)) {
    if(length(newList[[i]]) == 9) {
      names(newList[[i]]) <- c("DistinctLatLong", "lat", "long", "Toponyms", "ToponymCounts", "TotalCount", "TooVague", "Sea", "IndigenousName")
    }
    else {
      #print(c(c("i = ",i), " ??? ", newList[[i]]))
      badEntries <- c(badEntries,i)
    }
  }
  cleanList = newList[- badEntries]
  #print(newList[1:10])
  #print(cleanList[1:10])
  #names(newList[[]]) <- c("DistinctLatLong", "lat", "long", "Toponyms", "ToponymCounts", "TotalCount", "TooVague", "Sea", "IndigenousName")
  #print(newList[1:10])
  #print(placeList)
  #names(placeList) = c("Coords", "Lat", "Long", "Toponyms", "Counts", "TooVague", "Sea", "IndigName")
  #placeList[["Coords"]] = coords
  print(cleanList[1:20])
  return(cleanList)
}

#MAIN MAP MAKING FUNCTION
#input the filenames raw output of StanfordNER and a reference csv
#outputs a leaflet map
makeMapSV <- function(toponymdfFn, refFilename) {
  #get the list of the locations
  listOfLocns = makeMappableListSV(
    intermediateDFmakerSV(
      read.csv(toponymdfFn, header = FALSE),refFilename))
  #print(listOfLocns[1:4])
  #get the latitudes out
  lats = numeric(length(listOfLocns))
  for(i in 1:length(listOfLocns)) {
    lats[i] <- listOfLocns[[i]]$lat 
  }
  #get longitudes out
  longs = numeric(length(listOfLocns))
  for(i in 1:length(listOfLocns)) {
    longs[i] <- listOfLocns[[i]]$long 
  }
  #get total counts out
  totalCts = numeric(length(listOfLocns))
  for(i in 1:length(listOfLocns)) {
    totalCts[i] <- listOfLocns[[i]]$TotalCount 
  }
  #note the maximum count and its position, and make it a spatial point (for labelling on the map)
  maxCt = max(as.numeric(totalCts))
  maxPosn = which.max(as.numeric(totalCts))
  maxCoords = coordinates(as.matrix(cbind(as.numeric(longs[maxPosn]),as.numeric(lats[maxPosn]))))
  maxSpoints = SpatialPoints(maxCoords)
  #get seas, vagues, and indigNames out
  seaPosns = character(length(listOfLocns))
  for(i in 1:length(listOfLocns)) {
    seaPosns[i] <- listOfLocns[[i]]$Sea
  }
  vaguePosns = character(length(listOfLocns))
  for(i in 1:length(listOfLocns)) {
    vaguePosns[i] <- listOfLocns[[i]]$TooVague
  }
  indigPosns = character(length(listOfLocns))
  for(i in 1:length(listOfLocns)) {
    indigPosns[i] <- listOfLocns[[i]]$IndigenousName
  }
  #bind the coordinates in a matrix and make them spatial points
  coords = coordinates(as.matrix(cbind(as.numeric(longs),as.numeric(lats))))
  spoints = SpatialPoints(coords)
  #assign colors (land or sea)
  # cols = rep("olivedrab3", times=length(seaPosns))
  # for(i in 1:length(seaPosns)){
  #   if(seaPosns[i] == "Y") {
  #     cols[i] <- "lightblue"
  #   }
  # }
  
  #assign outlines (vague or specific)
  vagueOrNot = ifelse(vaguePosns == "Y",
                      FALSE,
                      TRUE)
  #print(vagueOrNot)
  #assign colors
  landOrSea = ifelse(seaPosns == "Y",
                     "blue",
                     "green")
  
  #for sizing
  weightFactor = 2000000 / maxCt
  
  #make the popup text
  popups = character(length = length(listOfLocns))
  tempText = character()
  tempMat = matrix()
  for(i in 1:length(listOfLocns)) {
    nNames = length(listOfLocns[[i]]$Toponyms)
    tempMat = cbind(as.character(listOfLocns[[i]]$Toponyms), 
                    as.character(listOfLocns[[i]]$ToponymCounts))
    for(j in 1:nNames){
      tempText <- paste(c(tempText, tempMat[j,1], " (", tempMat[j,2], ")  "), collapse = "")
      #print(tempText)
    }
    popups[i] <- paste(c(tempText, "TOTAL COUNT = ", totalCts[i]), collapse = "")
    tempText = character()
  }
  #print(popups[1:20])
  
  #prepare symbols for legend
  # redx <- makeSymbol("cross", width = 100, height = 100, color = "black", fillColor = "red",
  #                    opacity = .7, fillOpacity = .5)
  # htmltools::img(src = redx)
  # solidGreen <- makeSymbol('circle', width = 100, height = 100, color = 'green', fillColor = 'green',
  #                    opacity = .7, fillOpacity = .5)
  # outlineGreen <- makeSymbol('circle', width = 100, height = 100, color = 'green', fillColor = 'green',
  #                            opacity = .7, fillOpacity = 0)
  # solidBlue <- makeSymbol('circle', width = 100, height = 100, color = 'blue', fillColor = 'blue',
  #                          opacity = .7, fillOpacity = .5)
  # outlineBlue <- makeSymbol('circle', width = 100, height = 100, color = 'blue', fillColor = 'green',
  #                            opacity = .7, fillOpacity = 0)
  html_legend <- "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/Small-dark-green-circle.svg/1200px-Small-dark-green-circle.svg.png'style='width:10px;height:10px;'> Vague Land Place<br/>
 <img src='https://upload.wikimedia.org/wikipedia/commons/1/11/Pan_Green_Circle.png'style='width:10px;height:10px;'> Specific Land Place<br/>
  <img src='https://upload.wikimedia.org/wikipedia/commons/7/79/Blue_circle_frame.png'style='width:10px;height:10px;'> Vague Sea Place<br/>
  <img src='https://upload.wikimedia.org/wikipedia/commons/8/8e/Pan_Blue_Circle.png'style='width:10px;height:10px;'> Specific Sea Place"
  
  #make the map
  leafletMap <- leaflet() %>%
    addTiles() %>%
    addCircles(
      data = spoints,
      color = landOrSea,
      stroke = !vagueOrNot,
      fill = vagueOrNot, #only fill if vague
      radius = weightFactor * totalCts,
      popup = popups) %>%
    addCircles(
      data = spoints,
      radius = 25000,
      color = "black",
      stroke = FALSE, 
      fillOpacity = 0.2,
      popup = popups
    ) %>%
    addControl(html = html_legend, position = "bottomleft")
  # addLegend("bottomleft",
  #   colors = )
  #   addMarkers(lng = as.numeric(longs[which(vagueOrNot)]),
  # lat = as.numeric(lats[which(vagueOrNot)]),
  # popup = popups[which(vagueOrNot)])
  ###ADD LEGEND
  ###ADD TITLE
  return(leafletMap)
  
}