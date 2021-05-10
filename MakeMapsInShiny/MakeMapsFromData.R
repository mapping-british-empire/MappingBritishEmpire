# ###HOW TO USE THIS
# 1) use StanfordNER in the terminal to produce the .tsvs of NER from the texts you care about
# 1)a) can use excel to see how many names need to be in the reference sheet (unique to get unique names; countif to count how often they appear; vlookup to find them in NamesAndCoordsRef(NoBlanks))
# 2) feed makeMap the filenames of your reference sheet, followed by the filenames of as many texts as you want to map


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
library(readr)


# this function assumes that continguous place names are multiple-token place names, then combines them (up to any n-gram!)
# e.g., in, "We sailed from the Bay of Campeachy", it would combine "Bay of Campeachy"
# if places are distinct but abut in a text, they are usually separated by a comma or other punctuation mark
# e.g., "We left Oxford, Mississippi"
# which this script does not combine
# note that this is vulnerable to NER that fails to catch middle prepositions
# so if "of" in "Bay of Campeachy" is not recognized as a place, it won't combine them
#no prior manipulaition of StanfordNER output is necessary
#feed it a file of the StanfordNER output (two columns .tsv, column 1 is token, column 2 is NER tag of O or PNM)
SUNERtoToponymFileInMakeMap <- function(rawtoponymdf) {
  #prepare the data  
  input = rawtoponymdf
  
  #assign a count to each word
  counts = seq_along(input$X1)
  #bind to left
  df = cbind(counts,input)
  
  #filter the whole output down to just those rows that are place names 
  PNMrows = which(df$X2 == "PNM")
  PNMdf = df[PNMrows,]
  
  #split into the words and their original positions in the text
  words = as.character(unlist(PNMdf$X1))
  PNMposns = as.numeric(unlist(PNMdf$counts))
  #debugging
  #print(words[1:20])
  #print(PNMposns[1:20])
  #actions on data
  #flag to mark if we're in the midst of a name
  buildingName = FALSE
  #place name being built
  currentName = character()
  #output holder
  output = character()
  #main loop (do it as a for loop because order matters--can't vectorize)
  for(i in seq_along(words[1:length(words)-1])) {
    #check if already in a name
    #if not, make it current name
    if(!buildingName) {
      currentName <- words[i]
    }
    #if yes, add it to current name
    else {
      currentName <- paste(c(currentName,words[i]),collapse = " ")
    }
    
    #check if next place word is adjacent and change buildingName to True
    #make >= to catch possessives, e.g. "Gulf of St. Michael's"
    if(PNMposns[i] + 1 == PNMposns[i+1]) {
      buildingName <- TRUE
      #print(c(words[i],"merging names",words[i+1]))
      #print(currentName)
    }
    else {
      buildingName <- FALSE
    }
    #if we're not building a name with the next word, write the name to the output
    if(!buildingName) {
      output <- c(output,currentName)
    }
  }
  #for debugging, write the output to a file because it can be useful to see the place names
  #outputFile = paste(c(SUNERoutputfn, "_MergedFromRaw.csv"), collapse = "")
  #write.csv(output, file = outputFile)
  return(output)
}

#dummy function used in intermediateDFmaker
countToponyms <- function(entry, range) {
  return(length(which(entry == range)))
}

#dummy function used in intermediateDFmaker
#ref should have be a df formatted like NamesAndCoordsRef(NoBlanks)
lookUpToponyms <- function(entry, ref) {
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
intermediateDFmaker <- function(toponymdf,refFilename) {
  refdf = read.csv(refFilename)
  finalDf = data.frame(unique(toponymdf))
  finalDf$Count = lapply(as.character(finalDf[,1]), FUN = countToponyms, range = as.character(toponymdf))
  otherInfo = lapply(as.character(finalDf[,1]), FUN = lookUpToponyms, ref = refdf)
  otherInfodf = data.frame(do.call(rbind, otherInfo))
finalDf = cbind(finalDf, otherInfodf)
names(finalDf)[1] <- "Toponym"
#debugging
#print(finalDf[1:20,])
  #cbind(finalDf,otherInfo)
  #print(finalDf)
  #print(as.character(finalDf[,1]))
  #print(lapply(as.character(finalDf[,1]), FUN = countToponyms, range = as.character(toponymdf[,1])))
return(finalDf)
}

#split lat,long at comma
commaSplitter = function(text) {
  y = unlist(strsplit(text, split = ",", fixed = TRUE))
  return(y)
}

#find the right rows
rowFinder = function(a,b) {
  return(which(a == b))
}

#turn the intermediate df into a list where each entry is a coordinate with the associated names and counts
makeMappableList <- function(intermediateDf) {
  df = intermediateDf
  #get the coordinates
  coords = unique(df$DistinctLatLong)
  placeList = as.list(coords)
  
  
  #get the lat and long split
  sepCoords = lapply(as.character(coords), commaSplitter)
  sepCoords <- lapply(sepCoords,c)
  
  #get the place names in a list
  nameRows = lapply(coords, rowFinder, df$DistinctLatLong)
  toponymList = lapply(nameRows, function(l) df$Toponym[l])

  
  #get the counts of each place name
  countList = sapply(nameRows, function(l) df$Count[l])
  countList <- sapply(1:length(countList), function(i)
                                                    c(as.numeric(countList[[i]])))

  
  #get the sum of those counts
  sumCounts = lapply(countList, sum)

  
  #get the TooVague labels
  tooVagues = rep("N",length(coords))
  for (i in 1:length(tooVagues)) {
    #print(i)
    #choose just the first entry
   if(!is.na(coords[i])){
    activeRow <- which(as.character(coords)[i] == as.character(df$DistinctLatLong))
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
  for (i in 1:length(Seas)) {
    #choose just the first entry
    if(!is.na(coords[i])){
      activeRow <- which(as.character(coords)[i] == as.character(df$DistinctLatLong))
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
  for (i in 1:length(indigNames)) {
    #choose just the first entry
    if(!is.na(coords[i])){
      activeRow <- which(as.character(coords)[i] == as.character(df$DistinctLatLong))
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
  for (i in 1:length(newList)) {
    if(length(newList[[i]]) == 9) {
    names(newList[[i]]) <- c("DistinctLatLong", "lat", "long", "Toponyms", "ToponymCounts", "TotalCount", "TooVague", "Sea", "IndigenousName")
  }
  else {
    #print(c(c("i = ",i), " ??? ", newList[[i]]))
    badEntries <- c(badEntries,i)
  }
  }
  ####THIS VARIABLE IS USEFUL FOR SEEING WHAT PLACES ARE NOT ALREADY IN NAMESANDCOORDSREF(NOBLANKS).csv
  #UNMAPPEDPLACES <<-newList[badEntries]
    
  cleanList = newList[- badEntries]
  return(cleanList)
}

#MAIN MAP MAKING FUNCTION
#input the filenames raw output of StanfordNER and a reference csv
#outputs a leaflet map
makeMapOneText <- function(toponymdf, refFilename) {
  #get the list of the locations
  listOfLocns = makeMappableList(
    intermediateDFmaker(
      SUNERtoToponymFileInMakeMap(toponymdf),refFilename))
  #print(listOfLocns[1:4])
  #get the latitudes out
  lats = numeric(length(listOfLocns))
  distortionFac = numeric(length(listOfLocns))
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
  SPECIAL <<- listOfLocns
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
  return(leafletMap)
  
}

###MAIN FUNCTION FOR USER
#make a map that combines multiple texts
#recommend putting all the texts in a folder, then saving the output of list.files to a variable, then feeding that variable to the function
#feed it a toponym reference sheet .csv and  multiple outputs from SUNER .tsv
#returns a map of them all
makeMap <- function(toponymrefcsv,...) {
  x <- list(...)
  #print(x)
  #make sure the filenames are germane
  if(!is.character(unlist(x))) {
    print("ERROR: Make sure you specified one or more filenames after your reference .csv")
    return()
  }
  #main path
  #first combine them in a long .tsv with a single column
  else {
    rawToponymFns = unlist(x)
    combinedToponymDf = data.frame()
    for (i in 1:length(rawToponymFns)) {
      #print(read_tsv(rawToponymFns[i], col_names = FALSE)[1:20,])
      #print(combinedToponymDf)
      print(paste(c("working on ", rawToponymFns[i]), collapse = ""))
      combinedToponymDf <- rbind(combinedToponymDf, read_tsv(rawToponymFns[i], col_names = FALSE))
      #print(combinedToponymDf[1:20,])
    }
    #x <<- combinedToponymDf
    #now just feed that table to the main makeMapOneText function
    return(makeMapOneText(combinedToponymDf,toponymrefcsv))
  }
}