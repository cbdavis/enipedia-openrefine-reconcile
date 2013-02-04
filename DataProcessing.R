########## Data Processing ##########

# These functions are used for processing data into more usable forms.

extractCoordinates <- function(point){
  coords = colsplit(point, split=",", names=c("lat", "lon"))
  
  #make sure that this is a character vector, not a factor vector
  coords$lon = as.character(coords$lon)
  coords$lat = as.character(coords$lat)
  
  #fix up lat and lon values
  eastLonLocs = grep("E", coords$lon)
  coords$lon[eastLonLocs] = gsub(' E', '', coords$lon[eastLonLocs])
  coords$lon[eastLonLocs] = as.numeric(coords$lon[eastLonLocs])
  
  westLonLocs = grep("W", coords$lon)
  coords$lon[westLonLocs] = gsub(' W', '', coords$lon[westLonLocs])
  coords$lon[westLonLocs] = 0 - as.numeric(coords$lon[westLonLocs])
  
  northLatLocs = grep("N", coords$lat)
  coords$lat[northLatLocs] = gsub(' N', '', coords$lat[northLatLocs])
  coords$lat[northLatLocs] = as.numeric(coords$lat[northLatLocs])
  
  southLatLocs = grep("S", coords$lat)
  coords$lat[southLatLocs] = gsub(' S', '', coords$lat[southLatLocs])
  coords$lat[southLatLocs] = 0 - as.numeric(coords$lat[southLatLocs])
  
  coords$lon = as.numeric(coords$lon)
  coords$lat = as.numeric(coords$lat)
  
  return(coords)
}

#convert text to the most boring form possible
#this makes it easier to perform string comparisons
removeTheWeirdness <- function(text){
  text = iconv(text, to="ASCII//TRANSLIT") #work with simple ascii - this doesn't do anything to help with misspellings
  text = gsub('http://enipedia.tudelft.nl/wiki/', '', text)
  text = gsub('\\)', '', text)
  text = gsub('\\(', '', text)
  text = gsub('/', '', text)
  text = gsub(',', ' ', text)
  text = gsub('\\.', ' ', text)
  text = gsub('&', '', text)
  text = gsub('_', ' ', text)
  text = gsub('-', ' ', text)
  text = gsub('  ', ' ', text)
  text = gsub("'", "", text)
  text = trim(tolower(text))
  text = sapply(text, URLdecode)
  return(text)
}

# queryRequest is a list based on the json string that is sent by Open Refine
# This function converts the list to a simple vector.
# This makes it a bit more straightforward for other functions to just pass a vector to the matching function
convertQueryRequestToVector <- function(queryRequest){
  externalData = c()
  
  externalData$plant = queryRequest$query
  
  #extract the different properties that are used for matching, allow for various spellings to minimize user error
  #TODO include capacity, yearly production, emissions, etc
  for (property in queryRequest$properties){
    #print(property)
    if (tolower(property$p) == "country"){
      externalData$country = property$v
    }
    if (tolower(property$p) == "owner" || tolower(property$p) == "ownercompany"){
      externalData$owner = property$v
      externalData$owner = removeTheWeirdness(externalData$owner)
    }
    if (tolower(property$p) == "latitude" || tolower(property$p) == "lat"){
      externalData$latitude = as.numeric(property$v)
    }
    if (tolower(property$p) == "longitude" || tolower(property$p) == "long" || tolower(property$p) == "lon"){
      externalData$longitude = as.numeric(property$v)
    }
    if (tolower(property$p) == "point" || tolower(property$p) == "coords" || tolower(property$p) == "coordinates"){
      coords = property$v
      tmp = extractCoordinates(coords)
      externalData$latitude = as.numeric(tmp$lat)
      externalData$longitude = as.numeric(tmp$lon)
    }
  }
  return(externalData)
}

#return a set of tokens contained within a string
tokenize <- function(text){
  text = removeTheWeirdness(text)
  tokenList = unique(unlist(strsplit(text, split=" ")))
  return(tokenList)
}
