#This file contains several functions that are useful utilities in the process of instance matching

#never ever ever convert strings to factors
options(stringsAsFactors = FALSE)

library(SPARQL) #query Enipedia
library(rjson) #json is used for input/output data
library(reshape) #colsplit
library(gdata) #trim
library(RecordLinkage) #string matching

#http://en.wikipedia.org/wiki/Jaccard_index
#Look at length of intersection and union of tokens in two strings to determine similarity
jaccard_index <- function(text1, text2){
  set1 = tokenize(text1)
  set2 = tokenize(text2)
  jaccard_index = length(intersect(set1, set2)) / length(union(set1, set2))
  return(jaccard_index)
}

#return a set of tokens contained within a string
tokenize <- function(text){
  text = removeTheWeirdness(text)
  tokenList = unique(unlist(strsplit(text, split=" ")))
  return(tokenList)
}

retrieveCompanyDataFromEnipedia <- function(){
  enipediaData = NULL
  endpoint = "http://enipedia.tudelft.nl/sparql"
  queryString = paste(getPrefixes(), 
                      "select distinct ?company where {
                        ?x prop:Ownercompany ?company . 
                        }", sep="")
  d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  enipediaData = d$results
  colnames(enipediaData) = "name"
  return(enipediaData)  
}

retrieveCountryDataFromEnipedia <- function (country) {
  enipediaData = NULL
  if (country != ""){
    endpoint = "http://enipedia.tudelft.nl/sparql"
    queryString = paste(getPrefixes(), 
                        "select * where {
                      ?x rdf:type cat:Powerplant .
			OPTIONAL{?x prop:City ?city} .
                        ?x rdfs:label ?name . 
                        ?x prop:Country a:", gsub(" ", "_", country) ," . 
			OPTIONAL{?x prop:State ?state} .
                        OPTIONAL{?x prop:Ownercompany ?owner}. 
                        ?x prop:Point ?point . 
                        }", sep="")
    d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
    enipediaData = d$results
    if (dim(enipediaData)[1] > 0){
      coords = extractCoordinates(enipediaData$point)
      enipediaData$lat = coords$lat
      enipediaData$lon = coords$lon
    } else {
      print("no results for country")
    }
  } else {
    print("no country specified")
  }
  return(enipediaData)
}

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

#These are common prefixes used with the Enipedia SPARQL endpoint
getPrefixes <- function(){
  return("PREFIX a: <http://enipedia.tudelft.nl/wiki/>
          PREFIX prop: <http://enipedia.tudelft.nl/wiki/Property:>
         PREFIX cat: <http://enipedia.tudelft.nl/wiki/Category:>
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
         PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         PREFIX fn: <http://www.w3.org/2005/xpath-functions#>
         PREFIX afn: <http://jena.hpl.hp.com/ARQ/function#>")
}


#convert text to the most boring form possible
#this makes it easier to perform string comparisons
removeTheWeirdness <- function(text){
  text = URLdecode(text)
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
  return(text)
}

matchPowerPlants <- function(queryRequest, numResults){
  country = ""
  owner = ""
  point = NULL
  latitude = NULL
  longitude = NULL
  #extract the different properties that are used for matching, allow for various spellings to minimize user error
  #TODO include capacity, yearly production, emissions, etc
  for (property in queryRequest$properties){
    #print(property)
    if (tolower(property$p) == "country"){
      country = property$v
    }
    if (tolower(property$p) == "owner" || tolower(property$p) == "ownercompany"){
      owner = property$v
      owner = removeTheWeirdness(owner)
    }
    if (tolower(property$p) == "latitude" || tolower(property$p) == "lat"){
      latitude = property$v
    }
    if (tolower(property$p) == "longitude" || tolower(property$p) == "long" || tolower(property$p) == "lon"){
      longitude = property$v
    }
    if (tolower(property$p) == "point" || tolower(property$p) == "coords" || tolower(property$p) == "coordinates"){
      coords = property$v
      tmp = extractCoordinates(coords)
      latitude = tmp$lat
      longitude = tmp$lon
    }
    
  }
  
  #TODO need some check to correct the country - find closest match if slightly misspelled
  #or give feedback to the user about what they should do
  
  enipediaData = retrieveCountryDataFromEnipedia(country)
  
  #get geographic information
  coords = extractCoordinates(enipediaData$point)
  enipediaData$lat = coords$lat
  enipediaData$lon = coords$lon
  
  #TODO implement matching on a soup consisting of the owner, place, etc.
  enipediaCleanedName = removeTheWeirdness(gsub(' Powerplant', '', enipediaData$name))
  enipediaCleanedOwnerName = removeTheWeirdness(gsub(' Powerplant', '', enipediaData$owner))
  
  #write('name to match on is- ', stderr())
  #write(removeTheWeirdness(queryRequest$query), stderr())
  
  #perform string matching
  ldiff = levenshteinSim(removeTheWeirdness(queryRequest$query), 
                         enipediaCleanedName)
  
  jdiff = jarowinkler(removeTheWeirdness(queryRequest$query), 
                      enipediaCleanedName, 
                      r=0.5)

  ldiffOwner = levenshteinSim(removeTheWeirdness(owner), 
                         enipediaCleanedOwnerName)
  
  jdiffOwner = jarowinkler(removeTheWeirdness(owner), 
                      enipediaCleanedOwnerName, 
                      r=0.5)
  
  jaccard_index_values = unlist(lapply(enipediaCleanedName, function(x) {jaccard_index(x,removeTheWeirdness(queryRequest$query))}))
  
  #TODO allow json query strings to specify this
  distanceCutoff = 20000
  distanceScores = NULL
  if(!is.null(latitude) && !is.null(longitude)){
    
    distances = distCosine(cbind(longitude, 
                                 latitude), 
                           cbind(enipediaData$lon,
                                 enipediaData$lat)
    )
    #scale distance scores from 0 to 1, with 0 representing the distance cutoff
    #and 1 meaning that the point is directly on top of it
    distanceScores = distances
    distanceScores[which(distances > distanceCutoff)] = 0
    distanceScores = 1 - (distanceScores / distanceCutoff);
  }
  
  #TODO this will need to accomodate more things like matches based on company name, etc.
  #figure out some robust way to do this
  #may want to also do some sort of soup matching by default with the city, owner, etc.
  #find the distance from the origin
  
  summedSquareOfDistances = ldiff^2 + jdiff^2 + jaccard_index_values^2
  
  #add in additional values if we have done the calculations
  if (!is.null(distanceScores)){
    summedSquareOfDistances = summedSquareOfDistances + distanceScores^2
  } 
  
  if (owner != ""){
    summedSquareOfDistances = summedSquareOfDistances + ldiffOwner^2 + jdiffOwner^2
  }
  
  dist = sqrt(summedSquareOfDistances)

  locs = sort(dist, decreasing=TRUE, index.return=TRUE)$ix[c(1:numResults)]
  
  allResultsForQuery = list()
  for (loc in locs){
    resultSet = list(id=enipediaData$x[loc],
                     name=paste('name:',enipediaData$name[loc],'|',
                                'owner:',removeTheWeirdness(enipediaData$owner[loc]), '|',
                                'city:',removeTheWeirdness(enipediaData$city[loc]), sep=""),
                     type=list(c(id="http://enipedia.tudelft.nl/wiki/Category:Powerplant",
                                 name="Powerplant")),
                     score=dist[loc],
                     latitude = enipediaData$lat[loc],
                     longitude = enipediaData$lon[loc],
                     match=FALSE) # hard-coded letting the humans always check things off
    
    allResultsForQuery$result = c(allResultsForQuery$result,list(resultSet))
  }
  
  return(allResultsForQuery)  
}


matchEnergyCompany <- function (queryRequest, numResults=5) {
  company = queryRequest$query
  
  #Don't query this if we already have it
  enipediaData = retrieveCompanyDataFromEnipedia()
  enipediaCleanedName = removeTheWeirdness(enipediaData$name)
  ldiff = levenshteinSim(removeTheWeirdness(queryRequest$query), 
                         enipediaCleanedName)
  
  jdiff = jarowinkler(removeTheWeirdness(queryRequest$query), 
                      enipediaCleanedName, 
                      r=0.5)
  
  jaccard_index_values = unlist(lapply(enipediaCleanedName, function(x) {jaccard_index(x,removeTheWeirdness(queryRequest$query))}))
  
  dist = sqrt(ldiff^2 + jdiff^2 + jaccard_index_values^2)

  locs = sort(dist, decreasing=TRUE, index.return=TRUE)$ix[c(1:numResults)]
  
  allResultsForQuery = list()
  for (loc in locs){
    resultSet = list(id=enipediaData$x[loc],
                     name=enipediaData$name[loc],
                     type=list(c(id="http://enipedia.tudelft.nl/wiki/Category:Energy_Company",
                                 name="Energy_Company")),
                     score=dist[loc],
                     latitude = enipediaData$lat[loc],
                     longitude = enipediaData$lon[loc],
                     match=FALSE) # hard-coded letting the humans always check things off
    
    allResultsForQuery$result = c(allResultsForQuery$result,list(resultSet))
  }  
  return(allResultsForQuery)  
}

#processes single query requests
processMatchingQueryRequest <- function (queryRequest, numResults=5) {  
  
  #see what thing we're matching on
  if (queryRequest$type == "Category:Energy_Company"){
    return(matchEnergyCompany(queryRequest, numResults))
  } else { #assume we're trying to match power plants
    return(matchPowerPlants(queryRequest, numResults))
  }  
}

#This is the main function
#TODO adapt this for Rserve implementation
getMatches <- function(jsonString){
  
  request = fromJSON(jsonString)
  
  if (names(request)[1] == "q0") { #multiple queries 
    #query results are appended to this
    allResultsForAllQueries = list() 
    queryCount = 1
    for (queryRequest in request){
      allResultsForQuery = processMatchingQueryRequest(queryRequest)    
      
      #this tells us which query we're looking at
      #it's usually something like q0, q1, etc
      queryIdentifier = names(request)[queryCount]
      
      #add query results here
      allResultsForAllQueries[queryIdentifier] = list(allResultsForQuery)
      
      queryCount = queryCount + 1
    }
    #send output to calling php code
    #write(toJSON(allResultsForAllQueries), stdout())
    return(toJSON(allResultsForAllQueries))
  } else { #single query
    allResultsForQuery = processMatchingQueryRequest(request)
    #send output to calling php code
    #write(toJSON(allResultsForQuery), stdout())
    return(toJSON(allResultsForQuery))
  }
  
}
