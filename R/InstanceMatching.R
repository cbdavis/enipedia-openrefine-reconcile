#never ever ever convert strings to factors
options(stringsAsFactors = FALSE)

library(SPARQL) #query Enipedia
library(rjson) #json is used for input/output data
library(reshape) #colsplit
library(gdata) #trim
library(RecordLinkage) #string matching
library(geosphere) #distance calculations
library(Matrix) #sparse matrices
library(RCurl) # download data from the internet - used for DataRetrieval.R

#This is the main function that processes reconciliation requests from Open Refine
getMatches <- function(jsonString){
  
  request = fromJSON(jsonString)
  
  if (names(request)[1] == "q0") { #multiple queries 
    #query results are appended to this
    allResultsForAllQueries = list() 
    queryCount = 1
    for (queryRequest in request){
      
      print(queryRequest)
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

#processes single query requests
processMatchingQueryRequest <- function (queryRequest, numResults=5) {  
  
  #see what thing we're matching on
  if (queryRequest$type == "Category:Energy_Company"){
    return(matchEnergyCompany(queryRequest, numResults))
  } else { #assume we're trying to match power plants
    return(matchPowerPlants(queryRequest, numResults))
  }  
}


########## Instance Matching ##########

matchPowerPlants <- function(queryRequest, numResults=5){
  externalData = convertQueryRequestToVector(queryRequest)
  
  #TODO need some check to correct the country - find closest match if slightly misspelled
  #or give feedback to the user about what they should do
  
  enipediaData = retrieveCountryDataFromEnipedia(externalData$country)
  
  # better matching algorithms can be plugged in here instead
  indicesOfCandidateMatches = matchPowerPlantEntity(externalData, enipediaData, numResults)

  #only consider the number of top candidates specified by numResults
  #this assumes that indicesOfCandidateMatches is sorted with the best candidates first
  if (length(indicesOfCandidateMatches) > numResults) {
    indicesOfCandidateMatches = indicesOfCandidateMatches[c(1:numResults)]
  }
  
  allResultsForQuery = list()
  for (loc in indicesOfCandidateMatches){
    resultSet = list(id=enipediaData$x[loc],
                     name=paste('name:',enipediaData$name[loc],'|',
                                'owner:',normalizeText(enipediaData$owner[loc]), '|',
                                'city:',normalizeText(enipediaData$city[loc]), sep=""),
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

# This is the main matching function, and functions for alternative matching strategies should follow this template 
# so that it's easy to just drop in improved versions of the code.
#
# externalData is a vector for which externalData$plant and externalData$country must be specified.
# Other values checked for are owner, state, latitude, longitude
matchPowerPlantEntity <- function(externalData, enipediaData, numResults=5){
  
  #perform string matching
  ldiff = levenshteinSim(normalizeText(externalData$plant), 
                         enipediaData$CleanedPlantName)
  
  jdiff = jarowinkler(normalizeText(externalData$plant), 
                      enipediaData$CleanedPlantName, 
                      r=0.5)
  
  jaccard_index_values = unlist(lapply(enipediaData$CleanedPlantName, function(x) {jaccard_index(x,normalizeText(externalData$plant))}))
  
  if(!is.null(externalData$state)){
    ldiffState = levenshteinSim(externalData$state, 
                                enipediaData$CleanedStateName)
    
    jdiffState = jarowinkler(externalData$state, 
                             enipediaData$CleanedStateName, 
                             r=0.5)
  }
  
  if(!is.null(externalData$owner)){
    ldiffOwner = levenshteinSim(externalData$owner, 
                                enipediaData$CleanedOwnerName)
    
    jdiffOwner = jarowinkler(externalData$owner, 
                             enipediaData$CleanedOwnerName, 
                             r=0.5)
    
    #Takes too long
    #jaccard_index_values_owner = unlist(lapply(enipediaData$CleanedOwnerName, function(x) {jaccard_index(x,enipediaData$owner)}))
  }
  
  #TODO allow json query strings to specify this
  distanceCutoff = 20000
  distanceScores = NULL
  if(!is.null(latitude) && !is.null(externalData$longitude)){
    
    distances = distCosine(cbind(externalData$longitude, 
                                 externalData$latitude), 
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
  
  summedSquareOfDistances = ldiff^2 + jdiff^2 # + jaccard_index_values^2
  
  #add in additional values if we have done the calculations
  if (!is.null(distanceScores)){
    summedSquareOfDistances = summedSquareOfDistances + distanceScores^2
  } 
  
  if (externalData$owner != ""){
    summedSquareOfDistances = summedSquareOfDistances + ldiffOwner^2 + jdiffOwner^2
  }
  
  if (externalData$state != ""){
    summedSquareOfDistances = summedSquareOfDistances + ldiffState^2 + jdiffState^2
  }
  
  dist = sqrt(summedSquareOfDistances)
  
  locs = sort(dist, decreasing=TRUE, index.return=TRUE)$ix[c(1:numResults)]
  
  return(locs)
}


matchEnergyCompany <- function (queryRequest, numResults=5) {
  company = queryRequest$query
  
  #Don't query this if we already have it
  enipediaData = retrieveCompanyDataFromEnipedia()
  enipediaCleanedName = normalizeText(enipediaData$name)
  ldiff = levenshteinSim(normalizeText(queryRequest$query), 
                         enipediaCleanedName)
  
  jdiff = jarowinkler(normalizeText(queryRequest$query), 
                      enipediaCleanedName, 
                      r=0.5)
  
  jaccard_index_values = unlist(lapply(enipediaCleanedName, function(x) {jaccard_index(x,normalizeText(queryRequest$query))}))
  
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