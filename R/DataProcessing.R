########## Data Processing ##########

# These functions are used for processing data into more usable forms.

#http://stackoverflow.com/questions/3476782/how-to-check-if-the-number-is-integer
isInteger <- function(N){
  return(!length(grep("[^[:digit:]]", as.character(N))))
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

#convert text to the most boring form possible
#this makes it easier to perform string comparisons
normalizeText <- function(text){
  #TODO can have most of this be one giant regex search and replace
  text = iconv(text, to="ASCII//TRANSLIT") #work with simple ascii - this doesn't do anything to help with misspellings
  text = trim(tolower(text)) #everyone to lower case to make further processing easier
  text = gsub('http://enipedia.tudelft.nl/wiki/', '', text)
  text = gsub('\\)', '', text)
  text = gsub('\\(', '', text)
  text = gsub('/', ' ', text)
  text = gsub(',', ' ', text)
  text = gsub('\\.', ' ', text)
  text = gsub('&', '', text)
  text = gsub('_', ' ', text)
  text = gsub('-', ' ', text)
  text = gsub(':', ' ', text)
  text = gsub('  ', ' ', text)
  text = gsub("'", " ", text)
  text = gsub("\n", " ", text)
  text = gsub('"', " ", text)
  text = gsub("\\\\", " ", text)
  text = gsub("([a-z])centrale( |$)", "\\1 centrale\\2", text) #the Dutch add centrale as a suffix to power plant names
  text = sapply(text, URLdecode)
  text = removeStopWords(text) #remove terms that don't help us with matching
  names(text) = NULL #remove names - this is redundant and causes problems with RUnit tests
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
      externalData$owner = normalizeText(externalData$owner)
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
  text = normalizeText(text)
  tokenList = unique(unlist(strsplit(text, split=" ")))
  return(tokenList)
}

#TODO could just load these in from a file
removeStopWords = function(text){
  stopwords = c("les", 
                "sa", 
                "s a", 
                "b v", 
                "bv", 
                "n v", 
                "nv", 
                "de", 
                "des", 
                "la", 
                "le", 
                "du", 
                "los", 
                "las", 
                "el", 
                "del", 
                "the"
                "power plant", 
                "power station")
  for (stopword in stopwords){
    #the stopword can be at the start, middle or end of text
    searchTerm = paste("( |^)", stopword,"( |$)", sep="")
    text = gsub(searchTerm, " ", text)
  }
  #convert consecutive whitespace to single space
  text = gsub("[ ]{2,}", " ", text)
  #trim whitespace
  text = gsub("^\\s+|\\s+$", "", text)
  return(text)
}

#removeNumbers only works for integers currently
#this expects a a single string
#minTokenLength can be disabled by using values <= 0
getUniqueTokens <- function(charVector, removeNumbers=TRUE, minTokenLength=3){
  allUniqueTokens = sort(unique(unlist(strsplit(paste(charVector, collapse=" "), split=" "))))
  allUniqueTokens = normalizeText(allUniqueTokens)
  if (removeNumbers == TRUE){
    allUniqueTokens = allUniqueTokens[!aaply(allUniqueTokens, .margins=1, .fun=isInteger)]
  }
  if (minTokenLength > 0){
    allUniqueTokens = allUniqueTokens[nchar(allUniqueTokens) >= minTokenLength]
  }
  return(allUniqueTokens)
}

#this shows which entries contain which tokens
#removeNumbers only works for integers currently
getTokenLookupMap <- function(charVector, removeNumbers=TRUE){
  #must declare as a list as there are likely multiple entries that contain this token
  tokenLookup = list()
  charVector = normalizeText(charVector)
  allUniqueTokens = getUniqueTokens(charVector, removeNumbers)
  
  #this shows which of the entries contain which of the tokens
  tokenLookup[allUniqueTokens] = NULL
  curIndex = 1
  for (name in charVector){
    nameTokens = unlist(strsplit(name, split=" "))
    for (token in nameTokens){
      token = normalizeText(token)
      if (removeNumbers==TRUE){ #add extra check to make sure that we aren't adding numbers
        if (!isInteger(token)){ 
          tokenLookup[[token]] = append(tokenLookup[[token]], curIndex)  
        }
      } else {
        tokenLookup[[token]] = append(tokenLookup[[token]], curIndex)    
      }
    }
    curIndex = curIndex + 1
  }
  return(tokenLookup)
}

#This code constructs a matrix where rows represent tokens and columns represent entities in a particular data set
#The presence of a 1 indicates that the token is present in a particular entity in a data set
#allTokens means all the tokens for both data sets
getTokenEntityMatrix <- function(allTokens, data){
  tokensMatrixData = sparseMatrix(i=length(allTokens), j=length(data), x=0)
  rownames(tokensMatrixData) = allTokens
  
  #lookup vector that helps us find the index for a particular token
  rowNameLookup = c(1:length(allTokens))
  names(rowNameLookup) = allTokens

  #TODO this can be parallelized
 
  #returns a list where each entry is a vector of tokens for that entity
  uniqueTokensPerEntry = lapply(data, getUniqueTokens)
  
  tokenIDSequence = c(1:length(uniqueTokensPerEntry))
  numTokensPerEntry = unlist(lapply(uniqueTokensPerEntry, length))
  
  #the matrix can be populated quickly if we pass it a two column matrix
  #where the 1st column is the row index and the 2nd column is the column index
  rowColumnIndices = cbind(rowNameLookup[unlist(uniqueTokensPerEntry)], rep(tokenIDSequence, numTokensPerEntry))
  # get rid of duplicated i j values for the matrix
  rowColumnIndices = rowColumnIndices[!duplicated(rowColumnIndices),]
  tokensMatrixData[rowColumnIndices] = 1
  
  return(tokensMatrixData)
}

#takes as input two matrices for two different data sets
#rows are all the tokens (with the same order for both matrices)
#columns are entities in each data set
#These are essentially adjacency matrices indicating that a particular token is found within a particular data entity
#This method does fuzzy string matching so it's easier to find alternative forms of the same token in use
updateTokensEntityMatrixWithFuzzyStringMatch <- function(tokensMatrixData1, tokensMatrixData2, allTokens, equivalenceScore=0.9){
  #TODO this code is a bit slow - possible to parallelize?  Need to profile and find the bottlenecks
    
  # Don't have to compare all tokens versus all other tokens since double comparisons with occur
  # Here we compare the token at position i with those at i+1 to n
  # and then increment i by 1 until everything has been compared
  
  offset = 1
  token = head(allTokens, n=1)
  tokensToCompare = tail(allTokens, n=-1)

  equivalentEntries = list()
  
  while(length(tokensToCompare) > 0){
    print(token)
    jwScore = jarowinkler(token, tokensToCompare)
    lScore = levenshteinSim(token, tokensToCompare)
    token = head(tokensToCompare, n=1)
    tokensToCompare = tail(tokensToCompare, n=-1)
    locs = which(jwScore > equivalenceScore | lScore > equivalenceScore)
    if (length(locs) > 1)  {
      originalLoc = offset #add in offset since we're removing tokens from the start of the vector
      equivalentLocs = locs + offset
      equivalentEntries[originalLoc] = equivalentLocs
      
      colsums = colSums(tokensMatrixData1[locs,])
      nonZeroLocs = which(colsums > 0)
      #it's possible that none off the tokens may be used in this data set, but appear in another data set represented by a different matrix
      if (length(nonZeroLocs) > 0){ 
       tokensMatrixData1[locs,nonZeroLocs] = 1
      }
      
      colsums = colSums(tokensMatrixData2[locs,])
      nonZeroLocs = which(colsums > 0)
      #it's possible that none off the tokens may be used in this data set, but appear in another data set represented by a different matrix
      if (length(nonZeroLocs) > 0){ 
       tokensMatrixData2[locs,nonZeroLocs] = 1
      }
    }
    offset = offset + 1
  }
  
  
#   
#   for (token in allTokens){
#     print(token)
#     jwScore = jarowinkler(token, allTokens)
#     lScore = levenshteinSim(token, allTokens)
# 
#     #if either the Jaro Winkler or Levenshtein metric are greater than the equivalence score
#     #then consider the tokens to be the same
#     locs = which(jwScore > equivalenceScore | lScore > equivalenceScore)
#     
#     if (length(locs) > 1)  {
#     }
#     
# #      if (length(locs) > 1)  {
# #        colsums = colSums(tokensMatrixData1[locs,])
# #        nonZeroLocs = which(colsums > 0)
# #        #it's possible that none off the tokens may be used in this data set, but appear in another data set represented by a different matrix
# #        if (length(nonZeroLocs) > 0){ 
# #          tokensMatrixData1[locs,nonZeroLocs] = 1
# #        }
# #        
# #        colsums = colSums(tokensMatrixData2[locs,])
# #        nonZeroLocs = which(colsums > 0)
# #        #it's possible that none off the tokens may be used in this data set, but appear in another data set represented by a different matrix
# #        if (length(nonZeroLocs) > 0){ 
# #          tokensMatrixData2[locs,nonZeroLocs] = 1
# #        }
# #      }
#   }
  return(list(tokensMatrixData1, tokensMatrixData2))
}


#soup vectors must be present in both data1 and data2 - matching will be done on these
#The input consists of two vectors consisting of strings which can be further tokenized
calculateSelfInformationOfIntersectingTokens <- function(data1, data2, useFuzzyTokenMatches=FALSE, equivalenceScore=0.9){

  allTokens = getUniqueTokens(c(data1, data2))
  
  rowNameLookup = c(1:length(allTokens))
  names(rowNameLookup) = allTokens

  #these are matrices where the presence of a 1 indicates that the token (row) is present in an entity (column)
  tokensMatrixData1 = getTokenEntityMatrix(allTokens, data1)
  tokensMatrixData2 = getTokenEntityMatrix(allTokens, data2)
  
  if (useFuzzyTokenMatches == TRUE){
    tmp = updateTokensEntityMatrixWithFuzzyStringMatch(tokensMatrixData1, tokensMatrixData2, allTokens, equivalenceScore)
    tokensMatrixData1 = tmp[[1]]
    tokensMatrixData2 = tmp[[2]]
    rm(tmp)
  }
  
  #self information is calculated by looking at all of the tokens in both data sets
  rowSumMatrix1 = rowSums(tokensMatrixData1)
  rowSumMatrix2 = rowSums(tokensMatrixData2)
  countPerToken = rowSumMatrix1 + rowSumMatrix2
  numEntities = length(data1) + length(data2)
  selfInformation = -log10(countPerToken / numEntities) #basically, how often does this token occur in the data
  names(selfInformation) = allTokens
  
  # TODO this needs to be verified
  # Calculating tokensMatrixData2 * selfInformation converts the 1's in the matrix 
  # (which represents that a token is contained in a particular entity)
  # to the value of the self information for that token
  # Multiplying this then by t(tokensMatrixData1) then zeros out values for tokens not shared by both entities
  # and also adds up the self information scores
  selfInformationOfEntitiesWithIntersectingTokens = t(tokensMatrixData1) %*% (tokensMatrixData2 * selfInformation)

  #return multiple types of data that may be useful in further processing
  dataToReturn = list(selfInformationOfEntitiesWithIntersectingTokens, tokensMatrixData1, tokensMatrixData2, allTokens)
  names(dataToReturn) = c("selfInformationOfEntitiesWithIntersectingTokens", "tokensMatrixData1", "tokensMatrixData2", "allTokens")
  return(dataToReturn)
}

# This only returns the entities from two data sets that are each other's best matching candidates
# This function is an attempt to give the user first the (likely) obvious matches
returnMutualBestCandidates <- function(selfInfoOfIntersections){
  rowMaxLocations = apply(selfInfoOfIntersections, MARGIN=1, FUN=which.max)
  colMaxLocations = apply(selfInfoOfIntersections, MARGIN=2, FUN=which.max)
  
  colsToKeep = c()
  rowsToKeep = c()
  for (colNum in c(1:length(colMaxLocations))){
    if (colNum == rowMaxLocations[colMaxLocations[colNum]]){
      #keep this as a probable match - these entities correspond to each other better than any other entitiy
      colsToKeep = c(colsToKeep, colNum)
      rowsToKeep = c(rowsToKeep, colMaxLocations[colNum])
    } 
  }
  
  rowColumnsOfBestMatches = cbind(rowsToKeep, colsToKeep)
  
  return(rowColumnsOfBestMatches)
}

calculateMutualInformationOfIntersectingTokens <- function(tokensMatrixData1, tokensMatrixData2){
  #find probability of tokens occurring
  rowSumMatrix1 = rowSums(tokensMatrixData1)
  rowSumMatrix2 = rowSums(tokensMatrixData2)
  countPerToken = rowSumMatrix1 + rowSumMatrix2
  numEntities = dim(tokensMatrixData1)[2] + dim(tokensMatrixData2)[2]
  tokenProbability = countPerToken / numEntities #basically, how often does this token occur in the data
  
  # Now need to find co-occurrence of tokens.  We know Pi, Pj, but not Pij
  
  # This should give me an indication of which tokens co-occur the most
  test = tokensMatrixData1 %*% t(tokensMatrixData1)
  # zero out the diagonal, basically says that a token co-occurs with itself
  # this is more like how many times does it occur in the data, which could be useful if can figure out a smart way to use this information
  diag(test) = 0
  
  allTokens = something$allTokens
  # this shows which tokens co-occur the most
  allTokens[which(test == max(test), arr.ind = TRUE)]
  # matrix indicating the probability that two tokens will co-occur
  Pij = test / numEntities
  
  PiPj = tokenProbability %*% t(tokenProbability)
  
  intersectingTokens = which(test > 0, arr.ind = TRUE)
  
  Pi = tokenProbability[intersectingTokens[,1]]
  Pj = tokenProbability[intersectingTokens[,2]]
  
  mutualInformation = Pij[intersectingTokens] * log2(Pij[intersectingTokens] / (Pi * Pj))
  
  return(mutualInformation)
}