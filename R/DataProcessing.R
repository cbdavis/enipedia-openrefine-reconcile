########## Data Processing ##########

# These functions are used for processing data into more usable forms.

#http://stackoverflow.com/questions/3476782/how-to-check-if-the-number-is-integer
isInteger <- function(N){
  return(!length(grep("[^[:digit:]]", as.character(N))))
}

extractCoordinates <- function(point){
  coords = as.data.frame(matrix(unlist(strsplit(point, split=",")), ncol=2, byrow=TRUE))
  colnames(coords) = c("lat", "lon")
  
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
# This is based partially on the cleanup used for the 
# fingerprint method of Google Refine - https://code.google.com/p/google-refine/wiki/ClusteringInDepth
normalizeText <- function(text){
  text = gsub('http://enipedia.tudelft.nl/wiki/', '', text)
  # remove leading and trailing whitespace
  text = gsub('^ +| +$', '', text)
  # change all characters to their lowercase representation
  text = tolower(text)
  # remove all punctuation and control characters
  text = gsub("[[:punct:]]|[[:cntrl:]]", " ", text)
  # normalize extended western characters to their ASCII representation (for example "gödel" → "godel")
  text = iconv(text, to="ASCII//TRANSLIT") #work with simple ascii - this doesn't do anything to help with misspellings
  # get rid of consecutive spaces  
  text = gsub('  +', ' ', text)
  # TODO Russian plants often have "aya" at the end of the name
  text = gsub("([a-z])centrale( |$)", "\\1 centrale\\2", text) #the Dutch add centrale as a suffix to power plant names
  text = sapply(text, URLdecode)
  text = removeStopWords(text) #remove terms that don't help us with matching
  names(text) = NULL #remove names - this is redundant and causes problems with RUnit tests
  return(text)
}

# based on https://code.google.com/p/google-refine/wiki/ClusteringInDepth
fingerprint <- function(text, minTokenLength=3){
  text = gsub('http://enipedia.tudelft.nl/wiki/', '', text)
  # remove leading and trailing whitespace
  text = gsub('^ +| +$', '', text)
  # change all characters to their lowercase representation
  text = tolower(text)
  # remove all punctuation and control characters
  text = gsub("[[:punct:]]|[[:cntrl:]]", " ", text)
  # normalize extended western characters to their ASCII representation (for example "gödel" → "godel")
  text = iconv(text, to="ASCII//TRANSLIT") #work with simple ascii - this doesn't do anything to help with misspellings
  # get rid of consecutive spaces  
  text = gsub('  +', ' ', text)
  text = unlist(lapply(strsplit(text, " "), function(x){paste(unique(sort(x)), collapse=" ")}))
  text = unlist(lapply(strsplit(text, " "), function(x){locs = which(nchar(x) >= minTokenLength)
                                                        paste(unique(sort(x[locs])), collapse=" ")}))
  return(text)
}

# based on https://code.google.com/p/google-refine/wiki/ClusteringInDepth
ngramFingerprint <- function(text, n=2){
  text = gsub('http://enipedia.tudelft.nl/wiki/', '', text)
  # change all characters to their lowercase representation
  text = tolower(text)
  # remove all punctuation, whitespace, and control characters
  text = gsub("[[:punct:]]|[[:cntrl:]]|[[:space:]]", "", text)
  # normalize extended western characters to their ASCII representation
  text = iconv(text, to="ASCII//TRANSLIT") #work with simple ascii - this doesn't do anything to help with misspellings
  # obtain all the string n-grams
  text = unlist(lapply(text, function(x){ngrams = substring(x, c(1:(nchar(x)-(n-1))), c(n:(nchar(x))))
                                         # sort the n-grams and remove duplicates
                                         ngrams = unique(sort(ngrams))
                                         # join the sorted n-grams back together
                                         return(paste(ngrams, collapse="")) }))
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

# TODO could just load these in from a file
removeStopWords = function(text){
  # TODO should create stop words for different languages and tie these to the different countries
  stopwords = c("les", 
                "sa", 
                "s a", 
                "b v", 
                "bv", 
                "n v", 
                "nv", 
                "de", #spanish
                "des", #spanish 
                "la", #spanish
                "le", #french
                "du", 
                "los", 
                "las", 
                "el", 
                "del", 
                "the",
		            "inc", 
                "ltd", 
                "gmbh", 
                "und", 
                "von", 
                "der", 
                "mit", 
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
  uniqueTokensPerEntry = lapply(data, function(x){unique(sort(strsplit(x, " ")[[1]]))})
    
  tokenIDSequence = c(1:length(uniqueTokensPerEntry))
  numTokensPerEntry = unlist(lapply(uniqueTokensPerEntry, length))
  
  #the matrix can be populated quickly if we pass it a two column matrix
  #where the 1st column is the row index and the 2nd column is the column index
  rowColumnIndices = cbind(rowNameLookup[unlist(uniqueTokensPerEntry)], rep(tokenIDSequence, numTokensPerEntry))
  # get rid of duplicated i j values for the matrix
  rowColumnIndices = rowColumnIndices[!duplicated(rowColumnIndices),]
  # get rid of row names
  row.names(rowColumnIndices) = NULL
  # remove rows with NA
  rowColumnIndices = rowColumnIndices[!is.na(rowColumnIndices[,1]),]
  rowColumnIndices = rowColumnIndices[!is.na(rowColumnIndices[,2]),]
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


getTokenLocs = function(text){
  # export all the tokens to a file
  write.table(t(c("entityID", "token")), file="tokenLocs.txt", append=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
  for (i in c(1:length(text))){
    write.table(cbind(i, strsplit(text[i], " ")[[1]]), file="tokenLocs.txt", append=TRUE, col.names=FALSE, row.names=FALSE, sep="\t")
  }
  df = read.table("tokenLocs.txt", header=TRUE)
  return(df)
}


calculateSelfInformationOfIntersectingTokens_sqldf <- function(data1, data2){
  tokenLocs1 = getTokenLocs(data1$soup)
  tokenLocs2 = getTokenLocs(data2$soup)
  
  #tokenLocs2$entityID = tokenLocs2$entityID + nrow(tokenLocs1) # increment the ids so that they are unique
  
  sqldf("create index token_tokenLocs1_Index on tokenLocs1(token)")
  sqldf("create index token_tokenLocs2_Index on tokenLocs2(token)")
  
  allTokens = rbind(tokenLocs1, tokenLocs2)
  sqldf("create index token_allTokens_Index on allTokens(token)")
  sqldf("create index entityID_allTokens_Index on allTokens(entityID)")
  
  numEntities = nrow(allTokens)
  
  numTokens = length(unique(allTokens$token))
  # ^2 since everything versus everything
  # subtract numTokens since don't count a token matching with itself
  numPermutationsOfTokens = ((numTokens^2) - numTokens)
  
  tokenFrequency = sqldf("select token, count(*) as tokenCount from allTokens group by token")
  sqldf("create index token_tokenFrequency_Index on tokenFrequency(token)")
  
  tokenFrequency$selfInformation = -log10(tokenFrequency$tokenCount/numEntities)
  tokenFrequency$probability = tokenFrequency$tokenCount / nrow(tokenFrequency)
  
  # now try to calculate the self information of intersecting tokens
  resultsSummarized = sqldf("select tokenLocs1.entityID as entity1, 
                            tokenLocs2.entityID as entity2, 
                            sum(tokenFrequency.selfInformation) as totalSelfInfo, 
                            count(*) as numIntersectingTokens FROM tokenLocs1 JOIN tokenLocs2 ON tokenLocs1.token == tokenLocs2.token 
                            JOIN tokenFrequency ON tokenLocs1.token == tokenFrequency.token GROUP BY entity1, entity2")
  
  # calculate mutual information of terms
  
  # This is used to calculate Pxy
  tokenCoOccurrenceTable = sqldf("select T1.token as token1, 
                                 T2.token as token2, 
                                 COUNT(*) as combinationCount 
                                 FROM allTokens T1 
                                 JOIN allTokens T2 
                                 ON T1.entityID == T2.entityID 
                                 WHERE T1.token != T2.token 
                                 GROUP BY token1, token2")
  
  tokenCoOccurrenceTable$Pxy = tokenCoOccurrenceTable$combinationCount / numPermutationsOfTokens
  
  # calculating mutual information of intersecting tokens
  mutualInformationOfIntersectingTokens = sqldf("select tokenCoOccurrenceTable.token1, 
                                                tokenCoOccurrenceTable.token2, 
                                                tokenCoOccurrenceTable.Pxy as Pxy, 
                                                TF1.probability as Px, 
                                                TF2.probability as Py, 
                                                tokenCoOccurrenceTable.Pxy *log10(tokenCoOccurrenceTable.Pxy/(TF1.probability*TF2.probability)) as mutualInformation
                                                FROM tokenCoOccurrenceTable 
                                                JOIN tokenFrequency TF1 
                                                ON tokenCoOccurrenceTable.token1 == TF1.token
                                                JOIN tokenFrequency TF2 
                                                ON tokenCoOccurrenceTable.token2 == TF2.token")
  
  # also calculate sum of mutual information of intersecting tokens for each combination of entities
  intersectingTokensBetweenEntities = sqldf("select tokenLocs1.entityID as entity1, 
                                            tokenLocs2.entityID as entity2, 
                                            tokenLocs1.token as token  
                                            FROM tokenLocs1 
                                            JOIN tokenLocs2 
                                            ON tokenLocs1.token == tokenLocs2.token 
                                            ORDER BY entity1, entity2")
  
  # this takes forever!  not sure how much value it adds
  # mutualInformationBetweenMatchingEntities = sqldf("select TokenTable1.entity1 as entity1, 
  #                                                   TokenTable1.entity2 as entity2, 
  #                                                   sum(mutualInformationOfIntersectingTokens.mutualInformation) as summedMutualInformation
  #                                                   FROM intersectingTokensBetweenEntities TokenTable1
  #                                                   JOIN intersectingTokensBetweenEntities TokenTable2 
  #                                                   ON TokenTable1.entity1 ==  TokenTable2.entity1 
  #                                                   AND TokenTable1.entity2 ==  TokenTable2.entity2 
  #                                                   AND TokenTable1.token != TokenTable2.token 
  #                                                   JOIN mutualInformationOfIntersectingTokens 
  #                                                   ON mutualInformationOfIntersectingTokens.token1 == TokenTable1.token 
  #                                                   AND mutualInformationOfIntersectingTokens.token2 == TokenTable2.token 
  #                                                   GROUP BY entity1, entity2")
  
  
  #mutualInformation = pxy *log(pxy/(px*py))
  # which.max(mutualInformationBetweenMatchingEntities$summedMutualInformation)
  # mutualInformationBetweenMatchingEntities[which.max(mutualInformationBetweenMatchingEntities$summedMutualInformation),]
  # entity1 entity2 summedMutualInformation
  # 1292      43   25364           -1.050503e-08
  # data1$soup[43]
  # "elverlingsen energie nordrhein sudwestfalen wasser werdohl westfalen"
  # data2$soup[25364-nrow(tokenLocs1)]
  # "236 aktiengesellschaft an bundesstrasse der e elverlingsen kraftwerk mark werdohl"
  
  # TODO try to find the self information of a partitioning - shows the uniqueness of terms in this partitioning/intersection of tokens
  #sum(((1 - ((tokenFrequency.tokenCount - 2)/tokenFrequency.tokenCount)) * log10((1 - ((tokenFrequency.tokenCount - 2)/tokenFrequency.tokenCount))))) AS selfInfoPartitioning
  selfInformationOfPartitioningPerMatchedEntities = sqldf("SELECT intersectingTokensBetweenEntities.entity1 AS entity1, 
                                                          intersectingTokensBetweenEntities.entity2 AS entity2, 
                                                          count(*) as numTokens, 
                                                          -sum(((1.0 - ((cast(tokenFrequency.tokenCount as real) - 2.0)/tokenFrequency.tokenCount)) * log10((1.0 - ((cast(tokenFrequency.tokenCount as real) - 2.0)/cast(tokenFrequency.tokenCount as real)))))) AS selfInfoPartitioning
                                                          FROM intersectingTokensBetweenEntities 
                                                          JOIN tokenFrequency 
                                                          ON tokenFrequency.token == intersectingTokensBetweenEntities.token 
                                                          GROUP BY entity1, entity2 ORDER BY entity1, selfInfoPartitioning DESC")
  
  # there are two links between each combination of candiates
  # entity1 -> entity2
  # entity2 -> entity1
  
  
  # TODO now get the highest matches for each entity1 - the query above is sorted
  numTopMatches = 5
  isDuplicated = duplicated(selfInformationOfPartitioningPerMatchedEntities$entity1)
  #if not duplicated, then it's the highest value
  locs = which(isDuplicated==FALSE)
  #create sequences of length numTopMatches starting with the locations of the highest values (locs with FALSE for isDuplicated)
  # This is then used to return the top numTopMatches rows for each value of entity1
  keepTheseRows = unique(sort(unlist(lapply(locs, function(x){c(x:(x+numTopMatches-1))}))))
  selfInformationOfPartitioningPerMatchedEntities = selfInformationOfPartitioningPerMatchedEntities[keepTheseRows,]
  
  selfInformationOfPartitioningPerMatchedEntities = sqldf("SELECT * 
                                                        FROM selfInformationOfPartitioningPerMatchedEntities 
                                                        ORDER BY entity2, selfInfoPartitioning DESC")
  
  isDuplicated = duplicated(selfInformationOfPartitioningPerMatchedEntities$entity2)
  #if not duplicated, then it's the highest value
  locs = which(isDuplicated==FALSE)
  #create sequences of length numTopMatches starting with the locations of the highest values (locs with FALSE for isDuplicated)
  # This is then used to return the top numTopMatches rows for each value of entity1
  keepTheseRows = unique(sort(unlist(lapply(locs, function(x){c(x:(x+numTopMatches-1))}))))
  selfInformationOfPartitioningPerMatchedEntities = selfInformationOfPartitioningPerMatchedEntities[keepTheseRows,]
  
  matchCountPerEntity = sqldf("SELECT entity1, 
                            COUNT(*) AS matchCount 
                            FROM selfInformationOfPartitioningPerMatchedEntities 
                            GROUP BY entity1 ORDER BY matchCount DESC")
  
  
  row.names(selfInformationOfPartitioningPerMatchedEntities) = NULL
  
  # track down the entries, figure out which tokens they have in common, then dump the data out in a spreadsheet
  # also find the mutual best matches
  
  # remove NA rows
  selfInformationOfPartitioningPerMatchedEntities = selfInformationOfPartitioningPerMatchedEntities[complete.cases(selfInformationOfPartitioningPerMatchedEntities),]
  
  # for every entry in data2, show the top 5 matches
  selfInformationOfPartitioningPerMatchedEntities = sqldf("select * from selfInformationOfPartitioningPerMatchedEntities order by entity2, selfInfoPartitioning DESC")

  return(selfInformationOfPartitioningPerMatchedEntities)
}

#soup vectors must be present in both data1 and data2 - matching will be done on these
#The input consists of two vectors consisting of strings which can be further tokenized
calculateSelfInformationOfIntersectingTokens <- function(data1, data2, useFuzzyTokenMatches=FALSE, equivalenceScore=0.9){

  # normalize the data
  data1 = normalizeText(data1)
  data2 = normalizeText(data2)
  
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
