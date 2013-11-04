options(stringsAsFactors = FALSE)

getTokenLocs = function(text){
  # export all the tokens to a file
  write.table(t(c("entityID", "token")), file="tokenLocs.txt", append=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
  for (i in c(1:length(text))){
    token = strsplit(text[i], " ")[[1]]
    if (length(token) > 0){
      write.table(cbind(i, token), file="tokenLocs.txt", append=TRUE, col.names=FALSE, row.names=FALSE, sep="\t")
    }
  }
  df = read.table("tokenLocs.txt", header=TRUE)
  return(df)
}

findMutualBestMatches = function(selfInformationOfPartitioningPerMatchedEntities){
  entity1 = selfInformationOfPartitioningPerMatchedEntities$entity1
  entity2 = selfInformationOfPartitioningPerMatchedEntities$entity2
  
  selfInfoMatrix = sparseMatrix(i=max(entity1), j=max(entity2), x=0)
  rowColumnIndices = cbind(entity1, entity2)
  selfInfoMatrix[rowColumnIndices] = selfInformationOfPartitioningPerMatchedEntities$totalSelfInfo
  mutualBestCandidatesLocs = returnMutualBestCandidates(selfInfoMatrix)
  # take a subset of candidatesInfo
  
  indices = c(1:nrow(mutualBestCandidatesLocs))
  
  mutualBestRows = unlist(lapply(indices, function(x){
    row = mutualBestCandidatesLocs[x,1]
    col = mutualBestCandidatesLocs[x,2]
    loc = intersect(which(selfInformationOfPartitioningPerMatchedEntities$entity1 == row), 
                    which(selfInformationOfPartitioningPerMatchedEntities$entity2 == col))
    return(loc)
  }))
}

calculateSelfInformation = function(soup1, soup2, numTopMatches = 5){
  # just sum up the self information of the tokens that two entities have in common 
  if (class(soup1) == "character"){
    soup1 = data.frame(soup=soup1)
  }
  if (class(soup2) == "character"){
    soup2 = data.frame(soup=soup2)
  }
  
  tokenLocs1 = getTokenLocs(soup1$soup)
  tokenLocs2 = getTokenLocs(soup2$soup)
  
  # increment the ids so that they are unique
  # this is needed due to the allTokens data frame
  # which keeps track of the tokens found in each entry
  tokenLocs2$entityID = tokenLocs2$entityID + nrow(tokenLocs1) 
  
  sqldf("create index token_tokenLocs1_Index on tokenLocs1(token)")
  sqldf("create index token_tokenLocs2_Index on tokenLocs2(token)")
  
  allTokens = rbind(tokenLocs1, tokenLocs2)
  sqldf("create index token_allTokens_Index on allTokens(token)")
  sqldf("create index entityID_allTokens_Index on allTokens(entityID)")
  
  # there are the data entities, so all the entries from data set 1 and data set 2
  numEntities = nrow(allTokens)
  
  numTokens = length(unique(allTokens$token))
  # ^2 since everything versus everything
  # subtract numTokens since don't count a token matching with itself
  numPermutationsOfTokens = ((numTokens^2) - numTokens)
  
  tokenFrequency = sqldf("select token, count(*) as tokenCount from allTokens group by token")
  sqldf("create index token_tokenFrequency_Index on tokenFrequency(token)")
  
  tokenFrequency$selfInformation = -log10(tokenFrequency$tokenCount/numEntities)
  tokenFrequency$probability = tokenFrequency$tokenCount / nrow(tokenFrequency)
  
  # now try to calculate the sum of the self information of intersecting tokens
  resultsSummarized = sqldf("select tokenLocs1.entityID as entity1, 
                            tokenLocs2.entityID as entity2, 
                            sum(tokenFrequency.selfInformation) as totalSelfInfo, 
                            count(*) as numIntersectingTokens 
                            FROM tokenLocs1 
                            JOIN tokenLocs2 ON tokenLocs1.token == tokenLocs2.token 
                            JOIN tokenFrequency ON tokenLocs1.token == tokenFrequency.token 
                            GROUP BY entity1, entity2")
  
  # check if there are any matched terms at all
  if (nrow(resultsSummarized) > 0){
    resultsSummarized = sqldf("select * from resultsSummarized order by entity1, totalSelfInfo DESC, numIntersectingTokens DESC")

    isDuplicated = duplicated(resultsSummarized$entity1)
    #if not duplicated, then it's the highest value
    locs = which(isDuplicated==FALSE)
    #create sequences of length numTopMatches starting with the locations of the highest values (locs with FALSE for isDuplicated)
    # This is then used to return the top numTopMatches rows for each value of entity1
    keepTheseRows = unique(sort(unlist(lapply(locs, function(x){c(x:(x+numTopMatches-1))}))))
    resultsSummarized = resultsSummarized[keepTheseRows,]
    
    resultsSummarized$entity2 = resultsSummarized$entity2 - nrow(tokenLocs1)
    
    return(resultsSummarized)
  } else {
    # no matching tokens were found at all
    return(NULL)
  }
}


# Not currently used
# An issue with this is that if a token only appears in one partition 
# (i.e. in the set of intersecting tokens of a possible match), then it 
# contributes nothing to the overall score since its probability of occuring 
# within that set is 100% and log(1) = 0
calculateSelfInformationOfPartitioning = function(soup1, soup2, numTopMatches = 5){
  if (class(soup1) == "character"){
    soup1 = data.frame(soup=soup1)
  }
  if (class(soup2) == "character"){
    soup2 = data.frame(soup=soup2)
  }
  
  tokenLocs1 = getTokenLocs(soup1$soup)
  tokenLocs2 = getTokenLocs(soup2$soup)
  
  # increment the ids so that they are unique
  # this is needed due to the allTokens data frame
  # which keeps track of the tokens found in each entry
  tokenLocs2$entityID = tokenLocs2$entityID + nrow(tokenLocs1) 
  
  sqldf("create index token_tokenLocs1_Index on tokenLocs1(token)")
  sqldf("create index token_tokenLocs2_Index on tokenLocs2(token)")
  
  allTokens = rbind(tokenLocs1, tokenLocs2)
  sqldf("create index token_allTokens_Index on allTokens(token)")
  sqldf("create index entityID_allTokens_Index on allTokens(entityID)")
  
  # there are the data entities, so all the entries from data set 1 and data set 2
  numEntities = nrow(allTokens)
  
  numTokens = length(unique(allTokens$token))
  # ^2 since everything versus everything
  # subtract numTokens since don't count a token matching with itself
  numPermutationsOfTokens = ((numTokens^2) - numTokens)
  
  tokenFrequency = sqldf("select token, count(*) as tokenCount from allTokens group by token")
  sqldf("create index token_tokenFrequency_Index on tokenFrequency(token)")
  
  tokenFrequency$selfInformation = -log10(tokenFrequency$tokenCount/numEntities)
  tokenFrequency$probability = tokenFrequency$tokenCount / nrow(tokenFrequency)
  
  entity_entropy = sqldf("select allTokens.entityID as entityID, 
                          sum((tokenFrequency.probability * log10(tokenFrequency.probability))) as entropy 
                          FROM allTokens 
                          JOIN tokenFrequency ON allTokens.token == tokenFrequency.token 
                          GROUP BY entityID")
  
  # now try to calculate the self information of intersecting tokens
  # TODO this isn't actually used anywhere else, old code
  resultsSummarized = sqldf("select tokenLocs1.entityID as entity1, 
                            tokenLocs2.entityID as entity2, 
                            sum(tokenFrequency.selfInformation) as totalSelfInfo, 
                            count(*) as numIntersectingTokens 
                            FROM tokenLocs1 
                            JOIN tokenLocs2 ON tokenLocs1.token == tokenLocs2.token 
                            JOIN tokenFrequency ON tokenLocs1.token == tokenFrequency.token 
                            GROUP BY entity1, entity2")
  
  # check if there are any matched terms at all
  if (nrow(resultsSummarized) > 0){
    
    # calculate mutual information of terms
    
    # This is used to calculate Pxy
    # this is the co-occurence of tokens within a single entity
    tokenCoOccurrenceTable = sqldf("select T1.token as token1, 
                                   T2.token as token2, 
                                   COUNT(*) as combinationCount 
                                   FROM allTokens T1 
                                   JOIN allTokens T2 
                                   ON T1.entityID == T2.entityID 
                                   WHERE T1.token != T2.token 
                                   GROUP BY token1, token2")
    
    tokenCoOccurrenceTable$Pxy = tokenCoOccurrenceTable$combinationCount / numPermutationsOfTokens
    
    # include in Px and Py to help with later calculations
    tokenCoOccurrenceTable = sqldf("select token1, 
                                    token2, 
                                    combinationCount, 
                                    Pxy, 
                                    TF1.probability as Px, 
                                    TF2.probability as Py 
                                    FROM tokenCoOccurrenceTable 
                                    JOIN tokenFrequency TF1 ON TF1.token == token1 
                                    JOIN tokenFrequency TF2 ON TF2.token == token2")  
    
    # TODO this isn't used currently
    # There's a problem here if the entry consists of only a single word
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
    
    
    # get all the permutations of terms shared between two entities
    # P(x,y) should be zero for terms that don't intersect
    mutualInformation_X_Y = sqldf("select T1.entity1 as entity1, 
                                    T1.entity2 as entity2, 
                                    sum(tokenCoOccurrenceTable.Pxy * log10(tokenCoOccurrenceTable.Pxy / (tokenCoOccurrenceTable.Px * tokenCoOccurrenceTable.Py))) as mutualInformation, 
                                    count(T1.token) as permutations 
                                    FROM intersectingTokensBetweenEntities T1 
                                    JOIN intersectingTokensBetweenEntities T2
                                    ON T1.entity1 == T2.entity1 AND
                                    T1.entity2 == T2.entity2 AND
                                    T1.token != T2.token 
                                    JOIN tokenCoOccurrenceTable ON 
                                    T1.token == tokenCoOccurrenceTable.token1 AND
                                    T2.token == tokenCoOccurrenceTable.token2
                                    GROUP BY T1.entity1, T1.entity2")
    
    # http://en.wikipedia.org/wiki/Variation_of_information
    # This doesn't seem useful, some matches seem really good, while other seem like nonsense
    variation_of_information = sqldf("select entity1, 
                                      entity2, 
                                      (E1.entropy + E2.entropy - (2*mutualInformation)) as variationOfInformation, 
                                      E1.entropy AS entropy_x, 
                                      E2.entropy AS entropy_y 
                                      FROM mutualInformation_X_Y 
                                      JOIN entity_entropy E1 ON E1.entityID == entity1 
                                      JOIN entity_entropy E2 ON E2.entityID == entity2")
    
    variation_of_information$entity2 = variation_of_information$entity2 - nrow(tokenLocs1)
    
    # find the self information of a partitioning 
    # shows the uniqueness of terms in this partitioning/intersection of tokens
    # for selfInfoPartitioning, 2 is used since that there are technically two terms that are intersecting in the partition
    #-sum(((1.0 - ((cast(tokenFrequency.tokenCount as real) - 2.0)/tokenFrequency.tokenCount)) * log10((1.0 - ((cast(tokenFrequency.tokenCount as real) - 2.0)/cast(tokenFrequency.tokenCount as real)))))) AS selfInfoPartitioning
    
    
    selfInformationOfPartitioningPerMatchedEntities = sqldf("SELECT intersectingTokensBetweenEntities.entity1 AS entity1, 
                                                            intersectingTokensBetweenEntities.entity2 AS entity2, 
                                                            count(*) as numTokens, 
                                                            -sum((2.0/tokenFrequency.tokenCount) * log10(2.0/tokenFrequency.tokenCount)) AS selfInfoPartitioning
                                                            FROM intersectingTokensBetweenEntities 
                                                            JOIN tokenFrequency 
                                                            ON tokenFrequency.token == intersectingTokensBetweenEntities.token 
                                                            GROUP BY entity1, entity2 ORDER BY entity1, selfInfoPartitioning DESC, numTokens DESC")
    
    # there are two links between each combination of candiates
    # entity1 -> entity2
    # entity2 -> entity1
    
    # TODO now get the highest matches for each entity1 - the query above is sorted
    isDuplicated = duplicated(selfInformationOfPartitioningPerMatchedEntities$entity1)
    #if not duplicated, then it's the highest value
    locs = which(isDuplicated==FALSE)
    #create sequences of length numTopMatches starting with the locations of the highest values (locs with FALSE for isDuplicated)
    # This is then used to return the top numTopMatches rows for each value of entity1
    keepTheseRows = unique(sort(unlist(lapply(locs, function(x){c(x:(x+numTopMatches-1))}))))
    selfInformationOfPartitioningPerMatchedEntities = selfInformationOfPartitioningPerMatchedEntities[keepTheseRows,]
    
    # sort also by numTokens so in case there is a tie with the selfInfoPartitioning, the entries
    # with the most number of tokens in common will come out on top
    selfInformationOfPartitioningPerMatchedEntities = sqldf("SELECT * 
                                                          FROM selfInformationOfPartitioningPerMatchedEntities 
                                                          ORDER BY entity1, selfInfoPartitioning DESC, numTokens DESC")
    
    matchCountPerEntity = sqldf("SELECT entity1, 
                              COUNT(*) AS matchCount 
                              FROM selfInformationOfPartitioningPerMatchedEntities 
                              GROUP BY entity1 ORDER BY matchCount DESC")
    
    
    row.names(selfInformationOfPartitioningPerMatchedEntities) = NULL
    
    # track down the entries, figure out which tokens they have in common, then dump the data out in a spreadsheet
    # also find the mutual best matches
    
    # remove NA rows
    selfInformationOfPartitioningPerMatchedEntities = selfInformationOfPartitioningPerMatchedEntities[complete.cases(selfInformationOfPartitioningPerMatchedEntities),]
    # sort first by score, then by number of tokens (in case of tie)
    selfInformationOfPartitioningPerMatchedEntities = sqldf("select * from selfInformationOfPartitioningPerMatchedEntities order by entity1, selfInfoPartitioning DESC, numTokens DESC")
    
    # reset the identifiers for the entities (data entries) in data set 2
    selfInformationOfPartitioningPerMatchedEntities$entity2 = selfInformationOfPartitioningPerMatchedEntities$entity2 - nrow(tokenLocs1)
    return(selfInformationOfPartitioningPerMatchedEntities)
  } else {
    # no matched terms
    return(NULL)
  }
}