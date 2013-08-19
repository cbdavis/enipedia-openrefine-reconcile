options(stringsAsFactors = FALSE)

getTokenLocs = function(text){
  # export all the tokens to a file
  write.table(t(c("entityID", "token")), file="tokenLocs.txt", append=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
  for (i in c(1:length(text))){
    write.table(cbind(i, strsplit(text[i], " ")[[1]]), file="tokenLocs.txt", append=TRUE, col.names=FALSE, row.names=FALSE, sep="\t")
  }
  df = read.table("tokenLocs.txt", header=TRUE)
  return(df)
}

findMutualBestMatches = function(selfInformationOfPartitioningPerMatchedEntities){
  entity1 = selfInformationOfPartitioningPerMatchedEntities$entity1
  entity2 = selfInformationOfPartitioningPerMatchedEntities$entity2
  
  selfInfoMatrix = sparseMatrix(i=max(entity1), j=max(entity2), x=0)
  rowColumnIndices = cbind(entity1, entity2)
  selfInfoMatrix[rowColumnIndices] = selfInformationOfPartitioningPerMatchedEntities$selfInfoPartitioning
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
      
  # find the self information of a partitioning - shows the uniqueness of terms in this partitioning/intersection of tokens
  selfInformationOfPartitioningPerMatchedEntities = sqldf("SELECT intersectingTokensBetweenEntities.entity1 AS entity1, 
                                                          intersectingTokensBetweenEntities.entity2 AS entity2, 
                                                          count(*) as numTokens, 
                                                          -sum(((1.0 - ((cast(tokenFrequency.tokenCount as real) - 2.0)/tokenFrequency.tokenCount)) * log10((1.0 - ((cast(tokenFrequency.tokenCount as real) - 2.0)/cast(tokenFrequency.tokenCount as real)))))) AS selfInfoPartitioning
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
}