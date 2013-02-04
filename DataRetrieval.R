########## Data Retrieval ##########

#These are common prefixes used with the Enipedia SPARQL endpoint
getPrefixes <- function(){
  return("PREFIX a: <http://enipedia.tudelft.nl/wiki/>
         PREFIX prop: <http://enipedia.tudelft.nl/wiki/Property:>
         PREFIX cat: <http://enipedia.tudelft.nl/wiki/Category:>
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
         PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         PREFIX fn: <http://www.w3.org/2005/xpath-functions#>")
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
      
      #generate cleaned versions of the names that are more suited for matching
      enipediaData$CleanedPlantName = removeTheWeirdness(gsub(' Powerplant', '', enipediaData$name))
      enipediaData$CleanedOwnerName = removeTheWeirdness(enipediaData$owner)
      enipediaData$CleanedStateName = removeTheWeirdness(enipediaData$state)
      
    } else {
      print("no results for country")
    }
  } else {
    print("no country specified")
  }
  return(enipediaData)
}
