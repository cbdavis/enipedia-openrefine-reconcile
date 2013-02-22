########## Data Retrieval ##########

#These are common prefixes used with the Enipedia SPARQL endpoint
getPrefixes <- function(){
  return("PREFIX a: <http://enipedia.tudelft.nl/wiki/>
          PREFIX prop: <http://enipedia.tudelft.nl/wiki/Property:>
          PREFIX cat: <http://enipedia.tudelft.nl/wiki/Category:>
          PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX euets: <http://enipedia.tudelft.nl/data/EU-ETS/>
          PREFIX fn: <http://www.w3.org/2005/xpath-functions#>
          PREFIX wgs84: <http://www.w3.org/2003/01/geo/wgs84_pos#>
          PREFIX eprtr: <http://prtr.ec.europa.eu/rdf/schema.rdf#>
          PREFIX eprtrResource: <http://prtr.ec.europa.eu/rdf/>
         ")
}

#country is the two digit code - NL, DE, etc
retrievePlantDataFromEPRTR <- function(country){
  endpoint = "http://enipedia.tudelft.nl/sparql"
  queryString = paste(getPrefixes(), 
                      "select * where {
                        ?x rdf:type eprtr:Facility . 
                        ?x eprtr:facilityName ?name . 
                        ?x eprtr:facilityID ?facilityID . 
                        ?x eprtr:inCountry <http://prtr.ec.europa.eu/rdf/country/", country,">
                        OPTIONAL{ ?x wgs84:lat ?lat } . 
                        OPTIONAL{ ?x wgs84:long ?long } . 
                        OPTIONAL{ ?x eprtr:streetName ?streetName } . 
                        OPTIONAL{ ?x eprtr:city ?city } . 
                        OPTIONAL{ ?x eprtr:postalCode ?postalCode } . 
                        OPTIONAL{ ?x eprtr:latestReport ?latestReport . 
                        ?latestReport eprtr:parentCompanyName ?parentCompanyName . }
                      }", sep="")
  d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  data = d$results
  return(data)
}

#country is the two digit code - NL, DE, etc
retrievePlantDataFromEUETS <- function(country){
  endpoint = "http://enipedia.tudelft.nl/sparql"
  queryString = paste(getPrefixes(), "select * where {
                        ?installation rdfs:label ?name . 
                        ?installation euets:account ?account . 
                        ?installation euets:euetsID ?euetsID . 
                        ?installation euets:installationIdentifier ?installationIdentifier . 
                        OPTIONAL{?installation euets:address1 ?address1 } . 
                        OPTIONAL{?installation euets:address2 ?address2 } . 
                        OPTIONAL{?installation euets:city ?city } . 
                        ?installation euets:countryCode \"", country, "\" . 
                        ?account euets:AccountHolder ?account_holder .
                        ?account euets:identifierInReg ?identifierInReg . 
                      }", sep="")
  d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  data = d$results
  return(data)
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
                        OPTIONAL{?x prop:EU_ETS_ID ?euetsID } . 
                        OPTIONAL{?x prop:State ?state } .
                        OPTIONAL{?x prop:Ownercompany ?owner }. 
                        ?x prop:Point ?point . 
                        }", sep="")
    d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
    enipediaData = d$results
    if (dim(enipediaData)[1] > 0){
      coords = extractCoordinates(enipediaData$point)
      enipediaData$lat = coords$lat
      enipediaData$lon = coords$lon
      
      #generate cleaned versions of the names that are more suited for matching
      enipediaData$CleanedPlantName = normalizeText(gsub(' Powerplant', '', enipediaData$name))
      enipediaData$CleanedOwnerName = normalizeText(enipediaData$owner)
      enipediaData$CleanedStateName = normalizeText(enipediaData$state)
      
    } else {
      print("no results for country")
    }
  } else {
    print("no country specified")
  }
  return(enipediaData)
}
