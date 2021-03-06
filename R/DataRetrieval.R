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

retrievePlantDataFromEUETS_NEW <- function(country){
  endpoint = "http://localhost:3030/ds/query"
  queryString = paste(getPrefixes(), "select * where {
                      ?account euets:installation ?installation .
                      ?account euets:AccountHolder ?account_holder .
                      ?account euets:identifierInReg ?identifierInReg .
                      ?installation euets:name ?name .
                      ?installation euets:installationIdentifier ?installationIdentifier .
                      OPTIONAL{ ?installation euets:latitude ?lat } .
                      OPTIONAL{ ?installation euets:longitude ?long } .
                      OPTIONAL{ ?installation euets:address1 ?address1 } .
                      OPTIONAL{ ?installation euets:address2 ?address2 } .
                      OPTIONAL{ ?installation euets:city ?city } .
                      OPTIONAL{ ?installation euets:zipCode ?zip } .
                      ?installation euets:countryCode ?countryCode .
                      FILTER(?countryCode = \"", country, "\") .
                                      ?installation euets:permitIdentifier ?permitIdentifier .
                                      }", sep="")
  d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  data = d$results
  return(data)
}

retrievePlantDataFromIndustryAbout <- function(country){
  queryString = paste("PREFIX iaprop: <http://industryabout.com/property/>  
                      select * where {  
                      ?ppl iaprop:name ?name .   
                      ?ppl iaprop:url ?url .  
                      ?ppl iaprop:country ?country .   
                      OPTIONAL{ ?ppl iaprop:area ?area } .   
                      OPTIONAL{ ?ppl iaprop:type ?type } .   
                      OPTIONAL{ ?ppl iaprop:power_capacity ?power_capacity } .   
                      OPTIONAL{ ?ppl iaprop:owner ?owner } .   
                      OPTIONAL{ ?ppl iaprop:activity_since ?activity_since } .   
                      OPTIONAL{ ?ppl iaprop:longitude ?longitude } .   
                      OPTIONAL{ ?ppl iaprop:latitude ?latitude } .   
                      OPTIONAL{ ?ppl iaprop:web ?web } .   
                      OPTIONAL{ ?ppl iaprop:wikipedia ?wikipedia } .   
                      OPTIONAL{ ?ppl iaprop:notes ?notes } .   
                      OPTIONAL{ ?ppl iaprop:address ?address } .   
                      OPTIONAL{ ?ppl iaprop:fuel ?fuel } .   
                      OPTIONAL{ ?ppl iaprop:shareholders ?shareholders } .   
                      OPTIONAL{ ?ppl iaprop:other_name ?other_name } .   
                      FILTER(?country = '", country, "' ) .  
                      }", sep="")
  d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  data = d$results
  return(data)  
}

retrievePlantDataFromGlobalEnergyObservatory <- function(country){
  scraperURL = "https://api.scraperwiki.com/api/1.0/datastore/sqlite?format=csv&name=global_energy_observatory_power_plants&query=select%20Name%2C%20Country%2C%20Latitude_Start%2C%20Longitude_Start%2C%20GEO_Assigned_Identification_Number%2C%20Fuel_type%2C%20Design_Capacity_MWe_nbr%2C%20State%2C%20Location%2C%20CurrentPage_sys%2C%20Operating_Company%2C%20Year_Project_Commissioned%2C%20References1%2C%20References2%20from%20%60swdata%60"
  file="GlobalEnergyObservatory.csv"
  if (file.exists(file) == FALSE) {
    #should work
    download.file(scraperURL, file, method="curl")
  }
  geoData = read.csv(file, header=TRUE)
  locs = which(geoData$Country == country)
  if (length(locs) == 0){
    errorMessage = paste(country, "is not found in list of valid countries for the Global Energy Observatory, valid countries are ", paste(unique(sort(geoData$Country)), collapse=", "))
  }
  geoData = geoData[locs,]
  return(geoData)
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
                        OPTIONAL{ ?x eprtr:latestReport ?latestReport . 
                        ?latestReport eprtr:nationalID ?nationalID . }
                      }", sep="")
  d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  data = d$results
  return(data)
}

#This returns a lookup table
retrieveFacilityIDAndNationalIDFromEPRTR <- function(country){
  endpoint = "http://enipedia.tudelft.nl/sparql"
  queryString = paste(getPrefixes(), 
                      "select distinct ?facilityID ?nationalID where {
                        ?facilityReport rdf:type eprtr:FacilityReport .
                        ?facilityReport eprtr:nationalID ?nationalID . 
                        ?facilityReport eprtr:forFacility ?facility . 
                        ?facility eprtr:facilityID ?facilityID . 
                        ?facility eprtr:inCountry <http://prtr.ec.europa.eu/rdf/country/", country, ">
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
                        OPTIONAL{ ?installation euets:latitude ?lat } . 
                        OPTIONAL{ ?installation euets:longitude ?long } . 
                        OPTIONAL{ ?installation euets:address1 ?address1 } . 
                        OPTIONAL{ ?installation euets:address2 ?address2 } . 
                        OPTIONAL{ ?installation euets:city ?city } . 
                        ?installation euets:countryCode \"", country, "\" . 
                        ?account euets:AccountHolder ?account_holder .
                        ?account euets:identifierInReg ?identifierInReg . 
                      }", sep="")
  d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  data = d$results
  return(data)
}

retrievePlantDataFromEUETS_NEW <- function(country){
  endpoint = "http://localhost:3030/ds/query"
  queryString = paste(getPrefixes(), "select * where {
                                      ?account euets:installation ?installation . 
                                      ?account euets:AccountHolder ?account_holder .
                                      ?account euets:identifierInReg ?identifierInReg . 
                                      ?installation euets:name ?name . 
                                      ?installation euets:installationIdentifier ?installationIdentifier . 
                                      OPTIONAL{ ?installation euets:latitude ?lat } . 
                                      OPTIONAL{ ?installation euets:longitude ?long } . 
                                      OPTIONAL{ ?installation euets:address1 ?address1 } . 
                                      OPTIONAL{ ?installation euets:address2 ?address2 } . 
                                      OPTIONAL{ ?installation euets:city ?city } .
                                      OPTIONAL{ ?installation euets:zipCode ?zip } .  
                                      ?installation euets:countryCode ?countryCode . 
                                      FILTER(?countryCode = \"", country, "\") . 
                                      ?installation euets:permitIdentifier ?permitIdentifier .
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
    # fix encoding 
    endpoint = "http://enipedia.tudelft.nl/sparql"
    queryString = paste(getPrefixes(), 
                        "select * where {
                        ?x rdf:type cat:Powerplant .
                        OPTIONAL{?x prop:City ?city} .
                        ?x rdfs:label ?name . 
                        ?x prop:Country <http://enipedia.tudelft.nl/wiki/", gsub(" ", "_", country) ,"> . 
                        OPTIONAL{?x prop:EU_ETS_ID ?euetsID } . 
                        OPTIONAL{?x prop:State ?state } .
                        OPTIONAL{?x prop:Ownercompany ?owner }. 
                        OPTIONAL{?x prop:OpenStreetMap_link ?osmLink } . 
                        OPTIONAL{?x prop:Wikimapia_link ?wikimapiaLink } . 
                        OPTIONAL{?x prop:Wikipedia_page ?wikipedia } . 
                        OPTIONAL{?x prop:Global_Energy_Observatory_ID ?geoID } .
                        ?x prop:Point ?point . 
                        }", sep="")
    d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
    enipediaData = d$results
    
    if (ncol(enipediaData) == 1) {
      # query probably timed out, need to split up the queries using LIMIT and OFFSET      
      enipediaData = data.frame()
      keepLooking=TRUE
      offset = 0
      limit = 100
      while(keepLooking==TRUE){
        print(offset)
        queryStringWithOffset = paste(queryString, " OFFSET ", offset, " LIMIT ", limit, sep="")
        d <- SPARQL(url=endpoint, query=queryStringWithOffset, format='csv', extra=list(format='text/csv'))
        if (nrow(d$results) > 0 && ncol(d$results) > 1){ # check that the data doesn't look bogus
          enipediaData = rbind(enipediaData, d$results)
        } else {
          keepLooking = FALSE
        }
        offset = offset + limit
      }
    }
    
    if (dim(enipediaData)[1] > 0){
      coords = extractCoordinates(enipediaData$point)
      enipediaData$lat = coords$lat
      enipediaData$lon = coords$lon
      
      #generate cleaned versions of the names that are more suited for matching
      enipediaData$CleanedPlantName = normalizeText(gsub(' Powerplant', '', enipediaData$name))
      enipediaData$CleanedCityName = normalizeText(enipediaData$city)
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
