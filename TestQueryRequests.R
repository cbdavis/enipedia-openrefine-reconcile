library(rjson)

queryRequest = '{
                  "query":"Electriciteit de France",
                  "type":"Category:Energy_Company",
                  "properties":[
                    {
                      "pid":"Country",
                      "v":"France"
                    }
                  ]
                }'

queryRequest = fromJSON(queryRequest)