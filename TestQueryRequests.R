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


queryRequest = '{
                  "q0":{
                    "query":"RWE Power AG",
                    "type":"Category:Energy_Company",
                    "type_strict":"should"
                  },
                  "q1":{
                    "query":"Vattenfall Europe Generation AG",
                    "type":"Category:Energy_Company",
                    "type_strict":"should"
                  }
                }'

queryRequest = '{"q0":{"query":"Klingenberg","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"}]},"q1":{"query":"Niederwartha","type":"Category:Powerplant","type_strict":"should","properties":[]},"q2":{"query":"Wedel","type":"Category:Powerplant","type_strict":"should","properties":[]},"q3":{"query":"Thyrow","type":"Category:Powerplant","type_strict":"should","properties":[]},"q4":{"query":"Reuter West","type":"Category:Powerplant","type_strict":"should","properties":[]},"q5":{"query":"Windpark J\xc3\xa4nschwalde","type":"Category:Powerplant","type_strict":"should","properties":[]},"q6":{"query":"Brunsb\xc3\xbcttel","type":"Category:Powerplant","type_strict":"should","properties":[]},"q7":{"query":"Geesthacht","type":"Category:Powerplant","type_strict":"should","properties":[]},"q8":{"query":"Windpark alpha ventus","type":"Category:Powerplant","type_strict":"should","properties":[]},"q9":{"query":"Tiefstack","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"}]}}'

queryRequest = '{"query":"Klingenberg","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe W\xc3\xa4rme Aktiengesellschaft"}]}'

queryRequest = '{"q0":{"query":"Ahrensfelde","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe Generation AG"},{"pid":"point","v":"52.58195, 13.56734"}]},"q1":{"query":"Markersbach","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe Generation AG"},{"pid":"point","v":"50.52133, 12.87778"}]},"q2":{"query":"IKW R\xc3\xbcdersdorf","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe New Energy Ecopower GmbH"},{"pid":"point","v":"52.49099, 13.82981"}]},"q3":{"query":"Wilmersdorf","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe W\xc3\xa4rme AG"},{"pid":"point","v":"52.47966, 13.30782"}]},"q4":{"query":"EBS-HKW Rostock","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe New Energy Ecopower GmbH"},{"pid":"point","v":"54.14582, 12.13999"}]},"q5":{"query":"Thyrow","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe Generation AG"},{"pid":"point","v":"52.227691, 13.319105"}]},"q6":{"query":"Schwarze Pumpe","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe Generation AG"},{"pid":"point","v":"51.53552999999999, 14.36422"}]},"q7":{"query":"Boxberg","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe Generation AG"},{"pid":"point","v":"51.4076272, 14.5723372"}]},"q8":{"query":"Mitte","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe W\xc3\xa4rme AG"},{"pid":"point","v":"52.5100529, 13.420037"}]},"q9":{"query":"Charlottenburg","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe W\xc3\xa4rme AG"},{"pid":"point","v":"52.52135, 13.31023"}]}}'

queryRequest = '{"query":"Ahrensfelde","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"country","v":"Germany"},{"pid":"owner","v":"Vattenfall Europe Generation AG"},{"pid":"point","v":"52.58195, 13.56734"}]}'

queryRequest = '{"q0":{"query":"Rugeley ","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"International Power / Mitsui"}]},"q1":{"query":"Slieve Divena","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"Infinis"}]},"q2":{"query":"Castleford","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"E.On UK"}]},"q3":{"query":"Arnish","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"Scottish & Southern Energy plc"}]},"q4":{"query":"Lubreoch","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"Scottish & Southern Energy plc"}]},"q5":{"query":"Hare Hill","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"Scottish Power"}]},"q6":{"query":"Bowbeat","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"E.On UK"}]},"q7":{"query":"Walkaway","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"EDF Energy Renewables"}]}}'

queryRequest = '{"query":"Rugeley ","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"International Power / Mitsui"}]},"q1":{"query":"Slieve Divena","type":"Category:Powerplant","type_strict":"should","properties":[{"pid":"owner","v":"Infinis"}]}'

queryRequest = fromJSON(queryRequest)