#need to check for things like E.On, B.V., B. V., B V, BV
#What about "Électricité de France"? the "de" is removed as a stop word
test.normalizeText <- function() {
  checkEquals(normalizeText("CRISTAL UNION ET.D'ARCIS-SUR-AUBE"), "cristal union et d arcis sur aube")
  checkEquals(normalizeText("Smurfit Socar/papeterie Seine"), "smurfit socar papeterie seine")
  checkEquals(normalizeText("imprimerie didier mary 6 route ferte sous jouarre baly herve lecture seule 687"), "imprimerie didier mary route ferte sous jouarre baly herve lecture seule")
}