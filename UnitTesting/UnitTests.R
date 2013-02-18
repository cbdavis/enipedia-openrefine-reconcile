#never ever ever convert strings to factors
options(stringsAsFactors = FALSE)

library(RUnit)

#functions to test:
# extractCoordinates
# make sure that queries to Enipedia work

testsuite.normalizeText <- defineTestSuite("normalizeText",
                                 dirs = file.path("UnitTesting"),
                                 testFileRegexp = "^runit.+\\.R",
                                 testFuncRegexp = "^test.+",
                                 rngKind = "Marsaglia-Multicarry",
                                 rngNormalKind = "Kinderman-Ramage")

testResult <- runTestSuite(testsuite.normalizeText)
printTextProtocol(testResult)