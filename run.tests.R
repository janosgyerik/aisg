require(RUnit)

test.suite <- defineTestSuite(
  "all",
  dirs = file.path("tests"),
  testFileRegexp = 'tests\\.R')

printTextProtocol(runTestSuite(test.suite))
