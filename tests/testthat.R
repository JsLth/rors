library(testthat)
library(ORSRouting)

test_check("ORSRouting")

# The package isn't intended to be used on Mac or Solaris
if (Sys.info()["sysname"] == "Darwin") {
  return()
}
if (Sys.info()["sysname"] == "SunOS") {
  return()
}

# The tests are supposed to be run entirely and not stop on errors.
# If the tests stop during the process, you may risk setting up containers
# that are not removed after the tests fail.
