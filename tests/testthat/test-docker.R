# TODO: CI docker setups fail with cryptic errors
# CI Docker build: Docker version 26.1.3, build b72abbb
#   The service ran into the following errors:
# * StatusConsoleListener Unable to create file ./logs/ors.log
# * StatusConsoleListener Could not create plugin of type class org.apache.logging.log4j.core.appender.RollingFileAppender for element RollingFile: java.lang.IllegalStateException: ManagerFactory [org.apache.logging.log4j.core.appender.rolling.RollingFileManager$RollingFileManagerFactory@4f704591] unable to create manager for [./logs/ors.log] with data [org.apache.logging.log4j.core.appender.rolling.RollingFileManager$FactoryData@4b5189ac[pattern=${sys:LOG_PATH}/${date:yyyy-MM}/app-%d{yyyy-MM-dd-HH}-%i.log.gz, append=true, bufferedIO=true, bufferSize=8192, policy=CompositeTriggeringPolicy(policies=[SizeBasedTriggeringPolicy(size=10485760)]), strategy=DefaultRolloverStrategy(min=1, max=7, useMax=true), advertiseURI=null, layout=%d{yyyy-MM-dd HH:mm:ss} %p [%-40.40c{1.}] - %m%n, filePermissions=null, fileOwner=null]]
# * StatusConsoleListener Unable to invoke factory method in class org.apache.logging.log4j.core.appender.RollingFileAppender for element RollingFile: java.lang.IllegalStateException: No factory method found for class org.apache.logging.log4j.core.appender.RollingFileAppender
# * StatusConsoleListener Null object returned for RollingFile in Appenders.
# * StatusConsoleListener Unable to locate appender "File" for logger config "root"
# * ORS-Init [ o.h.o.r.RoutingProfileManager ]
# * ORS-Init [ o.h.o.r.RoutingProfileManager ] Failed to either read or execute the ors configuration and its parameters: java.lang.RuntimeException: Directory /home/ors/./graphs/car does not exist and cannot be created to place lock file there: /home/ors/./graphs/car/gh.lock
skip_on_ci()

skip_if_offline("github.com")
skip_on_cran()
if (on_os("darwin")) {
  skip_on_ci()
}
skip_on_os("solaris")
skip_if_not(docker_running(), "docker unavailable")

ors <- local_ors_instance(verbose = FALSE, dry = FALSE, version = "8.0.0")
ors$set_extract(file = test_pbf())
withr::local_options(list(rors_setup_warn = FALSE))

skip_if_not(startsWith(ors$compose$name, "test-rors"))

test_that("build checks can say no", {
  expect_false(ors$is_built())
  expect_false(ors$is_running())
  expect_false(ors$is_ready())
})

test_that("ors setup works with non-default extract", {
  ors$up()
  expect_true(ors$is_built())
  expect_true(ors$is_running())
  expect_true(ors$is_ready())
  expect_no_error(ors_inspect(test_coords(1:2), as = "list"))
})

test_that("can stop", {
  expect_no_error(ors$stop())
  expect_false(ors$is_running())
  expect_false(ors$is_ready())
})

test_that("can restart", {
  expect_no_error(ors$start())
})

test_that("logs are formatted", {
  logs <- ors$show_logs()
  expect_type(logs, "character")
  expect_gt(length(logs), 10) # arbitrary length that is higher than a few
  expect_true(any(cli::ansi_has_any(logs)))
  expect_false(any(cli::ansi_has_any(ors$show_logs(format = FALSE))))
})

test_that("info methods work", {
  expect_type(ors$get_container(), "list")
  expect_type(ors$get_image(), "list")
  expect_type(ors$get_status(), "list")
})

test_that("can die", {
  expect_no_error(ors$down())
  expect_false(ors$is_built())
})
