library(httptest2)
if (!nzchar(Sys.getenv("ORS_TOKEN"))) {
  Sys.setenv(ORS_TOKEN = "test")
}
