library(httptest2)

set_redactor(function(x) {
  gsub_response(x, "(http\\://)?localhost\\:[0-9]+/ors/v2/", "")
})

if (!nzchar(Sys.getenv("ORS_TOKEN"))) {
  Sys.setenv(ORS_TOKEN = "test")
}
